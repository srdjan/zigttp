# pi OpenAI provider cassettes

Spec for the recorded-replay harness that unblocks
[Phase 8](../packages/pi/README.md#deferred) of the pi port: a second
model provider, recorded once against the real API and replayed
deterministically in CI so the test suite never burns tokens.

## Scope

Everything in this document is deferred work. The `loop.ModelClient`
vtable in `packages/pi/src/loop.zig` is already provider-neutral; the
work below slots a second backend alongside `providers/anthropic/`
without reshaping the loop.

The single external dependency is one live `POST` per cassette
scenario. Three scenarios cover the full feature matrix; a first pass
can ship with one.

## The live call

Target: the OpenAI Responses API (`POST /v1/responses`), the unified
successor to Chat Completions for streaming tool-use.

Request body for the happy-path scenario:

```json
{
  "model": "gpt-4.1",
  "input": [
    {"role": "user", "content": "Reply with the single word 'pong'."}
  ],
  "stream": true,
  "temperature": 0
}
```

Headers:

```
Authorization: Bearer ${OPENAI_API_KEY}
Content-Type: application/json
Accept: text/event-stream
```

Why that prompt: a single-word deterministic response minimizes token
variance across re-recordings and exercises the
`output_text.delta` -> `output_text.done` event path that carries every
text-only response the agent receives.

## SSE byte stream

OpenAI emits typed SSE events for Responses API streams. The byte
stream for the `ping_pong` scenario looks like:

```
event: response.created
data: {"type":"response.created","response":{"id":"resp_...","model":"gpt-4.1",...}}

event: response.output_item.added
data: {"type":"response.output_item.added","item":{"id":"msg_...","role":"assistant","content":[]}}

event: response.content_part.added
data: {"type":"response.content_part.added","part":{"type":"output_text","text":""}}

event: response.output_text.delta
data: {"type":"response.output_text.delta","delta":"pong"}

event: response.output_text.done
data: {"type":"response.output_text.done","text":"pong"}

event: response.content_part.done
data: {"type":"response.content_part.done","part":{"type":"output_text","text":"pong"}}

event: response.output_item.done
data: {"type":"response.output_item.done","item":{...}}

event: response.completed
data: {"type":"response.completed","response":{"id":"resp_...","usage":{"input_tokens":12,"output_tokens":1,"total_tokens":13}}}
```

Approximate size: 1-2 KB per scenario.

## Cassette file format

Cassettes live at `packages/pi/testdata/cassettes/openai/<name>.jsonl`.
One line is the header; remaining lines are raw SSE chunks, one per
server-sent event boundary (`\n\n`-delimited in the wire stream):

```jsonl
{"v":1,"provider":"openai","model":"gpt-4.1","scenario":"ping_pong","recorded_at":"2026-04-23T15:00:00Z"}
{"sse":"event: response.created\ndata: {...}\n\n"}
{"sse":"event: response.output_item.added\ndata: {...}\n\n"}
{"sse":"event: response.output_text.delta\ndata: {\"type\":\"response.output_text.delta\",\"delta\":\"pong\"}\n\n"}
{"sse":"event: response.output_text.done\ndata: {...}\n\n"}
{"sse":"event: response.completed\ndata: {...}\n\n"}
```

The replay client (see below) concatenates every `sse` field and
feeds the bytes through the production `providers/openai/sse_parser.zig`
and `providers/openai/response_assembler.zig`. Parser and assembler
run unchanged between live and replay.

## The recorder

A standalone binary, `packages/pi/tools/cassette_record.zig`, wired as
a new `zig build cassette-record` step:

```bash
OPENAI_API_KEY=sk-... zig build cassette-record -- ping_pong
```

Flow:

1. Load the scenario by name from an inline `const scenarios = [_]Scenario{ ... }`.
2. Build the request body via the production `providers/openai/request.zig`
   so the recorded bytes match what pi sends at runtime.
3. POST to `https://api.openai.com/v1/responses`; read the response
   socket in 4 KiB chunks.
4. On each `\n\n` event boundary, write one JSONL line
   `{"sse": <raw chunk>}` to the cassette path (quotes and newlines
   escaped).
5. Exit 0 on success; leave a partial cassette on failure so the
   operator can inspect what was captured.

Total implementation size: about 120 lines of Zig.

## Minimum cassette set

Three scenarios cover the full feature matrix:

| Cassette             | Prompt shape                                      | What it proves                                           |
|----------------------|---------------------------------------------------|----------------------------------------------------------|
| `ping_pong.jsonl`    | "Reply with the single word 'pong'"               | text-only SSE path; usage accounting                     |
| `tool_call.jsonl`    | Instruction that forces a specific tool call      | tool_call event shape; args_json assembly                |
| `multi_turn.jsonl`   | User + tool_result + assistant follow-up          | conversation-state carry-over across turns               |

A first PR can land with `ping_pong.jsonl` only; the other two can
follow in subsequent PRs. Each recording takes about 3 seconds and
costs a few tenths of a cent.

## What can be built without an API key

Everything except the cassette files themselves. Mapping to concrete
targets:

- `packages/pi/src/providers/openai/{request,client,sse_parser,events,response_assembler,tools_schema,json_writer}.zig`
  - mirror the `anthropic/` layout, hand-written from the Responses
  API spec.
- `packages/pi/src/providers/cassette_client.zig` - implements the
  `ModelClient` vtable by reading a cassette path and replaying its
  SSE bytes through whichever provider's parser matches the header's
  `provider` field.
- `packages/pi/tools/cassette_record.zig` - the recorder binary above.
- `packages/pi/src/providers/models.zig` - add `gpt-4.1` entry with
  `provider: .openai` tag; extend the `Model` struct with the tag.
- `packages/pi/src/agent.zig:buildModelClient` - switch on the
  provider tag when constructing the backend client.
- Hand-written synthetic `ping_pong.jsonl` fixture using the format
  above. The test suite drives it through the same replay path the
  real cassette will use; the first live recording swaps the file.

The cross-provider test takes the form:

```zig
test "cross-provider: ping_pong scenario yields identical AssistantReply shape" {
    const allocator = testing.allocator;

    // Same transcript scripted against both providers.
    const anthropic_reply = try runAgainstCassette(allocator, "anthropic/ping_pong.jsonl");
    const openai_reply = try runAgainstCassette(allocator, "openai/ping_pong.jsonl");

    // Text differs (providers phrase differently); shape matches.
    try testing.expectEqual(@as(?turn.ResponseVariant, .final_text), std.meta.activeTag(anthropic_reply.response));
    try testing.expectEqual(@as(?turn.ResponseVariant, .final_text), std.meta.activeTag(openai_reply.response));
}
```

## What specifically needs the live call

Just this, once per scenario:

```bash
export OPENAI_API_KEY=sk-...
zig build cassette-record -- ping_pong
zig build cassette-record -- tool_call
zig build cassette-record -- multi_turn
```

After that, `zig build test` replays the cassettes and the full
cross-provider test suite runs deterministically in CI against both
backends.

## See also

- [../packages/pi/README.md](../packages/pi/README.md) - pi architecture
  reference; the Backends section describes the `loop.ModelClient`
  vtable this work plugs into.
- [./architecture.md](./architecture.md) - how `pi_app` fits alongside
  the other binaries in the workspace.
