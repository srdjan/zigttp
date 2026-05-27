# Hello, Claude

The smallest possible "AI is wired up" demo. You'll store an Anthropic API
key once, then watch `zigttp expert` stream a real Claude response into
your terminal while editing the tiny handler in this folder.

The streaming you see is real Server-Sent Events from
`api.anthropic.com/v1/messages`, parsed by the runtime's Anthropic provider
(`packages/pi/src/providers/anthropic/sse_parser.zig`) and rendered token
by token as it arrives. zigttp handlers themselves can't stream response
bodies; the streaming surface is `zigttp expert`.

## One-time setup

```
zigttp auth claude
```

You'll be prompted to paste an API key from
[console.anthropic.com](https://console.anthropic.com/). Input is hidden.
The key is written to `~/.zigttp/providers.json` with mode 0600 and stays
there across projects.

To check what's stored:

```
zigttp auth status
```

To remove the stored key:

```
zigttp auth revoke claude
```

If you'd rather use a shell-exported `ANTHROPIC_API_KEY`, do that instead.
The shell value always wins; the stored file only fills the gap when the
shell variable is unset.

## Run the demo

```
cd examples/hello-claude
zigttp expert
```

You should see the expert prompt appear. Try:

```
> read handler.ts and add a GET /version route that returns { version: "0.1.0" }
```

Claude's response streams in. When the agent proposes an edit, you'll be
asked to approve it. Accept it and the file is updated; reject it and the
agent tries a different approach.

Run the handler at any point to confirm the edit works:

```
zigttp dev
# then in another shell:
curl http://localhost:3000/version
```

## What you've just proved

- `zigttp auth claude` stores the key without touching `.env` or your shell rc.
- `zigttp dev`, `zigttp serve`, and `zigttp expert` all auto-inject the stored
  key into `ANTHROPIC_API_KEY` at process start. Handlers that declare the
  variable in their contract see it through `zigttp:env`.
- The Anthropic backend used by `zigttp expert` streams via SSE in real time:
  `packages/pi/src/providers/anthropic/client.zig` -> `sse_parser.zig`.

## Why isn't there a streaming JS handler example?

The zigttp runtime buffers handler response bodies (`http_types.zig`
defines `body: []const u8`), and `zigttp:fetch` blocks rather than
returning a Promise. So even if a handler called the Anthropic streaming
endpoint, it could not pipe the stream back to its own caller. The
streaming demo lives in `zigttp expert` because that's where the runtime
actually streams.

If you want to call Claude *from* a handler, do a non-streaming completion
with `zigttp:fetch` and return the buffered text. The shape is identical
to [`examples/fetch/webhook.ts`](../fetch/webhook.ts), just pointed at
`api.anthropic.com/v1/messages` with the right headers.
