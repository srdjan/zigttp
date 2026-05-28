//! Focused aggregator for the cassette harness tests so `zig build
//! test-cassette` can exercise the record + replay layer without
//! pulling in the full `pi_app` graph.

comptime {
    _ = @import("providers/cassette_client.zig");
    _ = @import("providers/cassette_record.zig");
    // Replay routes through both provider parsers; pull them in so any
    // compile-time drift here surfaces inside the cassette step.
    _ = @import("providers/anthropic/sse_parser.zig");
    _ = @import("providers/anthropic/response_assembler.zig");
    _ = @import("providers/anthropic/apply_edit.zig");
    _ = @import("providers/openai/client.zig");
}
