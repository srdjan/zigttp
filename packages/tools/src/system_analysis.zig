const std = @import("std");
const zigts = @import("zigts");

const precompile = @import("precompile.zig");
const handler_contract = zigts.handler_contract;
const system_linker = zigts.system_linker;

pub const CompiledSystem = struct {
    analysis: system_linker.SystemAnalysis,
    contracts: []handler_contract.HandlerContract,

    pub fn deinit(self: *CompiledSystem, allocator: std.mem.Allocator) void {
        self.analysis.deinit(allocator);
        for (self.contracts) |*contract| contract.deinit(allocator);
        allocator.free(self.contracts);
    }
};

pub fn loadCompiledSystem(
    allocator: std.mem.Allocator,
    system_path: []const u8,
) !CompiledSystem {
    const system_json = try zigts.file_io.readFile(allocator, system_path, 1024 * 1024);
    defer allocator.free(system_json);

    var config = try system_linker.parseSystemConfig(allocator, system_json);
    errdefer config.deinit(allocator);
    try precompile.resolveSystemHandlerPaths(allocator, system_path, &config);

    if (config.handlers.len == 0) return error.EmptySystemConfig;

    const contracts = try allocator.alloc(handler_contract.HandlerContract, config.handlers.len);
    errdefer allocator.free(contracts);

    var compiled_count: usize = 0;
    errdefer {
        for (contracts[0..compiled_count]) |*contract| contract.deinit(allocator);
    }

    for (config.handlers, 0..) |entry, idx| {
        var result = try precompile.runCheckOnly(allocator, entry.path, null, false, system_path);
        defer result.deinit(allocator);

        if (result.totalErrors() > 0) {
            return error.SystemHandlerCompilationFailed;
        }

        if (result.contract) |contract| {
            contracts[idx] = contract;
            result.contract = null;
            compiled_count += 1;
        } else {
            return error.MissingHandlerContract;
        }
    }

    const analysis = try system_linker.linkSystem(allocator, contracts, config);
    return .{
        .analysis = analysis,
        .contracts = contracts,
    };
}
