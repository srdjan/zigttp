const std = @import("std");
const ir = @import("parser/ir.zig");
const type_pool_mod = @import("type_pool.zig");

const IrView = ir.IrView;
const NodeIndex = ir.NodeIndex;
const null_node = ir.null_node;
const TypePool = type_pool_mod.TypePool;
const TypeIndex = type_pool_mod.TypeIndex;
const null_type_idx = type_pool_mod.null_type_idx;

pub const MatchAnalysis = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    pool: *TypePool,

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, pool: *TypePool) MatchAnalysis {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .pool = pool,
        };
    }

    pub fn narrowTypeForPattern(self: *const MatchAnalysis, source_type: TypeIndex, pattern: NodeIndex) TypeIndex {
        if (source_type == null_type_idx) return null_type_idx;
        if (pattern == null_node) return source_type;

        var matches = std.ArrayList(TypeIndex).empty;
        defer matches.deinit(self.allocator);

        self.collectMatchingTypes(source_type, pattern, &matches);
        return switch (matches.items.len) {
            0 => null_type_idx,
            1 => matches.items[0],
            else => self.pool.addUnion(self.allocator, matches.items),
        };
    }

    pub fn isMatchExhaustive(self: *const MatchAnalysis, discriminant_type: TypeIndex, me: ir.Node.MatchExpr) bool {
        if (discriminant_type == null_type_idx) return false;

        for (0..me.arms_count) |i| {
            const arm_idx = self.ir_view.getListIndex(me.arms_start, @intCast(i));
            const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
            if (arm.pattern == null_node) return true;
        }

        var variants = std.ArrayList(TypeIndex).empty;
        defer variants.deinit(self.allocator);
        self.collectVariants(discriminant_type, &variants);
        if (variants.items.len == 0) return false;

        for (variants.items) |variant| {
            var covered = false;
            for (0..me.arms_count) |i| {
                const arm_idx = self.ir_view.getListIndex(me.arms_start, @intCast(i));
                const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                if (arm.pattern == null_node or self.patternFullyCoversType(arm.pattern, variant)) {
                    covered = true;
                    break;
                }
            }
            if (!covered) return false;
        }
        return true;
    }

    fn collectMatchingTypes(self: *const MatchAnalysis, source_type: TypeIndex, pattern: NodeIndex, out: *std.ArrayList(TypeIndex)) void {
        var variants = std.ArrayList(TypeIndex).empty;
        defer variants.deinit(self.allocator);
        self.collectVariants(source_type, &variants);

        for (variants.items) |variant| {
            if (self.patternCanMatchType(pattern, variant)) {
                self.appendUnique(out, variant);
            }
        }
    }

    fn collectVariants(self: *const MatchAnalysis, type_idx: TypeIndex, out: *std.ArrayList(TypeIndex)) void {
        if (type_idx == null_type_idx) return;

        const tag = self.pool.getTag(type_idx) orelse return;
        switch (tag) {
            .t_union => {
                for (self.pool.getUnionMembers(type_idx)) |member| {
                    self.collectVariants(member, out);
                }
            },
            .t_nullable => {
                self.collectVariants(self.pool.getNullableInner(type_idx), out);
                self.appendUnique(out, self.pool.idx_undefined);
            },
            else => self.appendUnique(out, type_idx),
        }
    }

    fn appendUnique(self: *const MatchAnalysis, out: *std.ArrayList(TypeIndex), type_idx: TypeIndex) void {
        for (out.items) |existing| {
            if (existing == type_idx) return;
        }
        out.append(self.allocator, type_idx) catch {};
    }

    fn patternCanMatchType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        if (pattern == null_node or type_idx == null_type_idx) return pattern == null_node;

        const type_tag = self.pool.getTag(type_idx) orelse return false;
        switch (type_tag) {
            .t_union => {
                for (self.pool.getUnionMembers(type_idx)) |member| {
                    if (self.patternCanMatchType(pattern, member)) return true;
                }
                return false;
            },
            .t_nullable => {
                if (self.patternCanMatchType(pattern, self.pool.getNullableInner(type_idx))) return true;
                return self.patternCanMatchType(pattern, self.pool.idx_undefined);
            },
            else => {},
        }

        const pattern_tag = self.ir_view.getTag(pattern) orelse return false;
        return switch (pattern_tag) {
            .lit_string => self.stringPatternCanMatchType(pattern, type_idx),
            .lit_int => self.intPatternCanMatchType(pattern, type_idx),
            .lit_bool => self.boolPatternCanMatchType(pattern, type_idx),
            .lit_undefined => type_tag == .t_undefined,
            .match_pattern => self.objectPatternCanMatchType(pattern, type_idx),
            .array_pattern => self.arrayPatternCanMatchType(pattern, type_idx),
            else => false,
        };
    }

    fn patternFullyCoversType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        if (pattern == null_node) return type_idx != null_type_idx;
        if (type_idx == null_type_idx) return false;

        const type_tag = self.pool.getTag(type_idx) orelse return false;
        switch (type_tag) {
            .t_union => {
                for (self.pool.getUnionMembers(type_idx)) |member| {
                    if (!self.patternFullyCoversType(pattern, member)) return false;
                }
                return true;
            },
            .t_nullable => {
                return self.patternFullyCoversType(pattern, self.pool.getNullableInner(type_idx)) and
                    self.patternFullyCoversType(pattern, self.pool.idx_undefined);
            },
            else => {},
        }

        const pattern_tag = self.ir_view.getTag(pattern) orelse return false;
        return switch (pattern_tag) {
            .lit_string => self.stringPatternFullyCoversType(pattern, type_idx),
            .lit_int => self.intPatternFullyCoversType(pattern, type_idx),
            .lit_bool => self.boolPatternFullyCoversType(pattern, type_idx),
            .lit_undefined => type_tag == .t_undefined,
            .match_pattern => self.objectPatternFullyCoversType(pattern, type_idx),
            .array_pattern => self.arrayPatternFullyCoversType(pattern, type_idx),
            else => false,
        };
    }

    fn stringPatternCanMatchType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const type_tag = self.pool.getTag(type_idx) orelse return false;
        return switch (type_tag) {
            .t_string => true,
            .t_literal_string => blk: {
                const pattern_str = self.patternString(pattern) orelse break :blk false;
                const data = self.pool.getData(type_idx) orelse break :blk false;
                break :blk std.mem.eql(u8, pattern_str, self.pool.getName(data.a, @truncate(data.b)));
            },
            else => false,
        };
    }

    fn stringPatternFullyCoversType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const type_tag = self.pool.getTag(type_idx) orelse return false;
        if (type_tag != .t_literal_string) return false;
        return self.stringPatternCanMatchType(pattern, type_idx);
    }

    fn intPatternCanMatchType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const type_tag = self.pool.getTag(type_idx) orelse return false;
        return switch (type_tag) {
            .t_number => true,
            .t_literal_number => blk: {
                const pattern_int = self.ir_view.getIntValue(pattern) orelse break :blk false;
                const pattern_i16 = std.math.cast(i16, pattern_int) orelse break :blk false;
                const data = self.pool.getData(type_idx) orelse break :blk false;
                const member_val: i16 = @bitCast(data.a);
                break :blk member_val == pattern_i16;
            },
            else => false,
        };
    }

    fn intPatternFullyCoversType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const type_tag = self.pool.getTag(type_idx) orelse return false;
        if (type_tag != .t_literal_number) return false;
        return self.intPatternCanMatchType(pattern, type_idx);
    }

    fn boolPatternCanMatchType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const type_tag = self.pool.getTag(type_idx) orelse return false;
        return switch (type_tag) {
            .t_boolean => true,
            .t_literal_bool => blk: {
                const pattern_bool = self.ir_view.getBoolValue(pattern) orelse break :blk false;
                const data = self.pool.getData(type_idx) orelse break :blk false;
                break :blk (data.a != 0) == pattern_bool;
            },
            else => false,
        };
    }

    fn boolPatternFullyCoversType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const type_tag = self.pool.getTag(type_idx) orelse return false;
        if (type_tag != .t_literal_bool) return false;
        return self.boolPatternCanMatchType(pattern, type_idx);
    }

    fn objectPatternCanMatchType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        if (self.pool.getTag(type_idx) != .t_record) return false;

        const match_pattern = self.ir_view.getMatchPattern(pattern) orelse return false;
        for (0..match_pattern.props_count) |i| {
            const prop_idx = self.ir_view.getListIndex(match_pattern.props_start, @intCast(i));
            const prop = self.ir_view.getProperty(prop_idx) orelse return false;
            const prop_name = self.patternPropertyName(prop.key) orelse return false;
            const field = self.findRecordField(type_idx, prop_name) orelse return false;
            if (!self.patternCanMatchType(prop.value, field.type_idx)) return false;
        }
        return true;
    }

    fn objectPatternFullyCoversType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        if (self.pool.getTag(type_idx) != .t_record) return false;

        const match_pattern = self.ir_view.getMatchPattern(pattern) orelse return false;
        for (0..match_pattern.props_count) |i| {
            const prop_idx = self.ir_view.getListIndex(match_pattern.props_start, @intCast(i));
            const prop = self.ir_view.getProperty(prop_idx) orelse return false;
            const prop_name = self.patternPropertyName(prop.key) orelse return false;
            const field = self.findRecordField(type_idx, prop_name) orelse return false;
            if (field.optional) return false;
            if (!self.patternFullyCoversType(prop.value, field.type_idx)) return false;
        }
        return true;
    }

    fn arrayPatternCanMatchType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const array = self.ir_view.getArray(pattern) orelse return false;
        const type_tag = self.pool.getTag(type_idx) orelse return false;

        return switch (type_tag) {
            .t_tuple => blk: {
                const members = self.getTupleElements(type_idx);
                if (members.len != array.elements_count) break :blk false;
                for (0..array.elements_count) |i| {
                    const elem_pattern = self.ir_view.getListIndex(array.elements_start, @intCast(i));
                    if (!self.patternCanMatchType(elem_pattern, members[i])) break :blk false;
                }
                break :blk true;
            },
            .t_array => blk: {
                const elem_type = self.pool.getArrayElement(type_idx);
                for (0..array.elements_count) |i| {
                    const elem_pattern = self.ir_view.getListIndex(array.elements_start, @intCast(i));
                    if (!self.patternCanMatchType(elem_pattern, elem_type)) break :blk false;
                }
                break :blk true;
            },
            else => false,
        };
    }

    fn arrayPatternFullyCoversType(self: *const MatchAnalysis, pattern: NodeIndex, type_idx: TypeIndex) bool {
        const array = self.ir_view.getArray(pattern) orelse return false;
        if (self.pool.getTag(type_idx) != .t_tuple) return false;

        const members = self.getTupleElements(type_idx);
        if (members.len != array.elements_count) return false;

        for (0..array.elements_count) |i| {
            const elem_pattern = self.ir_view.getListIndex(array.elements_start, @intCast(i));
            if (!self.patternFullyCoversType(elem_pattern, members[i])) return false;
        }
        return true;
    }

    fn patternString(self: *const MatchAnalysis, pattern: NodeIndex) ?[]const u8 {
        const str_idx = self.ir_view.getStringIdx(pattern) orelse return null;
        return self.ir_view.getString(str_idx);
    }

    fn patternPropertyName(self: *const MatchAnalysis, node: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(node) orelse return null;
        return switch (tag) {
            .lit_string => self.patternString(node),
            else => null,
        };
    }

    fn findRecordField(self: *const MatchAnalysis, record_type: TypeIndex, name: []const u8) ?type_pool_mod.RecordField {
        for (self.pool.getRecordFields(record_type)) |field| {
            const field_name = self.pool.getName(field.name_start, field.name_len);
            if (std.mem.eql(u8, field_name, name)) return field;
        }
        return null;
    }

    fn getTupleElements(self: *const MatchAnalysis, tuple_type: TypeIndex) []const TypeIndex {
        const data = self.pool.getData(tuple_type) orelse return &.{};
        if (self.pool.getTag(tuple_type) != .t_tuple) return &.{};
        const start = data.a;
        const count = data.b;
        if (start + count > self.pool.members.items.len) return &.{};
        return self.pool.members.items[start .. start + count];
    }
};
