const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;

fn isSlice(comptime S: type) bool {
    return (@typeInfo(S) == .Pointer and @typeInfo(S).Pointer.size == .Slice);
}

// A graph with vertices holding things of type T
pub fn Graph(comptime T: type) type {
    return struct {
        const Self = @This();

        // mapping of nth vertex to other vertix
        const Edges = struct { tos: ArrayList(u32), vals: ArrayList(T) };

        allocator: Allocator,
        vertices: ArrayList(T),
        // List of from vertex -> Edges
        edges: ArrayList(Edges),

        pub fn initCapacity(allocator: Allocator, vertex_capacity: u32) !Self {
            return .{
                .allocator = allocator,
                .vertices = try ArrayList(T).initCapacity(allocator, @as(usize, vertex_capacity)),
                .edges = try ArrayList(Edges).initCapacity(allocator, @as(usize, vertex_capacity)),
            };
        }

        pub fn init(allocator: Allocator) !Self {
            return try Self.initCapacity(allocator, 0);
        }

        pub fn deinit(self: Self) void {
            self.vertices.deinit();
            for (self.edges.items) |e| {
                e.tos.deinit();
                e.vals.deinit();
            }
            self.edges.deinit();
        }

        pub fn pushVertex(self: *Self, v: T) !u32 {
            try self.vertices.append(v);
            try self.edges.append(.{ .tos = ArrayList(u32).init(self.allocator), .vals = ArrayList(T).init(self.allocator) });
            return @intCast(u32, self.vertices.items.len - 1);
        }

        pub fn connect(self: *Self, vertex_a: u32, vertex_b: u32, edge_val: T) !void {
            try self.edges.items[@as(usize, vertex_a)].tos.append(vertex_b);
            try self.edges.items[@as(usize, vertex_b)].tos.append(vertex_a);

            try self.edges.items[@as(usize, vertex_a)].vals.append(edge_val);
            try self.edges.items[@as(usize, vertex_b)].vals.append(edge_val);
        }

        pub fn edgesFor(self: *Self, vertex_a: u32) []u32 {
            return self.edges.items[@as(usize, vertex_a)].tos.items;
        }

        pub fn edgesAndValsFor(self: *Self, vertex_a: u32) Edges {
            return self.edges.items[@as(usize, vertex_a)];
        }

        pub fn clear(self: *Self) void {
            for (self.edges.items) |e| {
                e.tos.deinit();
                e.vals.deinit();
            }
            self.edges.resize(0) catch @panic("Failed to resize to 0");
            self.vertices.resize(0) catch @panic("Failed to resize to 0");
        }

        const Iterator = struct {
            to_return: ArrayList(NextVertex),

            // Catch loops
            seen: ArrayList(bool),
            graph: *Self,

            const NextVertex = struct {
                vertex: u32,
                parent: u32,
            };

            const ErrLoop = error.Loop;

            pub fn init(allocator: Allocator, graph: *Self, start_vertex: u32) !Iterator {
                var to_return = ArrayList(NextVertex).init(allocator);
                var seen = try ArrayList(bool).initCapacity(allocator, graph.vertices.items.len);
                try seen.appendNTimes(false, graph.vertices.items.len);

                const edges = graph.edgesFor(start_vertex);
                for (edges) |e| {
                    try to_return.append(.{ .parent = start_vertex, .vertex = e });
                }

                return .{
                    .seen = seen,
                    .to_return = to_return,
                    .graph = graph,
                };
            }

            pub fn deinit(self: @This()) void {
                self.to_return.deinit();
                self.seen.deinit();
            }

            pub fn next(self: *@This()) !?NextVertex {
                if (self.to_return.popOrNull()) |val| {
                    if (self.seen.items[val.vertex]) {
                        return ErrLoop;
                    }
                    self.seen.items[val.vertex] = true;

                    const edges = self.graph.edgesFor(val.vertex);
                    try self.to_return.ensureTotalCapacity(self.to_return.items.len + edges.len - 1);
                    for (edges) |e| {
                        if (e == val.parent) {
                            continue;
                        }
                        self.to_return.appendAssumeCapacity(.{ .vertex = e, .parent = val.vertex });
                    }
                    return val;
                }

                return null;
            }
        };

        pub fn depthFirstIterator(self: *Self, start_vertex: u32) !Iterator {
            return Iterator.init(self.allocator, self, start_vertex);
        }

        pub fn isAcyclic(self: *Self) !bool {
            var seen = try ArrayList(bool).initCapacity(self.allocator, self.vertices.items.len);
            defer seen.deinit();
            try seen.appendNTimes(false, self.vertices.items.len);

            for (self.vertices.items) |_, v| {
                if (seen.items[v]) {
                    // We've seen this vertex already while traversing. Skip.
                    continue;
                }

                var it = try self.depthFirstIterator(@intCast(u32, v));
                defer it.deinit();

                while (true) {
                    const maybe_next_vertex = it.next() catch |err| {
                        if (err == Self.Iterator.ErrLoop) {
                            return false;
                        } else {
                            return err;
                        }
                    };

                    if (maybe_next_vertex) |next_vertex| {
                        if (seen.items[next_vertex.vertex]) {
                            // We've already seen this, we have a loop
                            return false;
                        }
                        seen.items[next_vertex.vertex] = true;
                    } else {
                        break;
                    }
                }
            }

            return true;
        }
    };
}

// CHM 92: http://ilan.schnell-web.net/prog/perfect-hash/algo.html

pub fn PerfectHash(comptime GraphT: type, comptime hash_fns: anytype, comptime S: type, comptime strs: []const S) type {
    return struct {
        const Self = @This();
        g: Graph(GraphT),

        pub fn init(allocator: Allocator) !Self {
            const max_tries = 1000;
            var current_try: u32 = 0;
            var g = try Graph(GraphT).initCapacity(allocator, strs.len);
            var vertex_count = strs.len + 1;

            while (current_try < max_tries) : (current_try += 1) {
                vertex_count = strs.len + current_try + 1;

                var count: u32 = 0;
                while (count < vertex_count) : (count += 1) {
                    _ = g.pushVertex(0) catch @panic("Failed to push vertex");
                }

                for (strs) |s, i| {
                    try g.connect(
                        hash_fns.fn1(s) % @intCast(u32, vertex_count),
                        hash_fns.fn2(s) % @intCast(u32, vertex_count),
                        @intCast(GraphT, i),
                    );
                }

                if (try g.isAcyclic()) {
                    break;
                } else {
                    g.clear();
                }
            }

            if (g.vertices.items.len == 0) {
                std.debug.print("Vertex count {}", .{vertex_count});
                @panic("Failed to find graph");
            }

            // TODO: Assign values to each vertex such that, for each edge, you can add
            // the values for the two vertices and get the desired (hash) value for that
            // edge. This task is easy, because the graph is acyclic. This is done by
            // picking a vertex, and assigning it a value of 0. Then do a depth-first
            // search, assigning values to new vertices so that they sum up properly.

            var seen = try ArrayList(bool).initCapacity(allocator, g.vertices.items.len);
            defer seen.deinit();
            try seen.appendNTimes(false, g.vertices.items.len);

            for (g.vertices.items) |_, v| {
                if (seen.items[v]) {
                    // We've seen this vertex already while traversing. Skip.
                    continue;
                }
                seen.items[v] = true;

                var it = try g.depthFirstIterator(@intCast(u32, v));
                defer it.deinit();

                while (try it.next()) |next_vertex| {
                    if (seen.items[next_vertex.vertex]) {
                        // We shouldn't hit loops
                        @panic("Found loop");
                    }
                    seen.items[next_vertex.vertex] = true;

                    // Our value should be equal to our str_count+edge - parent_val.
                    // This way parent_val + our_val % str_count = edge. and we don't underflow.
                    const parent_val = g.vertices.items[next_vertex.parent];
                    const edge_val = eval: {
                        const edges = g.edgesAndValsFor(next_vertex.vertex);
                        for (edges.tos.items) |e, i| {
                            if (e == next_vertex.parent) {
                                break :eval edges.vals.items[i];
                            }
                        }
                        @panic("unreachable");
                    };

                    const our_val = ((@intCast(u32, strs.len)) + edge_val) - parent_val;
                    g.vertices.items[next_vertex.vertex] = @intCast(GraphT, our_val % @intCast(u32, strs.len));
                }
            }

            return .{
                .g = g,
            };
        }

        pub fn deinit(self: Self) void {
            self.g.deinit();
        }

        pub fn hash(self: *Self, s: S) GraphT {
            const vertex_count = @intCast(u32, self.g.vertices.items.len);

            const h1 = hash_fns.fn1(s) % vertex_count;
            const h2 = hash_fns.fn2(s) % vertex_count;

            const v1 = self.g.vertices.items[h1];
            const v2 = self.g.vertices.items[h2];

            return @intCast(GraphT, (v1 +% v2) % @intCast(u32, strs.len));
        }
    };
}

pub fn PerfectHashStatic(comptime GraphT: type, comptime vertices: []const GraphT, comptime hash_fns: anytype, comptime S: type, comptime strs: []const S) type {
    const is_slice = isSlice(S);
    return struct {
        const Self = @This();
        pub fn hash(s: S) GraphT {
            if (std.debug.runtime_safety) {
                var found = false;
                for (strs) |known_s| {
                    var cond = blk: {
                        if (is_slice) {
                            break :blk std.mem.eql(u8, known_s, s);
                        } else {
                            break :blk known_s == s;
                        }
                    };

                    if (cond) {
                        found = true;
                        break;
                    }
                }
                std.debug.assert(found);
            }

            const vertex_count = @intCast(u32, vertices.len);

            const h1 = hash_fns.fn1(s) % vertex_count;
            const h2 = hash_fns.fn2(s) % vertex_count;

            const v1 = vertices[h1];
            const v2 = vertices[h2];

            return @truncate(GraphT, (v1 +% v2));
        }
    };
}

pub fn HashFns(comptime seed: u32, comptime S: type, comptime strs: []const S) type {
    const is_slice = isSlice(S);
    const max_byte_len = max: {
        if (is_slice) {
            var max_so_far = 0;
            for (strs) |s| {
                if (s.len > max_so_far) {
                    max_so_far = s.len;
                }
            }
            break :max max_so_far;
        } else {
            break :max 0;
        }
    };

    comptime var rand_state = std.rand.Xoroshiro128.init(seed);
    const rng = &rand_state.random();
    const mask = mask: {
        if (is_slice) {
            var mask_mut = [_]u32{0} ** max_byte_len;
            for (mask_mut) |*m| {
                m.* = rng.int(u32);
            }
            break :mask mask_mut;
        } else {
            break :mask rng.int(S);
        }
    };
    const mask2 = mask: {
        if (is_slice) {
            var mask_mut = [_]u32{0} ** max_byte_len;
            for (mask_mut) |*m| {
                m.* = rng.int(u32);
            }
            break :mask mask_mut;
        } else {
            break :mask rng.int(S);
        }
    };

    return struct {
        inline fn fn1(in: S) u32 {
            if (is_slice) {
                var out: u32 = 0;
                for (in) |b, i| {
                    out +%= b *% mask[i];
                }
                return out;
            } else {
                return in +% mask;
            }
        }
        inline fn fn2(in: S) u32 {
            if (is_slice) {
                var out: u32 = 0;
                for (in) |b, i| {
                    out +%= b *% mask2[i];
                }
                return out;
            } else {
                return in +% mask2;
            }
        }
    };
}

test "HashFns" {
    _ = HashFns(2, []const u8, &[_][]const u8{"Hello"});
}

test "bench hash fns" {
    const b256 = @import("./b256.zig").alphabet;

    comptime var tries = 20;
    var min_seed: usize = 0;
    var min_time: usize = 10000000;
    var failures: usize = 0;
    inline while (tries > 0) : (tries -= 1) {
        @setEvalBranchQuota(100_000);
        const hash_fns = HashFns(tries, []const u8, &b256);

        {
            const now = try std.time.Instant.now();
            defer {
                const elapsed = (std.time.Instant.now() catch @panic("can't read time")).since(now);
                if (elapsed < min_time) {
                    min_time = elapsed;
                    min_seed = tries;
                }
                std.debug.print("Elapsed: {}\n", .{elapsed});
            }

            var runs: u32 = 1000;
            while (runs > 0) : (runs -= 1) {
                for (b256) |emoji| {
                    const res = hash_fns.fn1(emoji);
                    const res2 = hash_fns.fn2(emoji);
                    failures ^= res ^ res2;
                }
            }
        }
    }
    std.debug.print("some data so that the compiler doen't optimize the result: {}\n", .{failures});
}

test "graph" {
    const allocator = std.testing.allocator;
    var g = try Graph(u32).init(allocator);
    defer g.deinit();
    const v1 = try g.pushVertex(1);
    const v2 = try g.pushVertex(2);
    const v3 = try g.pushVertex(3);
    try g.connect(v1, v2, 0);
    try std.testing.expectEqualSlices(u32, g.edgesFor(v1), &[_]u32{v2});
    try g.connect(v3, v2, 0);
    try std.testing.expectEqualSlices(u32, g.edgesFor(v2), &[_]u32{ v1, v3 });
    try std.testing.expectEqualSlices(u32, g.edgesFor(v3), &[_]u32{v2});
}

test "graph iterator" {
    const allocator = std.testing.allocator;
    const expectEqual = std.testing.expectEqual;
    var g = try Graph(u32).init(allocator);
    defer g.deinit();
    _ = try g.pushVertex(0);
    const v1 = try g.pushVertex(1);
    const v2 = try g.pushVertex(2);
    const v3 = try g.pushVertex(3);
    const v4 = try g.pushVertex(4);
    const v5 = try g.pushVertex(5);
    try g.connect(v1, v2, 0);
    try g.connect(v2, v3, 0);
    try g.connect(v2, v4, 0);
    try g.connect(v3, v5, 0);

    var it = try g.depthFirstIterator(v1);
    defer it.deinit();

    // TODO this is wrong?
    try expectEqual(v2, (try it.next()).?.vertex);
    try expectEqual(v4, (try it.next()).?.vertex);
    try expectEqual(v3, (try it.next()).?.vertex);
    try expectEqual(v5, (try it.next()).?.vertex);
    try std.testing.expect((try it.next()) == null);
    try std.testing.expect(try g.isAcyclic());
}

test "graph iterator with loop" {
    const allocator = std.testing.allocator;
    var g = try Graph(u32).init(allocator);
    defer g.deinit();
    const v1 = try g.pushVertex(1);
    const v2 = try g.pushVertex(2);
    const v3 = try g.pushVertex(3);
    try g.connect(v1, v2, 0);
    try g.connect(v2, v3, 0);
    try g.connect(v3, v1, 0);
    try std.testing.expect(!(try g.isAcyclic()));

    var it = try g.depthFirstIterator(v1);
    defer it.deinit();

    _ = try it.next();
    _ = try it.next();
    _ = try it.next();
    try std.testing.expectError(Graph(u32).Iterator.ErrLoop, it.next());
}

test "perfect hash" {
    const allocator = std.testing.allocator;

    const strs = &[_][]const u8{
        "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
    };
    const hash_fns = HashFns(1, []const u8, strs);
    var ph = try PerfectHash(u32, hash_fns, []const u8, strs).init(allocator);
    defer ph.deinit();

    const expectEqual = std.testing.expectEqual;
    try expectEqual(@as(u32, 0), ph.hash("zero"));
    try expectEqual(@as(u32, 1), ph.hash("one"));
    try expectEqual(@as(u32, 2), ph.hash("two"));
    try expectEqual(@as(u32, 3), ph.hash("three"));
    try expectEqual(@as(u32, 4), ph.hash("four"));
    try expectEqual(@as(u32, 5), ph.hash("five"));
}

test "perfect hash 256" {
    const allocator = std.testing.allocator;

    const b256 = @import("./b256.zig").alphabet;

    const hash_fns = HashFns(796, []const u8, &b256);
    var ph = try PerfectHash(u32, hash_fns, []const u8, &b256).init(allocator);
    defer ph.deinit();

    const expectEqual = std.testing.expectEqual;
    std.debug.print("\nVertex count {any}\n", .{ph.g.vertices.items.len});
    for (b256) |emoji, i| {
        try expectEqual(@intCast(u32, i), ph.hash(emoji));
    }
}

test "perfect hash codepoint 256" {
    const allocator = std.testing.allocator;

    const b256 = @import("./b256.zig").codepoint_alphabet;

    const hash_fns = HashFns(796, u21, &b256);
    var ph = try PerfectHash(u32, hash_fns, u21, &b256).init(allocator);
    defer ph.deinit();

    std.debug.print("\nVertex count {any}\n", .{ph.g.vertices.items.len});

    const expectEqual = std.testing.expectEqual;
    for (b256) |emoji, i| {
        try expectEqual(@intCast(u32, i), ph.hash(emoji));
    }
}
