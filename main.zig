const std = @import("std");
const sort = std.sort;
const math = std.math;

const Node = struct {
    weight: usize = 0,
    character: u8 = 0,
    left: ?*Node = null,
    right: ?*Node = null,

    fn isLeaf(self: *Node) bool {
        return self.right == null and self.left == null;
    }

    fn getBranch(self: *Node, code: u8) *Node {
        if (code == 1) {
            return self.right.?;
        } else if (code == 0) {
            return self.left.?;
        }
        @panic("acha ufala");
    }
};

const Code = struct {
    len: u8 = 0,
    seq: u8 = 0,
};

const Huff = struct {
    codes: [256]Code = [_]Code{.{}} ** 256,
    trees: [256]Node = [_]Node{.{}} ** 256,
    treebuf: [256]*Node = [_]*Node{@ptrFromInt(0x0 + @sizeOf(Node))} ** 256,
    source: []const u8,
    codeslen: usize = 0,
    heap: [256]Node = [_]Node{.{}} ** 256,

    fn init(source: []const u8) Huff {
        var self = Huff{ .source = source };
        for (source) |b| {
            self.trees[b].character = b;
            self.trees[b].weight += 1;
        }

        sort.block(Node, self.trees[0..], {}, cmp);
        self.codeslen = blck: {
            for (self.trees[0..], 0..) |*t, i| {
                if (t.weight == 0) break :blck i;
                self.treebuf[i] = t;
            }
            break :blck self.trees.len;
        };

        var num_trees = self.codeslen;
        var heapidx: usize = 0;

        while (num_trees > 1) : ({
            num_trees -= 1;
            heapidx += 1;
        }) {
            const last = self.treebuf[num_trees - 1];
            const second_last = self.treebuf[num_trees - 2];

            const new_tree = Node{
                .left = @ptrCast(last),
                .right = @ptrCast(second_last),
                .weight = last.weight + second_last.weight,
            };

            const insert_point = blck: {
                for (self.treebuf[0..num_trees], 0..) |t, i| {
                    if (t.weight <= new_tree.weight) {
                        if (t.weight == new_tree.weight) {
                            if (t.isLeaf()) {
                                break :blck i;
                            } else {
                                break :blck i + 1;
                            }
                        }
                        break :blck i;
                    }
                }
                break :blck num_trees;
            };
            var endpoint = num_trees;
            while (endpoint > insert_point) : (endpoint -= 1) {
                self.treebuf[endpoint] = self.treebuf[endpoint - 1];
            }
            self.heap[heapidx] = new_tree;
            self.treebuf[insert_point] = &self.heap[heapidx];
        }
        flatten(self.treebuf[0], 0, 0, &self.codes);
        return self;
    }

    fn flatten(tree: *Node, code: u8, len: u8, codes: []Code) void {
        if (tree.left) |branch|
            flatten(branch, code << 1, len + 1, codes);
        if (tree.right) |branch| 
            flatten(branch, code << 1 | 1, len + 1, codes);
        if (tree.isLeaf()) 
            codes[tree.character] = Code{ .len = len, .seq = code };
    }

    fn encode(self: *Huff, output: []u8) []u8 {
        var byteoffset: u8 = 0;
        var acc: u8 = 0;
        var idx: usize = 0;
        for (self.source) |value| {
            const code = self.codes[value];
            const rem: u8 = @intCast(8 - byteoffset);
            if (rem == 0) {
                output[idx] = acc;
            } else if (rem == code.len) {
                acc <<= @intCast(code.len);
                acc |= code.seq;
                output[idx] = acc;
                idx += 1;
                byteoffset = 0;
                acc = 0;
            } else if (rem > code.len) {
                acc <<= @intCast(code.len);
                acc |= code.seq;
                byteoffset += code.len;
            } else if (rem < code.len) {
                acc <<= @intCast(rem);
                acc |= (code.seq >> @intCast(code.len - rem));
                output[idx] = acc;
                idx += 1;
                acc = code.seq & (math.pow(u8, 2, code.len - rem) - 1);
                byteoffset = code.len - rem;
            }
        }
        if (byteoffset != 0) {
            acc <<= @intCast(8 - byteoffset);
            output[idx] = acc;
            idx += 1;
        }
        return output[0..idx];
    }

    fn decode(self: *Huff, input: []const u8, output: []u8) []u8 {
        var outidx: usize = 0;
        var mask: u8 = 128;
        var t = self.treebuf[0];
        for (input) |value| {
            for (1..9) |i| {
                t = t.getBranch((value & mask) >> @intCast(8 - i));
                if (t.isLeaf()) {
                    output[outidx] = t.character;
                    t = self.treebuf[0];
                    outidx += 1;
                }
                mask /= 2;
            }
            mask = 128;
        }
        return output[0..self.source.len];
    }
};

fn cmp(_: void, lhs: Node, rhs: Node) bool {
    if (lhs.weight == rhs.weight) {
        return lhs.character > rhs.character;
    } else return lhs.weight > rhs.weight;
}

pub fn main() !void {
    var huf = Huff.init("no one can stop reggae!!!!!!!!!!!!!!!!!!!!");
    var encoded = [_]u8{0} ** 256;
    const out = huf.encode(encoded[0..]);
    var decoded = [_]u8{0} ** 256;
    const output = huf.decode(out, decoded[0..]);
    std.debug.assert(std.mem.eql(u8, huf.source, output));
}