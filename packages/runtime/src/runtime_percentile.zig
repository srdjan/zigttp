//! Approximate-percentile latency tracker backed by a fixed-size ring of
//! samples. Mutex-guarded so multiple workers can record concurrently.

const compat = @import("zigts").compat;

pub const PercentileTracker = struct {
    mutex: compat.Mutex = .{},
    samples: [SAMPLE_SIZE]u64 = [_]u64{0} ** SAMPLE_SIZE,
    total: usize = 0,

    const SAMPLE_SIZE = 1024;

    pub fn record(self: *PercentileTracker, ns: u64) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const i = self.total % SAMPLE_SIZE;
        self.samples[i] = ns;
        self.total += 1;
    }

    /// Returns the approximate p-th percentile (0-100) in nanoseconds.
    /// Copies the ring buffer to a stack array and sorts via insertion sort.
    pub fn getPercentile(self: *PercentileTracker, p: f64) u64 {
        self.mutex.lock();
        defer self.mutex.unlock();

        const n = self.total;
        if (n == 0) return 0;
        const active = @min(n, SAMPLE_SIZE);

        var sorted: [SAMPLE_SIZE]u64 = undefined;
        for (0..active) |i| {
            sorted[i] = self.samples[i];
        }

        for (1..active) |i| {
            const key = sorted[i];
            var j: usize = i;
            while (j > 0 and sorted[j - 1] > key) {
                sorted[j] = sorted[j - 1];
                j -= 1;
            }
            sorted[j] = key;
        }

        const rank = @as(usize, @intFromFloat(p / 100.0 * @as(f64, @floatFromInt(active - 1))));
        return sorted[@min(rank, active - 1)];
    }
};
