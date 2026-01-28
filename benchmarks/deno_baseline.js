// Deno baseline benchmark - equivalent to zts benchmark suite
// Run with: deno run benchmarks/deno_baseline.js

const ITERATIONS = 50000;

const benchmarks = [
  {
    name: "intArithmetic",
    iterations: ITERATIONS,
    run: function(iterations) {
      let sum = 0;
      for (let i = 0; i < iterations; i++) {
        sum = (sum + i) % 1000000;
        sum = (sum - (i % 1000) + 1000000) % 1000000;
        sum = (sum * 2) % 1000000;
        sum = (sum >> 1);
      }
      return sum;
    }
  },
  {
    name: "stringConcat",
    iterations: ITERATIONS,
    run: function(iterations) {
      let result = '';
      let resetCount = 0;
      for (let i = 0; i < iterations; i++) {
        result = result + 'x';
        resetCount = resetCount + 1;
        if (resetCount > 1000) {
          result = '';
          resetCount = 0;
        }
      }
      return result.length;
    }
  },
  {
    name: "stringOps",
    iterations: ITERATIONS,
    run: function(iterations) {
      let str = 'The quick brown fox jumps over the lazy dog';
      let count = 0;
      for (let i = 0; i < iterations; i++) {
        count = (count + str.indexOf('fox')) % 1000000;
        count = (count + str.length) % 1000000;
      }
      return count;
    }
  },
  {
    name: "objectCreate",
    iterations: ITERATIONS,
    run: function(iterations) {
      let objects = [];
      for (let i = 0; i < iterations; i++) {
        objects.push({ id: i, name: 'item' });
        if (objects.length > 100) {
          objects = [];
        }
      }
      return objects.length;
    }
  },
  {
    name: "propertyAccess",
    iterations: ITERATIONS,
    run: function(iterations) {
      let obj = { a: 1, b: 2, c: 3, d: 4, e: 5 };
      let sum = 0;
      for (let i = 0; i < iterations; i++) {
        sum = (sum + obj.a + obj.b + obj.c + obj.d + obj.e) % 1000000;
        obj.a = i % 100;
      }
      return sum;
    }
  },
  {
    name: "arrayOps",
    iterations: ITERATIONS,
    run: function(iterations) {
      let arr = [];
      let sum = 0;
      for (let i = 0; i < iterations; i++) {
        arr.push(i % 1000);
        if (arr.length > 100) {
          for (let val of arr) {
            sum = (sum + val) % 1000000;
          }
          arr = [];
        }
      }
      return sum;
    }
  },
  {
    name: "functionCalls",
    iterations: ITERATIONS,
    run: function(iterations) {
      function add(a, b) { return (a + b) % 1000000; }
      function compute(x, y) { return add(x, y); }
      let result = 0;
      for (let i = 0; i < iterations; i++) {
        result = compute(i % 1000, result);
      }
      return result;
    }
  },
  {
    name: "recursion",
    iterations: 25,
    run: function(n) {
      function fib(n) {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
      }
      return fib(n);
    }
  },
  {
    name: "jsonOps",
    iterations: 5000,
    run: function(iterations) {
      let obj = { users: [{ id: 1 }, { id: 2 }] };
      let count = 0;
      for (let i = 0; i < iterations; i++) {
        let json = JSON.stringify(obj);
        let parsed = JSON.parse(json);
        count = (count + parsed.users.length) % 1000000;
      }
      return count;
    }
  },
  {
    name: "gcPressure",
    iterations: ITERATIONS,
    run: function(iterations) {
      let count = 0;
      for (let i = 0; i < iterations; i++) {
        let obj = { a: i % 100, b: 'str' };
        let str = JSON.stringify(obj);
        count = (count + str.length) % 1000000;
      }
      return count;
    }
  },
  {
    name: "httpHandler",
    iterations: 5000,
    run: function(iterations) {
      let responses = 0;
      for (let i = 0; i < iterations; i++) {
        let response = {
          status: 200,
          body: JSON.stringify({ id: i % 100, name: 'User' })
        };
        responses = (responses + response.body.length) % 1000000;
      }
      return responses;
    }
  },
  {
    name: "httpHandlerHeavy",
    iterations: 2000,
    run: function(iterations) {
      let responses = 0;
      let baseHeaders = { 'content-type': 'application/json', 'cache-control': 'no-store' };
      let payload = JSON.stringify({ id: 1, name: 'User1', tags: ['alpha','beta','gamma'] });
      for (let i = 0; i < iterations; i++) {
        let reqPath = (i % 3 === 0) ? '/api/users' : '/api/users/42';
        let query = (i % 2 === 0) ? '?limit=10&offset=5' : '?limit=25&offset=0';
        let limit = (query.indexOf('limit=25') !== -1) ? 25 : 10;
        let offset = (query.indexOf('offset=5') !== -1) ? 5 : 0;
        let bodyObj = null;
        if (reqPath.indexOf('/api/users') === 0) {
          bodyObj = JSON.parse(payload);
          bodyObj.limit = limit;
          bodyObj.offset = offset;
        }
        let headers = {
          'content-type': baseHeaders['content-type'],
          'cache-control': baseHeaders['cache-control'],
          'x-request-id': 'req-' + (i % 1000)
        };
        let response = {
          status: (reqPath === '/api/users') ? 200 : 201,
          headers: headers,
          body: JSON.stringify({ ok: true, path: reqPath, data: bodyObj })
        };
        responses = (responses + response.body.length + response.status) % 1000000;
      }
      return responses;
    }
  },
  {
    name: "forOfLoop",
    iterations: ITERATIONS,
    run: function(iterations) {
      let arr = [];
      for (let i = 0; i < 100; i++) {
        arr.push(i);
      }
      let sum = 0;
      let loops = (iterations / 100) >> 0;
      for (let j = 0; j < loops; j++) {
        for (let val of arr) {
          sum = (sum + val) % 1000000;
        }
      }
      return sum;
    }
  }
];

console.log("");
console.log("=== Deno Baseline Benchmarks ===");
console.log("");

const results = [];
let totalTimeMs = 0;

for (const bench of benchmarks) {
  const start = performance.now();
  bench.run(bench.iterations);
  const end = performance.now();

  const elapsedMs = end - start;
  totalTimeMs += elapsedMs;
  const opsPerSec = Math.round(bench.iterations / (elapsedMs / 1000));

  results.push({
    name: bench.name,
    iterations: bench.iterations,
    time_ms: elapsedMs,
    ops_per_sec: opsPerSec
  });

  console.log(`${bench.name}: ${elapsedMs.toFixed(3)}ms (${opsPerSec} ops/sec)`);
}

console.log("");
console.log(`Total time: ${totalTimeMs.toFixed(1)}ms`);
console.log("");

// Output JSON for comparison
console.log("JSON output:");
console.log(JSON.stringify({ benchmarks: results, total_ms: totalTimeMs }, null, 2));
