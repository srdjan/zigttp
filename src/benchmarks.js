// MicroQuickJS Performance Benchmarks
// Tests key performance areas for before/after Zig port comparison

var benchmarks = {};

// 1. Integer arithmetic (tests value encoding/decoding)
benchmarks.intArithmetic = function(iterations) {
    var sum = 0;
    for (var i = 0; i < iterations; i++) {
        sum = sum + i;
        sum = sum - (i >> 1);
        sum = sum * 2;
        sum = sum / 2;
        sum = sum % 1000000;
    }
    return sum;
};

// 2. Float arithmetic (tests float value handling)
benchmarks.floatArithmetic = function(iterations) {
    var sum = 0.0;
    for (var i = 0; i < iterations; i++) {
        sum = sum + (i * 0.1);
        sum = sum - (i * 0.05);
        sum = sum * 1.001;
        sum = sum / 1.001;
    }
    return sum;
};

// 3. String concatenation (tests string allocation/interning)
benchmarks.stringConcat = function(iterations) {
    var result = '';
    for (var i = 0; i < iterations; i++) {
        result = result + 'x';
        if (result.length > 1000) {
            result = '';
        }
    }
    return result.length;
};

// 4. String operations (tests string methods)
benchmarks.stringOps = function(iterations) {
    var str = 'The quick brown fox jumps over the lazy dog';
    var count = 0;
    for (var i = 0; i < iterations; i++) {
        count = count + str.indexOf('fox');
        count = count + str.length;
        count = count + str.charAt(i % str.length).charCodeAt(0);
        var upper = str.toUpperCase();
        var lower = str.toLowerCase();
        count = count + upper.length + lower.length;
    }
    return count;
};

// 5. Object creation (tests object allocation)
benchmarks.objectCreate = function(iterations) {
    var objects = [];
    for (var i = 0; i < iterations; i++) {
        objects.push({
            id: i,
            name: 'item' + i,
            value: i * 1.5,
            active: i % 2 === 0
        });
        if (objects.length > 100) {
            objects = [];
        }
    }
    return objects.length;
};

// 6. Property access (tests property lookup/hash table)
benchmarks.propertyAccess = function(iterations) {
    var obj = {
        a: 1, b: 2, c: 3, d: 4, e: 5,
        f: 6, g: 7, h: 8, i: 9, j: 10,
        name: 'test', value: 42, active: true
    };
    var sum = 0;
    for (var i = 0; i < iterations; i++) {
        sum = sum + obj.a + obj.b + obj.c + obj.d + obj.e;
        sum = sum + obj.f + obj.g + obj.h + obj.i + obj.j;
        sum = sum + obj.value;
        obj.a = i % 100;
        obj.value = sum % 1000;
    }
    return sum;
};

// 7. Array operations (tests array allocation and access)
benchmarks.arrayOps = function(iterations) {
    var arr = [];
    var sum = 0;
    for (var i = 0; i < iterations; i++) {
        arr.push(i);
        if (arr.length > 100) {
            for (var j = 0; j < arr.length; j++) {
                sum = sum + arr[j];
            }
            arr = [];
        }
    }
    return sum;
};

// 8. Function calls (tests call stack and argument passing)
benchmarks.functionCalls = function(iterations) {
    function add(a, b) { return a + b; }
    function mul(a, b) { return a * b; }
    function compute(x, y, z) { return add(mul(x, y), z); }

    var result = 0;
    for (var i = 0; i < iterations; i++) {
        result = compute(i, i + 1, result % 1000);
    }
    return result;
};

// 9. Recursive function (tests call stack depth)
benchmarks.recursion = function(depth) {
    function fib(n) {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
    }
    return fib(depth);
};

// 10. Closure creation (tests closure allocation)
benchmarks.closures = function(iterations) {
    var sum = 0;
    for (var i = 0; i < iterations; i++) {
        var adder = (function(x) {
            return function(y) { return x + y; };
        })(i);
        sum = sum + adder(i);
    }
    return sum;
};

// 11. JSON operations (tests serialization)
benchmarks.jsonOps = function(iterations) {
    var obj = {
        users: [
            { id: 1, name: 'Alice', email: 'alice@example.com' },
            { id: 2, name: 'Bob', email: 'bob@example.com' },
            { id: 3, name: 'Charlie', email: 'charlie@example.com' }
        ],
        meta: { total: 3, page: 1 }
    };
    var count = 0;
    for (var i = 0; i < iterations; i++) {
        var json = JSON.stringify(obj);
        var parsed = JSON.parse(json);
        count = count + parsed.users.length + json.length;
    }
    return count;
};

// 12. GC pressure (tests garbage collector)
benchmarks.gcPressure = function(iterations) {
    var count = 0;
    for (var i = 0; i < iterations; i++) {
        // Create many short-lived objects
        var obj = { a: i, b: 'str' + i, c: [1, 2, 3] };
        var arr = [obj, obj, obj];
        var str = JSON.stringify(arr);
        count = count + str.length;
    }
    return count;
};

// 13. HTTP handler simulation (realistic workload)
benchmarks.httpHandler = function(iterations) {
    var responses = 0;
    for (var i = 0; i < iterations; i++) {
        // Simulate request parsing
        var request = {
            method: 'GET',
            url: '/api/users/' + i,
            headers: {
                'content-type': 'application/json',
                'accept': 'application/json',
                'user-agent': 'benchmark/1.0'
            }
        };

        // Simulate response building
        var response = {
            status: 200,
            body: JSON.stringify({
                id: i,
                name: 'User ' + i,
                created: Date.now()
            }),
            headers: {
                'content-type': 'application/json'
            }
        };

        responses = responses + response.body.length;
    }
    return responses;
};

// 14. for-of loop (ES6 feature)
benchmarks.forOfLoop = function(iterations) {
    var arr = [];
    for (var i = 0; i < 100; i++) {
        arr.push(i);
    }

    var sum = 0;
    for (var j = 0; j < iterations / 100; j++) {
        for (var val of arr) {
            sum = sum + val;
        }
    }
    return sum;
};

// 15. TypedArray operations
benchmarks.typedArrays = function(iterations) {
    var buffer = new ArrayBuffer(1024);
    var view = new Uint8Array(buffer);
    var sum = 0;

    for (var i = 0; i < iterations; i++) {
        var idx = i % 1024;
        view[idx] = i % 256;
        sum = sum + view[idx];
    }
    return sum;
};

// Export for use by benchmark runner
globalThis.benchmarks = benchmarks;
globalThis.runBenchmark = function(name, iterations) {
    if (!benchmarks[name]) {
        return { error: 'Unknown benchmark: ' + name };
    }

    var start = performance.now();
    var result = benchmarks[name](iterations);
    var end = performance.now();

    return {
        name: name,
        iterations: iterations,
        result: result,
        timeMs: end - start
    };
};

globalThis.runAllBenchmarks = function(iterations) {
    var results = [];
    var names = Object.keys(benchmarks);

    for (var i = 0; i < names.length; i++) {
        var name = names[i];
        var iters = name === 'recursion' ? 25 : iterations;
        results.push(runBenchmark(name, iters));
    }

    return results;
};
