// zts Performance Benchmarks
// Each benchmark runs and outputs results via console.log

// 1. Integer arithmetic
function runIntArithmetic(iterations) {
    let sum = 0;
    for (let i of range(iterations)) {
        sum = (sum + i) % 1000000;
        sum = (sum - (i % 1000) + 1000000) % 1000000;
        sum = (sum * 2) % 1000000;
        sum = (sum >> 1);
    }
    return sum;
}

// 2. String concat
function runStringConcat(iterations) {
    let result = '';
    for (let i of range(iterations)) {
        result = result + 'x';
        if (result.length > 1000) {
            result = '';
        }
    }
    return result.length;
}

// 3. String operations
function runStringOps(iterations) {
    let str = 'The quick brown fox jumps over the lazy dog';
    let count = 0;
    for (let i of range(iterations)) {
        count = (count + str.indexOf('fox')) % 1000000;
        count = (count + str.length) % 1000000;
    }
    return count;
}

// 4. Object creation
function runObjectCreate(iterations) {
    let objects = [];
    for (let i of range(iterations)) {
        objects.push({ id: i, name: 'item' });
        if (objects.length > 100) {
            objects = [];
        }
    }
    return objects.length;
}

// 5. Property access
function runPropertyAccess(iterations) {
    let obj = { a: 1, b: 2, c: 3, d: 4, e: 5 };
    let sum = 0;
    for (let i of range(iterations)) {
        sum = (sum + obj.a + obj.b + obj.c + obj.d + obj.e) % 1000000;
        obj.a = i % 100;
    }
    return sum;
}

// 6. Array operations
function runArrayOps(iterations) {
    let arr = [];
    let sum = 0;
    for (let i of range(iterations)) {
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

// 7. Function calls
function add(a, b) { return (a + b) % 1000000; }
function compute(x, y) { return add(x, y); }

function runFunctionCalls(iterations) {
    let result = 0;
    for (let i of range(iterations)) {
        result = compute(i % 1000, result);
    }
    return result;
}

// 8. Recursive function
function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

function runRecursion(depth) {
    return fib(depth);
}

// 9. JSON operations
function runJsonOps(iterations) {
    let obj = { users: [{ id: 1 }, { id: 2 }] };
    let count = 0;
    for (let i of range(iterations)) {
        let json = JSON.stringify(obj);
        let parsed = JSON.parse(json);
        count = (count + parsed.users.length) % 1000000;
    }
    return count;
}

// 10. GC pressure
function runGcPressure(iterations) {
    let count = 0;
    for (let i of range(iterations)) {
        let obj = { a: i % 100, b: 'str' };
        let str = JSON.stringify(obj);
        count = (count + str.length) % 1000000;
    }
    return count;
}

// 11. HTTP handler simulation
function runHttpHandler(iterations) {
    let responses = 0;
    for (let i of range(iterations)) {
        let response = {
            status: 200,
            body: JSON.stringify({ id: i % 100, name: 'User' })
        };
        responses = (responses + response.body.length) % 1000000;
    }
    return responses;
}

// 12. for-of loop over array
function runForOfLoop(iterations) {
    let arr = [];
    for (let i of range(100)) {
        arr.push(i);
    }
    let sum = 0;
    let loops = (iterations / 100) >> 0;
    for (let j of range(loops)) {
        for (let val of arr) {
            sum = (sum + val) % 1000000;
        }
    }
    return sum;
}

// Run all benchmarks
let ITERATIONS = 50000;
console.log('Running zts benchmarks (' + ITERATIONS + ' iterations each)...');
console.log('');

console.log('intArithmetic: ' + runIntArithmetic(ITERATIONS));
console.log('stringConcat: ' + runStringConcat(ITERATIONS));
console.log('stringOps: ' + runStringOps(ITERATIONS));
console.log('objectCreate: ' + runObjectCreate(ITERATIONS));
console.log('propertyAccess: ' + runPropertyAccess(ITERATIONS));
console.log('arrayOps: ' + runArrayOps(ITERATIONS));
console.log('functionCalls: ' + runFunctionCalls(ITERATIONS));
console.log('recursion(25): ' + runRecursion(25));
console.log('jsonOps: ' + runJsonOps(5000));
console.log('gcPressure: ' + runGcPressure(ITERATIONS));
console.log('httpHandler: ' + runHttpHandler(5000));
console.log('forOfLoop: ' + runForOfLoop(ITERATIONS));

console.log('');
console.log('All benchmarks complete');
