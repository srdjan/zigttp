// JIT microbench: function + method calls

function add(a, b) { return a + b; }

function runFunc(iterations) {
    let result = 0;
    for (let i of range(iterations)) {
        result = add(result, i);
    }
    return result;
}

function runMethod(iterations) {
    function add(x) { return x + 1; }
    let obj = { add: add };
    let result = 0;
    for (let i of range(iterations)) {
        result = obj.add(i) + result;
    }
    return result;
}
