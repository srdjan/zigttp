// JIT microbench: object creation + property access

function run(iterations) {
    let sum = 0;
    for (let i of range(iterations)) {
        let obj = { a: i, b: i + 1, c: i + 2 };
        sum = sum + obj.a + obj.b + obj.c;
    }
    return sum;
}
