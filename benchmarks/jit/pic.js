// JIT microbench: hidden-class PIC (monomorphic vs polymorphic)

function runMono(iterations) {
    let sum = 0;
    let obj = { a: 1, b: 2, c: 3 };
    for (let i of range(iterations)) {
        sum = sum + obj.a + obj.b;
        obj.a = i;
    }
    return sum;
}

function runPoly(iterations) {
    let sum = 0;
    for (let i of range(iterations)) {
        let obj = (i & 1) === 0 ? { a: 1, b: 2 } : { a: 1, b: 2, c: 3 };
        sum = sum + obj.a + obj.b;
    }
    return sum;
}
