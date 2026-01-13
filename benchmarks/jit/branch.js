// JIT microbench: truthiness and branching

function run(iterations) {
    let count = 0;
    let obj = { a: 1 };
    let str = "x";
    let nan = 0 / 0;
    for (let i of range(iterations)) {
        let v = i & 7;
        let x = (v === 0) ? 0
            : (v === 1) ? null
            : (v === 2) ? undefined
            : (v === 3) ? false
            : (v === 4) ? nan
            : (v === 5) ? str
            : (v === 6) ? obj
            : 1;
        if (x) {
            count = count + 1;
        } else {
            count = count - 1;
        }
    }
    return count;
}
