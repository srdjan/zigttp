// JIT microbench: arithmetic + bitwise + simple float path

function run(iterations) {
    let sum = 0;
    let acc = 1;
    let f = 0.25;
    for (let i of range(iterations)) {
        sum = sum + i;
        sum = sum - (i & 255);
        sum = sum ^ acc;
        sum = sum & 0x7fffffff;
        acc = (acc + 3) & 0xffff;
        f = f + 0.125;
        if (f > 1000.0) {
            f = f - 1000.0;
        }
    }
    return sum + acc;
}
