// JIT microbench: Math builtins (int + mixed float paths)

function runInt(iterations) {
    let a = 1;
    let b = -2;
    let acc = 0;
    for (let i of range(iterations)) {
        acc = Math.abs(b) + Math.floor(a) + Math.ceil(a) + Math.round(b);
        acc = acc + Math.min(acc, a) + Math.max(acc, b);
        a = (a + 3) | 0;
        b = (b - 5) | 0;
        if (a > 100000) a = -a;
        if (b < -100000) b = -b;
    }
    return acc;
}

function runMixed(iterations) {
    let f = 1.25;
    let g = -2.5;
    let acc = 0.0;
    for (let i of range(iterations)) {
        acc = Math.abs(g) + Math.floor(f) + Math.ceil(g) + Math.round(f);
        acc = acc + Math.min(f, i) + Math.max(g, f);
        f = f + 0.125;
        g = g - 0.5;
        if (f > 1000.0) f = f - 1000.0;
        if (g < -1000.0) g = g + 1000.0;
    }
    return acc;
}
