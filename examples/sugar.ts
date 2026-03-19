// Syntactic sugar demo

function handler(req) {
  const double = (x) => x * 2;

  let score = 100;
  score += 50;

  const items = [1, 2, 3, 4, 5];
  const evens = items.filter((n) => n % 2 === 0);
  const doubled = evens.map((n) => n * 2);
  const total = doubled.reduce((acc, n) => acc + n, 0);

  const piped = score |> double;

  const keys = Object.keys({a: 1, b: 2});

  return Response.json({
    score: score,
    evens: evens,
    doubled: doubled,
    total: total,
    piped: piped,
    keys: keys
  });
}
