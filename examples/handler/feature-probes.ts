// Focused runtime feature probes for the documented JS subset.

function spreadProbe() {
  const base = { a: 1, b: 2 };
  const obj = { ...base, c: 3 };
  const arr = [0, ...[1, 2], 3];
  const empty = [...[]];
  const emptyThenValue = [...[], 1];
  const valueThenEmpty = [0, ...[]];
  return Response.json({ obj: obj, arr: arr, empty: empty, emptyThenValue: emptyThenValue, valueThenEmpty: valueThenEmpty });
}

function rangeProbe() {
  let seen = "";
  let total = 0;

  for (const i of range(8)) {
    if (i === 1) continue;
    if (i === 5) break;
    seen = seen + i;
    total = total + i;
  }

  return Response.json({ seen: seen, total: total });
}

function optionalProbe() {
  const users = {
    active: { profile: { name: "Ada" } },
    empty: {},
  };

  const active = users.active?.profile?.name ?? "missing";
  const empty = users.empty?.profile?.name ?? "missing";
  const absent = users.absent?.profile?.name ?? "missing";

  return Response.json({ active: active, empty: empty, absent: absent });
}

function assertProbe(req) {
  assert req.headers.authorization !== undefined, Response.json({ error: "authorization required" }, { status: 400 });
  return Response.json({ authorization: req.headers.authorization });
}

function makeAdder(base) {
  return (value) => value + base;
}

function closureProbe() {
  const addSeven = makeAdder(7);
  return Response.json({ value: addSeven(5) });
}

function factorialTail(n, acc) {
  if (n === 0) {
    return acc;
  }
  return factorialTail(n - 1, acc * n);
}

function tailProbe() {
  return Response.json({ value: factorialTail(5, 1) });
}

function helpersProbe() {
  const obj = { a: 1, b: 2 };
  const values = Object.values(obj);
  const entries = Object.entries(obj);

  const nums = [-1.8, -1.2, 3.6];
  const floors = nums.map((n) => Math.floor(n));
  const rounded = nums.map((n) => Math.round(n));
  const abs = Math.abs(nums[0]);
  const min = Math.min(4, 9);
  const max = Math.max(4, 9);

  const forEachBox = { total: 0 };
  [1, 2, 3].forEach((n) => {
    forEachBox.total = forEachBox.total + n;
  });

  return Response.json({
    values: values,
    entries: entries,
    floors: floors,
    rounded: rounded,
    abs: abs,
    min: min,
    max: max,
    forEachTotal: forEachBox.total,
  });
}

function handler(req) {
  if (req.url === "/spread") return spreadProbe();
  if (req.url === "/range") return rangeProbe();
  if (req.url === "/optional") return optionalProbe();
  if (req.url === "/assert") return assertProbe(req);
  if (req.url === "/closure") return closureProbe();
  if (req.url === "/tail") return tailProbe();
  if (req.url === "/helpers") return helpersProbe();

  return Response.json({ error: "not found" }, { status: 404 });
}
