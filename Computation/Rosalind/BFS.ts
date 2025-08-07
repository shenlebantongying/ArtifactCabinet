import { printf } from "@std/fmt/printf";

const fields = Deno.readTextFileSync("./data/rosalind_bfs.txt").trimEnd().split(
  /\s+/,
).map((n) => parseInt(n, 10));

const n = fields[0];
const neighbors = new Map();
for (let i = 1; i <= n; i++) {
  neighbors.set(i, []);
}
for (let i = 2; i < fields.length; i += 2) {
  neighbors.get(fields[i]).push(fields[i + 1]);
}

function bfs(from: number, to: number) {
  if (from === to) {
    return 0;
  }
  let depth = 1;
  let cur_level: Array<number> = neighbors.get(from);
  const visited: Array<number> = [from];

  while (cur_level.length != 0) {
    const next_level: Array<number> = [];
    cur_level.map((i) => {
      const t = neighbors.get(i).filter((x: number) => !visited.includes(x));
      next_level.push(...t);
      visited.push(...t);
    });
    if (cur_level.includes(to)) {
      return depth;
    }
    cur_level = next_level;
    depth += 1;
  }
  return -1;
}

for (let i = 1; i <= n; i++) {
  printf("%d ", bfs(1, i));
}

console.log();
