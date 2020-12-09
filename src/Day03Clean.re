open Bread;

/*********************
 * Input preparation *
 *********************/

let prepareLine = line => {
  (line, String.length(line));
};

let prepareInput = lines => {
  let lines = Array.map(prepareLine, lines);
  (lines, Array.length(lines));
};

let isTree = (i, line) => {
  let (raw, n) = line;
  raw.[i mod n] === '#';
};

let traverse = (dx, dy, lines) => {
  let n = Array.length(lines);
  let count = ref(0);
  let x = ref(0);
  let y = ref(0);
  while (y^ < n) {
    if (isTree(x^, lines[y^])) {
      incr(count);
    };
    x := x^ + dx;
    y := y^ + dy;
  };
  count^;
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let (data, _) = prepareInput(lines);
  traverse(3, 1, data);
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (data, _) = prepareInput(lines);
  traverse(1, 1, data)
  * traverse(3, 1, data)
  * traverse(5, 1, data)
  * traverse(7, 1, data)
  * traverse(1, 2, data);
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("03");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
