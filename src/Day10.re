open Bread;

/*********************
 * Input preparation *
 *********************/

let prepareLine = line => {
  let line = int_of_string(line);
  line;
};

let prepareInput = lines => {
  let lines = Array.map(prepareLine, lines);
  (lines, Array.length(lines));
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let (data, n) = prepareInput(lines);
  Array.stable_sort(compare, data);
  let one = ref(1);
  let three = ref(1);
  let i = ref(0);
  while (i^ < n - 1) {
    if (data[i^] + 1 === data[i^ + 1]) {
      incr(one);
      incr(i);
    } else if (data[i^] + 3 >= data[i^ + 1]) {
      incr(three);
      incr(i);
    } else {
      failwith("Unreachable: " ++ string_of_int(i^));
    };
  };
  one^ * three^;
};

/*******************
 * Part 2 Solution *
 *******************/

let compatible = (i, j) => {
  i + 1 === j || i + 2 === j || i + 3 == j;
};

let part2 = lines => {
  let (data, n) = prepareInput(lines);
  Array.stable_sort(compare, data);
  let n = n + 1;
  let data = Array.init(n, i => i === 0 ? 0 : data[i - 1]);
  let ways = Array.init(n, _ => 0);
  ways[0] = 1;
  let i = ref(1);
  while (i^ < n) {
    let j = i^ - 1;
    let k = i^ - 2;
    let l = i^ - 3;
    let x = ref(0);
    if (j >= 0 && compatible(data[j], data[i^])) {
      x := x^ + ways[j];
    };
    if (k >= 0 && compatible(data[k], data[i^])) {
      x := x^ + ways[k];
    };
    if (l >= 0 && compatible(data[l], data[i^])) {
      x := x^ + ways[l];
    };
    ways[i^] = x^;
    incr(i);
  };
  ways[n - 1];
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("10");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
