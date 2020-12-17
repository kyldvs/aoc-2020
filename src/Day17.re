open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let data = Input.ints(lines);
  data;
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let data = prepareLines(lines);
  let n = Array.length(data);
  let sum = ref(0);
  for (i in 0 to n - 1) {
    sum := sum^ + data[i];
  };
  sum^;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let data = prepareLines(lines);
  let n = Array.length(data);
  let sum = ref(0);
  for (i in 0 to n - 1) {
    sum := sum^ + data[i];
  };
  sum^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("17");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
