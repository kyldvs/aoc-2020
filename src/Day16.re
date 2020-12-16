open Bread;

/*********************
 * Input preparation *
 *********************/

module Input = {
  let eachLine = (fn, lines) => {
    let lines = Array.map(fn, lines);
    (lines, Array.length(lines));
  };

  let ints = (lines) => {
    let lines = Array.map(int_of_string, lines);
    (lines, Array.length(lines));
  };

  let groups = (fn, lines) => {
    let groups = Utils.groupInput(lines);
    let data = Array.map(fn, groups);
    (data, Array.length(data));
  };
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let (_data, _n) = Input.ints(lines);
  0;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (_data, _n) = Input.ints(lines);
  0;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("16");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
