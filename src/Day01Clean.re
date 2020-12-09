open Bread;

let prepareInput = lines => {
  let lines = Array.map(int_of_string, lines);
  (lines, Array.length(lines));
};

let part1 = lines => {
  let (data, n) = prepareInput(lines);
  let ans = ref(-1);
  for (i in 0 to n - 1) {
    for (j in i + 1 to n - 1) {
      if (data[i] + data[j] === 2020) {
        ans := data[i] * data[j];
      };
    };
  };
  ans^;
};

let part2 = lines => {
  let (data, n) = prepareInput(lines);
  let ans = ref(-1);
  for (i in 0 to n - 1) {
    for (j in i + 1 to n - 1) {
      for (k in j + 1 to n - 1) {
        if (data[i] + data[j] + data[k] === 2020) {
          ans := data[i] * data[j] * data[k];
        };
      };
    };
  };
  ans^;
};

let run = () => {
  let lines = Utils.getInput("01");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
