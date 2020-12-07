let part1 = lines => {
  let n = Array.length(lines);
  let xs = Array.map(int_of_string, lines);
  let result = ref(-1);
  for (i in 0 to n - 1) {
    for (j in 0 to n - 1) {
      if (xs[i] + xs[j] === 2020) {
        result := xs[i] * xs[j];
      };
    };
  };
  result^;
};

let part2 = lines => {
  let n = Array.length(lines);
  let xs = Array.map(int_of_string, lines);
  let result = ref(-1);
  for (i in 0 to n - 1) {
    for (j in i + 1 to n - 1) {
      for (k in j + 1 to n - 1) {
        if (xs[i] + xs[j] + xs[k] === 2020) {
          result := xs[i] * xs[j] * xs[k];
        };
      };
    };
  };
  result^;
};

let run = () => {
  let lines = Utils.getInput("07");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
