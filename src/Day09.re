open Bread;

/*********************
 * Input preparation *
 *********************/

let prepareLine = line => {
  int_of_string(line);
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
  let i = ref(25);
  let break = ref(false);
  while (! break^) {
    let valid = ref(false);
    for (j in i^ - 25 to i^ - 1) {
      for (k in j + 1 to i^ - 1) {
        if (data[j] + data[k] === data[i^]) {
          valid := true;
        };
      };
    };
    if (valid^) {
      incr(i);
    } else {
      break := true;
    };
  };
  data[i^];
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (data, n) = prepareInput(lines);
  let sum = 50047984;
  let test = (i, j) => {
    let value = ref(0);
    for (x in i to j) {
      value := value^ + data[x];
    };
    value^;
  };
  let min = (i, j) => {
    let value = ref(data[i]);
    for (x in i + 1 to j) {
      if (data[x] < value^) {
        value := data[x];
      };
    };
    value^;
  };
  let max = (i, j) => {
    let value = ref(data[i]);
    for (x in i + 1 to j) {
      if (data[x] > value^) {
        value := data[x];
      };
    };
    value^;
  };
  let ans = ref(-1);

  for (i in 0 to n - 1) {
    for (j in i + 1 to n - 1) {
      if (test(i, j) === sum) {
        ans := min(i, j) + max(i, j);
      }
    }
  }
  ans^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("09");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
