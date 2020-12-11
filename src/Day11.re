open Bread;

/*********************
 * Input preparation *
 *********************/

let prepareLine = line => {
  let line = String.toCharArray(line);
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
  let m = Array.length(data[0]);

  let copy = () => {
    Array.init(n, i => Array.init(m, j => data[i][j]));
  };

  let paste = (a, b) => {
    let change = ref(false);
    for (i in 0 to n - 1) {
      for (j in 0 to m - 1) {
        if (b[i][j] !== a[i][j]) {
          change := true;
          b[i][j] = a[i][j];
        };
      };
    };
    change^;
  };

  let test = (i, j, arr) => {
    let occ =
      if (i >= 0 && j >= 0 && i < n && j < m && arr[i][j] === '#') {
        1;
      } else {
        0;
      };
    occ;
  };

  let count = (i, j, arr) => {
    test(i - 1, j - 1, arr)
    + test(i - 1, j, arr)
    + test(i - 1, j + 1, arr)
    + test(i, j - 1, arr)
    + test(i, j + 1, arr)
    + test(i + 1, j - 1, arr)
    + test(i + 1, j, arr)
    + test(i + 1, j + 1, arr);
  };

  let doit = () => {
    let next = copy();
    for (i in 0 to n - 1) {
      for (j in 0 to m - 1) {
        let occ = count(i, j, data);
        if (data[i][j] === 'L' && occ === 0) {
          next[i][j] = '#';
        };
        if (data[i][j] === '#' && occ >= 4) {
          next[i][j] = 'L';
        };
      };
    };
    paste(next, data);
  };
  while (doit()) {
    ();
  };

  let ans = ref(0);
  for (i in 0 to n - 1) {
    for (j in 0 to m - 1) {
      if (data[i][j] === '#') {
        incr(ans);
      };
    };
  };
  ans^;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (data, n) = prepareInput(lines);
  let m = Array.length(data[0]);

  let copy = () => {
    Array.init(n, i => Array.init(m, j => data[i][j]));
  };

  let paste = (a, b) => {
    let change = ref(false);
    for (i in 0 to n - 1) {
      for (j in 0 to m - 1) {
        if (b[i][j] !== a[i][j]) {
          change := true;
          b[i][j] = a[i][j];
        };
      };
    };
    change^;
  };

  let test = (i, j, di, dj, arr) => {
    let occ = ref(0);
    let break = ref(false);
    let i = ref(i);
    let j = ref(j);
    while (!break^) {
      i := i^ + di;
      j := j^ + dj;
      let i = i^;
      let j = j^;
      if (i >= 0 && j >= 0 && i < n && j < m) {
        if (arr[i][j] === 'L') {
          break := true;
        };
        if (arr[i][j] === '#') {
          break := true;
          occ := 1;
        };
      } else {
        break := true;
      };
    };
    occ^;
  };

  let count = (i, j, arr) => {
    test(i, j, -1, -1, arr)
    + test(i, j, -1, 0, arr)
    + test(i, j, -1, 1, arr)
    + test(i, j, 0, -1, arr)
    + test(i, j, 0, 1, arr)
    + test(i, j, 1, -1, arr)
    + test(i, j, 1, 0, arr)
    + test(i, j, 1, 1, arr);
  };

  let doit = () => {
    let next = copy();
    for (i in 0 to n - 1) {
      for (j in 0 to m - 1) {
        let occ = count(i, j, data);
        if (data[i][j] === 'L' && occ === 0) {
          next[i][j] = '#';
        };
        if (data[i][j] === '#' && occ >= 5) {
          next[i][j] = 'L';
        };
      };
    };
    paste(next, data);
  };
  while (doit()) {
    ();
  };

  let ans = ref(0);
  for (i in 0 to n - 1) {
    for (j in 0 to m - 1) {
      if (data[i][j] === '#') {
        incr(ans);
      };
    };
  };
  ans^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("11");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
