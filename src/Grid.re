open Bread;

let parse = lines => {
  let lines = Array.map(String.toCharArray, lines);
  (lines, Array.length(lines), Array.length(lines[0]));
};

let count = (fn, grid) => {
  let n = Array.length(grid);
  let count = ref(0);
  for (i in 0 to n - 1) {
    let m = Array.length(grid[i]);
    for (j in 0 to m - 1) {
      if (fn(grid[i][j])) {
        incr(count);
      };
    };
  };
  count^;
};
