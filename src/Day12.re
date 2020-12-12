open Bread;

/*********************
 * Input preparation *
 *********************/

type data = {
  dir: char,
  value: int,
};

let prepareLine = line => {
  let dir = line.[0];
  let value = int_of_string(String.sliceToEnd(1, line));
  {dir, value};
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
  let deg = ref(0);
  let x = ref(0);
  let y = ref(0);
  for (i in 0 to n - 1) {
    let {dir, value} = data[i];
    switch (dir) {
    | 'N' => y := y^ + value
    | 'S' => y := y^ - value
    | 'E' => x := x^ + value
    | 'W' => x := x^ - value
    | 'L' => deg := (deg^ + value) mod 360
    | 'R' => deg := (deg^ - value + 360) mod 360
    | 'F' =>
      switch (deg^) {
      | 0 => x := x^ + value
      | 90 => y := y^ + value
      | 180 => x := x^ - value
      | 270 => y := y^ - value
      | _ => failwith("invalid deg")
      }
    | _ => failwith("invalid ins")
    };
  };
  abs(x^) + abs(y^);
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (data, n) = prepareInput(lines);
  let dx = ref(10);
  let dy = ref(1);
  let x = ref(0);
  let y = ref(0);
  let ccw = (deg, dx, dy) => {
    let q =
      if (dx >= 0 && dy >= 0) {
        1;
      } else if (dx <= 0 && dy >= 0) {
        2;
      } else if (dx <= 0 && dy <= 0) {
        3;
      } else if (dx >= 0 && dy <= 0) {
        4;
      } else {
        failwith("unknown quad");
      };
    switch (deg, q) {
    | (0, _) => (dx, dy)
    | (90, 1) => (- dy, dx)
    | (90, 2) => (- dy, dx)
    | (90, 3) => (- dy, dx)
    | (90, 4) => (- dy, dx)
    | (180, 1) => (- dx, - dy)
    | (180, 2) => (- dx, - dy)
    | (180, 3) => (- dx, - dy)
    | (180, 4) => (- dx, - dy)
    | (270, 1) => (dy, - dx)
    | (270, 2) => (dy, - dx)
    | (270, 3) => (dy, - dx)
    | (270, 4) => (dy, - dx)
    | _ => failwith("invalid")
    };
  };
  for (i in 0 to n - 1) {
    let {dir, value} = data[i];
    switch (dir) {
    | 'N' => dy := dy^ + value
    | 'S' => dy := dy^ - value
    | 'E' => dx := dx^ + value
    | 'W' => dx := dx^ - value
    | 'L' =>
      let (ndx, ndy) = ccw(value, dx^, dy^);
      dx := ndx;
      dy := ndy;
    | 'R' =>
      let (ndx, ndy) = ccw(360 - value, dx^, dy^);
      dx := ndx;
      dy := ndy;
    | 'F' =>
      x := x^ + dx^ * value;
      y := y^ + dy^ * value;
    | _ => failwith("invalid ins")
    };
  };
  abs(x^) + abs(y^);
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("12");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
