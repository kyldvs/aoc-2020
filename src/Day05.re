let getRow = s => {
  let min = ref(0);
  let max = ref(127);
  for (i in 0 to 6) {
    let center = min^ + (max^ - min^) / 2;
    if (s.[i] === 'F') {
      max := center;
    } else if (s.[i] === 'B') {
      min := center + 1;
    } else {
      failwith("invalid");
    };
  };
  min^;
};

let getCol = s => {
  let min = ref(0);
  let max = ref(7);
  for (i in 7 to 9) {
    let center = min^ + (max^ - min^) / 2;
    if (s.[i] === 'L') {
      max := center;
    } else if (s.[i] === 'R') {
      min := center + 1;
    } else {
      failwith("invalid");
    };
  };
  min^;
};

let part1 = lines => {
  let n = Array.length(lines);
  let max = ref(-1);
  for (i in 0 to n - 1) {
    let row = getRow(lines[i]);
    let col = getCol(lines[i]);
    let id = row * 8 + col;
    if (id > max^) {
      max := id;
    };
  };

  max^;
};

let part2 = lines => {
  let ids = Array.init(1024, i => false);
  let n = Array.length(lines);
  for (i in 0 to n - 1) {
    let row = getRow(lines[i]);
    let col = getCol(lines[i]);
    let id = row * 8 + col;
    ids[id] = true;
  };
  let n = Array.length(ids);
  let mine = ref(-1);
  for (i in 1 to n - 2) {
    if (!ids[i] && ids[i - 1] && ids[i + 1]) {
      mine := i;
    };
  };
  mine^;
};

let run = () => {
  let lines = Utils.getInput("05");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
