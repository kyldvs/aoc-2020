let part1 = lines => {
  let x = Array.length(lines);
  let valid = ref(0);
  for (i in 0 to x - 1) {
    let s = Scanner.make([lines[i]]);
    let min = Scanner.nextInt(s);
    let max = Scanner.nextInt(s);
    let letter = Scanner.nextString(s).[0];
    let password = Scanner.nextString(s);
    let n = String.length(password);
    let count = ref(0);
    for (j in 0 to n - 1) {
      if (password.[j] === letter) {
        incr(count);
      };
    };
    if (count^ >= min && count^ <= max) {
      incr(valid);
    };
  };
  valid^;
};

let part2 = lines => {
  let x = Array.length(lines);
  let valid = ref(0);
  for (i in 0 to x - 1) {
    let s = Scanner.make([lines[i]]);
    let min = Scanner.nextInt(s) - 1;
    let max = Scanner.nextInt(s) - 1;
    let letter = Scanner.nextString(s).[0];
    let password = Scanner.nextString(s);
    let n = String.length(password);
    let count = ref(0);
    if (min < n && password.[min] === letter) {
      incr(count);
    };
    if (max < n && password.[max] === letter) {
      incr(count);
    };
    if (count^ === 1) {
      incr(valid);
    };
  };
  valid^;
};

let run = () => {
  let lines = Utils.getInput("02");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
