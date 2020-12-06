let part1 = lines => {
  let n = Array.length(lines);
  let count = ref(0);
  for (r in 0 to n - 1) {
    let c = r * 3;
    let lineLength = String.length(lines[r]);
    if (lines[r].[c mod lineLength] === '#') {
      incr(count);
    };
  };
  count^;
};

let part2 = lines => {
  let n = Array.length(lines);

  let s11 = ref(0);
  let s31 = ref(0);
  let s51 = ref(0);
  let s71 = ref(0);
  let s12 = ref(0);

  for (r in 0 to n - 1) {
    let m = String.length(lines[r]);

    if (lines[r].[r * 1 mod m] === '#') {
      incr(s11);
    };

    if (lines[r].[r * 3 mod m] === '#') {
      incr(s31);
    };

    if (lines[r].[r * 5 mod m] === '#') {
      incr(s51);
    };

    if (lines[r].[r * 7 mod m] === '#') {
      incr(s71);
    };

    if (r mod 2 === 0 && lines[r].[r / 2 mod m] === '#') {
      incr(s12);
    };
  };
  let product = s11^ * s31^ * s51^ * s71^ * s12^;
  product;
};

let run = () => {
  let lines = Utils.getInput(3);
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
