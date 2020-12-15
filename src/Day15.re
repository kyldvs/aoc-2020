open Bread;

/*********************
 * Input preparation *
 *********************/

module Input = {
  let eachLine = (fn, lines) => {
    let lines = Array.map(fn, lines);
    (lines, Array.length(lines));
  };

  let ints = lines => {
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
  let (data, n) = Input.ints(String.split(",", lines[0]) |> Array.of_list);
  let mem = ref(IntMap.empty);
  let last = ref(0);
  for (i in 0 to 2019) {
    if (i >= n) {
      let say = last^;
      let prev = IntMap.hasKey(say, mem^) ? IntMap.getExn(say, mem^) : [];
      let say =
        switch (prev) {
        | [] => 0
        | [a] => 0
        | [a, b, ...rest] =>
          // Printf.printf("  prev for m[%d] is %d - %d, len\n", say, a, b);
          a - b
        };
      let prev = IntMap.hasKey(say, mem^) ? IntMap.getExn(say, mem^) : [];
      mem := IntMap.set(say, [i, ...prev], mem^);
      // Printf.printf("m[%d] append %d\n", say, i);
      last := say;
    } else {
      let say = data[i];
      let prev = IntMap.hasKey(say, mem^) ? IntMap.getExn(say, mem^) : [];
      mem := IntMap.set(say, [i, ...prev], mem^);
      // Printf.printf("m[%d] append %d\n", say, i);
      last := say;
      ();
    };
  };
  last^;
};

/*******************
 * Part 2 Solution *
 *******************/

// TODO: This takes about 1 minute to run.
let part2 = lines => {
  let (data, n) = Input.ints(String.split(",", lines[0]) |> Array.of_list);
  let mem = ref(IntMap.empty);
  let last = ref(0);
  for (i in 0 to 30000000 - 1) {
    if (i >= n) {
      let say = last^;
      let prev = IntMap.hasKey(say, mem^) ? IntMap.getExn(say, mem^) : [];
      let say =
        switch (prev) {
        | [] => 0
        | [a] => 0
        | [a, b, ...rest] =>
          // Printf.printf("  prev for m[%d] is %d - %d, len\n", say, a, b);
          a - b
        };
      let prev = IntMap.hasKey(say, mem^) ? IntMap.getExn(say, mem^) : [];
      mem := IntMap.set(say, [i, ...prev], mem^);
      // Printf.printf("m[%d] append %d\n", say, i);
      last := say;
    } else {
      let say = data[i];
      let prev = IntMap.hasKey(say, mem^) ? IntMap.getExn(say, mem^) : [];
      mem := IntMap.set(say, [i, ...prev], mem^);
      // Printf.printf("m[%d] append %d\n", say, i);
      last := say;
      ();
    };
  };
  last^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("15");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
