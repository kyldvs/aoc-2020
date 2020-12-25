open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let data = Input.ints(lines);
  data;
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let data =
    String.toCharArray(lines[0])
    |> Array.map(c => int_of_char(c) - int_of_char('0'))
    |> Array.to_list;

  let getDest = (x, rest) => {
    // printf("dest: %d\n", x);
    let s = IntSet.fromList(rest);
    let curr = ref(x - 1);
    // printf("Dest? %s\n", Print.intList(rest));
    while (!IntSet.has(curr^, s)) {
      // printf("dest\n");
      // flush_all();
      decr(curr);
      if (curr^ <= 0) {
        curr := 9;
      };
    };
    curr^;
  };

  let splitOn = (x: int, rest: list(int)) => {
    let left = ref([]);
    let curr = ref(rest);
    while (List.hd(curr^) !== x) {
      // printf("split\n");
      flush_all();
      left := left^ @ [List.hd(curr^)];
      curr := List.tl(curr^);
    };
    (left^, List.tl(curr^));
  };

  let rot = ref(0);
  let unrot = l => {
    let curr = ref(l);
    for (i in 0 to rot^ - 1) {
      let last = curr^ |> List.rev |> List.hd;
      let b = curr^ |> List.rev |> List.tl |> List.rev;
      curr := [last, ...b];
    };
    curr^;
  };

  // let toString = l => String.join("", List.map(Print.int, l));

  let play = list => {
    ();
    switch (list) {
    | [curr, a, b, c, ...rest] =>
      let dest = getDest(curr, rest);
      let (left, right) = splitOn(dest, rest);
      let next = left @ [dest, a, b, c] @ right @ [curr];
      // printf("  %s -> %s\n", toString(list), toString(next));
      incr(rot);
      next;
    | _ => failwith("invalid")
    };
  };

  let curr = ref(data);
  for (i in 0 to 99) {
    curr := play(curr^);
  };

  let curr = unrot(curr^);

  let curr = List.map(Print.int, curr);
  int_of_string(String.join("", curr));
};

/*******************
 * Part 2 Solution *
 *******************/

let debug = false;

let part2 = lines => {
  printf("Part 2 Start!\n");
  flush_all();

  let data =
    String.toCharArray(lines[0])
    |> Array.map(c => int_of_char(c) - int_of_char('0'));

  // Lazy way to find max.
  let toAdd = 1000000 - 9;
  let start = ref(10);
  for (i in 0 to toAdd - 1) {
    incr(start);
  };
  let max = start^ - 1;

  // let arr = [| 3, 9, 8, 2, 5, 4, 7, 1, 6 |];
  // let arr = [|3, 8, 9, 1, 2, 5, 4, 6, 7|];
  let arr = data;

  let n = Array.length(arr);

  let pos = (value, arr) => {
    let n = Array.length(arr);
    let res = ref(-1);
    for (i in 0 to n - 1) {
      if (arr[i] === value) {
        res := i;
      };
    };
    res^;
  };

  // Anything above this is "virtual"
  let low = 10;

  printf("Part 2 Pre memo!\n");
  flush_all();

  // module Memo = {
  //   let c = Array.init(max + 1, _ => (-1));
  //   let d = Array.init(max + 1, _ => (-1));
  //   let r1 = Array.init(max + 1, _ => Array.init(max + 1, _ => (-1)));
  //   let r2 = Array.init(max + 1, _ => Array.init(max + 1, _ => (-1)));
  //   let r3 = Array.init(max + 1, _ => Array.init(max + 1, _ => (-1)));
  //   let r4 = Array.init(max + 1, _ => Array.init(max + 1, _ => (-1)));
  //   let l1 = Array.init(max + 1, _ => Array.init(max + 1, _ => (-1)));
  // };

  module Memo = {
    let c: ref(IntMap.t(int)) = ref(IntMap.empty);
    let d: ref(IntMap.t(int)) = ref(IntMap.empty);
    let r1: ref(IntMap.t(IntMap.t(int))) = ref(IntMap.empty);
    let r2: ref(IntMap.t(IntMap.t(int))) = ref(IntMap.empty);
    let r3: ref(IntMap.t(IntMap.t(int))) = ref(IntMap.empty);
    let r4: ref(IntMap.t(IntMap.t(int))) = ref(IntMap.empty);
    let l1: ref(IntMap.t(IntMap.t(int))) = ref(IntMap.empty);
  };

  let proCounter = ref(0);
  let lastP = ref(0);
  let progress = () => {
    incr(proCounter);
    if (proCounter^ - lastP^ > 100000) {
      lastP := proCounter^;
      printf("> progress %d\n", proCounter^);
      flush_all();
    };
  };

  let hasMemo1 = (memo, i) => {
    IntMap.hasKey(i, memo^);
  };
  let getMemo1 = (memo, i) => {
    IntMap.getExn(i, memo^);
  };
  let setMemo1 = (memo, i, value) => {
    progress();
    memo := IntMap.set(i, value, memo^);
  };
  let hasMemo2 = (memo, i, j) => {
    IntMap.hasKey(i, memo^) && IntMap.hasKey(j, IntMap.getExn(i, memo^));
  };
  let getMemo2 = (memo: ref(IntMap.t(IntMap.t(int))), i: int, j: int) => {
    IntMap.getExn(j, IntMap.getExn(i, memo^));
  };
  let setMemo2 = (memo, i, j, value) => {
    progress();
    let next =
      if (IntMap.hasKey(i, memo^)) {
        IntMap.getExn(i, memo^);
      } else {
        IntMap.empty;
      };
    let next = IntMap.set(j, value, next);
    memo := IntMap.set(i, next, memo^);
  };

  printf("Part 2 Post memo!\n");
  flush_all();

  // Current on turn i
  let rec c = i => {
    // if (i > 0 && i mod 100000 === 0) {
    //   printf("  c(%d)\n", i);
    //   flush_all();
    //   ();
    //   ();
    // };
    // if (debug) {
    //   printf("  c(%d) = ?\n", i);
    //   flush_all();
    // };
    // Confidence: High
    let memo = Memo.c;
    let ans =
      if (hasMemo1(memo, i)) {
        getMemo1(memo, i);
      } else {
        let res =
          if (i === 0) {
            arr[0];
          } else {
            r4(c(i - 1), i - 1);
          };
        setMemo1(memo, i, res);
        res;
      };
    // if (debug) {
    //   printf("  c(%d) = %d\n", i, ans);
    //   flush_all();
    // };
    ans;
  }
  // Destination on turn i
  and d = i => {
    // if (debug) {
    //   printf("  d(%d) = ?\n", i);
    //   flush_all();
    // };
    // Confidence: High
    let memo = Memo.d;
    let ans =
      if (hasMemo1(memo, i)) {
        getMemo1(memo, i);
      } else {
        let r1 = r1(c(i), i);
        let r2 = r2(c(i), i);
        let r3 = r3(c(i), i);
        let x = ref(c(i) - 1);
        while (x^ === r1 || x^ === r2 || x^ === r3) {
          decr(x);
          if (x^ === 0) {
            x := max;
          };
        };
        let res = x^;
        setMemo1(memo, i, res);
        res;
      };
    // if (debug) {
    //   printf("  d(%d) = %d\n", i, ans);
    //   flush_all();
    // };
    ans;
  }
  // Value 1 right of x on turn i
  and r1 = (x, i) => {
    // if (debug) {
    //   printf("  r1(%d, %d) = ?\n", x, i);
    //   flush_all();
    // };
    let memo = Memo.r1;
    // Confidence: High
    let ans =
      if (hasMemo2(memo, x, i)) {
        getMemo2(memo, x, i);
      } else {
        let res =
          if (i === 0) {
            // Compute the correct answers for turn 0.
            if (x >= low) {
              if (x === max) {
                arr[0];
              } else {
                x + 1;
              };
            } else {
              let pos = pos(x, arr);
              // if (debug) {
              //   printf("    pos(%d, arr) = %d\n", i, pos);
              // };
              // // 1 from r1
              let next = pos + 1;
              if (next >= n) {
                low + (n - next);
              } else {
                arr[next];
              };
            };
          } else if (x === c(i - 1)) {
            // r1 of the curr from last round changes
            r4(c(i - 1), i - 1);
          } else if (x === r3(c(i - 1), i - 1)) {
            // r1 of the r3 from last round changes
            r1(d(i - 1), i - 1);
          } else if (x === d(i - 1)) {
            // r1 from the dest of last round changes
            r1(c(i - 1), i - 1);
          } else {
            // otherwise r1 didn't change
            r1(x, i - 1);
          };
        setMemo2(memo, x, i, res);
        res;
      };
    // if (debug) {
    //   printf("  r1(%d, %d) = %d\n", x, i, ans);
    //   flush_all();
    // };
    ans;
  }
  and r2 = (x, i) => {
    // if (debug) {
    //   printf("  r2(%d, %d) = ?\n", x, i);
    //   flush_all();
    // };
    let memo = Memo.r2;
    let ans =
      if (hasMemo2(memo, x, i)) {
        getMemo2(memo, x, i);
      } else {
        let res = r1(r1(x, i), i);
        setMemo2(memo, x, i, res);
        res;
      };
    // if (debug) {
    //   printf("  r2(%d, %d) = %d\n", x, i, ans);
    //   flush_all();
    // };
    ans;
  }
  and r3 = (x, i) => {
    // if (debug) {
    //   printf("  r3(%d, %d) = ?\n", x, i);
    //   flush_all();
    // };
    let memo = Memo.r3;
    let ans =
      if (hasMemo2(memo, x, i)) {
        getMemo2(memo, x, i);
      } else {
        let res = r1(r1(r1(x, i), i), i);
        setMemo2(memo, x, i, res);
        res;
      };
    // if (debug) {
    //   printf("  r3(%d, %d) = %d\n", x, i, ans);
    //   flush_all();
    // };
    ans;
  }
  and r4 = (x, i) => {
    // if (debug) {
    //   printf("  r4(%d, %d) = ?\n", x, i);
    //   flush_all();
    // };
    let memo = Memo.r4;
    let ans =
      if (hasMemo2(memo, x, i)) {
        getMemo2(memo, x, i);
      } else {
        let res = r1(r1(r1(r1(x, i), i), i), i);
        setMemo2(memo, x, i, res);
        res;
      };
    // if (debug) {
    //   printf("  r4(%d, %d) = %d\n", x, i, ans);
    //   flush_all();
    // };
    ans;
  }
  and l1 = (x, i) => {
    // printf("l1(%d, %d)\n", x, i);
    // flush_all();
    // Confidence: High
    let memo = Memo.l1;
    if (hasMemo2(memo, x, i)) {
      getMemo2(memo, x, i);
    } else {
      let res =
        if (i === 0) {
          // Compute the correct answers for turn 0.
          if (x >= low) {
            if (x === low) {
              arr[n - 1];
            } else {
              x - 1;
            };
          } else {
            let pos = pos(i, arr);
            // 1 from r1
            let next = pos - 1;
            if (next <= 0) {
              max + next;
            } else {
              arr[next];
            };
          };
        } else if (l1(x, i - 1) === c(i - 1)) {
          d(i - 1);
        } else if (l1(x, i - 1) === r3(c(i - 1), i - 1)) {
          c(i - 1);
        } else if (l1(x, i - 1) === d(i - 1)) {
          r3(c(i - 1), i - 1);
        } else {
          // otherwise r1 didn't change
          r1(x, i - 1);
        };
      setMemo2(memo, x, i, res);
      res;
    };
  };

  // Maybe need this?
  let _ = l1;

  // printf("c(0) = %d\n", c(0));
  // printf("c(1) = %d\n", c(1));
  // printf("c(2000) = %d\n", c(20000));

  let turn = 1000000;
  let ans = r1(1, turn) * r2(1, turn);
  // let ans = (-1);
  ans;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("23.test");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
