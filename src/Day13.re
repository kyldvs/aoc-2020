open Bread;

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let est = int_of_string(lines[0]);
  let ids =
    lines[1]
    |> String.replaceAll("x,", "")
    |> String.split(",")
    |> Array.of_list;
  let ids = Array.map(int_of_string, ids);
  let n = Array.length(ids);
  let diff = ref(100000000000000000);
  let ans = ref(-1);
  for (i in 0 to n - 1) {
    let id = ids[i];
    let a = est / id;
    let l = (a + 1) * id;
    let rem = l - est;
    let rem = rem === id ? 0 : rem;
    if (rem < diff^) {
      diff := rem;
      ans := id * rem;
    };
  };
  ans^;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  // let ids =
  //   lines[1]
  //   |> String.replaceAll("x,", "-1,")
  //   |> String.split(",")
  //   |> Array.of_list;
  // let ids = Array.map(int_of_string, ids);
  // let ps = Array.mapi((i, id) => (id, i), ids);
  // let ps =
  //   ps
  //   |> Array.to_list
  //   |> List.filter(((id, _)) => id !== (-1))
  //   |> Array.of_list;
  // let n = Array.length(ps);
  // let c1 = t => {
  //   t
  //   mod 7 === 0
  //   && (t + 1)
  //   mod 13 === 0
  //   && (t + 4)
  //   mod 59 === 0
  //   && (t + 6)
  //   mod 31 === 0
  //   && (t + 7)
  //   mod 19 === 0;
  // };
  let c2 = t => {
    t
    mod 41 === 0
    && (t + 35)
    mod 37 === 0
    && (t + 41)
    mod 971 === 0
    && (t + 58)
    mod 17 === 0
    && (t + 59)
    mod 13 === 0
    && (t + 64)
    mod 23 === 0
    && (t + 70)
    mod 29 === 0
    && (t + 72)
    mod 487 === 0
    && (t + 91)
    mod 19 === 0;
  };
  // let check = t => {
  //   let break = ref(false);
  //   let i = ref(0);
  //   while (! break^ && i^ < n) {
  //     let (id, r) = ps[i^];
  //     if ((t + r) mod id !== 0) {
  //       break := true;
  //     };
  //     incr(i);
  //   };
  //   ! break^;
  // };
  let break = ref(false);

  let d = 971;
  let r = 41;
  // let min = 133000000000000;
  // let min = 244000000000000;
  let min = 404510000000000;
  let x = 971;

  // let d = 59;
  // let r = 4;
  // let min = 100;
  // let x = 59;

  let f = min / d;
  let t = ref(f * d - r);
  let ct = ref(0);
  while (! break^) {
    if (c2(t^)) {
      break := true;
    } else {
      t := t^ + x// if (ct^ mod 10000 === 0) {
                 //   print_int(t^);
                 //   print_endline("");
                 ;
                 // };
    };
    incr(ct);
  };
  t^;
};

let asc = (a, b) => (b, a);
let rec extended_euclid = (a, b) =>
  if (b === 0) {
    (1, 0);
  } else {
    let (x, y) = extended_euclid(b, a mod b);
    let k = a / b;
    (y, x - k * y);
  };
let cmr = (n1, r1, n2, r2) => {
  let (x, y) = extended_euclid(n1, n2);
  let m = n1 * n2;
  let n = r2 * x * n1 + r1 * y * n2;
  n mod (m + m) mod m;
};

let mulInv = (a, b) => {
  let b0 = b;
  let a = ref(a);
  let b = ref(b);
  let x0 = ref(0);
  let x1 = ref(1);
  if (b^ === 1) {
    1;
  } else {
    while (a^ > 1) {
      let q = a^ / b^;
      let amb = a^ mod b^;
      a := b^;
      b := amb;
      let xqx = x1^ - q * x0^;
      x1 := x0^;
      x0 := xqx;
    };

    if (x1^ < 0) {
      x1^ + b0;
    } else {
      x1^;
    };
  };
};

let multi_cmr = pairs => {
  let len = Array.length(pairs);
  let prod = Array.fold_left((acc, (x, r)) => acc * x, 1, pairs);
  let p = ref(0);
  let sum = ref(0);
  for (i in 0 to len - 1) {
    let (n, r) = pairs[i];
    p := prod / n;
    sum := sum^ + r * mulInv(p^, n) * p^;
  };
  sum^ mod prod;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("13.test");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
  Printf.printf(
    "cmr test: %d\n",
    multi_cmr([|
      // Test cases, not full problem
      (7, 0),
      (13, 12),
      (59, 55),
      (31, 25),
      (19, 12),
    |]),
  );
  Printf.printf(
    "cmr 2: %d\n",
    multi_cmr([|
      // Test cases, not full problem
      (1789, 0),
      (37, 37 - 1),
      (47, 47 - 2),
      (1889, 1889 - 3),
    |]),
  );
};
