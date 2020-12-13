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

module ChineseRemainderTheorem = {
  let multInverse = (a, b) => {
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

  let iterative = pairs => {
    let len = Array.length(pairs);
    let prod = Array.fold_left((acc, (x, r)) => acc * x, 1, pairs);
    let p = ref(0);
    let sum = ref(0);
    for (i in 0 to len - 1) {
      let (n, r) = pairs[i];
      p := prod / n;
      sum := sum^ + r * multInverse(p^, n) * p^;
    };
    sum^ mod prod;
  };
};

let part2 = lines => {
  let ids =
    lines[1]
    |> String.replaceAll("x,", "-1,")
    |> String.split(",")
    |> Array.of_list;
  let ids = Array.map(int_of_string, ids);
  let ps = Array.mapi((i, id) => (id, i), ids);
  let ps =
    ps
    |> Array.to_list
    |> List.filter(((id, _)) => id !== (-1))
    |> Array.of_list;
  let ps =
    Array.map(
      ((id, i)) => {
        let r = ref(id - i);
        while (r^ < 0) {
          r := r^ + id;
        };
        (id, r^);
      },
      ps,
    );
  ChineseRemainderTheorem.iterative(ps);
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("13");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
