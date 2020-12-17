open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let data = Grid.parse(lines);
  data;
};

let encode = ((i, j, k)) => {
  Print.int(i) ++ "," ++ Print.int(j) ++ "," ++ Print.int(k);
};

let decode = s => {
  let parts = String.split(",", s);
  let parts = Parse.intList(parts);
  let parts = Array.of_list(parts);
  (parts[0], parts[1], parts[2]);
};

let neighbors1 = i => [i + 1, i, i - 1];

let neighbors2 = ((i, j)) => {
  List.map(x => (i + 1, x), neighbors1(j))
  @ List.map(x => (i, x), neighbors1(j))
  @ List.map(x => (i - 1, x), neighbors1(j));
};

let neighbors = ((i, j, k)) => {
  let ns =
    List.map(((j, k)) => (i + 1, j, k), neighbors2((j, k)))
    @ List.map(((j, k)) => (i, j, k), neighbors2((j, k)))
    @ List.map(((j, k)) => (i - 1, j, k), neighbors2((j, k)));
  List.map(encode, ns);
};

let isActive = (key, map) => {
  switch (MMap.get(key, map)) {
  | Some(true) => true
  | _ => false
  };
};

let active = (key, map) => {
  let ns = neighbors(decode(key));
  let count = ref(0);
  List.iter(
    n =>
      if (n != key) {
        switch (MMap.get(n, map)) {
        | Some(true) => incr(count)
        | _ => ()
        };
      },
    ns,
  );
  count^;
};

let consider = map => {
  let res = ref(Set.empty);
  MMap.iter(
    (key, value) =>
      if (value) {
        let ns = neighbors(decode(key));
        List.iter(key => {res := Set.add(key, res^)}, ns);
      },
    map,
  );
  Set.toList(res^);
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let (data, n, m) = prepareLines(lines);

  let map = MMap.make();
  for (i in 0 to n - 1) {
    for (j in 0 to m - 1) {
      if (data[i][j] === '#') {
        MMap.set(encode((i, j, 0)), true, map);
      };
    };
  };

  for (ct in 1 to 6) {
    let next = MMap.make();
    List.iter(
      key => {
        let count = active(key, map);
        if (isActive(key, map)) {
          if (count === 2 || count === 3) {
            MMap.set(key, true, next);
          };
        } else if (count === 3) {
          MMap.set(key, true, next);
        };
      },
      consider(map),
    );
    map := next^;
  };

  let ans = ref(0);
  MMap.iter(
    (key, value) =>
      if (value) {
        incr(ans);
      },
    map,
  );
  ans^;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  0;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("17");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
