open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

type tile = {
  id: int,
  chars: array(array(char)),
  charsInner: array(array(char)),
};

type idx = (int, int, int);

// The transforms.
let identity: idx => idx = i => i;
let rotateCW: idx => idx =
  ((r, c, n)) => {
    (c, n - 1 - r, n);
  };
let flip: idx => idx = ((r, c, n)) => (n - 1 - r, c, n);

// Compose
let (@.) = (f, g, x) => f(g(x));

// All transforms:
let transforms = [
  identity,
  identity @. rotateCW,
  identity @. rotateCW @. rotateCW,
  identity @. rotateCW @. rotateCW @. rotateCW,
  flip,
  flip @. rotateCW,
  flip @. rotateCW @. rotateCW,
  flip @. rotateCW @. rotateCW @. rotateCW,
];

// The different edges:
let top = ((tile, fn)) => {
  let n = Array.length(tile.chars);
  String.init(
    n,
    i => {
      let (r, c, _) = fn((0, i, n));
      tile.chars[r][c];
    },
  );
};

let right = ((tile, fn)) => {
  let n = Array.length(tile.chars);
  String.init(
    n,
    i => {
      let (r, c, _) = fn((i, n - 1, n));
      tile.chars[r][c];
    },
  );
};

let bottom = ((tile, fn)) => {
  let n = Array.length(tile.chars);
  String.init(
    n,
    i => {
      let (r, c, _) = fn((n - 1, i, n));
      tile.chars[r][c];
    },
  );
};

let left = ((tile, fn)) => {
  let n = Array.length(tile.chars);
  String.init(
    n,
    i => {
      let (r, c, _) = fn((i, 0, n));
      tile.chars[r][c];
    },
  );
};

let prepareLines = lines => {
  let data = Input.groups(lines);
  let data =
    Array.map(
      tile => {
        let id = Input.extract("Tile {}:", tile[0])[0];
        let id = Parse.int(id);
        let tile = arrayRemoveFirst(tile);
        let n = Array.length(tile);
        let m = String.length(tile[0]);
        let tileArr = Array.map(String.toCharArray, tile);
        let charsInner =
          Array.init(n - 2, r => {
            Array.init(m - 2, c => {tileArr[r + 1][c + 1]})
          });

        let chars = Array.init(n, r => {Array.init(m, c => {tileArr[r][c]})});

        {id, chars, charsInner};
      },
      data,
    );

  data;
};

// Dumb sqrt.
let sqrt = x => {
  let ans = ref(0);
  while (ans^ * ans^ !== x && ans^ < 1000) {
    incr(ans);
  };
  if (ans^ === 1000) {
    failwith("bad sqrt");
  };
  ans^;
};

// Hashing for the map.
let en = (r, c) => Print.int(r) ++ "," ++ Print.int(c);
let de = s => {
  let parts = String.split(",", s);
  (Parse.int(List.hd(parts)), Parse.int(List.hd(List.tl(parts))));
};

/*******************
 * Part 1 Solution *
 *******************/

let getMap = lines => {
  let tiles = prepareLines(lines);
  let n = sqrt(Array.length(tiles));
  let tiles = tiles |> Array.to_list;

  let printMap = (map, n) => {
    for (r in 0 to n - 1) {
      for (c in 0 to n - 1) {
        switch (Map.get(en(r, c), map)) {
        | Some((t, _fn)) =>
          print_int(t.id);
          print_string(" ");
          print_string("\t");
        | _ => print_string("None\t")
        };
      };
      print_endline("");
    };
  };
  let printMapOpt = (map, n) => printMap(Option.getExn(map), n);
  let _ = printMapOpt;

  // Pair up transforms and tiles
  let all =
    List.fold_left(
      (all, fn) => {all @ List.map(tile => (tile, fn), tiles)},
      [],
      transforms,
    );

  let remaining = visited => {
    all |> List.filter(((tile, _fn)) => !IntSet.has(tile.id, visited));
  };

  let rec dfs = (m, visited, r, c, next) => {
    let aboveGood =
      if (r > 0) {
        let tileAbove = Map.getExn(en(r - 1, c), m);
        bottom(tileAbove) == top(next);
      } else {
        true;
      };

    let leftGood =
      if (c > 0) {
        let tileLeft = Map.getExn(en(r, c - 1), m);
        right(tileLeft) == left(next);
      } else {
        true;
      };

    if (aboveGood && leftGood) {
      let m = Map.set(en(r, c), next, m);
      let visited = IntSet.add(fst(next).id, visited);

      let c = c + 1;
      let (r, c) = c === n ? (r + 1, 0) : (r, c);
      if (r === n) {
        Some(m);
      } else {
        List.fold_left(
          (ans, next) =>
            if (ans == None) {
              dfs(m, visited, r, c, next);
            } else {
              ans;
            },
          None,
          remaining(visited),
        );
      };
    } else {
      None;
    };
  };

  // Start the dfs.
  let ans =
    List.fold_left(
      (ans, next) =>
        if (ans == None) {
          dfs(Map.empty, IntSet.empty, 0, 0, next);
        } else {
          ans;
        },
      None,
      remaining(IntSet.empty),
    );

  let ans = Option.getExn(ans);
  // printMap(ans, n);
  let product =
    fst(Map.getExn(en(0, 0), ans)).id
    * fst(Map.getExn(en(n - 1, 0), ans)).id
    * fst(Map.getExn(en(0, n - 1), ans)).id
    * fst(Map.getExn(en(n - 1, n - 1), ans)).id;
  (ans, product);
};

let part1 = lines => {
  let (_m, product) = getMap(lines);
  product;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  // Helpful for debugging.
  // let chars = tile => tile.chars;
  let chars = tile => tile.charsInner;

  let tiles = prepareLines(lines);
  let tileCt = sqrt(Array.length(tiles));
  let tileLen = Array.length(chars(tiles[0]));
  let (map, _) = getMap(lines);

  let n = tileCt * tileLen;
  let chars =
    Array.init(n, r =>
      Array.init(
        n,
        c => {
          let tileR = r / tileLen;
          let tileC = c / tileLen;
          let r = r mod tileLen;
          let c = c mod tileLen;
          let (tile, fn) = Map.getExn(en(tileR, tileC), map);
          let n = Array.length(chars(tile));
          let (r, c, _) = fn((r, c, n));
          chars(tile)[r][c];
        },
      )
    );
  let monster = Array.init(n, _ => Array.init(n, _ => false));

  // Build content

  let printContent = () => {
    let fn = rotateCW @. rotateCW @. rotateCW;
    for (r in 0 to n - 1) {
      for (c in 0 to n - 1) {
        let (r, c, _) = fn((r, c, n));
        print_char(chars[r][c]);
      };
      print_newline();
    };
  };
  let _ = printContent;

  /*
     01234567890123456789012
     -----------------------
   0|..................O....
   1|O....OO....OO....OOO...
   2|.O..O..O..O..O..O......
   */
  let monsterCoords = [
    (0, 18),
    (1, 0),
    (1, 5),
    (1, 6),
    (1, 11),
    (1, 12),
    (1, 17),
    (1, 18),
    (1, 19),
    (1, 19),
    (2, 1),
    (2, 4),
    (2, 7),
    (2, 10),
    (2, 13),
    (2, 16),
  ];

  for (r in 0 to n - 1) {
    for (c in 0 to n - 1) {
      List.iter(
        fn => {
          let coords =
            List.map(
              ((rOffset, cOffset)) => {
                let r = r + rOffset;
                let c = c + cOffset;
                // Not sure why the transform has to include the offset,
                // but doing the transform without the offset doesn't work.
                let (r, c, _) = fn((r, c, n));
                (r, c);
              },
              monsterCoords,
            );

          let isMonster =
            List.for_all(
              ((r, c)) =>
                if (r >= 0 && r < n && c >= 0 && c < n) {
                  chars[r][c] === '#';
                } else {
                  false;
                },
              coords,
            );

          if (isMonster) {
            List.iter(
              ((r, c)) => {
                // printf("  %d, %d\n", r, c);
                monster[r][c] = true;
                chars[r][c] = 'O';
              },
              coords,
            );
          };
        },
        transforms,
      );
    };
  };

  // printContent();

  let sum = ref(0);
  for (r in 0 to n - 1) {
    for (c in 0 to n - 1) {
      if (chars[r][c] === '#' && !monster[r][c]) {
        incr(sum);
      };
    };
  };
  sum^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("20");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
