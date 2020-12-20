open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let rotateCW = (r, c, n) => {

};

type tile = {
  id: int,
  left: string,
  right: string,
  top: string,
  bottom: string,
  leftR: string,
  rightR: string,
  topR: string,
  bottomR: string,
  content: array(array(char)),
};

type orient =
  | Top
  | Right
  | Left
  | Bottom;

let reverse = s => s |> String.toCharList |> List.rev |> String.fromCharList;

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
        let top = tile[0];
        let bottom = tile[n - 1];
        let left = tile |> Array.map(s => s.[0]) |> String.fromCharArray;
        let right = tile |> Array.map(s => s.[m - 1]) |> String.fromCharArray;
        let tileArr = Array.map(String.toCharArray, tile);
        let content =
          Array.init(n - 2, r => {
            Array.init(m - 2, c => {tileArr[r + 1][c + 1]})
          });

        let _content =
          Array.init(n, r => {Array.init(m, c => {tileArr[r][c]})});

        {
          id,
          top,
          left,
          right,
          bottom,
          topR: reverse(top),
          leftR: reverse(left),
          rightR: reverse(right),
          bottomR: reverse(bottom),
          content,
        };
      },
      data,
    );

  data;
};

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

let en = (i, j) => Print.int(i) ++ "," ++ Print.int(j);
let de = s => {
  let parts = String.split(",", s);
  (Parse.int(List.hd(parts)), Parse.int(List.hd(List.tl(parts))));
};

/*******************
 * Part 1 Solution *
 *******************/

let side = (entry, want) => {
  let (tile, orient, flip) = Option.getExn(entry);
  switch (orient, flip, want) {
  | (Top, false, Top) => tile.top
  | (Top, false, Right) => tile.right
  | (Top, false, Bottom) => tile.bottom
  | (Top, false, Left) => tile.left
  | (Top, true, Top) => tile.bottom
  | (Top, true, Right) => tile.rightR
  | (Top, true, Bottom) => tile.top
  | (Top, true, Left) => tile.leftR

  | (Right, false, Top) => tile.right
  | (Right, false, Right) => tile.bottomR
  | (Right, false, Bottom) => tile.left
  | (Right, false, Left) => tile.topR
  | (Right, true, Top) => tile.rightR
  | (Right, true, Right) => tile.topR
  | (Right, true, Bottom) => tile.leftR
  | (Right, true, Left) => tile.bottomR

  | (Bottom, false, Top) => tile.bottomR
  | (Bottom, false, Right) => tile.leftR
  | (Bottom, false, Bottom) => tile.topR
  | (Bottom, false, Left) => tile.rightR
  | (Bottom, true, Top) => tile.topR
  | (Bottom, true, Right) => tile.left
  | (Bottom, true, Bottom) => tile.bottomR
  | (Bottom, true, Left) => tile.right

  | (Left, false, Top) => tile.leftR
  | (Left, false, Right) => tile.top
  | (Left, false, Bottom) => tile.rightR
  | (Left, false, Left) => tile.bottom
  | (Left, true, Top) => tile.left
  | (Left, true, Right) => tile.bottom
  | (Left, true, Bottom) => tile.right
  | (Left, true, Left) => tile.top
  };
};

let eq = (s1, s2) => {
  s1 == s2;
};

let getMap = lines => {
  let tiles = prepareLines(lines);
  let n = sqrt(Array.length(tiles));

  let id = o => {
    // printf("id\n");
    let (tile, orient, flip) = Option.getExn(o);
    tile.id;
  };

  let printMap = (map, n) => {
    for (i in 0 to n - 1) {
      for (j in 0 to n - 1) {
        switch (Map.get(en(i, j), map)) {
        | Some(Some((t, orient, flip))) =>
          print_int(t.id);
          print_string(" ");
          switch (orient) {
          | Top => print_string("T")
          | Right => print_string("R")
          | Bottom => print_string("B")
          | Left => print_string("L")
          };
          flip ? print_string("F") : ();
          print_string("\t");
        | _ => print_string("None\t")
        };
      };
      print_endline("");
    };
  };
  let printMapOpt = (map, n) => printMap(Option.getExn(map), n);
  let _ = printMapOpt;

  let orients = [|Top, Right, Bottom, Left|];

  let all = MList.make();
  Array.iter(
    tile => {
      Array.iter(
        orient => {
          MList.addFirst((tile, orient, false), all);
          MList.addFirst((tile, orient, true), all);
        },
        orients,
      )
    },
    tiles,
  );
  let all = all^;

  let remaining = visited => {
    let r =
      List.filter(((tile, _, _)) => {!IntSet.has(tile.id, visited)}, all);
    List.map(x => Some(x), r) |> List.rev;
  };

  let rec dfs = (m, visited, i, j, next) =>
    if (j === n) {
      printMap(m, n);
      Some(m);
    } else {
      let aboveGood =
        if (j > 0) {
          let tileAbove = Map.getExn(en(i, j - 1), m);
          eq(side(tileAbove, Bottom), side(next, Top));
        } else {
          true;
        };

      let leftGood =
        if (i > 0) {
          let tileLeft = Map.getExn(en(i - 1, j), m);
          eq(side(tileLeft, Right), side(next, Left));
        } else {
          true;
        };

      if (aboveGood && leftGood) {
        let m = Map.set(en(i, j), next, m);
        let visited = IntSet.add(id(next), visited);
        let j = j + 1;
        let (i, j) = j === n ? (i + 1, 0) : (i, j);

        if (i === n) {
          Some(m);
        } else {
          let ans = ref(None);
          List.iter(
            next =>
              // printMap(m, n);
              if (ans^ == None) {
                ans := dfs(m, visited, i, j, next);
              },
            remaining(visited),
          );
          ans^;
        };
      } else {
        None;
      };
    };

  let ans = ref(None);
  List.iter(
    next =>
      // printMap(m, n);
      if (ans^ == None) {
        let nextAns = dfs(Map.empty, IntSet.empty, 0, 0, next);
        if (nextAns != None) {
          printMapOpt(nextAns, n);
          ans := nextAns;
        };
      },
    remaining(IntSet.empty),
  );

  let ans = Option.getExn(ans^);
  let product =
    id(Map.getExn(en(0, 0), ans))
    * id(Map.getExn(en(n - 1, 0), ans))
    * id(Map.getExn(en(0, n - 1), ans))
    * id(Map.getExn(en(n - 1, n - 1), ans));
  (ans, product);
};

let part1 = lines => {
  let (_m, product) = getMap(lines);
  product;
};

/*******************
 * Part 2 Solution *
 *******************/

let index = (r, c, n, orient, flip) => {
  switch (orient, flip) {
  | (Top, false) => (r, c)
  | (Top, true) => (n - 1 - r, c)

  | (Right, false) => (c, r)
  | (Right, true) => (n - 1 - c, r)

  | (Bottom, false) => (n - 1 - r, n - 1 - c)
  | (Bottom, true) => (r, n - 1 - c)

  | (Left, false) => (n - 1 - c, n - 1 - r)
  | (Left, true) => (c, r)
  };
};

let part2 = lines => {
  let tiles = prepareLines(lines);
  let tileCt = sqrt(Array.length(tiles));
  let tileLen = Array.length(tiles[0].content);
  let (m, _) = getMap(lines);

  let n = tileCt * tileLen;
  let content = Array.init(n, _ => Array.init(n, _ => ' '));
  let monster = Array.init(n, _ => Array.init(n, _ => false));

  let printContent = (orient, flip) => {
    for (r in 0 to n - 1) {
      for (c in 0 to n - 1) {
        let (r, c) = index(r, c, n, orient, flip);
        print_char(content[r][c]);
      };
      print_newline();
    };
  };

  m
  |> Map.toList
  |> List.iter(((key, tile)) => {
       //  let (tileRow, tileCol) = de(key);
       // I think I need to transpose them...
       let (tileCol, tileRow) = de(key);
       let rowOffset = tileRow * tileLen;
       let colOffset = tileCol * tileLen;
       let (tile, orient, flip) = Option.getExn(tile);
       for (r in 0 to tileLen - 1) {
         for (c in 0 to tileLen - 1) {
           let (rInTile, cInTile) = index(r, c, tileLen, orient, flip);
           let value = tile.content[rInTile][cInTile];
           content[rowOffset + r][colOffset + c] = value;
         };
       };
     });

  // printf("=T==\n");
  // printContent(Left, true);
  // printf("====\n");

  let orients = [|Top, Right, Bottom, Left|];
  let flips = [|true, false|];

  /*
     01234567890123456789012
     -----------------------
   0|.###...#.##...#.##O###.
   1|O##.#OO.###OO##..OOO##.
   2|.O#.O..O..O.#O##O##.###
   */
  let monsterCoords = [
    (18, 0),
    (0, 1),
    (5, 1),
    (6, 1),
    (11, 1),
    (12, 1),
    (17, 1),
    (18, 1),
    (19, 1),
    (19, 1),
    (1, 2),
    (4, 2),
    (7, 2),
    (10, 2),
    (13, 2),
    (16, 2),
  ];

  for (a in 0 to Array.length(orients) - 1) {
    for (b in 0 to Array.length(flips) - 1) {
      let orient = orients[a];
      let flip = flips[b];
      let monsterCoords =
        List.map(((i, j)) => index(i, j, n, orient, flip), monsterCoords);
      let isMonster = (i, j) => {
        List.for_all(
          ((iOffset, jOffset)) => {
            let i = i + iOffset;
            let j = j + jOffset;
            if (i >= 0 && i < n && j >= 0 && j < n) {
              content[i][j] === '#';
            } else {
              false;
            };
          },
          monsterCoords,
        );
      };
      let markMonster = (i, j) => {
        List.iter(
          ((iOffset, jOffset)) => {
            let i = i + iOffset;
            let j = j + jOffset;
            if (i >= 0 && i < n && j >= 0 && j < n) {
              monster[i][j] = true;
              content[i][j] = 'O';
            } else {
              failwith("invalid monster");
            };
          },
          monsterCoords,
        );
      };
      for (i in 0 to n - 1) {
        for (j in 0 to n - 1) {
          if (isMonster(i, j)) {
            markMonster(i, j);
          };
        };
      };
    };
  };

  printf("=T==\n");
  printContent(Left, true);
  printf("====\n");
  // printf("Right\n");
  // printContent(Right, true);
  // printf("Bottom\n");
  // printContent(Bottom, true);
  // printf("Left\n");
  // printContent(Left, false);

  let sum = ref(0);
  for (i in 0 to n - 1) {
    for (j in 0 to n - 1) {
      if (content[i][j] === '#' && !monster[i][j]) {
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
  let lines = Utils.getInput("20.test");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
