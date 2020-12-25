open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLine = line => {
  let line = String.replaceAll("se", " SE ", line);
  let line = String.replaceAll("ne", " NE ", line);
  let line = String.replaceAll("sw", " SW ", line);
  let line = String.replaceAll("nw", " NW ", line);
  let line = String.replaceAll("w", " W ", line);
  let line = String.replaceAll("e", " E ", line);
  let x = Scanner.make([line]);
  let xs = ref([]);
  while (Scanner.hasNext(x)) {
    xs := [Scanner.nextString(x), ...xs^];
  };
  xs^;
};

let prepareLines = lines => {
  let data = Array.map(prepareLine, lines) |> Array.to_list;
  data;
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let data = prepareLines(lines);
  let black = ref(Set.empty);
  let en = (x, y) => Print.int(x) ++ "," ++ Print.int(y);
  let flip = (x, y) => {
    let encoding = en(x, y);
    if (Set.has(encoding, black^)) {
      black := Set.remove(encoding, black^);
    } else {
      black := Set.add(encoding, black^);
    };
  };

  List.iter(
    xs => {
      // printf("%s\n", String.concat(", ", xs));
      let e = ref(0);
      let se = ref(0);
      List.iter(
        dir => {
          switch (dir) {
          | "E" => incr(e)
          | "W" => decr(e)
          | "SE" => incr(se)
          | "SW" =>
            decr(e);
            incr(se);
          | "NE" =>
            incr(e);
            decr(se);
          | "NW" => decr(se)
          | _ => ()
          }
        },
        xs,
      );
      flip(e^, se^);
    },
    data,
  );

  Set.size(black^);
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let data = prepareLines(lines);
  let black = ref(Set.empty);
  let en = (x, y) => Print.int(x) ++ "," ++ Print.int(y);
  let en2 = ((x, y)) => en(x, y);
  let de = s => {
    let parts = String.split(",", s);
    (Parse.int(List.hd(parts)), Parse.int(List.hd(List.tl(parts))));
  };
  let flipH = (r, x, y) => {
    let encoding = en(x, y);
    if (Set.has(encoding, r^)) {
      r := Set.remove(encoding, r^);
    } else {
      r := Set.add(encoding, r^);
    };
  };
  let flip = flipH(black);

  let adj = ((x, y)) => {
    [
      (x, y - 1), // NW
      (x + 1, y - 1), // NE
      (x + 1, y), // E
      (x, y + 1), // SE
      (x - 1, y + 1), // SW
      (x - 1, y), // W
    ];
  };
  let cons = () => {
    let c = ref([]);
    List.iter(
      s => {
        let add = [de(s), ...adj(de(s))];
        c := [add, ...c^];
      },
      Set.toList(black^),
    );
    c^
    |> List.concat
    |> List.map(((x, y)) => en(x, y))
    |> Set.fromList
    |> Set.toList;
  };

  List.iter(
    xs => {
      // printf("%s\n", String.concat(", ", xs));
      let e = ref(0);
      let se = ref(0);
      List.iter(
        dir => {
          switch (dir) {
          | "E" => incr(e)
          | "W" => decr(e)
          | "SE" => incr(se)
          | "SW" =>
            decr(e);
            incr(se);
          | "NE" =>
            incr(e);
            decr(se);
          | "NW" => decr(se)
          | _ => ()
          }
        },
        xs,
      );
      flip(e^, se^);
    },
    data,
  );

  for (i in 1 to 100) {
    let next = ref(black^);
    let flipNext = flipH(next);
    let check = cons();
    List.iter(
      s => {
        let (x, y) = de(s);
        let ct =
          Utils.countList(
            s => Set.has(s, black^),
            adj((x, y)) |> List.map(en2),
          );
        if (Set.has(s, black^)) {
          if (ct === 0 || ct > 2) {
            flipNext(x, y);
          };
        } else {
          ();
          ();
          if (ct === 2) {
            flipNext(x, y);
          };
        };
      },
      check,
    );
    black := next^;
  };

  Set.size(black^);
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("24");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
