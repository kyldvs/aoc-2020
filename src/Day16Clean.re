open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let groups = Input.groups(lines);
  groups[1] = arrayRemoveFirst(groups[1]);
  groups[2] = arrayRemoveFirst(groups[2]);

  let n = Array.length(groups[0]);
  let ranges =
    Array.init(
      n,
      i => {
        let line = groups[0][i];
        let parts = Input.extract("{}: {}-{} or {}-{}", line);
        let _key = parts[0];
        let low1 = Parse.int(parts[1]);
        let high1 = Parse.int(parts[2]);
        let low2 = Parse.int(parts[3]);
        let high2 = Parse.int(parts[4]);
        let test = x => {
          x >= low1 && x <= high1 || x >= low2 && x <= high2;
        };
        test;
      },
    );

  let valid = x => {
    arraySome(test => test(x), ranges);
  };

  let scanner = Scanner.makeArr(groups[2]);
  Scanner.foldLeftConsuming(
    (sum, token) => {
      let x = Parse.int(token);
      valid(x) ? sum : sum + x;
    },
    0,
    scanner,
  );
};

/*******************
 * Part 2 Solution *
 *******************/

// TODO: This is not cleaned up yet.
let part2 = lines => {
  let groups = Utils.groupInput(lines);
  let keys = MMap.make();
  let ranges = MList.make();
  Array.iter(
    line => {
      let parts = String.split(": ", line);
      let key = List.hd(parts);
      MMap.set(key, true, keys);
      let right = String.concat("", List.tl(parts));
      let s = Scanner.make([right]);
      let low1 = Scanner.nextInt(s);
      let high1 = Scanner.nextInt(s);
      let _ = Scanner.nextString(s);
      let low2 = Scanner.nextInt(s);
      let high2 = Scanner.nextInt(s);
      let inRange = i => {
        i >= low1 && i <= high1 || i >= low2 && i <= high2;
      };
      MList.addFirst((key, inRange), ranges);
      ();
    },
    groups[0],
  );

  let oneValid = x => {
    List.exists(((key, test)) => test(x), ranges^);
  };
  let allValid = xs => {
    List.for_all(oneValid, xs);
  };

  let nearby = groups[2] |> Array.to_list;
  let nearby = List.tl(nearby);
  let n = List.length(String.split(",", List.hd(nearby)));

  let possible = Array.init(n, _ => MMap.fromMap(keys^));
  let removeAll = key => {
    for (i in 0 to n - 1) {
      MMap.remove(key, possible[i]);
    };
  };

  List.iter(
    line => {
      let xs = String.split(",", line) |> List.map(int_of_string);
      if (allValid(xs)) {
        List.iteri(
          (i, value) => {
            MList.iter(
              ((key, test)) =>
                if (!test(value)) {
                  MMap.remove(key, possible[i]);
                },
              ranges,
            )
          },
          xs,
        );
        ();
      };
      ();
    },
    nearby,
  );

  let fields = Array.init(n, _ => None);

  let break = ref(false);
  while (! break^) {
    let change = ref(false);
    for (i in 0 to n - 1) {
      if (fields[i] == None && MMap.size(possible[i]) === 1) {
        let (key, _value) = List.hd(Map.toList((possible[i])^));
        removeAll(key);
        fields[i] = Some(key);
        change := true;
      };
    };

    if (! change^) {
      break := true;
    };
  };

  let index = MMap.make();

  let fields =
    Array.mapi(
      (i, field) => {
        switch (field) {
        | Some(field) =>
          MMap.set(field, i, index);
          field;
        | None => failwith("didn't find field: " ++ string_of_int(i))
        }
      },
      fields,
    );

  // print_endline(String.join(", ", Array.to_list(fields)));

  let me =
    String.split(",", groups[1][1])
    |> List.map(int_of_string)
    |> Array.of_list;
  let p = ref(1);
  for (i in 0 to n - 1) {
    if (String.indexOfInt("departure", fields[i]) === 0) {
      p := p^ * me[i];
    };
  };
  p^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("16");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
