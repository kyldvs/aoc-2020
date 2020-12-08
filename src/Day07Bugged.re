open Bread;

let cleanNumbers = s => {
  s
  |> String.replaceAll("0", "")
  |> String.replaceAll("1", "")
  |> String.replaceAll("2", "")
  |> String.replaceAll("3", "")
  |> String.replaceAll("4", "")
  |> String.replaceAll("5", "")
  |> String.replaceAll("6", "")
  |> String.replaceAll("7", "")
  |> String.replaceAll("8", "")
  |> String.replaceAll("9", "")
  |> String.trim;
};

let part1 = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(l => l |> String.replaceAll("bags", "bag"), lines);

  // let idMap = ref(Map.empty);
  let outer = ref(Set.empty);
  let containBy = ref(Map.empty);
  let contains = ref(Map.empty);

  let getID = s => {
    // if (!Map.hasKey(s, idMap^)) {
    //   let nextID = Map.size(idMap^);
    //   idMap := Map.set(s, nextID, idMap^);
    // };
    // let id = Map.getExn(s, idMap^);
    // id;
    s;
  };

  let addContain = (a, b) => {
    if (!Set.has(a, outer^)) {
      outer := Set.add(a, outer^);
    };
    if (!Map.hasKey(b, containBy^)) {
      containBy := Map.set(b, ref(Set.empty), containBy^);
    };
    let s = Map.getExn(b, containBy^);
    s := Set.add(a, s^);

    if (!Map.hasKey(a, contains^)) {
      contains := Map.set(a, ref(Set.empty), contains^);
    };
    let s = Map.getExn(a, contains^);
    s := Set.add(b, s^);
  };

  // print_endline("Hello");

  let _ =
    List.iter(
      line => {
        // print_endline(line);
        let line = String.replaceAll(".", "", line);
        let parts = String.split(" contain ", line);
        let left = List.hd(parts);
        let right = List.hd(List.tl(parts));
        let id = getID(left);
        if (right != "no other bag") {
          let parts = String.split(", ", right);
          let _ =
            List.iter(
              part => {
                let part = cleanNumbers(part);
                let idPart = getID(part);
                addContain(id, idPart);
              },
              parts,
            );
          ();
        };
      },
      lines,
    );

  // let reaches = ref(0);

  // let rec canReach = (key, visited) => {
  //   let cons = Map.getExn(, contains^)
  //   switch (stack) {
  //   | [] => false
  //   | [hd, ...rest] =>
  //     if (hd == "shiny gold bag") {
  //       break := true;
  //       true;
  //     } else {

  //     };
  //   };
  // };

  // let _ =
  //   Map.mapi(
  //     (key, value) => {
  //       if (canReach(key, ref(Set.empty), ref(false))) {
  //         incr(reaches);
  //       };
  //       value;
  //     },
  //     contains^,
  //   );

  let visited = ref(Set.empty);
  let rec dfs = stack => {
    switch (stack) {
    | [] => ()
    | [hd, ...rest] =>
      if (!Set.has(hd, visited^)) {
        visited := Set.add(hd, visited^);
        let next =
          try((Map.getExn(hd, containBy^))^) {
          | _ => Set.empty
          };
        let next = Set.toList(next);
        dfs(next @ rest);
      };
      // THE FIX...
      // else { dfs(rest); }
    };
  };
  // let _ = Map.mapi((key, v) => print_endline(key), containBy^);
  let root = (Map.getExn("shiny gold bag", containBy^))^;
  let root = Set.toList(root);
  dfs(root);

  let count = ref(0);
  let _ =
    Set.map(
      c => {
        print_endline(c);
        if (Set.has(c, outer^)) {
          incr(count);
        };
        c;
      },
      visited^,
    );
  // count^;

  print_int(Set.size(visited^));
  print_endline("");
  count^;
};

let part2 = lines => {
  let n = Array.length(lines);
  let xs = Array.map(int_of_string, lines);
  let result = ref(-1);
  for (i in 0 to n - 1) {
    for (j in i + 1 to n - 1) {
      for (k in j + 1 to n - 1) {
        if (xs[i] + xs[j] + xs[k] === 2020) {
          result := xs[i] * xs[j] * xs[k];
        };
      };
    };
  };
  result^;
};

let run = () => {
  let lines = Utils.getInput("07");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
