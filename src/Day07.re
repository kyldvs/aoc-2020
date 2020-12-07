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

  let containBy = ref(Map.empty);
  let contains = ref(Map.empty);

  let addContain = (a, b) => {
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

  let _ =
    List.iter(
      line => {
        let line = String.replaceAll(".", "", line);
        let parts = String.split(" contain ", line);
        let left = List.hd(parts);
        let right = List.hd(List.tl(parts));
        let id = left;
        if (right != "no other bag") {
          let parts = String.split(", ", right);
          let _ =
            List.iter(
              part => {
                let part = cleanNumbers(part);
                let idPart = part;
                addContain(id, idPart);
              },
              parts,
            );
          ();
        };
      },
      lines,
    );

  let visited = ref(Set.empty);
  let rec dfs = c => {
    ();
    if (!Set.has(c, visited^)) {
      visited := Set.add(c, visited^);
      let next =
        try((Map.getExn(c, containBy^))^) {
        | _ => Set.empty
        };
      let next = Set.toList(next);
      let _ = List.iter(dfs, next);
      ();
    };
  };

  let root = (Map.getExn("shiny gold bag", containBy^))^;
  let root = Set.toList(root);
  let _ = List.iter(dfs, root);

  Set.size(visited^);
};

let part2 = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(l => l |> String.replaceAll("bags", "bag"), lines);

  let containBy = ref(Map.empty);
  let contains = ref(Map.empty);

  let addContain = (a, b, count) => {
    if (!Map.hasKey(b, containBy^)) {
      containBy := Map.set(b, ref(Set.empty), containBy^);
    };
    let s = Map.getExn(b, containBy^);
    s := Set.add(a, s^);

    if (!Map.hasKey(a, contains^)) {
      contains := Map.set(a, ref(Map.empty), contains^);
    };
    let s = Map.getExn(a, contains^);
    s := Map.set(b, count, s^);
  };

  let _ =
    List.iter(
      line => {
        let line = String.replaceAll(".", "", line);
        let parts = String.split(" contain ", line);
        let left = List.hd(parts);
        let right = List.hd(List.tl(parts));
        let id = left;
        if (right != "no other bag") {
          let parts = String.split(", ", right);
          let _ =
            List.iter(
              part => {
                let s = String.split(" ", part);
                let count = int_of_string(List.hd(s));
                let color = String.concat(" ", List.tl(s));
                addContain(id, color, count);
              },
              parts,
            );
          ();
        };
      },
      lines,
    );

  let rec dfs = c => {
    ();
    switch (Map.get(c, contains^)) {
    | Some(next) =>
      let sum = ref(1);
      let _ =
        Map.mapi(
          (k, v) => {
            let subCount = dfs(k);
            sum := sum^ + v * subCount;
            v;
          },
          next^,
        );
      sum^;
    | None => 1
    };
  };
  let count = dfs("shiny gold bag");
  count - 1;
};

let run = () => {
  let lines = Utils.getInput("07");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
