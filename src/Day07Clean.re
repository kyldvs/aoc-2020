open Bread;

let part1 = lines => {
  let lines = Array.to_list(lines);
  let g =
    List.fold_left(
      (g, line) => {
        let line = String.replaceAll("bags", "bag", line);
        let line = String.replaceAll(".", "", line);
        let parts = String.split(" contain ", line);
        let left = List.hd(parts);
        let right = List.hd(List.tl(parts));
        let g =
          if (right != "no other bag") {
            let parts = String.split(", ", right);
            let g =
              List.fold_left(
                (g, part) => {
                  let s = String.split(" ", part);
                  let _count = int_of_string(List.hd(s));
                  let color = String.concat(" ", List.tl(s));
                  Graph.addDirected(color, left, g);
                },
                g,
                parts,
              );
            g;
          } else {
            g;
          };
        g;
      },
      Graph.empty,
      lines,
    );

  let visited =
    Graph.dfs(
      (acc, node) => Set.add(node, acc),
      Set.empty,
      "shiny gold bag",
      g,
    );

  Set.size(visited) - 1;
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
