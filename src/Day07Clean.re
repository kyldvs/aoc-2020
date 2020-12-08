open Bread;

let prepareInput = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(String.replaceAll("bags", "bag"), lines);
  let lines = List.map(String.replaceAll(".", ""), lines);
  lines;
};

let parseLine = line => {
  let parts = String.split(" contain ", line);
  let leftNode = List.hd(parts);
  let right = List.hd(List.tl(parts));
  let right =
    if (right == "no other bag") {
      [];
    } else {
      let parts = String.split(", ", right);
      let parts =
        List.map(
          part => {
            let s = String.split(" ", part);
            let count = int_of_string(List.hd(s));
            let color = String.concat(" ", List.tl(s));
            (color, count);
          },
          parts,
        );
      parts;
    };
  (leftNode, right);
};

let part1 = lines => {
  let lines = prepareInput(lines);
  let g =
    List.fold_left(
      (g, line) => {
        let (left, right) = parseLine(line);
        let g =
          List.fold_left(
            (g, (color, _count)) => {Graph.addDirected(color, left, g)},
            g,
            right,
          );
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
  let lines = prepareInput(lines);
  let g =
    List.fold_left(
      (g, line) => {
        let (left, right) = parseLine(line);
        let g =
          List.fold_left(
            (g, (color, count)) => {
              GraphWeighted.addDirected(left, color, count, g)
            },
            g,
            right,
          );
        g;
      },
      GraphWeighted.empty,
      lines,
    );

  let rec dfs = node => {
    let edges = GraphWeighted.getEdges(node, g);
    let count = ref(1);
    List.iter(
      ((next, weight)) => {
        let subCount = dfs(next);
        count := count^ + weight * subCount;
        ();
      },
      edges,
    );
    count^;
  };

  let count = dfs("shiny gold bag");
  count - 1;
};

let run = () => {
  let lines = Utils.getInput("07");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
