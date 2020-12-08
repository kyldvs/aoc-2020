open Bread;

let prepareInput = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(String.replaceAll("foo", "foo"), lines);
  lines;
};

let parseLine = line => {
  let parts = String.split(" contain ", line);
  parts;
};

let part1 = lines => {
  let lines = prepareInput(lines);
  List.length(lines);
};

let part2 = lines => {
  let lines = prepareInput(lines);
  List.length(lines);
};

let run = () => {
  let lines = Utils.getInput("08");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
