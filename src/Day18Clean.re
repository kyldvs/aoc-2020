open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let lines =
    Array.map(
      line => {
        let line = String.replaceAll("(", "( ", line);
        let line = String.replaceAll(")", " )", line);
        String.split(" ", line);
      },
      lines,
    );
  lines;
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  module RPN =
    RPN.Make({
      let config =
        Map.fromList([
          // + and * have equal precedence
          ("+", (1, RPN.Left, (+))),
          ("*", (1, RPN.Left, ( * ))),
        ]);
    });

  let data = prepareLines(lines);
  Array.fold_left((sum, tokens) => sum + RPN.run(tokens), 0, data);
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  module RPN =
    RPN.Make({
      let config =
        Map.fromList([
          // Now + has a higher precedence than *
          ("+", (2, RPN.Left, (+))),
          ("*", (1, RPN.Left, ( * ))),
        ]);
    });

  let data = prepareLines(lines);
  Array.fold_left((sum, tokens) => sum + RPN.run(tokens), 0, data);
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("18");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
