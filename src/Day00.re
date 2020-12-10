open Bread;

/*********************
 * Input preparation *
 *********************/

type data = {
  raw: string,
  value: int,
};

let prepareLine = line => {
  let line = String.replaceAll("foo", "foo", line);
  switch (String.split("dummytoken", line)) {
  | [part1] => {raw: line, value: (-1)}
  | _ => failwith("Invalid input")
  };
};

let prepareInput = lines => {
  let lines = Array.map(prepareLine, lines);
  (lines, Array.length(lines));
};

let prepareGroup = group => {
  let _arr = group;
  let list = Array.to_list(group);
  list;
};

let prepareInput2 = lines => {
  let groups = Utils.groupInput(lines);
  let data = Array.map(prepareGroup, groups);
  (data, Array.length(data));
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let (data, _) = prepareInput(lines);
  Utils.count(
    ({raw}) => {
      ();
      true;
    },
    data,
  );
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (data, _) = prepareInput(lines);
  Utils.count(
    ({raw}) => {
      ();
      true;
    },
    data,
  );
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("10");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
