open Bread;

/*********************
 * Input preparation *
 *********************/

type data = {
  min: int,
  max: int,
  letter: char,
  password: string,
};

let prepareLine = line => {
  let line = String.replaceAll(":", "", line);
  let line = String.replaceAll("-", " ", line);
  switch (String.split(" ", line)) {
  | [min, max, letter, password] => {
      min: int_of_string(min),
      max: int_of_string(max),
      letter: letter.[0],
      password,
    }
  | _ => failwith("Invalid input")
  };
};

let prepareInput = lines => {
  let lines = Array.map(prepareLine, lines);
  (lines, Array.length(lines));
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let (data, _) = prepareInput(lines);
  Utils.count(
    ({min, max, letter, password}) => {
      let chars = String.toCharArray(password);
      let count = Utils.count(c => c === letter, chars);
      count >= min && count <= max;
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
    ({min, max, letter, password}) => {
      // Given as 1-indexed
      let i = min - 1;
      let j = max - 1;
      let n = String.length(password);
      let conA = i < n && password.[i] === letter;
      let conB = j < n && password.[j] === letter;
      conA && !conB || conB && !conA;
    },
    data,
  );
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("02");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
