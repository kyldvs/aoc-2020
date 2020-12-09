open Bread;

/*********************
 * Input preparation *
 *********************/

type data = {
  byr: option(string),
  iyr: option(string),
  eyr: option(string),
  hgt: option(string),
  hcl: option(string),
  ecl: option(string),
  pid: option(string),
  cid: option(string),
};

let empty = {
  byr: None,
  iyr: None,
  eyr: None,
  hgt: None,
  hcl: None,
  ecl: None,
  pid: None,
  cid: None,
};

let prepareLine = line => {
  let data =
    List.fold_left(
      (data, part) => {
        let data =
          switch (String.split(":", part)) {
          | ["byr", byr] => {...data, byr: Some(byr)}
          | ["iyr", iyr] => {...data, iyr: Some(iyr)}
          | ["eyr", eyr] => {...data, eyr: Some(eyr)}
          | ["hgt", hgt] => {...data, hgt: Some(hgt)}
          | ["hcl", hcl] => {...data, hcl: Some(hcl)}
          | ["ecl", ecl] => {...data, ecl: Some(ecl)}
          | ["pid", pid] => {...data, pid: Some(pid)}
          | ["cid", cid] => {...data, cid: Some(cid)}
          | _ => data
          };
        data;
      },
      empty,
      String.split(" ", line),
    );
  data;
};

let prepareInput = lines => {
  let lines = Utils.groupInput(lines);
  let lines =
    Array.map(
      group => {
        let list = Array.to_list(group);
        String.concat(" ", list);
      },
      lines,
    );
  let lines = Array.map(prepareLine, lines);
  (lines, Array.length(lines));
};

/*******************
 * Part 1 Solution *
 *******************/

let allPresent = data => {
  switch (data) {
  | {
      byr: Some(_),
      iyr: Some(_),
      eyr: Some(_),
      hgt: Some(_),
      hcl: Some(_),
      ecl: Some(_),
      pid: Some(_),
    } =>
    true
  | _ => false
  };
};

let part1 = lines => {
  let (data, _) = prepareInput(lines);
  Utils.count(allPresent, data);
};

/*******************
 * Part 2 Solution *
 *******************/

let valid_byr = s => {
  let n = String.length(s);
  let i =
    try(int_of_string(s)) {
    | _ => (-1)
    };
  n === 4 && i >= 1920 && i <= 2002;
};

let valid_iyr = s => {
  let n = String.length(s);
  let i =
    try(int_of_string(s)) {
    | _ => (-1)
    };
  n === 4 && i >= 2010 && i <= 2020;
};

let valid_eyr = s => {
  let n = String.length(s);
  let i =
    try(int_of_string(s)) {
    | _ => (-1)
    };
  n === 4 && i >= 2020 && i <= 2030;
};

let valid_hgt = s => {
  let lastTwo = String.sliceToEnd(-2, s);
  let front = String.slice(0, -2, s);
  let i =
    try(int_of_string(front)) {
    | _ => (-1)
    };
  switch (lastTwo) {
  | "in" => i >= 59 && i <= 76
  | "cm" => i >= 150 && i <= 193
  | _ => false
  };
};

let valid_hcl = s => {
  let n = String.length(s);
  if (n === 7) {
    let valid = ref(true);
    if (s.[0] !== '#') {
      valid := false;
    };
    for (i in 1 to n - 1) {
      let okay = s.[i] >= '0' && s.[i] <= '9' || s.[i] >= 'a' && s.[i] <= 'f';
      if (!okay) {
        valid := false;
      };
    };
    valid^;
  } else {
    false;
  };
};

let valid_ecl = s => {
  switch (s) {
  | "amb"
  | "blu"
  | "brn"
  | "gry"
  | "grn"
  | "hzl"
  | "oth" => true
  | _ => false
  };
};

let valid_pid = s => {
  let n = String.length(s);
  let i =
    try(int_of_string(s)) {
    | _ => (-1)
    };
  n === 9 && i !== (-1);
};

let allValid = data => {
  switch (data) {
  | {
      byr: Some(byr),
      iyr: Some(iyr),
      eyr: Some(eyr),
      hgt: Some(hgt),
      hcl: Some(hcl),
      ecl: Some(ecl),
      pid: Some(pid),
    } =>
    valid_byr(byr)
    && valid_iyr(iyr)
    && valid_eyr(eyr)
    && valid_hgt(hgt)
    && valid_hcl(hcl)
    && valid_ecl(ecl)
    && valid_pid(pid)
  | _ => false
  };
};

let part2 = lines => {
  let (data, _) = prepareInput(lines);
  Utils.count(allValid, data);
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("04");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
