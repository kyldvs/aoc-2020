let skip_token = "SKIPXXXABCDEFGH";

let part1 = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(s => s == "" ? skip_token : s, lines);
  let s = Scanner.make(lines);
  let valid = ref(0);

  let byr = ref(false);
  let iyr = ref(false);
  let eyr = ref(false);
  let hgt = ref(false);
  let hcl = ref(false);
  let ecl = ref(false);
  let pid = ref(false);
  let cid = ref(false);

  let check = () =>
    if (byr^ && iyr^ && eyr^ && hgt^ && hcl^ && ecl^ && pid^) {
      incr(valid);
    };

  let reset = () => {
    byr := false;
    iyr := false;
    eyr := false;
    hgt := false;
    hcl := false;
    ecl := false;
    pid := false;
    cid := false;
  };

  while (Scanner.hasNext(s)) {
    let field = Scanner.nextString(s);
    if (field == skip_token) {
      check();
      reset();
    } else {
      let _value = Scanner.nextString(s);
      switch (field) {
      | "byr" => byr := true
      | "iyr" => iyr := true
      | "eyr" => eyr := true
      | "hgt" => hgt := true
      | "hcl" => hcl := true
      | "ecl" => ecl := true
      | "pid" => pid := true
      | "cid" => cid := true
      | _ => ()
      };
    };
  };
  check();
  valid^;
};

let part2 = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(s => s == "" ? skip_token : s, lines);
  let s = Scanner.make(lines);
  let valid = ref(0);

  let byr = ref(false);
  let iyr = ref(false);
  let eyr = ref(false);
  let hgt = ref(false);
  let hcl = ref(false);
  let ecl = ref(false);
  let pid = ref(false);
  let cid = ref(false);

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
    module String = Bread.String;
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
        let okay =
          switch (s.[i]) {
          | '0'
          | '1'
          | '2'
          | '3'
          | '4'
          | '5'
          | '6'
          | '7'
          | '8'
          | '9'
          | 'a'
          | 'b'
          | 'c'
          | 'd'
          | 'e'
          | 'f' => true
          | _ => false
          };
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

  let check = () =>
    if (byr^ && iyr^ && eyr^ && hgt^ && hcl^ && ecl^ && pid^) {
      incr(valid);
    };

  let reset = () => {
    byr := false;
    iyr := false;
    eyr := false;
    hgt := false;
    hcl := false;
    ecl := false;
    pid := false;
    cid := false;
  };

  while (Scanner.hasNext(s)) {
    let field = Scanner.nextString(s);
    if (field == skip_token) {
      check();
      reset();
    } else {
      let value = Scanner.nextString(s);
      switch (field) {
      | "byr" =>
        if (valid_byr(value)) {
          byr := true;
          ();
        }
      | "iyr" =>
        if (valid_iyr(value)) {
          iyr := true;
          ();
        }
      | "eyr" =>
        if (valid_eyr(value)) {
          eyr := true;
          ();
        }
      | "hgt" =>
        if (valid_hgt(value)) {
          hgt := true;
          ();
        }
      | "hcl" =>
        if (valid_hcl(value)) {
          hcl := true;
          ();
        }
      | "ecl" =>
        if (valid_ecl(value)) {
          ecl := true;
          ();
        }
      | "pid" =>
        if (valid_pid(value)) {
          pid := true;
          ();
        }
      | "cid" => cid := true
      | _ => ()
      };
    };
  };
  check();
  valid^;
};

let run = () => {
  let lines = Utils.getInput(4);
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
