open Bread;

/*********************
 * Input preparation *
 *********************/

type t =
  | Mask(int, int, string)
  | Mem(int, int);

let prepareLine = line => {
  switch (String.slice(0, 3, line)) {
  | "mas" =>
    let m = line |> String.split(" ") |> List.rev |> List.hd;
    let andWith = String.replaceAll("X", "1", m);
    let orWith = String.replaceAll("X", "0", m);
    // print_endline("binary");
    Mask(int_of_string("0b" ++ andWith), int_of_string("0b" ++ orWith), m);
  | "mem" =>
    let line = String.replaceAll("[", " ", line);
    let line = String.replaceAll("]", " ", line);
    let parts = String.split(" ", line);
    // print_endline("value");
    let value = int_of_string(List.hd(List.rev(parts)));
    // print_endline("i");
    let i = int_of_string(List.hd(List.tl(parts)));
    Mem(i, value);
  | _ => failwith("invalid parse")
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
  let (data, n) = prepareInput(lines);
  let max = ref(0);
  for (i in 0 to n - 1) {
    switch (data[i]) {
    | Mem(i, _) => max := i > max^ ? i : max^
    | _ => ()
    };
  };
  let max = max^;
  let mem = Array.make(max + 1, 0);
  let mask = ref((0, 0));
  for (i in 0 to n - 1) {
    switch (data[i]) {
    | Mem(i, value) =>
      let (andWith, orWith) = mask^;
      mem[i] = value land andWith;
      mem[i] = mem[i] lor orWith;
    | Mask(andWith, orWith, _) => mask := (andWith, orWith)
    };
  };

  let sum = ref(0);
  for (i in 0 to Array.length(mem) - 1) {
    sum := sum^ + mem[i];
  };
  sum^;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (data, n) = prepareInput(lines);
  let mask = ref((0, "0"));
  let mem = ref(IntMap.empty);

  let rec setAll = (i, m, value, curr) => {
    let n = String.length(m);
    let pos = n - curr - 1;
    if (curr === n) {
      // print_endline(m);
      let i = i land int_of_string("0b" ++ m);
      // print_int(i);
      // print_string(",");
      // print_int(value);
      // print_newline();
      mem := IntMap.set(i, value, mem^);
      ();
    } else if (m.[pos] === 'X') {
      // print_endline("fork");
      // print_int(pos);
      // print_endline("");
      // Force X to be 0
      setAll(
        i,
        String.slice(0, pos, m) ++ "0" ++ String.sliceToEnd(pos + 1, m),
        value,
        curr + 1,
      );
      setAll(
        i,
        String.slice(0, pos, m) ++ "1" ++ String.sliceToEnd(pos + 1, m),
        value,
        curr + 1,
      );
    } else {
      setAll(i, m, value, curr + 1);
      ();
    };
  };

  for (i in 0 to n - 1) {
    switch (data[i]) {
    | Mem(i, value) =>
      let (orWith, m) = mask^;
      let i = i lor orWith;
      setAll(i, m, value, 0);
      ();
    | Mask(_, _, s) =>
      let orWith = int_of_string("0b" ++ String.replaceAll("X", "1", s));
      let s = String.replaceAll("0", "1", s);
      mask := (orWith, s);
    };
  };

  let sum = ref(0);
  let _ = IntMap.map(value => sum := sum^ + value, mem^);
  sum^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("14");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
