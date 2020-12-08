open Bread;

let prepareInput = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(String.replaceAll("+", ""), lines);
  let lines =
    List.map(
      line => {
        let parts = String.split(" ", line);
        (List.hd(parts), int_of_string(List.hd(List.tl(parts))));
      },
      lines,
    );
  let lines = Array.of_list(lines);
  lines;
};

let part1 = lines => {
  let lines = prepareInput(lines);
  let n = Array.length(lines);
  let v = Array.make(n, false);
  let acc = ref(0);
  let break = ref(false);
  let i = ref(0);
  while (! break^) {
    if (v[i^]) {
      break := true;
      ();
    } else {
      v[i^] = true;
      switch (lines[i^]) {
      | ("nop", _) =>
        incr(i);
        ();
      | ("acc", value) =>
        acc := acc^ + value;
        incr(i);
        ();
      | ("jmp", value) =>
        i := i^ + value;
        ();
      | _ => failwith("invalid instruction")
      };
    };
  };
  acc^;
};

let part2 = lines => {
  let lines = prepareInput(lines);
  let n = Array.length(lines);
  let copy = i => {
    let arr = Array.init(n, x => lines[x]);
    let next =
      switch (arr[i]) {
      | ("jmp", value) => ("nop", value)
      | ("nop", value) => ("jmp", value)
      | x => x
      };
    arr[i] = next;
    arr;
  };

  let attempt = lines => {
    let n = Array.length(lines);
    let v = Array.make(n, false);
    let acc = ref(0);
    let break = ref(false);
    let i = ref(0);
    while (! break^ && i^ < n) {
      if (v[i^]) {
        break := true;
        ();
      } else {
        v[i^] = true;
        switch (lines[i^]) {
        | ("nop", _) =>
          incr(i);
          ();
        | ("acc", value) =>
          acc := acc^ + value;
          incr(i);
          ();
        | ("jmp", value) =>
          i := i^ + value;
          ();
        | _ => failwith("invalid instruction")
        };
      };
    };
    (! break^, acc^);
  };

  let i = ref(0);
  let break = ref(false);
  let result = ref(-1);
  while (! break^ && i^ < n) {
    let arr = copy(i^);
    let (finished, acc) = attempt(arr);
    if (finished) {
      result := acc;
      break := true;
    };
    incr(i);
  };
  result^;
};

let run = () => {
  let lines = Utils.getInput("08");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
