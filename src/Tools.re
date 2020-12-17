open Bread;

let printf = Printf.printf;
let sprintf = Printf.sprintf;
let fatal = s => failwith("[FATAL] See: " ++ s);

let arraySome = (fn, arr) => {
  let some = ref(false);
  let i = ref(0);
  let n = Array.length(arr);
  while (! some^ && i^ < n) {
    if (fn(arr[i^])) {
      some := true;
    };
    incr(i);
  };
  some^;
};

let arrayAll = (fn, arr) => {
  let all = ref(true);
  let i = ref(0);
  let n = Array.length(arr);
  while (all^ && i^ < n) {
    if (!fn(arr[i^])) {
      all := false;
    };
    incr(i);
  };
  all^;
};

let arrayRemoveFirst = arr => arr |> Array.to_list |> List.tl |> Array.of_list;

module Print = {
  let int = string_of_int;
  let bool = string_of_bool;
};

module Parse = {
  let int = int_of_string;
  let bool = bool_of_string;
  let intList = List.map(int_of_string);
};

module Input = {
  let ints = lines => {
    let lines = Array.map(int_of_string, lines);
    lines;
  };

  /**
   * group(lines)
   *
   * groups lines together that are separated by a single blank line.
   */
  let groups = lines => {
    let isSeparator = line => line == "";
    let n = Array.length(lines);
    let all = ref([]);
    let curr = ref([]);
    for (i in 0 to n - 1) {
      if (isSeparator(lines[i])) {
        all := [List.rev(curr^), ...all^];
        curr := [];
      } else {
        curr := [lines[i], ...curr^];
      };
    };
    let () =
      switch (curr^) {
      | [] => ()
      | _ => all := [List.rev(curr^), ...all^]
      };
    let listOfLists = List.rev(all^);
    let listOfArrays = List.map(Array.of_list, listOfLists);
    let arrayOfArrays = Array.of_list(listOfArrays);
    arrayOfArrays;
  };

  /**
   * extract(s, lines)
   *
   * s defines how the contents of each line should be extracted into tokens
   *
   *   - {} captures a token
   *   - anything else must exist and will be removed from the output
   *
   * extract("{}, {}, and {}", [| "one, two, and three" |])
   *   - Outputs: [| "one", "two", "three" |]
   */
  let extract = s => {
    if (String.contains("{}{}", s)) {
      failwith(
        "Invalid pattern while using extract: '{}{}'. This does not "
        ++ "make sense and should be simplified to a single '{}'.",
      );
    };

    let parts = s |> String.split("{}") |> Array.of_list;
    // This is how many occurences of "{}" there are.
    let n = Array.length(parts) - 1;
    let extractLine = line => {
      let cons = ref(line);
      let tokens = Array.make(n, "");
      for (i in 0 to n - 1) {
        // Need to consume the first part.
        if (i === 0) {
          let l = String.indexOfInt(parts[i], cons^);
          let partLen = String.length(parts[i]);
          if (l === (-1)) {
            failwith(
              "Could not find string '"
              ++ parts[i]
              ++ "' present in line: '"
              ++ line
              ++ "'",
            );
          };
          cons := String.sliceToEnd(l + partLen, cons^);
        };

        // Take the rest of it. Implicitly, i = n - 1, or the string contains
        // {}{}, which should be caught earlier.
        if (parts[i + 1] == "") {
          if (i !== n - 1) {
            failwith("How did you get here? Pattern might contain: '{}{}'");
          };
          tokens[i] = cons^;
          // So the last check doesn't fail.
          cons := "";
        } else {
          let r = String.indexOfInt(parts[i + 1], cons^);
          let partLen = String.length(parts[i + 1]);
          if (r === (-1)) {
            failwith(
              "Could not find string '"
              ++ parts[i + 1]
              ++ "' present in line: '"
              ++ line
              ++ "'",
            );
          };
          tokens[i] = String.slice(0, r, cons^);
          cons := String.sliceToEnd(r + partLen, cons^);
        };
      };

      if (cons^ != "") {
        failwith(
          "Extra contents, '"
          ++ cons^
          ++ "', left at end of line: '"
          ++ line
          ++ "'",
        );
      };

      tokens;
    };

    extractLine;
  };

  let extractLines = (s, lines) => Array.map(extract(s), lines);
};
