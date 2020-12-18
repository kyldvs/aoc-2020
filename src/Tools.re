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

module Math = {
  let rec pow = (a, b) => {
    switch (a) {
    | 0 => 1
    | 1 => a
    | n =>
      let b = pow(a, n / 2);
      let c = n mod 2 === 0 ? 1 : a;
      b * b * c;
    };
  };
};

module RPN = {
  type assoc =
    | Left
    | Right;

  // All operators are binary for now.
  type op = (int, int) => int;

  type result =
    | Int(int)
    | Op(string, op);

  module type Config = {let config: Map.t((int, assoc, op));};

  module Default = {
    let config =
      Map.fromList([
        ("^", (4, Right, Math.pow)),
        ("*", (3, Left, ( * ))),
        ("/", (3, Left, (/))),
        ("+", (2, Left, (+))),
        ("-", (2, Left, (-))),
      ]);
  };

  let debug = parsed => {
    String.join(
      " ",
      List.map(
        out => {
          switch (out) {
          | Int(value) => string_of_int(value)
          | Op(token, _) => token
          }
        },
        parsed,
      ),
    );
  };

  module Make = (Config: Config) => {
    let config = Config.config;

    /**
     * An implementation of the shunting-yard algorithm.
     */
    let parse = tokens => {
      let config = config;
      let getMatch = Map.get(_, config);
      let getMatchExn = Map.getExn(_, config);
      let tokenToOp = token => {
        let (_, _, op) = getMatchExn(token);
        Op(token, op);
      };

      let out = ref([]);
      let stack = ref([]);

      List.iter(
        token => {
          switch (getMatch(token)) {
          | Some((precedence, assoc, op)) =>
            try({
              let break = ref(false);
              while (! break^) {
                let token2 = List.hd(stack^);
                let (p2, a2, op2) = getMatchExn(token2);
                if (p2 > precedence || p2 === precedence && assoc == Left) {
                  out := [Op(token2, op2), ...out^];
                  stack := List.tl(stack^);
                } else {
                  break := true;
                };
              };
            }) {
            | _ => ()
            };
            stack := [token, ...stack^];
          | None =>
            switch (token) {
            | "(" => stack := ["(", ...stack^]
            | ")" =>
              try(
                {
                  while (List.hd(stack^) != "(") {
                    out := [tokenToOp(List.hd(stack^)), ...out^];
                    stack := List.tl(stack^);
                  };
                  // Remove the open paren
                  stack := List.tl(stack^);
                }
              ) {
              | e => failwith("Mismatched parenthesis. Extra )")
              }
            | token => out := [Int(int_of_string(token)), ...out^]
            }
          }
        },
        tokens,
      );
      List.rev(out^) @ List.map(tokenToOp, stack^);
    };

    let solve = parsed => {
      let n = ref(0);
      let values = ref([]);
      List.iter(
        out => {
          switch (out) {
          | Int(value) =>
            values := [value, ...values^];
            incr(n);
          | Op(_key, op) =>
            if (n^ < 2) {
              failwith(
                "Invalid equation. Not enough values for next operator.",
              );
            };
            let b = List.hd(values^);
            let a = List.hd(List.tl(values^));
            let next = op(a, b);
            values := [next, ...List.tl(List.tl(values^))];
            decr(n);
          }
        },
        parsed,
      );

      if (n^ != 1) {
        failwith(
          "Invalid equation. Did not resolve to single value. Not enough operators.",
        );
      };

      List.hd(values^);
    };

    let run = s => solve(parse(s));

    let debug = debug;
  };
};
