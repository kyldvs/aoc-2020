type t = ref(list(string));

let make =
    (~tokens: list(char)=[' ', '\n', ',', ':', '-'], lines: list(string))
    : t => {
  let result =
    List.fold_left(
      (lines, token) => {
        let lines = List.map(String.split_on_char(token), lines);
        let lines = List.flatten(lines);
        lines;
      },
      lines,
      tokens,
    );
  let result = List.filter(s => s != "", result);
  ref(result);
};

let makeArr =
    (~tokens: list(char)=[' ', '\n', ',', ':', '-'], lines: array(string))
    : t => {
  make(~tokens, Array.to_list(lines));
};

let nextLength = (l: int, t: t): string => {
  if (l <= 0) {
    failwith("Must request a postive length.");
  };
  switch (t^) {
  | [hd, ...rest] =>
    let n = String.length(hd);
    let (result, next) =
      if (n < l) {
        failwith("Next token does not have enought length.");
      } else if (n === l) {
        (hd, rest);
      } else {
        (
          Bread.String.slice(0, l, hd),
          [Bread.String.sliceToEnd(l, hd), ...rest],
        );
      };
    t := next;
    result;
  | [] => failwith("No more elements")
  };
};

let nextChar = (t: t): char => {
  switch (t^) {
  | [hd, ...rest] =>
    let n = String.length(hd);
    let (result, next) =
      if (n === 0) {
        failwith("Unreachable, empty strings should be filtered already.");
      } else if (n === 1) {
        (hd.[0], rest);
      } else {
        (hd.[0], [Bread.String.sliceToEnd(1, hd), ...rest]);
      };
    t := next;
    result;
  | [] => failwith("No more elements")
  };
};

let nextString = (t: t): string => {
  switch (t^) {
  | [hd, ...rest] =>
    t := rest;
    hd;
  | [] => failwith("No more elements")
  };
};

let nextInt = (t: t): int => {
  switch (t^) {
  | [hd, ...rest] =>
    t := rest;
    int_of_string(hd);
  | [] => failwith("No more elements")
  };
};

let nextIntOpt = (~consume=false, t: t): option(int) => {
  switch (t^) {
  | [hd, ...rest] =>
    let result =
      try(Some(int_of_string(hd))) {
      | _ => None
      };
    let () =
      switch (result, consume) {
      | (Some(_), _) =>
        t := rest;
        ();
      | (None, true) =>
        t := rest;
        ();
      | (None, false) => ()
      };
    result;
  | [] => failwith("No more elements")
  };
};

let hasNext = (t: t): bool => {
  switch (t^) {
  | [hd, ...rest] => true
  | [] => false
  };
};

let foldLeftConsuming = (fn, initial, t: t) => {
  let curr = ref(initial);
  while (hasNext(t)) {
    let s = nextString(t);
    curr := fn(curr^, s);
  };
  curr^;
};
