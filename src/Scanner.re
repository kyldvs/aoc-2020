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

let hasNext = (t: t): bool => {
  switch (t^) {
  | [hd, ...rest] => true
  | [] => false
  };
};
