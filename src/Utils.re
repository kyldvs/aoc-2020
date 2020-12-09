let root =
  Sys.executable_name
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname
  |> Filename.dirname
  |> Fp.absoluteExn;

let getInput = day => {
  let file = Fp.At.(root / "input" / ("day" ++ day ++ ".in"));
  let lines = Fs.readTextExn(file);
  let lines = Array.of_list(lines);
  lines;
};

let skip = "SKIPXXXABCDEFGH";

let skipBlankLines = lines => {
  let lines = List.map(line => line == "" ? skip : line, lines);
  lines;
};

let skipBlankLinesArr = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(line => line == "" ? skip : line, lines);
  let lines = Array.of_list(lines);
  lines;
};

let count = (fn, data) => {
  let answer = ref(0);
  let n = Array.length(data);
  for (i in 0 to n - 1) {
    if (fn(data[i])) {
      incr(answer);
    };
  };
  answer^;
};

let countList = (fn, data) => {
  let arr = Array.of_list(data);
  count(fn, arr);
};

let groupInput = lines => {
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
