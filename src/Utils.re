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
  let file = Fp.At.(root / "input" / ("day" ++ string_of_int(day) ++ ".in"));
  let lines = Fs.readTextExn(file);
  let lines = Array.of_list(lines);
  lines;
};

let getTestInput = day => {
  let file =
    Fp.At.(root / "input" / ("day" ++ string_of_int(day) ++ ".test.in"));
  let lines = Fs.readTextExn(file);
  let lines = Array.of_list(lines);
  lines;
};
