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
