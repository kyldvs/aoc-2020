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
