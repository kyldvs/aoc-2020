open Bread;
open Tools;

let whileLoop = () => {
  let break = ref(false);

  let ans = ref(0);
  let i = ref(0);

  let con = () => i^ < 10;

  let update = () => {
    incr(i);
    incr(ans);
  };

  while (! break^ && con()) {
    update();
  };
  ans^;
};

let sumArray = () => {
  let arr = [|"1", "2", "3"|];

  let fn = el => {
    Parse.int(el);
  };
  Array.fold_left((sum, el) => sum + fn(el), 0, arr);
};

let sumList = () => {
  let list = ["1", "2", "3"];

  let fn = el => {
    Parse.int(el);
  };
  List.fold_left((sum, el) => sum + fn(el), 0, list);
};

module GraphExamples = Day07Clean;

let prepareLines = lines => {
  // Replace things
  let lines =
    Array.map(
      line => {
        let line = String.replaceAll("(", "( ", line);
        let line = String.replaceAll(")", " )", line);
        line;
      },
      lines,
    );

  // Split things
  let lines =
    Array.map(
      line => {
        let line = String.split(" ", line);
        let line = Array.of_list(line);
        line;
      },
      lines,
    );

  // Reset
  let _ = lines;
  let lines = [||];

  // Parse things
  let lines = Parse.intArr(lines);

  // Reset
  let _ = lines;
  let lines = [||];

  // Extract things on each line
  let data = Input.extractLines("{}: {}-{} or {}-{}", lines);
  let data =
    Array.map(
      parts => {
        let x = parts[1] ++ parts[2];
        x;
      },
      data,
    );

  let _ = data;
  ();
};
