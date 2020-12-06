let skip_token = "SKIPXXXABCDEFGH";

let arr = () => Array.init(26, _ => false);
let arr2 = () => Array.init(26, _ => 0);

let countTrue = arr => {
  let count = ref(0);
  let n = Array.length(arr);
  for (i in 0 to n - 1) {
    if (arr[i]) {
      incr(count);
    };
  };
  count^;
};

let countN = (value, arr) => {
  let count = ref(0);
  let n = Array.length(arr);
  for (i in 0 to n - 1) {
    if (arr[i] === value) {
      incr(count);
    };
  };
  count^;
};

let part1 = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(s => s == "" ? skip_token : s, lines);
  let s = Scanner.make(lines);
  let result = ref(0);
  let group = arr();
  let reset = () => {
    let n = Array.length(group);
    for (i in 0 to n - 1) {
      group[i] = false;
    };
  };
  while (Scanner.hasNext(s)) {
    let line = Scanner.nextString(s);
    if (line == skip_token) {
      result := result^ + countTrue(group);
      reset();
    } else {
      let n = String.length(line);
      for (i in 0 to n - 1) {
        group[int_of_char(line.[i]) - int_of_char('a')] = true;
      };
    };
  };
  result := result^ + countTrue(group);
  result^;
};

let part2 = lines => {
  let lines = Array.to_list(lines);
  let lines = List.map(s => s == "" ? skip_token : s, lines);
  let s = Scanner.make(lines);
  let result = ref(0);
  let group = arr2();
  let groupCount = ref(0);
  let reset = () => {
    let n = Array.length(group);
    for (i in 0 to n - 1) {
      group[i] = 0;
    };
  };
  while (Scanner.hasNext(s)) {
    let line = Scanner.nextString(s);
    if (line == skip_token) {
      result := result^ + countN(groupCount^, group);
      groupCount := 0;
      reset();
    } else {
      incr(groupCount);
      let n = String.length(line);
      for (i in 0 to n - 1) {
        let index = int_of_char(line.[i]) - int_of_char('a');
        group[index] = group[index] + 1;
      };
    };
  };
  result := result^ + countN(groupCount^, group);
  result^;
};

let run = () => {
  let lines = Utils.getInput("06");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
