open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let data = Input.ints(lines);
  data;
};

/*******************
 * Part 1 Solution *
 *******************/

let buildRules = arr => {
  let n = Array.length(arr);
  let fail = (s, i) => (false, i);
  let rules = Array.init(1000, _ => fail);
  for (i in 0 to n - 1) {
    let line = String.replaceAll("\"", "", arr[i]);
    let parts = String.split(": ", line);
    let index = Parse.int(List.hd(parts));
    let content = List.hd(List.tl(parts));

    let rule =
      if (content == "a") {
        (s, i) => {
          let len = String.length(s);
          let good = i >= 0 && i < len && s.[i] === 'a';
          (good, i + 1);
        };
      } else if (content == "b") {
        (s, i) => {
          let len = String.length(s);
          let good = i >= 0 && i < len && s.[i] === 'b';
          (good, i + 1);
        };
      } else {
        let parts = String.split(" | ", content);
        let parts =
          List.map(
            s => String.split(" ", s) |> Array.of_list |> Parse.intArr,
            parts,
          )
          |> Array.of_list;

        (s, i) => {
          let res = ref((false, i));
          Array.iter(
            parts => {
              let curr = ref(i);
              let good = ref(true);
              Array.iter(
                r =>
                  if (good^) {
                    let (stillGood, next) = (rules[r])(s, curr^);
                    if (!stillGood) {
                      good := false;
                    } else {
                      curr := next;
                    };
                  },
                parts,
              );
              if (good^) {
                res := (good^, curr^);
              };
              ();
            },
            parts,
          );
          res^;
        };
      };

    rules[index] = rule;
  };
  rules;
};

let part1 = lines => {
  let groups = Input.groups(lines);
  let rules = buildRules(groups[0]);
  let test = (rule, s) => {
    let (match, i) = (rules[0])(s, 0);
    match && i === String.length(s);
  };
  Utils.count(test(rules[0]), groups[1]);
};

/*******************
 * Part 2 Solution *
 *******************/

let buildRules2 = arr => {
  let n = Array.length(arr);
  let fail = (s, i) => [];
  let rules = Array.init(1000, _ => fail);
  for (i in 0 to n - 1) {
    let line = String.replaceAll("\"", "", arr[i]);
    let parts = String.split(": ", line);
    let index = Parse.int(List.hd(parts));
    let content = List.hd(List.tl(parts));
    let content =
      if (index === 8) {
        let cs = ref([]);
        let curr = ref("42");
        let add = () => curr := curr^ ++ " 42";
        for (x in 0 to 30) {
          cs := [curr^, ...cs^];
          add();
        };
        String.join(" | ", List.rev(cs^));
      } else if (index === 11) {
        let cs = ref([]);
        let curr = ref("42 31");
        let add = () => curr := "42 " ++ curr^ ++ " 31";
        for (x in 0 to 30) {
          cs := [curr^, ...cs^];
          add();
        };
        String.join(" | ", List.rev(cs^));
      } else {
        content;
      };

    let rule =
      if (content == "a") {
        (s, i) => {
          let len = String.length(s);
          let good = i >= 0 && i < len && s.[i] === 'a';
          if (good) {
            [i + 1];
          } else {
            [];
          };
        };
      } else if (content == "b") {
        (s, i) => {
          let len = String.length(s);
          let good = i >= 0 && i < len && s.[i] === 'b';
          if (good) {
            [i + 1];
          } else {
            [];
          };
        };
      } else {
        let parts = String.split(" | ", content);
        let parts =
          List.map(
            s => String.split(" ", s) |> Array.of_list |> Parse.intArr,
            parts,
          )
          |> Array.of_list;

        (s, i) => {
          let res = ref([]);
          Array.iter(
            parts => {
              let curr = ref([i]);
              Array.iter(
                r => {
                  let next = ref([]);
                  List.iter(
                    iToTest => {
                      let rs = (rules[r])(s, iToTest);
                      next := next^ @ rs;
                      ();
                    },
                    curr^,
                  );
                  curr := next^;
                },
                parts,
              );
              res := res^ @ curr^;
            },
            parts,
          );
          res^;
        };
      };

    rules[index] = rule;
  };
  rules;
};

let part2 = lines => {
  let groups = Input.groups(lines);
  let rules = buildRules2(groups[0]);
  let test = (rule, s) => {
    let sLen = String.length(s);
    let indices = (rules[0])(s, 0);
    List.exists(i => i === sLen, indices);
  };
  Utils.count(test(rules[0]), groups[1]);
};

let part1 = lines =>
  try(part1(lines)) {
  | e =>
    let raw = Printexc.get_callstack(10);
    print_endline(Printexc.raw_backtrace_to_string(raw));
    raise(e);
  };

let part2 = lines =>
  try(part2(lines)) {
  | e =>
    let raw = Printexc.get_callstack(10);
    print_endline(Printexc.raw_backtrace_to_string(raw));
    raise(e);
  };

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("19");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
