open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let lines =
    Array.map(
      line => {
        let line = String.replaceAll("(", "( ", line);
        let line = String.replaceAll(")", " )", line);
        // printf("%s\n", line);
        String.split(" ", line) |> Array.of_list;
      },
      lines,
    );
  lines;
};

/*******************
 * Part 1 Solution *
 *******************/

let rec solve = (i, j, parts) => {
  let ans = ref(0);
  let parenStart = ref(-1);
  let parenCount = ref(0);
  let op = ref("?");
  let i = ref(i);

  let processValue = value => {
    ();
    if (op^ == "?") {
      if (ans^ != 0) {
        fatal("non zero");
      };
      ans := value;
    } else if (op^ == "+") {
      ans := ans^ + value;
      op := "?";
    } else if (op^ == "*") {
      ans := ans^ * value;
      op := "?";
    } else {
      fatal("bad op");
    };
  };

  while (i^ <= j) {
    if (parts[i^] == "(") {
      if (parenCount^ === 0) {
        parenStart := i^;
      };
      incr(parenCount);
    } else if (parts[i^] == ")") {
      decr(parenCount);
      if (parenCount^ === 0) {
        let subAns = solve(parenStart^ + 1, i^ - 1, parts);
        processValue(subAns);
        parenStart := (-1);
      };
    } else if (parenCount^ === 0) {
      if (parts[i^] == "+") {
        op := "+";
      } else if (parts[i^] == "*") {
        op := "*";
      } else {
        processValue(Parse.int(parts[i^]));
      };
    };

    incr(i);
  };

  ans^;
};

let part1 = lines => {
  let data = prepareLines(lines);
  let n = Array.length(data);
  let sum = ref(0);
  for (i in 0 to n - 1) {
    // printf("%s\n", String.concat("", data[i] |> Array.to_list));
    let eqLen = Array.length(data[i]);
    sum := sum^ + solve(0, eqLen - 1, data[i]);
  };
  sum^;
};

/*******************
 * Part 2 Solution *
 *******************/

let rec addParens = eq => {
  // printf("Start: %s\n", String.concat("", eq));
  let ct = ref(0);
  let ans = ref([]);
  let sub = ref([]);
  let endCt = ref(0);
  List.iter(
    part =>
      if (part == "(") {
        if (ct^ == 0) {
          if (endCt^ === 0) {
            ans := ans^ @ ["("];
            incr(endCt);
          };
        };
        if (ct^ > 0) {
          sub := sub^ @ [part];
        };
        incr(ct);
        // printf("%d\n", ct^);
      } else if (part == ")") {
        decr(ct);
        if (ct^ > 0) {
          sub := sub^ @ [part];
        };
        // printf("%d\n", ct^);
        if (ct^ === 0) {
          // printf("ENDED\n");
          ans := ans^ @ ["("] @ addParens(sub^) @ [")"];
          sub := [];
        };
      } else if (ct^ === 0) {
        if (part == "*") {
          while (endCt^ > 0) {
            ans := ans^ @ [")"];
            decr(endCt);
          };
          ans := ans^ @ ["*"];
        } else if (part == "+") {
          ans := ans^ @ ["+"];
        } else {
          if (endCt^ === 0) {
            ans := ans^ @ ["("];
            incr(endCt);
          };
          ans := ans^ @ [part];
        };
      } else if (ct^ > 0) {
        sub := sub^ @ [part];
      },
    eq,
  );

  while (endCt^ > 0) {
    ans := ans^ @ [")"];
    decr(endCt);
  };

  let ans = CamlList.isEmpty(sub^) ? ans^ : ans^ @ ["("] @ sub^ @ [")"];
  // printf("End: %s\n", String.concat("", ans));
  ans;
};

let part2 = lines => {
  let data = prepareLines(lines);
  let n = Array.length(data);
  let sum = ref(0);
  for (i in 0 to n - 1) {
    let eq = addParens(Array.to_list(data[i]));
    // 5*(9*((7*(3 *(3+9*(3 + (8 + 6 * 4))))))
    // printf("%s\n", String.concat("", eq));
    let eq = Array.of_list(eq);
    let eqLen = Array.length(eq);
    sum := sum^ + solve(0, eqLen - 1, eq);
  };
  sum^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("18");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
