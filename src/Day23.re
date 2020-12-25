open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let data =
    String.toCharArray(lines[0])
    |> Array.map(c => int_of_char(c) - int_of_char('0'))
    |> Array.to_list;
  data;
};

module Game = {
  type n = {
    value: int,
    mutable next: n,
    mutable prev: n,
  };

  type t = {
    nodes: array(n),
    mutable curr: n,
  };

  let print = (~count=9, t) => {
    let curr = ref(t.curr);
    let s = ref([]);
    for (i in 1 to count) {
      s := [Print.int(curr^.value), ...s^];
      curr := curr^.next;
    };
    "[" ++ String.join(", ", List.rev(s^)) ++ "]";
  };

  let make = (values, max) => {
    let initial = Array.of_list(values);
    let initialLen = Array.length(initial);

    // Create the nodes.
    let nodes =
      Array.init(
        max + 1,
        i => {
          let value =
            if (i >= 1 && i <= initialLen) {
              initial[i - 1];
            } else {
              i;
            };
          let rec node = {value, next: node, prev: node};
          node;
        },
      );

    // Set up the list.
    for (i in 2 to max) {
      let head = nodes[1];
      if (head.value === head.prev.value) {
        head.prev = nodes[i];
        head.next = nodes[i];
        nodes[i].prev = head;
        nodes[i].next = head;
      } else {
        head.prev.next = nodes[i];
        nodes[i].prev = head.prev;
        nodes[i].next = head;
        head.prev = nodes[i];
      };
    };

    // Correct the initial nodes' indices.
    let initial = ref([]);
    for (i in 1 to initialLen) {
      initial := [nodes[i], ...initial^];
    };
    List.iter(n => nodes[n.value] = n, initial^);

    // Return the game.
    {curr: nodes[List.hd(values)], nodes};
  };

  let turn = t => {
    let curr = t.curr;

    let a = curr.next;
    let b = curr.next.next;
    let c = curr.next.next.next;

    // Remove a, b, and c.
    let d = curr.next.next.next.next;
    d.prev = curr;
    curr.next = d;

    // Find dest.
    let dest = ref(curr.value - 1);
    while (dest^ < 1
           || dest^ === a.value
           || dest^ === b.value
           || dest^ === c.value) {
      decr(dest);
      if (dest^ < 1) {
        dest := Array.length(t.nodes) - 1;
      };
    };
    let dest = t.nodes[dest^];
    let destNext = dest.next;

    // Add a, b, and c.
    dest.next = a;
    a.prev = dest;
    c.next = destNext;
    destNext.prev = c;

    // Update curr.
    t.curr = curr.next;
  };
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  let data = prepareLines(lines);
  let game = Game.make(data, 9);

  printf("Start: %s\n", Game.print(game));
  flush_all();

  let turns = 100;
  for (i in 1 to turns) {
    Game.turn(game);
    // printf("Turn %d: %s\n", i, Game.print(game));
    // flush_all();
  };

  let curr = ref(game.nodes[1]);
  let ans =
    List.init(
      8,
      _ => {
        curr := curr^.next;
        curr^.value;
      },
    );
  let ans = List.map(Print.int, ans);
  int_of_string(String.join("", ans));
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let data = prepareLines(lines);
  let game = Game.make(data, 1_000_000);

  printf("Start: %s\n", Game.print(game));
  flush_all();

  let turns = 10_000_000;
  for (i in 1 to turns) {
    Game.turn(game);
    // printf("Turn %d: %s\n", i, Game.print(game));
    // flush_all();
  };

  let x = game.nodes[1].next.value;
  let y = game.nodes[1].next.next.value;
  printf("  %d * %d\n", x, y);
  x * y;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("23");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
