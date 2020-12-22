open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

let prepareLines = lines => {
  let groups = Input.groups(lines);
  let p1 = Parse.intArr(arrayRemoveFirst(groups[0])) |> Array.to_list;
  let p2 = Parse.intArr(arrayRemoveFirst(groups[1])) |> Array.to_list;
  (p1, p2);
};

/*******************
 * Part 1 Solution *
 *******************/

let part1 = lines => {
  // let (p1, p2) = prepareLines(lines);
  // // printf("%s\n", Print.intList(p1));
  // // printf("%s\n", Print.intList(p2));
  // let p1 = ref(p1);
  // let p2 = ref(p2);
  // let winner = ref([]);
  // let break = ref(false);
  // while (! break^) {
  //   if (CamlList.isEmpty(p1^)) {
  //     break := true;
  //     winner := p2^;
  //   } else if (CamlList.isEmpty(p2^)) {
  //     break := true;
  //     winner := p1^;
  //   } else {
  //     let (p1n, p2n) =
  //       switch (p1^, p2^) {
  //       | ([a, ...p1], [b, ...p2]) when a > b => (p1 @ [a, b], p2)
  //       | ([a, ...p1], [b, ...p2]) when b > a => (p1, p2 @ [b, a])
  //       | _ => fatal("invalid cards")
  //       };
  //     p1 := p1n;
  //     p2 := p2n;
  //   };
  // };
  // let m = ref(1);
  // let sum = ref(0);
  // winner^
  // |> List.rev
  // |> List.iter(c => {
  //      sum := sum^ + m^ * c;
  //      incr(m);
  //    });
  0;
  // sum^;
};

/*******************
 * Part 2 Solution *
 *******************/

let takeN = (n, l) => {
  let curr = ref(l);
  let res = ref([]);
  for (i in 0 to n - 1) {
    res := [List.hd(curr^), ...res^];
    curr := List.tl(curr^);
  };
  List.rev(res^);
};

let part2 = lines => {
  let (p1, p2) = prepareLines(lines);

  // Encode the game into a string to prevent infinite recursion.
  let en = (l1, l2) => Print.intList(l1 @ l2);

  // Some debugging to help print things out.
  let game = ref(1);
  let debugStartGame = () => {
    printf("Game %d:\n", game^);
    incr(game);
    game^ - 1;
  };
  let debugEndGame = game => {
    printf("Game %d End\n", game);
  };

  let latestWinner = ref([]);
  let rec getWinner = (game, p1, p2, v) => {
    ();
    // printf("  P1 %s\n", Print.intList(p1));
    if (Set.has(en(p1, p2), v)) {
      debugEndGame(game);
      latestWinner := p1;
      1;
    } else {
      // Add the previous configuration to visited to check in the following
      // rounds. This does not get checked in sub-games.
      let v = Set.add(en(p1, p2), v);

      if (CamlList.isEmpty(p1)) {
        // Player 1 has an empty deck, so player 2 wins.
        debugEndGame(game);
        latestWinner := p2;
        2;
      } else if (CamlList.isEmpty(p2)) {
        // Player 2 has an empty deck, so player 1 wins.
        debugEndGame(game);
        latestWinner := p1;
        1;
      } else {
        // Draw a card.
        let f1 = List.hd(p1);
        let f2 = List.hd(p2);
        let p1 = List.tl(p1);
        let p2 = List.tl(p2);

        // Figure out the result.
        let res =
          if (List.length(p1) < f1 || List.length(p2) < f2) {
            // The winner of the round is the player with the higher value card.
            if (f1 > f2) {
              getWinner(game, p1 @ [f1, f2], p2, v);
            } else {
              getWinner(game, p1, p2 @ [f2, f1], v);
            };
          } else {
            // The winner of the round is determined by the subgame.

            // Recursive sub-game.
            let p1Sub = takeN(f1, p1);
            let p2Sub = takeN(f2, p2);

            // Recurse on the copies of the sub decks, with a fresh
            // visited list.
            let id = getWinner(debugStartGame(), p1Sub, p2Sub, Set.empty);

            // ID won this round, we keep playing where the winner gets the
            // two cards with their own card above the loser's card on the
            // bottom of their deck.
            if (id === 1) {
              getWinner(game, p1 @ [f1, f2], p2, v);
            } else if (id === 2) {
              getWinner(game, p1, p2 @ [f2, f1], v);
            } else {
              fatal("bad winner");
            };
          };

        res;
      };
    };
  };

  let _ = getWinner(debugStartGame(), p1, p2, Set.empty);

  // This will contain the deck of the most recent winner, which will be
  // after the first recursive call ends, Game 1 in this case.
  let winner = latestWinner^;

  // Add up the score for the winner.
  let m = ref(1);
  let sum = ref(0);
  winner
  |> List.rev
  |> List.iter(c => {
       sum := sum^ + m^ * c;
       incr(m);
     });

  sum^;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("22");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
