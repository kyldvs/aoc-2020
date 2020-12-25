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

let part1 = lines => {
  let data = prepareLines(lines);

  let cardLoopSize = ref(0);
  let card = ref(1);
  while (card^ !== data[0]) {
    incr(cardLoopSize);
    card := card^ * 7 mod 20201227;
    // printf("card: %d\n", card^);
    // printf("cardLoopSize: %d\n", cardLoopSize^);
    // flush_all();
  };

  printf("card: %d\n", card^);
  printf("cardLoopSize: %d\n", cardLoopSize^);
  flush_all();

  let doorLoopSize = ref(0);
  let door = ref(1);
  while (door^ !== data[1]) {
    incr(doorLoopSize);
    door := door^ * 7 mod 20201227;
    // printf("door: %d\n", door^);
    // printf("doorLoopSize: %d\n", doorLoopSize^);
    // flush_all();
  };

  printf("door: %d\n", door^);
  printf("doorLoopSize: %d\n", doorLoopSize^);
  flush_all();

  let eKey = ref(1);
  for (i in 1 to doorLoopSize^) {
    eKey := eKey^ * data[0] mod 20201227;
  };

  eKey^;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  0;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("25");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
