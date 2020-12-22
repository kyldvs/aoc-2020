open Bread;
open Tools;

/*********************
 * Input preparation *
 *********************/

type t = {
  ingredients: Set.t,
  allergys: Set.t,
};

let prepareLines = lines => {
  let data = Input.extractLines("{} (contains {})", lines);
  let data =
    Array.map(
      parts => {
        let ingredients = String.split(" ", parts[0]) |> Set.fromList;
        let allergys = String.split(", ", parts[1]) |> Set.fromList;
        let _ = parts[2];
        {ingredients, allergys};
      },
      data,
    );
  let ingreds =
    Array.fold_left(
      (acc, {ingredients}) => acc @ Set.toList(ingredients),
      [],
      data,
    );
  let ingreds = ingreds |> Set.fromList;
  let allAllergys =
    Array.fold_left(
      (acc, {allergys}) => acc @ Set.toList(allergys),
      [],
      data,
    );
  let allAllergys = allAllergys |> Set.fromList;
  (data, ingreds, allAllergys);
};

/*******************
 * Part 1 Solution *
 *******************/

let part1Helper = lines => {
  let (data, ingreds, allAllergys) = prepareLines(lines);
  let iList = Set.toList(ingreds);
  // let n = Array.length(data);

  // ingredient => possible allergys
  let map = MMap.make();

  let ensureExists = (i, allergys) => {
    ();
    if (!MMap.hasKey(i, map)) {
      MMap.set(i, allergys, map);
    };
  };

  Array.iter(
    ({ingredients, allergys}) => {
      ingredients
      |> Set.toList
      |> List.iter(i => ensureExists(i, allAllergys))
    },
    data,
  );

  Array.iter(
    ({ingredients, allergys}) => {
      let ingredOnLine = i => Set.has(i, ingredients);
      let allergyOnLine = a => Set.has(a, allergys);

      List.iter(
        i =>
          if (MMap.hasKey(i, map) && !ingredOnLine(i)) {
            let curr = MMap.getExn(i, map);
            let next = Set.Impl.filter(a => !allergyOnLine(a), curr);
            MMap.set(i, next, map);
          },
        iList,
      );
    },
    data,
  );

  let count = i =>
    Array.fold_left(
      (acc, {ingredients}) => {
        let x = Set.has(i, ingredients) ? 1 : 0;
        acc + x;
      },
      0,
      data,
    );

  let sum = ref(0);
  map^
  |> Map.toList
  |> List.iter(((i, allergys)) => {
       ();
       if (Set.isEmpty(allergys)) {
         let ct = count(i);
         //  printf("  good: %s, %d\n", i, ct);
         sum := sum^ + ct;
       };
     });
  (map, sum^);
};

let part1 = lines => {
  let (_map, ans) = part1Helper(lines);
  ans;
};

/*******************
 * Part 2 Solution *
 *******************/

let part2 = lines => {
  let (map, _ans) = part1Helper(lines);
  let removeA = a => {
    let next: Map.t(Set.t) =
      Map.mapi((key, value) => Set.remove(a, value), map^);
    map := next;
  };

  let danger = MMap.make();
  let change = ref(true);
  while (change^) {
    change := false;
    Map.Impl.iter(
      (key, value) => {
        ();
        if (Set.size(value) === 1) {
          let a = value |> Set.toList |> List.hd;
          change := true;
          MMap.remove(key, map);
          removeA(a);
          MMap.set(key, a, danger);
        };
      },
      map^,
    );
  };

  let ans =
    danger^
    |> Map.toList
    |> List.sort((a, b) => compare(snd(a), snd(b)))
    |> List.map(fst)
    |> String.join(",");
  printf("Part 2: %s\n", ans);
  0;
};

/*****************
 * Main function *
 *****************/

let run = () => {
  let lines = Utils.getInput("21.test");
  Printf.printf("Part 1: %d\n", part1(lines));
  Printf.printf("Part 2: %d\n", part2(lines));
};
