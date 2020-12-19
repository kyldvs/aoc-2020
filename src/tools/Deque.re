module Config = DequeCore;

type t('el) = DequeCore.t('el);

let init = (count, f) =>
  Caml.Array.init(count, f) |> CamlArrayCore.toList |> Config.fromList;

let toCamlList = ds => ds |> Config.toList |> CamlListCore.fromList;
let fromCamlList = ds => ds |> CamlListCore.toList |> Config.fromList;

let toCamlArray = ds => ds |> Config.toList |> CamlArrayCore.fromList;
let fromCamlArray = ds => ds |> CamlArrayCore.toList |> Config.fromList;

let toDeque = ds => ds |> Config.toList |> DequeCore.fromList;
let fromDeque = ds => ds |> DequeCore.toList |> Config.fromList;

let everyi = (fn, ds) => {
  let list = Config.toList(ds);
  let list = ref(list);
  let result = ref(true);
  let i = ref(0);
  while (result^ && !CamlListCore.isEmpty(list^)) {
    switch (list^) {
    | [hd, ...rest] =>
      list := rest;
      if (!fn(i^, hd)) {
        result := false;
      };
      incr(i);
    | _ => raise(Exceptions.InternalError("FeatureSequence.everyi"))
    };
  };
  result^;
};

let every = (fn, ds) => everyi((i, el) => fn(el), ds);

let somei = (fn, ds) => {
  let list = Config.toList(ds);
  let list = ref(list);
  let result = ref(false);
  let i = ref(0);
  while (! result^ && !CamlListCore.isEmpty(list^)) {
    switch (list^) {
    | [hd, ...rest] =>
      list := rest;
      if (fn(i^, hd)) {
        result := true;
      };
      incr(i);
    | _ => raise(Exceptions.InternalError("FeatureSequence.somei"))
    };
  };
  result^;
};

let some = (fn, ds) => somei((i, el) => fn(el), ds);

let nonei = (fn, ds) => everyi((i, el) => !fn(i, el), ds);

let none = (fn, ds) => nonei((i, el) => fn(el), ds);

let forEachi = (fn, ds) => {
  let list = Config.toList(ds);
  Caml.List.iteri(fn, list);
};

let forEach = (fn, ds) => forEachi((i, el) => fn(el), ds);

let reverse = Config.reverse;

let reducei = (fn, initialValue, ds) => {
  let list = Config.toList(ds);
  let list = ref(list);
  let result = ref(initialValue);
  let i = ref(0);
  while (!CamlListCore.isEmpty(list^)) {
    switch (list^) {
    | [hd, ...rest] =>
      list := rest;
      result := fn(result^, i^, hd);
      incr(i);
    | _ => raise(Exceptions.InternalError("FeatureSequence.reducei"))
    };
  };
  result^;
};

let reduce = (fn, initialValue, ds) =>
  reducei((acc, i, el) => fn(acc, el), initialValue, ds);

let reduceReversei = (fn, initialValue, ds) => {
  let list = Config.toList(ds);
  let list = Caml.List.rev(list);
  let list = ref(list);
  let result = ref(initialValue);
  let i = Caml.List.length(list^) - 1;
  let i = ref(i);
  while (!CamlListCore.isEmpty(list^)) {
    switch (list^) {
    | [hd, ...rest] =>
      list := rest;
      result := fn(result^, i^, hd);
      decr(i);
    | _ => raise(Exceptions.InternalError("FeatureSequence.reduceReversei"))
    };
  };
  result^;
};

let reduceReverse = (fn, initialValue, ds) =>
  reduceReversei((acc, i, el) => fn(acc, el), initialValue, ds);

let mapi = (fn, ds) => {
  let list = Config.toList(ds);
  let list = Caml.List.mapi(fn, list);
  Config.fromList(list);
};

let map = (fn, ds) => mapi((i, el) => fn(el), ds);

let filterKeepi = (fn, ds) => {
  let list = Config.toList(ds);
  let list = ref(list);
  let resultRev = ref([]);
  let i = ref(0);
  while (!CamlListCore.isEmpty(list^)) {
    switch (list^) {
    | [hd, ...rest] =>
      list := rest;
      if (fn(i^, hd)) {
        resultRev := [hd, ...resultRev^];
      };
      incr(i);
    | _ => raise(Exceptions.InternalError("FeatureSequence.filterKeepi"))
    };
  };
  resultRev^ |> Caml.List.rev |> Config.fromList;
};

let filterKeep = (fn, ds) => filterKeepi((i, el) => fn(el), ds);

let filterDropi = (fn, ds) => filterKeepi((i, el) => !fn(i, el), ds);

let filterDrop = (fn, ds) => filterDropi((i, el) => fn(el), ds);

let filterNone = ds =>
  ds
  |> filterKeep(el =>
       switch (el) {
       | Some(_) => true
       | None => false
       }
     )
  |> map(el =>
       switch (el) {
       | Some(value) => value
       | None =>
         raise(Exceptions.InternalError("FeatureSequence.filterNone"))
       }
     );

let filterError = ds =>
  ds
  |> filterKeep(el =>
       switch (el) {
       | Ok(_) => true
       | Error(_) => false
       }
     )
  |> map(el =>
       switch (el) {
       | Ok(value) => value
       | Error(_) =>
         raise(Exceptions.InternalError("FeatureSequence.filterError"))
       }
     );

let filterOk = ds =>
  ds
  |> filterKeep(el =>
       switch (el) {
       | Ok(_) => false
       | Error(_) => true
       }
     )
  |> map(el =>
       switch (el) {
       | Error(value) => value
       | Ok(value) =>
         raise(Exceptions.InternalError("FeatureSequence.filterOk"))
       }
     );

let flattenArray = dsArray => {
  let arr1D = dsArray;
  let arr2D = Caml.Array.map(toCamlArray, arr1D);
  let lengths = Caml.Array.map(Caml.Array.length, arr2D);
  let sum = Caml.Array.fold_left((sum, x) => sum + x, 0, lengths);
  let arr = ref(0);
  let i = ref(0);
  let fullArray =
    Caml.Array.init(
      sum,
      _ => {
        while (i^ >= lengths[arr^]) {
          incr(arr);
          i := 0;
        };
        let value = arr2D[arr^][i^];
        incr(i);
        value;
      },
    );
  fromCamlArray(fullArray);
};

let flattenList = dsList => dsList |> Caml.Array.of_list |> flattenArray;

let flatten = ds2D => ds2D |> toCamlList |> flattenList;

let concat = (ds1, ds2) => flattenList([ds1, ds2]);

let getFirstExn = DequeCore.getFirstExn;

let getFirst = ds =>
  try(Some(getFirstExn(ds))) {
  | _ => None
  };

let getFirstNExn = (count, ds) => {
  let ds = ref(ds);
  let result = ref([]);
  for (_i in 0 to count - 1) {
    result := [getFirstExn(ds^), ...result^];
    ds := Config.dropFirstExn(ds^);
  };
  result^ |> Caml.List.rev |> Config.fromList;
};

let getFirstN = (count, ds) =>
  try(Some(getFirstNExn(count, ds))) {
  | _ => None
  };

let dropFirstExn = Config.dropFirstExn;

let dropFirstNExn = (count, ds) => {
  let ds = ref(ds);
  for (_i in 0 to count - 1) {
    ds := dropFirstExn(ds^);
  };
  ds^;
};

let dropFirstN = (count, ds) =>
  try(Some(dropFirstNExn(count, ds))) {
  | _ => None
  };

let dropFirst = ds =>
  try(Some(dropFirstExn(ds))) {
  | _ => None
  };

let addFirst = DequeCore.addFirst;

let removeFirstExn = ds => {
  let list = Config.toList(ds);
  switch (list) {
  | [] => raise(Exceptions.Empty("FeatureFront.removeFirstExn"))
  | [_, ...rest] => Config.fromList(rest)
  };
};

let removeFirstNExn = (count, ds) => {
  let ds = ref(ds);
  for (_i in 0 to count - 1) {
    ds := removeFirstExn(ds^);
  };
  ds^;
};

let removeFirstN = (count, ds) =>
  try(Some(removeFirstNExn(count, ds))) {
  | _ => None
  };

let removeFirst = ds =>
  try(Some(removeFirstExn(ds))) {
  | _ => None
  };

let updateFirstExn = (fn, ds) =>
  if (Config.isEmpty(ds)) {
    raise(Exceptions.Empty("FeatureFront.updateFirstExn"));
  } else {
    let first = getFirstExn(ds);
    let rest = removeFirstExn(ds);
    let newFirst = fn(first);
    addFirst(newFirst, rest);
  };

let updateFirst = (fn, ds) =>
  try(Some(updateFirstExn(fn, ds))) {
  | _ => None
  };

let getLastExn = Config.getLastExn;

let getLast = ds =>
  try(Some(getLastExn(ds))) {
  | _ => None
  };

let getLastNExn = (count, ds) => {
  let ds = ref(ds);
  let result = ref([]);
  for (i in 0 to count - 1) {
    result := [getLastExn(ds^), ...result^];
    ds := Config.removeLastExn(ds^);
  };
  result^ |> Config.fromList;
};

let getLastN = (count, ds) =>
  try(Some(getLastNExn(count, ds))) {
  | _ => None
  };

let addLast = Config.addLast;

let removeLastExn = Config.removeLastExn;

let removeLastNExn = (count, ds) => {
  let ds = ref(ds);
  for (i in 0 to count - 1) {
    ds := removeLastExn(ds^);
  };
  ds^;
};

let removeLastN = (count, ds) =>
  try(Some(removeLastNExn(count, ds))) {
  | _ => None
  };

let removeLast = ds =>
  try(Some(removeLastExn(ds))) {
  | _ => None
  };

let updateLastExn = (fn, ds) =>
  if (Config.isEmpty(ds)) {
    raise(Exceptions.Empty("FeatureBack.updateLastExn"));
  } else {
    let first = getLastExn(ds);
    let rest = removeLastExn(ds);
    let newLast = fn(first);
    addLast(newLast, rest);
  };

let updateLast = (fn, ds) =>
  try(Some(updateLastExn(fn, ds))) {
  | _ => None
  };
