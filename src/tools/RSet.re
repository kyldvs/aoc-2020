open Bread;

type t = {
  visited: ref(Set.t),
  size: unit => int,
  add: string => unit,
  has: string => bool,
  remove: string => unit,
  addList: list(string) => unit,
  addArray: array(string) => unit,
  addSet: Set.t => unit,
  removeList: list(string) => unit,
  removeArray: array(string) => unit,
  removeSet: Set.t => unit,
  toList: unit => list(string),
  toArray: unit => array(string),
  toSet: unit => Set.t,
  map: (string => string) => unit,
  iter: (string => unit) => unit,
};

let make = () => {
  let visited = ref(Set.empty);

  // Helpers
  let get = () => visited^;
  let set = s => visited := s;

  // Functions
  let size = () => Set.size(get());
  let add = s => set(Set.add(s, get()));
  let has = s => Set.has(s, get());
  let remove = s => set(Set.remove(s, get()));
  let addList = l => List.iter(add, l);
  let removeList = l => List.iter(remove, l);
  let addArray = l => Array.iter(add, l);
  let removeArray = l => Array.iter(remove, l);
  let addSet = s => Set.Impl.iter(add, s);
  let removeSet = s => Set.Impl.iter(remove, s);
  let toList = () => Set.toList(get());
  let toArray = () => Set.toArray(get());
  let toSet = get;
  let map = fn => set(Set.map(fn, get()));
  let iter = fn => Set.Impl.iter(fn, get());

  // Result
  {
    visited,
    size,
    add,
    has,
    remove,
    addList,
    addArray,
    addSet,
    removeList,
    removeArray,
    removeSet,
    toList,
    toArray,
    toSet,
    map,
    iter,
  };
};

let fromList = l => {
  let m = make();
  m.addList(l);
  m;
};

let fromArray = a => {
  let m = make();
  m.addArray(a);
  m;
};

let fromSet = s => {
  let m = make();
  m.addSet(s);
  m;
};
