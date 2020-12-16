open Bread;

type t('el) = ref(list('el));

let make = () => ref([]);

let fromList = (l: list('el)): t('el) => ref(l);

let isEmpty = t =>
  switch (t^) {
  | [] => true
  | _ => false
  };

let length = t => List.length(t^);

let flatten = t => {
  fromList(List.concat(t^));
};

let addFirst = (el, t) => {
  t := [el, ...t^];
  ();
};

let removeFirst = t => {
  t := List.tl(t^);
  ();
};

let addLast = (el, t) => {
  t := t^ @ [el];
  ();
};

let reverse = t => {
  t := List.rev(t^);
  ();
};

let get = t => t^;

let getFirst = t => List.hd(t^);

let getLast = t => List.hd(List.rev(t^));

let foldLeft = (fn, acc, t) => {
  List.fold_left(fn, acc, t^);
};

let filterKeep = (fn, t) => {
  t := List.filter(fn, t^);
  ();
};

let map = (fn, t) => {
  t := List.map(fn, t^);
  ();
};

let mapi = (fn, t) => {
  t := List.mapi(fn, t^);
  ();
};

let iter = (fn, t) => {
  List.iter(fn, t^);
  ();
};

let iteri = (fn, t) => {
  List.iteri(fn, t^);
  ();
};
