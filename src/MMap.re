open Bread;

type t('value) = ref(Map.t('value));

let make = () => ref(Map.empty);

let fromMap = m => ref(m);

let size = t => Map.size(t^);

let set = (k, v, t) => {
  t := Map.set(k, v, t^);
  ();
};

let get = (k, t) => {
  Map.get(k, t^);
};

let getExn = (k, t) => {
  Map.getExn(k, t^);
};

let hasKey = (k, t) => {
  Map.hasKey(k, t^);
};

let iter = (fn, t) => {
  Map.Impl.iter(fn, t^);
  ();
};

let map = (fn, t) => {
  fromMap(Map.map(fn, t^));
};

let mapi = (fn, t) => {
  fromMap(Map.mapi(fn, t^));
};
