module Node = {
  type n('value) = {
    mutable next: option(n('value)),
    mutable prev: option(n('value)),
    mutable value: 'value,
  };
  let node = v => {next: None, prev: None, value: v};
};
open Node;

type t('value) = {
  mutable size: int,
  mutable head: option(n('value)),
  mutable tail: option(n('value)),
};

let make = () => {
  let empty = {size: 0, head: None, tail: None};
  empty;
};

module Helper = {
  let fixEnds = t => {
    switch (t.head) {
    | Some(head) => head.prev = None
    | None => ()
    };
    switch (t.tail) {
    | Some(tail) => tail.next = None
    | None => ()
    };
  };

  let connect = (n1, n2) => {
    switch (n1, n2) {
    | (Some(n1), Some(n2)) =>
      n1.next = Some(n2);
      n2.prev = Some(n1);
    | _ => ()
    };
  };

  let before = (n, i, t) => {
    ();
    if (i === 0) {
      make();
    } else {
      let next = {size: i, head: t.head, tail: n.prev};
      fixEnds(next);
      next;
    };
  };

  let after = (n, i, t) => {
    ();
    if (i === t.size - 1) {
      make();
    } else {
      let next = {size: t.size - i - 1, head: n.next, tail: t.tail};
      fixEnds(next);
      next;
    };
  };
};

let size = t => t.size;

let set = (t, target) => {
  target.size = t.size;
  target.head = t.head;
  target.tail = t.tail;
};

let getFirst = t => {
  switch (t.head) {
  | Some({value}) => value
  | None => raise(Not_found)
  };
};

let getLast = t => {
  switch (t.tail) {
  | Some({value}) => value
  | None => raise(Not_found)
  };
};

let addFirst = (v, t) => {
  let n = node(v);
  if (t.size === 0) {
    t.head = Some(n);
    t.tail = Some(n);
  } else {
    let a = Option.get(t.head);
    a.prev = Some(n);
    n.next = Some(a);
    t.head = Some(n);
  };
  t.size = t.size + 1;
  ();
};

let addLast = (v, t) => {
  let n = node(v);
  if (t.size === 0) {
    t.head = Some(n);
    t.tail = Some(n);
  } else {
    let a = Option.get(t.tail);
    a.next = Some(n);
    n.prev = Some(a);
    t.tail = Some(n);
  };
  t.size = t.size + 1;
  ();
};

let removeFirst = t => {
  ();
  let result =
    if (t.size === 0) {
      raise(Not_found);
    } else if (t.size === 1) {
      let a = Option.get(t.head);
      t.head = None;
      t.tail = None;
      a.value;
    } else {
      let a = Option.get(t.head);
      t.head = a.next;
      Helper.fixEnds(t);
      a.value;
    };
  t.size = t.size - 1;
  result;
};

let removeLast = t => {
  ();
  let result =
    if (t.size === 0) {
      raise(Not_found);
    } else if (t.size === 1) {
      let a = Option.get(t.tail);
      t.head = None;
      t.tail = None;
      a.value;
    } else {
      let a = Option.get(t.tail);
      t.tail = a.prev;
      Helper.fixEnds(t);
      a.value;
    };
  t.size = t.size - 1;
  result;
};

let concat = (t1, t2) => {
  ();
  if (t1.size === 0) {
    set(t2, t1)
  } else if (t2.size === 0) {
    ();
  } else {
    Helper.connect(t1.tail, t2.head);
    let combined = {size: t1.size + t2.size, head: t1.head, tail: t2.tail};
    set(combined, t1);
  };
  // Force both t1 and t2 to be the concatenated lists. Completely destroy
  // any prior lists so they can't be used.
  set(t1, t2);
  ();
};

let split = (fn, t) => {
  ();
  if (t.size === 0) {
    raise(Not_found);
  } else {
    let curr = ref(Option.get(t.head));
    let break = ref(false);
    let i = ref(0);
    while (! break^) {
      if (fn(curr^.value)) {
        break := true;
      } else if (curr^.next == None) {
        raise(Not_found);
      } else {
        curr := Option.get(curr^.next);
        incr(i);
      };
    };
    let before = Helper.before(curr^, i^, t);
    let after = Helper.after(curr^, i^, t);
    (before, curr^.value, after);
  };
};

let splitRev = (fn, t) => {
  ();
  if (t.size === 0) {
    raise(Not_found);
  } else {
    let curr = ref(Option.get(t.tail));
    let break = ref(false);
    let i = ref(0);
    while (! break^) {
      if (fn(curr^.value)) {
        break := true;
      } else if (curr^.prev == None) {
        raise(Not_found);
      } else {
        curr := Option.get(curr^.prev);
        incr(i);
      };
    };
    let before = Helper.before(curr^, t.size - i^ - 1, t);
    let after = Helper.after(curr^, t.size - i^ - 1, t);
    (before, curr^.value, after);
  };
};

let print = (~max=20, fn, t) => {
  ();
  if (t.size === 0) {
    "[]";
  } else if (t.size > max) {
    let frontRev = ref([]);
    let curr = ref(Option.get(t.head));
    for (i in 1 to max / 2) {
      frontRev := [fn(curr^.value), ...frontRev^];
      curr := Option.get(curr^.next);
    };
    let back = ref([]);
    let curr = ref(Option.get(t.tail));
    for (i in 1 to max / 2) {
      back := [fn(curr^.value), ...back^];
      curr := Option.get(curr^.prev);
    };

    "["
    ++ Bread.String.join(", ", List.rev(frontRev^))
    ++ ", ... , "
    ++ Bread.String.join(", ", back^)
    ++ "]";
  } else {
    let back = ref([]);
    let curr = ref(Option.get(t.tail));
    for (i in 1 to t.size) {
      back := [fn(curr^.value), ...back^];
      if (curr^.prev != None) {
        curr := Option.get(curr^.prev);
      };
    };
    "[" ++ Bread.String.join(", ", back^) ++ "]";
  };
};
