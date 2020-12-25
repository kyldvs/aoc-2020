open Bread;

module Node = {
  type n = {
    mutable next: option(n),
    mutable prev: option(n),
    mutable value: int,
  };
  let node = v => {next: None, prev: None, value: v};
};
open Node;

type t = {
  mutable head: option(n),
  mutable tail: option(n),
  mutable nodes: array(n),
};

let make = arr => {
  let empty = {head: None, tail: None, nodes: arr};
  empty;
};

module Helper = {
  let clean = n => {
    switch (n.next) {
    | Some(next) => next.prev = None
    | None => ()
    };
    switch (n.prev) {
    | Some(prev) => prev.next = None
    | None => ()
    };
    n.next = None;
    n.prev = None;
  };
};

let isEmpty = t => {
  t.head == None;
};

let isSizeOne = t => {
  !isEmpty(t) && Option.getExn(t.head).value == Option.getExn(t.tail).value;
};

let set = (t, target) => {
  target.head = t.head;
  target.tail = t.tail;
  target.nodes = t.nodes;
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
  let n = t.nodes[v];
  Helper.clean(n);

  if (isEmpty(t)) {
    t.head = Some(n);
    t.tail = Some(n);
  } else {
    let head = Option.getExn(t.head);

    head.prev = Some(n);
    n.next = Some(head);

    t.head = Some(n);
  };
  ();
};

let addLast = (v, t) => {
  let n = t.nodes[v];
  Helper.clean(n);

  if (isEmpty(t)) {
    t.head = Some(n);
    t.tail = Some(n);
  } else {
    let tail = Option.getExn(t.tail);

    tail.next = Some(n);
    n.prev = Some(tail);

    t.tail = Some(n);
  };
  ();
};

let removeFirst = t => {
  ();
  let result =
    if (isEmpty(t)) {
      raise(Not_found);
    } else if (isSizeOne(t)) {
      let head = Option.getExn(t.head);
      Helper.clean(head);

      t.head = None;
      t.tail = None;

      head.value;
    } else {
      let head = Option.getExn(t.head);

      let newHead = head.next;
      switch (newHead) {
      | Some(newHead) => newHead.prev = None
      | None => ()
      };

      t.head = newHead;

      head.value;
    };
  result;
};

let removeLast = t => {
  ();
  let result =
    if (isEmpty(t)) {
      raise(Not_found);
    } else if (isSizeOne(t)) {
      let tail = Option.getExn(t.tail);
      Helper.clean(tail);

      t.head = None;
      t.tail = None;

      tail.value;
    } else {
      let tail = Option.getExn(t.tail);

      let newTail = tail.prev;
      switch (newTail) {
      | Some(newTail) => newTail.next = None
      | None => ()
      };

      t.tail = newTail;

      tail.value;
    };
  result;
};

let concat = (t1, t2) => {
  ();
  if (isEmpty(t1)) {
    t1.head = t2.head;
    t1.tail = t2.tail;
  } else if (isEmpty(t2)) {
    t2.head = t1.head;
    t2.tail = t1.tail;
  } else {
    Option.getExn(t1.tail).next = t2.head;
    Option.getExn(t2.head).prev = t1.tail;

    t1.head = t1.head;
    t1.tail = t2.tail;

    t2.head = t1.head;
    t2.tail = t2.tail;
  };
  ();
};

// Behavior is undefined if value is not in t. Presence is not checked
// because that is slow.
let split = (value, t) => {
  ();
  if (isEmpty(t)) {
    raise(Not_found);
  } else {
    let n = t.nodes[value];
    let head = Option.getExn(t.head);
    let tail = Option.getExn(t.tail);
    if (n.value === head.value) {
      let before = {head: None, tail: None, nodes: t.nodes};
      let middle = n.value;
      let _ = removeFirst(t);
      let after = t;
      (before, middle, after);
    } else if (n.value === tail.value) {
      let _ = removeLast(t);
      let before = t;
      let middle = n.value;
      let after = {head: None, tail: None, nodes: t.nodes};
      (before, middle, after);
    } else {
      let before = {head: t.head, tail: n.prev, nodes: t.nodes};
      let middle = n.value;
      let after = {head: n.next, tail: t.tail, nodes: t.nodes};

      switch (n.prev) {
      | Some(prev) => prev.next = None
      | None => ()
      };

      switch (n.next) {
      | Some(next) => next.prev = None
      | None => ()
      };

      n.prev = None;
      n.next = None;

      (before, middle, after);
    };
  };
};

let print = (~max=20, fn, t) => {
  ();
  if (isEmpty(t)) {
    "[]";
  } else {
    let break = ref(false);
    let curr = ref(Option.getExn(t.head));
    for (i in 1 to max) {
      if (! break^) {
        if (curr^.next == None) {
          break := true;
        } else {
          curr := Option.getExn(curr^.next);
        };
      };
    };
    let atLeastMax = ! break^;
    if (atLeastMax) {
      let frontRev = ref([]);
      let curr = ref(Option.getExn(t.head));
      for (i in 1 to max / 2) {
        frontRev := [fn(curr^.value), ...frontRev^];
        curr := Option.getExn(curr^.next);
      };
      let back = ref([]);
      let curr = ref(Option.getExn(t.tail));
      for (i in 1 to max / 2) {
        back := [fn(curr^.value), ...back^];
        curr := Option.getExn(curr^.prev);
      };

      "["
      ++ Bread.String.join(", ", List.rev(frontRev^))
      ++ ", ... , "
      ++ Bread.String.join(", ", back^)
      ++ "]";
    } else {
      let back = ref([]);
      let curr = ref(t.tail);
      while (curr^ != None) {
        let n = Option.getExn(curr^);
        back := [fn(n.value), ...back^];
        curr := n.prev;
      };
      "[" ++ Bread.String.join(", ", back^) ++ "]";
    };
  };
};
