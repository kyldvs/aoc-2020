type t('el);

let init: (int, int => 'el) => t('el);

let toCamlList: t('el) => CamlListCore.t('el);
let fromCamlList: CamlListCore.t('el) => t('el);

let toCamlArray: t('el) => CamlArrayCore.t('el);
let fromCamlArray: CamlArrayCore.t('el) => t('el);

let every: ('el => bool, t('el)) => bool;
let everyi: ((int, 'el) => bool, t('el)) => bool;

let forEach: ('el => unit, t('el)) => unit;
let forEachi: ((int, 'el) => unit, t('el)) => unit;

let none: ('el => bool, t('el)) => bool;
let nonei: ((int, 'el) => bool, t('el)) => bool;

let some: ('el => bool, t('el)) => bool;
let somei: ((int, 'el) => bool, t('el)) => bool;

let filterDrop: ('el => bool, t('el)) => t('el);
let filterDropi: ((int, 'el) => bool, t('el)) => t('el);

let filterKeep: ('el => bool, t('el)) => t('el);
let filterKeepi: ((int, 'el) => bool, t('el)) => t('el);

let filterNone: t(option('el)) => t('el);
let filterError: t(result('el, _)) => t('el);
let filterOk: t(result(_, 'err)) => t('err);

let map: ('a => 'b, t('a)) => t('b);
let mapi: ((int, 'a) => 'b, t('a)) => t('b);

/* let map2Exn: (('a, 'b) => 'c, t('a), t('b)) => t('c); */
/* let map2iExn: (('a, 'b) => 'c, t('a), t('b)) => t('c); */

let reverse: t('el) => t('el);

let reduce: (('acc, 'el) => 'acc, 'acc, t('el)) => 'acc;
let reducei: (('acc, int, 'el) => 'acc, 'acc, t('el)) => 'acc;

let reduceReverse: (('acc, 'el) => 'acc, 'acc, t('el)) => 'acc;
let reduceReversei: (('acc, int, 'el) => 'acc, 'acc, t('el)) => 'acc;

let flatten: t(t('el)) => t('el);
let flattenArray: array(t('el)) => t('el);
let flattenList: list(t('el)) => t('el);

let concat: (t('el), t('el)) => t('el);

/**
`getFirst(ds)` returns an option containing the first element of the data
structure `ds` if it exists, `None` otherwise.
```
getFirst([1, 2, 3, 4])  // Some(1)
getFirst([])            // None
```
 */
let getFirst: t('el) => option('el);

/**
`getFirstExn(ds)` returns the first element of the data structure `ds` if it
exists, raises an exception otherwise.
```
getFirstExn([1, 2, 3, 4])  // 1
getFirstExn([])            // raises exception
```
 */
let getFirstExn: t('el) => 'el;

/**
`getFirstN(n, ds)` returns an option containing the first `n` elements of the
data structure `ds` if they exist, `None` otherwise.
```
getFirstN(2, [1, 2, 3, 4])  // Some([1, 2])
getFirstN(1, [1, 2, 3, 4])  // Some([1])
getFirstN(0, [])            // Some([])
getFirstN(1000, [1, 2])     // None
getFirstN(1, [])            // None
```
 */
let getFirstN: (int, t('el)) => option(t('el));

/**
`getFirstNExn(n, ds)` returns the first `n` elements of the data structure `ds`
if they exist, raises an exception otherwise.
```
getFirstNExn(2, [1, 2, 3, 4])  // [1, 2]
getFirstNExn(1, [1, 2, 3, 4])  // [1]
getFirstNExn(0, [])            // []
getFirstNExn(1000, [1, 2])     // raises exception
getFirstNExn(1, [])            // raises exception
```
 */
let getFirstNExn: (int, t('el)) => t('el);

/**
`dropFirst(ds)` returns an option containing a new data structure with the
first element of the given data structure `ds` removed if it exists, None
otherwise.
__Note: This does NOT mutate the given data structure.__
__Note: Use `getFirst` to access the first element if needed.__
```
dropFirst([1, 2, 3])  // Some([2, 3])
dropFirst([])         // None
```
 */
let dropFirst: t('el) => option(t('el));

/**
`dropFirstExn(ds)` returns a new data structure with the first element of the
given data structure `ds` removed if it exists, raises an exception otherwise.
__Note: This does NOT mutate the given data structure.__
__Note: Use `getFirst` to access the first element if needed.__
```
dropFirstExn([1, 2, 3])  // [2, 3]
dropFirstExn([])         // raises exception
```
 */
let dropFirstExn: t('el) => t('el);

/**
`dropFirstN(n, ds)` returns an option containing a new data structure with the
first `n` elements of the given data structure `ds` removed if they exist, None
otherwise.
__Note: This does NOT mutate the given data structure.__
__Note: Use `getFirstN` to access the first elements if needed.__
```
dropFirstN(1, [1, 2, 3])     // Some([2, 3])
dropFirstN(2, [1, 2, 3])     // Some([3])
dropFirstN(0, [])            // Some([])
dropFirstN(1000, [1, 2, 3])  // None
dropFirstN(1, [])            // None
```
 */
let dropFirstN: (int, t('el)) => option(t('el));

/**
`dropFirstNExn(n, ds)` returns a new data structure with the first `n`
elements of the given data structure `ds` removed if they exist, raises an
exception otherwise.
__Note: This does NOT mutate the given data structure.__
__Note: Use `getFirstN` to access the first elements if needed.__
```
dropFirstNExn(1, [1, 2, 3])     // [2, 3]
dropFirstNExn(2, [1, 2, 3])     // [3]
dropFirstNExn(0, [])            // []
dropFirstNExn(1000, [1, 2, 3])  // raises exception
dropFirstNExn(1, [])            // raises exception
```
 */
let dropFirstNExn: (int, t('el)) => t('el);

/**
`addFirst(el, ds)` returns a new data structure with `el` added as the first
element of the given data structure `ds`.
```
addFirst(9, [1, 2, 3])  // [9, 1, 2, 3]
addFirst(9, [])         // [9]
```
 */
let addFirst: ('el, t('el)) => t('el);

/**
`removeFirst(ds)` returns an option containing a new data structure with the
first element of the given data structure `ds` removed if it exists, None
otherwise.
__Note: Use `getFirst` to access the first element if needed.__
```
removeFirst([1, 2, 3])  // Some([2, 3])
removeFirst([])         // None
```
 */
let removeFirst: t('el) => option(t('el));

/**
`removeFirstExn(ds)` returns a new data structure with the first element of the
given data structure `ds` removed if it exists, raises an exception otherwise.
__Note: Use `getFirst` to access the first element if needed.__
```
removeFirstExn([1, 2, 3])  // [2, 3]
removeFirstExn([])         // raises exception
```
 */
let removeFirstExn: t('el) => t('el);

/**
`removeFirstN(n, ds)` returns an option containing a new data structure with the
first `n` elements of the given data structure `ds` removed if they exist, None
otherwise.
__Note: Use `getFirstN` to access the first elements if needed.__
```
removeFirstN(1, [1, 2, 3])     // Some([2, 3])
removeFirstN(2, [1, 2, 3])     // Some([3])
removeFirstN(0, [])            // Some([])
removeFirstN(1000, [1, 2, 3])  // None
removeFirstN(1, [])            // None
```
 */
let removeFirstN: (int, t('el)) => option(t('el));

/**
`removeFirstNExn(n, ds)` returns a new data structure with the first `n`
elements of the given data structure `ds` removed if they exist, raises an
exception otherwise.
__Note: Use `getFirstN` to access the first elements if needed.__
```
removeFirstNExn(1, [1, 2, 3])     // [2, 3]
removeFirstNExn(2, [1, 2, 3])     // [3]
removeFirstNExn(0, [])            // []
removeFirstNExn(1000, [1, 2, 3])  // raises exception
removeFirstNExn(1, [])            // raises exception
```
 */
let removeFirstNExn: (int, t('el)) => t('el);

/**
`updateFirst(fn, ds)` returns an option containing a new data structure with the
first element of the given data structure `ds` updated by `fn` if it exists,
None otherwise.
```
updateFirst(x => x + 1, [1, 2, 3])  // Some([2, 2, 3])
updateFirst(x => x + 1, [])         // None
```
 */
let updateFirst: ('el => 'el, t('el)) => option(t('el));

/**
`updateFirstExn(fn, ds)` returns a new data structure with the first element of
the given data structure `ds` updated by `fn` if it exists, raises an exception
otherwise.
```
updateFirstExn(x => x + 1, [1, 2, 3])  // [2, 2, 3]
updateFirstExn(x => x + 1, [])         // raises exception
```
 */
let updateFirstExn: ('el => 'el, t('el)) => t('el);

/**
`getLast(ds)` returns an option containing the last element of the data
structure `ds` if it exists, `None` otherwise.
```
getLast([1, 2, 3, 4])  // Some(4)
getLast([])            // None
```
 */
let getLast: t('el) => option('el);

/**
`getLastExn(ds)` returns the last element of the data structure `ds` if it
exists, raises an exception otherwise.
```
getLastExn([1, 2, 3, 4])  // 4
getLastExn([])            // raises exception
```
 */
let getLastExn: t('el) => 'el;

/**
`getLastN(n, ds)` returns an option containing the last `n` elements of the
data structure `ds` if they exist, `None` otherwise.
```
getLastN(2, [1, 2, 3, 4])  // Some([3, 4])
getLastN(1, [1, 2, 3, 4])  // Some([4])
getLastN(0, [])            // Some([])
getLastN(1000, [1, 2])     // None
getLastN(1, [])            // None
```
 */
let getLastN: (int, t('el)) => option(t('el));

/**
`getLastNExn(n, ds)` returns the last `n` elements of the data structure `ds`
if they exist, raises an exception otherwise.
```
getLastNExn(2, [1, 2, 3, 4])  // [3, 4]
getLastNExn(1, [1, 2, 3, 4])  // [4]
getLastNExn(0, [])            // []
getLastNExn(1000, [1, 2])     // raises exception
getLastNExn(1, [])            // raises exception
```
 */
let getLastNExn: (int, t('el)) => t('el);

/**
`addLast(el, ds)` returns a new data structure with `el` added as the last
element of the given data structure `ds`.
```
addLast(9, [1, 2, 3])  // [1, 2, 3, 9]
addLast(9, [])         // [9]
```
 */
let addLast: ('el, t('el)) => t('el);

/**
`removeLast(ds)` returns an option containing a new data structure with the last
element of the given data structure `ds` removed if it exists, None otherwise.
__Note: Use `getLast` to access the last element if needed.__
```
removeLast([1, 2, 3])  // Some([1, 2])
removeLast([])         // None
```
 */
let removeLast: t('el) => option(t('el));

/**
`removeLastExn(ds)` returns a new data structure with the last element of the
given data structure `ds` removed if it exists, raises an exception otherwise.
__Note: Use `getLast` to access the last element if needed.__
```
removeLastExn([1, 2, 3])  // [1, 2]
removeLastExn([])         // raises exception
```
 */
let removeLastExn: t('el) => t('el);

/**
`removeLastN(n, ds)` returns an option containing a new data structure with the
last `n` elements of the given data structure `ds` removed if they exist, None
otherwise.
__Note: Use `getLastN` to access the last elements if needed.__
```
removeLastN(1, [1, 2, 3])     // Some([1, 2])
removeLastN(2, [1, 2, 3])     // Some([1])
removeLastN(0, [])            // Some([])
removeLastN(1000, [1, 2, 3])  // None
removeLastN(1, [])            // None
```
 */
let removeLastN: (int, t('el)) => option(t('el));

/**
`removeLastNExn(n, ds)` returns a new data structure with the last `n` elements of
the given data structure `ds` removed if they exist, raises an exception
otherwise.
__Note: Use `getLastN` to access the last elements if needed.__
```
removeLastNExn(1, [1, 2, 3])     // [1, 2]
removeLastNExn(2, [1, 2, 3])     // [1]
removeLastNExn(0, [])            // []
removeLastNExn(1000, [1, 2, 3])  // raises exception
removeLastNExn(1, [])            // raises exception
```
 */
let removeLastNExn: (int, t('el)) => t('el);

/**
`updateLast(fn, ds)` returns an option containing a new data structure with the
last element of the given data structure `ds` updated by `fn` if it exists,
None otherwise.
```
updateLast(x => x + 1, [1, 2, 3])  // Some([1, 2, 4])
updateLast(x => x + 1, [])         // None
```
 */
let updateLast: ('el => 'el, t('el)) => option(t('el));

/**
`updateLastExn(fn, ds)` returns a new data structure with the last element of
the given data structure `ds` updated by `fn` if it exists, raises an exception
otherwise.
```
updateLastExn(x => x + 1, [1, 2, 3])  // [1, 2, 4]
updateLastExn(x => x + 1, [])         // raises exception
```
 */
let updateLastExn: ('el => 'el, t('el)) => t('el);
