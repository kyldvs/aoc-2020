open Aoc;
open TestFramework;

describe("Scanner", ({test}) => {
  test("Basic 1", ({expect}) => {
    let s = Scanner.make(["1-2-3-4-5"]);
    expect.int(Scanner.nextInt(s)).toBe(1);
    expect.int(Scanner.nextInt(s)).toBe(2);
    expect.int(Scanner.nextInt(s)).toBe(3);
    expect.int(Scanner.nextInt(s)).toBe(4);
    expect.int(Scanner.nextInt(s)).toBe(5);
  });

  test("Basic 2", ({expect}) => {
    let s = Scanner.make(["1 2,3:4\n5"]);
    expect.int(Scanner.nextInt(s)).toBe(1);
    expect.int(Scanner.nextInt(s)).toBe(2);
    expect.int(Scanner.nextInt(s)).toBe(3);
    expect.int(Scanner.nextInt(s)).toBe(4);
    expect.int(Scanner.nextInt(s)).toBe(5);
  });

  test("Next Length", ({expect}) => {
    let s = Scanner.make(["123 456 7890123"]);
    expect.string(Scanner.nextLength(3, s)).toEqual("123");
    expect.string(Scanner.nextLength(2, s)).toEqual("45");
    expect.string(Scanner.nextLength(1, s)).toEqual("6");
    expect.string(Scanner.nextLength(2, s)).toEqual("78");
    expect.string(Scanner.nextLength(5, s)).toEqual("90123");
  });
});
