open Aoc;
open Bread;
open TestFramework;
open Tools;

describe("Tools", ({test}) => {
  test("Dummy 1", ({expect}) => {
    expect.int(1).toBe(1);
    expect.int(1).toBe(1);
  });

  test("Basic 1", ({expect}) => {
    let s = "{}, {}, and {}";
    let lines = [|
      "foo, bar, and baz",
      "foo, bar, and baz buz",
      "foo, bar, and and baz",
      "{}, {}, and {}",
    |];
    let actual = Input.extractLines(s, lines);

    expect.string(actual[0][0]).toEqual("foo");
    expect.string(actual[0][1]).toEqual("bar");
    expect.string(actual[0][2]).toEqual("baz");

    expect.string(actual[1][0]).toEqual("foo");
    expect.string(actual[1][1]).toEqual("bar");
    expect.string(actual[1][2]).toEqual("baz buz");

    expect.string(actual[2][0]).toEqual("foo");
    expect.string(actual[2][1]).toEqual("bar");
    expect.string(actual[2][2]).toEqual("and baz");

    expect.string(actual[3][0]).toEqual("{}");
    expect.string(actual[3][1]).toEqual("{}");
    expect.string(actual[3][2]).toEqual("{}");
  });

  test("Basic 2", ({expect}) => {
    let s = "Hello {}, {}, and {}! We hope you have a nice day.";
    let lines = [|
      "Hello Alice, Bob, and Carla! We hope you have a nice day.",
      "Hello Alice, , and Carla! We hope you have a nice day.",
    |];
    let actual = Input.extractLines(s, lines);

    expect.string(actual[0][0]).toEqual("Alice");
    expect.string(actual[0][1]).toEqual("Bob");
    expect.string(actual[0][2]).toEqual("Carla");

    expect.string(actual[1][0]).toEqual("Alice");
    expect.string(actual[1][1]).toEqual("");
    expect.string(actual[1][2]).toEqual("Carla");
  });

  test("Failure 1, ending", ({expect}) => {
    let s = "Hello {}, {}, and {}! We hope you have a nice day.";
    let lines = [|
      "Hello Alice, Bob, and Carla! We hope you have a nice day....",
    |];
    expect.fn(() => Input.extractLines(s, lines)).toThrow();
  });

  test("RPN 1", ({expect}) => {
    module RPN =
      RPN.Make({
        let config =
          Map.fromList([
            ("+", (1, RPN.Left, (+))),
            ("*", (1, RPN.Left, ( * ))),
          ]);
      });

    let run = s =>
      s
      |> String.replaceAll("(", "( ")
      |> String.replaceAll(")", " )")
      |> String.split(" ")
      |> RPN.parse
      |> RPN.solve;

    expect.int(run("1 + 2")).toBe(3);
    expect.int(run("1 + 2 * 3")).toBe(9);
    expect.int(run("1 + 2 * 3 + 4 * 5 + 6")).toBe(71);
    expect.int(run("2 * 3 + (4 * 5)")).toBe(26);
    expect.int(run("5 + (8 * 3 + 9 + 3 * 4 * 3)")).toBe(437);
    expect.int(run("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")).toBe(
      12240,
    );
    expect.int(run("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).toBe(
      13632,
    );
  });

  test("RPN 2", ({expect}) => {
    module RPN =
      RPN.Make({
        let config =
          Map.fromList([
            ("+", (2, RPN.Left, (+))),
            ("*", (1, RPN.Left, ( * ))),
          ]);
      });

    let run = s =>
      s
      |> String.replaceAll("(", "( ")
      |> String.replaceAll(")", " )")
      |> String.split(" ")
      |> RPN.parse
      |> RPN.solve;

    expect.int(run("1 + 2")).toBe(3);
    expect.int(run("1 + 2 * 3")).toBe(9);
    expect.int(run("1 + 2 * 3 + 4 * 5 + 6")).toBe(231);
    expect.int(run("1 + (2 * 3) + (4 * (5 + 6))")).toBe(51);
    expect.int(run("2 * 3 + (4 * 5)")).toBe(46);
    expect.int(run("5 + (8 * 3 + 9 + 3 * 4 * 3)")).toBe(1445);
    expect.int(run("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")).toBe(
      669060,
    );
    expect.int(run("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).toBe(
      23340,
    );
  });

  test("RPN 3", ({expect}) => {
    module RPN =
      RPN.Make({
        let config =
          Map.fromList([
            ("+", (1, RPN.Left, (+))),
            ("-", (1, RPN.Left, (-))),
          ]);
      });

    let run = s =>
      s
      |> String.replaceAll("(", "( ")
      |> String.replaceAll(")", " )")
      |> String.split(" ")
      |> RPN.run;

    expect.int(run("1 + 2 - 3")).toBe(0);
    expect.int(run("1 - 2 + 3")).toBe(2);
    expect.int(run("10 - 20 + 30 - 40 + 50 - 60")).toBe(-30);
  });
});
