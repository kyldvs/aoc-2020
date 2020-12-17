open Aoc;
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
});
