open Aoc;
open TestFramework;

describe("Example", ({test}) => {
  test("1", ({expect}) => {
    let actual = 42;
    let expected = 42;
    expect.int(actual).toBe(expected);
  });

  test("Day 01", ({expect}) => {
    let lines = Utils.getInput(1);
    expect.int(Day01.part1(lines)).toBe(926464);
    expect.int(Day01.part2(lines)).toBe(65656536);
  });

  test("Day 02", ({expect}) => {
    let lines = Utils.getInput(2);
    expect.int(Day02.part1(lines)).toBe(416);
    expect.int(Day02.part2(lines)).toBe(688);
  });

  test("Day 03", ({expect}) => {
    let lines = Utils.getInput(3);
    expect.int(Day03.part1(lines)).toBe(268);
    expect.int(Day03.part2(lines)).toBe(3093068400);
  });

  test("Day 04", ({expect}) => {
    let lines = Utils.getInput(4);
    expect.int(Day04.part1(lines)).toBe(237);
    expect.int(Day04.part2(lines)).toBe(172);
  });

  test("Day 05", ({expect}) => {
    let lines = Utils.getInput(5);
    expect.int(Day05.part1(lines)).toBe(866);
    expect.int(Day05.part2(lines)).toBe(583);
  });
});
