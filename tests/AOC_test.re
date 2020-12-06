open Aoc;
open TestFramework;

describe("Past Days", ({test}) => {
  test("Day 01", ({expect}) => {
    let lines = Utils.getInput("01");
    expect.int(Day01.part1(lines)).toBe(926464);
    expect.int(Day01.part2(lines)).toBe(65656536);
  });

  test("Day 02", ({expect}) => {
    let lines = Utils.getInput("02");
    expect.int(Day02.part1(lines)).toBe(416);
    expect.int(Day02.part2(lines)).toBe(688);
  });

  test("Day 03", ({expect}) => {
    let lines = Utils.getInput("03");
    expect.int(Day03.part1(lines)).toBe(268);
    expect.int(Day03.part2(lines)).toBe(3093068400);
  });

  test("Day 04", ({expect}) => {
    let lines = Utils.getInput("04");
    expect.int(Day04.part1(lines)).toBe(237);
    expect.int(Day04.part2(lines)).toBe(172);
  });

  test("Day 05", ({expect}) => {
    let lines = Utils.getInput("05");
    expect.int(Day05.part1(lines)).toBe(866);
    expect.int(Day05.part2(lines)).toBe(583);
  });
});
