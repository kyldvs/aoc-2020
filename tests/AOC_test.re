open Aoc;
open TestFramework;

describe("Past Days", ({test}) => {
  test("Day01", ({expect}) => {
    let lines = Utils.getInput("01");
    expect.int(Day01.part1(lines)).toBe(926464);
    expect.int(Day01.part2(lines)).toBe(65656536);
  });

  test("Day02", ({expect}) => {
    let lines = Utils.getInput("02");
    expect.int(Day02.part1(lines)).toBe(416);
    expect.int(Day02.part2(lines)).toBe(688);
  });

  test("Day03", ({expect}) => {
    let lines = Utils.getInput("03");
    expect.int(Day03.part1(lines)).toBe(268);
    expect.int(Day03.part2(lines)).toBe(3093068400);
  });

  test("Day04", ({expect}) => {
    let lines = Utils.getInput("04");
    expect.int(Day04.part1(lines)).toBe(237);
    expect.int(Day04.part2(lines)).toBe(172);
  });

  test("Day05", ({expect}) => {
    let lines = Utils.getInput("05");
    expect.int(Day05.part1(lines)).toBe(866);
    expect.int(Day05.part2(lines)).toBe(583);
  });

  test("Day06", ({expect}) => {
    let lines = Utils.getInput("06");
    expect.int(Day06.part1(lines)).toBe(6748);
    expect.int(Day06.part2(lines)).toBe(3445);
  });

  test("Day07", ({expect}) => {
    let lines = Utils.getInput("07");
    expect.int(Day07.part1(lines)).toBe(252);
    expect.int(Day07.part2(lines)).toBe(35487);
  });

  test("Day07Clean", ({expect}) => {
    let lines = Utils.getInput("07");
    expect.int(Day07Clean.part1(lines)).toBe(252);
    expect.int(Day07Clean.part2(lines)).toBe(35487);
  });
});
