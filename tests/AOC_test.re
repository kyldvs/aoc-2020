open Aoc;
open TestFramework;

describe("Past Days", ({test}) => {
  test("Day01", ({expect}) => {
    let lines = Utils.getInput("01");
    expect.int(Day01.part1(lines)).toBe(926464);
    expect.int(Day01.part2(lines)).toBe(65656536);
  });

  test("Day01Clean", ({expect}) => {
    let lines = Utils.getInput("01");
    expect.int(Day01Clean.part1(lines)).toBe(926464);
    expect.int(Day01Clean.part2(lines)).toBe(65656536);
  });

  test("Day02", ({expect}) => {
    let lines = Utils.getInput("02");
    expect.int(Day02.part1(lines)).toBe(416);
    expect.int(Day02.part2(lines)).toBe(688);
  });

  test("Day02Clean", ({expect}) => {
    let lines = Utils.getInput("02");
    expect.int(Day02Clean.part1(lines)).toBe(416);
    expect.int(Day02Clean.part2(lines)).toBe(688);
  });

  test("Day03", ({expect}) => {
    let lines = Utils.getInput("03");
    expect.int(Day03.part1(lines)).toBe(268);
    expect.int(Day03.part2(lines)).toBe(3093068400);
  });

  test("Day03Clean", ({expect}) => {
    let lines = Utils.getInput("03");
    expect.int(Day03Clean.part1(lines)).toBe(268);
    expect.int(Day03Clean.part2(lines)).toBe(3093068400);
  });

  test("Day04", ({expect}) => {
    let lines = Utils.getInput("04");
    expect.int(Day04.part1(lines)).toBe(237);
    expect.int(Day04.part2(lines)).toBe(172);
  });

  test("Day04Clean", ({expect}) => {
    let lines = Utils.getInput("04");
    expect.int(Day04Clean.part1(lines)).toBe(237);
    expect.int(Day04Clean.part2(lines)).toBe(172);
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

  test("Day08", ({expect}) => {
    let lines = Utils.getInput("08");
    expect.int(Day08.part1(lines)).toBe(1814);
    expect.int(Day08.part2(lines)).toBe(1056);
  });

  test("Day09", ({expect}) => {
    let lines = Utils.getInput("09");
    expect.int(Day09.part1(lines)).toBe(50047984);
    expect.int(Day09.part2(lines)).toBe(5407707);
  });

  test("Day10", ({expect}) => {
    let lines = Utils.getInput("10");
    expect.int(Day10.part1(lines)).toBe(2277);
    expect.int(Day10.part2(lines)).toBe(37024595836928);
  });

  test("Day11", ({expect}) => {
    let lines = Utils.getInput("11");
    expect.int(Day11.part1(lines)).toBe(2273);
    expect.int(Day11.part2(lines)).toBe(2064);
  });

  // TODO: Add missing days.

  test("Day16", ({expect}) => {
    let lines = Utils.getInput("16");
    expect.int(Day16.part1(lines)).toBe(26869);
    expect.int(Day16.part2(lines)).toBe(855275529001);
  });

  test("Day16Clean", ({expect}) => {
    let lines = Utils.getInput("16");
    expect.int(Day16Clean.part1(lines)).toBe(26869);
    expect.int(Day16Clean.part2(lines)).toBe(855275529001);
  });
});
