# aoc-2020

Advent of Code 2020: https://adventofcode.com/2020/

# Commands

Globally install [`esy`](https://www.npmjs.com/package/esy) so that the `esy`
command is available in the terminal (`npm install -g esy`, and perhaps
[global npm without sudo](https://github.com/sindresorhus/guides/blob/master/npm-global-without-sudo.md))

```bash
# Install things once, or when dependencies change.
esy && esy @exe && esy @test

# Run `Exe.re` (for solving each day)
esy exe

# Run test cases (for ensuring integrity of past days and utils)
esy test
```
