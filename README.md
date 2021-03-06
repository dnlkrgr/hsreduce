# hsreduce

Reducing Haskell programs for easier debugging of GHC crashes or performance regressions.

## Build Status

[![GHC 8.10.1](https://github.com/dnlkrgr/hsreduce/actions/workflows/haskell.yml/badge.svg)](https://github.com/dnlkrgr/hsreduce/actions/workflows/haskell.yml)

## Installing

Clone the repo:

```bash
$ https://github.com/dnlkrgr/hsreduce.git
$ cd hsreduce
```

(Optional) Install dependencies via nix:

```bash
nix-shell
```

Build and install the project:

```bash
$ cabal install
```

## Usage

### Reducing

The main use case of hsreduce is in reducing Haskell programs.
hsreduce expects as inputs a single Haskell file and a shell script which outputs whether the file is interesting.

Example:

```bash
hsreduce reduce --shellScript interesting.sh --testCase Bug.hs --numberOfThreads 4 --timeOut 30
```

Parameters:

|Parameter        |Explanation                                      |
|-----------------|-------------------------------------------------|
|--shellScript    |path to the interestingness test                 |
|--testCase       |path to the Haskell program you want to reduce   |
|--numberOfThreads|self-explanatory                                 |
|--timeout        |after how many seconds should tests be terminated|

After hsreduce is done with reducing, it outputs a file called `<original_file_name>_hsreduce.hs` (if the original file name was `Bug.hs` this would be `Bug_hsreduce.hs` then), which is the reduced test case.

### How do I write interestingness tests?

Example: Let's say you have a large Haskell file that prints "hello world" when you run it, but you're only interested in which part of the program makes it do that.
I think a good interestingness test would look like this:

```bash
#!/usr/bin/env bash
ghc Bug.s && ./Bug |& grep 'hello world'
```

### Merging

Another use case is merging cabal projects into single Haskell files, which can then be reduced.

Example:

```bash
hsreduce merge --projectType Executable --targetName containers
```

|Parameter        |Explanation                               |
|-----------------|------------------------------------------|
|--projectType    |enter either *Executable* or *Library*    |
|--targetName     |name of the cabal target you want to merge|

hsreduce takes your cabal project and tries to merge it into a module called `AllInOne.hs`, which is created in the current directory.

## FAQ

**Q: I find this to be strange behaviour: "a shell script that returns exit code 0 if the file contains interesting behavior (reproduces a bug) and returns 1 otherwise ". Surely it makes more sense that something that checks for buggy behaviour returns a non-zero exitcode if it finds a bug?**

A: You're correct.
But here reproducing the bug is the "normal behavior" that we seek and anything else not.
If a reduction is done that leads to a test case that crashes for another reason or leads to a timeout, then it's not "normal behavior".

**Q: what do you mean by reduction?**

A: By reduction I mean "making the Haskell file smaller to make it more easily readable by humans".
This is done by trying to delete various elements from the Haskell program or by simplifying elements.
