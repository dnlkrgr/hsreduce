# ToDo's / Protocol

## Evaluation
- disable each pass and see how performance changes
  - [x] might need to change how we receive a list of passes
    - [x] in Driver: get a name of pass

  - [x] need to change how we store statistics
    - let's do it in <hsreduce-dir>/statistics
    - naming scheme: hsreduce_<name-of-filtered-out-pass>_statistics.csv

  - [x] running test iterations:
  - then for each pass:
    - filter it out
    - call hsreduce
    - read statistics file
    - take difference of tokens, bytes reduced and running time
    - append a file: <hsreduce-dir>/hsreduce_diff_stats.csv
      - (tokens, bytes, seconds)

  <!-- WE DON'T NEED THAT, WE CAN JUST COMPARE THE NUMBER OF TOKENS DIRECTLY -->
  <!-- - first, we need to record the baseline
    - check running time => total amount will be ~ 31 times that amount -->

  - examine:
    - which are the most valuable passes?
      - how much tokens less do we reduce
      - how do the successful attempts change for the other passes?
    - which are the most expensive (running time) passes?

<!-- TAKES TOO MUCH TIME -->
<!-- - run different orderings of passes
  - call hsreduce from cli with list of passes as an argument
  
  - [x] parse list of passes
    - [x] filter "allPasses" to look if the pass is in there, return that pass
  
  - some passes should have a fixed place
    - at the start:
      - rmvDecls
    - at the end:
      - simplifyExpr
      - simplifyLit
  
  - record: chosen list of passes, time, reduced size, reduced number of tokens -->

## Testing
- [x] run passes that take less than an hour
- [x] count number of tokens
- check: 
  - [x] that number of tokens is not above a certain value
  - [x] that time didn't grow excessively

## Reporting to implement

- Passes
  - [ ] how much time was spent during the pass
  - [ ] if a pass looks at different elements: what kind of expression / element brought the most reduction
  - [ ] what are the best / worst passes, and why? How could they improve?

## creduce's core

```c++
current = original_test_case
do
  size_at_start = size(current)
  foreach (p, option) in pass_list
    state = p::new(current, option)
    do
      variant = current         // this is a file copy operation
      result = p::transform(variant, option, state)
      if result == ERROR
  report_problem_in_pass(p, option)
      if result == OK
        if is_interesting(variant)
          current = variant     // also a file copy
      else
        state = p::advance(current, option, state)
    while result == OK
while size(current) < size_at_start
```

## `creduce --not-c` Performance

- \<name of test-case>: <% amount reduced>, \<so far minimal file size>

- test-cases/ticket14270: **88.6 %, 389 bytes**
  - pass statistics:
    - method pass_balanced :: square-inside worked 1 times and failed 1 times
    - method pass_balanced :: square-only worked 1 times and failed 1 times
    - method pass_clex :: rm-toks-9 worked 1 times and failed 522 times
    - method pass_clex :: rm-toks-15 worked 1 times and failed 439 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_clex :: rm-toks-13 worked 1 times and failed 454 times
    - method pass_balanced :: parens-to-zero worked 1 times and failed 109 times
    - method pass_lines :: 2 worked 1 times and failed 377 times
    - method pass_clex :: rm-toks-16 worked 2 times and failed 407 times
    - method pass_peep :: a worked 2 times and failed 173 times
    - method pass_clex :: rm-toks-12 worked 2 times and failed 467 times
    - method pass_balanced :: parens-inside worked 2 times and failed 66 times
    - method pass_balanced :: parens-only worked 2 times and failed 21 times
    - method pass_clex :: rm-toks-10 worked 3 times and failed 492 times
    - method pass_balanced :: parens worked 3 times and failed 69 times
    - method pass_balanced :: curly worked 3 times and failed 24 times
    - method pass_lines :: 1 worked 4 times and failed 390 times
    - method pass_clex :: rm-toks-6 worked 4 times and failed 571 times
    - method pass_clex :: rm-toks-8 worked 5 times and failed 531 times
    - method pass_clex :: rm-toks-2 worked 5 times and failed 681 times
    - method pass_lines :: 6 worked 6 times and failed 402 times
    - method pass_clex :: rm-toks-5 worked 7 times and failed 595 times
    - method pass_clex :: rm-toks-4 worked 7 times and failed 626 times
    - method pass_clex :: rm-toks-3 worked 9 times and failed 654 times
    - method pass_clex :: rename-toks worked 13 times and failed 46 times
    - method pass_clex :: rm-toks-1 worked 14 times and failed 691 times
    - method pass_clex :: rm-tok-pattern-4 worked 36 times and failed 2719 times
    - method pass_lines :: 0 worked 39 times and failed 542 times
- test-cases/ticket14779: **77.6%, 521 bytes**
  - time:
    - real	**4m49.066s**
    - user	26m10.421s
    - sys	6m33.660s
  - pass statistics:
    - method pass_clex :: rm-toks-2 worked 1 times and failed 343 times
    - method pass_clex :: delete-string worked 1 times and failed 2 times
    - method pass_clex :: rm-toks-5 worked 1 times and failed 330 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_balanced :: square-inside worked 1 times and failed 7 times
    - method pass_clex :: rm-toks-10 worked 1 times and failed 320 times
    - method pass_clex :: rm-toks-4 worked 2 times and failed 335 times
    - method pass_clex :: rm-toks-1 worked 2 times and failed 345 times
    - method pass_lines :: 1 worked 3 times and failed 181 times
    - method pass_clex :: rm-tok-pattern-4 worked 6 times and failed 2448 times
    - method pass_clex :: rename-toks worked 13 times and failed 36 times  
    - method pass_lines :: 0 worked 20 times and failed 199 times
- test-cases/ticket15696_1: **68.6 %, 431 bytes**
  - pass statistics:
    - method pass_clex :: rm-toks-7 worked 1 times and failed 214 times
    - method pass_clex :: rm-toks-11 worked 1 times and failed 203 times
    - method pass_balanced :: square-inside worked 1 times and failed 1 times
    - method pass_clex :: rm-toks-15 worked 1 times and failed 188 times
    - method pass_balanced :: parens worked 1 times and failed 28 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_balanced :: parens-only worked 1 times and failed 23 times
    - method pass_clex :: rm-toks-3 worked 2 times and failed 229 times
    - method pass_clex :: rm-toks-4 worked 2 times and failed 221 times
    - method pass_clex :: rm-tok-pattern-4 worked 2 times and failed 1480 times
    - method pass_clex :: rm-toks-2 worked 4 times and failed 235 times
    - method pass_clex :: rm-toks-1 worked 5 times and failed 243 times
    - method pass_clex :: rename-toks worked 8 times and failed 57 times
    - method pass_lines :: 0 worked 16 times and failed 201 times
- test-cases/ticket15753: **66.2 %, 1312 bytes**
  - pass statistics:
    - method pass_lines :: 2 worked 1 times and failed 391 times
    - method pass_balanced :: parens-to-zero worked 1 times and failed 138 times
    - method pass_balanced :: parens worked 1 times and failed 100 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_clex :: rm-toks-8 worked 1 times and failed 1137 times
    - method pass_indent :: regular worked 1 times and failed 0 times
    - method pass_balanced :: curly worked 1 times and failed 34 times
    - method pass_clex :: rm-toks-11 worked 1 times and failed 1125 times
    - method pass_lines :: 1 worked 1 times and failed 413 times
    - method pass_clex :: rm-toks-5 worked 2 times and failed 1184 times
    - method pass_clex :: rm-toks-7 worked 3 times and failed 1145 times
    - method pass_clex :: rm-toks-6 worked 3 times and failed 1166 times
    - method pass_clex :: rm-toks-4 worked 3 times and failed 1194 times
    - method pass_clex :: rm-toks-3 worked 6 times and failed 1206 times
    - method pass_peep :: a worked 6 times and failed 1020 times
    - method pass_clex :: rm-toks-2 worked 6 times and failed 1224 times
    - method pass_lines :: 6 worked 7 times and failed 409 times
    - method pass_lines :: 0 worked 13 times and failed 505 times
    - method pass_clex :: rm-tok-pattern-4 worked 14 times and failed 8816 times
    - method pass_clex :: rm-toks-1 worked 17 times and failed 1236 times
    - method pass_clex :: rename-toks worked 19 times and failed 142 times
- test-cases/ticket15696_2: **96.3 %, 1970 bytes**
  - time:
    - real **83m7.090s**
    - user 377m37.378s
    - sys 65m23.330s
  - pass statistics:
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_clex :: rm-toks-15 worked 1 times and failed 935 times
    - method pass_clex :: rm-toks-8 worked 1 times and failed 1082 times
    - method pass_balanced :: square-inside worked 1 times and failed 7 times
    - method pass_clex :: rm-toks-11 worked 1 times and failed 1026 times
    - method pass_balanced :: square-only worked 1 times and failed 29 times
    - method pass_balanced :: angles worked 1 times and failed 97 times
    - method pass_clex :: rm-toks-14 worked 1 times and failed 950 times
    - method pass_clex :: rm-toks-13 worked 2 times and failed 964 times
    - method pass_balanced :: curly worked 2 times and failed 14 times
    - method pass_clex :: rm-toks-9 worked 2 times and failed 1064 times
    - method pass_ints :: a worked 2 times and failed 0 times
    - method pass_clex :: rm-toks-5 worked 2 times and failed 1134 times
    - method pass_clex :: rm-toks-7 worked 2 times and failed 1090 times
    - method pass_lines :: 10 worked 3 times and failed 1857 times
    - method pass_clex :: rm-toks-12 worked 3 times and failed 990 times
    - method pass_clex :: rm-toks-10 worked 3 times and failed 1034 times
    - method pass_balanced :: curly-only worked 4 times and failed 3 times
    - method pass_lines :: 8 worked 5 times and failed 1868 times
    - method pass_clex :: rm-toks-6 worked 5 times and failed 1104 times
    - method pass_balanced :: parens-inside worked 5 times and failed 243 times
    - method pass_lines :: 6 worked 7 times and failed 1908 times
    - method pass_clex :: rm-toks-3 worked 7 times and failed 1192 times
    - method pass_balanced :: parens-only worked 8 times and failed 127 times
    - method pass_balanced :: parens worked 9 times and failed 267 times
    - method pass_peep :: a worked 10 times and failed 337 times
    - method pass_clex :: rm-toks-4 worked 12 times and failed 1144 times
    - method pass_lines :: 3 worked 13 times and failed 2006 times
    - method pass_lines :: 4 worked 16 times and failed 1952 times
    - method pass_clex :: rm-tok-pattern-4 worked 22 times and failed 7088 times
    - method pass_clex :: rm-toks-2 worked 23 times and failed 1214 times
    - method pass_balanced :: parens-to-zero worked 25 times and failed 307 times
    - method pass_clex :: rm-toks-1 worked 26 times and failed 1261 times
    - method pass_lines :: 2 worked 28 times and failed 2225 times
    - method pass_clex :: rename-toks worked 29 times and failed 136 times
    - method pass_lines :: 1 worked 87 times and failed 2822 times
    - method pass_lines :: 0 worked 327 times and failed 3862 times
- test-cases/ticket14040_2: **59.1 %, 1057 bytes**
  - time:
    - real	**14m25.227s**
    - user	79m4.139s
    - sys	18m28.241s
  -pass statistics:
    - method pass_balanced :: parens-inside worked 1 times and failed 114 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_clex :: rm-toks-7 worked 1 times and failed 1011 times
    - method pass_clex :: rm-toks-9 worked 1 times and failed 994 times
    - method pass_balanced :: parens-to-zero worked 1 times and failed 154 times
    - method pass_lines :: 3 worked 1 times and failed 354 times
    - method pass_clex :: rm-toks-8 worked 1 times and failed 1003 times
    - method pass_clex :: rm-toks-5 worked 3 times and failed 1018 times
    - method pass_balanced :: parens worked 3 times and failed 115 times
    - method pass_balanced :: square-only worked 4 times and failed 43 times
    - method pass_clex :: rm-toks-1 worked 4 times and failed 1089 times
    - method pass_clex :: rm-toks-4 worked 4 times and failed 1033 times
    - method pass_clex :: rm-toks-2 worked 5 times and failed 1079 times
    - method pass_peep :: a worked 5 times and failed 1293 times
    - method pass_clex :: rename-toks worked 8 times and failed 101 times
    - method pass_clex :: rm-toks-3 worked 10 times and failed 1049 times
    - method pass_lines :: 0 worked 17 times and failed 399 times
    - method pass_clex :: rm-tok-pattern-4 worked 23 times and failed 7576 times
- test-cases/ticket16979: **45.2 % , 4141 bytes**
  - pass statistics:
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_balanced :: curly worked 1 times and failed 89 times
    - method pass_balanced :: parens-inside worked 1 times and failed 435 times
    - method pass_clex :: rm-toks-10 worked 1 times and failed 2981 times
    - method pass_ints :: a worked 1 times and failed 70 times
    - method pass_peep :: a worked 1 times and failed 3412 times
    - method pass_clex :: rm-toks-6 worked 2 times and failed 3009 times 
    - method pass_clex :: rm-toks-9 worked 2 times and failed 2991 times
    - method pass_balanced :: parens worked 2 times and failed 441 times
    - method pass_balanced :: parens-to-zero worked 3 times and failed 573 times
    - method pass_balanced :: parens-only worked 5 times and failed 398 times
    - method pass_clex :: rm-toks-5 worked 6 times and failed 3021 times
    - method pass_clex :: rm-toks-4 worked 7 times and failed 3051 times
    - method pass_lines :: 1 worked 8 times and failed 1239 times
    - method pass_clex :: rm-toks-1 worked 13 times and failed 3181 times
    - method pass_clex :: rm-toks-2 worked 15 times and failed 3151 times
    - method pass_clex :: rm-toks-3 worked 24 times and failed 3079 times
    - method pass_clex :: rename-toks worked 35 times and failed 242 times
    - method pass_clex :: rm-tok-pattern-4 worked 35 times and failed 23328 times
    - method pass_lines :: 0 worked 47 times and failed 1332 times
- test-cases/ticket8763: **72.7 %, 690 bytes**
  - pass statistics:
    - method pass_clex :: rm-toks-12 worked 1 times and failed 720 times
    - method pass_clex :: rm-toks-5 worked 1 times and failed 783 times
    - method pass_clex :: rm-toks-8 worked 1 times and failed 742 times
    - method pass_clex :: rm-toks-10 worked 1 times and failed 732 times
    - method pass_balanced :: square-inside worked 2 times and failed 2 times
    - method pass_clex :: rm-toks-6 worked 2 times and failed 771 times
    - method pass_balanced :: parens worked 2 times and failed 33 times
    - method pass_balanced :: parens-only worked 2 times and failed 28 times
    - method pass_clex :: rm-toks-7 worked 3 times and failed 750 times
    - method pass_clex :: rm-toks-3 worked 4 times and failed 808 times
    - method pass_clex :: rm-toks-4 worked 5 times and failed 788 times
    - method pass_clex :: rm-toks-2 worked 5 times and failed 820 times
    - method pass_clex :: rm-toks-1 worked 8 times and failed 830 times
    - method pass_balanced :: parens-to-zero worked 8 times and failed 47 times
    - method pass_clex :: rename-toks worked 14 times and failed 90 times
    - method pass_clex :: rm-tok-pattern-4 worked 19 times and failed 5464 times
    - method pass_lines :: 0 worked 24 times and failed 232 times
- test-cases/ticket14827: **87.9 %, 2397 bytes**
  - pass statistics:
    - method pass_balanced :: parens worked 1 times and failed 10 times
    - method pass_clex :: rename-toks worked 6 times and failed 30 times
    - method pass_lines :: 0 worked 24 times and failed 84 times
- test-cases/ticket18098: **69.2 %, 3512 bytes**
  - time:
    - real	**66m29.549s**
    - user	357m19.655s
    - sys	  90m57.966s
  - pass statistics:
    - method pass_clex :: rm-toks-9 worked 1 times and failed 3838 times
    - method pass_clex :: rm-toks-10 worked 1 times and failed 3828 times
    - method pass_lines :: 2 worked 1 times and failed 1477 times
    - method pass_clex :: rm-toks-6 worked 1 times and failed 3869 times
    - method pass_ints :: a worked 1 times and failed 27 times
    - method pass_balanced :: square worked 1 times and failed 0 times
    - method pass_balanced :: parens-inside worked 1 times and failed 315 times
    - method pass_lines :: 8 worked 1 times and failed 1463 times
    - method pass_clex :: rm-toks-8 worked 1 times and failed 3847 times
    - method pass_clex :: rm-toks-12 worked 1 times and failed 3816 times
    - method pass_clex :: rm-toks-7 worked 2 times and failed 3855 times
    - method pass_balanced :: curly-only worked 2 times and failed 33 times
    - method pass_peep :: a worked 3 times and failed 1346 times
    - method pass_balanced :: parens-only worked 4 times and failed 245 times
    - method pass_balanced :: parens worked 5 times and failed 324 times
    - method pass_clex :: rm-toks-3 worked 6 times and failed 3958 times
    - method pass_clex :: rm-toks-5 worked 7 times and failed 3875 times
    - method pass_balanced :: curly worked 7 times and failed 46 times
    - method pass_clex :: rm-toks-2 worked 11 times and failed 3976 times
    - method pass_clex :: rm-toks-4 worked 12 times and failed 3910 times
    - method pass_lines :: 1 worked 13 times and failed 1464 times
    - method pass_balanced :: parens-to-zero worked 15 times and failed 401 times
    - method pass_clex :: rm-toks-1 worked 21 times and failed 3998 times
    - method pass_clex :: rm-tok-pattern-4 worked 27 times and failed 30104 times
    - method pass_clex :: rename-toks worked 38 times and failed 245 times
    - method pass_lines :: 0 worked 98 times and failed 1565 times

## structureshrink performance

- ticket14040: **623 bytes**
  - time:
    - real	154m8.747s
    - user	100m25.036s
    - sys	26m15.715s


## Ausarbeitung

- Passes
  - was gab es anfangs für Probleme? wie wurden die behoben?
    - welche Änderungen musste man durchführen, um die Pässe zu verbessern?
  - welche Feinheiten sind zu beachten?
  - welche syntaktischen Elemente sind "wieviel wert"?
  - wie effektiv sind die Pässe?
  - was waren Herausforderungen für Implementierung der Pässe?