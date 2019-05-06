# effects-benchmarks

This is a set of memory and time benchmarks for the various Haskell effects libraries, including `fused-effects`, `polysemy`, `freer-simple`, and `extensible-effects`. Benchmarks for equivalent `mtl` and handwritten computations are included for comparison purposes. It uses Cabal's new Backpack feature to save a great deal of boilerplate code.

# tl;dr

`fused-effects` and `mtl` achieve roughly commensurate performance. `polysemy` is within an order of magnitude of `fused-effects` and `mtl`. `freer-simple` and `extensible-effects` are considerably slower.

# Full results

On a 15-inch 2017 MacBook Pro (3.1GHz Intel i7, 16GB RAM), here are results for space benchmarks:

```
Countdown

  Case                Allocated
  fused-effects              40
  mtl                        40
  polysemy                1,304
  freer-simple            1,096
  extensible-effects      1,304
  shallow                   232

Countdown + exc

  Case                Allocated  GCs
  fused-effects             336
  mtl                       336
  polysemy                  792
  freer-simple              864
  extensible-effects        984
  shallow                   320
```

(The fact that `fused-effects` and `mtl` have the same allocations is not an error; `fused-effects` was designed to have almost identical performance characteristics to `mtl`.)

Time (`State`-based countdown):

```
benchmarked Countdown/Put/fused-effects
time                 5.692 μs   (5.570 μs .. 5.835 μs)

benchmarked Countdown/Put/mtl
time                 5.500 μs   (5.386 μs .. 5.719 μs)

benchmarked Countdown/Put/polysemy
time                 1.903 ms   (1.845 ms .. 1.954 ms)

benchmarked Countdown/Put/freer-simple
time                 1.324 ms   (1.282 ms .. 1.361 ms)

benchmarked Countdown/Put/extensible-effects
time                 1.383 ms   (1.343 ms .. 1.422 ms)

benchmarked Countdown/Put/shallow
time                 275.1 μs   (270.1 μs .. 281.6 μs)
```

Time (`State` + `Error` countdown):

```
benchmarked Countdown/Put+Exc/fused-effects
time                 9.829 μs   (9.645 μs .. 10.04 μs)

benchmarked Countdown/Put+Exc/mtl
time                 9.968 μs   (9.779 μs .. 10.15 μs)

benchmarked Countdown/Put+Exc/polysemy
time                 418.1 μs   (408.9 μs .. 427.1 μs)

benchmarked Countdown/Put+Exc/freer-simple
time                 1.157 ms   (1.120 ms .. 1.199 ms)

benchmarked Countdown/Put+Exc/extensible-effects
time                 1.232 ms   (1.212 ms .. 1.262 ms)

benchmarked Countdown/Put+Exc/shallow
time                 55.12 μs   (54.34 μs .. 56.29 μs)
```

Time (custom HTTP DSL):

```
benchmarked HTTP/fused-effects
time                 8.311 μs   (8.060 μs .. 8.592 μs)

benchmarked HTTP/polysemy
time                 14.99 ms   (14.26 ms .. 15.80 ms)

benchmarked HTTP/extensible-effects
time                 1.612 ms   (1.578 ms .. 1.654 ms)

benchmarked HTTP/Deep embedding
time                 2.027 ms   (1.876 ms .. 2.184 ms)

benchmarked HTTP/Shallow embedding
time                 15.94 μs   (15.49 μs .. 16.46 μs)

benchmarked HTTP/freer-simple
time                 1.478 ms   (1.443 ms .. 1.515 ms)
```

# Future work
* More benchmarks: `Writer`, logging, `grammar`, `pipes`.
* `polysemy` can probably get some more speed out of it: some of these results are slightly baffling.
* I am going to make this emit pretty graphs, or at least graphable .csv files, someday.
