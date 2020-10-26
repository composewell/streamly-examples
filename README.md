# Running The Examples

## Running directly using stack
You can run these examples using `stack` like this:

```
$ stack build
$ stack examples/AcidRain.hs
```

Note: This method may not work for `CirclingSquare.hs` SDL animation example.

## Build and run

Build all the examples e.g.

```
stack build
cabal v2-build
```

Then run the executables, for example:

```
stack exec AcidRain
cabal v2-run AcidRain
```

The executable name are the same as the filenames.

## Running the SDL animation example

To include the SDL examples as well build with `examples-sdl` flag:

```
stack build --flag examples-sdl
cabal v2-build --flags examples-sdl
```

Make sure that you have the SDL OS package installed on your system and the
headers are visible to Haskell build tool.

```
stack exec CirclingSquare --flag examples-sdl
cabal v2-run CirclingSquare --flags examples-sdl
```
