# Streamly Examples

Practical examples to demonstrate the features and performance of
Streamly.  Includes examples about file IO, text processing, networking,
concurrent programming, reactive programming and more.

These examples also serve as a beginner's guide to express practical
programs using the dataflow programming (streaming) model.  Please visit
the [Streamly homepage](https://streamly.composewell.com) for more
details and comprehensive documentation.

## Running The Examples

When running the unstable version (downloaded from the git repository)
you must use `--project-file cabal.project.user` option otherwise the
build might fail. For example:

```
$ cabal run --project-file cabal.project.user AcidRain
```

Executable names are the same as the filenames.  To run an example:

```
$ cabal run AcidRain
```

To run SDL2 based examples, make sure that you have the OS package for
the sdl2 library installed on your system and the headers are visible to
cabal:

```
$ cabal run --flag sdl2 CirclingSquare
```

To run interop examples, use the `interop` build flag:

```
$ cabal run --flag interop Interop.Pipes
```

## List of examples

### General

* [Intro](https://github.com/composewell/streamly-examples/blob/master/examples/Intro.hs): Simple, introductory examples - loops, text
  processing, networking, concurrency.
* [MergeSort](https://github.com/composewell/streamly-examples/blob/master/examples/MergeSort.hs): Sort a stream concurrently using merge
  sort.
* [Rate](https://github.com/composewell/streamly-examples/blob/master/examples/Rate.hs): Run an action at a given rate.

### FileSystem

* [CoreUtils](https://github.com/composewell/streamly-examples/blob/master/examples/CoreUtils.hs): Implement simplified coreutils
  like `cat`, `cp`, `tee`, `grep` using `Streamly.FileSystem.File` API.
* [CoreUtilsHandle](https://github.com/composewell/streamly-examples/blob/master/examples/CoreUtilsHandle.hs): Implement simplified
  coreutils using `Streamly.FileSystem.Handle` API.
* [Split](https://github.com/composewell/streamly-examples/blob/master/examples/Split.hs): Implement file splitting utility `split`.
* [FileSystemEvent](https://github.com/composewell/streamly-examples/blob/master/examples/FileSystemEvent.hs): File watching/fsnotify API
  example.
* [ListDir](https://github.com/composewell/streamly-examples/blob/master/examples/ListDir.hs): List a directory tree recursively and
  concurrently, faster than rust fd.

### Text Processing

* [CamelCase](https://github.com/composewell/streamly-examples/blob/master/examples/CamelCase.hs): Convert a file to camel case.
* [WordCount](https://github.com/composewell/streamly-examples/blob/master/examples/WordCount.hs): Simple word counting (`wc`) program.
* [WordCount.c](https://github.com/composewell/streamly-examples/blob/master/examples/WordCount.c): C equivalent for perf comparison.
* [WordCountModular](https://github.com/composewell/streamly-examples/blob/master/examples/WordCountModular.hs): Modular version.
* [WordCountParallel](https://github.com/composewell/streamly-examples/blob/master/examples/WordCountParallel.hs): Concurrent version.
* [WordCountParallelUTF8](https://github.com/composewell/streamly-examples/blob/master/examples/WordCountParallelUTF8.hs): Concurrent
  version with full UTF8 handling.
* [WordFrequency](https://github.com/composewell/streamly-examples/blob/master/examples/WordFrequency.hs): Count word frequency in
  a file and print top 25 words.
* [CSVParser](https://github.com/composewell/streamly-examples/blob/master/examples/CSVParser.hs): Process a CSV file
* [DateTimeParser](https://github.com/composewell/streamly-examples/blob/master/examples/DateTimeParser.hs): Parse a Date/Time string.
* [LogParser](https://github.com/composewell/streamly-examples/blob/master/examples/LogParser.hs): Parse a quoted string with escaping.

### Networking

* [EchoServer](https://github.com/composewell/streamly-examples/blob/master/examples/EchoServer.hs): A concurrent TCP server that
  echoes everything that it receives.
* [MergeServer](https://github.com/composewell/streamly-examples/blob/master/examples/MergeServer.hs): Merges lines received from
  several client connections and writes them to a file.
* [FileSender](https://github.com/composewell/streamly-examples/blob/master/examples/FileSender.hs): Send many files concurrently to
  a server over multiple connections. Can be used to test `MergeServer`.
* [CmdServer](https://github.com/composewell/streamly-examples/blob/master/examples/CmdServer.hs): Receive a stream of commands from many
  clients and respond to them using command handlers.
* [CmdClient](https://github.com/composewell/streamly-examples/blob/master/examples/CmdClient.hs): Run multiple concurrent clients sending
  streams of commands to a server and receiving responses. Can be used to test
  `CmdServer`.
* [WordServer](https://github.com/composewell/streamly-examples/blob/master/examples/WordServer.hs): A word look up (dictionary)
  server, instead of performing a real DB query the server just adds a
  time delay to simulate the IO.

### FRP/Games/Animation

* [AcidRain](https://github.com/composewell/streamly-examples/blob/master/examples/AcidRain.hs): A console game with deteriorating health
  that can be modified by typing "harm" and "potion" commands.
* [CirclingSquare](https://github.com/composewell/streamly-examples/blob/master/examples/CirclingSquare.hs): Use SDL2 to display a
  square that moves in a circle, and follows the mouse pointer.

### Interoperation

* [Interop.Streaming](https://github.com/composewell/streamly-examples/blob/master/examples/Interop/Streaming.hs): Converting streamly
  stream type to and from `streaming` stream type.
* [Interop.Pipes](https://github.com/composewell/streamly-examples/blob/master/examples/Interop/Pipes.hs): Converting streamly
  stream type to and from `pipes` stream type.
* [Interop.Conduit](https://github.com/composewell/streamly-examples/blob/master/examples/Interop/Conduit.hs): Converting streamly
  stream type to and from `conduit` stream type.
* [Interop.Vector](https://github.com/composewell/streamly-examples/blob/master/examples/Interop/Vector.hs): Converting streamly
  stream type to and from `vector` stream type.

## Comparing Haskell Performance with C and Rust

### Word Count

`examples/WordCount.c` is the C equivalent of `examples/WordCount.hs`:

```
$ make
$ examples/WordCount input.txt
```

### Directory Traversal

`examples/ListDirBasic.c` is the C equivalent of `examples/ListDirBasic.hs`:

```
$ make
$ examples/ListDirBasic
```

`examples/ListDirBasic.rs` is the Rust equivalent of
`examples/ListDirBasic.hs` using `std::fs` :

```
$ cargo build --example ListDirBasic --release
$ target/release/examples/ListDirBasic
```

`example/WalkDirBasic.rs` is the Rust equivalent of
``example/ListDirBasic.hs` using the `walkdir` crate:

```
$ cargo build --example WalkDirBasic --release
$ target/release/examples/WalkDirBasic
```

## Licensing

Available under [Apache-2.0 license](https://github.com/composewell/streamly-examples/blob/master/LICENSE).
