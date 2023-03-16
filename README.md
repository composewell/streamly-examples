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

* [Intro](examples/Intro.hs): Simple, introductory examples - loops, text
  processing, networking, concurrency.
* [MergeSort](examples/MergeSort.hs): Sort a stream concurrently using merge
  sort.
* [Rate](examples/Rate.hs): Run an action at a given rate.

### FileSystem

* [CoreUtils](examples/CoreUtils.hs): Implement simplified coreutils
  like `cat`, `cp`, `tee`, `grep` using `Streamly.FileSystem.File` API.
* [CoreUtilsHandle](examples/CoreUtilsHandle.hs): Implement simplified
  coreutils using `Streamly.FileSystem.Handle` API.
* [Split](examples/Split.hs): Implement file splitting utility `split`.
* [FileSystemEvent](examples/FileSystemEvent.hs): File watching/fsnotify API
  example.
* [ListDir](examples/ListDir.hs): List a directory tree recursively and
  concurrently.

### Text Processing

* [CamelCase](examples/CamelCase.hs): Convert a file to camel case.
* [WordCount](examples/WordCount.hs): Simple word counting (`wc`) program.
* [WordCount.c](examples/WordCount.c): C equivalent for perf comparison.
* [WordCountModular](examples/WordCountModular.hs): Modular version.
* [WordCountParallel](examples/WordCountParallel.hs): Concurrent version.
* [WordCountParallelUTF8](examples/WordCountParallelUTF8.hs): Concurrent 
  version with full UTF8 handling.
* [WordFrequency](examples/WordFrequency.hs): Count word frequency in
  a file and print top 25 words.
* [CSVParser](examples/CSVParser.hs): Parse a CSV file.
* [DateTimeParser](examples/DateTimeParser.hs): Parse a Date/Time string.
* [LogParser](examples/LogParser.hs): Parse a quoted string with escaping.

### Networking

* [EchoServer](examples/EchoServer.hs): A concurrent TCP server that
  echoes everything that it receives.
* [MergeServer](examples/MergeServer.hs): Merges lines received from
  several client connections and writes them to a file.
* [FileSender](examples/FileSender.hs): Send many files concurrently to
  a server over multiple connections. Can be used to test `MergeServer`.
* [CmdServer](examples/CmdServer.hs): Receive a stream of commands from many
  clients and respond to them using command handlers.
* [CmdClient](examples/CmdClient.hs): Run multiple concurrent clients sending
  streams of commands to a server and receiving responses. Can be used to test
  `CmdServer`.
* [WordServer](examples/WordServer.hs): A word look up (dictionary)
  server, instead of performing a real DB query the server just adds a
  time delay to simulate the IO.

### FRP/Games/Animation

* [AcidRain](examples/AcidRain.hs): A console game with deteriorating health
  that can be modified by typing "harm" and "potion" commands.
* [CirclingSquare](examples/CirclingSquare.hs): Use SDL2 to display a
  square that moves in a circle, and follows the mouse pointer.

### Interoperation

* [Interop.Streaming](examples/Interop/Streaming.hs): Converting streamly
  stream type to and from `streaming` stream type.
* [Interop.Pipes](examples/Interop/Pipes.hs): Converting streamly
  stream type to and from `pipes` stream type.
* [Interop.Conduit](examples/Interop/Conduit.hs): Converting streamly
  stream type to and from `conduit` stream type.
* [Interop.Vector](examples/Interop/Vector.hs): Converting streamly
  stream type to and from `vector` stream type.

## Licensing

Available under [Apache-2.0 license](LICENSE).
