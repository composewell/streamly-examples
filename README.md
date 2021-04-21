# Running The Examples

Executable names are the same as the filenames.  To run an example:

```
$ cabal run AcidRain
```

For performance sensitive examples use fusion-plugin for best performance:

```
$ cabal run --flag fusion-plugin WordCount -- streamly-examples.cabal
```

To run SDL2 based examples, make sure that you have the OS package for
the sdl2 library installed on your system and the headers are visible to
cabal:

```
$ cabal run --flag examples-sdl CirclingSquare
```

# List of examples

## General

* [Intro](examples/Intro.hs): Simple, introductory examples - loops, text
  processing, networking, concurrency.
* [MergeSort](examples/MergeSort.hs): Merge sorted streams concurrently.
* [Rate](examples/Rate.hs): Run an action at a given rate.
* [ControlFlow](examples/ControlFlow.hs): Combining control flow manipulating
  transformers (`MaybeT`, `ExceptT`, `ContT`) with streamly.

## CoreUtils/FileSystem

* [CoreUtils](examples/CoreUtils.hs): Implement simplified coreutils
  like `cat`, `cp`, `tee`, `grep` using `Streamly.FileSystem.File` API.
* [CoreUtilsHandle](examples/CoreUtilsHandle.hs): Implement simplified
  coreutils using `Streamly.FileSystem.Handle` API.
* [Split](examples/Split.hs): Implement file splitting utility `split`.
* [FileSystemEvent](examples/FileSystemEvent.hs): File watching/fsnotify API
  example.
* [ListDir](examples/ListDir.hs): List a directory tree recursively and
  concurrently.

## Text Processing

* [CamelCase](examples/CamelCase.hs): Convert a file to camel case.
* [WordCount](examples/WordCount.hs): Simple word counting (`wc`) program.
* [WordCount.c](examples/WordCount.c): C equivalent for perf comparison.
* [WordCountModular](examples/WordCountModular.hs): Modular version.
* [WordCountParallel](examples/WordCountParallel.hs): Concurrent version.
* [WordCountUTF8](examples/WordCountUTF8.hs): Concurrent version with
  full UTF8 handling.
* [WordFrequency](examples/WordFrequency.hs): Count word frequency in
  a file and print top 25 words.

## Networking

* [EchoServer](examples/EchoServer.hs): A concurrent TCP server that
  echoes everything that it receives.
* [MergeServer](examples/MergeServer.hs): Merges lines received from
  several client connections and writes them to a file.
* [FromFileClient](examples/FromFileClient.hs): Send many files concurrently to
  a server over multiple connections. Can be used to test `FileSinkServer`.
* [CmdServer](examples/CmdServer.hs): Receive a stream of commands from many
  clients and respond to them using command handlers.
* [CmdClient](examples/CmdClient.hs): Run multiple concurrent clients sending
  streams of commands to a server and receiving responses. Can be used to test
  `CmdServer`.
* [WordServer](examples/WordServer.hs): A word look up (dictionary)
  server, instead of performing a real DB query the server just adds a
  time delay to simulate the IO.

## FRP/Games/Animation

* [AcidRain](examples/AcidRain.hs): A console game with deteriorating health
  that can be modified by typing "harm" and "potion" commands.
* [CirclingSquare](examples/CirclingSquare.hs): Use SDL2 to display a
  square that moves in a circle, and follows the mouse pointer.
