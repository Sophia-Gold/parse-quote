To run on a test file in order packets are received:

```
./parse-quote test/golden.pcap
```

...and in order of quote accept time:

```
./parse-quote -r test/golden.pcap
```

The project is intended to be built with Stack and Nix (for libpcap). It contains small test and benchmark suites using `tasty`, `HUnit`, and `criterion`.

The ELF executable was compiled with optimization level two and the following RTS flags: `-N4`, `-IO`, and `-A8G`. For optimal performance I recommend compiling with a number of processors and allocation area appropriate for your machine.

This project uses `Network.Pcap`, a thin libpcap wrapper. Packets are stored in strict `Data.Maps` wrapped in `MVars`. Packet bodies are parsed as `ByteStrings` using `attoparsec` with custom `TextShow` instances used for printing.This performance scales well with parallelism, although a stream-based approach would likely be both more efficient and flexible for a realtime system rather than dump file parser.
