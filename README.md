# Tsuru Capital Code Sample


To run on your test file in order packets are recieved:

```
./parse-quote test/mdf-kospi200.20110216-0.pcap
```

And in order of quote accept time:

```
./parse-quote -r test/mdf-kospi200.20110216-0.pcap
```

As instructed, packets with data types other than "B6" and/or information types other than "03" are discarded.

The project can be built with Stack and Nix (for libpcap and other dependencies). It also a small test suite using `tasty` and `HUnit` and benchmark suite using `criterion`.

The ELF executable was compiled with optimization level two and the following RTS flags: `-N4`, `-IO`, and `-A8G`. For optimal performance I recommend compiling with a number of processors and allocation area appropriate for your machine.

This project uses `Network.Pcap`, a thin libpcap wrapper. Please note it hasn't been maintained since 2012 and therefore likely would _not_ be my choice for capturing packets over UDP in production, but was at an ideal level of abstraction for this execise. In order to work with its callback-based design, packets are stored in strict `Data.Maps` wrapped in `MVars`. This performance scales well with parallelism, although a stream-based approach would likely be both more efficient and flexible for a realtime system rather than dump file parser.

Individual packet bodies are parsed as `ByteStrings` using `attoparsec`. This could likely be optimized further, e.g. quotes are represented as boxed `Ints` whereas primitive `Int16s` would have sufficed. However, benchmarks showed the parser was far from the bottleneck so I honored Knuth's advice and refrained from overoptimizing it. 

Custom `TextShow` instances are used for printing although this also contributes a very small amount to overrall performance.
