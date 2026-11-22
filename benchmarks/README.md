# Emissions layout benchmark

This standalone benchmark compares three layouts for synthetic records based on
GCAM emissions objects. Each record has two identifiers, two numerical
parameters, and eight periods of writable state. The object case allocates a
record and its state separately. The column cases use Apache Arrow CPU buffers
for parameters and state. The shared case uses a 32-bit row index to share
parameters across records.

Build with Apache Arrow C++, pkg-config, and a C++20 compiler:

```sh
c++ -O3 -std=c++20 $(pkg-config --cflags arrow) benchmarks/emissions_layout.cpp \
    $(pkg-config --libs arrow) -o /tmp/emissions_layout
```

Run each mode in a separate process. The second number gives the number of
distinct parameter rows. Use the record count in both arguments to measure the
cost of sharing when every row is unique.

```sh
for mode in objects columns shared; do
    /tmp/emissions_layout "$mode" 30000000 3000000
done
```

Output is CSV with the layout, number of records, number of distinct parameter
rows, peak RSS in MiB, update time in milliseconds for 20 passes, and a checksum.
The checksum should agree across layouts for a given input size and distinct-row
count. Peak RSS includes process startup and initialization. Update time excludes
initialization and the checksum calculation.

The benchmark isolates the cost of storing and scanning records. It omits
GCAM's emissions drivers, controls, and market solve. It does not measure the
solver-state change or predict GCAM's whole-process peak.

## Enable Arrow-backed solver state

On the Linux Makefile build, install the Arrow C++ package so that
`pkg-config --exists arrow` succeeds, then build with `USE_ARROW=1`:

```sh
make gcam USE_ARROW=1
```

Use a clean build when changing this flag. The flag selects C++20 and uses Arrow
CPU buffers for the existing base-state and scratch double arrays. The packed
pointer index is used with or without Arrow.

## Sample results

Medians of three runs on an Apple Silicon Mac with Arrow 26.0.1. These
rows use ten object records per distinct parameter row.

| Records | Layout | Peak RSS (MiB) | Update time (ms) |
| ---: | --- | ---: | ---: |
| 1,000,000 | Objects | 145.03 | 73.17 |
| 1,000,000 | Columns | 105.44 | 9.12 |
| 1,000,000 | Shared columns | 81.72 | 13.91 |
| 10,000,000 | Objects | 1329.94 | 947.65 |
| 10,000,000 | Columns | 929.59 | 102.41 |
| 10,000,000 | Shared columns | 693.00 | 145.05 |
| 30,000,000 | Objects | 3902.28 | 15731.95 |
| 30,000,000 | Columns | 2760.97 | 313.96 |
| 30,000,000 | Shared columns | 2051.23 | 452.66 |

At ten million unique parameter rows, plain and shared columns used 929.58
and 967.80 MiB respectively. Sharing requires a row ID for each object and
only saves memory when enough parameter rows repeat.
