# Chord

A modular DHT implementation based on
[chord](http://en.wikipedia.org/wiki/Chord_%28peer-to-peer%29). It is
not bound to a particular transport, only assuming that `B-encode` values
can be sent, and that the receiver knows where messages come from.

## Build

You will need OCaml >= 3.12 (at least, I test on 4.00.1). The library
`lwt` also needs to be installed, either by hand or with opam:

    $ opam install lwt

Then, type

    $ make

and it should build the library.

## TODO

- an implementation that uses local Lwt threads as nodes, to test the DHT
    in a single process
- an implementation that uses Lwt with UDP messages
- a CLI tool based on the UDP implementation
