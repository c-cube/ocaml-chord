# Chord

A simple DHT implementation based on
[chord](http://en.wikipedia.org/wiki/Chord_%28peer-to-peer%29). It
uses `bencode_rpc` to communicate between nodes.

## Build

You will need OCaml >= 3.12 (at least, I test on 4.00.1). The libraries
`bencode`, `bencode_rpc` and `lwt` also need to be installed, either by hand
or with opam:

    $ opam install lwt bencode bencode_rpc

Then, type

    $ make

and it should build the library.

## License

BSD2, see the file `LICENSE`

## TODO

- test TCP implementation
- simple IRC-like messaging protocol (with broadcasting protocol)
- a CLI tool with TCP implementation

