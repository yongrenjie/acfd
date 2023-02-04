## acfd

Command-line programme to find acronyms in a LaTeX project.

It's very crude; this mainly served as an OCaml learning exercise for myself.
Words are defined as anything matching the regex `[A-Za-z-]+`.
Acronyms are defined as words with more than two capital letters (`[A-Z]`) in them.
This leads to, obviously, lots of false-positives and also lots of spurious 'acronyms' which are really LaTeX commands.

To use, first make sure you have `opam` installed, then

```
git clone git@github.com:yongrenjie/acfd.git
cd acfd
opam install dune cmdliner ounit2
dune build
dune exec -- acfd <OPTIONS>
```

`ounit2` is a test dependency, so technically you don't need to install it, and you can compile just the binary using `dune build bin` instead of `dune build`.
