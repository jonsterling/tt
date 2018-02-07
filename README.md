# tt

secret project

## Installing

### Prerequisites

| prerequisite |      | version                                                        | how to install                  |
| ------------ | ---- | :------------------------------------------------------------- | ------------------------------- |
| Opam         | `>=` | [`1.2.2`](https://github.com/ocaml/opam/releases/tag/1.2.2)    | manually or via package manager |
| OCaml        | `>=` | [`4.06.0`](https://github.com/ocaml/ocaml/releases/tag/4.06.0) | `opam switch 4.06.0`            |
| utop         | `>=` | [`2.0.2`](https://github.com/diml/utop/releases/tag/2.0.2)     | `opam install utop` (optional)  |

### Building

```
$ git clone https://github.com/jonsterling/tt
$ cd tt
$ opam update
$ opam pin add -y .
```

### Toplevel

Requires `utop` (see prerequisites).

```
$ ./script/top
```
