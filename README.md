# tt

secret project

### Elaboration
Elaboration structure and algorithm is inspired by the following, in no specific order:

+ McBride's OLEG and its descendents (Epigram, Idris)
+ Sterling and Harper's "Dependent LCF"
+ Huet's "Constructive Engine"

We take the idea of tactics as information-increasing transitions between valid contexts (which contain definitional extensions) from McBride; unlike McBride we use explicit substitutions.

It is possible to view this as a generalization of Dependent LCF's proof state structure to include definitional extension; while Dependent LCF probably cannot be implemented efficiently, this generalization can.

## Installing

### Prerequisites

| prerequisite |      | version                                                                | how to install                  |
| ------------ | ---- | :--------------------------------------------------------------------- | ------------------------------- |
| Opam         | `>=` | [`1.2.2`](https://github.com/ocaml/opam/releases/tag/1.2.2)            | manually or via package manager |
| OCaml        | `>=` | [`4.06.1+flambda`](https://github.com/ocaml/ocaml/releases/tag/4.06.1) | `opam switch 4.06.1+flambda`    |
| utop         | `>=` | [`2.0.2`](https://github.com/diml/utop/releases/tag/2.0.2)             | `opam install utop` (optional)  |

### Installing Dependencies

```
$ git clone https://github.com/jonsterling/tt
$ cd tt
$ opam update
$ opam pin add -y .
```

### Building

```
$ make
```

### Toplevel

Requires `utop` (see prerequisites).

```
$ make top
```

### Tests

```
$ make test
```
