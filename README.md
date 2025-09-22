[![build](https://github.com/davidboers/psephology/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/davidboers/psephology/actions?query=workflow%3AHaskell-CI)

<!-- [![Hackage](https://img.shields.io/hackage/v/psephology.svg)](https://hackage.haskell.org/package/psephology) -->

[Psephology](https://en.wikipedia.org/wiki/Psephology) • [Social choice theory](https://en.wikipedia.org/wiki/Social_choice_theory) • [Spoiler effect](https://en.wikipedia.org/wiki/Spoiler_effect) • [Comparison of electoral systems](https://en.wikipedia.org/wiki/Comparison_of_voting_rules) • [Arrow's impossibility theorem](https://en.wikipedia.org/wiki/Arrow%27s_impossibility_theorem) • [Strategic voting](https://en.wikipedia.org/wiki/Strategic_voting) • [Gibbard's theorem](https://en.wikipedia.org/wiki/Gibbard%27s_theorem) • [McKelvey-Schofield chaos theorem](https://en.wikipedia.org/wiki/McKelvey%E2%80%93Schofield_chaos_theorem) • [Redistricting](https://en.wikipedia.org/wiki/Redistricting) • [Proportional representation](https://en.wikipedia.org/wiki/Proportional_representation)

<img src="images/logo.svg" alt="logo" width="100%">

A Haskell language library dedicated to the study of psephology.

- Compiled using the Glasgow Haskell Compiler, version 9.12.2
- Cabal version 3.12.1.0 (later Cabal versions are finicky with HLS)
- Docker version 28.4.0

# Docker

Use Docker to run test suite.

```shell
$ docker build -t psephology .
$ docker run --rm -t psephology # run tests
```

Using a custom GHC version:

```shell
$ docker build -t psephology --build-arg GHC_VERSION=${GHC_VERSION} .
```

Or you can run one of the examples:

```shell
$ docker run --rm -t psephology cabal run example-${EX}
```

# Docs

Access docs:

```shell
$ ./docs.sh
$ docs/index.html
```

# TODO list

Planned features (not necessarily planned for first release):

- Proxy cycles
- Convert a list of formal voters into a list of theoretical voters (use k-means)
- System-wise efficiency
- Finish weak orderings
- Polsby-Popper
- Test suite
    - Property lists
