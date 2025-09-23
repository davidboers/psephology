[![build](https://github.com/davidboers/psephology/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/davidboers/psephology/actions?query=workflow%3AHaskell-CI)

<!-- [![Hackage](https://img.shields.io/hackage/v/psephology.svg)](https://hackage.haskell.org/package/psephology) -->

[Psephology](https://en.wikipedia.org/wiki/Psephology) • [Social choice theory](https://en.wikipedia.org/wiki/Social_choice_theory) • [Spoiler effect](https://en.wikipedia.org/wiki/Spoiler_effect) • [Comparison of electoral systems](https://en.wikipedia.org/wiki/Comparison_of_voting_rules) • [Arrow's impossibility theorem](https://en.wikipedia.org/wiki/Arrow%27s_impossibility_theorem) • [Strategic voting](https://en.wikipedia.org/wiki/Strategic_voting) • [Gibbard's theorem](https://en.wikipedia.org/wiki/Gibbard%27s_theorem) • [McKelvey-Schofield chaos theorem](https://en.wikipedia.org/wiki/McKelvey%E2%80%93Schofield_chaos_theorem) • [Redistricting](https://en.wikipedia.org/wiki/Redistricting) • [Proportional representation](https://en.wikipedia.org/wiki/Proportional_representation) • [Fraud detection](https://en.wikipedia.org/wiki/Electoral_fraud)

<img src="images/logo.svg" alt="logo" width="100%">

A Haskell language library dedicated to the study of psephology.

- Compiled using the Glasgow Haskell Compiler, version 9.12.2
- Cabal version 3.12.1.0 (later Cabal versions are finicky with HLS)
- Docker version 28.4.0

# Feature list

- Algorithms to determine the victor under various single-winner election systems, including:
    - Borda count
    - Condorcet voting
    - First-past-the-post
    - Instant-Runoff Voting
    - Rated voting (range, STAR, graduated majority)
    - Sortition
    - Two-round system
    - and tools to evaluate the susceptibility of these systems to pathologies.
- Seat projections
    - Multilevel regression with poststratification (MRP) polling
    - Swingometer
- Algorithms to determine seat allocation under various party-list proportional representation systems, including both the highest averages method and the largest remainder method.
    - Tools to evaluate the proportionality of these systems.
    - Tools to evaluate the susceptibility of these systems to pathologies.
- A new algorithm to reapportion voting districts.
- Tools to evaluate and study the spoiler effect.
- Tools to devise and study voting strategies (strategic voting).
- Interface to handle .blt files.
- Tools to evaluate voting system efficiency.
- Tools to identify voter fraud through a statistical analysis of published election results.
- Tools to study the McKelvey-Schofield chaos theorem.
- Tools to generate synthetic voters under the single-peaked preferences model.
- and many others!

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

Building the Docker container using the command above will produce a directory called `docs/`.
You can then pop into `docs/index.html`.

# TODO list

Planned features (not necessarily planned for first release):

- Proxy cycles
- Convert a list of formal voters into a list of theoretical voters (use k-means)
- System-wise efficiency
- Finish weak orderings
- Polsby-Popper
- Test suite
    - Property lists

Requests and suggestions are welcome!