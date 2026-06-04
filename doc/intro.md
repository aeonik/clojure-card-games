# Introduction to clojure-card-games

This project currently focuses on Karbosh game mechanics.

The engine is modeled as pure state transitions over event maps. Terminal I/O
is kept at the edge in `clojure-card-games.io`, so tests can exercise rules and
game state without user input.
