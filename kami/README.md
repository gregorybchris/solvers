# Flood Solver

### Problem

There are many games like [Flood-It](https://itunes.apple.com/us/app/flood-it/id476943146?mt=8), [KAMI 2](https://itunes.apple.com/us/app/kami-2/id1133161444?mt=8), and [Globs](http://www.deadwhale.com/play.php?game=131) that require the user to change the colors of regions until there is only one region left. These are often called flood fill games. [Clifford et al](https://arxiv.org/abs/1001.4420) found that determining the best possible score on these puzzles is NP-hard.

### Motivation

I chose to code up a solver for this problem because I am interested in the idea that there are games that humans can solve easily, but computers have trouble solving. I think this qualifies as such a game given that as the numbers of allowed moves and colors increases, the runtimes of brute-force solutions increase exponentially and yet humans have a intuitive understanding of these puzzles that is difficult to put into computer instructions or even words.

## Installation

Install the current PyPI release:

```bash
pip install flood-solve
```

Or install from source:

```bash
pip install git+https://github.com/gregorybchris/flood-solver
```

## Usage

```bash
# Run the flood solver on a predefined puzzle (section 3, puzzle 6)
flood -s 3 -p 6
```
