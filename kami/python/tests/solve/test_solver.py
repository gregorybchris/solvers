"""Test Solver."""
import unittest

from flood_solve.core.puzzle_factory import PuzzleFactory
from flood_solve.solve.solver import Solver


class SolverTests(unittest.TestCase):
    """Test Solver."""

    def test_solve(self):
        puzzles = PuzzleFactory.get_all(PuzzleFactory.KAMI_TYPE)
        for puzzle in puzzles:
            Solver.solve(puzzle)
