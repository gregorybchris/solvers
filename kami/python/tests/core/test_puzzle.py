"""Test Puzzle."""
import unittest

from flood_solve.core.puzzle_factory import PuzzleFactory
from flood_solve.core.color import Color


class PuzzleTests(unittest.TestCase):
    """Test Puzzle."""

    def test_get_package_puzzle(self):
        puzzle = PuzzleFactory.get_kami(1, 1)
        self.assertEqual(puzzle.get_id(), '1-1')
        self.assertEqual(puzzle.get_moves(), 1)

    def test_all_package_puzzles(self):
        package_puzzles = PuzzleFactory.get_all(PuzzleFactory.KAMI_TYPE)
        self.assertGreater(len(package_puzzles), 0)
        puzzle_id_set = set()
        for puzzle in package_puzzles:
            puzzle_id = puzzle.get_id()
            try:
                puzzle._check_valid()
            except Exception:
                raise ValueError(f"Puzzle {puzzle_id} not valid")

            self.assertNotIn(puzzle_id, puzzle_id_set)
            puzzle_id_set.add(puzzle_id)

    def test_contract(self):
        puzzle = PuzzleFactory.get_kami(3, 4)
        puzzle.contract(4, Color.BLACK)
        puzzle.contract(4, Color.ORANGE)
        puzzle.contract(4, Color.GREEN)
        self.assertEqual(puzzle.get_colors(), {Color.GREEN})
        self.assertEqual(puzzle.get_regions(), {4})
