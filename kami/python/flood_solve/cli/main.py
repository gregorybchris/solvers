"""Flood Solve CLI."""
import argparse
import re
import time

from flood_solve.core.puzzle_factory import PuzzleFactory
from flood_solve.solve.solver import Solver


def solve(target_section, target_puzzle):
    puzzles = PuzzleFactory.get_all(PuzzleFactory.KAMI_TYPE, check_valid=True)
    for puzzle in puzzles:
        puzzle_id = puzzle.get_id()
        match = re.match(r"([0-9]*)-([0-9]*)", puzzle_id)
        puzzle_section, puzzle_number = int(match[1]), int(match[2])

        if target_section is not None and target_section != puzzle_section:
            continue
        if target_puzzle is not None and target_puzzle != puzzle_number:
            continue

        start_time = time.time()
        contractions = Solver.solve(puzzle)
        total_time = time.time() - start_time

        if contractions is None:
            print(f"No solution found for puzzle {puzzle.get_id()}")
        else:
            print(f"Solved puzzle {puzzle.get_id()} in {total_time}s")
            for region, color in contractions:
                print(f"\tRegion {region} -> {color.value}")


def run():
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--section_number', type=int)
    parser.add_argument('-p', '--puzzle_number', type=int)

    args = parser.parse_args()
    solve(args.section_number, args.puzzle_number)
