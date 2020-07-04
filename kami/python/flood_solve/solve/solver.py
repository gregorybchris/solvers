"""Flood fill solver."""
import copy

from flood_solve.core.color import Color


class Solver:
    """Flood fill solver."""

    @classmethod
    def solve(cls, puzzle):
        """Solve the flood puzzle."""
        return cls._solve(puzzle)

    @classmethod
    def _solve(cls, puzzle, moves=[]):
        if len(puzzle.get_colors()) == 1:
            return moves

        if len(puzzle.get_colors()) > puzzle.get_moves() + 1:
            return None
        if puzzle.get_n_non_singular_colors() > puzzle.get_moves():
            return None

        last_move_region = None if len(moves) == 0 else moves[-1][0]
        region_ids = puzzle.get_regions_centrality_ranked(
            last_region_constraint=last_move_region)

        # Try the same region again first
        # if len(moves) != 0:
        #     last_region_id, _ = moves[-1]
        #     region_ids.insert(0, last_region_id)
        #     region_ids.remove(last_region_id)

        for region_id in region_ids:
            for color in puzzle.get_colors_neighbor_ranked(region_id):
                p = copy.deepcopy(puzzle)
                m = copy.deepcopy(moves)

                p.contract(region_id, color)
                m.append((region_id, color))
                solution = cls._solve(p, moves=m)
                if solution is not None:
                    return solution
        return None
