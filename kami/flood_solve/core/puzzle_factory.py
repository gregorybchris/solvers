"""Factory for Puzzles."""
import json
import importlib.resources as pkg_resources

from flood_solve import puzzles as package_puzzles
from flood_solve.core.color import Color
from flood_solve.core.puzzle import Puzzle


class PuzzleFactory:
    """Factory for Puzzles."""

    PUZZLE_ID_FIELD = 'id'
    PUZZLE_MOVES_FIELD = 'moves'
    PUZZLE_REGIONS_FIELD = 'regions'

    REGION_ID_FIELD = 'id'
    REGION_COLOR_FIELD = 'color'
    REGION_NEIGHBORS_FIELD = 'neighbors'

    PUZZLES_META_FILENAME_TEMPLATE = '{}-meta.json'
    PUZZLES_META_FILENAMES_FIELD = 'filenames'

    KAMI_TYPE = 'kami'
    USER_TYPE = 'user'

    @classmethod
    def get_kami(cls, section_number, puzzle_number, check_valid=False):
        """
        Get a puzzle based on the section and puzzle numbers.

        :param section_number: Section of puzzle.
        :param puzzle_number: Number of puzzle in section.
        :param check_valid: Check whether the puzzle is valid before returning.
        :return: New puzzle.
        """
        puzzle_filename = f'kami-{section_number}-{puzzle_number}.json'
        puzzle = cls._load_puzzle_by_filename(puzzle_filename)
        if check_valid:
            puzzle._check_valid()
        return puzzle

    @classmethod
    def get_all(cls, puzzle_type, check_valid=False):
        """
        Get a list of all puzzles available.

        :param check_valid: Check whether the puzzle is valid before returning.
        :return: List of all puzzles.
        """
        meta_filename = cls.PUZZLES_META_FILENAME_TEMPLATE.format(puzzle_type)
        puzzle_meta_dict = json.loads(pkg_resources.read_text(package_puzzles, meta_filename))
        filenames = puzzle_meta_dict[cls.PUZZLES_META_FILENAMES_FIELD]
        puzzles = []
        for filename in filenames:
            puzzle = cls._load_puzzle_by_filename(filename)
            if check_valid:
                puzzle._check_valid()
            puzzles.append(puzzle)
        return puzzles

    @classmethod
    def _load_puzzle_from_dict(cls, puzzle_dict):
        """Populate puzzle based on a dictionary."""
        puzzle = Puzzle(puzzle_dict[cls.PUZZLE_ID_FIELD],
                        puzzle_dict[cls.PUZZLE_MOVES_FIELD])
        for region in puzzle_dict[cls.PUZZLE_REGIONS_FIELD]:
            region_id = region[cls.REGION_ID_FIELD]
            region_color = Color.get(region[cls.REGION_COLOR_FIELD])
            puzzle.add_region(region_id, region_color)
            for neighbor_id in region[cls.REGION_NEIGHBORS_FIELD]:
                puzzle.assign_neighbor(region_id, neighbor_id)
        return puzzle

    @classmethod
    def _load_puzzle_by_filename(cls, puzzle_filename):
        puzzle_dict = json.loads(pkg_resources.read_text(package_puzzles, puzzle_filename))
        return cls._load_puzzle_from_dict(puzzle_dict)
