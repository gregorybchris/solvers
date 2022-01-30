from typing import List

from wordle.solve.tile import Tile
from wordle.solve.color import Color
from wordle.solve.status import Status

COLOR_MAP = {
    Status.BLACK: Color.BLUE,
    Status.YELLOW: Color.ORANGE,
    Status.GREEN: Color.GREEN,
}


def print_attempt(attempt: List[Tile]) -> None:
    print(attempt_to_str(attempt))


def attempt_to_str(attempt: List[Tile]) -> str:
    return " ".join([tile_to_str(tile) for tile in attempt])


def tile_to_str(tile: Tile) -> str:
    color = COLOR_MAP[tile.status]
    return color + tile.letter + Color.RESET
