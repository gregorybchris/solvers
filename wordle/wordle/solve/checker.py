from typing import List

from wordle.solve.tile import Tile
from wordle.solve.status import Status


def get_valid_words(attempts_tiles: List[Tile], words: List[str]) -> List[str]:
    return [word for word in words if is_valid(word, attempts_tiles)]


def is_valid(attempt_letters: str, attempts_tiles: List[List[Tile]]) -> bool:
    # if len(attempt_letters) != 5:
    #     return False

    for attempt_tiles in attempts_tiles:
        for attempt_tile_number, attempt_tile in enumerate(attempt_tiles):
            if attempt_tile.status == Status.BLACK:
                if attempt_tile.letter in attempt_letters:
                    return False
            elif attempt_tile.status == Status.YELLOW:
                if attempt_tile.letter not in attempt_letters:
                    return False
                if attempt_letters[attempt_tile_number] == attempt_tile.letter:
                    return False
            elif attempt_tile.status == Status.GREEN:
                if attempt_letters[attempt_tile_number] != attempt_tile.letter:
                    return False
    return True


def evaluate_attempt(attempt_letters: str, target_letters: str) -> List[Tile]:
    # assert len(attempt_letters) == len(target_letters) == 5

    attempt_tiles: List[Tile] = []
    for attempt_letter_number, attempt_letter in enumerate(attempt_letters):
        if target_letters[attempt_letter_number] == attempt_letter:
            attempt_status = Status.GREEN
        elif attempt_letter in target_letters:
            attempt_status = Status.YELLOW
        else:
            attempt_status = Status.BLACK

        attempt_tile = Tile(attempt_letter, attempt_status)
        attempt_tiles.append(attempt_tile)

    return attempt_tiles
