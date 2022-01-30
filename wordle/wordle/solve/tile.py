from wordle.solve.status import Status


class Tile:
    def __init__(self, letter: str, status: Status):
        self.letter = letter
        self.status = status
