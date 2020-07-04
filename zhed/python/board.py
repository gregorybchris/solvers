class Board:
    EMPTY = -1
    BLANK = -2
    GOAL = -3

    def __init__(self, width, height):
        self._width = width
        self._height = height
        self._pieces = [[Board.EMPTY for _ in range(width)] for _ in range(height)]

    def get(self, row, col):
        assert(self.in_bounds(row, col))
        return self._pieces[row][col]

    def set(self, row, col, piece):
        assert(self.in_bounds(row, col))
        self._pieces[row][col] = piece

    def width(self):
        return self._width

    def height(self):
        return self._height

    def in_bounds(self, row, col):
        return row >= 0 and row < self._height and col >= 0 and col < self._width

    def __repr__(self):
        to_return = ""
        for row in self._pieces:
            for piece in row:
                to_return += self._piece_to_string(piece) + " "
            to_return += "\n"
        return to_return

    def _piece_to_string(self, piece):
        if piece == Board.GOAL:
            return "@"
        elif piece == Board.EMPTY:
            return "."
        elif piece == Board.BLANK:
            return "="
        else:
            return str(piece)
