from board import Board
# from random import random

class Game:
    UP = 1
    DOWN = 2
    LEFT = 3
    RIGHT = 4
    DIRECTIONS = [UP, DOWN, LEFT, RIGHT]

    def __init__(self, board):
        self._moves = []
        self._board = board
        self._is_won = False

    def get_options(self):
        options = []
        width = self._board.width()
        height = self._board.height()
        for row in range(height):
            for col in range(width):
                val = self._board.get(row, col)
                if val != Board.BLANK and val != Board.EMPTY and val != Board.GOAL:
                    options.append((row, col))
        return options

    def has_won(self):
        return self._is_won

    def get_moves(self):
        return list(map(self._extract_move_location, self._moves))

    def _extract_move_location(self, move):
        row, col, direction, undo_list = move
        return (row, col, direction)

    def _translate_location(self, row, col, direct, offset):
        assert(direct == Game.UP or direct == Game.DOWN or \
            direct == Game.LEFT or direct == Game.RIGHT)
        if direct == Game.UP:
            return (row + -1 * offset, col)
        elif direct == Game.DOWN:
            return (row + 1 * offset, col)
        elif direct == Game.LEFT:
            return (row, col + -1 * offset)
        elif direct == Game.RIGHT:
            return (row, col + 1 * offset)

    def make_move(self, row, col, direction):
        piece = self._board.get(row, col)
        assert(piece != Board.EMPTY and piece != Board.BLANK and piece != Board.GOAL)
        self._board.set(row, col, Board.BLANK)
        undo_list = [(piece, (row, col))]
        offset = 1
        while offset <= piece:
            row_trans, col_trans = self._translate_location(row, col, direction, offset)
            if not self._board.in_bounds(row_trans, col_trans):
                break

            if self._board.get(row_trans, col_trans) == Board.EMPTY:
                self._board.set(row_trans, col_trans, Board.BLANK)
                undo_list.append((Board.EMPTY, (row_trans, col_trans)))
            elif self._board.get(row_trans, col_trans) == Board.BLANK:
                piece += 1
            elif self._board.get(row_trans, col_trans) == Board.GOAL:
                self._is_won = True
            else:
                piece += 1

            offset += 1
        self._moves.append((row, col, direction, undo_list))
        # if random() < 0.001:
            # print("Moves: ", self.get_moves())
        # print(self._board)

    def undo_move(self):
        last_move = self._moves[-1]
        del self._moves[-1]
        piece, row, col, undo_list = last_move
        for undo in undo_list:
            old_value, (row, col) = undo
            self._board.set(row, col, old_value)
