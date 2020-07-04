import json

from goal_tile import GoalTile
from value_tile import ValueTile


class Board:
    KEY_ROW_GOALS = 'row_goals'
    KEY_COL_GOALS = 'col_goals'
    KEY_VALUES = 'values'

    def __init__(self, values, row_goals, col_goals):
        self._validate_inputs(values, row_goals, col_goals)

        self._value_tiles = [[ValueTile(v, ValueTile.STATUS_MAYBE) for v in r] for r in values]
        self._row_goal_tiles = [GoalTile(v, GoalTile.STATUS_UNSAT) for v in row_goals]
        self._col_goal_tiles = [GoalTile(v, GoalTile.STATUS_UNSAT) for v in col_goals]

    def _validate_inputs(self, values, row_goals, col_goals):
        if len(row_goals) < 2:
            raise ValueError("Size of row_goals must be at least 2")
        if len(col_goals) < 2:
            raise ValueError("Size of col_goals must be at least 2")
        if len(values) != len(row_goals):
            raise ValueError("Size of row_goals did not match values")
        if len(values[0]) != len(col_goals):
            raise ValueError("Size of col_goals did not match values")
        for i in range(1, len(values)):
            if len(values[i]) != len(values[i - 1]):
                raise ValueError("Inconsistent sizes of board value rows")

    def get_value_tiles(self):
        return self._value_tiles

    def get_row_goal(self, row):
        return self._row_goal_tiles[row]

    def get_row_goals(self):
        return self._row_goal_tiles

    def get_col_goal(self, col):
        return self._col_goal_tiles[col]

    def get_col_goals(self):
        return self._col_goal_tiles

    def get_row(self, row):
        return self._value_tiles[row]

    def get_col(self, col):
        return [row[col] for row in self._value_tiles]

    @property
    def n_cols(self):
        return len(self._col_goal_tiles)

    @property
    def n_rows(self):
        return len(self._row_goal_tiles)

    @classmethod
    def from_file(cls, filename):
        with open(filename) as f:
            board_json = json.load(f)
            values = board_json[cls.KEY_VALUES]
            row_goals = board_json[cls.KEY_ROW_GOALS]
            col_goals = board_json[cls.KEY_COL_GOALS]
            return Board(values, row_goals, col_goals)
