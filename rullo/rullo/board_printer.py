from goal_tile import GoalTile
from printer_config import PrinterConfig
from value_tile import ValueTile


class BoardPrinter:
    COLOR_BLUE = '\x1b[34m'
    COLOR_CYAN = '\x1b[36m'
    COLOR_GREEN = '\x1b[32m'
    COLOR_ORANGE = '\x1b[33m'
    COLOR_RED = '\x1b[31m'
    COLOR_RESET = '\x1b[39m'

    COLOR_MAP = {
        PrinterConfig.KEY_COLOR_BLUE: COLOR_BLUE,
        PrinterConfig.KEY_COLOR_CYAN: COLOR_CYAN,
        PrinterConfig.KEY_COLOR_GREEN: COLOR_GREEN,
        PrinterConfig.KEY_COLOR_ORANGE: COLOR_ORANGE,
        PrinterConfig.KEY_COLOR_RED: COLOR_RED,
        PrinterConfig.KEY_COLOR_RESET: COLOR_RESET,
    }

    STATUS_MAP = {
        PrinterConfig.KEY_STATUS_MAYBE: ValueTile.STATUS_MAYBE,
        PrinterConfig.KEY_STATUS_YES: ValueTile.STATUS_YES,
        PrinterConfig.KEY_STATUS_NO: ValueTile.STATUS_NO,
        PrinterConfig.KEY_STATUS_SAT: GoalTile.STATUS_SAT,
        PrinterConfig.KEY_STATUS_UNSAT: GoalTile.STATUS_UNSAT,
    }

    DEAFULT_COLOR_SETTINGS = {
        ValueTile.STATUS_MAYBE: COLOR_BLUE,
        ValueTile.STATUS_YES: COLOR_GREEN,
        ValueTile.STATUS_NO: COLOR_RED,
        GoalTile.STATUS_SAT: COLOR_CYAN,
        GoalTile.STATUS_UNSAT: COLOR_ORANGE,
    }

    def __init__(self, config=None):
        self._color_settings = BoardPrinter._get_color_settings(config)

    @classmethod
    def _get_color_settings(cls, config):
        settings = dict()
        for k, v in BoardPrinter.DEAFULT_COLOR_SETTINGS.items():
            settings[k] = v
        if config is not None and config.colors is not None:
            for k, v in config.colors.items():
                settings[cls.STATUS_MAP[k]] = cls.COLOR_MAP[v]
        return settings

    def _tile_str(self, tile, padding=True):
        color = self._color_settings[tile.get_status()]

        tile_value = tile.get_value()
        val_str = str(tile_value)
        if padding:
            val_str += (' ' * (3 - len(str(tile_value))))

        return color + val_str + BoardPrinter.COLOR_RESET

    def _board_str(self, board):
        to_return = '      '
        for c in range(board.n_cols):
            col_goal = board.get_col_goal(c)
            to_return += self._tile_str(col_goal) + ' '
        to_return += '\n'
        for r in range(board.n_rows):
            row_goal = board.get_row_goal(r)
            row_tiles = board.get_row(r)
            to_return += self._tile_str(row_goal) + ' '
            to_return += '[ '
            for tile in row_tiles:
                to_return += self._tile_str(tile) + ' '
            to_return += ']\n'
        return to_return

    def print(self, board):
        print(self._board_str(board))
