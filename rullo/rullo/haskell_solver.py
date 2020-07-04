import ctypes

from solver import Solver
from value_tile import ValueTile


class HaskellSolver(Solver):
    HASKELL_INTERFACE_PATH = 'hs/libffi-HsInterface.so'

    @classmethod
    def solve(cls, board, **kwargs):
        hslib = ctypes.cdll.LoadLibrary(cls.HASKELL_INTERFACE_PATH)
        hslib.haskell_init()

        n_rows = board.n_rows
        n_cols = board.n_cols

        # Simplify board data to be passed to Haskell
        flat_values = [x.get_value() for xs in board.get_value_tiles() for x in xs]
        row_goals = [x.get_value() for x in board.get_row_goals()]
        col_goals = [x.get_value() for x in board.get_col_goals()]

        # Convert board data to C-type arrays
        c_flat_values = cls._convert_ctype_array(flat_values, ctypes.c_int)
        c_row_goals = cls._convert_ctype_array(row_goals, ctypes.c_int)
        c_col_goals = cls._convert_ctype_array(col_goals, ctypes.c_int)

        hslib.solveHs.restype = ctypes.POINTER(ctypes.c_int * (n_rows * n_cols))
        flat_int_results = list(hslib.solveHs(
            c_flat_values, c_row_goals, c_col_goals, n_rows, n_cols).contents)

        # If all values are 0 the board could not be solved
        # This was an arbitrary design decision
        if all(v == 0 for v in flat_int_results):
            return None

        # Update board object with final statuses
        int_results = [flat_int_results[r * n_cols:(r + 1) * n_cols] for r in range(n_rows)]
        results = [[bool(value) for value in row] for row in int_results]
        value_tiles = board.get_value_tiles()
        for c in range(n_cols):
            for r in range(n_rows):
                if results[r][c]:
                    value_tiles[r][c].set_status(ValueTile.STATUS_YES)
                else:
                    value_tiles[r][c].set_status(ValueTile.STATUS_NO)

        hslib.haskell_end()
        return board

    @classmethod
    def _convert_ctype_array(cls, a, ctype):
        return (ctype * len(a))(*a)
