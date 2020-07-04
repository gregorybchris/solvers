from copy import deepcopy
from queue import Queue

from solver import Solver
from value_tile import ValueTile


class PythonSolver(Solver):
    @classmethod
    def solve(cls, board, board_printer=None, print_intermediate=False):
        if print_intermediate and board_printer is None:
            raise ValueError("board_printer must be provided if print_intermediate is set")

        q_a = Queue()
        q_b = Queue()
        size = board.n_rows * board.n_cols
        for r in range(board.n_rows):
            row = board.get_row(r)
            goal = board.get_row_goal(r)
            q_a.put((row, goal))
        for c in range(board.n_cols):
            col = board.get_col(c)
            goal = board.get_col_goal(c)
            q_a.put((col, goal))

        while True:
            line_count = 0
            while not q_a.empty():
                line, goal = q_a.get()
                new_line = cls._fold_possible_lines(line, goal)
                if not cls._lines_equal(line, new_line):
                    if print_intermediate:
                        board_printer.print(board)
                cls._set_line_statuses(line, new_line, goal)
                if not cls._test_line(line, goal):
                    q_b.put((line, goal))
                    line_count += 1
            if line_count == size:
                return None
            if line_count == 0:
                return board
            size = line_count
            q_t = q_a
            q_a = q_b
            q_b = q_t

    @classmethod
    def _lines_equal(cls, line_a, line_b):
        for tile_a, tile_b in zip(line_a, line_b):
            if tile_a.get_status() != tile_b.get_status():
                return False
            if tile_a.get_value() != tile_b.get_value():
                return False
        return True

    @classmethod
    def _set_line_statuses(cls, target, source, goal):
        for source_tile, target_tile in zip(source, target):
            target_tile.set_status(source_tile.get_status())
        # TODO: Set the goal status for perpendicular lines that get solved
        # if all_satisfied:
        #     goal.set_status(GoalTile.STATUS_SAT)

    @classmethod
    def _fold_possible_lines(cls, line, goal):
        possible_lines = cls._get_possible_lines(line, goal)

        def transpose(l):
            return list(map(list, zip(*l)))

        def fold_statuses(statuses):
            if all(status == ValueTile.STATUS_YES for status in statuses):
                return ValueTile.STATUS_YES
            if all(status == ValueTile.STATUS_NO for status in statuses):
                return ValueTile.STATUS_NO
            else:
                return ValueTile.STATUS_MAYBE

        flipped_possibles = transpose(possible_lines)
        new_line = deepcopy(line)
        for i, (tile, possibles) in enumerate(zip(new_line, flipped_possibles)):
            tile.set_status(fold_statuses([pos.get_status() for pos in possibles]))
        return new_line

    @classmethod
    def _get_possible_lines(cls, line, goal):
        possible = []
        q = Queue()
        q.put(line)
        while not q.empty():
            potential_line = q.get()
            maybe_found = False
            for i, tile in enumerate(potential_line):
                if tile.get_status() == ValueTile.STATUS_MAYBE:
                    line_copy_yes = deepcopy(potential_line)
                    line_copy_no = deepcopy(potential_line)
                    line_copy_yes[i].set_status(ValueTile.STATUS_YES)
                    line_copy_no[i].set_status(ValueTile.STATUS_NO)
                    q.put(line_copy_yes)
                    q.put(line_copy_no)
                    maybe_found = True
                    break
            if not maybe_found:
                if cls._test_line(potential_line, goal):
                    possible.append(deepcopy(potential_line))
        return possible

    @classmethod
    def _test_line(cls, line, goal):
        goal_value = goal.get_value()
        line_sum = sum(tile.get_value() for tile in line if tile.get_status() == ValueTile.STATUS_YES)
        return line_sum == goal_value
