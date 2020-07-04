import argparse

from board import Board
from board_printer import BoardPrinter
from printer_config import PrinterConfig
from python_solver import PythonSolver
from haskell_solver import HaskellSolver


SOLVER_HASKELL = 'haskell'
SOLVER_PYTHON = 'python'

ALL_SOLVERS = {SOLVER_HASKELL, SOLVER_PYTHON}


def run_solver(
    board_filename,
    solver_type,
    printer_config_filename,
    print_intermediate
):
    board = Board.from_file(board_filename)
    printer_config = None
    if printer_config_filename is not None:
        printer_config = PrinterConfig.from_file(printer_config_filename)
    printer = BoardPrinter(config=printer_config)
    solver = get_solver(solver_type)
    solution = solver.solve(board,
                            board_printer=printer,
                            print_intermediate=print_intermediate)

    if solution is not None:
        print("Solved")
        printer.print(board)
    else:
        print("No unique solution")


def get_solver(solver_type):
    return {
        SOLVER_PYTHON: PythonSolver,
        SOLVER_HASKELL: HaskellSolver,
    }[solver_type]


def parse_args():
    parser = argparse.ArgumentParser(description="Solve Rullo board")
    parser.add_argument('board', type=str, help="Board filename")
    parser.add_argument('--printer_config', type=str, help="Path to a printer configuration file.")
    parser.add_argument('--inter', default=False, action='store_true',
                        dest='print_intermediate', help="Whether to print intermediate board states.")
    parser.add_argument('--solver', default=SOLVER_PYTHON, choices=ALL_SOLVERS,
                        help="Solvers with different implementations.")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    run_solver(args.board, args.solver,
               args.printer_config, args.print_intermediate)
