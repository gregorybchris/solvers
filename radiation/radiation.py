import json
import time

from colorama import init, Fore, Style


init(autoreset=True)


SIZE = 14
UNKNOWN = '_'
SYMBOL_X = 'X'
SYMBOL_O = 'O'


def is_solved(board):
    for row in board:
        for symbol in row:
            if symbol == UNKNOWN:
                return False
    return True


def inbounds(x, y):
    return x >= 0 and x < SIZE and y >= 0 and y < SIZE


def opposite(symbol):
    if symbol not in [SYMBOL_X, SYMBOL_O]:
        raise ValueError
    return SYMBOL_X if symbol == SYMBOL_O else SYMBOL_O


def print_board(board, highlight=None):
    for r in range(SIZE):
        for c in range(SIZE):
            symbol = board[r][c]
            if highlight is None:
                print(f"{symbol} ", end='')
            elif (r, c) in highlight:
                print(f"{Fore.GREEN}{Style.BRIGHT}{symbol} ", end='')
            else:
                print(f"{Style.DIM}{symbol} ", end='')
        print()
    print()


def is_valid_complete(line):
    n_x = sum([1 if symbol == SYMBOL_X else 0 for symbol in line])
    n_o = sum([1 if symbol == SYMBOL_O else 0 for symbol in line])

    if n_x > SIZE / 2 or n_o > SIZE / 2:
        return False, False

    for i in range(SIZE - 2):
        if line[i] != UNKNOWN:
            if line[i] == line[i + 1] and line[i] == line[i + 2]:
                return False, False

    if n_x != SIZE / 2 or n_o != SIZE / 2:
        return True, False

    return True, True


def find_all_valid_lines(line, all_valid=None):
    if all_valid is None:
        all_valid = []

    for index, line_symbol in enumerate(line):
        if line_symbol == UNKNOWN:
            for symbol in [SYMBOL_X, SYMBOL_O]:
                new_line = line.copy()
                new_line[index] = symbol
                valid, complete = is_valid_complete(new_line)
                if valid:
                    if complete:
                        all_valid.append(new_line)
                    find_all_valid_lines(new_line, all_valid=all_valid)
    return all_valid


def line_induction(line, max_induction_size):
    blank_indices = [i for i, symbol in enumerate(line) if symbol == UNKNOWN]
    if len(blank_indices) > max_induction_size:
        return []
    all_valid = find_all_valid_lines(line, all_valid=[])
    new_moves = []
    for blank_index in blank_indices:
        for symbol in [SYMBOL_X, SYMBOL_O]:
            if all([valid_line[blank_index] == symbol for valid_line in all_valid]):
                new_moves.append((blank_index, symbol))
                continue
    return new_moves


def update(board, new_moves, reason, print_iters=False):
    for (r, c), symbol in new_moves:
        board[r][c] = symbol
        if print_iters:
            print(f"{Fore.GREEN}{Style.BRIGHT}{(r, c)} -> {symbol} ({reason})")
    if print_iters:
        print_board(board, highlight=[location for location, _ in new_moves])


def solve(board, max_induction_size, iteration=0, print_iters=False):
    if is_solved(board):
        return True, board

    if print_iters:
        print(f"Iteration: {iteration}")

    # Check for adjacent same symbol (e.g. _ O O _ => X O O X)
    for i in range(SIZE):
        for j in range(SIZE - 1):
            # Horizontal adjacent
            if board[i][j] == board[i][j + 1] and board[i][j] != UNKNOWN:
                for k in [j - 1, j + 2]:
                    if inbounds(i, k) and board[i][k] == UNKNOWN:
                        new_moves = [((i, k), opposite(board[i][j]))]
                        update(board, new_moves, "Horizontal Adjacent", print_iters=print_iters)
                        return solve(board, max_induction_size, iteration + 1, print_iters=print_iters)

            # Vertical adjacent
            if board[j][i] == board[j + 1][i] and board[j][i] != UNKNOWN:
                for k in [j - 1, j + 2]:
                    if inbounds(k, i) and board[k][i] == UNKNOWN:
                        new_moves = [((k, i), opposite(board[j][i]))]
                        update(board, new_moves, "Vertical Adjacent", print_iters=print_iters)
                        return solve(board, max_induction_size, iteration + 1, print_iters=print_iters)

    # Check for sandwich (e.g. X _ X => X O X)
    for i in range(SIZE):
        for j in range(SIZE - 2):
            # Horizontal sandwich
            if board[i][j] == board[i][j + 2] and board[i][j] != UNKNOWN:
                if board[i][j + 1] == UNKNOWN:
                    new_moves = [((i, j + 1), opposite(board[i][j]))]
                    update(board, new_moves, "Horizontal Sandwich", print_iters=print_iters)
                    return solve(board, max_induction_size, iteration + 1, print_iters=print_iters)

            # Vertical sandwich
            if board[j][i] == board[j + 2][i] and board[j][i] != UNKNOWN:
                if board[j + 1][i] == UNKNOWN:
                    new_moves = [((j + 1, i), opposite(board[j][i]))]
                    update(board, new_moves, "Vertical Sandwich", print_iters=print_iters)
                    return solve(board, max_induction_size, iteration + 1, print_iters=print_iters)

    # Perform expensive line induction (e.g. X O O X O O X X O X X O _ _ => )
    for i in range(SIZE):
        # Check rows
        new_moves = line_induction([board[i][j] for j in range(SIZE)], max_induction_size)
        new_moves = [((i, j), symbol) for j, symbol in new_moves]
        if len(new_moves) > 0:
            update(board, new_moves, "Row Induction", print_iters=print_iters)
            return solve(board, max_induction_size, iteration + 1, print_iters=print_iters)

        # Check columns
        new_moves = line_induction([board[j][i] for j in range(SIZE)], max_induction_size)
        new_moves = [((j, i), symbol) for j, symbol in new_moves]
        if len(new_moves) > 0:
            update(board, new_moves, "Column Induction", print_iters=print_iters)
            return solve(board, max_induction_size, iteration + 1, print_iters=print_iters)

    return False, board


def read_board(filename):
    with open(filename, 'r') as f:
        board_json = json.load(f)
    return board_json['indicators']


if __name__ == '__main__':
    board_file = 'puzzle.json'
    max_induction_size = 5
    print_iterations = True

    board = read_board(board_file)
    print("Initial board:")
    print_board(board)
    time_start = time.time()
    status, final_board = solve(board, max_induction_size, print_iters=print_iterations)
    print(f"Solved board: {round(time.time() - time_start, 2)}s")
    print_board(final_board)
