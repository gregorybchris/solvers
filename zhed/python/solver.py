from game import Game

class Solver:
    @staticmethod
    def solve(board):
        game = Game(board)
        solution = Solver._solve_game(game)
        return solution

    @staticmethod
    def _solve_game(game):
        options = game.get_options()
        for option in options:
            row, col = option
            for direction in Game.DIRECTIONS:
                game.make_move(row, col, direction)
                if game.has_won():
                    return game.get_moves()
                else:
                    solution = Solver._solve_game(game)
                    if solution is None:
                        game.undo_move()
                    else:
                        return solution
        return None
