from solver import Solver
from board import Board

'''
    Level 6 (4 Number Tiles)
    . . . . . . . .
    . . @ . . . 3 .
    . . . . . . . .
    . . . . . . . .
    . . 2 . . . . .
    . . . . . 3 . .
    . . . 2 . . . .
    . . . . . . . .
    Solution: [((6,3),U),((4,2),R),((5,5),U),((1,6),L)]
'''
# board = Board(8, 8)
# board.set(1, 6, 3)
# board.set(4, 2, 2)
# board.set(6, 3, 2)
# board.set(5, 5, 3)
# board.set(1, 2, Board.GOAL)


'''
    Level 7 (5 Number Tiles)
    . . . . . . . .
    . . . . . . . .
    . . . 1 . . . .
    . @ . . . . 1 .
    . . . . . 1 . .
    . . 2 . 2 . . .
    . . . . . . . .
    . . . . . . . .
    Solution: [((2,3),D),((4,5),U),((5,2),U),((5,4),U),((3,6),L)]
'''
# board = Board(8, 8)
# board.set(2, 3, 1)
# board.set(5, 2, 2)
# board.set(5, 4, 2)
# board.set(4, 5, 1)
# board.set(3, 6, 1)
# board.set(3, 1, Board.GOAL)


'''
    Level 12 (6 Number Tiles)
    . . . . . . . . .
    . . . . . . . . .
    . . 5 . . 4 . . .
    . 2 . . . . . . .
    . 1 . . . . . . .
    . . . 1 . . . . .
    . . . . . . . . .
    . 3 . . . . @ . .
    . . . . . . . . .
    Solution: [((2,2),D),((4,1),R),((5,3),U),((3,1),R),((2,5),D),((7,1),R)]
'''
# board = Board(9, 9)
# board.set(2, 2, 5)
# board.set(2, 5, 4)
# board.set(3, 1, 2)
# board.set(4, 1, 1)
# board.set(5, 3, 1)
# board.set(7, 1, 3)
# board.set(7, 6, Board.GOAL)

'''
    Level 40 (7 Number Tiles)
    . . . . . . . . . .
    . . . . . . . . . .
    . . @ . . . . 2 . .
    . . . . . . . . . .
    . . . . . . . 1 . .
    . . . . . . . 3 . .
    . . . 2 3 . . . . .
    . . . . . 2 3 . . .
    . . . . . . . . . .
    . . . . . . . . . .
    Solution: [((5,7),L),((6,3),R),((6,4),U),((7,6),U),((4,7),L),((7,5),U),((2,7),L)]
'''
board = Board(10, 10)
board.set(2, 7, 2)
board.set(4, 7, 1)
board.set(5, 7, 3)
board.set(6, 3, 2)
board.set(6, 4, 3)
board.set(7, 5, 2)
board.set(7, 6, 3)
board.set(2, 2, Board.GOAL)


print(board)

solution = Solver.solve(board)
print(solution)
