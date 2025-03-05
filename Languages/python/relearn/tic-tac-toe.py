# tic-tac-toe with basic string, recursion, input() & print()

# Each position of the board has 3 states
# X -> player 1
# O -> player 2
# * -> None

# The board looks like below. Numbers are position coordinates.
#   012
# 0 O**
# 1 **X
# 2 ***
#
# The corresponding positions of above O & X  are
# O -> position (0,0)
# X -> position (1,2)

# The board will be coded as
# board = "o****x***"
# index is 012345678
# board[0:3] -> "o**"
# board[3:6] -> "**x"
# board[6:9] -> "***"

init_board = "*********"
player_1 = "X"
player_2 = "O"


def print_board(board):
    print("  012")
    print("0", board[0:3])
    print("1", board[3:6])
    print("2", board[6:9])


# change board position (x,y) to "who" and return a new board
def player_put_x_y(who, x, y, board):
    pos = x + y * 3  # Convert (x,y) to board's index
    return board[:pos] + who + board[pos + 1 :]


# Does anyone win? return the one who won
def check_win(board):
    # check the rows are the same, and they is not None
    if board[0] == board[1] == board[2] != "*":
        return board[0]
    if board[3] == board[4] == board[5] != "*":
        return board[3]
    if board[6] == board[7] == board[8] != "*":
        return board[6]
    # check the columns are the same, and they is not None
    if board[0] == board[3] == board[6] != "*":
        return board[0]
    if board[1] == board[4] == board[7] != "*":
        return board[1]
    if board[2] == board[5] == board[8] != "*":
        return board[2]
    # check diagonals are the same, and they are not None
    if board[0] == board[4] == board[8] != "*":
        return board[0]
    if board[6] == board[4] == board[2] != "*":
        return board[6]

    # No one win
    return None


# if player 1, then return player2
def flip_who(who):
    if who == player_1:
        return player_2
    else:
        return player_1


def loop(who, board, round_n):
    print("-----------------")
    print("Round: " + str(round_n))
    print_board(board)
    print(
        "Player " + who + " please input a position like 12 which means position (1,2)"
    )

    s = input()
    updated_board = player_put_x_y(who, int(s[0]), int(s[1]), board)

    player_who_won = check_win(updated_board)

    # if nobody won, go to next round
    if player_who_won is None:
        return loop(flip_who(who), updated_board, round_n + 1)
    else:
        print_board(updated_board)
        return player_who_won


print("Player ", loop(player_1, init_board, 0), "Won !")
