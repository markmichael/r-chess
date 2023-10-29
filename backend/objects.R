game <- setClass("game",
    contains = "list",
    slots = c(
        id = "character",
        board = "list", # current board
        turn = "character", # white or black
        moves = "list", # list of all previous moves
        check = "logical", # whether the current player is in check
        checkmate = "logical" # whether the current player is in checkmate
    )
)

piece <- setClass("piece",
    contains = "list",
    slots = c(
        color = "character",
        row = "integer",
        col = "integer",
        available_moves = "list",
        piece_type = "character",
        piece_symbol = "character",
        moved = "logical"
    )
)
