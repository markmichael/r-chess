
piece <- setClass("piece",
  contains = "list",
  slots = c(
    color = "character",
    row = "integer",
    col = "character",
    available_moves = "list",
    piece_type = "character",
    piece_symbol = "character",
    checked = "logical",
    moved = "logical"
  )
)
