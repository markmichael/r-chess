check_for_checks <- function(game, color) {
  ### get position of king
  king_position <- get_king_position(game, color)
  if (is_position_attacked(game, king_position)) {
    game@check <- TRUE
  }
  ### check available moves for kings position
}

get_king_position <- function(game, color) {
  lapply(game@board, function(column) {
    lapply(column, function(row) {
      if (game@board[[column]][[row]]@color == color &&
        game@board[[column]][[row]]@piece_type == "king") {
        return(list(col = column, row = row))
      }
    })
  })
}

is_position_attacked <- function(game, test_square) {
### change turn
  # game@turn <- ifelse(game@turn == "white", "black", "white")
  # game <- check_all_available_moves(game)
  board <- game@board
  any(
    lapply(board, function(column) {
      any(lapply(column, function(row) {
        any(
          lapply(row@available_moves, function(x) {
            if (x$col == test_square$col && x$row == test_square$row) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          })
        )
      }))
    })
  )
}
