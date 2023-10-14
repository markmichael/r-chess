check_for_checks <- function(game, color) {
  ### get position of king
  king_position <- get_king_position(game, color)
if (is_position_attacked(game@board, king_position)) {
    game@check=TRUE
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

is_position_attacked <- function(board, test_square) {
    lapply(board, function(column) {
        any(lapply(column, function(row) {
            return(any(row@available_moves == test_square))
        }))
    })
}
