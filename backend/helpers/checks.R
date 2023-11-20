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
  board <- game@board
  result <- any(
    lapply(board, function(column) {
      any(lapply(column, function(row) {
        if (row@color == game@turn) {
          return(FALSE)
        } else {
          if (any(list(test_square) %in% row@available_moves)) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      }))
    })
  )
  print("is position attacked")
  print(test_square)
  print(result)
  return(result)
}
