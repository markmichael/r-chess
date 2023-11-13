knight_available_moves <- function(board, piece) {
  if (piece@piece_type != "knight") {
    return(piece)
  }
  ### test left 2 up 1
  test_position_x <- piece@col - 2
  test_position_y <- piece@row + 1
  if (test_position_x >= 1 && test_position_y <= 8) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }

  ### test right 2 up 1
  test_position_x <- piece@col + 2
  test_position_y <- piece@row + 1
  if (test_position_x <= 8 && test_position_y <= 8) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }
  ### test left 2 down 1
  test_position_x <- piece@col - 2
  test_position_y <- piece@row - 1
  if (test_position_x >= 1 && test_position_y >= 1) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }

  ### test right 2 down 1
  test_position_x <- piece@col + 2
  test_position_y <- piece@row - 1
  if (test_position_x <= 8 && test_position_y >= 1) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }
  ### test left 1 up 2
  test_position_x <- piece@col - 1
  test_position_y <- piece@row + 2
  if (test_position_x >= 1 && test_position_y <= 8) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }

  ### test right 1 up 2
  test_position_x <- piece@col + 1
  test_position_y <- piece@row + 2
  if (test_position_x <= 8 && test_position_y <= 8) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }
  ### test left 1 down 2
  test_position_x <- piece@col - 1
  test_position_y <- piece@row - 2
  if (test_position_x >= 1 && test_position_y >= 1) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }

  ### test right 1 down 2
  test_position_x <- piece@col + 1
  test_position_y <- piece@row - 2
  if (test_position_x <= 8 && test_position_y >= 1) {
    if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    }
  }
  return(piece)
}
