rook_available_moves <- function(board, piece) {
  if (!(piece@piece_type %in% c("king", "queen", "rook"))) {
    return(piece)
  }
  ### test above rook
  test_position_y <- piece@row + 1
  while (test_position_y <= 8) {
    if (board[[piece@col]][[test_position_y]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = piece@col, row = test_position_y)))
    } else if (board[[piece@col]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = piece@col, row = test_position_y)))
      break
    } else {
      break
    }
    test_position_y <- test_position_y + 1
  }

  ### test below rook

  test_position_y <- piece@row - 1
  while (test_position_y >= 1) {
    if (board[[piece@col]][[test_position_y]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = piece@col, row = test_position_y)))
    } else if (board[[piece@col]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = piece@col, row = test_position_y)))
      break
    } else {
      break
    }
    test_position_y <- test_position_y - 1
  }
  ### test left rook
  test_position_x <- piece@col - 1
  while (test_position_x >= 1) {
    if (board[[test_position_x]][[piece@row]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = piece@row)))
    } else if (board[[test_position_x]][[piece@row]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = piece@row)))
      break
    } else {
      break
    }
    test_position_x <- test_position_x - 1
  }

  ### test right rook
  test_position_x <- piece@col + 1
  while (test_position_x <= 8) {
    if (board[[test_position_x]][[piece@row]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = piece@row)))
    } else if (board[[test_position_x]][[piece@row]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = piece@row)))
      break
    } else {
      break
    }
    test_position_x <- test_position_x + 1
  }
  return(piece)
}
