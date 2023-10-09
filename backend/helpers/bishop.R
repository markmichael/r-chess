bishop_available_moves <- function(board, piece) {
  if (!(piece@piece_type %in% c("king", "queen", "bishop"))) {
    return(piece)
  }

  ### test above right diagonal
  test_position_x <- letters[which(letters == piece@col) + 1]
  test_position_y <- piece@row + 1
  while (test_position_x <= "h" && test_position_y <= 8) {
    if (board[[test_position_x]][[test_position_y]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    } else if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
      break
    } else {
      break
    }
    test_position_x <- letters[which(letters == test_position_x) + 1]
    test_position_y <- test_position_y + 1
  }

  ### test above left diagonal
  test_position_x <- letters[which(letters == piece@col) - 1]
  test_position_y <- piece@row + 1
  print("bishop above left diagonal")
  print(test_position_x)
  print(test_position_y)
  if (length(test_position_x) > 0) {
  while (length(test_position_x) > 0 && test_position_x >= "a" && test_position_y <= 8) {
  print(board[[test_position_x]][[test_position_y]])
    if (board[[test_position_x]][[test_position_y]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    } else if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
      break
    } else {
      break
    }
    test_position_x <- letters[which(letters == test_position_x) - 1]
    test_position_y <- test_position_y + 1
  }
  }

  ### test below left diagonal
  test_position_x <- letters[which(letters == piece@col) - 1]
  test_position_y <- piece@row - 1
  if (length(test_position_x) > 0) {
  while (length(test_position_x) > 0 && test_position_x >= "a" && test_position_y >= 1) {
    if (board[[test_position_x]][[test_position_y]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    } else if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
      break
    } else {
      break
    }
    test_position_x <- letters[which(letters == test_position_x) - 1]
    test_position_y <- test_position_y - 1
  }
  }

  ### test below right diagonal
  test_position_x <- letters[which(letters == piece@col) + 1]
  test_position_y <- piece@row - 1
  while (test_position_x <= "h" && test_position_y >= 1) {
    if (board[[test_position_x]][[test_position_y]]@color == "none") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
    } else if (board[[test_position_x]][[test_position_y]]@color != piece@color) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = test_position_x, row = test_position_y)))
      break
    } else {
      break
    }
    test_position_x <- letters[which(letters == test_position_x) + 1]
    test_position_y <- test_position_y - 1
  }
  return(piece)
}
