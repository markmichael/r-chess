white_pawn_available_moves <- function(board, piece) {
  if (piece@moved == FALSE &&
    board[[piece@col]][[piece@row + 2]]@color == "none" &&
    board[[piece@col]][[piece@row + 1]]@color == "none") {
    piece@available_moves <- piece@available_moves |>
      append(list(list(col = piece@col, row = piece@row + 2)))
  }
  if (
    board[[piece@col]][[piece@row + 1]]@color == "none" &&
      piece@row + 1 <= 8) {
    piece@available_moves <- piece@available_moves |>
      append(list(list(col = piece@col, row = piece@row + 1)))
  }
  ### check for attack
  if (piece@col != 8L) {
    if (board[[piece@col + 1]][[piece@row + 1]]@color == "black") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(
          col = piece@col + 1,
          row = piece@row + 1
        )))
    }
  }
  if (piece@col != 1L) {
    if (board[[piece@col - 1]][[piece@row + 1]]@color == "black") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(
          col = piece@col - 1,
          row = piece@row + 1
        )))
    }
  }
  return(piece)
}

black_pawn_available_moves <- function(board, piece) {
  if (piece@moved == FALSE &&
    board[[piece@col]][[piece@row - 2]]@color == "none" &&
    board[[piece@col]][[piece@row - 1]]@color == "none") {
    piece@available_moves <- piece@available_moves |>
      append(list(list(col = piece@col, row = piece@row - 2)))
  }
  if (
    board[[piece@col]][[piece@row - 1]]@color == "none" &&
      piece@row - 1 >= 1) {
    piece@available_moves <- piece@available_moves |>
      append(list(list(col = piece@col, row = piece@row - 1)))
  }
  ### check for attack
  if (piece@col != 8L) {
    if (board[[piece@col + 1]][[piece@row - 1]]@color == "white") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(
          col = piece@col + 1,
          row = piece@row - 1
        )))
    }
  }
  if (piece@col != 1L) {
    if (board[[piece@col - 1]][[piece@row - 1]]@color == "white") {
      piece@available_moves <- piece@available_moves |>
        append(list(list(
          col = piece@col - 1],
          row = piece@row - 1
        )))
    }
  }
  return(piece)
}
