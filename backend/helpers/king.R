king_available_moves <- function(board, piece) {
  if (piece@piece_type != "king") {
    return(piece)
  }
  piece <- queen_available_moves(board, piece)
  return(piece)

  piece@available_moves <- piece@available_moves |>
    lapply(function(x) {
      if ((x$col %in% c(
        letters[which(letters == piece@col) + 1],
        letters[which(letters == piece@col) - 1],
        letters[which(letters == piece@col)]
      )) &&
        x$row %in% c(piece@row - 1, piece@row, piece@row + 1)) {
        return(x)
      } else {
        return(NULL)
      }
    })
  return(piece)
}
