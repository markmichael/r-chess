source("../objects.R")
source("./bishop.R")
source("./rook.R")

queen_available_moves <- function(board, piece) {
  if (!(piece@piece_type %in% c("king", "queen"))) {
    return(piece)
  }
  piece <- rook_available_moves(board, piece)
  piece <- bishop_available_moves(board, piece)
  return(piece)
}
