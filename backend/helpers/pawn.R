white_pawn_available_moves <- function(game, piece) {
  board <- game@board
  if (piece@row < 8) {
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
    ### check for en passant
    if (piece@row == 5) {
      print("check for en passant")
      ### check left up diagonal is empty
      if (piece@col > 1) {
        print("check left up diagonal is empty")
        if (board[[piece@col - 1]][[6]]@color == "none") {
          print("left up diagonal is empty")
          ### check if enemy pawn to the left
          print("check if enemy pawn to the left")
          if (board[[piece@col - 1]][[5]]@color == "black") {
            print("enemy pawn to the left")
            ### check if most recent move was pawn move
            most_recent_move <- game@moves[[length(game@moves)]]
            print("most recent move")
            print(most_recent_move)
            print(substr(most_recent_move, 1, 1))
            if (substr(most_recent_move, 1, 1) == letters[piece@col - 1] &&
              substr(most_recent_move, 2, 2) == "7" &&
              substr(most_recent_move, 3, 3) == letters[piece@col - 1] &&
              substr(most_recent_move, 4, 4) == "5") {
              piece@available_moves <- piece@available_moves |>
                append(list(list(
                  col = piece@col - 1,
                  row = piece@row + 1
                )))
            }
          }
        }
      }
      ### check right up diagonal is empty
      if (piece@col < 8) {
        if (board[[piece@col + 1]][[piece@row + 1]]@color == "none") {
          ### check if enemy pawn to the right
          if (board[[piece@col + 1]][[piece@row]]@color == "black") {
            ### check if most recent move was pawn move
            most_recent_move <- game@moves[[length(game@moves)]]
            if (substr(most_recent_move, 1, 1) == letters[piece@col + 1] &&
              substr(most_recent_move, 2, 2) == "7" &&
              substr(most_recent_move, 3, 3) == letters[piece@col + 1] &&
              substr(most_recent_move, 4, 4) == "5") {
              piece@available_moves <- piece@available_moves |>
                append(list(list(
                  col = piece@col + 1,
                  row = piece@row + 1
                )))
            }
          }
        }
      }
    }
  }
  return(piece)
}

black_pawn_available_moves <- function(game, piece) {
  board <- game@board
  if (piece@row > 1) {
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
            col = piece@col - 1,
            row = piece@row - 1
          )))
      }
    }
    if (piece@row == 4) {
      ### check left down diagonal is empty
      if (piece@col > 1) {
        if (board[[piece@col - 1]][[piece@row - 1]]@color == "none") {
          ### check if enemy pawn to the left
          if (board[[piece@col - 1]][[piece@row]]@color == "white") {
            ### check if most recent move was pawn move
            most_recent_move <- game@moves[[length(game@moves)]]
            if (substr(most_recent_move, 1, 1) == letters[piece@col - 1] &&
              substr(most_recent_move, 2, 2) == "2" &&
              substr(most_recent_move, 3, 3) == letters[piece@col - 1] &&
              substr(most_recent_move, 4, 4) == "4") {
              piece@available_moves <- piece@available_moves |>
                append(list(list(
                  col = piece@col - 1,
                  row = piece@row - 1
                )))
            }
          }
        }
      }
      ### check right down diagonal is empty
      if (piece@col < 8) {
        if (board[[piece@col + 1]][[piece@row - 1]]@color == "none") {
          ### check if enemy pawn to the right
          if (board[[piece@col + 1]][[piece@row]]@color == "white") {
            ### check if most recent move was pawn move
            most_recent_move <- game@moves[[length(game@moves)]]
            if (substr(most_recent_move, 1, 1) == letters[piece@col + 1] &&
              substr(most_recent_move, 2, 2) == "2" &&
              substr(most_recent_move, 3, 3) == letters[piece@col + 1] &&
              substr(most_recent_move, 4, 4) == "4") {
              piece@available_moves <- piece@available_moves |>
                append(list(list(
                  col = piece@col + 1,
                  row = piece@row - 1
                )))
            }
          }
        }
      }
    }
  }
  return(piece)
}
