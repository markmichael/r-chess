king_available_moves <- function(game, piece) {
  board <- game@board
  if (piece@piece_type != "king") {
    return(piece)
  }
  piece <- queen_available_moves(board, piece)
  piece@available_moves <- piece@available_moves |>
    lapply(function(x) {
      if ((x$col %in% c(
        piece@col - 1,
        piece@col,
        piece@col + 1
      )) &&
        x$row %in% c(
          piece@row - 1,
          piece@row,
          piece@row + 1
        )) {
        return(x)
      } else {
        return(NULL)
      }
    })

  ### check for king in original location
  if ((piece@row == 1 &&
    piece@col == 5) ||
    (piece@row == 8 &&
      piece@col == 5)) {
    print("king is in original location")
    print("evaluate for short castle")
    if (piece@moved == FALSE &&
      piece@row %in% c(1, 8) &&
      board[[piece@col + 1]][[piece@row]]@color == "none" &&
      board[[piece@col + 2]][[piece@row]]@color == "none" &&
      board[[piece@col + 3]][[piece@row]]@piece_type == "rook" &&
      !board[[piece@col + 3]][[piece@row]]@moved &&
      !is_position_attacked(
        game,
        list(col = piece@col + 1, row = piece@row)
      ) &&
      !is_position_attacked(
        game,
        list(col = piece@col + 2, row = piece@row)
      ) &&
      !is_position_attacked(
        game,
        list(col = piece@col, row = piece@row)
      )) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = piece@col + 2, row = piece@row)))
    } else {
      print("castle not possible")
    }
    ### check for long castle
    print("evaluate for long castle")
    if (piece@moved == FALSE &&
      piece@row %in% c(1, 8) &&
      board[[piece@col - 1]][[piece@row]]@color == "none" &&
      board[[piece@col - 2]][[piece@row]]@color == "none" &&
      board[[piece@col - 3]][[piece@row]]@color == "none" &&
      board[[piece@col - 4]][[piece@row]]@piece_type == "rook" &&
      board[[piece@col - 4]][[piece@row]]@moved == FALSE &&
      !is_position_attacked(
        game,
        list(col = piece@col - 1, row = piece@row)
      ) &&
      !is_position_attacked(
        game,
        list(col = piece@col - 2, row = piece@row)
      ) &&
      !is_position_attacked(
        game,
        list(col = piece@col, row = piece@row)
      )
    ) {
      piece@available_moves <- piece@available_moves |>
        append(list(list(col = piece@col - 2, row = piece@row)))
    }
  }
  piece@available_moves <- Filter(Negate(is.null), piece@available_moves)
  return(piece)
}
