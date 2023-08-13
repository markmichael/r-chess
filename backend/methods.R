new_game <- function() {
  game_board <- list(
    a = list(),
    b = list(),
    c = list(),
    d = list(),
    e = list(),
    f = list(),
    g = list(),
    h = list()
  )
  print(game_board$a)
  ## create an empty board
  game_board <- game_board |>
    lapply(function(column) {
      print(column)
      return(
        list(
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL
        )
      )
    })
  ## place pawns
  game_board <- seq_along(game_board) |>
    lapply(function(i) {
      final_column <- game_board[[i]]
      white_pawn <- piece(
        color = "white",
        row = 2L,
        col = letters[[i]],
        piece_type = "pawn",
        piece_symbol = "p",
        available_moves = list(),
        checked = FALSE,
        moved = FALSE
      )
      black_pawn <- piece(
        color = "black",
        row = 7L,
        col = letters[[i]],
        piece_type = "pawn",
        piece_symbol = "p",
        available_moves = list(),
        checked = FALSE,
        moved = FALSE
      )
      if (i %in% c(1, 8)) {
        white_rook <- piece(
          color = "white",
          row = 1L,
          col = letters[[i]],
          piece_type = "rook",
          piece_symbol = "r",
          available_moves = list(),
          checked = FALSE,
          moved = FALSE
        )
        black_rook <- piece(
          color = "black",
          row = 8L,
          col = letters[[i]],
          piece_type = "rook",
          piece_symbol = "r",
          available_moves = list(),
          checked = FALSE,
          moved = FALSE
        )
        final_column[[1]] <- white_rook
        final_column[[8]] <- black_rook
      }
      if (i %in% c(2, 7)) {
        white_knight <- piece(
          color = "white",
          row = 1L,
          col = letters[[i]],
          piece_type = "knight",
          piece_symbol = "n",
          available_moves = list(),
          checked = FALSE,
          moved = FALSE
        )
        black_knight <- piece(
          color = "black",
          row = 8L,
          col = letters[[i]],
          piece_type = "knight",
          piece_symbol = "n",
          available_moves = list(),
          checked = FALSE,
          moved = FALSE
        )
        final_column[[1]] <- white_knight
        final_column[[8]] <- black_knight
      }
      if (i %in% c(3, 6)) {
        white_bishop <- piece(
          color = "white",
          row = 1L,
          col = letters[[i]],
          piece_type = "bishop",
          piece_symbol = "b",
          available_moves = list(),
          checked = FALSE,
          moved = FALSE
        )
        black_bishop <- piece(
          color = "black",
          row = 8L,
          col = letters[[i]],
          piece_type = "bishop",
          piece_symbol = "b",
          available_moves = list(),
          checked = FALSE,
          moved = FALSE
        )
        final_column[[1]] <- white_bishop
        final_column[[8]] <- black_bishop
        if (i == 4) {
          white_queen <- piece(
            color = "white",
            row = 1L,
            col = letters[[i]],
            piece_type = "queen",
            piece_symbol = "q",
            available_moves = list(),
            checked = FALSE,
            moved = FALSE
          )
          black_queen <- piece(
            color = "black",
            row = 8L,
            col = letters[[i]],
            piece_type = "queen",
            piece_symbol = "q",
            available_moves = list(),
            checked = FALSE,
            moved = FALSE
          )
          final_column[[1]] <- white_queen
          final_column[[8]] <- black_queen
        }
        if (i == 5) {
          white_king <- piece(
            color = "white",
            row = 1L,
            col = letters[[i]],
            piece_type = "king",
            piece_symbol = "k",
            available_moves = list(),
            checked = FALSE,
            moved = FALSE
          )
          black_king <- piece(
            color = "black",
            row = 8L,
            col = letters[[i]],
            piece_type = "king",
            piece_symbol = "k",
            available_moves = list(),
            checked = FALSE,
            moved = FALSE
          )
          final_column[[1]] <- white_king
          final_column[[8]] <- black_king
        }
      }
      final_column[[2]] <- white_pawn
      final_column[[7]] <- black_pawn
      return(final_column)
    })
  names(game_board) <- letters[1:8]
  ## place rooks

  return(game_board)
}
source("./objects.R")
a <- new_game()
print(a)

check_available_moves <- function(piece, board) {
  if (piece@piece_type == "pawn" && piece@color == "white") {
      return(white_pawn_available_moves(piece, board))
  }
  if (piece@piece_type == "pawn" && piece@color == "black") {
      return(black_pawn_available_moves(piece, board))
  }
  return(piece)
}
white_pawn_available_moves <- function(piece, board) {
    if (piece@moved == FALSE &&
        board[[piece@col]][[piece@row + 2]] == NULL &&
        board[[piece@col]][[piece@row + 1]] == NULL) {
      piece@available_moves <- piece@available_moves |>
        append(list(col = piece@col, row = piece@row + 2))
    }
    if (
        board[[piece@col]][[piece@row + 1]] == NULL &&
        piece@row + 1 <= 8) {
      piece@available_moves <- piece@available_moves |>
        append(list(col = piece@col, row = piece@row + 1))
    }
  return(piece)
}

black_pawn_available_moves <- function(piece, board) {
    if (piece@moved == FALSE &&
        board[[piece@col]][[piece@row - 2]] == NULL &&
        board[[piece@col]][[piece@row - 1]] == NULL) {
      piece@available_moves <- piece@available_moves |>
        append(list(col = piece@col, row = piece@row + 2))
    }
    if (
        board[[piece@col]][[piece@row - 1]] == NULL &&
        piece@row - 1 >= 1) {
      piece@available_moves <- piece@available_moves |>
        append(list(col = piece@col, row = piece@row + 1))
    }
  return(piece)
}

check_available_moves(a[["a"]][[2]], a)
a[["a"]][[2]]
