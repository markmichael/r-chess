sapply(list.files("./helpers", full.names = TRUE), source)
new_game <- function() {
  game_board <- chess_game(
    id = "",
    board = list(),
    turn = "white",
    moves = list(),
    check = FALSE,
    checkmate = FALSE
  )
  baseuuid <- paste(sample(c(letters[1:6], 0:9), 30, replace = TRUE), collapse = "")
  game_board@id <- paste(
    substr(baseuuid, 1, 8),
    "-",
    substr(baseuuid, 9, 12),
    "-",
    "4",
    substr(baseuuid, 13, 15),
    "-",
    sample(c("8", "9", "a", "b"), 1),
    substr(baseuuid, 16, 18),
    "-",
    substr(baseuuid, 19, 30),
    sep = "",
    collapse = ""
  )
  game_board@board <- list(
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list(),
    list()
  )
  ## create an empty board
  game_board@board <- game_board@board |>
    lapply(function(column) {
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
  game_board@board <- seq_along(game_board@board) |>
    lapply(function(i) {
      i <- as.integer(i)
      final_column <- game_board@board[[i]]
      white_pawn <- piece(
        color = "white",
        row = 2L,
        col = i,
        piece_type = "pawn",
        piece_symbol = "p",
        available_moves = list(),
        moved = FALSE
      )
      black_pawn <- piece(
        color = "black",
        row = 7L,
        col = i,
        piece_type = "pawn",
        piece_symbol = "p",
        available_moves = list(),
        moved = FALSE
      )
      if (i %in% c(1, 8)) {
        white_rook <- piece(
          color = "white",
          row = 1L,
          col = i,
          piece_type = "rook",
          piece_symbol = "r",
          available_moves = list(),
          moved = FALSE
        )
        black_rook <- piece(
          color = "black",
          row = 8L,
          col = i,
          piece_type = "rook",
          piece_symbol = "r",
          available_moves = list(),
          moved = FALSE
        )
        final_column[[1]] <- white_rook
        final_column[[8]] <- black_rook
      }
      if (i %in% c(2, 7)) {
        white_knight <- piece(
          color = "white",
          row = 1L,
          col = i,
          piece_type = "knight",
          piece_symbol = "n",
          available_moves = list(),
          moved = FALSE
        )
        black_knight <- piece(
          color = "black",
          row = 8L,
          col = i,
          piece_type = "knight",
          piece_symbol = "n",
          available_moves = list(),
          moved = FALSE
        )
        final_column[[1]] <- white_knight
        final_column[[8]] <- black_knight
      }
      if (i %in% c(3, 6)) {
        white_bishop <- piece(
          color = "white",
          row = 1L,
          col = i,
          piece_type = "bishop",
          piece_symbol = "b",
          available_moves = list(),
          moved = FALSE
        )
        black_bishop <- piece(
          color = "black",
          row = 8L,
          col = i,
          piece_type = "bishop",
          piece_symbol = "b",
          available_moves = list(),
          moved = FALSE
        )
        final_column[[1]] <- white_bishop
        final_column[[8]] <- black_bishop
      }
      if (i == 4) {
        white_queen <- piece(
          color = "white",
          row = 1L,
          col = i,
          piece_type = "queen",
          piece_symbol = "q",
          available_moves = list(),
          moved = FALSE
        )
        black_queen <- piece(
          color = "black",
          row = 8L,
          col = i,
          piece_type = "queen",
          piece_symbol = "q",
          available_moves = list(),
          moved = FALSE
        )
        final_column[[1]] <- white_queen
        final_column[[8]] <- black_queen
      }
      if (i == 5) {
        white_king <- piece(
          color = "white",
          row = 1L,
          col = i,
          piece_type = "king",
          piece_symbol = "k",
          available_moves = list(),
          moved = FALSE
        )
        black_king <- piece(
          color = "black",
          row = 8L,
          col = i,
          piece_type = "king",
          piece_symbol = "k",
          available_moves = list(),
          moved = FALSE
        )
        final_column[[1]] <- white_king
        final_column[[8]] <- black_king
      }

      final_column[[2]] <- white_pawn
      final_column[[7]] <- black_pawn

      final_column[[3]] <- null_piece <- piece(
        color = "none",
        row = 3L,
        col = i,
        piece_type = "none",
        piece_symbol = "",
        available_moves = list(),
        moved = FALSE
      )
      final_column[[4]] <- null_piece <- piece(
        color = "none",
        row = 4L,
        col = i,
        piece_type = "none",
        piece_symbol = "",
        available_moves = list(),
        moved = FALSE
      )
      final_column[[5]] <- null_piece <- piece(
        color = "none",
        row = 5L,
        col = i,
        piece_type = "none",
        piece_symbol = "",
        available_moves = list(),
        moved = FALSE
      )
      final_column[[6]] <- null_piece <- piece(
        color = "none",
        row = 6L,
        col = i,
        piece_type = "none",
        piece_symbol = "",
        available_moves = list(),
        moved = FALSE
      )

      return(final_column)
    })
  game_board <- game_board |>
    check_all_available_moves()
  saveRDS(game_board, paste0("./games/", game_board@id, ".rds"))
  return(game_board)
}

check_available_moves <- function(game, piece) {
  piece@available_moves <- list()
  if (piece@color != game@turn) {
    return(piece)
  } else {
    if (piece@piece_type == "pawn" && piece@color == "white") {
      piece <- white_pawn_available_moves(game@board, piece)
    }
    if (piece@piece_type == "pawn" && piece@color == "black") {
      piece <- black_pawn_available_moves(game@board, piece)
    }
    if (piece@piece_type == "rook") {
      piece <- rook_available_moves(game@board, piece)
    }
    if (piece@piece_type == "bishop") {
      piece <- bishop_available_moves(game@board, piece)
    }
    if (piece@piece_type == "knight") {
      piece <- knight_available_moves(game@board, piece)
    }
    if (piece@piece_type == "queen") {
      piece <- queen_available_moves(game@board, piece)
    }
    if (piece@piece_type == "king") {
      piece <- king_available_moves(game, piece)
    }
    return(piece)
  }
}

check_all_available_moves <- function(game) {
  game@board <- lapply(game@board, FUN = function(col, game) {
    col <- lapply(col, check_available_moves, game = game)
  }, game = game)
  saveRDS(game, paste0("./games/", game@id, ".rds"))
  return(game)
}

game2json <- function(game) {
  list_game <- list(
    id = game@id,
    board = game@board,
    turn = game@turn,
    moves = game@moves,
    check = game@check,
    checkmate = game@checkmate
  )
  list_game$board <- lapply(game@board, FUN = function(col) {
    lapply(col, FUN = function(piece) {
      return(
        list(
          color = piece@color,
          row = piece@row,
          col = letters[piece@col],
          piece_type = piece@piece_type,
          piece_symbol = piece@piece_symbol,
          available_moves = piece@available_moves,
          moved = piece@moved
        )
      )
    })
  })
  names(list_game$board) <- c("a", "b", "c", "d", "e", "f", "g", "h")
  return(list_game)
}

json2game <- function(list_game) {
  game <- game(
    id = list_game$id,
    board = list_game$board,
    turn = list_game$turn,
    moves = list_game$moves,
    check = list_game$check,
    checkmate = list_game$checkmate
  )
  game@board <- lapply(game@board, FUN = function(col) {
    lapply(col, FUN = function(piece) {
      return(piece(
        color = piece$color,
        row = piece$row,
        col = piece$col,
        piece_type = piece$piece_type,
        piece_symbol = piece$piece_symbol,
        available_moves = piece$available_moves,
        moved = piece$moved
      ))
    })
  })
  return(game)
}
