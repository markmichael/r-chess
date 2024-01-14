check_for_checks <- function(game, color) {
  ### get position of king
  king_position <- get_king_position(game, color)
  print("king position")
  print(king_position)
  print(is_position_attacked(game, king_position))
  if (is_position_attacked(game, king_position)) {
    print("check from check function")
    return(TRUE)
  } else {
    print("no check from check function")
    return(FALSE)
  }
  ### check available moves for kings position
}

get_king_position <- function(game, color) {
  print("getting king position")
  print(color)
  board <- unlist(game@board, recursive = FALSE)
  print("king position")
  king <- board[sapply(board, function(x) {
    x@color == color && x@piece_type == "king"
  })]
  print(king)
    return(list(col = king[[1]]@col, row = king[[1]]@row))
}

check_for_checkmate <- function(game, color) {
  print("checking for checkmate")
  ### cycle through all available moves to see if checkmate resolves
  board <- unlist(game@board, recursive = FALSE)
  print("board")
  result <- !any(lapply(board, function(x, game) {
                          print(x)
    if (x@color == color) {
      any(lapply(x@available_moves, function(y, game, current_location) {
        print("is this error")
        print(current_location)
        print(y)
        game_modified <- update_location_with_piece(game, current_location, y)
        game_modified <- convert_to_null(game_modified, current_location)
        game_modified <- check_all_available_moves(game_modified)
        print("turn")
        print(game_modified@turn)
        if (check_for_checks(game_modified, game_modified@turn)) {
          print("check not resolved with this available move")
          return(FALSE)
        } else {
          print("check resolved with this available move")
          return(TRUE)
        }
      }, game = game, current_location = list(col = x@col, row = x@row)))
    } else {
      return(FALSE)
    }
  }, game = game))
  return(result)
}


is_position_attacked <- function(game, test_square) {
  print("here is my test square")
  print(test_square)
  print("here is my game turn")
  print(game@turn)
  board <- game@board
  result <- any(
    lapply(board, function(column) {
      any(lapply(column, function(row) {
        if (row@color == game@turn) {
          return(FALSE)
        } else {
          if (any(list(test_square) %in% row@available_moves)) {
            print("position is attacked")
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      }))
    })
  )
  return(result)
}

eliminate_self_checks <- function(game, piece) {
## cycle through piece's available moves
  print(piece)
piece@available_moves <- lapply(piece@available_moves, function(move) {
print(move)
  game_modified <- update_location_with_piece(game, list(col = piece@col, row = piece@row), move)
  game_modified <- convert_to_null(game_modified, list(col = piece@col, row = piece@row))
  game_modified <- check_all_available_moves(game_modified)
  if (check_for_checks(game_modified, game_modified@turn)) {
    return(NULL)
  } else {
    return(move)
  }
})
return(piece)
}
