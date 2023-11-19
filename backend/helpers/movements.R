move_piece <- function(game_id, current_location, new_location) {
  ### retrieve game
  game <- readRDS(paste0("./games/", game_id, ".rds"))
  ### check valid move
  if (check_move(game, current_location, new_location)) {
print('move valid')
    ### check for castling
print('checking for castling')
    if (check_for_castle(game, current_location, new_location)) {
      game <- castle_king(game, current_location, new_location)
    } else {   ### standard move/capture
      print('standard move')
    ### update new location with piece
    game <- update_location_with_piece(game, current_location, new_location)
    ### convert current location to null_piece
    game <- convert_to_null(game, current_location)
    }
    ### update turn
    game@turn <- ifelse(game@turn == "white", "black", "white")
    ### update game
    game <- check_all_available_moves(game)
    ### save game
    saveRDS(game, paste0("./games/", game@id, ".rds"))
  } else {
    return(game) # move not valid, return game unchanged
  }
  return(game)
}

check_move <- function(game, current_location, new_location) {
  ### check valid move
  if (any(list(new_location) %in% game@board[[current_location[["col"]]]][[current_location[["row"]]]]@available_moves)) {
    return(TRUE)
  } else {
    print("move not valid")
    return(FALSE)
  }
}

check_for_castle <- function(game, current_location, new_location) {
  piece <- game@board[[current_location[["col"]]]][[current_location[["row"]]]]
  if (piece@piece_type == "king" && abs(current_location[["col"]] - new_location[["col"]]) == 2) {
    return(TRUE)
  } else {
    print("not a castle")
    return(FALSE)
  }
}

castle_king <- function(game, current_location, new_location) {
  ### update new location with piece
  game <- update_location_with_piece(game, current_location, new_location)
  ### convert current location to null_piece
  game <- convert_to_null(game, current_location)
  ### determine which rook to move
  if (current_location[["col"]] < new_location[["col"]]) {
    rook_location <- list(col = 8L, row = current_location[["row"]])
  } else {
    rook_location <- list(col = 1L, row = current_location[["row"]])
  }
### determine where to move rook
  if (rook_location[["col"]] == 8L) {
    rook_new_location <- list(col = 6L, row = current_location[["row"]])
  } else {
    rook_new_location <- list(col = 4L, row = current_location[["row"]])
  }
  ### update rook
  game <- update_location_with_piece(game, rook_location, rook_new_location)
  ### convert rook to null_piece
  game <- convert_to_null(game, rook_location)
  return(game)
}

convert_to_null <- function(game, location) {
  ### convert current location to null_piece
  game@board[[location[[1]]]][[location[[2]]]]@available_moves <- list()
  game@board[[location[[1]]]][[location[[2]]]]@moved <- FALSE
  game@board[[location[[1]]]][[location[[2]]]]@piece_type <- "none"
  game@board[[location[[1]]]][[location[[2]]]]@piece_symbol <- ""
  game@board[[location[[1]]]][[location[[2]]]]@color <- "none"
  return(game)
}

update_location_with_piece <- function(game, current_location, new_location) {
  ### update new location with piece
  game@board[[new_location[[1]]]][[new_location[[2]]]] <- game@board[[current_location[[1]]]][[current_location[[2]]]]
  ### update location of moved piece
  game@board[[new_location[[1]]]][[new_location[[2]]]]@row <- new_location[[2]]
  game@board[[new_location[[1]]]][[new_location[[2]]]]@col <- new_location[[1]]
  ### update moved status of moved piece
  game@board[[new_location[[1]]]][[new_location[[2]]]]@moved <- TRUE
  return(game)
}
