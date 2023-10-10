move_piece <- function(game_id, current_location, new_location) {
  ### retrieve game
  game <- readRDS(paste0("./games/", game_id, ".rds"))
  ### check valid move
  if (check_move(game, current_location, new_location)) {
    ### update new location with piece
    game <- update_location_with_piece(game, current_location, new_location)
    ### convert current location to null_piece
    game <- convert_to_null(game, current_location)
    ### update game
    game <- check_all_available_moves(game)
    ### update turn
    game@turn <- ifelse(game@turn == "white", "black", "white")
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
  return(game)
}
