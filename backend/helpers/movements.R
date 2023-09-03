move_piece <- function(game, current_location, new_location) {
  ### check valid move
  if (check_move(game, current_location, new_location)) {
    ### update new location with piece
    game <- update_location_with_piece(game, current_location, new_location)
    ### convert current location to null_piece
    game <- convert_to_null(game, current_location)
  } else {
    return(game) # move not valid, return game unchanged
  }
  return(game)
}

check_move <- function(game, current_location, new_location) {
  ### check valid move
  if (new_location %in% game@board[[current_location[[1]]]][[current_location[[2]]]]@available_moves) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

convert_to_null <- function(game, location) {
  ### convert current location to null_piece
  game@board[[current_location[[1]]]][[current_location[[2]]]]@available_moves <- list()
  game@board[[current_location[[1]]]][[current_location[[2]]]]@moved <- FALSE
  game@board[[current_location[[1]]]][[current_location[[2]]]]@piece_type <- "none"
  game@board[[current_location[[1]]]][[current_location[[2]]]]@piece_symbol <- ""
  game@board[[current_location[[1]]]][[current_location[[2]]]]@color <- "none"
  return(game)
}

update_location_with_piece <- function(game, current_location, new_location) {
  ### update new location with piece
  game@board[[new_location[[1]]]][[new_location[[2]]]] <- game@board[[current_location[[1]]]][[current_location[[2]]]]
  ### update location of moved piece
  game@board[[new_location[[1]]]][[new_location[[2]]]]@row <- new_location[[1]]
  game@board[[new_location[[1]]]][[new_location[[2]]]]@col <- new_location[[2]]
  return(game)
}
