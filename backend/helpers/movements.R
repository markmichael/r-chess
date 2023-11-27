move_piece <- function(game_id,
                       current_location,
                       new_location,
                       promotion_string = NULL) {
  print(game_id)
  print("current location")
  print(current_location)
  print("new location")
  print(new_location)
  print("promotion")
  print(promotion_string)
  ### retrieve game
  game <- readRDS(paste0("./games/", game_id, ".rds"))
  move_string <- paste0(letters[current_location[["col"]]], current_location[["row"]], letters[new_location[["col"]]], new_location[["row"]])
  game_modified <- game
  ### check valid move
  if (check_move(game_modified, current_location, new_location)) {
    print("move valid")
    ### check for promotion
    if(check_for_promotion(game_modified, current_location, new_location, promotion_string)) {
      print(paste0("promoting to ", promotion_string))
      game_modified <- promote_pawn(game_modified, current_location, new_location, promotion_string)
    } else if (check_for_castle(game_modified, current_location, new_location)) {
      game_modified <- castle_king(game_modified, current_location, new_location)
    } else { ### standard move/capture
      print("standard move")
      ### update new location with piece
      game_modified <- update_location_with_piece(game_modified, current_location, new_location)
      ### convert current location to null_piece
      game_modified <- convert_to_null(game_modified, current_location)
      game_modified <- check_all_available_moves(game_modified)
    }
    ### if there was a check, check that it resolved. Also check that no self checks are created
    if (check_for_checks(game_modified, game_modified@turn)) {
      print("you cannot end turn in check")
      saveRDS(game, paste0("./games/", game@id, ".rds"))
      return(game)
    } else {
      print("turn ended without check")
      game_modified@check <- FALSE
    }
    ### update turn
    game_modified@turn <- ifelse(game_modified@turn == "white", "black", "white")
    ### add move to history
    game_modified@moves <- c(game_modified@moves, move_string)
    ### update game
    game_modified <- check_all_available_moves(game_modified)

    ### check for new check
    if (check_for_checks(game_modified, game_modified@turn)) {
      if (check_for_checkmate(game_modified, game_modified@turn)) {
        print("checkmate!")
        game_modified@checkmate <- TRUE
      } else {
        print("check!")
        game_modified@check <- TRUE
      }
    }
    ### save game
    saveRDS(game_modified, paste0("./games/", game@id, ".rds"))
  } else {
    return(game) # move not valid, return game unchanged
  }
  return(game_modified)
}

check_move <- function(game, current_location, new_location) {
  print("here i am checking move")
  print("current location")
  print(current_location)
  print("new location")
  print(new_location)
  print("pieces")
  print(game@board[[current_location[["col"]]]][[current_location[["row"]]]])
  print(game@board[[new_location[["col"]]]][[new_location[["row"]]]])
  ### check valid move
  if (any(list(new_location) %in% game@board[[current_location[["col"]]]][[current_location[["row"]]]]@available_moves) &&
    game@board[[current_location[["col"]]]][[current_location[["row"]]]]@color == game@turn) {
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
  print("castling king")
  game <- update_location_with_piece(game, current_location, new_location)
  ### convert current location to null_piece
  game <- convert_to_null(game, current_location)
  ## determine which rook to move
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
  print("converting to null")
  ### convert current location to null_piece
  game@board[[location[[1]]]][[location[[2]]]]@available_moves <- list()
  game@board[[location[[1]]]][[location[[2]]]]@moved <- FALSE
  game@board[[location[[1]]]][[location[[2]]]]@piece_type <- "none"
  game@board[[location[[1]]]][[location[[2]]]]@color <- "none"
  return(game)
}

update_location_with_piece <- function(game, current_location, new_location) {
  print("updating location")
  ### check for en passant
  if (game@board[[current_location[[1]]]][[current_location[[2]]]]@piece_type == "pawn" &&
    current_location[[1]] != new_location[[1]] &&
    game@board[[new_location[[2]]]][[new_location[[1]]]]@color == "none") {
    print("capturing en passant")
    game <- convert_to_null(game, list(col = new_location[[1]], row = current_location[[2]]))
  }
  ### update new location with piece
  game@board[[new_location[[1]]]][[new_location[[2]]]] <- game@board[[current_location[[1]]]][[current_location[[2]]]]
  ### update location of moved piece
  game@board[[new_location[[1]]]][[new_location[[2]]]]@row <- new_location[[2]] |> as.integer()
  game@board[[new_location[[1]]]][[new_location[[2]]]]@col <- new_location[[1]] |> as.integer()
  ### update moved status of moved piece
  game@board[[new_location[[1]]]][[new_location[[2]]]]@moved <- TRUE
  return(game)
}
