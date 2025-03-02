#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* Serve page
#* @assets ../frontend /
index <- function() {
  plumber::forward()
}

#* Start new game
#* @get /newgame
newgame <- function() {
  print("hello")
  modified_game <- new_game() |>
    check_all_available_moves() |>
    game2json()
}

#* Move Piece
#* @post /movepiece
#* @param gameId
#* @param currentLocation
#* @param newLocation
movepiece <- function(gameId, currentLocation, newLocation, promotion = "none") {
  ### parse locations
  current_location_list <- strsplit(x = currentLocation, split = "")
  current_location_list <- list(
    col = which(letters == current_location_list[[1]][1]),
    row = current_location_list[[1]][2] |> as.integer()
  )
  new_location_list <- strsplit(x = newLocation, split = "")
  new_location_list <- list(
    col = which(letters == new_location_list[[1]][1]),
    row = new_location_list[[1]][2] |> as.integer()
  )
  modified_game <- move_piece(gameId, current_location_list, new_location_list, promotion_string = promotion) |>
    game2json()
  return(modified_game)
}
