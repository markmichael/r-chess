#' @filter cors
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

#* Start new game
#* @get /newgame
newgame <- function() {
  new_game() |>
    check_all_available_moves() |>
    game2json()
}

#* Move Piece
#* @preempt cors
#* @post /movepiece
#* @param gameId
#* @param currentLocation
#* @param newLocation
movepiece <- function(gameId, currentLocation, newLocation) {
  ### parse locations
  current_location_list <- strsplit(x = currentLocation, split = "")
  current_location_list <- list(
    row = current_location_list[2],
    col = current_location_list[1]
  )
  new_location_list <- strsplit(x = newLocation, split = "")
  new_location_list <- list(
    row = new_location_list[2],
    col = new_location_list[1]
  )

  newgame <- move_piece(gameId, current_location_list, new_location_list) |>
    check_all_available_moves()
  game2json()
  return(newgame)
}
