#* @filter cors
cors <- function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
}

#* Start new game
#* @get /newgame
newgame <- function() {
  source("./methods.R")
  new_game() |>
    check_all_available_moves() |>
    game2json()
}

#* Move Piece
#* @post /movepiece
#* @param current_location
#* @param new_location
movepiece <- function(current_location, new_location) {
  source("./helpers/movements.R")
  ### parse locations
  current_location_list <- strsplit(x = current_location, split = "")
  current_location_list <- list(
    row = current_location_list[2],
    col = current_location_list[1]
  )
  new_location_list <- strsplit(x = new_location, split = "")
  new_location_list <- list(
    row = new_location_list[2],
    col = new_location_list[1]
  )

  newgame <- move_piece(newgame, current_location_list, new_location_list) |>
    check_all_available_moves()
  game2json()
}
