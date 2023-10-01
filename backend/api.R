#* Start new game
#* @get /newgame
newgame <- function() {
  source("./methods.R")
  new_game() |>
      game2json()
}
