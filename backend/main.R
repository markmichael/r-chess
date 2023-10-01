library(jsonlite)
library(plumber)
source("objects.R")
source("methods.R")
source("api.R")
game1 <- new_game()

game1 <- check_all_available_moves(game1)

game1 <- move_piece(game1, list("b", 1L), list(col = "c", row = 3L))

game1@board[["b"]][[1]]
game1@board[["c"]][[3]]

game1 <- check_all_available_moves(game1)

pr("api.R") |>
    pr_run(port=8001)

