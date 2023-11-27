check_for_promotion <- function(game,
                       current_location,
                       new_location,
                       promotion_string) {
  if (game@board[[current_location[[1]]]][[current_location[[2]]]]@piece_type == "pawn" &&
      new_location[[2]] %in% c(8L, 1L) &&
      promotion_string %in% c("queen", "knight", "rook", "bishop")) {
        ### promotion valid
    print("promotion valid")
        return(TRUE)
      } else {
        print("promotion not valid")
        return(FALSE)
      }
}

promote_pawn <- function(game, current_location, new_location, promotion_string) {
  print("promoting")
  ### update new location with piece
  game <- update_location_with_piece(game, current_location, new_location)
  ### convert current location to null_piece
  game <- convert_to_null(game, current_location)
  ### update piece type
switch(promotion_string,
  "queen" =  game@board[[new_location[[1]]]][[new_location[[2]]]]@piece_type <- "queen",
  "knight" = game@board[[new_location[[1]]]][[new_location[[2]]]]@piece_type <- "knight",
  "rook" = game@board[[new_location[[1]]]][[new_location[[2]]]]@piece_type <- "rook",
  "bishop" = game@board[[new_location[[1]]]][[new_location[[2]]]]@piece_type <- "bishop"
)
}
