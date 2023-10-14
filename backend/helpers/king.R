king_available_moves <- function(board, piece) {
    if (piece@piece_type != "king") {
        return(piece)
    }
    piece <- queen_available_moves(board, piece)
    return(piece)

    piece@available_moves <- piece@available_moves |>
        lapply(function(x) {
            if ((x$col %in% c(
                letters[which(letters == piece@col) + 1],
                letters[which(letters == piece@col) - 1],
                letters[which(letters == piece@col)]
            )) &&
                x$row %in% c(piece@row - 1, piece@row, piece@row + 1)) {
                return(x)
            } else {
                return(NULL)
            }
        })
    ### check for short castle
    if (piece@moved == FALSE &&
        piece@row == 1 &&
        board[[letters[which(letters == piece@col) + 1]]][[piece@row]]@color == "none" &&
        board[[letters[which(letters == piece@col) + 2]]][[piece@row]]@color == "none" &&
        board[[letters[which(letters == piece@col) + 3]]][[piece@row]]@moved == FALSE &&
        is_position_attacked(
            board,
            list(col = letters[which(letters == piece@col) + 1], row = piece@row)
        ) &&
        is_position_attacked(
            board,
            list(col = letters[which(letters == piece@col) + 2], row = piece@row)
        ) &&
        is_position_attacked(
            board,
            list(col = letters[which(letters == piece@col) + 3], row = piece@row)
        )) {
        piece@available_moves <- piece@available_moves |>
            append(list(list(col = piece@col, row = piece@row + 2)))
    }
    return(piece)
}
