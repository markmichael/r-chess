const boardInfo = new XMLHttpRequest()
boardInfo.responseType = 'json'
boardInfo.open('GET', 'http://localhost:8001/newgame', true)
boardInfo.send()

// create divs for each piece on the board
const board = document.getElementById('board')
boardInfo.onload = () => {
  const cols = boardInfo.response.board
  console.log(cols)
  for (const col in cols) {
    const row = (cols[col])
    for (const square in row) {
      if (row[square].piece_type[0] !== 'none') {
        const pieceLocation = (row[square].col + row[square].row)
        const piece = document.createElement('div')
        piece.classList.add(row[square].color + '_' + row[square].piece_type)
        document.getElementById(pieceLocation).appendChild(piece)
      }
    }
  }
}
