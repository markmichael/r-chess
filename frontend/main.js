const boardInfo = new XMLHttpRequest()
boardInfo.responseType = 'json'
boardInfo.open('GET', 'http://localhost:8001/newgame', true)
boardInfo.send()

// create divs for each piece on the board
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

// post move after clicking submit
const submit = document.getElementById('submitMove')
submit.onclick = () => {
  const gameId = boardInfo.response.id
  const currentLocation = document.getElementById('current_location').value
  const newLocation = document.getElementById('new_location').value
  const boardInfoNew = new XMLHttpRequest()
  boardInfoNew.responseType = 'json'
  boardInfoNew.open('POST', 'http://localhost:8001/movepiece', true)
  boardInfoNew.setRequestHeader('Content-Type', 'application/json')
  boardInfoNew.send(JSON.stringify({ gameId, currentLocation, newLocation }))
  // wait for response
  boardInfoNew.onload = () => {
    console.log(boardInfoNew.response)
    const cols = boardInfoNew.response.board
    console.log(cols)
    for (const col in cols) {
      const row = (cols[col])
      for (const square in row) {
        const pieceLocation = (row[square].col + row[square].row)
        if (row[square].piece_type[0] === 'none') {
          document.getElementById(pieceLocation).innerHTML = ''
        } else {
          const piece = document.createElement('div')
          piece.classList.add(row[square].color + '_' + row[square].piece_type)
          document.getElementById(pieceLocation).innerHTML = ''
          document.getElementById(pieceLocation).appendChild(piece)
        }
      }
    }
  }
}
