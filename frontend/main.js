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
  const currentLocation = document.getElementById('current_location').value
  const newLocation = document.getElementById('new_location').value
  const xhr = new XMLHttpRequest()
  xhr.responseType = 'json'
  xhr.open('POST', 'http://localhost:8001/movepiece', true)
  xhr.setRequestHeader('Content-Type', 'application/json')
  xhr.send(JSON.stringify({ currentLocation, newLocation }))
}
