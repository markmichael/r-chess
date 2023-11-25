// make squares allow drops
document.querySelectorAll('.square').forEach((square) => {
  square.addEventListener('dragover', (event) => {
    event.preventDefault()
    square.classList.add('highlight')
  })
  square.addEventListener('dragleave', (event) => {
    square.classList.remove('highlight')
  })
  square.allowDrop = true
  square.addEventListener('drop', (event) => {
    event.preventDefault()
    square.classList.remove('highlight')
    console.log(event.dataTransfer.getData('text/plain'))
    console.log(event.target.id)
    console.log(event)

  const gameId = boardInfo.response.id
  const currentLocation = event.dataTransfer.getData('text/plain')
    // get new location square. if id is undefined, get parent id
  const newLocation = event.target.id || event.target.parentNode.id
    console.log(newLocation)
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
          piece.draggable = true
          piece.allowDrop = false
        piece.addEventListener('dragstart', (event) => {
          // datatransfer parent node id
          event.preventDefault = true
          event.dataTransfer.setData('text/plain', event.target.parentNode.id)
          event.target.classList.add('dragging')
        })
        piece.addEventListener('dragend', (event) => {
          event.target.classList.remove('dragging')
        })
          document.getElementById(pieceLocation).innerHTML = ''
          document.getElementById(pieceLocation).appendChild(piece)
        }
      }
    }
  // load turn
  const turn = document.getElementById('turn')
  turn.innerHTML = boardInfoNew.response.turn
  // load check
  const check = document.getElementById('check')
  check.innerHTML = boardInfoNew.response.check
  // load checkmate
  const checkmate = document.getElementById('checkmate')
  checkmate.innerHTML = boardInfoNew.response.checkmate
  }
  })
})

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
        // set piece as draggable
        piece.draggable = true
        piece.allowDrop = false
        // register drag event listeners
        piece.addEventListener('dragstart', (event) => {
          event.preventDefault = true
          // datatransfer parent node id
          event.dataTransfer.setData('text/plain', event.target.parentNode.id)
          event.target.classList.add('dragging')

        })
        piece.addEventListener('dragend', (event) => {
          event.target.classList.remove('dragging')
        })

        document.getElementById(pieceLocation).appendChild(piece)
      }
    }
  }
  // load turn
  const turn = document.getElementById('turn')
  turn.innerHTML = boardInfo.response.turn
  // load check
  const check = document.getElementById('check')
  check.innerHTML = boardInfo.response.check
  // load checkmate
  const checkmate = document.getElementById('checkmate')
  checkmate.innerHTML = boardInfo.response.checkmate

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
          piece.draggable = true
          document.getElementById(pieceLocation).innerHTML = ''
          document.getElementById(pieceLocation).appendChild(piece)
        }
      }
    }
  }
}
