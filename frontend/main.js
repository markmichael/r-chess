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

  const gameId = boardInfoNew.response.id
  const currentLocation = event.dataTransfer.getData('text/plain')
    // get new location square. if id is undefined, get parent id
  const newLocation = event.target.id || event.target.parentNode.id
    // check if current location in board info is a pawn and new location is row 1 or 8
    console.log("checking for pawn promotion")
    console.log(boardInfoNew.response.board[currentLocation[0]][currentLocation[1]-1].piece_type[0])
  if (boardInfoNew.response.board[currentLocation[0]][currentLocation[1]-1].piece_type[0] === 'pawn' && (newLocation[1] === '1' || newLocation[1] === '8')) {
    console.log('pawn promotion')
    // show promotion modal
    document.getElementById('promotion').style.display = 'block'
    document.getElementById('promotionTable').style.display = 'block'
    // check for color and show that row of the promotion table
    if (boardInfoNew.response.board[currentLocation[0]][currentLocation[1]-1].color[0] === 'white') {
      document.getElementById('white').style.display = 'block'
      document.getElementById('black').style.display = 'none'
    } else {
      document.getElementById('white').style.display = 'none'
      document.getElementById('black').style.display = 'block'
    }
// wait for user to select which piece to promote to
    document.getElementById('promotionTable').addEventListener('click', (event) => {
      // hide promotion modal
      document.getElementById('promotion').style.display = 'none'
      const promotion = event.target.id
      const boardInfoNew = submitMoveRefreshBoard(gameId, currentLocation, newLocation, promotion)
    })
  }else{
    const boardInfoNew = submitMoveRefreshBoard(gameId, currentLocation, newLocation) 
}
})
})
function submitMoveRefreshBoard(gameId, currentLocation, newLocation, promotion = 'none') {
  console.log('submitMoveRefreshBoard')
  console.log(gameId)
  console.log(currentLocation)
  console.log(newLocation)
  console.log(promotion)
  const boardInfoNew = new XMLHttpRequest()
  boardInfoNew.responseType = 'json'
  boardInfoNew.open('POST', 'http://localhost:8001/movepiece', true)
  boardInfoNew.setRequestHeader('Content-Type', 'application/json')
  boardInfoNew.send(JSON.stringify({ gameId, currentLocation, newLocation,promotion }))
        // wait for response
  boardInfoNew.onload = () => {
    console.log(boardInfoNew.response)
    const cols = boardInfoNew.response.board
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
  
    const historyTable = document.getElementById('historyTable')
    historyTable.innerHTML = ''
    console.log(boardInfoNew.response)
    for (const move in boardInfoNew.response.moves) {
      console.log(move)
      const row = document.createElement('tr')
      const cell = document.createElement('td')
      cell.innerHTML = boardInfoNew.response.moves[move]
      row.appendChild(cell)
      historyTable.appendChild(row)
    }
    }
  return boardInfoNew
  }


const boardInfoNew = loadBoard()
// create divs for each piece on the board
function loadBoard() {
const boardInfoNew = new XMLHttpRequest()
boardInfoNew.responseType = 'json'
boardInfoNew.open('GET', 'http://localhost:8001/newgame', true)
boardInfoNew.send()
boardInfoNew.onload = () => {
  const cols = boardInfoNew.response.board
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
  turn.innerHTML = boardInfoNew.response.turn
  // load check
  const check = document.getElementById('check')
  check.innerHTML = boardInfoNew.response.check
  // load checkmate
  const checkmate = document.getElementById('checkmate')
  checkmate.innerHTML = boardInfoNew.response.checkmate

}
return boardInfoNew
}
// post move after clicking submit
// const submit = document.getElementById('submitMove')
// submit.onclick = () => {
//   const gameId = boardInfo.response.id
//   const currentLocation = document.getElementById('current_location').value
//   const newLocation = document.getElementById('new_location').value
//   const boardInfoNew = new XMLHttpRequest()
//   boardInfoNew.responseType = 'json'
//   boardInfoNew.open('POST', 'http://localhost:8001/movepiece', true)
//   boardInfoNew.setRequestHeader('Content-Type', 'application/json')
//   boardInfoNew.send(JSON.stringify({ gameId, currentLocation, newLocation }))
//   // wait for response
//   boardInfoNew.onload = () => {
//     console.log(boardInfoNew.response)
//     const cols = boardInfoNew.response.board
//     console.log(cols)
//     for (const col in cols) {
//       const row = (cols[col])
//       for (const square in row) {
//         const pieceLocation = (row[square].col + row[square].row)
//         if (row[square].piece_type[0] === 'none') {
//           document.getElementById(pieceLocation).innerHTML = ''
//         } else {
//           const piece = document.createElement('div')
//           piece.classList.add(row[square].color + '_' + row[square].piece_type)
//           piece.draggable = true
//           document.getElementById(pieceLocation).innerHTML = ''
//           document.getElementById(pieceLocation).appendChild(piece)
//         }
//       }
//     }
//   }
// }
