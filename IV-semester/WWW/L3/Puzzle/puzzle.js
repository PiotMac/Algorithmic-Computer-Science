        var _stage;
        var _canvas;

        var _img;
        var _pieces;
        var _puzzleWidth;
        var _puzzleHeight;
        var _pieceWidth;
        var _pieceHeight;
        var _currentPiece;
        var _currentDropPiece;  
        var _hoveredPiece;

        var _mouse;

        var columnsInput;
        var rowsInput;
        var imageInput;
        var submitButton;
        var bluePiece;
        var blueAdjacentPieces = [];

        const MAX_WIDTH = 10000;
        const MAX_HEIGHT = 1000;
	  	
        function init(){
            _img = new Image();
            _img.addEventListener('load',onImage);
            _img.src = "myszka.jpg";
            //_img.width = 100%;
            columnsInput = document.getElementById("columns");
            rowsInput = document.getElementById("rows");
            imageInput = document.getElementById("image");
            submitButton = document.getElementById("submit");
        }
        function submitParameters(){
            const columns = columnsInput.value;
            const rows = rowsInput.value;
            if (imageInput.files[0] == null) {

            }
            else {
                _img.src = URL.createObjectURL(imageInput.files[0]);
            }
            onImage();
        }
        function onImage(){
            /*
            var width = _img.width;
            var height = _img.height;
            if (width > height) {
                if (width > MAX_WIDTH) {
                    height = height * (MAX_WIDTH / width);
                    width = MAX_WIDTH;
                }
            }
            else {
                if (height > MAX_HEIGHT) {
                    width = width * (MAX_HEIGHT / height);
                    height = MAX_HEIGHT;
                }
            }
            */
            _pieceWidth = Math.floor(_img.width / columnsInput.value);
            _pieceHeight = Math.floor(_img.height / rowsInput.value);
            //_pieceWidth = Math.floor(width / columnsInput.value);
            //_pieceHeight = Math.floor(height / rowsInput.value);
            _puzzleWidth = _pieceWidth * columnsInput.value;
            _puzzleHeight = _pieceHeight * rowsInput.value;
            setCanvas();
            initPuzzle();
        }
        function setCanvas(){
            _canvas = document.getElementById('canvas');
            _stage = _canvas.getContext('2d');
            _canvas.width = _puzzleWidth;
            _canvas.height = _puzzleHeight;
            _canvas.style.border = "1px solid black";
        }
        function initPuzzle(){
            _pieces = [];
            _mouse = {x:0,y:0};
            _currentPiece = null;
            _currentDropPiece = null;
            _stage.drawImage(_img, 0, 0, _puzzleWidth, _puzzleHeight, 0, 0, _puzzleWidth, _puzzleHeight);
            createTitle("Click to Start Puzzle");
            buildPieces();
        }
        function createTitle(msg){
            _stage.fillStyle = "#808080";
            _stage.globalAlpha = .4;
            _stage.fillRect(100,_puzzleHeight - 40,_puzzleWidth - 200,40);
            _stage.fillStyle = "#FFFFFF";
            _stage.globalAlpha = 1;
            _stage.textAlign = "center";
            _stage.textBaseline = "middle";
            _stage.font = "20px Arial";
            _stage.fillText(msg,_puzzleWidth / 2,_puzzleHeight - 20);
        }
        function buildPieces(){
            var i;
            var piece;
            var xPos = 0;
            var yPos = 0;
            for(i = 0; i < columnsInput.value * rowsInput.value; i++){
                piece = {};
                piece.sx = xPos;
                piece.sy = yPos;
                _pieces.push(piece);
                xPos += _pieceWidth;
                if(xPos >= _puzzleWidth){
                    xPos = 0;
                    yPos += _pieceHeight;
                }
            }
            document.onmousedown = shufflePuzzle;
        }
        function shufflePuzzle(){
            _pieces = shuffleArray(_pieces);
            //isSolvable(_pieces);
            _stage.clearRect(0,0,_puzzleWidth,_puzzleHeight);
            var i;
            var piece;
            var xPos = 0;
            var yPos = 0;
            for(i = 0;i < _pieces.length;i++){
                piece = _pieces[i];
                piece.xPos = xPos;
                piece.yPos = yPos;
                if (i == 0) {
                    bluePiece = _pieces[i];
                    _stage.fillStyle = "blue";
                    _stage.fillRect(bluePiece.xPos, bluePiece.yPos, _pieceWidth, _pieceHeight);
                    blueAdjacentPieces.push(_pieces[1]);
                    if (columnsInput.value == 1 || rowsInput.value == 1) {
                        
                    }
                    else {
                        blueAdjacentPieces.push(_pieces[columnsInput.value]);
                    }

                }
                else {
                    if (blueAdjacentPieces.includes(piece)) {
                        _stage.globalAlpha = 1.0;
                    }
                    else {
                        _stage.globalAlpha = 0.5;
                    }
                    _stage.drawImage(_img, piece.sx, piece.sy, _pieceWidth, _pieceHeight, xPos, yPos, _pieceWidth, _pieceHeight);
                }
                _stage.strokeRect(xPos, yPos, _pieceWidth,_pieceHeight);
                xPos += _pieceWidth;
                if(xPos >= _puzzleWidth){
                    xPos = 0;
                    yPos += _pieceHeight;
                }
            }
            document.onmousemove = onPuzzleHover;
            document.onmousedown = onPuzzleClick;
        }
        
        function onPuzzleHover(e){
            if(e.layerX || e.layerX == 0){
                _mouse.x = e.layerX - _canvas.offsetLeft;
                _mouse.y = e.layerY - _canvas.offsetTop;
            }
            else if(e.offsetX || e.offsetX == 0){
                _mouse.x = e.offsetX - _canvas.offsetLeft;
                _mouse.y = e.offsetY - _canvas.offsetTop;
            }

            _hoveredPiece = checkPieceHovered();
            if (_hoveredPiece != null) {
                //_stage.clearRect(_hoveredPiece.xPos, _hoveredPiece.yPos, _pieceWidth, _pieceHeight);
                _stage.strokeStyle = "red";
                _stage.strokeRect(_hoveredPiece.xPos, _hoveredPiece.yPos, _pieceWidth, _pieceHeight)
            }
            else {
                var i;
                var piece;
                _stage.strokeStyle = "black";
                for(i = 0; i < _pieces.length; i++){
                    piece = _pieces[i];
                    _stage.strokeRect(piece.xPos, piece.yPos, _pieceWidth, _pieceHeight);
                }    
            }
        }
        /*
        function isSolvable() {
            var inversionsCount = 0;
            var i;
            var j;
            for (i = 0; i < columnsInput.value * rowsInput.value - 1; i++) {
                for (j = i + 1; j < columnsInput.value * rowsInput.value; j++) {
                    if (_pieces[i].sy == _pieces[j].sy) {
                        if (_pieces[i].sx )
                    }
                }
            }
        }
        */
        function checkPieceHovered(){
            var i;
            var piece;
            for(i = 0; i < _pieces.length; i++){
                piece = _pieces[i];
                if(_mouse.x < piece.xPos || _mouse.x > (piece.xPos + _pieceWidth) || _mouse.y < piece.yPos || _mouse.y > (piece.yPos + _pieceHeight)){
                    //PIECE NOT HIT
                }
                else if (!blueAdjacentPieces.includes(piece)){
                    //Piece not adjacent to blue piece
                }
                else{
                    return piece;
                }
            }
            return null;
        }
        function onPuzzleClick(e){
            if(e.layerX || e.layerX == 0){
                _mouse.x = e.layerX - _canvas.offsetLeft;
                _mouse.y = e.layerY - _canvas.offsetTop;
            }
            else if(e.offsetX || e.offsetX == 0){
                _mouse.x = e.offsetX - _canvas.offsetLeft;
                _mouse.y = e.offsetY - _canvas.offsetTop;
            }
            _currentPiece = checkPieceClicked();
            if(_currentPiece != null){
                var tmp = {xPos:_currentPiece.xPos,yPos:_currentPiece.yPos};
                _currentPiece.xPos = bluePiece.xPos;
                _currentPiece.yPos = bluePiece.yPos;
                bluePiece.xPos = tmp.xPos;
                bluePiece.yPos = tmp.yPos;
                
                addNeighbours();
                console.log(blueAdjacentPieces.length);
                for(i = 0; i < _pieces.length; i++){
                    if (_pieces[i].xPos == bluePiece.xPos && _pieces[i].yPos == bluePiece.yPos) {
                        _stage.clearRect(_pieces[i].xPos, _pieces[i].yPos, _pieceWidth, _pieceHeight);
                        _stage.globalAlpha = 1.0;
                        _stage.fillStyle = "blue";
                        _stage.fillRect(_pieces[i].xPos, _pieces[i].yPos, _pieceWidth, _pieceHeight);
                        continue;
                    }
                    if (blueAdjacentPieces.includes(_pieces[i])) {
                        _stage.globalAlpha = 1.0;
                    }
                    else {
                        _stage.globalAlpha = 0.5;
                    }
                    _stage.clearRect(_pieces[i].xPos, _pieces[i].yPos, _pieceWidth, _pieceHeight);
                    _stage.drawImage(_img, _pieces[i].sx, _pieces[i].sy, _pieceWidth, _pieceHeight, _pieces[i].xPos, _pieces[i].yPos, _pieceWidth, _pieceHeight);
                    _stage.strokeStyle = "black";
                    _stage.strokeRect(_pieces[i].xPos, _pieces[i].yPos, _pieceWidth, _pieceHeight);
                }
                checkWin();
            }
        }
        function checkPieceClicked(){
            var i;
            var piece;
            for(i = 0;i < _pieces.length;i++){
                piece = _pieces[i];
                if(_mouse.x < piece.xPos || _mouse.x > (piece.xPos + _pieceWidth) || _mouse.y < piece.yPos || _mouse.y > (piece.yPos + _pieceHeight)){
                    //PIECE NOT HIT
                }
                else if (!blueAdjacentPieces.includes(piece)){
                    //Piece not adjacent to blue piece
                }
                else{
                    return piece;
                }
            }
            return null;
        }
        function addNeighbours(){
            var i;
            var piece;
            blueAdjacentPieces.length = 0;
            var top = false;
            var bottom = false;
            var left = false;
            var right = false;
            if (bluePiece.yPos - _pieceHeight >= 0) {
                top = true;
            }
            if (bluePiece.yPos + _pieceHeight <=_puzzleHeight) {
                bottom = true;
            }
            if (bluePiece.xPos - _pieceWidth >= 0) {
                left = true;
            }
            if (bluePiece.xPos + _pieceWidth <= _puzzleWidth) {
                right = true;
            }
            for(i = 0;i < _pieces.length;i++){
                piece = _pieces[i];
                if (piece.xPos == bluePiece.xPos && piece.yPos == bluePiece.yPos) {
                    continue;
                }
                if (top === true && piece.yPos === (bluePiece.yPos - _pieceHeight) && piece.xPos === bluePiece.xPos) {
                    blueAdjacentPieces.push(piece);
                }
                else if (bottom === true && piece.yPos === (bluePiece.yPos + _pieceHeight) && piece.xPos === bluePiece.xPos) {
                    blueAdjacentPieces.push(piece);
                }
                else if (left === true && piece.xPos === (bluePiece.xPos - _pieceWidth) && piece.yPos === bluePiece.yPos) {
                    blueAdjacentPieces.push(piece);
                }
                else if (right === true && piece.xPos === (bluePiece.xPos + _pieceWidth) && piece.yPos === bluePiece.yPos) {
                    blueAdjacentPieces.push(piece);
                }
            }
        }
        function checkWin(){
            var gameWin = true;
            for(i = 0;i < _pieces.length;i++){
                if(_pieces[i].xPos != _pieces[i].sx || _pieces[i].yPos != _pieces[i].sy){
                    gameWin = false;
                }
            }
            if(gameWin){
                setTimeout(gameOver,500);
            }
        }
        function gameOver(){
            document.onmousedown = null;
            document.onmousemove = null;
            initPuzzle();
        }
        
        function shuffleArray(o){
            for(var j, x, i = o.length; i; j = parseInt(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
            return o;
        }

        function resizeImage(imgToResize, resizingFactor = 0.5) {
            //const canvas = document.createElement("canvas");
            const context = _canvas.getContext("2d");

            const originalWidth = _img.width;
            const originalHeight = _img.height;

            const canvasWidth = originalWidth * resizingFactor;
            const canvasHeight = originalHeight * resizingFactor;

            _canvas.width = canvasWidth;
            _canvas.height = canvasHeight;

            context.drawImage(_img, 0, 0, originalWidth * resizingFactor, originalHeight * resizingFactor);
            return canvas.toDataURL();
        }
