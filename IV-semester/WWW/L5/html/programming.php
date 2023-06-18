<?php
	require "../session.php";
    require "../nav.php";
    require "../counter_and_comments.php";
    session_start();
    track_session();
?>

<!DOCTYPE html>
<html lang = "en">
<head>
	<meta charset="UTF-8">
	<link href="../css/styles.css" rel="stylesheet" type="text/css"/>
	<title>Programming</title>
	<meta name = "viewport" content = "width = 
			device−width, initial−scale=1.0">
</head>
<body>
    <?php
		create_nav();
	?>
	<main>
    <article>
      <h1>My programming projects</h1>
      <section class="listing">
        <h2>5 most interesting programs:</h2>
        <p>Down below I listed 5 of (in my opinion) most interesting programming projects that I've done in the past. I hope you find them as interesting as I do^^.</p>
        <div class="first_row">
          <div class="check">
            <h3>CHECKERS</h3>
            <p>Last semester I was tasked with creating a fully functional checkers game. The whole project involved two people, me and my colleague. Mainly I was responsible for the server and front-end of the whole applications altough I also took part in creating the whole logic for the game. If u want to know more about my checkers game click this link: <a href="#checkers">see more</a></p>
          </div>
          <div class="minesweeper">
            <h3>MINESWEEPER</h3>
            <p>Not so long ago I had to create my own minesweeper application for mobile devices. To fulfill my task I had to learn about programming language called Kotlin, which is very similar to Java but still has many differences. For more info click this link: <a href="#minesweeper">see more</a></p>
          </div>
          <div class="marie">
            <h3>MARIE SIMULATOR</h3>
            <p>During last semester I was given a task to understand a machine architecture and assembly language of MARIE. In order to prove that I managed to do that I had explain some programs and then write my own. For further information click here: <a href="#marie">see more</a></p>
          </div>
        </div>
        <div class="second_row">
          <div class="monte">
            <h3>MONTE CARLO METHODS</h3>
            <p>My job was to implement Monte Carlo methods, which are a broad class of computational algorithms that rely on repeated random sampling to obtain numerical results. I've managed to do that and using this tool I was able to for example approximate value of number "pi" or give answers to other interesting dilemmas.</p>
          </div>
          <div class="hangman">
            <h3>HANGMAN</h3>
            <p>As a part of my Mobile Applications course I had to create a game of "hangman". It is a simple game but for me it was really enjoyable to write the code and then play my game myself. While writing the code I realised that I am quite fond of developing mobile apps and this subject has gained my attention.</p>
          </div>
        </div>
      </section>
    </article>
    <article>
      <h1 id="checkers">CHECKERS</h1>
      <section>
        <h2>General info</h2>
        <p>Here are listed the most important attributes of my checkers project:</p>
        <ul>
          <li>Programming language: Java</li>
          <li>Architecture: server-client</li>
          <li>Game variants: English, Russian, Thai</li>
        </ul>
      </section>
      <h2>Creating the whole logic</h2>
      <section>
        <h3>Creating board for checkers</h3>
          <pre>
            <code>
public void createBoard() {
        if (buttons[0][0] != null) {
            for (Button[] buttonRow : buttons) {
                for (Button button : buttonRow) {
                    super.remove(button);
                }
            }
        }
        // Tworzenie wszystkich przycisków
        for (int i = 0; i &lt; size; i++) {
            for (int j = 0; j &lt; size; j++) {
                if ((i+j)%2==0) {
                    buttons[i][j] = new Button("");
                    buttons[i][j].setBackground(Color.BLACK);
                } else if (!(CheckersBoard.board[i][j].isTaken())) {
                    buttons[i][j] = new Button("");
                    buttons[i][j].setBackground(Color.RED);
                } else if (CheckersBoard.board[i][j].isTaken() && CheckersBoard.board[i][j].getTeam() == Piece.Team.BLACK) {
                    buttons[i][j] = new Button();
                    buttons[i][j].setLabel("O");
                    //buttons[count] = new Button("\u26C2");
                    buttons[i][j].setForeground(Color.BLACK);
                    buttons[i][j].setBackground(Color.RED);
                } else if (CheckersBoard.board[i][j].isTaken() && CheckersBoard.board[i][j].getTeam() == Piece.Team.WHITE) {
                    buttons[i][j] = new Button("O");
                    //buttons[count] = new Button("\u26C0");
                    buttons[i][j].setForeground(Color.WHITE);
                    buttons[i][j].setBackground(Color.RED);
                }
                buttons[i][j].setSize(300, 300);
                buttons[i][j].setFont(font);
                buttons[i][j].addActionListener(this);
                // W przypadku naciśnięcia na przycisk wysyłają się jego współrzędne
                buttons[i][j].setActionCommand("" + i + " " + j);
                super.add(buttons[i][j]);
            }
        }
        super.setMinimumSize(checkersBoardSize);
        getBoardReady();
}
            </code>
          </pre>
          <h3>An example of logic for King piece</h3>
          <pre>
            <code>
private int[] getCaptureMoves(Square currentSquare, int direction) {
        Square squareBehind = currentSquare.getNeighbours()[direction];
        while (squareBehind!=null) {
            if (squareBehind.isTaken() && squareBehind.getTeam()!=this.getTeam()) {
                Square squareOneBack = squareBehind.getNeighbours()[direction];
                if (squareOneBack != null && !squareOneBack.isTaken()) {
                    return new int[]{squareOneBack.x, squareOneBack.y, 1, this.PieceTypeId};
                } else {
                    return null;
                }
            } else if (squareBehind.isTaken() && squareBehind.getTeam()==this.getTeam()) {
                return null;
            }
            squareBehind = squareBehind.getNeighbours()[direction];
        }
        return null;
    }

    private void getNonCaptures(Square currentSquare, int direction, List&lt;int[]&gt; listToAdd) {
        Square squareBehind = currentSquare.getNeighbours()[direction];
        while (squareBehind!=null && !squareBehind.isTaken()) {
            listToAdd.add(new int[]{squareBehind.x, squareBehind.y, 0, this.PieceTypeId});
            squareBehind = squareBehind.getNeighbours()[direction];
        }
    }

    public List&lt;int[]&gt; checkLegalMoves(boolean functionality) {
        List&lt;int[]&gt; nonCaptureMoves = new ArrayList&lt;&gt;();
        List&lt;int[]&gt; captureMoves = new ArrayList&lt;&gt;();
        if (functionality) {
            //próba znalezienia bicia
            for (int i=0; i&lt;4; i++) {
                if (getCaptureMoves(this.currentSquare,i)!=null) {
                    captureMoves.add(getCaptureMoves(this.currentSquare,i));
                }
            }
            //jeżeli nie udało znaleźć się bicia, dodaj do listy do zwrócenia możliwe ruchy
            if (captureMoves.isEmpty()) {
                for (int i=0; i&lt;4; i++) {
                    getNonCaptures(this.currentSquare, i, nonCaptureMoves);
                }
            }
        } else {
            for (int i = 0; i&lt;4; i++) {
                if (neighbours[i]!=null){
                    if (checkCapture(neighbours[i],i,this.PieceTypeId)!=null) {
                        captureMoves.add(checkCapture(neighbours[i], i, this.PieceTypeId));
                    }
                    else if (!neighbours[i].isTaken()) {
                        nonCaptureMoves.add(new int[] {neighbours[i].getX(), neighbours[i].getY(), 0, this.PieceTypeId});
                    }
                }
            }
        }
        if (captureMoves.size()&gt;0) {
            return captureMoves;
        } else if (nonCaptureMoves.size()&gt;0) {
            return nonCaptureMoves;
        } else {
            return null;
        }
    }
            </code>
          </pre>
          <p>The biggest challenge in my opinion was to define all the functions so well, that multiple variants of the game could be played.</p>
      </section>
      <h2>Creating server-client connection</h2>
      <section>
        <h3>Setting up the server</h3>
        <pre>
          <code>
public static void main(String[] args) {
        // Włączenie serwera
        try {
            serverSocket = new ServerSocket(4445);
        } catch (IOException ex) {
            System.out.println("Server exception: " + ex.getMessage());
            ex.printStackTrace();
        }
        System.out.println("Server is listening on port 4445");
        // Konfiguracja ustawień serwera
        init();
}

    private static void init() {
        while (true) {
            try {
                // Czekanie na pierwszego gracza
                firstClient = serverSocket.accept();
                System.out.println("First client connected");
                // Inicjalizacja pobierania od socketa dla player1
                InputStream inputF = firstClient.getInputStream();
                BufferedReader inF = new BufferedReader(new InputStreamReader(inputF));
                // Inicjalizacja wysyłania do socketa dla player1
                OutputStream outputF = firstClient.getOutputStream();
                PrintWriter outF = new PrintWriter(outputF, true);
                // Wysyłanie ID gracza
                outF.println("1");
                System.out.println("Creating board . . .");
                // Tworzenie planszy na podstawie wyboru pierwszego gracza
                //String choice = inF.readLine();
                type = inF.readLine();
                client = new CheckersClient();
                switch (type) {
                    case "1" -&gt; checkersBoard = new ThaiCheckersBoard();
                    case "2" -&gt; checkersBoard = new EnglishCheckersBoard();
                    case "3" -&gt; checkersBoard = new ShashkiCheckersBoard();
                    default -&gt; throw new IllegalArgumentException();
                }
                board = checkersBoard.getBoard();
                System.out.println("Waiting for choice of game mode . . .");
                String gameMode = inF.readLine();
                switch (gameMode) {
                    case "Single" -&gt; singleMode();
                    case "Multi" -&gt; multiMode();
                    default -&gt; throw new IllegalArgumentException();
                }
                configure();
            }
            catch (IOException ex) {
                System.out.println("Server exception: " + ex.getMessage());
                ex.printStackTrace();
            }
        }

    }

    private static void configure() throws IOException {
        // Inicjalizacja pobierania od socketa dla player2
        InputStream inputS = secondClient.getInputStream();
        BufferedReader inS = new BufferedReader(new InputStreamReader(inputS));
        // Inicjalizacja wysyłania do socketa dla player2
        OutputStream outputS = secondClient.getOutputStream();
        PrintWriter outS = new PrintWriter(outputS, true);
        // Wysyłanie ID gracza
        if (isBotActivated) {
            outS.println("3");
        }
        else {
            outS.println("2");
        }
        // Tworzenie planszy dla drugiego gracza
        outS.println(type);

        playingGame = true;
        System.out.println("Starting the game . . .");
        // Rozpoczęcie rozgrywki
        Checkers checkers = new Checkers(firstClient, secondClient);
        checkersThread = new Thread(checkers);
        checkersThread.start();

        // Dopóki rozgrywana jest gra żaden nowy klient nie może dołączyć do serwera
        while(playingGame) {
            Socket waitingClient = serverSocket.accept();
            OutputStream outputW = waitingClient.getOutputStream();
            PrintWriter outW = new PrintWriter(outputW, true);
            outW.println("Limit of players has been reached!");
            System.out.println("Prevented new player from joining the server!");
            waitingClient.close();
        }
    }
          </code>
        </pre>
        <h3>Connecting client to the server</h3>
        <pre>
          <code>
public void listenSocket() {
        try {
            frame.socket = new Socket("localhost", 4445);
            // Inicjalizacja wysyłania do serwera
            frame.out = new PrintWriter(socket.getOutputStream(), true);
            // Inicjalizacja odbierania z serwera
            frame.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        } catch (UnknownHostException e) {
            System.out.println("Unknown host: localhost");
            System.exit(1);
        } catch (IOException e) {
            System.out.println("No I/O");
            System.exit(1);
        }
}

    private void send(int[] move) {
        // Wysyłanie do serwera
        frame.out.println(move[0] + " " + move[1] + " " + move[2]
         + " " + move[3] + " " + move[4]+ " " + move[5]);
    }

    private void updateMove(int firstX, int firstY, int secondX,
     int secondY, boolean yourMove, boolean successiveCapMode) {
        List&lt;int[]&gt; squaresToUpdate = 
        frame.checkersBoard.updateMove(firstX, firstY,
         secondX, secondY, yourMove, successiveCapMode);
        reprintBoard(squaresToUpdate);
    }

    private void receive() {
        try {
            // Odbieranie z serwera
            str = frame.in.readLine();
            if (str == null) {
                System.out.println("YOU'VE WON!");
                System.exit(0);
            }
            String[] coordinates = str.split(" ");
            // Jeżeli ten gracz zrobił ruch
            if(coordinates.length==1 && Integer.parseInt(coordinates[0])==0 && frame.player == PLAYER1) {
                updateMove(frame.first_click[0], frame.first_click[1],
                 frame.second_click[0], frame.second_click[1], true, successiveCaptureMode);
            } else if (coordinates.length==1 && Integer.parseInt(coordinates[0])==0 && frame.player == BOT) {
                updateMove(botMove[0],botMove[1],botMove[2],botMove[3], true, successiveCaptureMode);
            }
            // Jeżeli przeciwnik zrobił ruch
            else {
                int[] coords = new int[6];
                for (int i = 0; i&lt;coordinates.length; i++) {
                    coords[i] = Integer.parseInt(coordinates[i]);
                }
                if (coords[5]==0) {
                    attemptedMovedPieceType = Piece.PieceType.MAN;
                } else if (coords[5]==1) {
                    attemptedMovedPieceType = Piece.PieceType.KING;
                }
                updateMove(coords[0],coords[1],coords[2],coords[3], false, coords[4]==2);
            }
        } catch (IOException e) {
            System.out.println("Read failed");
            System.exit(1);
        }
    }
          </code>
        </pre>
        <p>The whole communication is based on sending and receiving strings of coordinates that the players make and then checking if they are permitted or not.</p>
      </section>
    </article>
    <article>
      <h1 id="minesweeper">MINESWEEPER</h1>
      <section>
        <h2>General info</h2>
        <p>Here are listed the most important attributes of my minesweeper project:</p>
        <ul>
          <li>Programming language: Kotlin</li>
          <li>Architecture: singleplayer</li>
          <li>Game variants: 9x9 with 10 bombs and no time limit</li>
        </ul>
      </section>
      <h2>Creating class for square in the project</h2>
      <section>
        <h3>Getters and setters</h3>
        <pre>
          <code>
class Square(private val x: Int, private val y: Int) {
    private var unTouched = true
    private var flagSet = false
    private var bombSet = false
    private var nearbyBombs = 0
    private var neighbours = arrayListOf&lt;Square&gt;()

    fun getX() : Int {
        return x
    }

    fun getY() : Int {
        return y
    }

    fun setNeighbours(neighbours : ArrayList&lt;Square&gt;) {
        for (i in 0 until neighbours.size) {
            this.neighbours.add(neighbours[i])
        }
    }

    fun getNeighbours(): ArrayList&lt;Square&gt; {
        return neighbours
    }


    fun setNearbyBombs() {
        for (i in 0 until neighbours.size) {
            if (neighbours[i].isBombSet()) {
                nearbyBombs++
            }
        }
    }

    fun getNearbyBombs(): Int {
        return nearbyBombs
    }

    fun isUntouched() : Boolean {
        return unTouched
    }

    fun setTouched() {
        unTouched = false
    }

    fun isFlagSet(): Boolean {
        return flagSet
    }

    fun isBombSet(): Boolean {
        return bombSet
    }

    fun setFlag() {
        flagSet = true;
    }

    fun resetFlag() {
        flagSet = false;
    }

    fun setBomb() {
        bombSet = true;
    }

    fun resetBomb() {
        bombSet = false;
    }
}
          </code>
        </pre>
        <p>Basically creating the Square class was narrowed to making functions focused on getting and setting some information from this class.</p>
      </section>
      <h2>Creating the whole logic and GUI</h2>
      <section>
        <h3>GUI</h3>
        <pre>
          <code>
private fun createUI() {
        val grid = findViewById&lt;GridLayout&gt;(R.id.grid)
        grid.removeAllViews()
        toRevealTiles.clear()
        goodFlagCount = 0
        flagsSet = numberOfBombs
        findViewById&lt;TextView&gt;(R.id.flagsOnBoard).text = flagsSet.toString()

        val displayMetrics = DisplayMetrics()
        windowManager.defaultDisplay.getMetrics(displayMetrics)
        val height = displayMetrics.heightPixels
        val width = displayMetrics.widthPixels

        grid.columnCount = columns
        grid.rowCount = rows

        for (i in 0 until rows) {
            for (j in 0 until columns) {
                val first = "" + i
                val second = "" + j
                tiles[i][j] = ImageButton(this)
                tiles[i][j]?.setImageDrawable(resources.getDrawable(R.drawable.untouched, applicationContext.theme))
                tiles[i][j]?.id = Integer.parseInt(first + second)
                tiles[i][j]?.setOnClickListener(onClick)
                tiles[i][j]?.setOnLongClickListener(onLongClick)
                tiles[i][j]?.adjustViewBounds = true
                tiles[i][j]?.maxWidth = width / columns
                tiles[i][j]?.maxHeight = height / rows
                tiles[i][j]?.layoutParams?.height = LayoutParams.WRAP_CONTENT
                tiles[i][j]?.layoutParams?.width = LayoutParams.WRAP_CONTENT
                tiles[i][j]?.setPadding(0,0,0,0)
                grid.addView(tiles[i][j])
            }
        }
        setLogic()
}
          </code>
        </pre>
        <h3>Clearing empty tiles</h3>
        <pre>
          <code>
private fun clearAll(square: Square?, id : Int) {
        if (square == null) {
            return
        }
        if (square.isUntouched()) {
            revealTile(square, id)
        }
        if (square.getNearbyBombs() == 0) {
            val neighbours = square.getNeighbours()
            val untouched = arrayListOf&lt;Square&gt;()
            for (i in 0 until neighbours.size) {
                if (neighbours[i].isUntouched()) {
                    untouched.add(neighbours[i])
                }
            }
            for (i in 0 until untouched.size) {
                val x = "" + untouched[i].getX()
                val y = "" + untouched[i].getY()
                val coords = Integer.parseInt(x + y)
                if (untouched[i].getNearbyBombs() > 0) {
                    revealTile(untouched[i], coords)
                }
                else if (untouched[i].isBombSet()) {
                    continue
                }
                else {
                    clearAll(untouched[i], coords)
                }
            }
        }
    }

    private fun revealTile(square: Square?, id: Int) {
        if (square == null) {
            return
        }
        square.setTouched()
        val bombs = square.getNearbyBombs()
        if (bombs == 0){
            if (!square.isBombSet()) {
                findViewById&lt;ImageButton&gt;(id).setImageDrawable
                (resources.getDrawable(R.drawable.empty, applicationContext.theme))
            }
            else {
                findViewById&lt;ImageButton&gt;(id).setImageDrawable
                (resources.getDrawable(R.drawable.bomb, applicationContext.theme))
            }
        }
        else {
            findViewById&lt;ImageButton&gt;(id).setImageDrawable
            (resources.getDrawable(numbers[bombs - 1], applicationContext.theme))
        }
        if (toRevealTiles.contains(square)) {
            toRevealTiles.remove(square)
        }
        if (goodFlagCount == numberOfBombs && toRevealTiles.isEmpty()) {
            Toast.makeText(this, "You've won!", Toast.LENGTH_SHORT).show()
            createUI()
        }
}
          </code>
        </pre>
        <p>I think this part is the most interesting one: it's an algorithm to reveal all empty tiles after clicking on an empty one.</p>
      </section>
    </article>
    <article>
      <h1 id="marie">MARIE SIMULATOR</h1>
      <p><b>DISCLAIMER:</b> If you want to learn more abot MARIE, <a href="https://marie.js.org/about.html">click here</a>.</p>
      <h2>Showing some examples of code</h2>
      <section>
        <h3>Changing upper-case letters to lower-case</h3>
        <pre>
          <code>
        LOAD        str_ptr
                    STORE       tolower_ptr
                    JNS         tolower
                    HALT

tolower_itr,        DEC     0
tolower_ptr,        HEX     0
tolower_idx,        HEX     0
tolower_offset,     HEX     20
tolower,            HEX     0

tolower_while,      LOAD        tolower_ptr
                    ADD         tolower_itr
                    STORE       tolower_idx
                    CLEAR
                    ADDI        tolower_idx
                    SKIPCOND    400
                    JUMP        tolower_do
                    JUMPI       tolower

tolower_do,         ADD         tolower_offset
                    OUTPUT
                    LOAD        tolower_itr
                    ADD         ONE
                    STORE       tolower_itr
                    JUMP        tolower_while

str_ptr,            HEX     18          / memory location of str
str,                HEX     48          / H
                    HEX     45          / E
                    HEX     4C          / L
                    HEX     4C          / L
                    HEX     4F          / O
                    HEX     D           / carriage return
                    HEX     57          / W
                    HEX     4F          / O
                    HEX     52          / R
                    HEX     4C          / L
                    HEX     44          / D
                    HEX     0           / NULL char

/ constants
ONE,                DEC     1
          </code>
        </pre>
        <h3>Printing first 10 Fibbonacci numbers</h3>
        <pre>
          <code>
      Cond,       LOAD        COUNT       / Load count into AC
            SUBT        TEN         / Remove 10 from count
            SKIPCOND    000         / Skipcond 000 if AC &lt; 0
            JUMP        End         / End Loop

Loop,       LOAD        COUNT       / Load count into AC
            ADD         ONE         / Increment Count by 1
            STORE       COUNT       / Store AC in count
            JNS         Fibb
            JUMP        Cond        / Check loop conditions

Fibb,       HEX         000         / Store value for JNS
            CLEAR                   / AC = 0

            / Fi = F1 + F2
            ADD         F1          / AC + F1
            ADD         F2          / AC + F2
            STORE       Fi          / Fi = AC

            / F1 = F2
            LOAD        F2          / AC = F2
            STORE       F1          / F1 = AC

            / F2 = Fi
            LOAD        Fi          / AC = Fi
            STORE       F2          / F2 = AC 

            / Quick Output
            LOAD        Fi          / AC = FI
            OUTPUT                  / Output AC

            JUMPI       Fibb

End,        HALT                    / Halt process

/ variables
COUNT,      DEC         0           / count for loop
Fi,         DEC         0
F1,         DEC         0
F2,         DEC         1

/ constant values
ZERO,       DEC         0
ONE,        DEC         1
TWO,        DEC         2
THREE,      DEC         3
FOUR,       DEC         4
FIVE,       DEC         5
SIX,        DEC         6
SEVEN,      DEC         7
EIGHT,      DEC         8
NINE,       DEC         9
TEN,        DEC         10
          </code>
        </pre>
      </section>
      <h2>My code</h2>
      <section>
        <h3>Program to divide numbers and print result and remainder</h3>
        <pre>
          <code>
/ Division Calculator
/ by Piotr Maciejończyk

/ Prompt user to type in integers
Clear
Store result
Input
Store X
Input
Store Y

check,  Clear
         / check if Y is zero, if it is, then we jump to halt
         Load Y
         Skipcond 400
         Jump ybiggerthanx / false
         Jump halt / true

ybiggerthanx,   Load X
        Subt Y
        Skipcond 000
        Jump loop
        Jump fremainder
                
/ Loop for performing iterative subtraction
loop,    Load X
         Subt Y
     Store X
         
         Clear
         Load result
         Add one
         Store result

     Jump ybiggerthanx

/ Function to calculate the remainder
fremainder,  Load X
       Store remainder
             Clear
             Jump halt

      
halt,    Load result
         Output
         Clear
         Load remainder
     Output
         Halt

X, DEC 0
Y, DEC 0
one, DEC 1
result, DEC 0
remainder, DEC 0
          </code>
        </pre>
        <p>In MARIE there are only operators for addition and substractions, that is why in order to implement dividing numbers, I had to iteratively substract two numbers. With the usage of variable counter, I was able to get the result of such "division" and also the remainder.</p>
      </section>
    </article>
    <hr class = "rounded">
	<?php
		create_footer();
	?>
  </main>
</body>