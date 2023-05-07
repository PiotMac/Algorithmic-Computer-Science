@file:OptIn(ExperimentalMaterial3Api::class)
package com.example.tictactoe

import android.app.Activity
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.setContent
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel


@Composable
fun GameScreen(width: Int, activity: MainActivity, gameViewModel: GameViewModel = viewModel()) {
    val gameUiState by gameViewModel.uiState.collectAsState()
    val context = LocalContext.current
    val options = mutableListOf<String>()
    for (i in 3..20) {
        options += String.format("$i x $i")
    }
    var expanded by remember { mutableStateOf(false) }
    var selectedText by remember { mutableStateOf(options[width - 3]) }

    Column {
        Row(
            horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically
        ) {
            ExposedDropdownMenuBox(
                expanded = expanded,
                onExpandedChange = {
                    expanded = !expanded
                }
            ) {
                TextField(
                    value = selectedText,
                    onValueChange = {},
                    readOnly = true,
                    trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded) },
                    modifier = Modifier.menuAnchor()
                )
                ExposedDropdownMenu(
                    expanded = expanded,
                    onDismissRequest = { expanded = false }
                ) {
                    options.forEach { item ->
                        DropdownMenuItem(
                            text = { Text(text = item) },
                            onClick = {
                                selectedText = item
                                expanded = false
                                Toast.makeText(context, item, Toast.LENGTH_SHORT).show()
                            }
                        )
                    }
                }
            }
            Button(
                onClick = {
                    val w = options.indexOf(selectedText) + 3
                    gameViewModel.resetGame(w)
                    activity.setContent {
                        GameScreen(width = w, activity = activity)
                    }
                },
                modifier = Modifier.padding(10.dp)
            ) {
                Text("New Game")
            }
        }
        GameLayout(width = width, gameUiState = gameUiState, gameViewModel = gameViewModel)
        if (gameUiState.isGameOver) {
            FinalScoreDialog(
                turn = gameUiState.turn,
                onPlayAgain = {
                    gameViewModel.resetGame(gameUiState.currentSize)
                    activity.setContent {
                        GameScreen(width = width, activity = activity)
                    }
                }
            )
        }
    }
}

@Composable
fun GameLayout(
    width : Int,
    gameUiState: GameUiState,
    gameViewModel: GameViewModel
) {
    val configuration = LocalConfiguration.current
    val screenWidth = configuration.screenWidthDp
    val size = (screenWidth / width)
    //val cells = ArrayList<ArrayList<Cell>>()
    val cells = Array(width) { arrayOfNulls<Cell>(width) }

    Column {
        for (x in 0 until width){
            Row {
                for (y in 0 until width){
                    GameSquare(
                        x = x,
                        y = y,
                        s = size,
                        gameUiState = gameUiState,
                        gameViewModel = gameViewModel
                        //onUserClick = { gameViewModel.updateBoard(x, y, gameUiState.turn) },
                        //userClick = gameViewModel.userGuess
                    )
                    cells[x][y] = Cell(x, y)
                }
            }
        }
    }
    gameUiState.cells = cells
    updateNeighbours(width, gameUiState.cells!!)
}

/*
 * Creates and shows an AlertDialog with final score.
 */

@Composable
private fun FinalScoreDialog(
    turn: TURN,
    onPlayAgain: () -> Unit,
    modifier: Modifier = Modifier
) {
    var score = ""
    val activity = (LocalContext.current as Activity)
    if (turn == TURN.CIRCLE) {
        score = "Circle has won!"
    }
    else {
        score = "Cross has won!"
    }

    AlertDialog(
        onDismissRequest = {

        },
        title = { Text(text = "Congratulations!") },
        text = { Text(text =  score) },
        modifier = modifier,
        dismissButton = {
            TextButton(
                onClick = {
                    activity.finish()
                }
            ) {
                Text(text = "Exit")
            }
        },
        confirmButton = {
            TextButton(onClick = onPlayAgain) {
                Text(text = "Play again!")
            }
        }
    )
}


@Composable
fun GameSquare(
    x: Int,
    y: Int,
    s: Int,
    gameUiState: GameUiState,
    gameViewModel: GameViewModel
) {
    var state by remember { mutableStateOf(" ") }
    Box(
        contentAlignment = Alignment.Center,
        modifier = Modifier
            .background(Color.DarkGray)
            .size(s.dp)
            .border(width = 1.dp, color = Color.White)
            .clickable {
                if (state != " ") {

                } else {
                    val res = gameUiState.turn
                    if (res == TURN.CROSS) {
                        state = "X"
                    } else if (res == TURN.CIRCLE) {
                        state = "O"
                    }
                    gameViewModel.updateBoard(x, y, gameUiState.turn)
                    gameViewModel.checkBoard()
                    if (gameUiState.isGameOver) {
                        Log.i("check", "WIN!")
                    }
                }
            }
    ) {
        Text(
            text = state,
            color = Color.White,
        )
    }
}