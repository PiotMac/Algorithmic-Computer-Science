package com.example.tictactoe

import androidx.lifecycle.ViewModel
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update

class GameViewModel : ViewModel() {
    private val _uiState = MutableStateFlow(GameUiState())
    val uiState: StateFlow<GameUiState> = _uiState.asStateFlow()
    var userX = 0
    var userY = 0

    fun resetGame(width : Int) {
        _uiState.update { currentState ->
            currentState.copy(
                currentSize = width,
                isGameOver = false,
                cells = null,
                turn = TURN.CIRCLE
            )
        }
    }

    fun updateBoard(x: Int, y: Int, turn: TURN) {
        userX = x
        userY = y
        if (_uiState.value.cells?.get(x)?.get(y)?.text != " ") {

        }
        else {
            if (turn == TURN.CIRCLE) {
                //_uiState.value.turn = TURN.CROSS
                _uiState.value.cells?.get(x)?.get(y)?.text = "O"
            }
            else {
                //_uiState.value.turn = TURN.CIRCLE
                _uiState.value.cells?.get(x)?.get(y)?.text = "X"
            }
        }
    }

    fun checkBoard() {
        //Checking rows
        for (i in 0 until _uiState.value.currentSize) {
            if (_uiState.value.turn == TURN.CIRCLE) {
                if (_uiState.value.cells?.get(userX)?.get(i)?.text != "O") {
                    break
                }
                if (i == _uiState.value.currentSize - 1) {
                    //_uiState.value.isGameOver = true
                    _uiState.update { currentState ->
                        currentState.copy(
                            isGameOver = true
                        )
                    }
                }
            }
            else {
                if (_uiState.value.cells?.get(userX)?.get(i)?.text != "X") {
                    break
                }
                if (i == _uiState.value.currentSize - 1) {
                    //_uiState.value.isGameOver = true
                    _uiState.update { currentState ->
                        currentState.copy(
                            isGameOver = true
                        )
                    }
                }
            }
        }
        //Checking columns
        if (!_uiState.value.isGameOver) {
            for (i in 0 until _uiState.value.currentSize) {
                if (_uiState.value.turn == TURN.CIRCLE) {
                    if (_uiState.value.cells?.get(i)?.get(userY)?.text != "0") {
                        break
                    }
                    if (i == _uiState.value.currentSize - 1) {
                        //_uiState.value.isGameOver = true
                        _uiState.update { currentState ->
                            currentState.copy(
                                isGameOver = true
                            )
                        }
                    }
                }
                else {
                    if (_uiState.value.cells?.get(i)?.get(userY)?.text != "X") {
                        break
                    }
                    if (i == _uiState.value.currentSize - 1) {
                        //_uiState.value.isGameOver = true
                        _uiState.update { currentState ->
                            currentState.copy(
                                isGameOver = true
                            )
                        }
                    }
                }
            }
            if (!_uiState.value.isGameOver) {
                //Checking first diagonal
                for (i in 0 until _uiState.value.currentSize) {
                    if (_uiState.value.turn == TURN.CIRCLE) {
                        if (_uiState.value.cells?.get(i)?.get(i)?.text != "0") {
                            break
                        }
                        if (i == _uiState.value.currentSize - 1) {
                            //_uiState.value.isGameOver = true
                            _uiState.update { currentState ->
                                currentState.copy(
                                    isGameOver = true
                                )
                            }
                        }
                    }
                    else {
                        if (_uiState.value.cells?.get(i)?.get(i)?.text != "X") {
                            break
                        }
                        if (i == _uiState.value.currentSize - 1) {
                            //_uiState.value.isGameOver = true
                            _uiState.update { currentState ->
                                currentState.copy(
                                    isGameOver = true
                                )
                            }
                        }
                    }
                }
                if (!_uiState.value.isGameOver) {
                    //Checking second diagonal
                    for (i in 0 until _uiState.value.currentSize) {
                        if (_uiState.value.turn == TURN.CIRCLE) {
                            if (_uiState.value.cells?.get(_uiState.value.currentSize - 1 - i)?.get(i)?.text != "0") {
                                break
                            }
                            if (i == _uiState.value.currentSize - 1) {
                                //_uiState.value.isGameOver = true
                                _uiState.update { currentState ->
                                    currentState.copy(
                                        isGameOver = true
                                    )
                                }
                            }
                        }
                        else {
                            if (_uiState.value.cells?.get(_uiState.value.currentSize - 1 - i)?.get(i)?.text != "X") {
                                break
                            }
                            if (i == _uiState.value.currentSize - 1) {
                                //_uiState.value.isGameOver = true
                                _uiState.update { currentState ->
                                    currentState.copy(
                                        isGameOver = true
                                    )
                                }
                            }
                        }
                    }
                }
            }
        }
        if (!_uiState.value.isGameOver) {
            if (_uiState.value.turn == TURN.CIRCLE) {
                _uiState.value.turn = TURN.CROSS
            } else {
                _uiState.value.turn = TURN.CIRCLE
            }
        }
    }
}