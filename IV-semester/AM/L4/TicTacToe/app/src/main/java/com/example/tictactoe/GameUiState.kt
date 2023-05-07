package com.example.tictactoe

data class GameUiState(
    var turn: TURN = TURN.CIRCLE,
    var isGameOver: Boolean = false,
    val currentSize: Int = 3,
    var cells: Array<Array<Cell?>>? = null
)

fun updateNeighbours(width: Int, cells: Array<Array<Cell?>>) {
    for (i in 0 until width) {
        for (j in 0 until width) {
            if (cells[i][j]?.xx != 0 && cells[i][j]?.xx != width - 1 && cells[i][j]?.yy != 0 && cells[i][j]?.yy != width - 1) {
                cells[i][j]?.neighbours?.add(cells[i - 1][j - 1])
                cells[i][j]?.neighbours?.add(cells[i - 1][j])
                cells[i][j]?.neighbours?.add(cells[i - 1][j + 1])
                cells[i][j]?.neighbours?.add(cells[i][j - 1])
                cells[i][j]?.neighbours?.add(cells[i][j + 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j - 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j])
                cells[i][j]?.neighbours?.add(cells[i + 1][j + 1])
            }
            if (cells[i][j]?.yy == 0 && cells[i][j]?.xx != 0 && cells[i][j]?.xx != width - 1) {
                cells[i][j]?.neighbours?.add(cells[i - 1][j])
                cells[i][j]?.neighbours?.add(cells[i + 1][j])
                cells[i][j]?.neighbours?.add(cells[i - 1][j + 1])
                cells[i][j]?.neighbours?.add(cells[i][j + 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j + 1])
            }
            if (cells[i][j]?.yy == width - 1 && cells[i][j]?.xx != 0 && cells[i][j]?.xx != width - 1) {
                cells[i][j]?.neighbours?.add(cells[i - 1][j])
                cells[i][j]?.neighbours?.add(cells[i + 1][j])
                cells[i][j]?.neighbours?.add(cells[i - 1][j - 1])
                cells[i][j]?.neighbours?.add(cells[i][j - 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j - 1])
            }
            if (cells[i][j]?.xx == 0 && cells[i][j]?.yy != 0 && cells[i][j]?.yy != width - 1) {
                cells[i][j]?.neighbours?.add(cells[i][j + 1])
                cells[i][j]?.neighbours?.add(cells[i][j - 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j - 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j])
                cells[i][j]?.neighbours?.add(cells[i + 1][j + 1])
            }
            if (cells[i][j]?.xx == width - 1 && cells[i][j]?.yy != 0 && cells[i][j]?.yy != width - 1) {
                cells[i][j]?.neighbours?.add(cells[i][j + 1])
                cells[i][j]?.neighbours?.add(cells[i][j - 1])
                cells[i][j]?.neighbours?.add(cells[i - 1][j - 1])
                cells[i][j]?.neighbours?.add(cells[i - 1][j])
                cells[i][j]?.neighbours?.add(cells[i - 1][j + 1])
            }
            if (cells[i][j]?.xx == 0 && cells[i][j]?.yy == 0) {
                cells[i][j]?.neighbours?.add(cells[i + 1][j])
                cells[i][j]?.neighbours?.add(cells[i][j + 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j + 1])
            }
            if (cells[i][j]?.xx == width - 1 && cells[i][j]?.yy == 0) {
                cells[i][j]?.neighbours?.add(cells[i - 1][j])
                cells[i][j]?.neighbours?.add(cells[i][j + 1])
                cells[i][j]?.neighbours?.add(cells[i - 1][j + 1])
            }
            if (cells[i][j]?.xx == 0 && cells[i][j]?.yy == width - 1) {
                cells[i][j]?.neighbours?.add(cells[i + 1][j - 1])
                cells[i][j]?.neighbours?.add(cells[i][j - 1])
                cells[i][j]?.neighbours?.add(cells[i + 1][j])
            }
            if (cells[i][j]?.xx == width - 1 && cells[i][j]?.yy == width - 1) {
                cells[i][j]?.neighbours?.add(cells[i - 1][j - 1])
                cells[i][j]?.neighbours?.add(cells[i][j - 1])
                cells[i][j]?.neighbours?.add(cells[i - 1][j])
            }
        }
    }

}