package com.example.tictactoe

class Cell(x : Int, y : Int) {
    val xx = x
    val yy = y
    var neighbours = arrayListOf<Cell?>()
    var text = " "
}