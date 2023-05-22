package com.example.l5spaceinvaders

import android.graphics.RectF

class DefenceBrick(row: Int, column: Int, shelterNumber: Int, screenX: Int, screenY: Int) {
    val rect: RectF
    var visibility: Boolean
        private set

    init {
        val width = screenX / 90
        val height = screenY / 40
        visibility = true
        val brickPadding = 1
        val shelterPadding = screenX / 9
        val startHeight = screenY - screenY / 8 * 2

        // what the brick will look like
        rect = RectF(
            (column * width + brickPadding + shelterPadding * shelterNumber + shelterPadding + shelterPadding * shelterNumber).toFloat(),
            (row * height + brickPadding + startHeight).toFloat(),
            (column * width + width - brickPadding + shelterPadding * shelterNumber + shelterPadding + shelterPadding * shelterNumber).toFloat(),
            (row * height + height - brickPadding + startHeight).toFloat())
    }

    fun setInvisible() {
        visibility = false
    }
}