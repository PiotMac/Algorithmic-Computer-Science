package com.example.l5spaceinvaders

import android.graphics.RectF

class Bullet(screenY: Int) {
    private var x = 0f
    private var y = 0f
    val rect: RectF

    // direction
    val UP = 0
    private val DOWN = 1

    // headed nowhere
    private var heading = -1
    private var speed = 800f
    private val width = 5
    private val height: Int
    var status: Boolean
        private set

    init {
        height = screenY / 30
        status = false
        rect = RectF()
    }

    fun setInactive() {
        status = false
    }

    val impactPointY: Float
        get() = if (heading == DOWN) y + height else y

    fun shoot(startX: Float, startY: Float, direction: Int): Boolean {
        if (!status) {
            x = startX
            y = startY
            heading = direction
            status = true
            return true
        }
        return false
    }

    fun update(fps: Long) {

        // just move up or down
        y = if (heading == UP) y - speed / fps else y + speed / fps
        // update rect
        rect.left = x
        rect.right = x + width
        rect.top = y
        rect.bottom = y + height
    }
}