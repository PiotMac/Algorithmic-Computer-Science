package com.example.l5spaceinvaders
import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.RectF

class PlayerShip(context: Context, screenX: Int, screenY: Int) {
    var rect: RectF = RectF()

    // the player ship will be represented by a Bitmap
    var bitmap: Bitmap
    val length: Float
    private val height: Float

    // x is the far left of the rectangle which forms the ship, y is the top coordinate
    var x: Float
        private set
    private val y: Float
    private val shipSpeed: Float
    val STOPPED = 0
    val LEFT = 1
    val RIGHT = 2

    // ship's movement status and direction
    private var shipMoving = STOPPED
    init {
        // initialize a blank RectF
        length = (screenX / 10).toFloat()
        height = (screenY / 10).toFloat()

        // start ship in roughly the screen centre
        x = (screenX / 2).toFloat()
        y = (screenY - 20).toFloat()
        // initialize the bitmap
        bitmap = BitmapFactory.decodeResource(context.resources, R.drawable.playership)

        // stretch the bitmap to a size appropriate for the screen resolution
        bitmap = Bitmap.createScaledBitmap(bitmap, length.toInt(), height.toInt(), false)

        // how fast is the spaceship in pixels per second
        shipSpeed = 800f
    }

    fun setMovementState(state: Int) {
        shipMoving = state
    }

    fun update(fps: Long) {
        if (shipMoving == LEFT) if (x > length * .5) x -= shipSpeed / fps
        if (shipMoving == RIGHT) if (x < length * 9.5) x += shipSpeed / fps

        // update rect which is used to detect hits
        rect.top = y
        rect.bottom = y + height
        rect.left = x
        rect.right = x + length
        shipMoving = STOPPED
    }
}