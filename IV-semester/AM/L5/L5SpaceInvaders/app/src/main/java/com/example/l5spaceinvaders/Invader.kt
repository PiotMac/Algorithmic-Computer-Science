package com.example.l5spaceinvaders
import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.RectF
import java.util.Random


class Invader(context: Context, row: Int, column: Int, screenX: Int, screenY: Int) {
    var rect: RectF = RectF()
    private var generator = Random()

    // the player ship will be represented by a Bitmap
    var bitmap: Bitmap
    var bitmap2: Bitmap

    // how long and high the paddle will be
    val length: Float
    private val height: Float

    // x coordinate of the far left
    var x: Float
        private set

    // y coordinate of the top
    var y: Float
        private set
    private var shipSpeed: Float
    private val LEFT = 1
    private val RIGHT = 2
    private var shipMoving = RIGHT
    var visibility: Boolean

    init {
        length = (screenX / 20).toFloat()
        height = (screenY / 20).toFloat()
        visibility = true
        val padding = screenX / 25
        x = column * (length + padding)
        y = row * (length + padding / 4)

        // initialize the bitmap
        bitmap = BitmapFactory.decodeResource(context.resources, R.drawable.invader1)
        bitmap2 = BitmapFactory.decodeResource(context.resources, R.drawable.invader2)

        // stretch the first bitmap to a size appropriate for the screen resolution
        bitmap = Bitmap.createScaledBitmap(
            bitmap, length.toInt(), height.toInt(),
            false
        )

        // stretch the first bitmap to a size appropriate for the screen resolution
        bitmap2 = Bitmap.createScaledBitmap(
            bitmap2, length.toInt(), height.toInt(),
            false
        )
        shipSpeed = 40f
    }

    fun setInvisible() {
        visibility = false
    }

    fun update(fps: Long) {
        if (shipMoving == LEFT) x -= shipSpeed / fps
        if (shipMoving == RIGHT) x += shipSpeed / fps

        // update rect which is used to detect hits
        rect.top = y
        rect.bottom = y + height
        rect.left = x
        rect.right = x + length
    }

    fun dropDownAndReverse() {
        shipMoving = if (shipMoving == LEFT) RIGHT else LEFT
        y += height
        shipSpeed *= 1.18f
    }

    fun takeAim(playerShipX: Float, playerShipLength: Float): Boolean {
        var randomNumber = -1
        // if invader is near the player
        if (playerShipX + playerShipLength > x && (playerShipX + playerShipLength < x + length) || (playerShipX > x) && (playerShipX < x + length)) {
            // 1 in 150 chance to shoot
            randomNumber = generator.nextInt(150)
            if (randomNumber == 0) return true
        }

        // if firing randomly (not near the player) a 1 in 2000 chance
        randomNumber = generator.nextInt(2000)
        return randomNumber == 0
    }
}