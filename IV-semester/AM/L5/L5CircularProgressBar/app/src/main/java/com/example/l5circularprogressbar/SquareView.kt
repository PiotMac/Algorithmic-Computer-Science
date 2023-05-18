package com.example.l5circularprogressbar

import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.util.AttributeSet
import android.view.MotionEvent
import android.view.View
import kotlin.random.Random

class SquareView(context: Context, attrs: AttributeSet) : View(context, attrs) {

    private val squarePaint = Paint()

    private var squareX: Float = 0f
    private var squareY: Float = 0f
    private var squareSize: Float = 200f

    private var isSquareTouched = false

    init {
        squarePaint.color = Color.RED
    }

    override fun onDraw(canvas: Canvas) {
        super.onDraw(canvas)

        // Draw the square
        canvas.drawRect(squareX, squareY, squareX + squareSize, squareY + squareSize, squarePaint)
    }

    override fun onTouchEvent(event: MotionEvent): Boolean {
        val touchX = event.x
        val touchY = event.y
        if (event.action == MotionEvent.ACTION_DOWN) {
            isSquareTouched = touchX >= squareX && touchX <= squareX + squareSize &&
                    touchY >= squareY && touchY <= squareY + squareSize
            if (isSquareTouched) {
                SquaresActivity.isSquareHit = true
            }
        }
        return true
    }

    fun moveSquare() {
        val screenWidth = width.toFloat()
        val screenHeight = height.toFloat()

        // Generate random position for the square
        squareX = Random.nextFloat() * (screenWidth - squareSize)
        squareY = Random.nextFloat() * (screenHeight - squareSize)

        // Invalidate the view to trigger a redraw
        invalidate()
    }
}
