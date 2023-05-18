package com.example.l5circularprogressbar

import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.util.AttributeSet
import android.view.MotionEvent
import android.view.View

class PongView(context: Context, attrs: AttributeSet) : View(context, attrs) {
    private val paddlePaint = Paint()
    private val ballPaint = Paint()

    private var paddleX: Float = 0f
    private var paddleY: Float = 0f

    private var ballX: Float = 0f
    private var ballY: Float = 0f

    private var ballRadius: Float = 60f
    private var ballSpeedX: Float = 15f
    private var ballSpeedY: Float = 15f

    private var screenWidth: Int = 0
    private var screenHeight: Int = 0

    private var holdFlag = false

    init {
        paddlePaint.color = Color.rgb(255,140,0)//Color.WHITE
        ballPaint.color = Color.rgb(255,140,0)//Color.WHITE
    }

    override fun onDraw(canvas: Canvas) {
        super.onDraw(canvas)

        //canvas.drawColor(Color.BLACK)

        // Draw the paddle
        canvas.drawRect(paddleX, paddleY, paddleX + 400f, paddleY + 20f, paddlePaint)

        // Draw the ball
        canvas.drawCircle(ballX, ballY, ballRadius, ballPaint)

        // Move the ball
        ballX += ballSpeedX
        ballY += ballSpeedY

        // Check for collision with walls
        if (ballX + ballRadius > screenWidth || ballX - ballRadius < 0) {
            ballSpeedX *= -1
        }

        if (ballY + ballRadius > screenHeight || ballY - ballRadius < 0) {
            ballSpeedY *= -1
        }

        // Check for collision with paddle
        if (ballY + ballRadius > paddleY && ballX > paddleX && ballX < paddleX + 400f) {
            PongActivity.wasPaddleHit = true
            ballSpeedY *= -1
            ballY = paddleY - ballRadius - 1f
        }

        // Invalidate the view to trigger a redraw
        invalidate()
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        screenWidth = w
        screenHeight = h

        // Set initial positions of paddle and ball
        paddleX = (screenWidth / 2) - 200f
        paddleY = (screenHeight - 200).toFloat()
        ballX = screenWidth / 2f
        ballY = screenHeight / 2f
    }

    override fun onTouchEvent(event: MotionEvent): Boolean {
        if (event.action == MotionEvent.ACTION_DOWN) {
            holdFlag = true
        }
        else if (event.action == MotionEvent.ACTION_UP) {
            holdFlag = false
        }

        if (holdFlag) {
            paddleX = event.x - 200f // Adjust paddle position to center it on touch
            if (paddleX < 0) paddleX = 0f
            if (paddleX + 400f > screenWidth) paddleX = (screenWidth - 400).toFloat()
        }

        return true
    }
}
