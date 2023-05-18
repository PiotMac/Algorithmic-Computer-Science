package com.example.l5circularprogressbar

import android.annotation.SuppressLint
import android.content.Context
import android.graphics.*
import android.util.AttributeSet
import android.view.View

class CircularProgressBar(context: Context, attrs: AttributeSet) : View(context, attrs) {
    var progress = 0
    private var progressColor = Color.GREEN
    private var backgroundColor = Color.GRAY

    private val paint = Paint(Paint.ANTI_ALIAS_FLAG)
    private val paintText = Paint(Paint.ANTI_ALIAS_FLAG)
    private val paintBackground = Paint(Paint.ANTI_ALIAS_FLAG)
    private val rectF = RectF()
    private var percentageText = ""
    private val rect = Rect()

    init {
        paint.color = progressColor
        paint.style = Paint.Style.FILL
        paintText.color = Color.WHITE
        paintText.textSize = 80f
        paintText.textAlign = Paint.Align.CENTER
        paintBackground.color = Color.BLACK
    }

    @SuppressLint("DrawAllocation")
    override fun onDraw(canvas: Canvas) {
        super.onDraw(canvas)

        val centerX = width / 2f
        val centerY = height / 2f
        val radius = (width / 2f) - (paint.strokeWidth / 2f)

        // Draw progress
        rectF.set(centerX - radius, centerY - radius, centerX + radius, centerY + radius)
        canvas.drawArc(rectF, -90f, (progress / 100f) * 360f, true, paint)

        // Draw background
        canvas.drawCircle(centerX, centerY, radius - 100f, paintBackground)

        // Draw percentage
        percentageText = "$progress%"
        paintText.getTextBounds(percentageText, 0, percentageText.length, rect)
        canvas.drawText("$progress%", centerX, centerY, paintText)
    }

    fun updateProgressBar(progress: Int) {
        this.progress = progress
        this.percentageText = "${this.progress}%"
        invalidate()
    }
}