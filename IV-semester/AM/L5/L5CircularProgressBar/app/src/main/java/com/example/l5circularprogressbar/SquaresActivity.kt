package com.example.l5circularprogressbar

import android.os.Bundle
import androidx.appcompat.app.AppCompatActivity

class SquaresActivity : AppCompatActivity() {
    lateinit var squareView: SquareView
    lateinit var progressBar: CircularProgressBar

    companion object {
        var isSquareHit = false
    }
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.squares_activity)
        initWidgets()
        theGame()
    }

    private fun initWidgets() {
        squareView = findViewById(R.id.squareView)
        progressBar = findViewById(R.id.circularProgressSquare)
        progressBar.updateProgressBar(0)
    }

    private fun theGame() {
        Thread {
            while(progressBar.progress != 100) {
                    squareView.moveSquare()
                    Thread.sleep(1000)
                    if (isSquareHit) {
                        progressBar.updateProgressBar(progressBar.progress + 10)
                        isSquareHit = false
                    }
            }
            finish()
        }.start()
    }
}