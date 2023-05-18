package com.example.l5circularprogressbar

import android.os.Bundle
import android.util.Log
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity

class PongActivity : AppCompatActivity() {
    lateinit var circularProgressBar: CircularProgressBar
    companion object {
        var wasPaddleHit = false
    }
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.pong_activity)
        initWidgets()
        startGame()
    }

    private fun initWidgets() {
        circularProgressBar = findViewById(R.id.progressBarPong)
        circularProgressBar.updateProgressBar(0)
    }

    private fun startGame() {
        var i = 0
        Thread {
            while (circularProgressBar.progress != 100) {
                i++
                Log.i("iter", "$i")
                runOnUiThread {
                    if (wasPaddleHit) {
                        circularProgressBar.updateProgressBar(circularProgressBar.progress + 10)
                        wasPaddleHit = false
                    }
                }
            }
            finish()
        }.start()
    }
}