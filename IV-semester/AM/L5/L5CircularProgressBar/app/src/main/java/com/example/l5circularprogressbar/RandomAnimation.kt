package com.example.l5circularprogressbar

import android.annotation.SuppressLint
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import java.util.*
import kotlin.concurrent.thread

class RandomAnimation : AppCompatActivity() {
    lateinit var circularProgressBar: CircularProgressBar
    lateinit var startButton: Button
    private var isStarted = false
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.random_activity)
        initWidgets()
    }

    private fun initWidgets() {
        circularProgressBar = findViewById(R.id.progressBar)
        circularProgressBar.updateProgressBar(50)
        startButton = findViewById(R.id.startRandom)
    }

    @SuppressLint("SetTextI18n")
    fun onStartClick(view : View) {
        if (!isStarted) {
            isStarted = true
            startButton.text = "Running..."
            val random = Random()
            var add: Boolean

            Thread {
                for (i in 1..1000) {
                    add = random.nextBoolean()
                    if (add && circularProgressBar.progress <= 99) {
                        circularProgressBar.updateProgressBar(circularProgressBar.progress + 1)
                    }
                    else if (!add && circularProgressBar.progress >= 1){
                        circularProgressBar.updateProgressBar(circularProgressBar.progress - 1)
                    }
                    Thread.sleep(0, 100)
                }
                runOnUiThread {
                    startButton.text = "Start!"
                    isStarted = false
                }
            }.start()
        }
        else {
            Toast.makeText(this, "Program is running!", Toast.LENGTH_SHORT).show()
        }
    }
}