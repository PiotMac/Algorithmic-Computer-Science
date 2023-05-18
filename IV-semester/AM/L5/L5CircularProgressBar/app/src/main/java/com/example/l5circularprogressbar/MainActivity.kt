package com.example.l5circularprogressbar

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.view.View
import android.widget.Button

class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
    }

    fun startRandomProgress(view: View) {
        val intent = Intent(this, RandomAnimation::class.java)
        startActivity(intent)
    }

    fun startPongGame(view: View) {
        val intent = Intent(this, PongActivity::class.java)
        startActivity(intent)
    }

    fun startSquaresGame(view: View) {
        val intent = Intent(this, SquaresActivity::class.java)
        startActivity(intent)
    }
}