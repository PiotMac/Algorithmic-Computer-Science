package com.example.tictactoe

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.ui.Modifier
import com.example.tictactoe.ui.theme.TicTacToeTheme

enum class TURN {
    CIRCLE,
    CROSS
}

var turn = TURN.CIRCLE

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        turn = TURN.CIRCLE
        setContent {
            TicTacToeTheme() {
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colorScheme.background
                ) {
                    GameScreen(3, this)
                }
            }
        }
    }
}