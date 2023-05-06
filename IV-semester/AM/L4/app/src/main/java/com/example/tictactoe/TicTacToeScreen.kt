package com.example.tictactoe

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Outline
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.example.tictactoe.ui.theme.TicTacToeTheme
import kotlin.random.Random

class TicTacToeScreen : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val selectedSize : Int? = intent.getStringExtra("selectedSize")?.toInt()
        setContent {
            TicTacToeTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colors.background
                ) {
                    if (selectedSize != null) {
                        TicTacToeLayout(selectedSize)
                    }
                    Text(text = "" + selectedSize)
                }
            }
        }
    }
}

@Composable
fun TicTacToeLayout(selectedSize : Int) {
    val cells = arrayOfNulls<Int>(selectedSize * selectedSize)
    LazyVerticalGrid(
        columns = GridCells.Fixed(selectedSize)
    ) {
        items(cells) { item ->
            Card(
                //modifier = Modifier.padding(4.dp),
                //backgroundColor = Color(
                //    red = Random.nextInt(0, 255),
               //     green = Random.nextInt(0, 255),
                //    blue = Random.nextInt(0, 255)
                //),
                backgroundColor = Color.White,
                border = BorderStroke(1.dp, Color.Black)
            ) {
                Text(
                    text = "O",
                    color = Color.Black,
                    //fontSize = 42.sp,
                    textAlign = TextAlign.Center,
                    //modifier = Modifier.padding(24.dp)
                )
            }
        }
    }
}