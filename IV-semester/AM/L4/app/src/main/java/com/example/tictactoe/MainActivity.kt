package com.example.tictactoe

import android.annotation.SuppressLint
import android.content.Intent
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowDown
import androidx.compose.material.icons.filled.KeyboardArrowUp
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.toSize
import com.example.tictactoe.ui.theme.TicTacToeTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        //val mContext = Intent(this, TicTacToeScreen::class.java)
        setContent {
            TicTacToeTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colors.background
                ) {
                    //TicTacToeLayout(5)
                    TitleBar()
                }
            }
        }
    }
}

@SuppressLint("UnusedMaterialScaffoldPaddingParameter")
@Composable
fun TitleBar() {
    Scaffold(
        topBar = { TopAppBar(title = { Text("Tic Tac Toe", color = Color.White) },
            backgroundColor = Color(0xff0f9d58)) },
        content = { ChoiceScreen() }
    )
}

@OptIn(ExperimentalMaterialApi::class)
@Composable
fun ChoiceScreen() {
    val mContext = LocalContext.current
    val maxSize = 20
    val minSize = 3
    val sizes = ArrayList<String>()
    var chosenSize = 0

    for (i in minSize..maxSize) {
        sizes.add("$i x $i")
    }

    var expanded by remember { mutableStateOf(false) }
    var selectedSize by remember { mutableStateOf("") }
    var textFieldSize by remember { mutableStateOf(Size.Zero)}
    val icon = if (expanded)
        Icons.Filled.KeyboardArrowUp
    else {
        Icons.Filled.KeyboardArrowDown
    }

    Column {
        Row {
            DropdownMenu(
                expanded = expanded,
                onDismissRequest = { expanded = false }
            ) {
                sizes.forEach { size ->
                    DropdownMenuItem(
                        onClick = {
                            selectedSize = size
                            expanded = false
                        }
                    ) {
                        Text(text = size)
                    }
                }
            }
            OutlinedTextField(
                value = selectedSize,
                onValueChange = { selectedSize = it },
                modifier = Modifier
                    .fillMaxWidth()
                    .onGloballyPositioned { coordinates ->
                        // This value is used to assign to
                        // the DropDown the same width
                        textFieldSize = coordinates.size.toSize()
                    },
                placeholder = {Text("Choose size")},
                trailingIcon = {
                    Icon(icon,"contentDescription",
                        Modifier.clickable { expanded = !expanded })
                }
            )
        }
        Button(
            onClick = {
                if (selectedSize.isNotEmpty()) {
                    if (selectedSize[0].code in 49..57) {
                        chosenSize = selectedSize[0].code - 48
                    }
                    if (selectedSize[1].code in 49..57) {
                        chosenSize = chosenSize * 10 +  selectedSize[1].code - 48
                    }
                }
                val intent = Intent(mContext, TicTacToeScreen::class.java)
                intent.putExtra("selectedSize", chosenSize.toString())
                mContext.startActivity(intent)
            }
        ) {
            Text(text = "Start!")
        }
    }
}

@Preview(showBackground = true)
@Composable
fun DefaultPreview() {
    TicTacToeTheme {
        ChoiceScreen()
    }
}