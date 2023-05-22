package com.example.l5spaceinvaders

import androidx.room.Entity
import androidx.room.PrimaryKey
import java.io.Serializable

@Entity
data class Game(
    var score: Int,
    var date: String,
    var won: Boolean
) : Serializable {
    @PrimaryKey(autoGenerate = true) var id: Long = 0
}