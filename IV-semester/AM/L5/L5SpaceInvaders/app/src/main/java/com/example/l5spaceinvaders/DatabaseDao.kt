package com.example.l5spaceinvaders

import androidx.room.*
import java.util.Date

@Dao
interface DatabaseDao {
    @Query("SELECT * FROM Game")
    fun selectAllGames() : List<Game>

    @Query("SELECT * FROM Game WHERE won = :isWon")
    fun selectWinsOrLosses(isWon: Boolean) : List<Game>

    @Query("SELECT MAX(score) FROM Game")
    fun getRecord() : Int

    @Query("SELECT * FROM Game WHERE date BETWEEN :startingDate AND :endingDate")
    fun selectSpecificDate(startingDate: String, endingDate: String) : List<Game>

    @Insert
    fun insertGame(game: Game): Long

    @Update
    fun updateGame(updatedGame: Game)

    @Delete
    fun deleteGame(deletedGame: Game)
}