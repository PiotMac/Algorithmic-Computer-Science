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

    @Query("DELETE FROM Game WHERE Game.score = :score AND Game.date = :date AND Game.won = :result")
    fun deleteGame(score: Int, date: String, result: Boolean)

    @Delete
    fun deleteRecord(game: Game)

    @Query("SELECT * FROM Game ORDER BY Game.score ASC")
    fun sortAscScore() : List<Game>

    @Query("SELECT * FROM Game ORDER BY Game.score DESC")
    fun sortDescScore() : List<Game>

    @Query("SELECT * FROM Game ORDER BY Game.date ASC")
    fun sortAscDate() : List<Game>

    @Query("SELECT * FROM Game ORDER BY Game.date DESC")
    fun sortDescDate() : List<Game>

    @Query("SELECT * FROM Game ORDER BY Game.won ASC")
    fun sortAscResult() : List<Game>

    @Query("SELECT * FROM Game ORDER BY Game.won DESC")
    fun sortDescResult() : List<Game>

    @Query("DELETE FROM Game")
    fun nukeTable()
}