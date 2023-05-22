package com.example.l5spaceinvaders

import android.app.Activity
import android.os.Bundle
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.room.Room
import java.util.ArrayList

class ScoresActivity : Activity() {
    companion object {
        var gamesList : ArrayList<Game> = ArrayList()
        lateinit var dao : DatabaseDao
        lateinit var gamesRecyclerView : RecyclerView
    }
    lateinit var database: Database

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.scores_activity)
        gamesRecyclerView = findViewById(R.id.gamesRecyclerView)
        database = Room.databaseBuilder(this, Database::class.java, "database").build()
        dao = database.Dao()
        getGames()
    }

    private fun getGames() {
        val layoutManager = LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false)
        gamesRecyclerView.layoutManager = layoutManager
        val gameAdapter = ScoresRecyclerAdapter(gamesList)
        gamesRecyclerView.adapter = gameAdapter
    }
}