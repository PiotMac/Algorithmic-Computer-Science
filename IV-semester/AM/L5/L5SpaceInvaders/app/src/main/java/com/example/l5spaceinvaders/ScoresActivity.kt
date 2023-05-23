package com.example.l5spaceinvaders

import android.annotation.SuppressLint
import android.app.Activity
import android.os.Bundle
import android.view.View
import android.widget.Button
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import java.util.ArrayList

class ScoresActivity : Activity() {
    companion object {
        lateinit var gamesList : ArrayList<Game>
        lateinit var dao : DatabaseDao
        lateinit var gamesRecyclerView : RecyclerView
        lateinit var database: Database
        fun deleteRecord(position: Int) {
            dao.deleteRecord(gamesList[position])
            gamesList.remove(gamesList[position])
            val gameAdapter = ScoresRecyclerAdapter(gamesList)
            gamesRecyclerView.adapter = gameAdapter
            gamesRecyclerView.adapter?.notifyDataSetChanged()
        }
    }

    private lateinit var sortingScoreButton : Button
    private lateinit var sortingDateButton : Button
    private lateinit var sortingResultButton : Button

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.scores_activity)
        gamesRecyclerView = findViewById(R.id.gamesRecyclerView)
        sortingScoreButton = findViewById(R.id.sortingScoreButton)
        sortingDateButton = findViewById(R.id.sortingDateButton)
        sortingResultButton  = findViewById(R.id.sortingResultButton)
        getGames()
    }

    private fun getGames() {
        val layoutManager = LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false)
        gamesRecyclerView.layoutManager = layoutManager
        val gameAdapter = ScoresRecyclerAdapter(gamesList)
        gamesRecyclerView.adapter = gameAdapter
    }

    @SuppressLint("SetTextI18n", "NotifyDataSetChanged")
    fun sortScore(view: View) {
        if (sortingScoreButton.text == "SORT \u2B06") {
            sortingScoreButton.text = "SORT \u2B07"
            gamesList = ArrayList(dao.sortAscScore())
        }
        else {
            sortingScoreButton.text = "SORT \u2B06"
            gamesList = ArrayList(dao.sortDescScore())
        }
        val gameAdapterUpdated = ScoresRecyclerAdapter(gamesList)
        gamesRecyclerView.adapter = gameAdapterUpdated
        gamesRecyclerView.adapter?.notifyDataSetChanged()
    }

    @SuppressLint("SetTextI18n", "NotifyDataSetChanged")
    fun sortDate(view: View) {
        if (sortingDateButton.text == "SORT \u2B06") {
            sortingDateButton.text = "SORT \u2B07"
            gamesList = ArrayList(dao.sortAscDate())
        }
        else {
            sortingDateButton.text = "SORT \u2B06"
            gamesList = ArrayList(dao.sortDescDate())
        }
        val gameAdapterUpdated = ScoresRecyclerAdapter(gamesList)
        gamesRecyclerView.adapter = gameAdapterUpdated
        gamesRecyclerView.adapter?.notifyDataSetChanged()
    }

    @SuppressLint("SetTextI18n", "NotifyDataSetChanged")
    fun sortResult(view: View) {
        if (sortingResultButton.text == "SORT \u2B06") {
            sortingResultButton.text = "SORT \u2B07"
            gamesList = ArrayList(dao.sortAscResult())
        }
        else {
            sortingResultButton.text = "SORT \u2B06"
            gamesList = ArrayList(dao.sortDescResult())
        }
        val gameAdapterUpdated = ScoresRecyclerAdapter(gamesList)
        gamesRecyclerView.adapter = gameAdapterUpdated
        gamesRecyclerView.adapter?.notifyDataSetChanged()
    }


}