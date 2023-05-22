package com.example.l5spaceinvaders

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import androidx.activity.result.contract.ActivityResultContracts
import androidx.appcompat.app.AppCompatActivity
import androidx.room.Room
import com.example.l5spaceinvaders.ScoresActivity.Companion.dao
import com.example.l5spaceinvaders.ScoresActivity.Companion.database
import com.example.l5spaceinvaders.ScoresActivity.Companion.gamesList
import com.example.l5spaceinvaders.ScoresActivity.Companion.gamesRecyclerView
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import java.time.LocalDate
import java.util.ArrayList

class MainMenuActivity : AppCompatActivity() {
    companion object {
        var score = 0
        var outcome = false
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        database = Room.databaseBuilder(this, Database::class.java, "database").allowMainThreadQueries().build()
        dao = database.Dao()
        gamesList = ArrayList(dao.selectAllGames())
        setContentView(R.layout.main_menu_activity)
    }

    fun playSpaceInvaders(view: View) {
        val intent = Intent(this, SpaceInvadersActivity::class.java)
        resultLauncher.launch(intent)
    }

    private var resultLauncher = registerForActivityResult(ActivityResultContracts.StartActivityForResult()) { result ->
        if (result.resultCode == Activity.RESULT_OK) {

            val month = LocalDate.now().month.value
            val monthString: String = if (month < 10) {
                "0$month"
            } else {
                "$month"
            }
            val year = LocalDate.now().year
            val day = LocalDate.now().dayOfMonth
            val dayString: String = if (day < 10) {
                "0$day"
            } else {
                "$day"
            }
            val dateString = "$dayString-$monthString-$year"
            Game(score, dateString, outcome).let {
                GlobalScope.launch(Dispatchers.IO) {
                    it.id = dao.insertGame(it)
                }
                gamesList.add(it)
                gamesRecyclerView.adapter?.notifyItemInserted(gamesList.size)
            }
        }
    }

    fun showScores(view: View) {
        val intent = Intent(this, ScoresActivity::class.java)
        startActivity(intent)
    }
}