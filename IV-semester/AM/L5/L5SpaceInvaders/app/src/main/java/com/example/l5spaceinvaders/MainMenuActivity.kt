package com.example.l5spaceinvaders

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import androidx.activity.result.contract.ActivityResultContracts
import androidx.appcompat.app.AppCompatActivity
import com.example.l5spaceinvaders.ScoresActivity.Companion.dao
import com.example.l5spaceinvaders.ScoresActivity.Companion.gamesList
import com.example.l5spaceinvaders.ScoresActivity.Companion.gamesRecyclerView
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import java.text.SimpleDateFormat
import java.util.ArrayList
import java.util.Date

class MainMenuActivity : AppCompatActivity() {

    @SuppressLint("SimpleDateFormat")
    val formatter = SimpleDateFormat("dd-MM-yyyy")

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.main_menu_activity)
    }

    fun playSpaceInvaders(view: View) {
        val intent = Intent(this, SpaceInvadersActivity::class.java)
        resultLauncher.launch(intent)
    }

    private var resultLauncher = registerForActivityResult(ActivityResultContracts.StartActivityForResult()) { result ->
        if (result.resultCode == Activity.RESULT_OK) {
            val data: Intent? = result.data
            val position = data?.getIntExtra("position", 0)
            val score = data?.getIntExtra("score", 0)
            val dateString = data?.getStringExtra("date")
            var date : Date = formatter.parse("01-01-2023") as Date
            if (dateString != null) {
                date = formatter.parse(dateString) as Date
            }
            val outcome = data?.getBooleanExtra("result", false)
            if (score != null && dateString != null && outcome != null) {
                Game(score, dateString, outcome).let {
                    GlobalScope.launch(Dispatchers.IO) {
                        it.id = dao.insertGame(it)
                    }
                    gamesList.add(it)
                    gamesRecyclerView.adapter?.notifyItemInserted(gamesList.size)
                }
            }
        }
    }

    fun showScores(view: View) {
        val intent = Intent(this, ScoresActivity::class.java)
        startActivity(intent)
    }
}