package com.example.l5spaceinvaders

import android.app.Activity
import android.content.Intent
import android.graphics.Point
import android.os.Bundle

class SpaceInvadersActivity : Activity() {
    var spaceInvadersView: SpaceInvadersView? = null
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        // get a Display object to access screen details
        val display = windowManager.defaultDisplay
        val size = Point()
        display.getSize(size)
        spaceInvadersView = SpaceInvadersView(this, size.x, size.y)
        setContentView(spaceInvadersView)
        playTheGame()
    }

    private fun playTheGame() {
        spaceInvadersView?.resume()
        while (!SpaceInvadersView.lost) {

        }
    }

    // This method executes when the player starts the game
    override fun onResume() {
        super.onResume()
        spaceInvadersView?.resume()
    }



    // This method executes when the player quits the game
    override fun onPause() {
        super.onPause()
        spaceInvadersView?.pause()
        val intent = Intent()
        intent.putExtra("score", SpaceInvadersView.score)
        intent.putExtra("result", SpaceInvadersView.lost)
        setResult(RESULT_OK, intent)
        finish()
    }
}