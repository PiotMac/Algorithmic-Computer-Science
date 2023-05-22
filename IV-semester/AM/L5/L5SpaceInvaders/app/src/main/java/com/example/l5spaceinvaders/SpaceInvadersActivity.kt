package com.example.l5spaceinvaders

import android.app.Activity
import android.graphics.Point
import android.os.Bundle

class SpaceInvadersActivity : Activity() {
    private var spaceInvadersView: SpaceInvadersView? = null

    companion object {
        @Volatile var isPlaying : Boolean = true
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val display = windowManager.defaultDisplay
        val size = Point()
        display.getSize(size)
        spaceInvadersView = SpaceInvadersView( this, size.x, size.y)
        setContentView(spaceInvadersView)
        spaceInvadersView?.resume()
    }
}