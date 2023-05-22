package com.example.l5spaceinvaders

import android.content.Context
import android.content.Intent
import android.content.res.AssetFileDescriptor
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.graphics.RectF
import android.media.AudioManager
import android.media.SoundPool
import android.util.Log
import android.view.MotionEvent
import android.view.SurfaceHolder
import android.view.SurfaceView
import java.io.IOException


class SpaceInvadersView(private val context: Context, x: Int, y: Int) : SurfaceView(context), Runnable {
    companion object {
        var score : Int = 0
        var lost : Boolean = false
    }

    private var gameThread: Thread? = null
    private val ourHolder: SurfaceHolder

    // a boolean for if the game is being played
    @Volatile
    private var playing = false

    // game is paused at the start
    private var paused = true
    private var canvas: Canvas? = null
    private val paint: Paint

    // used to help calculate fps
    private var timeFrame: Long = 0
    private var timeThisFrame: Long = 0
    private var fps: Long = 0

    //private var lost: Boolean = false

    // screen size in pixels
    private val screenX: Int
    private val screenY: Int
    private lateinit var playerShip: PlayerShip
    private val playerBullets: Array<Bullet?> = arrayOfNulls<Bullet>(250)
    private var numBullets = 0
    private var nextInvaderBullet = 0
    private val invadersBullets: Array<Bullet?> = arrayOfNulls<Bullet>(50)
    private val maxInvaderBullets = 50
    private val invaders: Array<Invader?> = arrayOfNulls<Invader>(30)
    private var numInvaders = 0
    //private var score = 0
    private val bricks: Array<DefenceBrick?> = arrayOfNulls<DefenceBrick>(200)
    private var numBricks = 0

    // for sound FX
    private val soundPool: SoundPool
    private var playerExplodeID = -1
    private var invaderExplodeID = -1
    private var shootID = -1
    private var damageShelterID = -1
    private var uhID = -1
    private var ohID = -1

    // score increase per kill and initial lives
    private val INCREASE = 10
    private val INIT_LIVES = 50

    private var lives = INIT_LIVES

    // how menacing should the sound be
    private var menaceInterval: Long = 1000

    // alternate which menace sound should play next
    private var uhOrOh = false

    // when the last menace sound was played
    private var lastMenaceTime = System.currentTimeMillis()
    private var lastMenace = System.nanoTime() / 1000000

    init {
        ourHolder = holder
        paint = Paint()
        screenX = x
        screenY = y
        score = 0
        lost = false
        soundPool = SoundPool(10, AudioManager.STREAM_MUSIC, 0)
        try {
            // create objects of the 2 required classes
            val assetManager = context.assets
            var descriptor: AssetFileDescriptor
            descriptor = assetManager.openFd("shoot.ogg")
            shootID = soundPool.load(descriptor, 0)
            descriptor = assetManager.openFd("invaderexplode.ogg")
            invaderExplodeID = soundPool.load(descriptor, 0)
            descriptor = assetManager.openFd("damageshelter.ogg")
            damageShelterID = soundPool.load(descriptor, 0)
            descriptor = assetManager.openFd("playerexplode.ogg")
            playerExplodeID = soundPool.load(descriptor, 0)
            descriptor = assetManager.openFd("damageshelter.ogg")
            damageShelterID = soundPool.load(descriptor, 0)
            descriptor = assetManager.openFd("uh.ogg")
            uhID = soundPool.load(descriptor, 0)
            descriptor = assetManager.openFd("oh.ogg")
            ohID = soundPool.load(descriptor, 0)
        } catch (e: IOException) {
            Log.e("error", "failed to load sound files")
        }
        prepareLevel()
    }

    private fun prepareLevel() {
        paused = true
        lives = INIT_LIVES
        score = 0
        numBullets = 0

        // make a new player space ship, player's bullets, invader's bullets, invaders, shelters
        playerShip = PlayerShip(context, screenX, screenY)
        for (i in playerBullets.indices) playerBullets[i] = Bullet(screenY)
        for (i in invadersBullets.indices) invadersBullets[i] = Bullet(screenY)
        numInvaders = 0
        for (column in 0..5) for (row in 0..4) invaders[numInvaders++] =
            Invader(context, row, column, screenX, screenY)
        numBricks = 0
        for (shelterNumber in 0..3) for (column in 0..9) for (row in 0..4) bricks[numBricks++] =
            DefenceBrick(row, column, shelterNumber, screenX, screenY)

        // Reset the menace level
        menaceInterval = 1000
    }

    override fun run() {
        while (playing) {
            // capture the current time in milliseconds in startFrameTime
            val startTime = System.nanoTime()
            val startFrameTime = System.currentTimeMillis()
            // update the frame
            if (!paused) {
                update()
                draw()
            }
            // calculate FPS
            timeThisFrame = System.currentTimeMillis() - startFrameTime
            timeFrame = (System.nanoTime() - startTime) / 1000000
            if (timeFrame >= 1) fps = 1000 / timeFrame
            // play a sound based on the menace level
            if (!paused) {
                if (startFrameTime - lastMenace > menaceInterval) {
                    if (uhOrOh)
                        soundPool.play(uhID, 1f, 1f, 0, 0, 1f)
                    else
                        soundPool.play(ohID, 1f, 1f, 0, 0, 1f)
                    // reset the last menace time
                    lastMenaceTime = System.currentTimeMillis()
                    lastMenace = System.nanoTime() / 1000000
                    // flip value of uhOrOh
                    uhOrOh = !uhOrOh
                }
            }
        }
    }


    private fun update() {
        // true if an invader bumps into the side of the screen
        var bumped = false
        // true if the player lost
        //var lost = false
        // update the player's ship
        playerShip.update(fps)

        // update the players bullet
        for (bullet in playerBullets) {

            if (bullet == null) {
                return
            }
            if (bullet.status) {
                bullet.update(fps)
            }
        }

        // update all the invaders bullets if active
        for (bullet in invadersBullets) {
            if (bullet == null)
                return
            if (bullet.status)
                bullet.update(fps)
        }
        // update all the invaders if visible
        for (invader in invaders) {//if (invader.getVisibility()) {
            if (invader == null) {
                return
            }
            // move the next invader
            invader.update(fps)
            // if invader will shoot
            if (invader.takeAim(playerShip.x, playerShip.length)) {// if so spawn a bullet
                if (invadersBullets[nextInvaderBullet]?.shoot(
                        invader.x + invader.length / 2,
                        invader.y,
                        1
                    ) == true
                ) {
                    // shot fired, prepare for the next shot
                    nextInvaderBullet++
                    // loop back to the first one if we have reached the last
                    if (nextInvaderBullet == maxInvaderBullets) // makes sure that only one bullet is fired at a time
                        nextInvaderBullet = 0
                }
            }
            // if that move caused them to bump the screen change bumped to true
            if (invader.x > screenX - invader.length || invader.x < 0)
                bumped = true
        }


        // determines what to do if an invader bumps into the edge of the screen
        if (bumped) {
            // move all the invaders down and change direction
            for (invader in invaders) {
                if (invader == null) {
                    return
                }

                invader.dropDownAndReverse()
                // have the invaders landed
                if (invader.y > screenY - screenY / 10) lost = true
            }
            // increase the menace level by making the sounds more frequent
            menaceInterval = menaceInterval - 80
        }
        if (lost) pause()//prepareLevel()

        // determines if the player's bullet hit the top of the screen
        for (bullet in playerBullets) {
            if (bullet == null) {
                return
            }
            if (bullet.impactPointY < 0) bullet.setInactive()
        }

        // renders bullets inactive after they've hit the bottom of the screen
        for (bullet in invadersBullets) {
            if (bullet == null) {
                return
            }
            if (bullet.impactPointY > screenY) bullet.setInactive()
        }

        // determines if a player's bullets have hit an invader
        for (bullet in playerBullets) {
            if (bullet == null) {
                return
            }
            if (bullet.status) {
                for (invader in invaders) {
                    if (invader == null) {
                        return
                    }
                    if (invader.visibility && RectF.intersects(bullet.rect, invader.rect)) {
                        invader.setInvisible()
                        bullet.setInactive()
                        soundPool.play(invaderExplodeID, 1f, 1f, 0, 0, 1f)
                        score += INCREASE
                        // checks to see if the player has won
                        if (score == numInvaders * INCREASE) {
                            prepareLevel()
                            break
                        }
                    }
                }
            }
        }


        // determines if an alien bullet hit a shelter brick
        for (bullet in invadersBullets) {
            if (bullet == null) {
                return
            }

            if (bullet.status) {
                for (brick in bricks) {
                    if (brick == null) {
                        return
                    }
                    if (brick.visibility && RectF.intersects(bullet.rect, brick.rect)) {
                        // if a collision has occurred
                        bullet.setInactive()
                        brick.setInvisible()
                        soundPool.play(damageShelterID, 1f, 1f, 0, 0, 1f)
                    }
                }
            }
        }
        // determines what to do if a player bullet hits a shelter brick
        for (bullet in playerBullets) {
            if (bullet == null) {
                return
            }
            if (bullet.status) {
                for (brick in bricks) {
                    if (brick == null) {
                        return
                    }
                    if (brick.visibility && RectF.intersects(bullet.rect, brick.rect)) {
                        // if a collision has occurred
                        bullet.setInactive()
                        brick.setInvisible()
                        soundPool.play(damageShelterID, 1f, 1f, 0, 0, 1f)
                    }
                }
            }
        }

        for (i in invadersBullets.indices) {
            if (invadersBullets[i]?.status == true) {
                val invaderBullet : Bullet = invadersBullets[i]!!
                if (RectF.intersects(playerShip.rect, invaderBullet.rect)) {
                    invaderBullet.setInactive()
                    lives--
                    soundPool.play(playerExplodeID, 1f, 1f, 0, 0, 1f)
                    // checks if game is over
                    if (lives == 0) {
                        //prepareLevel()
                        pause()
                    }
                }
            }
        }
    }

    private fun draw() {
        if (ourHolder.surface.isValid) {
            // lock the canvas to make it ready for drawing
            canvas = ourHolder.lockCanvas()
            // draw the background color
            canvas!!.drawColor(Color.argb(255, 26, 128, 182))
            paint.color = Color.argb(255, 255, 255, 255)
            // draw the player spaceship
            canvas!!.drawBitmap(playerShip.bitmap, playerShip.x, screenY.toFloat() - 50f, paint)
            // draw the invaders
            for (i in 0 until numInvaders) {
                val invader : Invader = invaders[i]!!
                if (invader.visibility) {
                    if (uhOrOh) {
                        canvas!!.drawBitmap(invader.bitmap, invader.x, invader.y, paint)
                    }
                    else {
                        canvas!!.drawBitmap(invader.bitmap2, invader.x, invader.y, paint)
                    }
                }
            }
            // draw the bricks if visible
            for (i in 0 until numBricks) {
                if (bricks[i]?.visibility == true) {
                    if (bricks[i] == null) {
                        return
                    }
                    val brick : DefenceBrick = bricks[i]!!
                    canvas!!.drawRect(brick.rect, paint)
                }
            }
            // draw the players bullet if active
            for (i in playerBullets.indices) {
                val bullet: Bullet = playerBullets[i] ?: return
                if (bullet.status) {
                    canvas!!.drawRect(bullet.rect, paint)
                }
            }
            // update all the invader's bullets if active
            for (i in invadersBullets.indices) {
                if (invadersBullets[i]?.status == true) {
                    val bullet : Bullet = invadersBullets[i]!!
                    canvas!!.drawRect(bullet.rect, paint)
                }
            }
            // draw the score and remaining lives
            paint.color = Color.argb(255, 249, 129, 0)
            paint.textSize = 40f
            canvas!!.drawText("Score: $score   Lives: $lives", 10f, 50f, paint)
            // draw everything to the screen
            ourHolder.unlockCanvasAndPost(canvas)
        }
    }

    fun pause() {
        playing = false
        try {
            gameThread!!.join()
        } catch (e: InterruptedException) {
            Log.e("Error:", "joining thread")
        }
    }

    fun resume() {
        playing = true
        gameThread = Thread(this)
        gameThread!!.start()
    }

    override fun onTouchEvent(motionEvent: MotionEvent): Boolean {
        var run = true
        var j = 0
        var shot = false
        while (run && j < 100) {
            j++
            val switchInt = motionEvent.action and MotionEvent.ACTION_MASK
            var id2Exists = true
            var id2 = 0
            try {
                id2 = motionEvent.getPointerId(1)
            } catch (e: Exception) {
                id2Exists = false
            }
            when (switchInt) {
                MotionEvent.ACTION_MOVE, MotionEvent.ACTION_DOWN -> {
                    paused = false
                    // if touch is above bottom eigth, interpret as movement
                    if (motionEvent.y > screenY * 3 / 4)
                        if (motionEvent.x > screenX / 2)
                            playerShip.setMovementState(playerShip.RIGHT)
                        else
                            playerShip.setMovementState(playerShip.LEFT)
                    else if (id2Exists && motionEvent.getY(id2) > screenY * 3 / 4)
                        if (motionEvent.getX(id2) > screenX / 2)
                            playerShip.setMovementState(playerShip.RIGHT)
                        else
                            playerShip.setMovementState(playerShip.LEFT)

                    // shooting
                    if (motionEvent.y <= screenY * 3 / 4) {
                        // shots fired
                        val bullet = Bullet(screenY)
                        if (numBullets < playerBullets.size) {
                            playerBullets[numBullets] = bullet
                            if (bullet.shoot(playerShip.x + playerShip.length / 2, screenY.toFloat(), bullet.UP) && shot) {
                                soundPool.play(shootID, 1f, 1f, 0, 0, 1f)
                                shot = false
                            }
                            numBullets++
                        } else numBullets = 0
                        run = false
                    }
                    else if (id2Exists) {
                        if (motionEvent.getY(id2) < screenY * 3 / 4) {
                            // shots fired
                            val bullet = Bullet(screenY)
                            if (numBullets < playerBullets.size) {
                                playerBullets[numBullets] = bullet
                                if (bullet.shoot(playerShip.x + playerShip.length / 2, screenY.toFloat(), bullet.UP) && shot) {
                                    soundPool.play(shootID, 1f, 1f, 0, 0, 1f)
                                    shot = false
                                }
                                numBullets++
                            } else numBullets = 0
                            run = false
                        }
                    }
                }

                MotionEvent.ACTION_CANCEL, MotionEvent.ACTION_UP -> {
                    run = false
                    if (motionEvent.y > screenY * 3 / 4)
                        playerShip.setMovementState(playerShip.STOPPED)
                    else if (id2Exists)
                        if (motionEvent.getY(id2) > screenY * 3 / 4)
                            playerShip.setMovementState(playerShip.STOPPED)
                    playerShip.setMovementState(playerShip.STOPPED)
                }
            }
        }
        return true
    }
}