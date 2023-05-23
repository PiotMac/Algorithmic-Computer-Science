package com.example.l5spaceinvaders

import android.annotation.SuppressLint
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageButton
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView

class ScoresRecyclerAdapter(private val games: List<Game>) : RecyclerView.Adapter<ScoresRecyclerAdapter.ViewHolder>() {
    class ViewHolder(view: View) : RecyclerView.ViewHolder(view) {
        val scoreTV : TextView
        val dateTV : TextView
        val winTV : TextView
        val deleteButton : ImageButton
        init {
            scoreTV = view.findViewById(R.id.scoreTV)
            dateTV = view.findViewById(R.id.dateTV)
            winTV = view.findViewById(R.id.winTV)
            deleteButton = view.findViewById(R.id.deleteRecordButton)
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        val view = LayoutInflater.from(parent.context).inflate(R.layout.game_item, parent, false)
        return ViewHolder(view)
    }

    @SuppressLint("SetTextI18n")
    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        holder.scoreTV.text = games[position].score.toString()
        holder.dateTV.text = games[position].date.toString()
        if (games[position].won) {
            holder.winTV.text = "YES"
        }
        else {
            holder.winTV.text = "NO"
        }
        holder.deleteButton.setOnClickListener {
            ScoresActivity.deleteRecord(position)
        }
    }

    override fun getItemCount(): Int {
        return games.size
    }
}