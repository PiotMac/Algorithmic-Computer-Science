const express = require('express');
const jwt = require('jsonwebtoken');
const mongoose = require('mongoose');
const path = require('path');

const app = express();
const port = 3000;

app.use(express.static('public'));

// Connect to MongoDB
mongoose.connect('mongodb://localhost/notes_db', {
  useNewUrlParser: true,
  useUnifiedTopology: true,
});

// Define the Note schema
const noteSchema = new mongoose.Schema({
  title: String,
  content: String,
});

// Define the Note model
const Note = mongoose.model('Note', noteSchema);

// Middleware to verify JWT
const verifyToken = (req, res, next) => {
  const token = req.headers.authorization?.split(' ')[1];
  if (!token) {
    return res.status(401).json({ message: 'No token provided' });
  }

  jwt.verify(token, 'secretKey', (err, decoded) => {
    if (err) {
      return res.status(403).json({ message: 'Invalid token' });
    }
    req.userId = decoded.id;
    next();
  });
};

// API routes
app.use(express.json());

app.get('/login', (req, res) => {
    res.sendFile(path.join(__dirname, 'login.html'));
});

// Login route
app.post('/login', (req, res) => {
  const { username, password } = req.body;
  if (username === 'admin' && password === 'password') {
    const user = { id: 'admin' };
    const token = jwt.sign(user, 'secretKey', { expiresIn: '5s' });
    res.json({ token });
  }
  else {
    res.status(401).json({ message: 'Invalid username or password' });
  }
});


app.get('/note', async (req, res) => {
    res.sendFile(path.join(__dirname, 'all_notes.html'));
});


// Protected route - Get all notes
app.get('/notes', verifyToken, async (req, res) => {
  try {
    const notes = await Note.find({}); // Fetch all notes from the database
    res.json(notes);
  } catch (err) {
    res.status(500).json({ message: 'Error fetching notes' });
  }
});


// Protected route - Get a specified note
app.get('/note/:id', verifyToken, async (req, res) => {
  const noteId = req.params.id;
  try {
    const note = await Note.findById(noteId);
    if (note == null) {
        return res.status(404).json({ message: 'Note not found!' });
    }
    res.json(note);
  } catch (err) {
    res.status(500).json({ message: err.message });
  }
});


// Protected route - Create a new note
app.post('/note', verifyToken, async (req, res) => {
  try {
    const { title, content } = req.body;
    const newNote = new Note({ title, content });
    await newNote.save();
    res.status(201).json(newNote);
  } catch (err) {
    res.status(500).json({ message: 'Error creating note' });
  }
});

// Protected route - Update a specified note
app.put('/note/:id', verifyToken, async (req, res) => {
  const noteId = req.params.id;
  const updatedNote = req.body;

  try {
    const existingNote = await Note.findById(noteId);
    if (!existingNote) {
      return res.status(404).json({ message: 'Note not found!' });
    }

    existingNote.title = updatedNote.title;
    existingNote.content = updatedNote.content;

    const updatedNoteResult = await existingNote.save();
    res.json(updatedNoteResult);
  } 
  catch (err) {
    res.status(500).json({ message: err.message });
  }
});

// Protected route - Delete a note
app.delete('/note/:id', verifyToken, async (req, res) => {
  try {
    const { id } = req.params;
    await Note.findByIdAndDelete(id);
    res.json({ message: 'Note deleted successfully' });
  } catch (err) {
    res.status(500).json({ message: 'Error deleting note' });
  }
});

// Start the server
app.listen(port, () => {
  console.log(`Server listening on port ${port}`);
});
