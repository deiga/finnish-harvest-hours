const mongoose = require('mongoose');

const userSchema = new mongoose.Schema({
    id: Number,
    previousBalance: Number,
    currentCity: {
      type: String,
      enum: ['Helsinki', 'Lund', 'Berlin']
    }
});

module.exports = mongoose.model('User', userSchema);
