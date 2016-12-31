

var express = require("express");

var app = express();

app.use(express.static(__dirname + "/"));

//app.get("/", function(req, res) {
//  res.send('/')
//})

app.listen(3001, function() {
  console.log("Listening on port 3001")
})
