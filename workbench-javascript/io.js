const fs = require('fs')
const os = require('os')
fs.readFile('./nice.txt','utf8',(err,data) =>{
    console.log(data.split(os.EOL))
    }
)