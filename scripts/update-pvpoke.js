const fs = require("fs");

const fetch = require('node-fetch');

fetch("https://pvpoke.com/data/gobattleleague/overall/rankings-2500.json")
    .then(res => res.json())
    .then(json => {
        fs.writeFileSync("../src/assets/rankings-2500.json", JSON.stringify(json));
        console.log("written", json);
    })
