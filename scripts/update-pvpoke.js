const fs = require("fs");

const fetch = require('node-fetch');

let r2500 = "https://pvpoke.com/data/all/overall/rankings-2500.json";

fetch(r2500)
    .then(res => res.json())
    .then(json => {
        fs.writeFileSync("../src/assets/rankings-2500.json", JSON.stringify(json));
        console.log("written", json);
    });
