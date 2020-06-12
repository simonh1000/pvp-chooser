const fs = require("fs");

const fetch = require('node-fetch');

let r2500 = [ "https://pvpoke.com/data/all/overall/rankings-2500.json", "rankings-2500"];
let r10000 = ["https://pvpoke.com/data/all/overall/rankings-10000.json", "rankings-10000"];

mapRanking = ranking => {
    delete ranking.matchups;
    delete ranking.counters;
    delete ranking.moves;
    delete ranking.moveStr;
    delete ranking.rating;
    return ranking;
}

function convert(item) {
    fetch(item[0])
        .then(res => res.json())
        .then(json => {
            json = json.map(mapRanking)
            fs.writeFileSync(`../src/assets/${item[1]}.json`, JSON.stringify(json));
            console.log("written", json);
        });
}

convert(r2500)