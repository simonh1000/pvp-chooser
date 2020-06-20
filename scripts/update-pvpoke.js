const fs = require("fs");

const fetch = require('node-fetch');

function getGamemaster() {
    let filename = `gamemaster.json`;
    let url = `https://pvpoke.com/data/${filename}`;
    let dstLocation = `../src/assets/${filename}`;
    fetch(url)
        .then(res => res.json())
        .then(json => {
            fs.writeFileSync(dstLocation, JSON.stringify(json));
            console.log("written", json);
        })
        .catch(err => console.log("bad fetch", err));
}

mapRanking = ranking => {
    delete ranking.matchups;
    delete ranking.counters;
    delete ranking.moves;
    delete ranking.moveStr;
    delete ranking.rating;
    return ranking;
}

function convert(item) {
    let filename = `rankings-${item}.json`;
    let url = `https://pvpoke.com/data/all/overall/${filename}`;
    let dstLocation = `../src/assets/${filename}`;
    fetch(url)
        .then(res => res.json())
        .then(json => {
            json = json.map(mapRanking)
            fs.writeFileSync(dstLocation, JSON.stringify(json));
            console.log("written", json);
        });
}

getGamemaster();
convert("1500");
convert("2500");
convert("10000");