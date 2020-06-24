const fs = require("fs");

const fetch = require('node-fetch');

function getGamemaster() {
    let filename = `gamemaster.json`;
    let url = `https://pvpoke.com/data/${filename}`;
    let dstLocation = `../src/assets/${filename}`;
    fetch(url)
        .then(res => res.json())
        .then(json => {
            let tmp = {};
            tmp.pokemon = json.pokemon;
            tmp.moves = json.moves;
            fs.writeFileSync(dstLocation, JSON.stringify(tmp));
            console.log("written", tmp);
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

function convert(league, item) {
    let filename = `rankings-${item}.json`;
    let url = `https://pvpoke.com/data/${league}/overall/${filename}`;
    let dstLocation = `../src/assets/${league}/${filename}`;
    fetch(url)
        .then(res => res.json())
        .then(json => {
            json = json.map(mapRanking)
            fs.writeFileSync(dstLocation, JSON.stringify(json));
            console.log("written", json);
        });
}

getGamemaster();
convert("all","1500");
convert("all","2500");
convert("all","10000");
convert("premier","10000");