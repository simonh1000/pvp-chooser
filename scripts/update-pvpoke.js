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
            // console.log("written", tmp);
        })
        .catch(err => console.log("** gamemaster **", err));
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
    let url = `https://pvpoke.com/data/rankings/${league}/overall/${filename}`;
    let dstLocation = `../src/assets/${league}/${filename}`;
    fetch(url)
        .then(res => res.json())
        .then(json => {
            json = json.map(mapRanking)
            fs.writeFileSync(dstLocation, JSON.stringify(json));
            // console.log("written", json);
        })
        .catch(err => console.log(`** ${league} ${item} **`, err));
}

getGamemaster();
// https://pvpoke.com/data/rankings/love/overall/rankings-1500.json
convert("love", "1500");
// standard competitions
convert("all", "1500");
convert("premier", "2500");
convert("all", "2500");
convert("premier", "10000");
convert("all", "10000");
