const fs = require("fs");

// https://pogoapi.net/api/v1/current_pokemon_moves.json
const pmns = require("./current_pokemon_moves.json");
// https://pogoapi.net/api/v1/pokemon_types.json
const pokemon_types = require("./pokemon_types.json");

const getType = (name, form) => {
    let tmp = pokemon_types.filter(p => {
        if (form != "Normal") {
            return p.pokemon_name == name && p.form == form;
        } else
            // name must match and either form is Normal or it is not present
            return p.pokemon_name == name && (p.form == form || typeof p.form == 'undefined');
    });
    if (tmp.length) {
        return tmp[0].type;
    } else {
        throw `Could not look up ${name} ${form}`;
    }
}

const formsWeWant = [
    "Normal",
    "A",
    "Alola",
    "Altered",
    "Attack",
    "Defense",
    "Rainy",
    "Snowy",
    "Sunny",
    "Speed",
    "Origin"
];

const tmp = pmns
    .filter(p => !p.form || formsWeWant.indexOf(p.form) > -1)
    .map(p => {
        // let tp = tps[p.pokemon_id.toString()];
        // console.log(p.pokemon_id.toString(), tp);
        // p.type = tp && tp.types ? tp.types.map(tp => tp.type) : [];
        // FIXME needs to work out type taking form into account
        p.type = getType(p.pokemon_name, p.form || "Normal");
        return p;
    });

// fs.writeFileSync("./pokedex.json", JSON.stringify(tmp));
fs.writeFileSync("../src/assets/pokedex.json", JSON.stringify(tmp));
console.log("New pokedex.json written");
