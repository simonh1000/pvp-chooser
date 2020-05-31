"use strict";

require("./styles.css");
require("./styles.scss");

// require("@mdi/font/css/materialdesignicons.min.css");

const storageKey = "pokemons";

// import pokemon from "./assets/pokedex.json";
// import charged from "./assets/chargedmoves.json";
// import fast from "./assets/fastmoves.json";

import gamemaster from "./assets/gamemaster.json";

const myPokemon = JSON.parse(localStorage.getItem(storageKey) || "[]");

const flags = Object.assign(gamemaster, {
    myPokemon,
    debug : false
});
// console.log(flags);

const { Elm } = require("./Main");
var app = Elm.Main.init({
    flags
});

app.ports.toJs.subscribe(data => {
    localStorage.setItem(storageKey, JSON.stringify(data.payload));
});
