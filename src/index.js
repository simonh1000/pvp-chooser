"use strict";

require("./styles.css");
require("./styles.scss");

// require("@mdi/font/css/materialdesignicons.min.css");

const storageKey = "pokemons";

import gamemaster from "./assets/gamemaster.json";
import rankings2500 from "./assets/rankings-2500.json";

const myPokemon = JSON.parse(localStorage.getItem(storageKey) || "[]");

const flags = Object.assign(gamemaster, {
    myPokemon,
    rankings2500,
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
