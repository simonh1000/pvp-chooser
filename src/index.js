"use strict";

require("./styles.css");
require("./styles.scss");

const storageKey = "pokemons";

const myPokemon = JSON.parse(localStorage.getItem(storageKey) || "[]");

const flags = {
    myPokemon,
    debug: false
};
// console.log(flags);

const {Elm} = require("./Main");
var app = Elm.Main.init({
    flags
});

app.ports.toJs.subscribe(data => {
    localStorage.setItem(storageKey, JSON.stringify(data.payload));
});
