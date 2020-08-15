# Pokemon PVP team manager

See it in action: https://pvp.netlify.app/

This app will help you keep track of your pokemon and their types as well as of the competitors you encounter. The data you add is stored on your computer and no where else.

![screenshot](https://github.com/simonh1000/pvp-chooser/blob/master/src/assets/images/screenshot.png?raw=true)

To start, you add your pokemon for each league, and those that you encounter in combat. You select you pokemons' attacks, and the PvPoke recommendations are shown in line (Ultra League only at present).

The app enables you to build teams of three and to compare them. Each team gets a score. The absolute value is meaningless, but the relative scores may help you. The algorithm focuses on type dominance and does not take into account the details of energy generation and usage. It is also unlikely to recommend an unbalanced team, even though some top players are using them - I reached level 8 in season 1 so don't consider me an expert! YMMV

A summary page is available while battling - perhaps it will help you choose the right attack in the heat of the moment!

## Installation - see also https://github.com/simonh1000/elm-webpack-starter

Clone this repo into a new project folder and run `npm install`.

## Developing

Start with Elm debug tool with either
```sh
$ npm start
or
$ npm start --nodebug
```

the `--nodebug` removes the Elm debug tool. This can become valuable if your model becomes very large.

Open http://localhost:3000 and start modifying the code in /src.  **Note** that this starter expects you have installed [elm-format globally](https://github.com/avh4/elm-format#installation-). 

## Production

Build production assets (js and css together) with:

```sh
npm run prod
```

Run the script in scripts directory to update the rankings files from pvpoke

## Changelog

1.3 Better picker for Registering page
1.2 Add support for Ultra Premier Cup
1.1 Add support for Master Premier Cup
