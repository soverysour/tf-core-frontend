// Styles
require('./assets/styles/main.scss');

// Vendor JS is imported as an entry in webpack.config.js

// Elm
var storageKey = "tradeForallStorage";
var Elm = require('./elm/Main.elm').Elm;
var flags = localStorage.getItem(storageKey);
var app = Elm.Main.init({ flags: flags });

app.ports.storeToCache.subscribe(function (value) {
  if (value === null) {
    localStorage.removeItem(storageKey);
  } else {
    localStorage.setItem(storageKey, JSON.stringify(value));
  }
});
