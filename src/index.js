import { Elm } from "./Main.elm";

function getRandomInt() {
  return Math.floor(Math.random() * 999999);
}

const highScoresKey = 'burger-boss:highScores';

const storedScores = localStorage.getItem(highScoresKey);
const initialScores = storedScores ? JSON.parse(storedScores) : [];
// const initialScores = [1, 2, 3]

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: {initialSeed: getRandomInt(), highScores: initialScores},
});

app.ports.saveScores.subscribe(function(scores) {
  localStorage.setItem(highScoresKey, JSON.stringify(scores));
});