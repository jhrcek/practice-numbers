# practice-numbers

A small web app for drilling comprehension and pronunciation of Portuguese numbers (0-9999).

Try it at [janhrcek.cz/practice-numbers](https://janhrcek.cz/practice-numbers/).

## What it does

You pick a number range and a session goal, then practice in one of two modes:

- **Listen** - a number is played aloud, you type what you heard and the app checks it.
- **Speak** - a number is shown, you say it out loud, then play the recording to compare and judge yourself.

At the end of a session you get a summary with accuracy and average time per answer.

## Development

The app is written in [Elm](https://elm-lang.org/); pre-generated mp3 recordings live in `dist/audio/`.

```
make build    # compile src/Main.elm to dist/elm.js
make minify   # build + minify dist/elm.js (requires uglifyjs)
make format   # format Elm sources (requires elm-format)
```

The contents of `dist/` are committed and deployed to GitHub Pages by a workflow on every push to master.
