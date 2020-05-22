"Elm in Action" by Richard Feldman
---

[![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)

You can get this book directly from https://www.manning.com/books/elm-in-action. You can choose to support the stream by using my affiliate link https://www.manning.com/books/elm-in-action?a_aid=chiroptical&a_bid=b15edc5c.

## Notes

- Chapter 1 is essentially all in the REPL sorry!

## Running The Application

- If anything before chapter 7, can use:

```console
elm-live src/PhotoGroove.elm --open --start-page=index.html -- --output=elm.js
```

- Else:

```console
http-server-spa .
```

## Chapter 7 Notes

Chapter 7 and 8 were combined into a single application in `chapter7/` folder.
This was just the order in which I wanted to work through the book. You could
split the chapter into two if you so choose.

## Questions

- Does `elm make ... --optimize` make a large difference in a larger
  application?
- For range sliders, can we only update after releasing the mouse to avoid
  lag when dragging quickly
