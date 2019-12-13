#!/usr/bin/env bash

while inotifywait -e close_write src/PhotoGroove.elm; do
  elm make src/PhotoGroove.elm --output=elm.js
done
