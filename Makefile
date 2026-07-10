.PHONY: build
build:
	elm make src/Main.elm --optimize --output=dist/elm.js

.PHONY: format
format:
	elm-format --yes src/
