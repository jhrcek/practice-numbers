.PHONY: build
build:
	elm make src/Main.elm --optimize --output=dist/elm.js
