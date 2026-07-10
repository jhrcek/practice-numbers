.PHONY: build
build:
	elm make src/Main.elm --optimize --output=dist/elm.js

.PHONY: format
format:
	elm-format --yes src/

.PHONY: minify
minify: build
	uglifyjs dist/elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output dist/elm.min.js
	mv dist/elm.min.js dist/elm.js
