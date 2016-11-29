build:
	stack build --fast

build-watch:
	stack build --fast --file-watch

clean:
	stack clean

tests:
	stack test --fast

ghci:
	stack ghci haskell-bbcode-parser-reactflux
