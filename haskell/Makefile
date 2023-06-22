# Make dayN to run the day
# Make dayN input=./path/to/input.txt to use custom input
day1:
	$(eval input ?= ./src/DayOne/input.txt)
	cabal run haskell-exe -- 1 $(input)

.PHONY: day1

day2:
	$(eval input ?= ./src/DayTwo/input.txt)
	cabal run haskell-exe -- 2 $(input)

.PHONY: day


