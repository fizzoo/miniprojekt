all: haskell

install: haskell
	install stopwatch /usr/local/bin/

haskell:
	ghc stopwatch.hs

cpp:
	g++ -std=c++14 -pthread timer.cc -o timer

clean:
	rm stopwatch
	rm stopwatch.{hi,o}
	rm timer
