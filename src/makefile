all:
	ghc -Wall -fno-warn-unused-do-bind -o parser Parser.hs -main-is Parser
	ghc -Wall -fno-warn-unused-do-bind -o eval Eval.hs -main-is Eval
	ghc -Wall -fno-warn-unused-do-bind -o repl Repl.hs -main-is Repl
clean:
	rm -f *.hi *.o parser eval repl
