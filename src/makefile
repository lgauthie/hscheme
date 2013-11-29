all:
	ghc -Wall -fno-warn-unused-do-bind -o eval Eval.hs -main-is Eval
	ghc -Wall -fno-warn-unused-do-bind -o hscheme Repl.hs -main-is Repl
clean:
	rm -f *.hi *.o parser eval repl
