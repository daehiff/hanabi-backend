run:
		source .env && ghcid \
				--allow-eval \
				--command "stack ghci" \
				--test "DevMain.update"
ptest: 
		source .tenv && stack test

ghci: 
		stack exec ghci


doc:
	 apidoc -i src/ -o static/doc/ -f "[A-z]*\.hs" --parse-languages .hs=$HOME/haskell.js
