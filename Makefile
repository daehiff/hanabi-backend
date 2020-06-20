run:
		source .env && ghcid \
				--allow-eval \
				--command "stack ghci" \
				--test "DevMain.update"
ptest: 
		stack test

ghci: 
		stack exec ghci