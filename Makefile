run:
		source .env && ghcid \
				--allow-eval \
				--command "stack ghci" \
				--test "DevMain.update"
ptest: 
		source .tenv && stack test

ghci: 
		stack exec ghci