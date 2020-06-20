run:
		source .env && ghcid \
				--allow-eval \
				--command "stack ghci" \
				--test "DevMain.update"


ghci: 
		stack exec ghci