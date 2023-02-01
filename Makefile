# Because you can't `dune exec` while running `dune build --watch`. Because
# reasons.
run:
	./_build/default/bin/main.exe
