.PHONY: ExpRiment_install ExpRiment_test ExpRiment_document ExpRiment_check krakenator_install_ExpRiment

krakenator_install_ExpRiment:
	git status
	ssh -t cayek@krakenator.imag.fr "cd ~/Projects/Thesis/ExpRiment/; git pull; make ExpRiment_install"


## Rpackage

ExpRiment_install:
	R -e 'devtools::install(pkg = "./")'

ExpRiment_test:
	R -e 'devtools::test(pkg = "./")'

ExpRiment_document:
	R -e 'devtools::document(pkg = "./")'

ExpRiment_check:
	R -e 'devtools::check(pkg = "./")'
