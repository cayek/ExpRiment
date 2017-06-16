.PHONY: ExpRiment_install ExpRiment_test ExpRiment_document ExpRiment_check


## Rpackage

ExpRiment_install:
	R -e 'devtools::install(pkg = "./")'

ExpRiment_test:
	R -e 'devtools::test(pkg = "./")'

ExpRiment_document:
	R -e 'devtools::document(pkg = "./")'

ExpRiment_check:
	R -e 'devtools::check(pkg = "./")'
