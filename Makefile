.PHONY: ExpRiment_install ExpRiment_test ExpRiment_document ExpRiment_check krakenator_install_ExpRiment

## krak

krakenator_deploy:
	git status
## git commit --allow-empty -am "deploy on krakenator"
	git push krakenator master

krakenator_push_hook:
	scp ./hooks/post-receive.sh cayek@krakenator:/home/cayek/Gits/2017/ExpRiment.git/hooks/post-receive


## Rpackage

ExpRiment_install:
	R -e 'devtools::install(pkg = "./")'

ExpRiment_test:
	R -e 'devtools::test(pkg = "./")'

ExpRiment_document:
	R -e 'devtools::document(pkg = "./")'

ExpRiment_check:
	R -e 'devtools::check(pkg = "./")'
