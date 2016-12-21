default: dist/deployment.zip

dirs:
	mkdir -p tmp/ dist/
	rm -rf tmp/*
	rm -rf dist/*

dist/hs-main: dirs
ifndef EXECUTABLE
	$(error EXECUTABLE is undefined)
endif
	stack install $(EXECUTABLE) --local-bin-path tmp/
	mv tmp/$(EXECUTABLE) dist/hs-main

dist/handler.js: dirs
	cp handler.js dist/handler.js

dist/deployment.zip: dist/hs-main dist/handler.js
	zip -j dist/deployment.zip dist/hs-main dist/handler.js
