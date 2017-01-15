dirs:
	mkdir -p tmp/
	rm -rf tmp/*

dist/simple: dirs
	stack install --docker simple --local-bin-path tmp/

dist/serverless-hs: 
	stack install serverless-hs --local-bin-path tmp/

upload: dist/simple dist/serverless-hs
	tmp/serverless-hs tmp/smpl test.yaml 

