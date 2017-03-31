all: crawler_services order pagerank dict graph query askshiebs webtypes askshiebs_tests cS51 myset crawl nodescore time_tests

crawler_services: crawler_services.ml

	ocamlbuild -use-ocamlfind crawler_services.byte

order: order.ml

	ocamlbuild -use-ocamlfind order.byte

pagerank: pagerank.ml
	
	ocamlbuild -use-ocamlfind pagerank.byte

dict: dict.ml
	
	ocamlbuild -use-ocamlfind dict.byte

graph: graph.ml

	ocamlbuild -use-ocamlfind graph.byte

query: query.ml

	ocamlbuild -use-ocamlfind query.byte

askshiebs: askshiebs.ml
	
	ocamlbuild -use-ocamlfind askshiebs.byte

webtypes: webtypes.ml

	ocamlbuild -use-ocamlfind webtypes.byte

askshiebs_tests: askshiebs_tests.ml

	ocamlbuild -use-ocamlfind askshiebs_tests.byte

cS51: cS51.ml

	ocamlbuild -use-ocamlfind cS51.byte

myset: myset.ml

	ocamlbuild -use-ocamlfind myset.byte

crawl: crawl.ml

	ocamlbuild -use-ocamlfind crawl.byte

nodescore: nodescore.ml

	ocamlbuild -use-ocamlfind nodescore.byte

time_tests: time_tests.ml

	ocamlbuild -use-ocamlfind time_tests.byte


clean:
	rm -rf _build *.byte