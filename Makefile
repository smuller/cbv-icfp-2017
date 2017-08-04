default: basicoffline

basiconline:
	mlton -const 'Exn.keepHistory true' -output basiconline src/basiconline.mlb

basicoffline:
	mlton -const 'Exn.keepHistory true' -output basicoffline src/basicoffline.mlb
