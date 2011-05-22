all: serial.beam parallel.beam

serial.beam: serial.erl
	erlc serial.erl

parallel.beam: parallel.erl
	erlc parallel.erl

clean:
	rm -f *.beam

test: serial.beam parallel.beam
	erl -eval "eunit:test([serial, parallel])" -noshell -s init stop

docs:
	erl -noshell -run edoc_run files '["serial.erl", "parallel.erl"]' '[{dir, "doc"}, {private, true}]' -s init stop

clean-docs:
	rm -f doc/edoc-info doc/*.html doc/*.css doc/*.png
