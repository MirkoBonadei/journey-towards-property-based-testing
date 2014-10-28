QCLIB := lib/eqc-1.0.1/ebin/
all:
		erlc -o ebin/ -pa $(QCLIB) src/*.erl test/*.erl
.PHONY:	test
test:	all
		erl -pa ebin/ -pa $(QCLIB) -eval 'eqc:module($(mod))' -eval 'init:stop()'
