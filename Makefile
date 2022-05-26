A = .a
O = .o
B = .beam
E =
.SUFFIXES : .h .c .i $O $E .hrl .erl .beam .sh

PROJ		:= httpb

BIN		:= _build/default/bin
ELIB		:= _build/default/lib
EBIN		:= ${ELIB}/${PROJ}/ebin
ERLC_FLAGS	:= -o${EBIN}

$E$B:
	erlc ${ERLC_FLAGS} $@

all: build

build:
	rebar3 compile

clean:
	-rm -rf src/*$B test/*$B ./*$B *dump *.core

distclean: clean
	-rm -rf _build _checkouts ebin

tar:
	git archive --format tar.gz --prefix ${PROJ}/ -o ${PROJ}.tar.gz HEAD

test: dialyzer ct
	rebar3 cover

dialyzer:
	rebar3 cover --reset
	-rebar3 dialyzer

ct:
	-rebar3 ct --cover

logs: test
	lynx _build/test/logs/index.html

view:
	lynx _build/test/cover/index.html

