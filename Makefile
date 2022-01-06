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
	-rm -rf src/*$B ./*$B *dump *.core

distclean: clean
	-rm -rf _build _checkouts ebin

tar:
	git archive --format tar.gz --prefix ${PROJ}/ -o ${PROJ}.tar.gz HEAD

test: dialyzer unit

dialyzer:
	rebar3 dialyzer

unit:
	rebar3 eunit --cover
	rebar3 cover

cover: unit
	lynx _build/test/cover/index.html
