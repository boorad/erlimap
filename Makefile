EFLAGS=+debug_info
ESOURCES=$(wildcard *.erl)
EHEADERS=$(wildcard *.hrl)

all: $(ESOURCES:.erl=.beam)
	ctags $(ESOURCES) $(EHEADERS)

%.beam: %.erl $(EHEADERS)
	erlc $(EFLAGS) $<

clean:
	rm -f tags *.beam erl_crash.dump

test: $(ESOURCES:.erl=.beam)
	erl -noshell -run imap_fsm test -run init stop
