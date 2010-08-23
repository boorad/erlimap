EFLAGS=+debug_info
ESOURCES=$(wildcard *.erl)
EHEADERS=$(wildcard *.hrl)

all: $(ESOURCES:.erl=.beam)
	ctags $(ESOURCES) $(EHEADERS)

%.beam: %.erl $(EHEADERS)
	erlc $(EFLAGS) $<

clean:
	rm -f tags *.beam erl_crash.dump

test:
	erl -noshell -run imap test -run init stop
