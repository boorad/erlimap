# erlimap

This is an attempt at making an Erlang IMAP client library.  It is not complete.

## Mostly Complete Commands

* LOGIN
* EXAMINE
* SEARCH
* FETCH (ish)

## Other Notes

Further, it is not intelligent about clearing out the fsm's list of untagged responses, and matching them up with the command to which the server was responding.

Lastly, it is a regex shitshow that needs to be converted to a grammar/parser rig, something like [neotoma](http://github.com/seancribbs/neotoma), like what was done in [diemap](http://github.com/vagabond/diemap).
