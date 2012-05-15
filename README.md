# erlimap

This is an attempt at making an Erlang IMAP client library.  It is not complete.

## Mostly Complete Commands

* connect/disconnect (tcp or ssl)
* LOGIN
* EXAMINE (read-only)
* SELECT  (read-write)
* SEARCH
* FETCH
* STORE
    * . store 1 flags \Seen
    * . store 1,2 -flags \Seen
* NOOP

## Releated Links

* http://www.imapwiki.org/
* http://dovecot.org/imap-client-coding-howto.html
* http://dovecot.org/client-commandments.txt
* http://bobpeers.com/technical/telnet_imap

## Other Notes

Further, it is not intelligent about clearing out the fsm's list of untagged responses, and matching them up with the command to which the server was responding.

Lastly, it is a regex shitshow that needs to be converted to a grammar/parser rig, something like [neotoma](http://github.com/seancribbs/neotoma), like what was done in [diemap](http://github.com/vagabond/diemap).

LICENSE: BSD