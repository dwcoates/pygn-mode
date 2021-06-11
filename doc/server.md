# Overview

Many features of `pygn-mode` are powered by a Python server using the
[chess](https://pypi.org/project/chess/) library.

The server script `pygn_server.py` targets Python 3 (3.6 or newer).

# Development Mode

When executed by Emacs, the server runs persistently, and accepts single-line
requests on the standard input.

The server also exits when it reaches a normal EOF.  For development purposes,
the server can be driven in single-shot mode at the command line:

```
cat doc/examples/example_request.txt | python pygn_server.py
```

# Server Protocol

## Request Format

Requests to the `pygn_server.py` server are on a single line, terminated by
newline, in the form

```
:version <version> <command> [<options>] -- <payload-type> <payload-data>
```

Note that `<options>` are not required, but a single double-dash `--` is
mandatory.

Example:

```
:version 0.5.0 :pgn-to-board -pixels=200 -- :pgn [Event "?"]\n[Site ...
```

### `<version>`

The request `<version>` string must agree between the client and server.  If
the version string does not agree, the server may refuse to respond.

### `<command>`

The request `<command>` determines the action taken by the server.  Commands
always begin with `:`.  Commands known at the time of writing are

 * `:pgn-to-fen` -- render a FEN from a PGN payload
 * `:pgn-to-board` -- render a board image from a PGN payload
 * `:pgn-to-score` -- render an engine score from a PGN payload
 * `:pgn-to-mainline` -- render the main line from a PGN payload

### `<options>`

`<options>` are CLI-like flags and key-value pairs, with leading dashes on
keys, as accepted by the Python library [argparse](https://docs.python.org/3/library/argparse.html).  Options known at the time
of writing are

 * `:pgn-to-fen`
   - _none_
 * `:pgn-to-board`
   - `-pixels=<int>` -- the size of the board, corresponding to Elisp customizable variable `pygn-mode-board-size`
 * `:pgn-to-score`
   - `-engine=<path>` -- path to a UCI engine executable
   - `-depth=<int>` -- depth to which to limit the evaluation
 * `:pgn-to-mainline`
   - _none_

### double-dash separator

A single double-dash `--` before `<payload-type>` is mandatory, even if
`<options>` are not present.

### `<payload-type>`

`<payload-type>` defines the type of the data payload to follow.  It always
begins with `:`.  Request payload types known at the time of writing are

 * command `:pgn-to-fen` -- request `<payload-type>` `:pgn`
 * command `:pgn-to-board` -- request `<payload-type>` `:pgn`
 * command `:pgn-to-score` -- request `<payload-type>` `:pgn`
 * command `:pgn-to-mainline` -- request `<payload-type>` `:pgn`

### `<payload-data>`

 * `<payload-data>` cannot begin with whitespace.
 * Since each request is confined to a single line, it cannot contain newlines.
 * It should not contain the EOF character (control-D).
 * There are otherwise no restrictions.

`<payload-data>` is usually a PGN with newlines escaped to "\n".

## Response Format

Responses from the `pygn_server.py` server are on a single line, terminated by
newline, in the form

```
:version <version> <payload-type> <payload-data>
```

Example:

```
:version 0.5.0 :fen r1bRQ3/ppp3pp/4p3/2b1P1k1/1np1q3/8/PPP3PP/5R1K w - - 0 20
```

### `<version>`

The response `<version>` string must agree between the client and server.  If
the version string does not agree, the client may attempt to restart the
server.

### `<payload-type>`

`<payload-type>` defines the type of the data payload to follow.  It always
begins with `:`.  Response payload types known at the time of writing are

 * `:fen`
 * `:board-svg`
 * `:score`
 * `:san`

### `<payload-data>`

 * `<payload-data>` cannot begin with whitespace.
 * Since each response is confined to a single line, it cannot contain newlines.
 * It should not contain the EOF character (control-D).
 * There are otherwise no restrictions.
