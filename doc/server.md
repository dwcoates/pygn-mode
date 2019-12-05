# Overview

Many features of `pygn-mode` are powered by a Python server using the
[python-chess](https://pypi.org/project/python-chess/) library.

The server script `pygn_server.py` targets Python 3 (3.5 or newer).

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
<command> [<options>] -- <payload-type> <payload-data>
```

Note that `<options>` are not required, but a single double-dash `--` is
mandatory.

Example:

```
:pgn-to-board -pixels=200 -- :pgn [Event "?"]\n[Site ...
```

### `<command>`

The request `<command>` determines the action taken by the server.  Commands
always begin with `:`.  Commands known at the time of writing are

 * `:pgn-to-fen` -- render a FEN from a PGN payload
 * `:pgn-to-board` -- render a board image from a PGN payload
 * `:pgn-to-score` -- render an engine score from a PGN payload

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

### double-dash separator

A single double-dash `--` before `<payload-type>` is mandatory, even if
`<options>` are not present.

### `<payload-type>`

`<payload-type>` defines the type of the data payload to follow.  It always
begins with `:`.  Request payload types known at the time of writing are

 * command `:pgn-to-fen` -- request `<payload-type>` `:pgn`
 * command `:pgn-to-board` -- request `<payload-type>` `:pgn`
 * command `:pgn-to-score` -- request `<payload-type>` `:pgn`

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
<payload-type> <payload-data>
```

Example:

```
:fen r1bRQ3/ppp3pp/4p3/2b1P1k1/1np1q3/8/PPP3PP/5R1K w - - 0 20
```

### `<payload-type>`

`<payload-type>` defines the type of the data payload to follow.  It always
begins with `:`.  Response payload types known at the time of writing are

 * `:fen`
 * `:board-svg`
 * `:score`

### `<payload-data>`

 * `<payload-data>` cannot begin with whitespace.
 * Since each response is confined to a single line, it cannot contain newlines.
 * It should not contain the EOF character (control-D).
 * There are otherwise no restrictions.