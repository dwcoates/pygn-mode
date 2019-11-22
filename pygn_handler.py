#!/usr/bin/env python
#
# pgn_to_fen
#
# return the FEN after the last move in a PGN input file
#
# notes
#
#     requires python-chess
#
# bugs
#
# todo
#

###
### version
###

__version__ = '1.00'

###
### imports
###

import sys
import argparse
import signal
import io
import re

import chess.pgn
import chess.svg

###
### file-scoped configurable variables
###

CALLBACKS = {
    ":fen": lambda board,args: board.fen(),
    ":board": lambda board,args: chess.svg.board(
        board=board,
        size=args.pixels[0])
}

###
### subroutines
###

def listen():
    """
    Listen for messages on stdin and send response data on stdout.
    """

    argparser = generate_argparser()

    while True:
        input_str = sys.stdin.readline()

        # TODO: test readline and empty-line handling on Windows
        # Handle terminating characters and garbage.
        if len(input_str) == 0:
            # eof
            break
        if input_str == '\n':
            continue

        # Parse request.
        m = re.compile("(:\S+)(.*?) --").search(input_str)
        if (not m):
            print("Bad pgn-mode python process input: {}".format(input_str), file=sys.stderr)
            continue
        args = argparser.parse_args(m.group(2).split())

        # Grab command code for handling input.
        code = m.group(1)
        if code not in CALLBACKS:
            print("Bad request code (unknown): {}".format(code), file=sys.stderr)
            continue

        # Build game board.
        pgn = input_str[input_str.index(m.group(0)) + len(m.group(0)):].strip()
        pgn = re.sub(r'\\n', '\n', pgn)
        pgn = pgn + '\n\n'
        game = chess.pgn.read_game(io.StringIO(pgn))
        board = game.board()
        for move in game.mainline_moves():
            board.push(move)

        # Send response to client.
        print(CALLBACKS[code](board,args))

###
### argument processing
###

def generate_argparser():
    argparser = argparse.ArgumentParser()
    argparser.add_argument('-pixels', '--pixels',
                           metavar='PIXELS',
                           nargs=1,
                           type=int,
                           default=[400],
                           help='set pixel-per-side for the SVG board output. Default is 400.')
    return argparser

###
### main
###

if __name__ == '__main__':
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)

    if sys.argv[1] == '-version' or sys.argv[1] == '--version':
        print(__version__)
        exit(0)

    listen()

#
# Emacs
#
# Local Variables:
# coding: utf-8
# End:
#
# LocalWords:
#
