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
import textwrap
import signal
import io
import re

import chess.pgn
import chess.svg

###
### file-scoped configurable variables
###

# TODO: breaks pixel configuration
CALLBACKS = {
    "1": lambda board: board.fen(),
    "2": lambda board: chess.svg.board(board=board),
}

###
### subroutines
###

def listen():
    """
    Listen for messages on stdin and send response data on stdout.
    """

    while True:
        input_str = sys.stdin.readline()

        # Handle terminating characters and garbage.
        # TODO: make more safe.
        if len(input_str) == 0:
            # eof
            break
        if len(input_str) == 1:
            # just newline
            continue

        # Parse request.
        m = re.compile("([0-9]+) --").search(input_str)
        if (not m):
            print("Bad pgn-mode python process input: {}".format(input_str), file=sys.stderr)
            continue

        # Grab command code for handling input.
        code = m.group(1)
        if code not in CALLBACKS:
            print("Bad request code (unknown): {}".format(code), file=sys.stderr))
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
        print(CALLBACKS[code](board))

###
### argument processing
###

def generate_argparser():
    argparser = argparse.ArgumentParser(description=textwrap.dedent(
                                        '''
                                        Return the FEN after the last move in a PGN <file>.
                                        '''),
                                        formatter_class=argparse.RawDescriptionHelpFormatter)
    argparser.add_argument('file',
                           metavar='<file>',
                           nargs='*',
                           type=argparse.FileType('r'),
                           help='File to analyze.  Input on the standard input is also accepted.')
    argparser.add_argument('-quiet', '--quiet',
                           action='store_true',
                           help='Emit less diagnostic output.')
    argparser.add_argument('-verbose', '--verbose',
                           action='store_true',
                           help='Emit more diagnostic output.')
    argparser.add_argument('-help',
                           action='help',
                           help=argparse.SUPPRESS)
    argparser.add_argument('-version', '--version',
                           action='version',
                           version=__version__)
    return argparser

###
### main
###


if __name__ == '__main__':
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)

    ARGPARSER = generate_argparser()
    if sys.stdin.isatty() and len(sys.argv) == 1:
        args = ARGPARSER.parse_args(['--help'])
    else:
        args = ARGPARSER.parse_args()

    if args.quiet and args.verbose:
        print('pgn_to_fen: -quiet and -verbose are incompatible', file=sys.stderr)
        exit(1)

    input_files = args.file
    if not sys.stdin.isatty():
        input_files = [sys.stdin, *input_files]


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
