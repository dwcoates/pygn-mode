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
import re
import io
import argparse
import textwrap
import signal

import chess.pgn

###
### file-scoped configurable variables
###

###
### subroutines
###

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

callbacks = {
    "1": lambda board: print(board.fen()),
    "2": lambda board: print(chess.svg.board(board=board, size=args.pixels[0])),
}

if __name__ == '__main__':
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)

    argparser = generate_argparser()
    if sys.stdin.isatty() and len(sys.argv) == 1:
        args = argparser.parse_args(['--help'])
    else:
        args = argparser.parse_args()

    if args.quiet and args.verbose:
        print('pgn_to_fen: -quiet and -verbose are incompatible', file=sys.stderr)
        exit(1)



    while True:
        input_str = sys.stdin.read()
        p = re.compile("([0-9]+) --")
        m = p.search(input_str)

        if (not m):
            print("Bad pgn-mode python process input: {}".format(input_str))
            continue

        code = m.group(1) # Command code for handling input.
        pgn = input_str[input_str.index(m.group(0)) + len(m.group(0)):].strip()

        game = chess.pgn.read_game(io.StringIO(pgn))
        board = game.board()

        for move in game.mainline_moves():
            board.push(move)

        print(callbacks[code](board))
# Emacs
#
# Local Variables:
# coding: utf-8
# End:
#
# LocalWords:
#
