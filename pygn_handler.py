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
import atexit

import chess.pgn
import chess.svg
import chess.engine

###
### file-scoped variables
###

ENGINES = {}

###
### subroutines
###

def instantiate_engine(engine_path):
    if not engine_path in ENGINES:
        ENGINES[engine_path] = chess.engine.SimpleEngine.popen_uci(engine_path)
    return ENGINES[engine_path]

def cleanup():
    for e in ENGINES.values():
        try:
            e.quit()
        except:
            pass

def board_callback(board,args):
    return chess.svg.board(
        board=board,
        size=args.pixels[0])

def fen_callback(board,args):
    return board.fen()

def score_callback(board,args):
    engine = instantiate_engine(args.engine[0])
    uci_info = engine.analyse(board, chess.engine.Limit(depth=args.depth[0]))
    return uci_info["score"]

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
            cleanup()
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
    argparser.add_argument('-engine', '--engine',
                           nargs=1,
                           type=str,
                           default=["stockfish"],
                           help='set path to UCI engine for analysis. Default is "stockfish".')
    argparser.add_argument('-depth', '--depth',
                           nargs=1,
                           type=int,
                           default=[10],
                           help='set depth for depth-limited to UCI evaluations. Default is 10.')
    return argparser

###
### main
###

if __name__ == '__main__':
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)

    if len(sys.argv) > 1 and (sys.argv[1] == '-version' or sys.argv[1] == '--version'):
        print(__version__)
        exit(0)

    CALLBACKS = {
        ":fen": fen_callback,
        ":board": board_callback,
        ":score": score_callback,
    }

    atexit.register(cleanup)

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
