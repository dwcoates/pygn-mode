#!/usr/bin/env python
#
# pygn_server
#
# driver for Emacs mode pygn-mode.el
#
# notes
#
#     requires python-chess
#
#     documentation of the server request format is at doc/server.md
#
# bugs
#
# todo
#

###
### version
###

__version__ = '0.50'

###
### imports
###

import sys
import argparse
import signal
import io
import re
import atexit
import shlex

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

def board_callback(board,last_move,args):
    return ':board-svg ' + chess.svg.board(
        board=board,
        lastmove=last_move,
        size=args.pixels[0])

def fen_callback(board,last_move,args):
    return ':fen ' + board.fen()

def score_callback(board,last_move,args):
    engine = instantiate_engine(args.engine[0])
    uci_info = engine.analyse(board, chess.engine.Limit(depth=args.depth[0]))
    return ':score ' + str(uci_info["score"])

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
        m = re.compile("(:\S+)(.*?)\s+--\s+(:\S+)\s+(\S.*)\n").search(input_str)
        if (not m):
            print("Bad pgn-mode python process input: {}".format(input_str), file=sys.stderr)
            continue

        # Command code for handling input.
        command = m.group(1)
        if command not in CALLBACKS:
            print("Bad request command (unknown): {}".format(command), file=sys.stderr)
            continue

        # Options to modify operation of the command.
        try:
            args = argparser.parse_args(shlex.split(m.group(2)))
        except:
            print("Bad request options: {}".format(m.group(2)), file=sys.stderr)
            continue

        # Payload_type is for future extensibility, currently always :pgn
        payload_type = m.group(3)
        if not payload_type == ":pgn":
            print("Bad request :payload-type (unknown): {}".format(payload_type), file=sys.stderr)
            continue

        # Build game board.
        pgn = m.group(4)
        pgn = re.sub(r'\\n', '\n', pgn)
        pgn = pgn + '\n\n'
        game = chess.pgn.read_game(io.StringIO(pgn))
        board = game.board()
        last_move = False
        for move in game.mainline_moves():
            last_move = move
            board.push(move)

        # Send response to client.
        print(CALLBACKS[command](board,last_move,args))

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
        ":pgn-to-fen": pgn_to_fen_callback,
        ":pgn-to-board": pgn_to_board_callback,
        ":pgn-to-score": pgn_to_score_callback,
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
