#!/usr/bin/env python
#
# pygn_server
#
# driver for Emacs mode pygn-mode.el
#
# notes
#
#     requires chess library
#
#     documentation of the server protocol is at doc/server.md
#
# bugs
#
# todo
#

###
### version
###

__version__ = '0.5.1'

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

def pgn_to_board_callback(_game,board,last_move,args):
    if args.board_format[0] == 'svg':
        svg = chess.svg.board(board=board,
                              lastmove=last_move,
                              size=args.pixels[0])
        return f':board-svg {svg}'
    elif args.board_format[0] == 'text':
        text = board.unicode(borders=True)
        text = re.sub(r'·', ' ', text)
        text = re.sub(r'-----------------', '├───┼───┼───┼───┼───┼───┼───┼───┤', text)
        text = re.sub(r'\A  ├───┼───┼───┼───┼───┼───┼───┼───┤', '  ┌───┬───┬───┬───┬───┬───┬───┬───┐', text)
        text = re.sub(r'├───┼───┼───┼───┼───┼───┼───┼───┤\n   a', '└───┴───┴───┴───┴───┴───┴───┴───┘\n   a', text)
        text = re.sub( r'a b c d e f g h',   ' a   b   c   d   e   f   g   h',   text)
        text = re.sub(r'\|', ' │ ', text)
        text = re.sub(r'^(\d) ', '\\1', text, flags=re.MULTILINE)
        text = text.translate(str.maketrans('♜♞♝♛♚♟♖♘♗♕♔♙⭘','RNBQKPrnbqkp '))
        text = re.sub(r'\n', '\\\\n', text)
        return f':board-text {text}'
    else:
        print(f'Bad pgn-mode -board_format value: {args.board_format[0]}', file=sys.stderr)
        return None

def pgn_to_fen_callback(_game,board,_last_move,_args):
    return f':fen {board.fen()}'

def pgn_to_score_callback(_game,board,_last_move,args):
    engine = instantiate_engine(args.engine[0])
    uci_info = engine.analyse(board, chess.engine.Limit(depth=args.depth[0]))
    return f':score {uci_info["score"]}'

def pgn_to_mainline_callback(game,_board,_last_move,_args):
    clean_exporter = chess.pgn.StringExporter(columns=None,
                                              headers=False,
                                              variations=False,
                                              comments=False)
    mainline = game.accept(clean_exporter)
    mainline = re.sub(r'\s+\S+\Z', '', mainline)
    return f':san {mainline}'

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
        match = re.compile(r'\A:version\s+(\S+)\s+(:\S+)(.*?)\s+--\s+(:\S+)\s+(\S.*)\n').search(input_str)
        if not match:
            print(f'Bad pgn-mode server request. Could not parse: {input_str}', file=sys.stderr)
            continue
        [req_version,
         req_command,
         req_options,
         req_payload_type,
         req_payload] = match.groups()

        if not req_version == __version__:
            print(f'Bad request: version mismatch: {req_version}', file=sys.stderr)
            continue

        # Command code for handling input.
        if req_command not in CALLBACKS:
            print(f'Bad request command (unknown): {req_command}', file=sys.stderr)
            continue

        # Options to modify operation of the command.
        try:
            args = argparser.parse_args(shlex.split(req_options))
        except:
            print(f'Bad request options: {req_options}', file=sys.stderr)
            continue

        # :payload-type is for future extensibility, currently always :pgn
        if not req_payload_type == ':pgn':
            print(f'Bad request :payload-type (unknown): {req_payload_type}', file=sys.stderr)
            continue

        # Build game board.
        pgn = req_payload
        pgn = re.sub(r'\\n', '\n', pgn)
        pgn = pgn + '\n\n'
        game = chess.pgn.read_game(io.StringIO(pgn))
        board = game.board()
        last_move = False
        for move in game.mainline_moves():
            last_move = move
            board.push(move)

        # Compute response.
        response = CALLBACKS[req_command](game,board,last_move,args)

        # Send response to client.
        if response:
            print(f':version {__version__} {response}')

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
    argparser.add_argument('-board_format', '--board_format',
                           nargs=1,
                           type=str,
                           default=['svg'],
                           help='format for board output.  Default is "svg".')
    argparser.add_argument('-engine', '--engine',
                           nargs=1,
                           type=str,
                           default=['stockfish'],
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
        sys.exit(0)

    CALLBACKS = {
        ':pgn-to-fen': pgn_to_fen_callback,
        ':pgn-to-board': pgn_to_board_callback,
        ':pgn-to-score': pgn_to_score_callback,
        ':pgn-to-mainline': pgn_to_mainline_callback,
    }

    atexit.register(cleanup)

    print(f'Server started.')

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
