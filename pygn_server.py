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

__version__ = '0.6.3'

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
import logging
import traceback

import chess.pgn
import chess.svg
import chess.engine

###
### logging
###

def setup_logging(log_file=None, debug=False):
    """Configure the [pygn-mode] logger.

    Format matches the Elisp side: HH:MM:SS.mmm [pygn-mode] <message>.
    No severity level in the output — matches claude-repl convention.

    File sink (when log_file is given):
      debug=False  captures INFO+  (pgn_log lines, not pgn_log_verbose)
      debug=True   captures DEBUG+ (both pgn_log and pgn_log_verbose)

    Stderr sink:
      debug=False  captures WARNING+ only (errors surfaced to Emacs)
      debug=True   captures DEBUG+ (all lines visible during debugging)
    """
    fmt = logging.Formatter('%(asctime)s.%(msecs)03d [pygn-mode] %(message)s',
                            datefmt='%H:%M:%S')
    root = logging.getLogger('pygn-mode')
    root.setLevel(logging.DEBUG)

    stderr_handler = logging.StreamHandler(sys.stderr)
    stderr_handler.setLevel(logging.DEBUG if debug else logging.WARNING)
    stderr_handler.setFormatter(fmt)
    root.addHandler(stderr_handler)

    if log_file:
        try:
            file_handler = logging.FileHandler(log_file, encoding='utf-8')
            file_handler.setLevel(logging.DEBUG if debug else logging.INFO)
            file_handler.setFormatter(fmt)
            root.addHandler(file_handler)
        except OSError as e:
            root.warning("Could not open log file %s: %s", log_file, e)

    return root

_logger = logging.getLogger('pygn-mode')

def pgn_log(msg, *args):
    """Standard log: always to file (when configured), to stderr when --debug.
    Mirrors Elisp pygn-mode--log: file-always, echo-when-debug."""
    _logger.info(msg, *args)

def pgn_log_verbose(msg, *args):
    """Verbose log: file and stderr only when --debug is active.
    Mirrors Elisp pygn-mode--log-verbose: only in verbose/debug mode."""
    _logger.debug(msg, *args)

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

def pgn_to_board_callback(_game,board,last_move,last_fen,args):
    if args.board_format[0] == 'svg':
        svg = chess.svg.board(board=board,
                              lastmove=last_move,
                              size=args.pixels[0],
                              flipped=args.flipped)
        svg = re.sub(r'\r?\n', r' ', svg)
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
        text = text.translate(str.maketrans('♖♘♗♕♔♙♜♞♝♛♚♟⭘','RNBQKPrnbqkp '))
        text = re.sub(r'\n', '\\\\n', text)
        return f':board-text {text}'
    else:
        pgn_log("pgn-to-board-callback: bad board_format value=%s", args.board_format[0])
        return None

def pgn_to_fen_callback(_game,board,_last_move,_last_fen,_args):
    return f':fen {board.fen()}'

def pgn_to_score_callback(_game,board,_last_move,_last_fen,args):
    engine = instantiate_engine(args.engine[0])
    uci_info = engine.analyse(board, chess.engine.Limit(depth=args.depth[0]))
    return f':score {uci_info["score"]}'

def pgn_to_mainline_callback(game,_board,_last_move,_last_fen,_args):
    clean_exporter = chess.pgn.StringExporter(columns=None,
                                              headers=False,
                                              variations=False,
                                              comments=False)
    mainline = game.accept(clean_exporter)
    mainline = re.sub(r'\s+\S+\Z', '', mainline)
    return f':san {mainline}'

# todo should all responses be in sexp form?
def pgn_to_last_move_info_callback(_game,_board,last_move,last_fen,_args):
    return f':last-move-info (:fen "{shlex.quote(last_fen)}" :move-uci "{shlex.quote(last_move.uci())}")'

def listen():
    """
    Listen for messages on stdin and send response data on stdout.
    """

    argparser = generate_argparser()
    pgn_log_verbose("listen: entering request loop")

    while True:
        input_str = sys.stdin.readline()

        # TODO: test readline and empty-line handling on Windows
        # Handle terminating characters and garbage.
        if len(input_str) == 0:
            # eof
            pgn_log_verbose("listen: EOF received, shutting down")
            cleanup()
            break
        if input_str == '\n':
            continue

        pgn_log_verbose("listen: request received len=%d", len(input_str))

        # Parse request.
        match = re.compile(r'\A:version\s+(\S+)\s+(:\S+)(.*?)\s+--\s+(:\S+)\s+(\S.*)\n').search(input_str)
        if not match:
            pgn_log("listen: could not parse request: %r", input_str[:200])
            continue
        [req_version,
         req_command,
         req_options,
         req_payload_type,
         req_payload] = match.groups()

        pgn_log_verbose("listen: cmd=%s payload-type=%s payload-len=%d",
                        req_command, req_payload_type, len(req_payload))

        if not req_version == __version__:
            pgn_log("listen: version mismatch expected=%s got=%s",
                    __version__, req_version)
            continue

        # Command code for handling input.
        if req_command not in CALLBACKS:
            pgn_log("listen: unknown command=%s", req_command)
            continue

        # Options to modify operation of the command.
        try:
            args = argparser.parse_args(shlex.split(req_options))
        except Exception as e:
            pgn_log("listen: bad request options=%r error=%s", req_options, e)
            continue

        # :payload-type is for future extensibility, currently always :pgn
        if not req_payload_type == ':pgn':
            pgn_log("listen: unknown payload-type=%s", req_payload_type)
            continue

        # Build game board.
        pgn = req_payload
        pgn = re.sub(r'\\n', '\n', pgn)
        pgn = pgn + '\n\n'
        try:
            game = chess.pgn.read_game(io.StringIO(pgn))
            if game is None:
                pgn_log("listen: chess.pgn.read_game returned None for cmd=%s", req_command)
                continue
        except Exception as e:
            pgn_log("listen: PGN parse error cmd=%s: %s\n%s",
                    req_command, e, traceback.format_exc())
            continue

        board = game.board()
        last_move = False
        last_fen = board.fen()
        for move in game.mainline_moves():
            last_move = move
            last_fen = board.fen()
            board.push(move)

        # Compute response.
        try:
            response = CALLBACKS[req_command](game, board, last_move, last_fen, args)
        except Exception as e:
            pgn_log("listen: callback error cmd=%s: %s\n%s",
                    req_command, e, traceback.format_exc())
            continue

        # Send response to client.
        if response:
            pgn_log_verbose("listen: sending response type=%s len=%d",
                            response.split()[0] if response else '-', len(response))
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
    argparser.add_argument('-flipped', '--flipped',
                           action='store_true',
                           help='display board flipped (Black perspective).')
    return argparser

###
### main
###

if __name__ == '__main__':
    try:
        signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    except:
        pass

    if len(sys.argv) > 1 and (sys.argv[1] == '-version' or sys.argv[1] == '--version'):
        print(__version__)
        sys.exit(0)

    # Parse startup arguments before entering the request loop.
    startup_parser = argparse.ArgumentParser(add_help=False)
    startup_parser.add_argument('-', dest='stdin_mode', action='store_true',
                                help='read requests from stdin (server mode)')
    startup_parser.add_argument('--log-file', dest='log_file', default=None,
                                metavar='PATH',
                                help='append [pygn-mode] log lines to PATH')
    startup_parser.add_argument('--debug', dest='debug', action='store_true',
                                help='enable DEBUG-level logging to stderr')
    startup_args, _ = startup_parser.parse_known_args()

    setup_logging(log_file=startup_args.log_file, debug=startup_args.debug)
    pgn_log("server starting version=%s interpreter=%s", __version__, sys.executable)

    CALLBACKS = {
        ':pgn-to-fen': pgn_to_fen_callback,
        ':pgn-to-board': pgn_to_board_callback,
        ':pgn-to-score': pgn_to_score_callback,
        ':pgn-to-mainline': pgn_to_mainline_callback,
        ':pgn-to-last-move-info': pgn_to_last_move_info_callback,
    }

    atexit.register(cleanup)

    print(f'Server started.')
    pgn_log("server ready version=%s", __version__)

    listen()
    pgn_log("server exiting")

#
# Emacs
#
# Local Variables:
# coding: utf-8
# End:
#
# LocalWords:
#
