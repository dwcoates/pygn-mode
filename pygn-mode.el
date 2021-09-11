;;; pygn-mode.el --- Major-mode for chess PGN files, powered by Python -*- lexical-binding: t -*-

;; Copyright (c) 2019-2021 Dodge Coates and Roland Walker

;; Author: Dodge Coates and Roland Walker
;; Homepage: https://github.com/dwcoates/pygn-mode
;; URL: https://raw.githubusercontent.com/dwcoates/pygn-mode/master/pygn-mode.el
;; Version: 0.6.0
;; Last-Updated: 06 Aug 2021
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.2") (tree-sitter-langs "0.10.7") (uci-mode "0.5.4") (nav-flash "1.0.0") (ivy "0.10.0"))
;; Keywords: data, games, chess

;;; License

;; Simplified BSD License:

;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:

;;   1. Redistributions of source code must retain the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer.

;;   2. Redistributions in binary form must reproduce the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer in the documentation and/or other materials
;;      provided with the distribution.

;; This software is provided by the authors "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall the authors or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.

;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of the authors.

;;; Internal notes

;; Bugs

;; TODO

;;     Extensive ert test coverage of
;;      - pygn-mode-pgn-at-pos-as-if-variation

;;     pygn-mode-go-searchmoves which defaults to searching move under point

;;     Flash current move on selection

;; IDEA

;;     UCI moves to pgn: UCI position command arguments to pgn and/or graphical display

;;     count games in current file? Display in modeline?

;;     evil text objects?

;;; Commentary:

;; Quickstart

;;     (require 'pygn-mode)

;;     M-x pygn-mode-run-diagnostic

;; Explanation

;;     Pygn-mode is a major-mode for viewing and editing chess PGN files.
;;     Directly editing PGN files is interesting for programmers who are
;;     developing chess engines, or advanced players who are doing deep
;;     analysis on games.  This mode is not useful for simply playing chess.

;; Bindings

;;     No keys are bound by default.  Consider

;;         (eval-after-load "pygn-mode"
;;           (define-key pygn-mode-map (kbd "M-f") 'pygn-mode-next-move)
;;           (define-key pygn-mode-map (kbd "M-b") 'pygn-mode-previous-move))

;; Customization

;;     M-x customize-group RET pygn RET

;; See Also

;;     http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm

;;     https://github.com/dwcoates/uci-mode

;; Prior Art

;;     https://github.com/jwiegley/emacs-chess

;; Notes

;; Compatibility and Requirements

;;     GNU Emacs 26.1+, compiled with dynamic module support

;;     tree-sitter.el and tree-sitter-langs.el

;;     Python and the chess library are needed for numerous features such
;;     as SVG board images:

;;         https://pypi.org/project/chess/

;;     A version of the Python chess library is bundled with this package.
;;     Note that the chess library has its own license (GPL3+).

;;; Code:

(defconst pygn-mode-version "0.6.0")

;;; Imports

(require 'cl-lib)
(require 'comint)
(require 'uci-mode nil t)
(require 'nav-flash nil t)
(require 'ivy nil t)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)

;;; Declarations

(eval-when-compile
  (require 'subr-x)
  (defvar nav-flash-delay)
  (defvar uci-mode-engine-buffer)
  (defvar uci-mode-engine-buffer-name))

(declare-function uci-mode-engine-proc   "uci-mode.el")
(declare-function uci-mode-run-engine    "uci-mode.el")
(declare-function uci-mode-send-stop     "uci-mode.el")
(declare-function uci-mode-send-commands "uci-mode.el")

(declare-function ivy-completing-read "ivy.el")

;;; Customizable variables

;;;###autoload
(defgroup pygn nil
  "A major-mode for chess PGN files, powered by Python."
  :version pygn-mode-version
  :prefix "pygn-mode-"
  :group 'data
  :group 'games)

(defcustom pygn-mode-python-executable "python"
  "Path to a Python 3.7+ interpreter."
  :group 'pygn
  :type 'string)

(defcustom pygn-mode-pythonpath
  (expand-file-name
   "lib/python/site-packages"
   (file-name-directory
    (or load-file-name
        (bound-and-true-p byte-compile-current-file)
        (buffer-file-name (current-buffer)))))
  "A colon-delimited path to prepend to the `$PYTHONPATH' environment variable.

The default points to the bundled Python `chess' library.  Set to nil to
ignore the bundled library and use only the system `$PYTHONPATH'."
  :group 'pygn
  :type 'string)

(defcustom pygn-mode-board-size 400
  "Size for graphical board display, expressed as pixels-per-side."
  :group 'pygn
  :type 'int)

(defcustom pygn-mode-board-flipped nil
  "If non-nil, display the board flipped."
  :group 'pygn-mode
  :type 'boolean)

(defcustom pygn-mode-flash-full-game nil
  "If non-nil, flash the entire PGN on game selection actions."
  :group 'pygn
  :type 'boolean)

(defcustom pygn-mode-default-engine-depth 20
  "Default depth for engine analysis."
  :group 'pygn
  :type 'int)

(defcustom pygn-mode-default-engine-time 15
  "Default seconds for engine analysis."
  :group 'pygn
  :type 'int)

(defcustom pygn-mode-server-stderr-buffer-name nil
  "Buffer name for server stderr output, nil to redirect stderr to null device."
  :group 'pygn
  :type 'string)

;;;###autoload
(defgroup pygn-faces nil
  "Faces used by pygn-mode."
  :group 'pygn)

(defface pygn-mode-tagpair-key-face
  '((t (:inherit font-lock-keyword-face)))
  "pygn-mode face for tagpair (header) keys."
  :group 'pygn-faces)

(defface pygn-mode-tagpair-value-face
  '((t (:inherit font-lock-string-face)))
  "pygn-mode face for tagpair (header) values."
  :group 'pygn-faces)

(defface pygn-mode-tagpair-bracket-face
   '((t (:foreground "Gray50")))
  "pygn-mode face for tagpair (header) square brackets."
  :group 'pygn-faces)

(defface pygn-mode-annotation-face
  '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for annotation symbols."
  :group 'pygn-faces)

(defface pygn-mode-inline-comment-face
  '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for inline comments."
  :group 'pygn-faces)

(defface pygn-mode-rest-of-line-comment-face
  '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for rest-of-line comments."
  :group 'pygn-faces)

(defface pygn-mode-twic-section-comment-face
  '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for TWIC-style section comments."
  :group 'pygn-faces)

(defface pygn-mode-move-face
  '((t (:inherit default)))
  "pygn-mode face for moves."
  :group 'pygn-faces)

(defface pygn-mode-move-number-face
  '((t (:inherit default)))
  "pygn-mode face for move numbers."
  :group 'pygn-faces)

(defface pygn-mode-variation-move-face
  '((t (:foreground "Gray50")))
  "pygn-mode face for variation moves."
  :group 'pygn-faces)

(defface pygn-mode-variation-move-number-face
  '((t (:foreground "Gray50")))
  "pygn-mode face for variation move numbers."
  :group 'pygn-faces)

(defface pygn-mode-variation-delimiter-face
  '((t (:foreground "Gray50")))
  "pygn-mode face for variation delimiters."
  :group 'pygn-faces)

(defface pygn-mode-variation-annotation-face
  '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for annotation symbols within variations."
  :group 'pygn-faces)

(defface pygn-mode-variation-inline-comment-face
  '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for inline comments within variations."
  :group 'pygn-faces)

(defface pygn-mode-variation-rest-of-line-comment-face
  '((t (:inherit font-lock-comment-face)))
  "pygn-mode face for rest-of-line comments within variations."
  :group 'pygn-faces)

(defface pygn-mode-result-face
  '((t (:inherit font-lock-builtin-face)))
  "pygn-mode face for result codes."
  :group 'pygn-faces)

(defface pygn-mode-invalid-face
  '((t (:inherit font-lock-warning-face)))
  "pygn-mode face for spans of text which are not valid PGN."
  :group 'pygn-faces)

(define-obsolete-face-alias
  'pygn-mode-nag-face
  'pygn-mode-annotation-face
  "0.6.0")

;;; Variables

(defvar pygn-mode-script-directory
  (file-name-directory
   (or load-file-name
       (bound-and-true-p byte-compile-current-file)
       (buffer-file-name (current-buffer))))
  "Directory to find Python server script \"pygn_server.py\".")

(defvar pygn-mode-python-chess-succeeded nil
  "Whether a simple external command using the Python chess library has succeeded.")

(defvar pygn-mode-fen-buffer-name "*pygn-mode-fen*"
  "Buffer name used to display FENs.")

(defvar pygn-mode-board-buffer-name "*pygn-mode-board*"
  "Buffer name used to display boards.")

(defvar pygn-mode-line-buffer-name "*pygn-mode-line*"
  "Buffer name used to display SAN lines.")

(defvar pygn-mode-diagnostic-output-buffer-name "*pygn-mode-diagnostic-output*"
  "Buffer name used to display results of a diagnostic check.")

(defvar pygn-mode--server-process nil
  "Python-based server which powers many `pygn-mode' features.")

(defvar pygn-mode--server-buffer-name " *pygn-mode-server*"
  "Buffer name used to associate a server process.")

(defvar pygn-mode--server-buffer nil
  "Buffer to which the `pygn-mode' server process is associated.")

(defvar pygn-mode--server-receive-every-seconds 0.01
  "How often `pygn-mode--server-receive' should check the server for output when polling.")

(defvar pygn-mode--server-receive-max-seconds 0.5
  "The maximum amount of time `pygn-mode--server-receive' should check the server for output when polling.")

(defvar pygn-mode-annotation-names
  (let ((names (make-hash-table :test 'equal)))
    (puthash "$1"   "Good move"                                                      names)
    (puthash "$2"   "Poor move or mistake"                                           names)
    (puthash "$3"   "Very good or brilliant move"                                    names)
    (puthash "$4"   "Very poor move or blunder"                                      names)
    (puthash "$5"   "Speculative or interesting move"                                names)
    (puthash "$6"   "Questionable or dubious move"                                   names)
    (puthash "$7"   "Forced move (all others lose quickly) or only move"             names)
    (puthash "$8"   "Singular move (no reasonable alternatives)"                     names)
    (puthash "$9"   "Worst move"                                                     names)
    (puthash "$10"  "Drawish position or even"                                       names)
    (puthash "$11"  "Equal chances, quiet position"                                  names)
    (puthash "$12"  "Equal chances, active position"                                 names)
    (puthash "$13"  "Unclear position"                                               names)
    (puthash "$14"  "White has a slight advantage"                                   names)
    (puthash "$15"  "Black has a slight advantage"                                   names)
    (puthash "$16"  "White has a moderate advantage"                                 names)
    (puthash "$17"  "Black has a moderate advantage"                                 names)
    (puthash "$18"  "White has a decisive advantage"                                 names)
    (puthash "$19"  "Black has a decisive advantage"                                 names)
    (puthash "$20"  "White has a crushing advantage (Black should resign)"           names)
    (puthash "$21"  "Black has a crushing advantage (White should resign)"           names)
    (puthash "$22"  "White is in zugzwang"                                           names)
    (puthash "$23"  "Black is in zugzwang"                                           names)
    (puthash "$24"  "White has a slight space advantage"                             names)
    (puthash "$25"  "Black has a slight space advantage"                             names)
    (puthash "$26"  "White has a moderate space advantage"                           names)
    (puthash "$27"  "Black has a moderate space advantage"                           names)
    (puthash "$28"  "White has a decisive space advantage"                           names)
    (puthash "$29"  "Black has a decisive space advantage"                           names)
    (puthash "$30"  "White has a slight time (development) advantage"                names)
    (puthash "$31"  "Black has a slight time (development) advantage"                names)
    (puthash "$32"  "White has a moderate time (development) advantage"              names)
    (puthash "$33"  "Black has a moderate time (development) advantage"              names)
    (puthash "$34"  "White has a decisive time (development) advantage"              names)
    (puthash "$35"  "Black has a decisive time (development) advantage"              names)
    (puthash "$36"  "White has the initiative"                                       names)
    (puthash "$37"  "Black has the initiative"                                       names)
    (puthash "$38"  "White has a lasting initiative"                                 names)
    (puthash "$39"  "Black has a lasting initiative"                                 names)
    (puthash "$40"  "White has the attack"                                           names)
    (puthash "$41"  "Black has the attack"                                           names)
    (puthash "$42"  "White has insufficient compensation for material deficit"       names)
    (puthash "$43"  "Black has insufficient compensation for material deficit"       names)
    (puthash "$44"  "White has sufficient compensation for material deficit"         names)
    (puthash "$45"  "Black has sufficient compensation for material deficit"         names)
    (puthash "$46"  "White has more than adequate compensation for material deficit" names)
    (puthash "$47"  "Black has more than adequate compensation for material deficit" names)
    (puthash "$48"  "White has a slight center control advantage"                    names)
    (puthash "$49"  "Black has a slight center control advantage"                    names)
    (puthash "$50"  "White has a moderate center control advantage"                  names)
    (puthash "$51"  "Black has a moderate center control advantage"                  names)
    (puthash "$52"  "White has a decisive center control advantage"                  names)
    (puthash "$53"  "Black has a decisive center control advantage"                  names)
    (puthash "$54"  "White has a slight kingside control advantage"                  names)
    (puthash "$55"  "Black has a slight kingside control advantage"                  names)
    (puthash "$56"  "White has a moderate kingside control advantage"                names)
    (puthash "$57"  "Black has a moderate kingside control advantage"                names)
    (puthash "$58"  "White has a decisive kingside control advantage"                names)
    (puthash "$59"  "Black has a decisive kingside control advantage"                names)
    (puthash "$60"  "White has a slight queenside control advantage"                 names)
    (puthash "$61"  "Black has a slight queenside control advantage"                 names)
    (puthash "$62"  "White has a moderate queenside control advantage"               names)
    (puthash "$63"  "Black has a moderate queenside control advantage"               names)
    (puthash "$64"  "White has a decisive queenside control advantage"               names)
    (puthash "$65"  "Black has a decisive queenside control advantage"               names)
    (puthash "$66"  "White has a vulnerable first rank"                              names)
    (puthash "$67"  "Black has a vulnerable first rank"                              names)
    (puthash "$68"  "White has a well protected first rank"                          names)
    (puthash "$69"  "Black has a well protected first rank"                          names)
    (puthash "$70"  "White has a poorly protected king"                              names)
    (puthash "$71"  "Black has a poorly protected king"                              names)
    (puthash "$72"  "White has a well protected king"                                names)
    (puthash "$73"  "Black has a well protected king"                                names)
    (puthash "$74"  "White has a poorly placed king"                                 names)
    (puthash "$75"  "Black has a poorly placed king"                                 names)
    (puthash "$76"  "White has a well placed king"                                   names)
    (puthash "$77"  "Black has a well placed king"                                   names)
    (puthash "$78"  "White has a very weak pawn structure"                           names)
    (puthash "$79"  "Black has a very weak pawn structure"                           names)
    (puthash "$80"  "White has a moderately weak pawn structure"                     names)
    (puthash "$81"  "Black has a moderately weak pawn structure"                     names)
    (puthash "$82"  "White has a moderately strong pawn structure"                   names)
    (puthash "$83"  "Black has a moderately strong pawn structure"                   names)
    (puthash "$84"  "White has a very strong pawn structure"                         names)
    (puthash "$85"  "Black has a very strong pawn structure"                         names)
    (puthash "$86"  "White has poor knight placement"                                names)
    (puthash "$87"  "Black has poor knight placement"                                names)
    (puthash "$88"  "White has good knight placement"                                names)
    (puthash "$89"  "Black has good knight placement"                                names)
    (puthash "$90"  "White has poor bishop placement"                                names)
    (puthash "$91"  "Black has poor bishop placement"                                names)
    (puthash "$92"  "White has good bishop placement"                                names)
    (puthash "$93"  "Black has good bishop placement"                                names)
    (puthash "$94"  "White has poor rook placement"                                  names)
    (puthash "$95"  "Black has poor rook placement"                                  names)
    (puthash "$96"  "White has good rook placement"                                  names)
    (puthash "$97"  "Black has good rook placement"                                  names)
    (puthash "$98"  "White has poor queen placement"                                 names)
    (puthash "$99"  "Black has poor queen placement"                                 names)
    (puthash "$100" "White has good queen placement"                                 names)
    (puthash "$101" "Black has good queen placement"                                 names)
    (puthash "$102" "White has poor piece coordination"                              names)
    (puthash "$103" "Black has poor piece coordination"                              names)
    (puthash "$104" "White has good piece coordination"                              names)
    (puthash "$105" "Black has good piece coordination"                              names)
    (puthash "$106" "White has played the opening very poorly"                       names)
    (puthash "$107" "Black has played the opening very poorly"                       names)
    (puthash "$108" "White has played the opening poorly"                            names)
    (puthash "$109" "Black has played the opening poorly"                            names)
    (puthash "$110" "White has played the opening well"                              names)
    (puthash "$111" "Black has played the opening well"                              names)
    (puthash "$112" "White has played the opening very well"                         names)
    (puthash "$113" "Black has played the opening very well"                         names)
    (puthash "$114" "White has played the middlegame very poorly"                    names)
    (puthash "$115" "Black has played the middlegame very poorly"                    names)
    (puthash "$116" "White has played the middlegame poorly"                         names)
    (puthash "$117" "Black has played the middlegame poorly"                         names)
    (puthash "$118" "White has played the middlegame well"                           names)
    (puthash "$119" "Black has played the middlegame well"                           names)
    (puthash "$120" "White has played the middlegame very well"                      names)
    (puthash "$121" "Black has played the middlegame very well"                      names)
    (puthash "$122" "White has played the ending very poorly"                        names)
    (puthash "$123" "Black has played the ending very poorly"                        names)
    (puthash "$124" "White has played the ending poorly"                             names)
    (puthash "$125" "Black has played the ending poorly"                             names)
    (puthash "$126" "White has played the ending well"                               names)
    (puthash "$127" "Black has played the ending well"                               names)
    (puthash "$128" "White has played the ending very well"                          names)
    (puthash "$129" "Black has played the ending very well"                          names)
    (puthash "$130" "White has slight counterplay"                                   names)
    (puthash "$131" "Black has slight counterplay"                                   names)
    (puthash "$132" "White has moderate counterplay"                                 names)
    (puthash "$133" "Black has moderate counterplay"                                 names)
    (puthash "$134" "White has decisive counterplay"                                 names)
    (puthash "$135" "Black has decisive counterplay"                                 names)
    (puthash "$136" "White has moderate time control pressure"                       names)
    (puthash "$137" "Black has moderate time control pressure"                       names)
    (puthash "$138" "White has severe time control pressure"                         names)
    (puthash "$139" "Black has severe time control pressure"                         names)
    (puthash "$140" "With the idea…"                                                 names)
    (puthash "$141" "Aimed against…"                                                 names)
    (puthash "$142" "Better is…"                                                     names)
    (puthash "$143" "Worse is…"                                                      names)
    (puthash "$144" "Equivalent is…"                                                 names)
    (puthash "$145" "Editorial comment"                                              names)
    (puthash "$146" "Novelty"                                                        names)
    (puthash "$238" "Space advantage"                                                names)
    (puthash "$239" "File"                                                           names)
    (puthash "$240" "Diagonal"                                                       names)
    (puthash "$241" "Center"                                                         names)
    (puthash "$242" "Kingside"                                                       names)
    (puthash "$243" "Queenside"                                                      names)
    (puthash "$244" "Weak point"                                                     names)
    (puthash "$245" "Ending"                                                         names)
    (puthash "$246" "Bishop pair"                                                    names)
    (puthash "$247" "Opposite Bishops"                                               names)
    (puthash "$248" "Same Bishops"                                                   names)
    (puthash "$249" "Connected pawns"                                                names)
    (puthash "$250" "Isolated pawns"                                                 names)
    (puthash "$251" "Doubled pawns"                                                  names)
    (puthash "$252" "Passed pawn"                                                    names)
    (puthash "$253" "Pawn majority"                                                  names)
    (puthash "$254" "With"                                                           names)
    (puthash "$255" "Without"                                                        names)
    (puthash "!"    "Good move"                                                      names)
    (puthash "?"    "Poor move or mistake"                                           names)
    (puthash "‼"    "Very good or brilliant move"                                    names)
    (puthash "!!"   "Very good or brilliant move"                                    names)
    (puthash "⁇"    "Very poor move or blunder"                                      names)
    (puthash "??"   "Very poor move or blunder"                                      names)
    (puthash "⁉"    "Speculative or interesting move"                                names)
    (puthash "!?"   "Speculative or interesting move"                                names)
    (puthash "⁈"    "Questionable or dubious move"                                   names)
    (puthash "?!"   "Questionable or dubious move"                                   names)
    (puthash "□"    "Forced move (all others lose quickly) or only move"             names)
    (puthash "="    "Drawish position or even"                                       names)
    (puthash "∞"    "Unclear position"                                               names)
    (puthash "⩲"    "White has a slight advantage"                                   names)
    (puthash "⩱"    "Black has a slight advantage"                                   names)
    (puthash "±"    "White has a moderate advantage"                                 names)
    (puthash "∓"    "Black has a moderate advantage"                                 names)
    (puthash "+−"   "White has a decisive advantage"                                 names)
    (puthash "−+"   "Black has a decisive advantage"                                 names)
    (puthash "⨀"    "Player is in zugzwang"                                          names)
    (puthash "○"    "Player has a moderate space advantage"                          names)
    (puthash "⟳"    "Player has a moderate time (development) advantage"             names)
    (puthash "↑"    "Player has the initiative"                                      names)
    (puthash "→"    "Player has the attack"                                          names)
    (puthash "=/∞"  "Player has sufficient compensation for material deficit"        names)
    (puthash "⇆"    "Player has moderate counterplay"                                names)
    (puthash "⨁"    "Player has severe time control pressure"                        names)
    (puthash "∆"    "With the idea…"                                                 names)
    (puthash "∇"    "Aimed against…"                                                 names)
    (puthash "⌓"    "Better is…"                                                     names)
    (puthash "<="   "Worse is…"                                                      names)
    (puthash "=="   "Equivalent is…"                                                 names)
    (puthash "RR"   "Editorial comment"                                              names)
    (puthash "N"    "Novelty"                                                        names)
    (puthash "○"    "Space advantage"                                                names)
    (puthash "⇔"    "File"                                                           names)
    (puthash "⇗"    "Diagonal"                                                       names)
    (puthash "⊞"    "Center"                                                         names)
    (puthash "⟫"    "Kingside"                                                       names)
    (puthash "⟪"    "Queenside"                                                      names)
    (puthash "✕"    "Weak point"                                                     names)
    (puthash "⊥"    "Ending"                                                         names)
    names)
  "Names and descriptions of annotations recognized by `pygn-mode'.")

(defvar pygn-mode--annotation-completions nil
  "A like of annotation completions for use in `ivy-completing-read'.")

(maphash (lambda (k v)
           (push (concat k " " v)
                 pygn-mode--annotation-completions))
         pygn-mode-annotation-names)

;;; Syntax table

(defvar pygn-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (with-syntax-table st
      (modify-syntax-entry ?{ "<")
      (modify-syntax-entry ?} ">")
      (modify-syntax-entry ?\n "-")
      (modify-syntax-entry ?\r "-")
      (modify-syntax-entry ?\\ "\\")
      (modify-syntax-entry ?\" "\"")
      (modify-syntax-entry ?& "w")
      (modify-syntax-entry ?! "w")
      (modify-syntax-entry ?* "w")
      (modify-syntax-entry ?+ "w")
      (modify-syntax-entry ?- "w")
      (modify-syntax-entry ?. "w")
      (modify-syntax-entry ?/ "w")
      (modify-syntax-entry ?< "w")
      (modify-syntax-entry ?= "w")
      (modify-syntax-entry ?? "w")
      (modify-syntax-entry ?\⟪ "w")
      (modify-syntax-entry ?\⟫ "w")
      (modify-syntax-entry ?| "w")
      (modify-syntax-entry ?± "w")
      (modify-syntax-entry ?½ "w")
      (modify-syntax-entry ?٭ "w")
      (modify-syntax-entry ?᛭ "w")
      (modify-syntax-entry ?‐ "w")
      (modify-syntax-entry ?‑ "w")
      (modify-syntax-entry ?‒ "w")
      (modify-syntax-entry ?– "w")
      (modify-syntax-entry ?— "w")
      (modify-syntax-entry ?‼ "w")
      (modify-syntax-entry ?‽ "w")
      (modify-syntax-entry ?⁄ "w")
      (modify-syntax-entry ?⁇ "w")
      (modify-syntax-entry ?⁈ "w")
      (modify-syntax-entry ?⁉ "w")
      (modify-syntax-entry ?⁎ "w")
      (modify-syntax-entry ?↑ "w")
      (modify-syntax-entry ?→ "w")
      (modify-syntax-entry ?⇆ "w")
      (modify-syntax-entry ?⇔ "w")
      (modify-syntax-entry ?⇗ "w")
      (modify-syntax-entry ?∆ "w")
      (modify-syntax-entry ?∇ "w")
      (modify-syntax-entry ?− "w")
      (modify-syntax-entry ?∓ "w")
      (modify-syntax-entry ?∕ "w")
      (modify-syntax-entry ?∗ "w")
      (modify-syntax-entry ?∞ "w")
      (modify-syntax-entry ?≤ "w")
      (modify-syntax-entry ?⊞ "w")
      (modify-syntax-entry ?⊥ "w")
      (modify-syntax-entry ?⌓ "w")
      (modify-syntax-entry ?└ "w")
      (modify-syntax-entry ?┘ "w")
      (modify-syntax-entry ?□ "w")
      (modify-syntax-entry ?○ "w")
      (modify-syntax-entry ?◺ "w")
      (modify-syntax-entry ?◿ "w")
      (modify-syntax-entry ?♂ "w")
      (modify-syntax-entry ?✕ "w")
      (modify-syntax-entry ?✱ "w")
      (modify-syntax-entry ?➕ "w")
      (modify-syntax-entry ?➖ "w")
      (modify-syntax-entry ?⟋ "w")
      (modify-syntax-entry ?⟳ "w")
      (modify-syntax-entry ?⧸ "w")
      (modify-syntax-entry ?⨀ "w")
      (modify-syntax-entry ?⨁ "w")
      (modify-syntax-entry ?⩱ "w")
      (modify-syntax-entry ?⩲ "w")
      (modify-syntax-entry ?⬒ "w")
      (modify-syntax-entry ?⬓ "w"))
    st)
  "Syntax table used while in `pygn-mode'.")

;;; Keymaps

(defvar pygn-mode-map
  (let ((map (make-sparse-keymap)))
    ;; menu bar and lighter
    (define-key map [menu-bar PyGN]
      (cons "PyGN" (make-sparse-keymap "PyGN")))
    (define-key map [menu-bar PyGN pygn-mode-select-game]
      '(menu-item "Select Game" pygn-mode-select-game
                  :help "Select the current game"))
    (define-key map [menu-bar PyGN pygn-mode-ivy-jump-to-game-by-fen]
      '(menu-item "Jump to Game by FEN" pygn-mode-ivy-jump-to-game-by-fen
                  :enable (featurep 'ivy)
                  :help "Jump to a game by FEN"))
    (define-key map [menu-bar PyGN pygn-mode-ivy-jump-to-game-by-any-header]
      '(menu-item "Jump to Game by Header" pygn-mode-ivy-jump-to-game-by-header
                  :enable (featurep 'ivy)
                  :help "Jump to a game by any header content"))
    (define-key map [menu-bar PyGN pygn-mode-previous-game]
      '(menu-item "Previous Game" pygn-mode-previous-game
                  :help "Navigate to the previous game"))
    (define-key map [menu-bar PyGN pygn-mode-next-game]
      '(menu-item "Next Game" pygn-mode-next-game
                  :help "Navigate to the next game"))
    (define-key map [menu-bar PyGN sep] menu-bar-separator)
    (define-key map [menu-bar PyGN pygn-mode-ivy-insert-annotation]
      '(menu-item "Insert annotation" pygn-mode-ivy-insert-annotation
                  :enable (and (featurep 'ivy)
                               (pygn-mode--true-containing-node 'movetext))
                  :help "Insert an annotation glyph interactively"))
    (define-key map [menu-bar PyGN sep-2] menu-bar-separator)
    (define-key map [menu-bar PyGN pygn-mode-previous-move]
      '(menu-item "Previous Move" pygn-mode-previous-move
                  :help "Navigate to the previous move"))
    (define-key map [menu-bar PyGN pygn-mode-next-move]
      '(menu-item "Next Move" pygn-mode-next-move
                  :help "Navigate to the next move"))
    (define-key map [menu-bar PyGN sep-3] menu-bar-separator)
    (define-key map [menu-bar PyGN pygn-mode-engine-go-time]
      '(menu-item "Go Time at Point" pygn-mode-engine-go-time
                  :enable (featurep 'uci-mode)
                  :help "UCI Engine \"go time\" at point in separate window"))
    (define-key map [menu-bar PyGN pygn-mode-engine-go-depth]
      '(menu-item "Go Depth at Point" pygn-mode-engine-go-depth
                  :enable (featurep 'uci-mode)
                  :help "UCI Engine \"go depth\" at point in separate window"))
    (define-key map [menu-bar PyGN pygn-mode-describe-annotation-at-pos]
      '(menu-item "Annotation at Point" pygn-mode-describe-annotation-at-pos
                  :enable (pygn-mode--true-containing-node 'annotation)
                  :help "Describe annotation at point in the echo area"))
    (define-key map [menu-bar PyGN pygn-mode-display-fen-at-pos]
      '(menu-item "FEN at Point" pygn-mode-display-fen-at-pos
                  :help "Display FEN at point in separate window"))
    (define-key map [menu-bar PyGN pygn-mode-display-board-at-pos]
      '(menu-item "Board at Point" pygn-mode-display-board-at-pos
                  :help "Display board at point in separate window"))
    (define-key map [menu-bar PyGN pygn-mode-display-variation-line-at-pos]
      '(menu-item "Line at Point" pygn-mode-display-variation-line-at-pos
                  :help "Display SAN line at point in separate window"))

    ;; mouse
    (define-key map [mouse-2] 'pygn-mode-mouse-display-variation-board)

    ;; example keystrokes:
    ;;
    ;; (define-key map (kbd "C-c C-n") 'pygn-mode-next-game)
    ;; (define-key map (kbd "C-c C-p") 'pygn-mode-previous-game)
    ;; (define-key map (kbd "M-f")     'pygn-mode-next-move)
    ;; (define-key map (kbd "M-b")     'pygn-mode-previous-move)
    ;; (define-key map (kbd "C-h $")   'pygn-mode-describe-annotation-at-pos)
    ;;
    ;; and note that `down-list'/`backward-up-list' already works to
    ;; enter/exit a parenthesized variation
    map)
  "Keymap for `pygn-mode'.")

;;; Utility functions

(defun pygn-mode-comment-region-contextually (beg end &optional arg)
  "Wrap `comment-region-default' to remove `comment-continue' characters.

This allows multi-line `comment-region' to work in `pygn-mode'
without adding extra characters at beginning-of-line."
  (if (and (pygn-mode--true-containing-node 'movetext beg)
           (pygn-mode--true-containing-node 'movetext end))
      (save-match-data
        (comment-region-default beg end arg)
        (save-restriction
          (exchange-point-and-mark)
          (narrow-to-region (min (point) (mark) beg) (max (point) (mark) end))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (concat "^" (regexp-quote (or comment-continue "|"))) nil t)
              (replace-match "")))))
    ;; else
    (let ((comment-start ";")
          (comment-end "")
          (comment-style 'plain))
      (comment-region-default beg end arg))))

(defun pygn-mode--get-or-create-board-buffer ()
  "Get or create the `pygn-mode' board buffer."
  (let ((buf (get-buffer-create pygn-mode-board-buffer-name)))
    (with-current-buffer buf
      (unless (eq 'pygn-board-mode major-mode)
        (pygn-board-mode)))
    buf))

(defun pygn-mode--opts-to-argparse (opt-plist)
  "Convert OPT-PLIST into an options string consumable by Python's argparse.

To produce a flag which takes no options, give a plist value of t."
  (let ((key-string nil)
        (argparse-string ""))
    (cl-loop for (key value) on opt-plist by (function cddr)
             do (progn
                  (setq key-string
                        (replace-regexp-in-string
                         "^:" "-" (symbol-name key)))
                  (cond
                   ((eq value t)
                    (setq argparse-string (concat
                                           argparse-string
                                           " " key-string)))
                   ;; if option is nil then we just don't send the flag.
                   ((not (eq value nil))
                    (let ((val-string (shell-quote-argument
                                       (format "%s" value))))
                      (setq argparse-string (concat
                                             argparse-string
                                             (format " %s=%s" key-string val-string))))))))
    argparse-string))

(defun pygn-mode--set-python-path ()
  "Prepend `pygn-mode-pythonpath' to the system `$PYTHONPATH'."
  (setenv "PYTHONPATH" (concat pygn-mode-pythonpath ":" (getenv "PYTHONPATH"))))

(defun pygn-mode--server-running-p ()
  "Return non-nil iff `pygn-mode--server-process' is running."
  (and pygn-mode--server-process (process-live-p pygn-mode--server-process)))

(defun pygn-mode--python-chess-guard ()
  "Throw an error unless the Python chess library is available."
  (unless pygn-mode-python-chess-succeeded
    (let ((process-environment (cl-copy-list process-environment)))
      (when pygn-mode-pythonpath
        (pygn-mode--set-python-path))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import chess"))
          (setq pygn-mode-python-chess-succeeded t)
        (error "The Python interpreter at `pygn-mode-python-path' must have the Python chess library available")))))

(defun pygn-mode--get-stderr-buffer ()
  "Get or create the buffer to which to redirect the standard error."
  (when pygn-mode-server-stderr-buffer-name
    (get-buffer-create pygn-mode-server-stderr-buffer-name)))

(defun pygn-mode--server-start (&optional force)
  "Initialize `pygn-mode--server-process'.

Optionally FORCE recreation if the server already exists."
  (pygn-mode--python-chess-guard)
  (if force
      (pygn-mode--server-kill)
    (when (pygn-mode--server-running-p)
      (error "The pygn-mode server process is already running.  Use optional `force' to recreate")))
  (message "Initializing pygn-mode server process%s." (if force " (forcing)" ""))
  (let ((process-environment (cl-copy-list process-environment)))
    (when pygn-mode-pythonpath
      (pygn-mode--set-python-path))
    (setenv "PYTHONIOENCODING" "UTF-8")
    (setq pygn-mode--server-buffer (get-buffer-create pygn-mode--server-buffer-name))
    (setq pygn-mode--server-process
          (make-process :name "pygn-mode-server"
                        :buffer pygn-mode--server-buffer
                        :noquery t
                        :sentinel #'ignore
                        :coding 'utf-8
                        :connection-type 'pipe
                        :stderr (or (pygn-mode--get-stderr-buffer) null-device)
                        :command (list pygn-mode-python-executable
                                       "-u"
                                       (expand-file-name "pygn_server.py" pygn-mode-script-directory)
                                       "-"))))
  (unless (string-match-p (regexp-quote  "Server started.") (pygn-mode--server-receive))
    (error "Server for `pygn-mode' failed to start.  Try running `pygn-mode-run-diagnostic'")))

(defun pygn-mode--server-kill ()
  "Stop the currently running `pygn-mode--server-process'."
  (when (pygn-mode--server-running-p)
    (process-send-eof pygn-mode--server-process)
    (delete-process pygn-mode--server-process)
    (setq pygn-mode--server-process nil)
    (message "pygn-mode server process killed.")))

(cl-defun pygn-mode--server-send (&key command options payload-type payload)
  "Send a message to the running `pygn-mode--server-process'.

The server request format is documented more completely at doc/server.md
in the source distribution for `pygn-mode'.

:COMMAND should be a symbol such as :pgn-to-fen, which is a command
known by the server.  :OPTIONS should be a plist such as (:pixels 400)
in which the keys correspond to argparse arguments known by the server.
:PAYLOAD-TYPE should be a symbol such as :pgn, identifying the type of the
data payload, and :PAYLOAD may contain arbitrary data."
  (unless (pygn-mode--server-running-p)
    (error "The pygn-mode server is not running -- cannot send a message"))
  (setq payload (replace-regexp-in-string "\n" "\\\\n" payload))
  (setq payload (replace-regexp-in-string "[\n\r]*$" "\n" payload))
  (process-send-string
   pygn-mode--server-process
   (mapconcat #'identity
              (list
               (symbol-name :version)
               pygn-mode-version
               (symbol-name command)
               (pygn-mode--opts-to-argparse options)
               "--"
               (symbol-name payload-type)
               payload)
              " ")))

(defun pygn-mode--server-receive ()
  "Receive a response after `pygn-mode--server-send'.

Respects the variables `pygn-mode--server-receive-every-seconds' and
`pygn-mode--server-receive-max-seconds'."
  (unless (pygn-mode--server-running-p)
    (error "The pygn-mode server is not running -- cannot receive a response"))
  (unless (get-buffer pygn-mode--server-buffer)
    (error "The pygn-mode server output buffer does not exist -- cannot receive a response"))
  (with-current-buffer pygn-mode--server-buffer
    (let ((tries 0)
          (server-message nil))
      (goto-char (point-min))
      (while (and (not (eq ?\n (char-before (point-max))))
                  (< (* tries pygn-mode--server-receive-every-seconds) pygn-mode--server-receive-max-seconds))
        (accept-process-output pygn-mode--server-process pygn-mode--server-receive-every-seconds nil 1)
        (cl-incf tries))
      (setq server-message (buffer-substring-no-properties (point-min) (point-max)))
      (erase-buffer)
      server-message)))

(cl-defun pygn-mode--server-query (&key command options payload-type payload)
  "Send a request to `pygn-mode--server-process', wait, and return response.

:COMMAND, :OPTIONS, :PAYLOAD-TYPE, and :PAYLOAD are as documented at
`pygn-mode--server-send'."
  (unless (pygn-mode--server-running-p)
    (pygn-mode--server-start))
  (pygn-mode--server-send
   :command      command
   :options      options
   :payload-type payload-type
   :payload      payload)
  (pygn-mode--server-receive))

;; it is a bit muddy that the parser is permitted to restart the server
(defun pygn-mode--parse-response (response)
  "Parse RESPONSE string into a list of payload-id and payload.

Restart `pygn-mode--server-process' if the response version string does
not match the client."
  (let ((response-version nil))
    (save-match-data
      (setq response (replace-regexp-in-string "\n+\\'" "" response))
      (when (string= "" response)
        (error "Bad response from `pygn-mode' server -- empty response"))
      (unless (string-match
               "\\`:version\\s-+\\(\\S-+\\)\\s-+\\(.*\\)" response)
        (pygn-mode--server-start 'force)
        (error "Bad response from `pygn-mode' server -- no :version.  Attempted restart"))
      (setq response-version (match-string 1 response))
      (setq response (match-string 2 response))
      (unless (equal response-version pygn-mode-version)
        (pygn-mode--server-start 'force)
        (error "Bad response from `pygn-mode' server -- unexpected :version value: '%s'.  Attempted restart" response-version))
      (unless (string-match
               "\\`\\(:\\S-+\\)\\(.*\\)" response)
        (error "Bad response from `pygn-mode' server"))
      (list
       (intern (match-string 1 response))
       (replace-regexp-in-string
        "\\`\s-*" "" (match-string 2 response))))))

;; would be nicer if multi-type were chosen by how far up we must go in the
;; syntax tree to find a node of the type, instead of the latest-starting by
;; buffer position.
(cl-defun pygn-mode--true-containing-node (&optional type pos)
  "Return the node of type TYPE which contains POS, adjusting whitespace.

TYPE defaults to the nearest containing node.  If TYPE is a symbol, find
the first containing node of that type.  If TYPE is a list of symbols,
find the latest-starting containing node of any of the given types.

POS defaults to the point.

If a node has leading or trailing whitespace, and POS is in that
whitespace, ignore the result, and consult the parent node.  This is a
major difference between this function and `tree-sitter-node-at-pos'.

Also respect narrowing.

If TYPE is unset, and an appropriate containing node is not found,
return the root node."
  (cl-callf or pos (point))
  (save-excursion
    (goto-char pos)
    (let ((best-first -1)
          (best-node nil)
          (type-list (if (listp type) (or type '(nil)) (list type))))
      (dolist (tp type-list)
        (let ((node (tree-sitter-node-at-pos tp pos 'ignore-invalid-types))
              (first nil)
              (last nil))
          (while node
            (setq first (pygn-mode--true-node-first-position node))
            (setq last (pygn-mode--true-node-last-position node))
            (cond
              ((and (>= pos first)
                    (<= pos last)
                    (> first best-first))
               (setq best-first first)
               (setq best-node node)
               (setq node nil))
              (tp
               (setq node nil))
              (t
               (setq node (tsc-get-parent node)))))))
      (if best-node
          best-node
        (if (or (not type)
                (memq 'series_of_games type-list))
          (tsc-root-node tree-sitter-tree))))))

(defun pygn-mode--true-node-first-position (node)
  "Return the true first position within NODE, adjusting whitespace.

Also respect narrowing."
  (let ((first (max (point-min) (tsc-node-start-position node))))
    (cond
      ((eq node (tsc-root-node tree-sitter-tree))
       (setq first (point-min)))
      (t
       (save-excursion
         (goto-char first)
         (skip-syntax-forward "-")
         (setq first (point)))))
    first))

(defun pygn-mode--true-node-last-position (node)
  "Return the true last position within NODE, adjusting whitespace.

Also respect narrowing."
  (let ((last (min (point-max) (tsc-node-end-position node))))
    (cond
      ((eq node (tsc-root-node tree-sitter-tree))
       (setq last (point-max)))
      (t
       (save-excursion
         (goto-char last)
         (skip-syntax-backward "-")
         (while (and (> (point) (point-min))
                     (looking-at-p "\\s-"))
           (forward-char -1))
         (setq last (point)))))
    last))

(defun pygn-mode--true-node-after-position (node)
  "Return the true first position after NODE, adjusting whitespace.

Also respect narrowing."
  (let ((last (pygn-mode--true-node-last-position node)))
    (min (point-max) (1+ last))))

(defun pygn-mode--true-node-before-position (node)
  "Return the true first position before NODE, adjusting whitespace.

Also respect narrowing."
  (let ((first (pygn-mode--true-node-first-position node)))
    (max (point-min) (1- first))))

(defun pygn-mode-inside-movetext-comment-p (&optional pos)
  "Whether POS is inside a PGN movetext comment.

Movetext comments are any of:
 * inline comments surrounded by curly brackets
 * rest-of-line comments introduced by semicolon, within movetext
 * full-line \"escapes\" introduced by the percent symbol, within movetext

Game comments are not:
 * standalone annotations
 * any comments outside movetext

POS defaults to the point."
  (when (pygn-mode--true-containing-node 'movetext pos)
    (or (pygn-mode-inside-inline-comment-p pos)
        (pygn-mode-inside-rest-of-line-comment-p pos)
        (pygn-mode-inside-escaped-line-comment-p pos))))

(defun pygn-mode-inside-inline-comment-p (&optional pos)
  "Whether POS is inside an inline PGN comment.

POS defaults to the point."
  (pygn-mode--true-containing-node 'inline_comment pos))

(defun pygn-mode-inside-rest-of-line-comment-p (&optional pos)
  "Whether POS is inside a rest-of-line PGN comment.

POS defaults to the point."
  (when-let ((comment-node (pygn-mode--true-containing-node 'rest_of_line_comment pos))
             (first-pos (pygn-mode--true-node-first-position comment-node))
             (comment-delimiter (char-after first-pos)))
    (when (eq ?\; comment-delimiter)
      comment-node)))

;; buglet: the spec only permits % at line-beginning-position, but the current
;; grammar permits it at any position.
(defun pygn-mode-inside-escaped-line-comment-p (&optional pos)
  "Whether POS is inside a rest-of-line PGN comment.

POS defaults to the point."
  (when-let ((comment-node (pygn-mode--true-containing-node 'rest_of_line_comment pos))
             (first-pos (pygn-mode--true-node-first-position comment-node))
             (comment-delimiter (char-after first-pos)))
    (when (eq ?\% comment-delimiter)
      comment-node)))

(defun pygn-mode-inside-variation-p (&optional pos)
  "Whether POS is inside a PGN variation.

POS defaults to the point."
  (save-excursion
    ;; TODO: this is to cover up what is still buggy about tree-sitter-node-at-point
    ;; and its wrapper pygn-mode--true-node-first-position.  When resting on
    ;; whitespace, a parent variation node may not be returned, instead nil.
    ;; The nil happens because an adjacent recursive variation spills over
    ;; onto the whitespace occupied by the point.
    (goto-char pos)
    (when (pygn-mode--true-containing-node 'movetext)
      (when (looking-at-p "\\s-")
        (skip-syntax-backward "-")
        (forward-char -1)
        (when (looking-at-p ")")
          (forward-char 1))))
    (pygn-mode--true-containing-node 'variation)))

(defun pygn-mode-inside-variation-or-comment-p (&optional pos)
  "Whether POS is inside a PGN comment or a variation.

POS defaults to the point."
  (or (pygn-mode-inside-movetext-comment-p pos)
      (pygn-mode-inside-variation-p pos)))

(defun pygn-mode-inside-header-p (&optional pos)
  "Whether POS is inside a PGN header.

POS defaults to the point."
  (pygn-mode--true-containing-node 'header pos))

;; Unlike some other defuns, the node returned here does not represent the
;; separator, because the separator is whitespace, and there is no such node.
(defun pygn-mode-inside-separator-p (&optional pos)
  "Whether POS is inside a PGN separator.

Separators are empty lines after tagpair headers or after games.

POS defaults to the point."
  (when-let ((node (pygn-mode--true-containing-node nil pos))
             (type (tsc-node-type node)))
    (when (and (memq type '(game series_of_games))
               (not (tree-sitter-node-at-pos 'result_code pos)))
      node)))

(defun pygn-mode-looking-at-result-code ()
  "Whether the point is looking at a PGN movetext result code."
  (pygn-mode--true-containing-node 'result_code))

(defun pygn-mode-looking-at-suffix-annotation ()
  "Whether the point is looking at a SAN suffix annotation."
  (when-let ((annotation-node (pygn-mode--true-containing-node 'annotation)))
    (save-excursion
      (goto-char (pygn-mode--true-node-first-position annotation-node))
      (ignore-errors (forward-char -1))
      (when (pygn-mode--true-containing-node '(san_move lan_move))
        annotation-node))))

(defun pygn-mode-game-start-position (&optional pos)
  "Start position for the PGN game which contains position POS.

If POS is not within a game, returns nil.

POS defaults to the point."
  (when-let ((game-node (pygn-mode--true-containing-node 'game pos)))
    (pygn-mode--true-node-first-position game-node)))

(defun pygn-mode-game-start-position-forgive-trailing-pos (&optional pos)
  "Start position for the PGN game which contains or is before position POS.

If POS is not within a game or after a game, returns nil.

POS defaults to the point."
  (let ((game-node (pygn-mode--true-containing-node 'game pos)))
    (if game-node
        (pygn-mode--true-node-first-position game-node)
      ;; else
      (save-excursion
        (goto-char (or pos (point)))
        (skip-syntax-backward "-")
        (ignore-errors (forward-char -1))
        (setq game-node (pygn-mode--true-containing-node 'game))
        (when game-node
          (pygn-mode--true-node-first-position game-node))))))

(defun pygn-mode-game-end-position (&optional pos)
  "End position for the PGN game which contains position POS.

If POS is not within a game, returns nil.

POS defaults to the point."
  (when-let ((game-node (pygn-mode--true-containing-node 'game pos)))
    (pygn-mode--true-node-after-position game-node)))

(defun pygn-mode-backward-exit-variations-and-comments ()
  "However deep in nested variations and comments, exit and skip backward."
  (while (and (pygn-mode--true-containing-node
               '(variation inline_comment rest_of_line_comment))
              (pygn-mode--true-containing-node 'movetext))
    (goto-char (pygn-mode--true-node-before-position
                (pygn-mode--true-containing-node
                 '(variation inline_comment rest_of_line_comment))))))

(defun pygn-mode-pgn-at-pos (pos)
  "Return a single-game PGN string inclusive of any move at POS.

We crudely truncate when in the middle of a comment or variation,
and depend on the Python chess library to clean up trailing
garbage."
  (save-excursion
    (goto-char pos)
    (when-let ((header-node (pygn-mode-inside-header-p)))
      (unless (= pos (line-end-position))
        (goto-char (line-beginning-position))
        (when (<= (point)
                  (pygn-mode--true-node-first-position header-node))
          (forward-line 1))))
    (when-let ((move-node (pygn-mode--true-containing-node '(san_move lan_move))))
      (goto-char (pygn-mode--true-node-after-position move-node)))
    ;; todo returning nil might not be the best behavior when pos trails a game
    (when-let ((start-pos (pygn-mode-game-start-position)))
      (buffer-substring-no-properties
       start-pos
       (point)))))

(defun pygn-mode--pgn-at-pos-or-stub (pos)
  "Return a single-game PGN string inclusive of any move at POS.

Identical to `pygn-mode-pgn-at-pos' except that a stub value is returned
when POS is not inside a game."
  (or (pygn-mode-pgn-at-pos pos)
      "[Event \"?\"]\n\n*\n"))

;; TODO this code assumes that a variation begins with the next move,
;; which is not always the case.  Detect when the variation leads with
;; the current move, and include the played move in delete-region.
(defun pygn-mode-pgn-at-pos-as-if-variation (pos)
  "Return a single-game PGN string as if a variation had been played.

Inclusive of any move at POS."
  (if-let ((variation-node (pygn-mode-inside-variation-p pos)))
      (progn
        (when-let ((move-node (pygn-mode--true-containing-node '(san_move lan_move))))
          (goto-char (pygn-mode--true-node-after-position move-node)))
        (let* ((start-pos (pygn-mode-game-start-position))
               (paren-pos nil)
               (paren-offsets '())
               (pgn (buffer-substring-no-properties
                     (pygn-mode-game-start-position)
                     (point))))
          (while variation-node
            (setq paren-pos (pygn-mode--true-node-first-position variation-node))
            (push (- paren-pos start-pos) paren-offsets)
            (goto-char (1- paren-pos))
            (setq variation-node (pygn-mode-inside-variation-p)))
          (with-temp-buffer
            ;; this temp buffer does not need to be in pygn-mode
            (insert (replace-regexp-in-string "[ )]*\\'" "" pgn))
            (dolist (po (reverse paren-offsets))
              (delete-region (1+ po) (+ 2 po)))
            (buffer-substring-no-properties
             (point-min)
             (point-max)))))
      ;; else pos not in variation
      (pygn-mode-pgn-at-pos pos)))

(defun pygn-mode-pgn-to-fen (pgn)
  "Return the FEN corresponding to the position after PGN."
  (let ((response (pygn-mode--server-query
                   :command      :pgn-to-fen
                   :payload-type :pgn
                   :payload      pgn)))
    (cl-callf pygn-mode--parse-response response)
    (unless (eq :fen (car response))
      (error "Bad response from `pygn-mode' server"))
    (cadr response)))

(defun pygn-mode-pgn-to-board (pgn format)
  "Return a board representation for the position after PGN.

FORMAT may be either 'svg or 'text."
  (let ((response (pygn-mode--server-query
                   :command      :pgn-to-board
                   :options      `(:pixels       ,pygn-mode-board-size
                                   :board_format ,format
                                   :flipped      ,pygn-mode-board-flipped)
                   :payload-type :pgn
                   :payload      pgn)))
    (cl-callf pygn-mode--parse-response response)
    (unless (memq (car response) '(:board-svg :board-text))
      (error "Bad response from `pygn-mode' server"))
    (cadr response)))

(defun pygn-mode-pgn-to-line (pgn)
  "Return the SAN line corresponding to the position after PGN."
  (let ((response (pygn-mode--server-query
                   :command      :pgn-to-mainline
                   :payload-type :pgn
                   :payload      pgn)))
    (cl-callf pygn-mode--parse-response response)
    (unless (eq :san (car response))
      (error "Bad response from `pygn-mode' server"))
    (cadr response)))

(defun pygn-mode-focus-game-at-point ()
  "Recenter the window and highlight the current game at point."
  ;; todo the ignore-errors is for automated testing.  is there a better way?
  (ignore-errors
    (recenter-window-group))
  (when (fboundp 'nav-flash-show)
    (let ((nav-flash-delay 0.2)
          (beg (if pygn-mode-flash-full-game
                   (pygn-mode-game-start-position)
                 nil))
          (end (if pygn-mode-flash-full-game
                   (pygn-mode-game-end-position)
                 nil)))
      (nav-flash-show beg end))))

(defun pygn-mode-all-header-coordinates ()
  "Find PGN headers for all games in the buffer.

Returns an alist of cells in the form (CONTENT . POS), where CONTENT contains
strings from header tagpairs of games, and POS is the starting position of a
game in the buffer.

For use in `pygn-mode-ivy-jump-to-game-by-any-header'."
  (let* ((header-coordinates nil)
         (root-node (tsc-root-node tree-sitter-tree))
         (max-count (tsc-count-children root-node))
         (index 0)
         (game-node nil)
         (header-node nil))
    (while (< index max-count)
      (setq game-node (tsc-get-nth-child root-node index))
      (cl-incf index)
      (when (and game-node (eq 'game (tsc-node-type game-node)))
        (setq header-node
              (tree-sitter-node-at-pos 'header
                                       (tsc-node-start-position game-node)))
        (when header-node
          (push (cons (replace-regexp-in-string
                       "\\`\\s-+" ""
                       (replace-regexp-in-string
                        "\\s-+\\'" ""
                        (replace-regexp-in-string
                         "[\r\n]" " "
                         (replace-regexp-in-string
                          "^\\[\\S-+\\s-+\"[?.]*\"\\]" ""
                          (tsc-node-text header-node)))))
                      (tsc-node-start-position header-node))
                header-coordinates))))
    (nreverse header-coordinates)))

(defun pygn-mode-fen-coordinates ()
  "Find PGN FEN headers for all games in the buffer.

Returns an alist of cells in the form (CONTENT . POS), where CONTENT contains
strings from FEN header tagpairs of games, and POS is the starting position
of a game in the buffer.

For use in `pygn-mode-ivy-jump-to-game-by-fen'."
  (let ((all-coordinates (pygn-mode-all-header-coordinates))
        (fen-coordinates nil)
        (fen nil))
    (cl-loop for cell in (cl-remove-if-not
                          (lambda (x) (cl-search "[FEN " (car x)))
                          all-coordinates)
             do (progn
                  (setq fen
                        (replace-regexp-in-string
                         "\\`.*?\\[FEN\\s-+\"\\(.*?\\)\".*" "\\1"
                         (car cell)))
                  (push (cons fen (cdr cell)) fen-coordinates)))
    (nreverse fen-coordinates)))

;;; Font-lock

(defvar pygn-mode-tree-sitter-patterns
  [
   (tagpair_delimiter_open) @tagpair-bracket
   (tagpair_delimiter_close) @tagpair-bracket
   (tagpair_key) @tagpair-key
   (tagpair tagpair_value_delimiter: (double_quote) @tagpair-value)
   (tagpair_value_contents) @tagpair-value

   (variation_delimiter_open) @variation-delimiter
   (variation_delimiter_close) @variation-delimiter
   (variation_movetext variation_move_number: (move_number) @variation-move-number)
   (variation_movetext variation_san_move: (san_move) @variation-move)
   (variation_movetext variation_annotation: (annotation) @variation-annotation)
   (variation_movetext variation_comment: (inline_comment) @variation-inline-comment)
   (variation_movetext variation_comment: (rest_of_line_comment) @variation-rest-of-line-comment)

   (inline_comment) @inline-comment
   (rest_of_line_comment) @rest-of-line-comment
   (old_style_twic_section_comment) @twic-section-comment

   (movetext (move_number) @move-number)
   (movetext (san_move) @move)

   (annotation) @annotation

   (result_code) @result

   (ERROR) @invalid
   ]
  "A tree-sitter \"query\" which defines syntax highlighting for pygn-mode.")

(defun pygn-mode--capture-face-mapper (capture-name)
  "Return the default face used to highlight CAPTURE-NAME."
  (intern (format "pygn-mode-%s-face" capture-name)))

;;; Major-mode definition

;;;###autoload
(define-derived-mode pygn-mode fundamental-mode "PyGN"
  "A major-mode for chess PGN files, powered by Python."
  :syntax-table pygn-mode-syntax-table
  :group 'pygn

  (setq-local tree-sitter-hl-default-patterns pygn-mode-tree-sitter-patterns)
  (setq-local tree-sitter-hl-face-mapping-function #'pygn-mode--capture-face-mapper)

  (setq-local comment-start "{")
  (setq-local comment-end "}")
  (setq-local comment-multi-line t)
  (setq-local comment-style 'multi-line)
  (setq-local comment-use-syntax t)
  (setq-local comment-quote-nested nil)
  ;; why does newcomment.el _force_ a non-whitespace comment-continue?
  ;; comment-region-function must then be overridden to remove the
  ;; continue character when not wanted.
  (setq-local comment-continue "|")
  (setq-local comment-region-function 'pygn-mode-comment-region-contextually)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)

  (tree-sitter-hl-mode)

  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (default-value 'mode-line-major-mode-keymap))
    (define-key map (kbd "<mode-line> <mouse-4>")     'pygn-mode-previous-move)
    (define-key map (kbd "<mode-line> <mouse-5>")     'pygn-mode-next-move)
    (define-key map (kbd "<mode-line> <wheel-up>")    'pygn-mode-previous-move)
    (define-key map (kbd "<mode-line> <wheel-down>")  'pygn-mode-next-move)
    (define-key map (kbd "<mode-line> <wheel-left>")  'ignore)
    (define-key map (kbd "<mode-line> <wheel-right>") 'ignore)
    (setq-local mode-line-major-mode-keymap map)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[pP][gG][nN]\\'" . pygn-mode))

;;;###autoload
(define-derived-mode pygn-board-mode special-mode "PyGN Board"
  "A major-mode for displaying chess boards."
  :group 'pygn
  :abbrev-table nil
  :syntax-table nil)

;;; Minor-mode definition

(define-minor-mode pygn-mode-follow-minor-mode
  "Minor mode for `pygn-mode'.

With a prefix argument ARG, enable mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable mode if ARG is omitted or nil.

When turned on, cursor motion in a PyGN buffer causes automatic display of
a board representation corresponding to the point.  The displayed board
will respect variations.

In addition, if the cursor rests on an annotation symbol, the
meaning of the symbol will be displayed in the echo area."
  :group 'pygn
  :init-value nil
  :lighter " fol"
  (if pygn-mode-follow-minor-mode
      (progn
        (pygn-mode-follow-mode-post-command-hook)
        (add-hook 'post-command-hook #'pygn-mode-follow-mode-post-command-hook nil t))
    (remove-hook 'post-command-hook #'pygn-mode-follow-mode-post-command-hook t)
    (when (get-buffer pygn-mode-board-buffer-name)
      (let ((win (get-buffer-window (get-buffer pygn-mode-board-buffer-name))))
        (when (window-live-p win)
          (delete-window win))))))

(defun pygn-mode-follow-mode-post-command-hook ()
  "Driver for function `pygn-mode-follow-minor-mode'.

Intended for use in `post-command-hook'."
  (pygn-mode-describe-annotation-at-pos (point) nil 'no-error)
  (pygn-mode-display-variation-board-at-pos (point)))

(cl-defun pygn-mode--run-diagnostic ()
  "Open a buffer containing a `pygn-mode' dependency/configuration diagnostic."
  (let ((buf (get-buffer-create pygn-mode-diagnostic-output-buffer-name))
        (process-environment (cl-copy-list process-environment)))
    (with-current-buffer buf
      (erase-buffer)
      (when pygn-mode-pythonpath
        (pygn-mode--set-python-path))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "pass"))
          (insert (format "[x] Good. We can execute the pygn-mode-python-executable at '%s'\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. We cannot execute the interpreter '%s'.  Try installing Python 3.7+ and/or customizing the value of pygn-mode-python-executable.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode--run-diagnostic nil))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import sys; exit(0 if sys.hexversion >= 0x3000000 else 1)"))
          (insert (format "[x] Good. The pygn-mode-python-executable at '%s' is a Python 3 interpreter.\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. The executable '%s' is not a Python 3 interpreter.  Try installing Python 3.7+ and/or customizing the value of pygn-mode-python-executable.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode--run-diagnostic nil))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import sys; exit(0 if sys.hexversion >= 0x3070000 else 1)"))
          (insert (format "[x] Good. The pygn-mode-python-executable at '%s' is better than or equal to Python version 3.7.\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. The executable '%s' is not at least Python version 3.7.  Try installing Python 3.7+ and/or customizing the value of pygn-mode-python-executable.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode--run-diagnostic nil))
      (if (zerop (call-process pygn-mode-python-executable nil nil nil "-c" "import chess"))
          (insert (format "[x] Good. The pygn-mode-python-executable at '%s' can import the Python chess library.\n\n" pygn-mode-python-executable))
        ;; else
        (insert
         (format
          "[ ] Bad. The executable '%s' cannot import the Python chess library.  Try installing chess, and/or customizing the value of pygn-mode-pythonpath.\n\n"
          pygn-mode-python-executable))
        (cl-return-from pygn-mode--run-diagnostic nil))
      (let ((server-script-path (expand-file-name "pygn_server.py" pygn-mode-script-directory)))
        (if (and (file-exists-p server-script-path)
                 (zerop (call-process pygn-mode-python-executable  nil nil nil server-script-path "-version")))
           (insert (format "[x] Good. The pygn-mode-script-directory ('%s') is found and the server script is callable.\n\n" pygn-mode-script-directory))
         (insert
          (format
           "[ ] Bad. The pygn-mode-script-directory ('%s') is bad or does not contain working server script (pygn_server.py).\n\n" pygn-mode-script-directory))
         (cl-return-from pygn-mode--run-diagnostic nil)))
      (dolist (melpa-lib '(uci-mode nav-flash ivy))
        (if (featurep melpa-lib)
            (insert (format "[x] Good.  The `%s' library is available.\n\n" melpa-lib))
          ;; else
          (insert
           (format
            "[ ] Bad but not a requirement.  The `%s' library is not available.  Try installing it from MELPA.\n\n" melpa-lib))))
      (insert (format "------------------------------------\n\n"))
      (insert (format "All pygn-mode required diagnostics completed successfully.\n"))))
  (cl-return-from pygn-mode--run-diagnostic t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun pygn-mode-run-diagnostic (&optional as-command)
  "Run a dependency/configuration diagnostic for `pygn-mode'.

When called as an interactive command (when AS-COMMAND is non-nil), display
a buffer with diagnostic details.

When called noninteractively, the return value is non-nil iff required
diagnostic tests were successful."
  (interactive "p")
  (if as-command
      (progn
        (pygn-mode--run-diagnostic)
        (display-buffer (get-buffer pygn-mode-diagnostic-output-buffer-name) '(display-buffer-reuse-window)))
    ;; else
    (if (pygn-mode--run-diagnostic)
        (or (message "pygn-mode diagnostics passed.") t)
      (message "WARN: pygn-mode diagnostics failed (see '%s' buffer for details)"
               pygn-mode-diagnostic-output-buffer-name)
      nil)))

(defun pygn-mode-next-game (&optional arg)
  "Advance to the next game in a multi-game PGN buffer.

With numeric prefix ARG, advance ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (if (< arg 0)
      (pygn-mode-previous-game (* -1 arg))
    ;; else
    (let ((node (pygn-mode--true-containing-node '(game series_of_games))))
      (dotimes (_ arg)
        (cond
          ((eq 'series_of_games (tsc-node-type node))
           (let ((newpos (save-excursion
                           (skip-syntax-forward "-")
                           (point))))
             (if (pygn-mode--true-containing-node 'game newpos)
                 (progn
                   (goto-char newpos)
                   (setq node (pygn-mode--true-containing-node 'game)))
               (error "No more games"))))
          (t
           (setq node (tsc-get-next-sibling node))
           (if node
               (goto-char (pygn-mode--true-node-first-position node))
             (error "No more games"))))))
    (pygn-mode-focus-game-at-point)))

(defun pygn-mode-previous-game (&optional arg)
  "Move back to the previous game in a multi-game PGN buffer.

With numeric prefix ARG, move back ARG games."
  (interactive "p")
  (cl-callf or arg 1)
  (if (< arg 0)
      (pygn-mode-next-game (* -1 arg))
    ;; else
    (let ((node (pygn-mode--true-containing-node '(game series_of_games))))
      (dotimes (_ arg)
        (cond
          ((eq 'series_of_games (tsc-node-type node))
           (let ((newpos (save-excursion
                           (skip-syntax-backward "-")
                           (unless (= (point) (point-min))
                             (forward-char -1))
                           (point))))
             (if-let ((newnode (pygn-mode--true-containing-node 'game newpos)))
                 (goto-char (pygn-mode--true-node-first-position newnode))
               (error "No more games"))))
          (t
           (setq node (tsc-get-prev-sibling node))
           (if node
               (goto-char (pygn-mode--true-node-first-position node))
             (error "No more games"))))))
    (pygn-mode-focus-game-at-point)))

;; when tree-sitter-node-at-pos is used instead of pygn-mode--true-containing-node
;; here, that is intentional, for two related reasons: tree-sitter-node-at-pos
;; will return a leaf node even on whitespace, and we plan to call
;; tsc-get-next-sibling-node on the return value.
(defun pygn-mode-next-move (&optional arg)
  "Advance to the next player move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it is considered to be on the move followed by that move number.
But the advancing motion will skip over move numbers when possible.

With numeric prefix ARG, advance ARG moves forward."
  (interactive "p")
  (cl-callf or arg 1)
  (if (< arg 0)
      (pygn-mode-previous-move (* -1 arg))
    ;; else
    (save-restriction
      (when (eq 'series_of_games (tsc-node-type (pygn-mode--true-containing-node)))
        (let ((newpos (save-excursion
                        (skip-syntax-forward "-")
                        (point))))
          (when (pygn-mode--true-containing-node 'game newpos)
            (goto-char newpos))))
      (narrow-to-region (or (pygn-mode-game-start-position) (point))
                        (or (pygn-mode-game-end-position) (point)))
      (when-let ((game-node (pygn-mode--true-containing-node 'game))
                 (movetext-or-result-node (tsc-get-nth-child game-node 1))
                 (movetext-or-result-start (pygn-mode--true-node-first-position movetext-or-result-node)))
        (when (< (point) movetext-or-result-start)
          (goto-char movetext-or-result-start)))
      (unless (pygn-mode--true-containing-node 'movetext)
        (error "No more moves"))
      (dotimes (_ arg)
        (let ((node (tree-sitter-node-at-pos))
              (thumb (point)))
          (when-let ((move-node (pygn-mode--true-containing-node '(san_move lan_move))))
            (goto-char (pygn-mode--true-node-after-position move-node)))
          (while (not (pygn-mode--true-containing-node '(san_move lan_move)))
            (setq node (tree-sitter-node-at-pos))
            (cond
              ((>= (pygn-mode--true-node-last-position node)
                   (point-max))
               (goto-char thumb)
               (error "No more moves"))
              ((pygn-mode--true-containing-node
                '(variation inline_comment rest_of_line_comment))
               (goto-char (pygn-mode--true-node-after-position
                           (pygn-mode--true-containing-node
                            '(variation inline_comment rest_of_line_comment)))))
              ((looking-at-p "\\s-")
               (skip-syntax-forward "-"))
              (t
               (setq node (tsc-get-next-sibling node))
               (if node
                   (goto-char (pygn-mode--true-node-first-position node))
                 (forward-char 1)))))
          (skip-syntax-forward "-"))))))

;; when tree-sitter-node-at-pos is used instead of pygn-mode--true-containing-node
;; here, that is intentional, for two related reasons: tree-sitter-node-at-pos
;; will return a leaf node even on whitespace, and we plan to call
;; tsc-get-next-sibling-node on the return value.
(defun pygn-mode-previous-move (&optional arg)
  "Move back to the previous player move in a PGN game.

Treats move numbers purely as punctuation.  If the point is on a move
number, it is considered to be on the move followed by that move number.
But the backward motion will skip over move numbers when possible.

With numeric prefix ARG, move ARG moves backward."
  (interactive "p")
  (cl-callf or arg 1)
  (if (< arg 0)
      (pygn-mode-next-move (* -1 arg))
    ;; else
    (save-restriction
      (save-match-data
        (when (eq 'series_of_games (tsc-node-type (pygn-mode--true-containing-node)))
          (let ((newpos (save-excursion
                          (skip-syntax-backward "-")
                          (unless (= (point) (point-min))
                            (forward-char -1))
                          (point))))
            (when (pygn-mode--true-containing-node 'game newpos)
              (goto-char newpos))))
        (narrow-to-region (or (pygn-mode-game-start-position) (point))
                          (or (pygn-mode-game-end-position) (point)))
        (when-let ((game-node (pygn-mode--true-containing-node 'game))
                   (movetext-or-result-node (tsc-get-nth-child game-node 1))
                   (movetext-or-result-end (pygn-mode--true-node-last-position movetext-or-result-node)))
          (when (> (point) movetext-or-result-end)
            (goto-char movetext-or-result-end)))
        (unless (pygn-mode--true-containing-node 'movetext)
          (error "No more moves"))
        (dotimes (_ arg)
          (let ((node (tree-sitter-node-at-pos))
                (thumb (point)))
            (when-let ((move-node (pygn-mode--true-containing-node '(san_move lan_move))))
              (if (= (point) (pygn-mode--true-node-first-position move-node))
                  (goto-char (pygn-mode--true-node-before-position move-node))
                (goto-char (pygn-mode--true-node-first-position move-node))))
            (while (or (not (pygn-mode--true-containing-node '(san_move lan_move)))
                       (pygn-mode--true-containing-node 'variation))
              (setq node (tree-sitter-node-at-pos))
              (cond
                ((<= (pygn-mode--true-node-first-position node)
                     (point-min))
                 (goto-char thumb)
                 (error "No more moves"))
                ((pygn-mode--true-containing-node
                  '(variation inline_comment rest_of_line_comment))
                 (goto-char (pygn-mode--true-node-before-position
                             (pygn-mode--true-containing-node
                              '(variation inline_comment rest_of_line_comment)))))
                ((looking-back "\\s-" 1)
                 (skip-syntax-backward "-"))
                (t
                 (setq node (tsc-get-prev-sibling node))
                 (if node
                     (goto-char (pygn-mode--true-node-first-position node))
                   (forward-char -1)))))
            (skip-syntax-backward "w")
            (skip-syntax-forward "-")))))))

(defun pygn-mode-select-game (pos)
  "Select current game in a multi-game PGN buffer.

When called non-interactively, select the game containing POS."
  (interactive "d")
  (goto-char pos)
  (push-mark (pygn-mode-game-end-position) t t)
  (goto-char (pygn-mode-game-start-position)))

(defun pygn-mode-describe-annotation-at-pos (pos &optional do-copy no-error)
  "Describe the annotation symbol at point in the echo area.

When called non-interactively, describe the annotation
symbol corresponding to POS.

With `prefix-arg' DO-COPY, copy the description to the kill ring,
and to the system clipboard when running a GUI Emacs.

When NO-ERROR is set, noninteractively, do not signal an error
when POS is not on an annotation symbol."
  (interactive "d\nP")
  (let ((annotation-node (pygn-mode--true-containing-node 'annotation pos)))
    (if annotation-node
        (let* ((annotation-text (buffer-substring-no-properties
                                 (pygn-mode--true-node-first-position annotation-node)
                                 (pygn-mode--true-node-after-position annotation-node)))
               (description (or (gethash annotation-text pygn-mode-annotation-names) "Unknown"))
               (full-description (format "%s - %s" annotation-text description)))
          (when do-copy
            (kill-new full-description)
            (when (and (fboundp 'gui-set-selection)
                       (display-graphic-p))
              (gui-set-selection 'CLIPBOARD full-description)))
          (message "%s%s" full-description (if do-copy (propertize "\t(copied)" 'face '(:foreground "grey33")) "")))
      ;; else
      (unless no-error
        (error "No annotation symbol")))))

(defun pygn-mode-echo-fen-at-pos (pos &optional do-copy)
  "Display the FEN corresponding to the point in the echo area.

When called non-interactively, display the FEN corresponding to POS.

With `prefix-arg' DO-COPY, copy the FEN to the kill ring, and to the system
clipboard when running a GUI Emacs."
  (interactive "d\nP")
  (let ((fen (pygn-mode-pgn-to-fen (pygn-mode--pgn-at-pos-or-stub pos))))
    (when do-copy
      (kill-new fen)
      (when (and (fboundp 'gui-set-selection)
                 (display-graphic-p))
        (gui-set-selection 'CLIPBOARD fen)))
    (message "%s%s" fen (if do-copy (propertize "\t(copied)" 'face '(:foreground "grey33")) ""))))

(defun pygn-mode-flip-board ()
  "Flip the board display."
  (interactive)
  (setq pygn-mode-board-flipped (not pygn-mode-board-flipped))
  (pygn-mode-display-gui-board-at-pos (point)))

(defun pygn-mode-display-fen-at-pos (pos)
  "Display the FEN corresponding to the point in a separate buffer.

When called non-interactively, display the FEN corresponding to POS."
  (interactive "d")
  (let* ((fen (pygn-mode-pgn-to-fen (pygn-mode--pgn-at-pos-or-stub pos)))
         (buf (get-buffer-create pygn-mode-fen-buffer-name))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (erase-buffer)
      (insert fen)
      (goto-char (point-min))
      (display-buffer buf '(display-buffer-reuse-window))
      (unless win
        (setq win (get-buffer-window buf))
        (set-window-dedicated-p win t)
        (resize-temp-buffer-window win)))))

(defun pygn-mode-display-variation-fen-at-pos (pos)
  "Respecting variations, display the FEN corresponding to the point.

When called non-interactively, display the FEN corresponding to POS."
  (interactive "d")
  (let ((pgn (pygn-mode-pgn-at-pos-as-if-variation pos)))
    ;; todo it might be a better design if a temp buffer wasn't needed here
    (with-temp-buffer
      (insert pgn)
      ;; todo re-running the mode seems wasteful
      (pygn-mode)
      (pygn-mode-display-fen-at-pos (point-max)))))

;; interactive helper
(defun pygn-mode--save-gui-board-at-pos (pos)
  "Save the board image corresponding to POS to a file."
  (let* ((pygn-mode-board-size (completing-read "Pixels per side: " nil nil nil nil nil pygn-mode-board-size))
         (filename (read-file-name "SVG filename: "))
         (svg-data (pygn-mode-pgn-to-board (pygn-mode--pgn-at-pos-or-stub pos) 'svg)))
    (with-temp-buffer
      (insert svg-data)
      (write-file filename))))

(defun pygn-mode-display-gui-board-at-pos (pos)
  "Display a GUI board corresponding to the point in a separate buffer.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let* ((svg-data (pygn-mode-pgn-to-board (pygn-mode--pgn-at-pos-or-stub pos) 'svg))
         (buf (pygn-mode--get-or-create-board-buffer))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (setq cursor-type nil)
        (erase-buffer)
        (insert-image (create-image svg-data 'svg t))))
    (display-buffer buf '(display-buffer-reuse-window))
    (unless win
      (setq win (get-buffer-window buf))
      (set-window-dedicated-p win t)
      (resize-temp-buffer-window win))))

(defun pygn-mode-display-text-board-at-pos (pos)
  "Display a text board corresponding to the point in a separate buffer.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let* ((text-data (pygn-mode-pgn-to-board (pygn-mode--pgn-at-pos-or-stub pos) 'text))
         (buf (pygn-mode--get-or-create-board-buffer))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (replace-regexp-in-string
                 "\\\\n" "\n"
                 text-data))
        (goto-char (point-min))))
    (display-buffer buf '(display-buffer-reuse-window))
    (unless win
      (setq win (get-buffer-window buf))
      (set-window-dedicated-p win t)
      (resize-temp-buffer-window win))))

(defun pygn-mode-display-board-at-pos (pos &optional arg)
  "Display a board corresponding to the point in a separate buffer.

When called non-interactively, display the board corresponding to POS.

The board format will be determined automatically based on
`display-graphic-p'.  To force a GUI or TUI board, call
`pygn-mode-display-gui-board-at-pos' or
`pygn-mode-display-text-board-at-pos' directly.

With optional universal prefix ARG, write a board image to a file,
prompting for image size."
  (interactive "d\nP")
  (cond
    (arg
     (pygn-mode--save-gui-board-at-pos pos))
    ((display-graphic-p)
     (pygn-mode-display-gui-board-at-pos pos))
    (t
     (pygn-mode-display-text-board-at-pos pos))))

(defun pygn-mode-mouse-display-variation-board (event)
  "Display the board corresponding to a mouse click in a separate buffer.

The mouse click corresponds to EVENT.

The board display respects variations."
  (interactive "@e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (let ((pgn (pygn-mode-pgn-at-pos-as-if-variation (point))))
    ;; todo it might be a better design if a temp buffer wasn't needed here
    (with-temp-buffer
      (insert pgn)
      ;; todo re-running the mode seems wasteful
      (pygn-mode)
      (pygn-mode-display-board-at-pos (point)))))

(defun pygn-mode-display-variation-board-at-pos (pos)
  "Respecting variations, display the board corresponding to the point.

When called non-interactively, display the board corresponding to POS."
  (interactive "d")
  (let ((pgn (pygn-mode-pgn-at-pos-as-if-variation pos)))
    ;; todo it might be a better design if a temp buffer wasn't needed here
    (with-temp-buffer
      (insert pgn)
      ;; todo invoking the mode seems like it would be slow, compared to using
      ;; the parse we already have
      (pygn-mode)
      (pygn-mode-display-board-at-pos (point-max)))))

(defun pygn-mode-display-line-at-pos (pos)
  "Display the SAN line corresponding to the point in a separate buffer.

When called non-interactively, display the line corresponding to POS."
  (interactive "d")
  (let* ((line (pygn-mode-pgn-to-line (pygn-mode--pgn-at-pos-or-stub pos)))
         (buf (get-buffer-create pygn-mode-line-buffer-name))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (erase-buffer)
      (insert line)
      (goto-char (point-min))
      (display-buffer buf '(display-buffer-reuse-window))
      (unless win
        (setq win (get-buffer-window buf))
        (set-window-dedicated-p win t)
        (resize-temp-buffer-window win)))))

(defun pygn-mode-display-variation-line-at-pos (pos)
  "Display the SAN line corresponding to the point in a separate buffer.

When called non-interactively, display the line corresponding to POS.

The SAN line respects variations."
  (interactive "d")
  (let* ((line (pygn-mode-pgn-to-line (pygn-mode-pgn-at-pos-as-if-variation pos)))
         (buf (get-buffer-create pygn-mode-line-buffer-name))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (erase-buffer)
      (insert line)
      (goto-char (point-min))
      (display-buffer buf '(display-buffer-reuse-window))
      (unless win
        (setq win (get-buffer-window buf))
        (set-window-dedicated-p win t)
        (resize-temp-buffer-window win)))))

(defun pygn-mode-previous-move-follow-board (arg)
  "Move back to the previous player move and display the updated board.

With numeric prefix ARG, move ARG moves backward."
  (interactive "p")
  (pygn-mode-previous-move arg)
  (pygn-mode-display-board-at-pos (point)))

(defun pygn-mode-next-move-follow-board (arg)
  "Advance to the next player move and display the updated board.

With numeric prefix ARG, move ARG moves forward."
  (interactive "p")
  (pygn-mode-next-move arg)
  (pygn-mode-display-board-at-pos (point)))

(defun pygn-mode-engine-go-depth (pos &optional depth)
  "Evaluate the position at POS in a `uci-mode' engine buffer.

DEPTH defaults to `pygn-mode-default-engine-depth'.  It may be overridden
directly as a numeric prefix argument, or prompted for interactively by
giving a universal prefix argument."
  (interactive "d\nP")
  (setq depth
        (cond
          ((numberp depth)
           depth)
          ((and depth (listp depth))
           (completing-read "Depth: " nil))
          (t
           pygn-mode-default-engine-depth)))
  (unless (and uci-mode-engine-buffer
               (window-live-p
                (get-buffer-window uci-mode-engine-buffer)))
    (uci-mode-run-engine))
  (let ((fen (pygn-mode-pgn-to-fen (pygn-mode-pgn-at-pos-as-if-variation pos))))
    (sleep-for 0.05)
    (uci-mode-send-stop)
    (uci-mode-send-commands
     (list (format "position fen %s" fen)
           (format "go depth %s" depth)))))

(defun pygn-mode-engine-go-time (pos &optional seconds)
  "Evaluate the position at POS in a `uci-mode' engine buffer.

SECONDS defaults to `pygn-mode-default-engine-time'.  It may be overridden
directly as a numeric prefix argument, or prompted for interactively by
giving a universal prefix argument."
  (interactive "d\nP")
  (setq seconds
        (cond
          ((numberp seconds)
           seconds)
          ((and seconds (listp seconds))
           (completing-read "Seconds: " nil))
          (t
           pygn-mode-default-engine-time)))
  (unless (and uci-mode-engine-buffer
               (window-live-p
                (get-buffer-window uci-mode-engine-buffer)))
    (uci-mode-run-engine))
  (let ((fen (pygn-mode-pgn-to-fen (pygn-mode-pgn-at-pos-as-if-variation pos))))
    (sleep-for 0.05)
    (uci-mode-send-stop)
    (uci-mode-send-commands
     (list (format "position fen %s" fen)
           (format "go time %s" seconds)))))

(defun pygn-mode-triple-window-layout-bottom ()
  "Set up three windows for PGN buffer, board image, and UCI interaction.

Place the board and UCI windows below the PGN window."
  (interactive)
  (unless (eq major-mode 'pygn-mode)
    (error "Select a buffer in `pygn-mode'"))
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer
   (pygn-mode--get-or-create-board-buffer))
  (split-window-horizontally)
  (when (> (point-max) (point-min))
    (let ((fit-window-to-buffer-horizontally t))
      (fit-window-to-buffer (get-buffer-window (current-buffer)))))
  (other-window 1)
  (switch-to-buffer
   (get-buffer-create (or uci-mode-engine-buffer-name "*UCI*")))
  (set-window-scroll-bars
   (get-buffer-window (current-buffer)) nil nil nil 'bottom)
  (other-window 1))

(defun pygn-mode-triple-window-layout-right ()
  "Set up three windows for PGN buffer, board image, and UCI interaction.

Place the board and UCI windows to the right of the PGN window."
  (interactive)
  (unless (eq major-mode 'pygn-mode)
    (error "Select a buffer in `pygn-mode'"))
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer
   (pygn-mode--get-or-create-board-buffer))
  (split-window-vertically)
  (when (> (point-max) (point-min))
    (let ((fit-window-to-buffer-horizontally t))
      (fit-window-to-buffer (get-buffer-window (current-buffer)))))
  (other-window 1)
  (switch-to-buffer
   (get-buffer-create (or uci-mode-engine-buffer-name "*UCI*")))
  (set-window-scroll-bars
   (get-buffer-window (current-buffer)) nil nil nil 'bottom)
  (other-window 1))

(defun pygn-mode-ivy-jump-to-game-by-any-header ()
  "Navigate to a game by `ivy-completing-read' against header tagpairs.

Header tagpairs for which the value is \"?\" or empty are elided from
the search string."
  (interactive)
  (let* ((read-collection (pygn-mode-all-header-coordinates))
         (choice (ivy-completing-read "Choose Game: " read-collection)))
    (when (and choice (not (zerop (length choice))))
      (goto-char (cdr (assoc choice read-collection)))
      (pygn-mode-focus-game-at-point))))

(defun pygn-mode-ivy-jump-to-game-by-fen ()
  "Navigate to a game by `ivy-completing-read' against FEN tagpair values.

Games without FEN tagpairs are not represented in the search."
  (interactive)
  (let* ((read-collection (pygn-mode-fen-coordinates))
         (choice (ivy-completing-read "Choose Game: " read-collection)))
    (when (and choice (not (zerop (length choice))))
      (goto-char (cdr (assoc choice read-collection)))
      (pygn-mode-focus-game-at-point))))

;; todo refuse to insert into non-whitespace, and/or scan forward
;; todo pad around the glyph with whitespace, but only if needed
(defun pygn-mode-ivy-insert-annotation ()
  "Insert an annotation interactively via `ivy-completing-read'."
  (interactive)
  (unless (pygn-mode--true-containing-node 'movetext)
    (error "Point is not within movetext"))
  (let* ((choice (ivy-completing-read "Insert annotation: " pygn-mode--annotation-completions))
         (space-pos (string-match-p " " choice))
         (glyph (substring choice 0 space-pos)))
    (insert glyph)))

(provide 'pygn-mode)

;;
;; Emacs
;;
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: ARGS alist
;;

;;; pygn-mode.el ends here
