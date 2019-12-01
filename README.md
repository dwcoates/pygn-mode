<a href="/doc/images/pygn-mode-follow.gif">
<img src="/doc/images/pygn-mode-board.png" width=300 align="right"/>
</a>

# Overview

An Emacs major-mode for chess PGN files, powered by Python.

 * [Quickstart](#quickstart)
 * [pygn-mode](#pygn-mode)
 * [Interactive Commands](#interactive-commands)
 * [Minor Mode](#minor-mode)
 * [Prior Art](#prior-art)
 * [Compatibility and Requirements](#compatibility-and-requirements)

## Quickstart

```bash
$ pip install python-chess
```

```elisp
;; maybe
;; (eval-after-load "pygn-mode"
;;   (define-key pygn-mode-map (kbd "M-f") 'pygn-mode-next-move)
;;   (define-key pygn-mode-map (kbd "M-b") 'pygn-mode-previous-move))

;; or maybe
;; (eval-after-load "pygn-mode"
;;   (define-key pygn-mode-map (kbd "M-f") 'pygn-mode-next-move-follow-board)
;;   (define-key pygn-mode-map (kbd "M-b") 'pygn-mode-previous-move-follow-board))

(require 'pygn-mode)
```

<kbd>M-x</kbd> `pygn-mode-dependency-check`

## pygn-mode

Provides

 * syntax highlighting via `font-lock`, including highlighting of bracketed
   `{comments}` and parenthesized `(variations)`.
 * customizable faces
 * navigation and selection commands
 * computation of FEN at point (requires [python-chess](https://pypi.org/project/python-chess/) library)
 * computation and display of board at point (requires [python-chess](https://pypi.org/project/python-chess/))

## Interactive Commands

No keys are bound by default.  Consider binding keys in an `eval-after-load`
form.

Default mouse bindings are provided:

 * mouse-2 — `pygn-mode-mouse-display-variation-board`
 * double-mouse-2 — `pygn-mode-mouse-display-variation-board-inclusive`

In English, clicking the middle mouse button on a move in Emacs displays a
board image computed before that move was made.  Double-clicking the mouse
button on a move displays a board after that move was made.

In addition, the mouse wheel (buttons 4/5) is bound to `pygn-mode-next-move`
and `pygn-mode-previous-move` when hovering over the `PyGN` lighter in the
modeline.

### Game Navigation Commands

A PGN file may contain multiple concatenated games.  Navigation commands
depend on the convention of each game starting with an `[Event "?"]` tagpair.
(The value is ignored).

Next-game and previous-game commands accept a positive numeric prefix argument.

 * `pygn-mode-next-game`
 * `pygn-mode-previous-game`
 * `pygn-mode-ivy-jump-to-game-by-any-header`
 * `pygn-mode-ivy-jump-to-game-by-fen`

### Move Navigation Commands

Move navigation commands navigate only among the _actual_ played moves of the
chess game.  If the point is within a bracketed comment or a parenthesized
variation, the point will leave the comment or variation in search of a player
move.  This will happen even if the point is looking at a notional move
within the comment or variation.  Similarly, the point will advance over
intervening comments and variations when advancing moves.

Move navigation commands treat move numbers as whitespace.  If the point is
on the move number for a move, it is considered to be on that move.  Move
numbers will be skipped over whenever possible by move navigation.

 * `pygn-mode-next-move` — suggested binding <kbd>M-f</kbd>
 * `pygn-mode-previous-move` — suggested binding <kbd>M-b</kbd>

### Selection Commands

Like game navigation commands, game selection commands depend on the convention
of each game starting with an `[Event "?"]` tagpair.

 * `pygn-mode-select-game`

### FEN Commands

 * `pygn-mode-echo-fen-at-point` — echo FEN, optionally copying to clipboard
 * `pygn-mode-display-fen-at-point` — display FEN in another buffer
 * `pygn-mode-display-variation-fen-at-point` — display FEN, respecting variations

### Board Commands

 * `pygn-mode-display-board-at-point` — display board image in another buffer (format automatic)
 * `pygn-mode-display-gui-board-at-point` — display graphical board image in another buffer
 * `pygn-mode-display-text-board-at-point` — display text board image in another buffer
 * `pygn-mode-display-variation-board-at-point` — display board image, respecting variations
 * `pygn-mode-previous-move-follow-board` — advance to next move and display board image
 * `pygn-mode-next-move-follow-board` — move point to previous move and display board image

### Diagnostic Commands

 * `pygn-mode-dependency-check` — check Python and python-chess dependencies

## Minor Mode

Enabling `pygn-mode-follow-minor-mode` causes a GUI board to be displayed and
updated after any changes to the cursor position.

## Prior Art

emacs-chess  
<https://github.com/jwiegley/emacs-chess>  
[Comparison to emacs-chess](doc/comparison_to_emacs_chess.md)  

## Compatibility and Requirements

GNU Emacs 25

Needed for board images and FENs: [python-chess](https://pypi.org/project/python-chess/)

Needed for jump commands: [ivy-mode.el](https://github.com/abo-abo/swiper)

Uses if present: [nav-flash.el](http://github.com/rolandwalker/nav-flash)
