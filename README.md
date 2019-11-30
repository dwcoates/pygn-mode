<a href="/doc/images/pygn-mode-follow.gif">
<img src="/doc/images/pygn-mode-board.png" width=300 align="right"/>
</a>

# Overview

An Emacs major-mode for chess PGN files, powered by Python.

 * [Quickstart](#quickstart)
 * [pygn-mode](#pygn-mode)
 * [Interactive Commands](#interactive-commands)
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

No interactive commands are bound by default.  Consider binding keys in an
`(eval-after-load "pygn-mode" … )` form.

### Game Navigation Commands

A PGN file may contain multiple concatenated games.  Navigation commands
depend on the convention of each game starting with an `[Event "?"]` tagpair.
(The value is ignored).

Both commands accept a positive numeric prefix argument.

* `pygn-mode-next-game`
* `pygn-mode-previous-game`

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

## Prior Art

emacs-chess  
<https://github.com/jwiegley/emacs-chess>  
[Comparison to emacs-chess](doc/comparison_to_emacs_chess.md)  

## Compatibility and Requirements

Emacs 25

Needed for board images and FENs: [python-chess](https://pypi.org/project/python-chess/)

Uses if present: [nav-flash.el](http://github.com/rolandwalker/nav-flash)
