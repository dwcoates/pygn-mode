# Overview

An Emacs major-mode for viewing chess PGN files.

 * [Quickstart](#quickstart)
 * [pgn-mode](#pgn-mode)
 * [Interactive Commands](#interactive-commands)
 * [Prior Art](#prior-art)
 * [Compatibility and Requirements](#compatibility-and-requirements)

## Quickstart

```elisp
;; maybe
;; (setq font-lock-maximum-decoration t)

;; maybe
;; (eval-after-load "pgn-mode"
;;   (define-key pgn-mode-map (kbd "M-f") 'pgn-mode-next-move)
;;   (define-key pgn-mode-map (kbd "M-b") 'pgn-mode-previous-move))

(require 'pgn-mode)
```

## pgn-mode

Provides

 * syntax highlighting via `font-lock`, including highlighting of bracketed
   `{comments}` and parenthesized `(variations)`.
 * customizable faces
 * navigation and selection commands

## Interactive Commands

No interactive commands are bound by default.  Consider binding keys in an
`(eval-after-load "pgn-mode" … )` form.

### Game Navigation Commands

A PGN file may contain multiple concatenated games.  Navigation commands
depend on the convention of each game starting with an `[Event "?"]` tagpair.
(The value is ignored).

Both commands accept a positive numeric prefix argument.

* `pgn-mode-next-game`
* `pgn-mode-previous-game`

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

 * `pgn-mode-next-move` — suggested binding <kbd>M-f</kbd>
 * `pgn-mode-previous-move` — suggested binding <kbd>M-b</kbd>

### Selection Commands

Like game navigation commands, game selection commands depend on the convention
of each game starting with an `[Event "?"]` tagpair.

 * `pgn-mode-select-game`

## Prior Art

emacs-chess  
<https://github.com/jwiegley/emacs-chess>  

## Compatibility and Requirements

Uses if present: [nav-flash.el](http://github.com/rolandwalker/nav-flash)
