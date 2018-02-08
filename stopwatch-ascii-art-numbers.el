;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(defconst STOPWATCH-ASCII-ART-NUM-ROWS 4)

;; This ASCII art is built using the box drawing unicode characters, drawn from the range U+2500 to U+256E.
(defconst STOPWATCH-ASCII-ART-NUMBERS
  '((?: . ("  "
           " o"
           " o"
           "  "))

    (?0 . (" ┌────╮"
           " │ ┌┐ │"
           " │ └┘ │"
           " └────┘"))

    (?1 . ("  ┌──╮ "
           "  └┐ │ "
           "  ┌┘ └┐"
           "  └───┘"))

    (?2 . (" ┌────╮"
           " ├──  │"
           " │  ──┤"
           " └────┘"))

    (?3 . (" ┌────╮"
           " ├──  │"
           " ├──  │"
           " └────┘"))

    (?4 . (" ┌─┐┌─╮"
           " │ └┘ │"
           " └──┐ │"
           "    └─┘"))

    (?5 . (" ┌────╮"
           " │  ──┤"
           " ├──  │"
           " └────┘"))

    (?6 . (" ┌─╮   "
           " │ └──╮"
           " │  ─ │"
           " └────┘"))

    (?7 . (" ┌────╮"
           " └──┐ │"
           "    │ │"
           "    └─┘"))

    (?8 . (" ┌────╮"
           " │ ── │"
           " │ ── │"
           " └────┘"))

    (?9 . (" ┌────╮"
           " │ ─  │"
           " └──┐ │"
           "    └─┘"))))

;;; stopwatch-ascii-art-numbers.el ends here
