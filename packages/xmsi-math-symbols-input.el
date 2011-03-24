;;-*- coding: utf-8 -*-
;; xmsi-math-symbols-input.el -- a mode to input math chars

;; Copyright © 2010-12-08 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: math symbols, unicode, input

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2.

;;; DESCRIPTION

;; A minor mode for inputing hundreds of math symbols
;; for download location and documentation, see:
;; http://xahlee.org/emacs/xmsi-math-symbols-input.html

;;; INSTALL

;; Open the file, then type 【Alt+x eval-buffer】. That's it.

;; To have emacs automatically load the file when it restarts, follow these steps:

;; Rename the file to 〔xmsi-math-symbols-input.el〕 (if the file is not already that name).
;; place the file in the dir 〔~/.emacs.d/〕. On Windows, it's 〔$HOMEPATH\.emacs.d\〕. Create the 〔.emacs.d〕 folder if you don't have it.

;; Now, put the following lines in your emacs init file “.emacs”:

;;; xmsi-mode 〔xmsi-math-symbols-input.el〕 for inputting math (Unicode) symbols.
;; (add-to-list 'load-path "~/.emacs.d/")
;; (autoload 'xmsi-mode "xmsi-math-symbols-input" "Load xmsi minor mode for inputting math (Unicode) symbols." t)
;; (xmsi-mode 1) ; activate the mode.

;; Then, restart emacs.

;;; DOCUMENTATION

;; Type “inf”, then press 【Shift+Space】, then it becomes “∞”.
;; Type “a”, then press 【Shift+Space】, then it becomes “α”.
;; Type “p”, then press 【Shift+Space】, then it becomes “π”.
;; Type “!=”, then press 【Shift+Space】, then it becomes “≠”.
;; Type “>=”, then press 【Shift+Space】, then it becomes “≥”.
;; Type “=>”, then press 【Shift+Space】, then it becomes “⇒”.
;; Type “->”, then press 【Shift+Space】, then it becomes “→”.
;; Type “and”, then press 【Shift+Space】, then it becomes “∧”.
;; etc.

;; For full list, call “xmsi-list-math-symbols”.

;; The abbreviations are based on Mathematica's aliases 【Esc abbrv Esc】, LaTeX's 「\symbolName」, and SGML/HTML/XML char entity abbreviations.

;; Full documentation is at: http://xahlee.org/emacs/xmsi-math-symbols-input.html

;; To see the inline documentation, call “describe-function” 【Ctrl+h f】, then type “xmsi-mode”.
;; (if you have not load the mode yet, first load it by typing 【Alt+x xmsi-mode】)

;;; HISTORY

;; version 1.2.9, 2011-03-24 Added about 102 full width version chars. Example, “fw&” becomes “＆”. Abbrev all start with “fw”. Remove about 5 redundant abbrevs.
;; version 1.2.8, 2011-02-12 Added ⟵ ⟶ ⟷ ⇔ ⟸ ⟹ ⟺ ↚ ↛ ↮ ⇍ ⇏ ⇎. (few other chars may have been added but missed being mentioned here.)
;; version 1.2.6, 2011-02-11 emdash, endash, figure dash, now have keys m- n- f-. Also, removed some full words keys: “degree” (deg), “pi” (p), “infinity” (inf), “angle” (ang).
;; version 1.2.5, 2011-02-01 added a figure dash.
;; version 1.2.4, 2011-01-11 made the error message more clear. • added symbol for circled number ① ② ... ⒈⒉ ... 🄂 🄃 ...
;; version 1.2.3, 2011-01-06 More clean up, adding symbols. The input string is changed. Now, it's from the cursor position to the left whitespace. Before this change, the input is determined by whitespace chars to the left and right.
;; version 1.2.2, 2010-12-22 Added nbsp for non-breaking-space and some others symbols.
;; version 1.2.1, 2010-12-14 Added about 10 more symbols.
;; version 1.2, 2010-12-14 Added support to enter char by unicode decimal or hexadecimal.
;; version 1.1, 2010-12-12 added more symbols.
;; version 1.0, 2010-12-08 First version.

;;; References
;; http://ia.wikipedia.org/wiki/Wikipedia:LaTeX_symbols
;; http://en.wikipedia.org/wiki/Help:Displaying_a_formula
;; http://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
;; http://www.ctan.org/tex-archive/info/symbols/comprehensive/symbols-a4.pdf
;;  〈Math Symbols in Unicode〉 http://xahlee.org/comp/unicode_math_operators.html
;; http://en.wikipedia.org/wiki/Blackletter
;; http://en.wikipedia.org/wiki/Fraktur_%28script%29
;; http://en.wikipedia.org/wiki/Mathematical_alphanumeric_symbols
;; http://en.wikipedia.org/wiki/Astronomical_symbol
;; http://en.wikipedia.org/wiki/Double_struck

;;; Code:

(setq xmsi-version "1.2.9")

(defvar xmsi-abrvs nil "A abbreviation hash table that maps a string to unicode char.")

(progn
  (setq xmsi-abrvs (make-hash-table :test 'equal))

  (progn
    ;; sgml/html/xhtml/xml entities
    (puthash "nbsp" " " xmsi-abrvs)
    (puthash "bull" "•" xmsi-abrvs)
    (puthash "iexcl" "¡" xmsi-abrvs)
    (puthash "cent" "¢" xmsi-abrvs)
    (puthash "pound" "£" xmsi-abrvs)
    (puthash "curren" "¤" xmsi-abrvs)
    (puthash "yen" "¥" xmsi-abrvs)
    (puthash "brvbar" "¦" xmsi-abrvs)
    (puthash "sect" "§" xmsi-abrvs)
    (puthash "uml" "¨" xmsi-abrvs)
    (puthash "copy" "©" xmsi-abrvs)
    (puthash "ordf" "ª" xmsi-abrvs)
    (puthash "laquo" "«" xmsi-abrvs)
    (puthash "not" "¬" xmsi-abrvs)
    (puthash "reg" "®" xmsi-abrvs)
    (puthash "macr" "¯" xmsi-abrvs)
    (puthash "deg" "°" xmsi-abrvs)
    (puthash "plusmn" "±" xmsi-abrvs)
    (puthash "sup2" "²" xmsi-abrvs)
    (puthash "sup3" "³" xmsi-abrvs)
    (puthash "acute" "´" xmsi-abrvs)
    (puthash "micro" "µ" xmsi-abrvs)
    (puthash "para" "¶" xmsi-abrvs)
    (puthash "middot" "·" xmsi-abrvs)
    (puthash "cedil" "¸" xmsi-abrvs)
    (puthash "sup1" "¹" xmsi-abrvs)
    (puthash "ordm" "º" xmsi-abrvs)
    (puthash "raquo" "»" xmsi-abrvs)
    (puthash "frac14" "¼" xmsi-abrvs)
    (puthash "frac12" "½" xmsi-abrvs)
    (puthash "frac34" "¾" xmsi-abrvs)
    (puthash "iquest" "¿" xmsi-abrvs)
    (puthash "Agrave" "À" xmsi-abrvs)
    (puthash "Aacute" "Á" xmsi-abrvs)
    (puthash "Acirc" "Â" xmsi-abrvs)
    (puthash "Atilde" "Ã" xmsi-abrvs)
    (puthash "Auml" "Ä" xmsi-abrvs)
    (puthash "Aring" "Å" xmsi-abrvs)
    (puthash "AElig" "Æ" xmsi-abrvs)
    (puthash "Ccedil" "Ç" xmsi-abrvs)
    (puthash "Egrave" "È" xmsi-abrvs)
    (puthash "Eacute" "É" xmsi-abrvs)
    (puthash "Ecirc" "Ê" xmsi-abrvs)
    (puthash "Euml" "Ë" xmsi-abrvs)
    (puthash "Igrave" "Ì" xmsi-abrvs)
    (puthash "Iacute" "Í" xmsi-abrvs)
    (puthash "Icirc" "Î" xmsi-abrvs)
    (puthash "Iuml" "Ï" xmsi-abrvs)
    (puthash "ETH" "Ð" xmsi-abrvs)
    (puthash "Ntilde" "Ñ" xmsi-abrvs)
    (puthash "Ograve" "Ò" xmsi-abrvs)
    (puthash "Oacute" "Ó" xmsi-abrvs)
    (puthash "Ocirc" "Ô" xmsi-abrvs)
    (puthash "Otilde" "Õ" xmsi-abrvs)
    (puthash "Ouml" "Ö" xmsi-abrvs)
    (puthash "times" "×" xmsi-abrvs)
    (puthash "Oslash" "Ø" xmsi-abrvs)
    (puthash "Ugrave" "Ù" xmsi-abrvs)
    (puthash "Uacute" "Ú" xmsi-abrvs)
    (puthash "Ucirc" "Û" xmsi-abrvs)
    (puthash "Uuml" "Ü" xmsi-abrvs)
    (puthash "Yacute" "Ý" xmsi-abrvs)
    (puthash "THORN" "Þ" xmsi-abrvs)
    (puthash "szlig" "ß" xmsi-abrvs)
    (puthash "agrave" "à" xmsi-abrvs)
    (puthash "aacute" "á" xmsi-abrvs)
    (puthash "acirc" "â" xmsi-abrvs)
    (puthash "atilde" "ã" xmsi-abrvs)
    (puthash "auml" "ä" xmsi-abrvs)
    (puthash "aring" "å" xmsi-abrvs)
    (puthash "aelig" "æ" xmsi-abrvs)
    (puthash "ccedil" "ç" xmsi-abrvs)
    (puthash "egrave" "è" xmsi-abrvs)
    (puthash "eacute" "é" xmsi-abrvs)
    (puthash "ecirc" "ê" xmsi-abrvs)
    (puthash "euml" "ë" xmsi-abrvs)
    (puthash "igrave" "ì" xmsi-abrvs)
    (puthash "iacute" "í" xmsi-abrvs)
    (puthash "icirc" "î" xmsi-abrvs)
    (puthash "iuml" "ï" xmsi-abrvs)
    (puthash "eth" "ð" xmsi-abrvs)
    (puthash "ntilde" "ñ" xmsi-abrvs)
    (puthash "ograve" "ò" xmsi-abrvs)
    (puthash "oacute" "ó" xmsi-abrvs)
    (puthash "ocirc" "ô" xmsi-abrvs)
    (puthash "otilde" "õ" xmsi-abrvs)
    (puthash "ouml" "ö" xmsi-abrvs)
    (puthash "divide" "÷" xmsi-abrvs)
    (puthash "oslash" "ø" xmsi-abrvs)
    (puthash "ugrave" "ù" xmsi-abrvs)
    (puthash "uacute" "ú" xmsi-abrvs)
    (puthash "ucirc" "û" xmsi-abrvs)
    (puthash "uuml" "ü" xmsi-abrvs)
    (puthash "yacute" "ý" xmsi-abrvs)
    (puthash "thorn" "þ" xmsi-abrvs)
    (puthash "yuml" "ÿ" xmsi-abrvs)
    (puthash "OElig" "Œ" xmsi-abrvs)
    (puthash "oelig" "œ" xmsi-abrvs)
    (puthash "Scaron" "Š" xmsi-abrvs)
    (puthash "scaron" "š" xmsi-abrvs)
    (puthash "Yuml" "Ÿ" xmsi-abrvs)
    (puthash "fnof" "ƒ" xmsi-abrvs)
    (puthash "circ" "ˆ" xmsi-abrvs)
    (puthash "tilde" "˜" xmsi-abrvs)
    (puthash "Alpha" "Α" xmsi-abrvs)
    (puthash "Beta" "Β" xmsi-abrvs)
    (puthash "Gamma" "Γ" xmsi-abrvs)
    (puthash "Delta" "Δ" xmsi-abrvs)
    (puthash "Epsilon" "Ε" xmsi-abrvs)
    (puthash "Zeta" "Ζ" xmsi-abrvs)
    (puthash "Eta" "Η" xmsi-abrvs)
    (puthash "Theta" "Θ" xmsi-abrvs)
    (puthash "Iota" "Ι" xmsi-abrvs)
    (puthash "Kappa" "Κ" xmsi-abrvs)
    (puthash "Lambda" "Λ" xmsi-abrvs)
    (puthash "Mu" "Μ" xmsi-abrvs)
    (puthash "Nu" "Ν" xmsi-abrvs)
    (puthash "Xi" "Ξ" xmsi-abrvs)
    (puthash "Omicron" "Ο" xmsi-abrvs)
    (puthash "Pi" "Π" xmsi-abrvs)
    (puthash "Rho" "Ρ" xmsi-abrvs)
    (puthash "Sigma" "Σ" xmsi-abrvs)
    (puthash "Tau" "Τ" xmsi-abrvs)
    (puthash "Upsilon" "Υ" xmsi-abrvs)
    (puthash "Phi" "Φ" xmsi-abrvs)
    (puthash "Chi" "Χ" xmsi-abrvs)
    (puthash "Psi" "Ψ" xmsi-abrvs)
    (puthash "Omega" "Ω" xmsi-abrvs)
    (puthash "alpha" "α" xmsi-abrvs)
    (puthash "beta" "β" xmsi-abrvs)
    (puthash "gamma" "γ" xmsi-abrvs)
    (puthash "delta" "δ" xmsi-abrvs)
    (puthash "epsilon" "ε" xmsi-abrvs)
    (puthash "zeta" "ζ" xmsi-abrvs)
    (puthash "eta" "η" xmsi-abrvs)
    (puthash "theta" "θ" xmsi-abrvs)
    (puthash "iota" "ι" xmsi-abrvs)
    (puthash "kappa" "κ" xmsi-abrvs)
    (puthash "lambda" "λ" xmsi-abrvs)
    (puthash "mu" "μ" xmsi-abrvs)
    (puthash "nu" "ν" xmsi-abrvs)
    (puthash "xi" "ξ" xmsi-abrvs)
    (puthash "omicron" "ο" xmsi-abrvs)
    (puthash "pi" "π" xmsi-abrvs)
    (puthash "rho" "ρ" xmsi-abrvs)
    (puthash "sigmaf" "ς" xmsi-abrvs)
    (puthash "sigma" "σ" xmsi-abrvs)
    (puthash "tau" "τ" xmsi-abrvs)
    (puthash "upsilon" "υ" xmsi-abrvs)
    (puthash "phi" "φ" xmsi-abrvs)
    (puthash "chi" "χ" xmsi-abrvs)
    (puthash "psi" "ψ" xmsi-abrvs)
    (puthash "omega" "ω" xmsi-abrvs)
    (puthash "thetasym" "ϑ" xmsi-abrvs)
    (puthash "upsih" "ϒ" xmsi-abrvs)
    (puthash "piv" "ϖ" xmsi-abrvs)
    (puthash "ndash" "–" xmsi-abrvs)
    (puthash "mdash" "—" xmsi-abrvs)
    (puthash "lsquo" "‘" xmsi-abrvs)
    (puthash "rsquo" "’" xmsi-abrvs)
    (puthash "sbquo" "‚" xmsi-abrvs)
    (puthash "ldquo" "“" xmsi-abrvs)
    (puthash "rdquo" "”" xmsi-abrvs)
    (puthash "bdquo" "„" xmsi-abrvs)
    (puthash "dagger" "†" xmsi-abrvs)
    (puthash "Dagger" "‡" xmsi-abrvs)
    (puthash "hellip" "…" xmsi-abrvs)
    (puthash "permil" "‰" xmsi-abrvs)
    (puthash "prime" "′" xmsi-abrvs)
    (puthash "Prime" "″" xmsi-abrvs)
    (puthash "lsaquo" "‹" xmsi-abrvs)
    (puthash "rsaquo" "›" xmsi-abrvs)
    (puthash "oline" "‾" xmsi-abrvs)
    (puthash "frasl" "⁄" xmsi-abrvs)
    (puthash "euro" "€" xmsi-abrvs)
    (puthash "image" "ℑ" xmsi-abrvs)
    (puthash "weierp" "℘" xmsi-abrvs)
    (puthash "real" "ℜ" xmsi-abrvs)
    (puthash "trade" "™" xmsi-abrvs)
    (puthash "alefsym" "ℵ" xmsi-abrvs)
    (puthash "larr" "←" xmsi-abrvs)
    (puthash "uarr" "↑" xmsi-abrvs)
    (puthash "rarr" "→" xmsi-abrvs)
    (puthash "darr" "↓" xmsi-abrvs)
    (puthash "harr" "↔" xmsi-abrvs)
    (puthash "crarr" "↵" xmsi-abrvs)
    (puthash "lArr" "⇐" xmsi-abrvs)
    (puthash "uArr" "⇑" xmsi-abrvs)
    (puthash "rArr" "⇒" xmsi-abrvs)
    (puthash "dArr" "⇓" xmsi-abrvs)
    (puthash "hArr" "⇔" xmsi-abrvs)
    (puthash "forall" "∀" xmsi-abrvs)
    (puthash "part" "∂" xmsi-abrvs)
    (puthash "exist" "∃" xmsi-abrvs)
    (puthash "nabla" "∇" xmsi-abrvs)
    (puthash "isin" "∈" xmsi-abrvs)
    (puthash "notin" "∉" xmsi-abrvs)
    (puthash "ni" "∋" xmsi-abrvs)
    (puthash "prod" "∏" xmsi-abrvs)
    (puthash "sum" "∑" xmsi-abrvs)
    (puthash "minus" "−" xmsi-abrvs)
    (puthash "lowast" "∗" xmsi-abrvs)
    (puthash "radic" "√" xmsi-abrvs)
    (puthash "prop" "∝" xmsi-abrvs)
    (puthash "infin" "∞" xmsi-abrvs)
    (puthash "ang" "∠" xmsi-abrvs)
    (puthash "and" "∧" xmsi-abrvs)
    (puthash "or" "∨" xmsi-abrvs)
    (puthash "cap" "∩" xmsi-abrvs)
    (puthash "cup" "∪" xmsi-abrvs)
    (puthash "int" "∫" xmsi-abrvs)
    (puthash "there4" "∴" xmsi-abrvs)
    (puthash "sim" "∼" xmsi-abrvs)
    (puthash "cong" "≅" xmsi-abrvs)
    (puthash "asymp" "≈" xmsi-abrvs)
    (puthash "ne" "≠" xmsi-abrvs)
    (puthash "equiv" "≡" xmsi-abrvs)
    (puthash "le" "≤" xmsi-abrvs)
    (puthash "ge" "≥" xmsi-abrvs)
    (puthash "sub" "⊂" xmsi-abrvs)
    (puthash "sup" "⊃" xmsi-abrvs)
    (puthash "nsub" "⊄" xmsi-abrvs)
    (puthash "sube" "⊆" xmsi-abrvs)
    (puthash "supe" "⊇" xmsi-abrvs)
    (puthash "oplus" "⊕" xmsi-abrvs)
    (puthash "otimes" "⊗" xmsi-abrvs)
    (puthash "perp" "⊥" xmsi-abrvs)
    (puthash "sdot" "⋅" xmsi-abrvs)
    (puthash "lceil" "⌈" xmsi-abrvs)
    (puthash "rceil" "⌉" xmsi-abrvs)
    (puthash "lfloor" "⌊" xmsi-abrvs)
    (puthash "rfloor" "⌋" xmsi-abrvs)
    (puthash "lang" "〈" xmsi-abrvs)
    (puthash "rang" "〉" xmsi-abrvs)
    (puthash "loz" "◊" xmsi-abrvs)
    (puthash "spades" "♠" xmsi-abrvs)
    (puthash "clubs" "♣" xmsi-abrvs)
    (puthash "hearts" "♥" xmsi-abrvs)
    (puthash "diams" "♦" xmsi-abrvs)
    )

  ;; Double struck letter forms (aka Double struck; double stroke)
  ;; others outside of the BMP (Unicode's Basic Multilingual Plane). Not much font supports it.
  (puthash "dsC" "ℂ" xmsi-abrvs)
  (puthash "dsH" "ℍ" xmsi-abrvs)
  (puthash "dsN" "ℕ" xmsi-abrvs)
  (puthash "dsP" "ℙ" xmsi-abrvs)
  (puthash "dsQ" "ℚ" xmsi-abrvs)
  (puthash "dsR" "ℝ" xmsi-abrvs)
  (puthash "dsZ" "ℤ" xmsi-abrvs)

  (puthash "dd" "ⅆ" xmsi-abrvs)
  (puthash "ee" "ⅇ" xmsi-abrvs)
  (puthash "ii" "ⅈ" xmsi-abrvs)
  (puthash "jj" "ⅉ" xmsi-abrvs)

  (puthash "dsd" "ⅆ" xmsi-abrvs)
  (puthash "dse" "ⅇ" xmsi-abrvs)
  (puthash "dsi" "ⅈ" xmsi-abrvs)
  (puthash "dsj" "ⅉ" xmsi-abrvs)

  ;; gothic letter forms (aka FRANKTUR). Most are outside BMP
  (puthash "goA" "𝔄" xmsi-abrvs)
  (puthash "goB" "𝔅" xmsi-abrvs)
  (puthash "goC" "ℭ" xmsi-abrvs)
  (puthash "goD" "𝔇" xmsi-abrvs)
  (puthash "goE" "𝔈" xmsi-abrvs)
  (puthash "goF" "𝔉" xmsi-abrvs)
  (puthash "goG" "𝔊" xmsi-abrvs)
  (puthash "goH" "ℌ" xmsi-abrvs)
  (puthash "goI" "ℑ" xmsi-abrvs)
  (puthash "goJ" "𝔍" xmsi-abrvs)
  (puthash "goK" "𝔎" xmsi-abrvs)
  (puthash "goL" "𝔏" xmsi-abrvs)
  (puthash "goM" "𝔐" xmsi-abrvs)
  (puthash "goN" "𝔑" xmsi-abrvs)
  (puthash "goO" "𝔒" xmsi-abrvs)
  (puthash "goP" "𝔓" xmsi-abrvs)
  (puthash "goQ" "𝔔" xmsi-abrvs)
  (puthash "goR" "ℜ" xmsi-abrvs)
  (puthash "goS" "𝔖" xmsi-abrvs)
  (puthash "goT" "𝔗" xmsi-abrvs)
  (puthash "goU" "𝔘" xmsi-abrvs)
  (puthash "goV" "𝔙" xmsi-abrvs)
  (puthash "goW" "𝔚" xmsi-abrvs)
  (puthash "goX" "𝔛" xmsi-abrvs)
  (puthash "goY" "𝔜" xmsi-abrvs)
  (puthash "goZ" "ℨ" xmsi-abrvs)
  (puthash "goa" "𝔞" xmsi-abrvs)
  (puthash "gob" "𝔟" xmsi-abrvs)
  (puthash "goc" "𝔠" xmsi-abrvs)
  (puthash "god" "𝔡" xmsi-abrvs)
  (puthash "goe" "𝔢" xmsi-abrvs)
  (puthash "gof" "𝔣" xmsi-abrvs)
  (puthash "gog" "𝔤" xmsi-abrvs)
  (puthash "goh" "𝔥" xmsi-abrvs)
  (puthash "goi" "𝔦" xmsi-abrvs)
  (puthash "goj" "𝔧" xmsi-abrvs)
  (puthash "gok" "𝔨" xmsi-abrvs)
  (puthash "gol" "𝔩" xmsi-abrvs)
  (puthash "gom" "𝔪" xmsi-abrvs)
  (puthash "gon" "𝔫" xmsi-abrvs)
  (puthash "goo" "𝔬" xmsi-abrvs)
  (puthash "gop" "𝔭" xmsi-abrvs)
  (puthash "goq" "𝔮" xmsi-abrvs)
  (puthash "gor" "𝔯" xmsi-abrvs)
  (puthash "gos" "𝔰" xmsi-abrvs)
  (puthash "got" "𝔱" xmsi-abrvs)
  (puthash "gou" "𝔲" xmsi-abrvs)
  (puthash "gov" "𝔳" xmsi-abrvs)
  (puthash "gow" "𝔴" xmsi-abrvs)
  (puthash "gox" "𝔵" xmsi-abrvs)
  (puthash "goy" "𝔶" xmsi-abrvs)
  (puthash "goz" "𝔷" xmsi-abrvs)

  ;; Scripted letter forms. Most are outside BMP.
  (puthash "sca" "𝒶" xmsi-abrvs)
  (puthash "scb" "𝒷" xmsi-abrvs)
  (puthash "scc" "𝒸" xmsi-abrvs)
  (puthash "scd" "𝒹" xmsi-abrvs)
  (puthash "sce" "ℯ" xmsi-abrvs) ; in BMP
  (puthash "scf" "𝒻" xmsi-abrvs)
  (puthash "scg" "ℊ" xmsi-abrvs) ; in BMP
  (puthash "sch" "𝒽" xmsi-abrvs)
  (puthash "sci" "𝒾" xmsi-abrvs)
  (puthash "scj" "𝒿" xmsi-abrvs)
  (puthash "sck" "𝓀" xmsi-abrvs)
  (puthash "scl2" "𝓁" xmsi-abrvs)
  (puthash "scl" "ℓ" xmsi-abrvs) ;in BMP
  (puthash "scm" "𝓂" xmsi-abrvs)
  (puthash "scn" "𝓃" xmsi-abrvs)
  (puthash "sco" "ℴ" xmsi-abrvs) ; in BMP
  (puthash "scp" "𝓅" xmsi-abrvs)
  (puthash "scq" "𝓆" xmsi-abrvs)
  (puthash "scw" "𝓌" xmsi-abrvs)
  (puthash "scx" "𝓍" xmsi-abrvs)
  (puthash "scy" "𝓎" xmsi-abrvs)
  (puthash "scz" "𝓏" xmsi-abrvs)

  (puthash "scB" "ℬ" xmsi-abrvs)
  (puthash "scE" "ℰ" xmsi-abrvs)
  (puthash "scF" "ℱ" xmsi-abrvs)
  (puthash "scH" "ℋ" xmsi-abrvs)
  (puthash "scI" "ℐ" xmsi-abrvs)
  (puthash "scL" "ℒ" xmsi-abrvs)
  (puthash "scM" "ℳ" xmsi-abrvs)
  (puthash "scP" "℘" xmsi-abrvs)
  (puthash "scR" "ℛ" xmsi-abrvs)

 ;; a b c d e f g h i j k l m n o p q w x y z
 ;; A B C D E F G H I J K L M N O P Q W X Y Z

  ;; accented letters
  (puthash "a`" "à" xmsi-abrvs)
  (puthash "e`" "è" xmsi-abrvs)
  (puthash "i`" "ì" xmsi-abrvs)
  (puthash "o`" "ò" xmsi-abrvs)
  (puthash "u`" "ù" xmsi-abrvs)
  (puthash "A`" "À" xmsi-abrvs)
  (puthash "E`" "È" xmsi-abrvs)
  (puthash "I`" "Ì" xmsi-abrvs)
  (puthash "O`" "Ò" xmsi-abrvs)
  (puthash "U`" "Ù" xmsi-abrvs)

  (puthash "a^" "â" xmsi-abrvs)
  (puthash "e^" "ê" xmsi-abrvs)
  (puthash "i^" "î" xmsi-abrvs)
  (puthash "o^" "ô" xmsi-abrvs)
  (puthash "u^" "û" xmsi-abrvs)
  (puthash "A^" "Â" xmsi-abrvs)
  (puthash "E^" "Ê" xmsi-abrvs)
  (puthash "I^" "Î" xmsi-abrvs)
  (puthash "O^" "Ô" xmsi-abrvs)
  (puthash "U^" "Û" xmsi-abrvs)

  (puthash "a'" "á" xmsi-abrvs)
  (puthash "e'" "é" xmsi-abrvs)
  (puthash "i'" "í" xmsi-abrvs)
  (puthash "o'" "ó" xmsi-abrvs)
  (puthash "u'" "ú" xmsi-abrvs)
  (puthash "y'" "ý" xmsi-abrvs)
  (puthash "A'" "Á" xmsi-abrvs)
  (puthash "E'" "É" xmsi-abrvs)
  (puthash "I'" "Í" xmsi-abrvs)
  (puthash "O'" "Ó" xmsi-abrvs)
  (puthash "U'" "Ú" xmsi-abrvs)
  (puthash "Y'" "Ý" xmsi-abrvs)

  (puthash "A\"" "Ä" xmsi-abrvs)
  (puthash "E\"" "Ë" xmsi-abrvs)
  (puthash "I\"" "Ï" xmsi-abrvs)
  (puthash "O\"" "Ö" xmsi-abrvs)
  (puthash "U\"" "Ü" xmsi-abrvs)
  (puthash "a\"" "ä" xmsi-abrvs)
  (puthash "e\"" "ë" xmsi-abrvs)
  (puthash "i\"" "ï" xmsi-abrvs)
  (puthash "o\"" "ö" xmsi-abrvs)
  (puthash "u\"" "ü" xmsi-abrvs)
  (puthash "s\"" "ß" xmsi-abrvs)
  (puthash "y\"" "ÿ" xmsi-abrvs)

  (puthash "Ao" "Å" xmsi-abrvs)
  (puthash "ao" "å" xmsi-abrvs)

  (puthash "AE" "Æ" xmsi-abrvs)
  (puthash "ae" "æ" xmsi-abrvs)

  (puthash "a~" "ã" xmsi-abrvs)
  (puthash "n~" "ñ" xmsi-abrvs)
  (puthash "o~" "õ" xmsi-abrvs)
  (puthash "A~" "Ã" xmsi-abrvs)
  (puthash "N~" "Ñ" xmsi-abrvs)
  (puthash "O~" "Õ" xmsi-abrvs)

  ;; misc non-math symbols
  (puthash "tm" "™" xmsi-abrvs)
  (puthash "3/4" "¾" xmsi-abrvs)
  (puthash "1/2" "½" xmsi-abrvs)
  (puthash "1/4" "¼" xmsi-abrvs)
  (puthash "..." "…" xmsi-abrvs)
  (puthash "dag" "†" xmsi-abrvs)
  (puthash "ddag" "‡" xmsi-abrvs)
  (puthash "--" "—" xmsi-abrvs)
  (puthash "-" "‒" xmsi-abrvs)
  (puthash "s&" "﹠" xmsi-abrvs)

  (puthash "ltrib" "◀" xmsi-abrvs)
  (puthash "rtrib" "▶" xmsi-abrvs)
  (puthash "trib" "▲" xmsi-abrvs)
  (puthash "dtrib" "▼" xmsi-abrvs)
  (puthash "ltri" "◁" xmsi-abrvs)
  (puthash "rtri" "▷" xmsi-abrvs)
  (puthash "tri" "△" xmsi-abrvs)
  (puthash "dtri" "▽" xmsi-abrvs)
  (puthash "sq" "□" xmsi-abrvs)
  (puthash "cir" "○" xmsi-abrvs)
  (puthash "dia" "◇" xmsi-abrvs)
  (puthash "sqb" "■" xmsi-abrvs)
  (puthash "cirb" "●" xmsi-abrvs)
  (puthash "diab" "◆" xmsi-abrvs)
  (puthash "<3" "♥" xmsi-abrvs)

  (puthash ":)" "☺" xmsi-abrvs)
  (puthash ":(" "☹" xmsi-abrvs)

  ;; computer keys and symbols
  (puthash "cmd" "⌘" xmsi-abrvs)
  (puthash "opt" "⌥" xmsi-abrvs)
  (puthash "caret" "‸" xmsi-abrvs)
  (puthash "pgup" "⇞"xmsi-abrvs)
  (puthash "pgdn" "⇟"xmsi-abrvs)
  (puthash "nwarr" "↖"xmsi-abrvs)
  (puthash "searr" "↘"xmsi-abrvs)
  (puthash "ldel" "⌫"xmsi-abrvs)
  (puthash "del" "⌦"xmsi-abrvs)
  (puthash "power" "⎋"xmsi-abrvs)
  (puthash "eject" "⏏"xmsi-abrvs)
  (puthash "undo" "↶"xmsi-abrvs)
  (puthash "redo" "↷"xmsi-abrvs)
  (puthash "shift" "⇧"xmsi-abrvs)
  (puthash "enter" "⌤"xmsi-abrvs)

  (puthash "lrarr" "⇄"xmsi-abrvs)
  (puthash "|<-" "⇤"xmsi-abrvs)
  (puthash "->|" "⇥"xmsi-abrvs)
  (puthash "tab" "↹"xmsi-abrvs)
  (puthash "return3" "↵"xmsi-abrvs)
  (puthash "return2" "↩"xmsi-abrvs)
  (puthash "return" "⏎"xmsi-abrvs)
  (puthash "clear" "⌧"xmsi-abrvs)
  (puthash "keyboard" "⌨"xmsi-abrvs)
  (puthash "tt" "␣"xmsi-abrvs)
  (puthash "ibeam" "⌶"xmsi-abrvs)
  (puthash "prevpage" "⎗"xmsi-abrvs)
  (puthash "nextpage" "⎘"xmsi-abrvs)
  (puthash "print" "⎙"xmsi-abrvs)
  (puthash "watch" "⌚"xmsi-abrvs)
  (puthash "hourglass" "⌛"xmsi-abrvs)
  (puthash "scissor" "✂"xmsi-abrvs)
  (puthash "scissorw" "✄"xmsi-abrvs)
  (puthash "envelope" "✉"xmsi-abrvs)
  (puthash "writing" "✍"xmsi-abrvs)

  ;; misc math
  (puthash "+-" "±" xmsi-abrvs)
  (puthash "-+" "∓" xmsi-abrvs)

  ;; superscripts
  (puthash "^0" "⁰" xmsi-abrvs)
  (puthash "^1" "¹" xmsi-abrvs)
  (puthash "^2" "²" xmsi-abrvs)
  (puthash "^3" "³" xmsi-abrvs)
  (puthash "^4" "⁴" xmsi-abrvs)
  (puthash "^5" "⁵" xmsi-abrvs)
  (puthash "^6" "⁶" xmsi-abrvs)
  (puthash "^7" "⁷" xmsi-abrvs)
  (puthash "^8" "⁸" xmsi-abrvs)
  (puthash "^9" "⁹" xmsi-abrvs)
  (puthash "^+" "⁺" xmsi-abrvs)
  (puthash "^-" "⁻" xmsi-abrvs)
  (puthash "^=" "⁼" xmsi-abrvs)
  (puthash "^(" "⁽" xmsi-abrvs)
  (puthash "^)" "⁾" xmsi-abrvs)
  (puthash "^n" "ⁿ" xmsi-abrvs)
  (puthash "^i" "ⁱ" xmsi-abrvs)

  ;; subscripts
  (puthash "_(" "₍" xmsi-abrvs)
  (puthash "_)" "₎" xmsi-abrvs)
  (puthash "_+" "₊" xmsi-abrvs)
  (puthash "_-" "₋" xmsi-abrvs)
  (puthash "_0" "₀" xmsi-abrvs)
  (puthash "_1" "₁" xmsi-abrvs)
  (puthash "_2" "₂" xmsi-abrvs)
  (puthash "_3" "₃" xmsi-abrvs)
  (puthash "_4" "₄" xmsi-abrvs)
  (puthash "_5" "₅" xmsi-abrvs)
  (puthash "_6" "₆" xmsi-abrvs)
  (puthash "_7" "₇" xmsi-abrvs)
  (puthash "_8" "₈" xmsi-abrvs)
  (puthash "_9" "₉" xmsi-abrvs)
  (puthash "_=" "₌" xmsi-abrvs)
  (puthash "_a" "ₐ" xmsi-abrvs)
  (puthash "_e" "ₑ" xmsi-abrvs)
  (puthash "_i" "ᵢ" xmsi-abrvs)
  (puthash "_j" "ⱼ" xmsi-abrvs)
  (puthash "_o" "ₒ" xmsi-abrvs)
  (puthash "_schwa" "ₔ" xmsi-abrvs)
  (puthash "_v" "ᵥ" xmsi-abrvs)
  (puthash "_x" "ᵣ" xmsi-abrvs)
  (puthash "_x" "ᵤ" xmsi-abrvs)
  (puthash "_x" "ₓ" xmsi-abrvs)

  ;; astronomy
  (puthash "sun" "☉" xmsi-abrvs)
  (puthash "sunray" "☼" xmsi-abrvs)
  (puthash "moon" "☾" xmsi-abrvs)
  (puthash "moonr" "☽" xmsi-abrvs)
  (puthash "mercury" "☿" xmsi-abrvs)
  (puthash "earth" "♁" xmsi-abrvs)
  (puthash "saturn" "♄" xmsi-abrvs)
  (puthash "uranus" "♅" xmsi-abrvs)
  (puthash "neptune" "♆" xmsi-abrvs)
  (puthash "pluto" "♇" xmsi-abrvs)
  (puthash "jupiter" "♃" xmsi-abrvs)
  (puthash "male" "♂" xmsi-abrvs)
  (puthash "mars" "♂" xmsi-abrvs)
  (puthash "female" "♀" xmsi-abrvs)
  (puthash "venus" "♀" xmsi-abrvs)
  (puthash "comet" "☄" xmsi-abrvs)

  ;; forms for constants-like things
  (puthash "inf" "∞" xmsi-abrvs)
  (puthash "O/" "∅" xmsi-abrvs)
  (puthash "o/" "Ø" xmsi-abrvs)
  (puthash "es" "Ø" xmsi-abrvs)

  ;; brackets, matching pairs
  (puthash "flr" "⌊⌋" xmsi-abrvs)
  (puthash "ceil" "⌈⌉" xmsi-abrvs)
  (puthash "\"" "“”" xmsi-abrvs)
  (puthash "[" "「」" xmsi-abrvs)
  (puthash "[[" "『』" xmsi-abrvs)
  (puthash "[2" "【】" xmsi-abrvs)
  (puthash "[3" "〖〗" xmsi-abrvs)
  (puthash "(" "〔〕" xmsi-abrvs)
  (puthash "<" "〈〉" xmsi-abrvs)
  (puthash "<<" "《》" xmsi-abrvs)
  (puthash "<2" "‹›" xmsi-abrvs)
  (puthash "<<2" "«»" xmsi-abrvs)

  ;; number forms
  (puthash "c1" "①" xmsi-abrvs)
  (puthash "c2" "②" xmsi-abrvs)
  (puthash "c3" "③" xmsi-abrvs)
  (puthash "c4" "④" xmsi-abrvs)
  (puthash "c5" "⑤" xmsi-abrvs)
  (puthash "c6" "⑥" xmsi-abrvs)
  (puthash "c7" "⑦" xmsi-abrvs)
  (puthash "c8" "⑧" xmsi-abrvs)
  (puthash "c9" "⑨" xmsi-abrvs)
  (puthash "c0" "⓪" xmsi-abrvs)

(puthash "1." "⒈" xmsi-abrvs)
(puthash "2." "⒉" xmsi-abrvs)
(puthash "3." "⒊" xmsi-abrvs)
(puthash "4." "⒋" xmsi-abrvs)
(puthash "5." "⒌" xmsi-abrvs)
(puthash "6." "⒍" xmsi-abrvs)
(puthash "7." "⒎" xmsi-abrvs)
(puthash "8." "⒏" xmsi-abrvs)
(puthash "9." "⒐" xmsi-abrvs)
(puthash "0." "🄀" xmsi-abrvs)

(puthash "1," "🄂" xmsi-abrvs)
(puthash "2," "🄃" xmsi-abrvs)
(puthash "3," "🄄" xmsi-abrvs)
(puthash "4," "🄅" xmsi-abrvs)
(puthash "5," "🄆" xmsi-abrvs)
(puthash "6," "🄇" xmsi-abrvs)
(puthash "7," "🄈" xmsi-abrvs)
(puthash "8," "🄉" xmsi-abrvs)
(puthash "9," "🄊" xmsi-abrvs)
(puthash "0," "🄁" xmsi-abrvs)

  ;; music
(puthash "notes4" "♩" xmsi-abrvs)
(puthash "notes8" "♪" xmsi-abrvs)
(puthash "notes8d" "♫" xmsi-abrvs)
(puthash "notes16d" "♬" xmsi-abrvs)
(puthash "flat" "♭" xmsi-abrvs)
(puthash "natural" "♮" xmsi-abrvs)
(puthash "sharp" "♯" xmsi-abrvs)

  ;; letters
  ;; greek alphabets http://en.wikipedia.org/wiki/Greek_alphabet
  ;; ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ
  ;; αβγδεζηθικλμνξοπρστυφχψω
  ;;                  ς
  (puthash "a" "α" xmsi-abrvs)
  (puthash "b" "β" xmsi-abrvs)
  (puthash "g" "γ" xmsi-abrvs)
  (puthash "d" "δ" xmsi-abrvs)
  (puthash "e" "ε" xmsi-abrvs)
  (puthash "z" "ζ" xmsi-abrvs)
  (puthash "h" "η" xmsi-abrvs)
  (puthash "q" "θ" xmsi-abrvs)
  (puthash "i" "ι" xmsi-abrvs)
  (puthash "k" "κ" xmsi-abrvs)
  (puthash "l" "λ" xmsi-abrvs)
  (puthash "m" "μ" xmsi-abrvs)
  (puthash "n" "ν" xmsi-abrvs)
  (puthash "x" "ξ" xmsi-abrvs)
  ;; (puthash "a" "ο" xmsi-abrvs)
  (puthash "p" "π" xmsi-abrvs)
  (puthash "r" "ρ" xmsi-abrvs)
  (puthash "s" "σ" xmsi-abrvs)
  (puthash "t" "τ" xmsi-abrvs)
  (puthash "v" "υ" xmsi-abrvs)
  (puthash "f" "φ" xmsi-abrvs)
  (puthash "c" "χ" xmsi-abrvs)
  (puthash "y" "ψ" xmsi-abrvs)
  (puthash "o" "ω" xmsi-abrvs)

  (puthash "A" "Α" xmsi-abrvs)
  (puthash "B" "Β" xmsi-abrvs)
  (puthash "G" "Γ" xmsi-abrvs)
  (puthash "D" "Δ" xmsi-abrvs)
  (puthash "E" "Ε" xmsi-abrvs)
  (puthash "Z" "Ζ" xmsi-abrvs)
  (puthash "h" "Η" xmsi-abrvs)
  (puthash "Q" "Θ" xmsi-abrvs)
  (puthash "I" "Ι" xmsi-abrvs)
  (puthash "K" "Κ" xmsi-abrvs)
  (puthash "L" "Λ" xmsi-abrvs)
  (puthash "M" "Μ" xmsi-abrvs)
  (puthash "N" "Ν" xmsi-abrvs)
  (puthash "X" "Ξ" xmsi-abrvs)
  ;; (Puthash "A" "Ο" xmsi-abrvs)
  (puthash "P" "Π" xmsi-abrvs)
  (puthash "r" "Ρ" xmsi-abrvs)
  (puthash "S" "Σ" xmsi-abrvs)
  (puthash "T" "Τ" xmsi-abrvs)
  (puthash "V" "Υ" xmsi-abrvs)
  (puthash "F" "Φ" xmsi-abrvs)
  (puthash "C" "Χ" xmsi-abrvs)
  (puthash "Y" "Ψ" xmsi-abrvs)
  (puthash "O" "Ω" xmsi-abrvs)

  ;; letter-like forms
  (puthash "al" "ℵ" xmsi-abrvs)
  (puthash "alef" "ℵ" xmsi-abrvs)
  (puthash "aleph" "ℵ" xmsi-abrvs)
  (puthash "beth" "ב" xmsi-abrvs)
  (puthash "gimel" "ג" xmsi-abrvs)
  (puthash "dalet" "ד" xmsi-abrvs)
  (puthash "daleth" "ד" xmsi-abrvs)
  (puthash "Digamma" "Ϝ" xmsi-abrvs)
  (puthash "digamma" "ϝ" xmsi-abrvs)
  (puthash "wp" "℘" xmsi-abrvs)
  (puthash "angstrom" "Å" xmsi-abrvs)
  (puthash "R2" "ℝ²" xmsi-abrvs)
  (puthash "R3" "ℝ³" xmsi-abrvs)
  (puthash "fun" "ƒ" xmsi-abrvs)

  ;; relations
  (puthash "<=" "≤" xmsi-abrvs)
  (puthash ">=" "≥" xmsi-abrvs)
  (puthash "!el" "∉" xmsi-abrvs)
  (puthash "el" "∈" xmsi-abrvs)
  (puthash "&&" "∧" xmsi-abrvs)
  (puthash "||" "∨" xmsi-abrvs)
  (puthash "not" "¬" xmsi-abrvs) ; not
  (puthash "===" "≡" xmsi-abrvs) ; equivalent
  (puthash "!=" "≠" xmsi-abrvs) (puthash "notequal" "≠" xmsi-abrvs) ; not equal
  (puthash "fa" "∀" xmsi-abrvs) (puthash "forall" "∀" xmsi-abrvs) ; FOR ALL
  (puthash "ex" "∃" xmsi-abrvs) ; THERE EXISTS

  ;; operators
  (puthash "c+" "⊕" xmsi-abrvs)
  (puthash "c*" "⊗" xmsi-abrvs)
  (puthash "*" "×" xmsi-abrvs)
  (puthash "'" "′" xmsi-abrvs)
  (puthash "''" "″" xmsi-abrvs)
  (puthash "'''" "‴" xmsi-abrvs)
  (puthash "." "·" xmsi-abrvs)
  (puthash "root" "√" xmsi-abrvs)
  (puthash "sqrt" "√" xmsi-abrvs)
  (puthash "rt" "√" xmsi-abrvs)
  (puthash "del" "∇" xmsi-abrvs)
  (puthash "part" "∂" xmsi-abrvs)
  (puthash "partial" "∂" xmsi-abrvs)
  (puthash "pd" "∂" xmsi-abrvs)
  (puthash "cross" "⨯" xmsi-abrvs)
  (puthash "cint" "∮" xmsi-abrvs) ; contour integral
  (puthash "ccint" "∲" xmsi-abrvs)
  (puthash "cccint" "∳" xmsi-abrvs)
  (puthash "union" "∩" xmsi-abrvs)
  (puthash "intersection" "∪" xmsi-abrvs)

  (puthash "/_" "∠" xmsi-abrvs)
  (puthash "rightangle" "⦜" xmsi-abrvs)
  (puthash "|_" "⦜" xmsi-abrvs)
  (puthash "measuredangle" "∡" xmsi-abrvs)
  (puthash "sphericalangle" "∢" xmsi-abrvs)

  (puthash "intersection" "∪" xmsi-abrvs)
  (puthash "intersection" "∪" xmsi-abrvs)

  ;; arrows and maps
  (puthash "<-" "←" xmsi-abrvs)
  (puthash "->" "→" xmsi-abrvs)
  (puthash "<->" "↔" xmsi-abrvs)
  (puthash "!<-" "↚" xmsi-abrvs)
  (puthash "!->" "↛" xmsi-abrvs)
  (puthash "!<->" "↮" xmsi-abrvs)

(puthash "<=2" "⇐" xmsi-abrvs)
(puthash "=>" "⇒" xmsi-abrvs)
(puthash "<=>" "⇔" xmsi-abrvs)
(puthash "!<=" "⇍" xmsi-abrvs)
(puthash "!=>" "⇏" xmsi-abrvs)
(puthash "!=>" "⇎" xmsi-abrvs)
  
(puthash "<==" "⟸" xmsi-abrvs)
(puthash "==>" "⟹" xmsi-abrvs)
(puthash "<==>" "⟺" xmsi-abrvs)
 
  (puthash "<-|" "↤" xmsi-abrvs)
  (puthash "|->" "↦" xmsi-abrvs)

  (puthash "<--" "⟵" xmsi-abrvs)
  (puthash "-->" "⟶" xmsi-abrvs)
  (puthash "<-->" "⟷" xmsi-abrvs)

  (puthash "xor" "⊻" xmsi-abrvs)
  (puthash "nand" "⊼" xmsi-abrvs)
  (puthash "nor" "⊽" xmsi-abrvs)

  ;; duplicates. full word as input or variant abbrev. Only duplicates appear here.
  (puthash "m-" "—" xmsi-abrvs)
  (puthash "f-" "‒" xmsi-abrvs)
  (puthash "n-" "–" xmsi-abrvs)
  (puthash "star" "★" xmsi-abrvs)
  (puthash "(c)" "©" xmsi-abrvs)

(puthash "fw," "，" xmsi-abrvs)
(puthash "fw." "．" xmsi-abrvs)
(puthash "fw:" "：" xmsi-abrvs)
(puthash "fw;" "；" xmsi-abrvs)
(puthash "fw!" "！" xmsi-abrvs)
(puthash "fw?" "？" xmsi-abrvs)
(puthash "fw`" "｀" xmsi-abrvs)
(puthash "fw'" "＇" xmsi-abrvs)
(puthash "fw\"" "＂" xmsi-abrvs)
(puthash "fw&" "＆" xmsi-abrvs)

(puthash "fw(" "（" xmsi-abrvs)
(puthash "fw)" "）" xmsi-abrvs)
(puthash "fw[" "［" xmsi-abrvs)
(puthash "fw]" "］" xmsi-abrvs)
(puthash "fw{" "｛" xmsi-abrvs)
(puthash "fw}" "｝" xmsi-abrvs)

(puthash "fw@" "＠" xmsi-abrvs)
(puthash "fw^" "＾" xmsi-abrvs) ; CIRCUMFLEX ACCENT
(puthash "fw`" "｀" xmsi-abrvs) ; GRAVE ACCENT
(puthash "fw~" "～" xmsi-abrvs)
(puthash "fw_" "＿" xmsi-abrvs)
(puthash "fw¯" "￣" xmsi-abrvs) ; MACRON

(puthash "fw#" "＃" xmsi-abrvs)
(puthash "fw+" "＋" xmsi-abrvs)
(puthash "fw-" "－" xmsi-abrvs)
(puthash "fw*" "＊" xmsi-abrvs)
(puthash "fw=" "＝" xmsi-abrvs)
(puthash "fw<" "＜" xmsi-abrvs)
(puthash "fw>" "＞" xmsi-abrvs)
(puthash "fw%" "％" xmsi-abrvs)

(puthash "fw|" "｜" xmsi-abrvs)
(puthash "fw¦" "￤" xmsi-abrvs)
(puthash "fw/" "／" xmsi-abrvs)
(puthash "fw\\" "＼" xmsi-abrvs)
(puthash "fw¬" "￢" xmsi-abrvs)

(puthash "fw((" "｟" xmsi-abrvs)
(puthash "fw))" "｠" xmsi-abrvs)

(puthash "fw$" "＄" xmsi-abrvs)
(puthash "fw£" "￡" xmsi-abrvs)
(puthash "fw¢" "￠" xmsi-abrvs)
(puthash "fw₩" "￦" xmsi-abrvs) ; WON SIGN
(puthash "fw¥" "￥" xmsi-abrvs) ; YEN SIGN

(puthash "fw0" "０" xmsi-abrvs)
(puthash "fw1" "１" xmsi-abrvs)
(puthash "fw2" "２" xmsi-abrvs)
(puthash "fw3" "３" xmsi-abrvs)
(puthash "fw4" "４" xmsi-abrvs)
(puthash "fw5" "５" xmsi-abrvs)
(puthash "fw6" "６" xmsi-abrvs)
(puthash "fw7" "７" xmsi-abrvs)
(puthash "fw8" "８" xmsi-abrvs)
(puthash "fw9" "９" xmsi-abrvs)

(puthash "fwA" "Ａ" xmsi-abrvs)
(puthash "fwB" "Ｂ" xmsi-abrvs)
(puthash "fwC" "Ｃ" xmsi-abrvs)
(puthash "fwD" "Ｄ" xmsi-abrvs)
(puthash "fwE" "Ｅ" xmsi-abrvs)
(puthash "fwF" "Ｆ" xmsi-abrvs)
(puthash "fwG" "Ｇ" xmsi-abrvs)
(puthash "fwH" "Ｈ" xmsi-abrvs)
(puthash "fwI" "Ｉ" xmsi-abrvs)
(puthash "fwJ" "Ｊ" xmsi-abrvs)
(puthash "fwK" "Ｋ" xmsi-abrvs)
(puthash "fwL" "Ｌ" xmsi-abrvs)
(puthash "fwM" "Ｍ" xmsi-abrvs)
(puthash "fwN" "Ｎ" xmsi-abrvs)
(puthash "fwO" "Ｏ" xmsi-abrvs)
(puthash "fwP" "Ｐ" xmsi-abrvs)
(puthash "fwQ" "Ｑ" xmsi-abrvs)
(puthash "fwR" "Ｒ" xmsi-abrvs)
(puthash "fwS" "Ｓ" xmsi-abrvs)
(puthash "fwT" "Ｔ" xmsi-abrvs)
(puthash "fwU" "Ｕ" xmsi-abrvs)
(puthash "fwV" "Ｖ" xmsi-abrvs)
(puthash "fwW" "Ｗ" xmsi-abrvs)
(puthash "fwX" "Ｘ" xmsi-abrvs)
(puthash "fwY" "Ｙ" xmsi-abrvs)
(puthash "fwZ" "Ｚ" xmsi-abrvs)
(puthash "fwa" "ａ" xmsi-abrvs)
(puthash "fwb" "ｂ" xmsi-abrvs)
(puthash "fwc" "ｃ" xmsi-abrvs)
(puthash "fwd" "ｄ" xmsi-abrvs)
(puthash "fwe" "ｅ" xmsi-abrvs)
(puthash "fwf" "ｆ" xmsi-abrvs)
(puthash "fwg" "ｇ" xmsi-abrvs)
(puthash "fwh" "ｈ" xmsi-abrvs)
(puthash "fwi" "ｉ" xmsi-abrvs)
(puthash "fwj" "ｊ" xmsi-abrvs)
(puthash "fwk" "ｋ" xmsi-abrvs)
(puthash "fwl" "ｌ" xmsi-abrvs)
(puthash "fwm" "ｍ" xmsi-abrvs)
(puthash "fwn" "ｎ" xmsi-abrvs)
(puthash "fwo" "ｏ" xmsi-abrvs)
(puthash "fwp" "ｐ" xmsi-abrvs)
(puthash "fwq" "ｑ" xmsi-abrvs)
(puthash "fwr" "ｒ" xmsi-abrvs)
(puthash "fws" "ｓ" xmsi-abrvs)
(puthash "fwt" "ｔ" xmsi-abrvs)
(puthash "fwu" "ｕ" xmsi-abrvs)
(puthash "fwv" "ｖ" xmsi-abrvs)
(puthash "fww" "ｗ" xmsi-abrvs)
(puthash "fwx" "ｘ" xmsi-abrvs)
(puthash "fwy" "ｙ" xmsi-abrvs)
(puthash "fwz" "ｚ" xmsi-abrvs)

  ;; 2010-12-10. char to add
  ;; soft hyphen ­
  ;; ↥ ↧ ⇤ ⇥ ⤒ ⤓ ↨

  )

(defun xmsi-hash-to-list (hashtable)
  "Return a list that represent the HASHTABLE."
  (let (mylist)
    (maphash (lambda (kk vv) (setq mylist (cons (list vv kk) mylist))) hashtable)
    mylist
    )
  )

(defun xmsi-list-math-symbols ()
  "Print a list of math symbols and their input abbreviations.
See `xmsi-mode'."
  (interactive)

  (let (mylist mylistSorted)
    ;; get the hash table into a list
    (setq mylist (xmsi-hash-to-list xmsi-abrvs))

    ;; sort and print it out
    (setq mylistSorted (sort mylist (lambda (a b) (string< (car a) (car b)))) )

    (with-output-to-temp-buffer "*xmsi math symbol input*"

      (mapc (lambda (tt) "" (interactive)
              (princ (concat (car tt) " " (car (cdr tt)) "\n")) )
            mylistSorted) ) ) )

(defvar xmsi-keymap nil "Keymap for xmsi-math-symbols-input mode.")

(progn
  (setq xmsi-keymap (make-sparse-keymap))

  (define-key xmsi-keymap (kbd "S-SPC") 'xmsi-change-to-symbol)
  )

(defun xmsi-change-to-symbol ()
  "DOCSTRING"
  (interactive)

  (let (p1 p2 myWord resultSymbol)
    (if (and transient-mark-mode mark-active)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (save-excursion
        (progn

          (setq p2 (point) )
          (if (re-search-backward "\t\\|\n\\| " nil t)
              (progn (forward-char)
                     (setq p1 (point) ) )
            (setq p1 (line-beginning-position) )
            )

          ;; (if (re-search-forward "\t\\|\n\\| " nil t)
          ;;     (progn (backward-char)
          ;;            (setq p2 (point) ))
          ;;   (setq p2 (line-end-position) ) )

 )) )

    (setq myWord (buffer-substring-no-properties p1 p2) )

    ;; If string is XML syntax such as 「&#945;」 or 「&#x3b1」
    (if (string-match "&#[0-9]+" myWord) ; e.g. &#945;
        (let ( (num (substring myWord 2)) )
          (delete-region p1 p2) (ucs-insert (string-to-number num)) )

      (if (string-match "&#x.+" myWord) ; e.g. &#x3b1
          (let ( (hexnum (substring myWord 3)) )
            (delete-region p1 p2) (ucs-insert (string-to-number hexnum 16) ) )
        (progn
          (setq resultSymbol (gethash myWord xmsi-abrvs))
          (if resultSymbol
              (progn
                (delete-region p1 p2)
                (insert resultSymbol)
                )
            (error "Not a valid abbrev. Call “xmsi-list-math-symbols” for a list. Or use XML entity names. Or use &#945; or &#x3b1; to enter α. Ending semicolon is optional." ) ) )
        ) ) ) )

(define-minor-mode xmsi-mode
  "Toggle math symbol input (minor) mode.

A mode for inputting a few hundred math (Unicode) symbols.

Type “inf”, then press 【Shift+Space】, then it becomes “∞”.
Type “a”, then press 【Shift+Space】, then it becomes “α”.
Type “p”, then press 【Shift+Space】, then it becomes “π”.
Type “!=”, then press 【Shift+Space】, then it becomes “≠”.
Type “>=”, then press 【Shift+Space】, then it becomes “≥”.
Type “=>”, then press 【Shift+Space】, then it becomes “⇒”.
Type “->”, then press 【Shift+Space】, then it becomes “→”.
Type “and”, then press 【Shift+Space】, then it becomes “∧”.
etc.

If you have a text selection, then selected word will be taken as
input. For example, type 「sin(a)」, select the “a”, then press
 【Shift+Space】, then it becomse 「sin(α)」.

For full list, call `xmsi-list-math-symbols'.
All XML char entity abbrevs are supported. For example, 「copy」 becomes 「©」.
To type a unicode by decimal, e.g.  「&#945;」 becomes 「α」.
To type a unicode by hexadecimal, e.g., 「&#x3b1;」 becomes 「α」.
Ending semicolon “;” is optional.

To type any unicode by the char's unicode full name, type
 【Ctrl+x 8 Enter】 (ucs-insert). Asterisk “*” can be used as a
wildcard to find the char. For example, calling “ucs-insert”,
then type 「*arrow」 then Tab, then emacs will list all unicode
char names that has “arrow” in it. (this feature is part of Emacs
23)

Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.

For full documentation, see:
URL `http://xahlee.org/emacs/xmsi-math-symbols-input.html'"
  nil
  :global t
  :lighter " ∑"
  :keymap xmsi-keymap
  )

(provide 'xmsi-math-symbols-input)
