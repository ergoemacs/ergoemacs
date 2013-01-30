;;; ergoemacs-layouts.el --- Keyboard Layouts for use in ergoemacs -*- coding: utf-8 -*-
;;; Code:
(defvar ergoemacs-layout-sw
  '("" "½" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "+" "’" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "å" "\"" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ö" "ä" "'" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "§" "!" "@" "#" "¤" "%" "&" "/" "(" ")" "=" "?" "`" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "Å" "^" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ö" "Ä" "*" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Swedish layout")

(defvar ergoemacs-layout-da
  '("" "½" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "+" "’" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "á" "\"" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "æ" "ø" "'" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "§" "!" "@" "#" "¤" "%" "&" "/" "(" ")" "=" "?" "`" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "Á" "^" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Æ" "Ø" "*" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Danish layout")

(defvar ergoemacs-layout-pt-nativo
  '("" "+" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "º" "<" ""
    "" ""  "'" "," "." "h" "x" "w" "l" "t" "c" "p" "~" "-" ""
    "" ""  "i" "e" "a" "o" "u" "m" "d" "s" "r" "n" "'" "|" ""
    "" "«"  "y" "ç" "j" "b" "k" "q" "v" "g" "f" "z" "" "" ""
    ;; Shifted
    "" "*" "!" "\"" "#" "$" "%" "&" "/" "(" ")" "=" "ª" ">" ""
    "" ""  "?" ";" ":" "H" "X" "W" "L" "T" "C" "P" "^" "_" ""
    "" ""  "I" "E" "A" "O" "U" "M" "D" "S" "R" "N" "`" "\\" ""
    "" "»"  "Y" "Ç" "J" "B" "K" "Q" "V" "G" "F" "Z" "" "" "")
  "PT Nativo layout URL `http://xahlee.info/kbd/pt-nativo_keyboard_layout.html'")

(defvar ergoemacs-layout-us
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
  "US English QWERTY layout")

(defvar ergoemacs-layout-dv
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "[" "]" ""
    "" ""  "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" ""  ""
    "" ""  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" ""  ""  ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "{" "}"  ""
    "" ""  "\"" "," "." "P" "Y" "F" "G" "C" "R" "L" "?" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Dvorak layout. URL `http://en.wikipedia.org/wiki/Dvorak_Simplified_Keyboard'")

(defvaralias 'ergoemacs-layout-us_dvorak 'ergoemacs-layout-dv)

(defvar ergoemacs-layout-programmer-dv
  '("" "$" "&" "[" "{" "}" "(" "=" "*" ")" "+" "]" "!" "#" ""
    "" ""  "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "" ""
    "" ""  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" "" "" ""
    ;; Shifted
    "" "" "%" "7" "5" "3" "1" "9" "0" "2" "4" "6" "8" "`"  ""
    "" ""  "\"" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "?" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Programmer Dvorak layout")

(defvar ergoemacs-layout-gb-dv
  '("" "`" "[" "7" "5" "3" "1" "9" "0" "2" "4" "6" "8" "]"  ""
    "" ""  "/" "," "." "p" "y" "f" "g" "c" "r" "l" "'" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "#" ""
    "" "\\"  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" "" "" ""
    ;; Shifted
    "" "¬" "{" "&" "%" "£" "!" "(" ")" "\"" "$" "^" "*" "}" ""
    "" ""  "?" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "@" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "~" ""
    "" "|"  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "UK Dvorak layout")

(defvar ergoemacs-layout-colemak
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "f" "p" "g" "j" "l" "u" "y" ";" "[" "]" "\\"
    "" ""  "a" "r" "s" "t" "d" "h" "n" "e" "i" "o" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "k" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "F" "P" "G" "J" "L" "U" "Y" ":" "{" "}" "|"
    "" ""  "A" "R" "S" "T" "D" "H" "N" "E" "I" "O" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "K" "M" "<" ">" "?" "" "" "")
  "US Colemak layout URL `http://colemak.com/'")

(defvar ergoemacs-layout-asset
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "j" "f" "g" "y" "p" "u" "l" ";" "[" "]" "\\"
    "" ""  "a" "s" "e" "t" "d" "h" "n" "i" "o" "r" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "k" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "J" "F" "G" "Y" "P" "U" "L" ":" "{" "}" "|"
    "" ""  "A" "S" "E" "T" "D" "H" "N" "I" "O" "R" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "K" "M" "<" ">" "?" "" "" "")
  "US Asset layout. URL `http://millikeys.sourceforge.net/asset/'")

(defvar ergoemacs-layout-workman
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "d" "r" "w" "b" "j" "f" "u" "p" ";" "[" "]" "\\"
    "" ""  "a" "s" "h" "t" "g" "y" "n" "e" "o" "i" "'" "" ""
    "" ""  "z" "x" "m" "c" "v" "k" "l" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "D" "R" "W" "B" "J" "F" "U" "P" ":" "{" "}" "|"
    "" ""  "A" "S" "H" "T" "G" "Y" "N" "E" "O" "I" "\"" "" ""
    "" ""  "Z" "X" "M" "C" "V" "K" "L" "<" ">" "?" "" "" "")
  "US Workman layout. URL `http://www.workmanlayout.com/blog/'")

(defvar ergoemacs-layout-gb
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "#" ""
    "" "\\"  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "¬" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "@" "~" ""
    "" "|"  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
  "UK layout. URL `http://en.wikipedia.org/wiki/Keyboard_layout'")

(defvar ergoemacs-layout-it
  '("" "\\" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "'" "¡" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "è" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ò" "à" "ù" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "|" "!" "\"" "£" "$" "%" "&" "/" "(" ")" "=" "?" "^" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "é" "+" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "ç" "°" "§" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Italian layout. URL `http://en.wikipedia.org/wiki/Keyboard_layout'")

(defvar ergoemacs-layout-sp
  '("" "°" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "'" "¡" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "`" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ñ" "'" "ç" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "ª" "!" "\"" "£" "$" "%" "&" "/" "(" ")" "=" "?" "¿" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "^" "*" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ñ" "\"" "Ç" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Spanish layout. URL `http://en.wikipedia.org/wiki/Keyboard_layout'")

(defvar ergoemacs-layout-fr
  '("" "²" "&" "é" "\"" "'" "(" "-" "è" "_" "ç" "à" ")" "=" ""
    "" ""  "a" "z" "e" "r" "t" "y" "u" "i" "o" "p" "^" "$" ""
    "" ""  "q" "s" "d" "f" "g" "h" "j" "k" "l" "m" "ù" "*" ""
    "" "<"  "w" "x" "c" "v" "b" "n" "," ";" ":" "!" "" "" ""
    ;; Shifted
    "" "³" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "°" "+" ""
    "" ""  "A" "Z" "E" "R" "T" "Y" "U" "I" "O" "P" "" "£" ""
    "" ""  "Q" "S" "D" "F" "G" "H" "J" "K" "L" "M" "%" "μ" ""
    "" ">"  "W" "X" "C" "V" "B" "N" "?" "." "/" "§" "" "" "")
  "French AZERTY layout. URL `http://en.wikipedia.org/wiki/Keyboard_layout'")

;; From workhorse.t,

(defvar ergoemacs-layout-ge
  '("" "" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "ß" "" ""
    "" ""  "q" "w" "e" "r" "t" "z" "u" "i" "o" "p" "ü" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ö" "ä" "#" ""
    "" ""  "y" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "°" "!" "\"" "§" "$" "%" "&" "/" "(" ")" "=" "?" "" ""
    "" ""  "Q" "W" "E" "R" "T" "Z" "U" "I" "O" "P" "Ü" "*" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ö" "Ä" "'" ""
    "" ""  "Y" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "German QWERTZ layout")


;;; Layout Functions
(defun ergoemacs-get-layouts-type ()
  "Gets the customization types for `ergoemacs-keyboard-layout'"
  `(choice ,@(mapcar
              (lambda(elt)
                `(const :tag ,elt :value ,elt))
              (sort (ergoemacs-get-layouts) 'string<))))

(defun ergoemacs-set-layout (layout)
  "Set the ergoemacs layout."
  (ergoemacs-set-default 'ergoemacs-keyboard-layout layout))

(defun ergoemacs-get-layouts-menu ()
  "Gets the easymenu entry for ergoemacs-layouts."
  `("Keyboard Layouts"
    ,@(mapcar
       (lambda(lay)
         (let* ((variable (intern (concat "ergoemacs-layout-" lay)))
                (alias (condition-case nil
                           (indirect-variable variable)
                         (error variable)))
                (is-alias nil)
                (doc nil))
           (setq doc (or (documentation-property variable 'variable-documentation)
                         (progn
                           (setq is-alias t)
                           (documentation-property alias 'variable-documentation))))
           `[,(concat lay " - " doc)
             (lambda() (interactive)
               (ergoemacs-set-layout ,lay)) :style radio :selected (string= ergoemacs-keyboard-layout ,lay)]))
       (ergoemacs-get-layouts))))

(defun ergoemacs-get-layouts-doc ()
  "Gets the list of all known layouts and the documentation associated with the layouts."
  (let ((lays (sort (ergoemacs-get-layouts) 'string<)))
    (mapconcat
     (lambda(lay)
       (let* ((variable (intern (concat "ergoemacs-layout-" lay)))
              (alias (condition-case nil
                         (indirect-variable variable)
                       (error variable)))
              (is-alias nil)
              (doc nil))
         (setq doc (or (documentation-property variable 'variable-documentation)
                       (progn
                         (setq is-alias t)
                         (documentation-property alias 'variable-documentation))))
         (concat "\"" lay "\" (" doc ")" (if is-alias ", alias" ""))))
     lays "\n")))

(defun ergoemacs-get-layouts (&optional ob)
  "Gets the list of all known layouts"
  (let (ret)
    (mapatoms (lambda(s)
                (let ((sn (symbol-name s)))
                  (and (string-match "^ergoemacs-layout-" sn)
                       (setq ret (cons (replace-regexp-in-string "ergoemacs-layout-" "" sn) ret)))))
              ob)
    ret))

(provide 'ergoemacs-layouts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-layouts.el ends here