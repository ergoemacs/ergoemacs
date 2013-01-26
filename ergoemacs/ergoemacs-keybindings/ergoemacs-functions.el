;;; ergoemacs-functions.el --- Functions for use in ergoemacs -*- coding: utf-8 -*-
;;; Code:


(require 'redo "redo.elc" t) ; for redo shortcut

(delete-selection-mode 1) ; turn on text selection highlighting and make typing override selected text (Note: when delete-selection-mode is on, then transient-mark-mode is automatically on too.)

(defcustom ergoemacs-isearch-backward-char-to-edit nil
  "Backward char will edit isearch."
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs-movement-functions
  '(scroll-down move-beginning-of-line move-end-of-line scroll-up scroll-down forward-block backward-block forward-word backward-word next-line previous-line forward-char backward-char)
  "Defines movement functions that ergoemacs is aware of.")

(defvar ergoemacs-delete-functions
  '(delete-backward-char delete-char kill-word backward-kill-word)
  "Defines deletion functions that ergoemacs is aware of.")

;; Shifted movement command fixes (without advising cua-mode)
(defmacro ergoemacs-create-movement-commands (command)
  "Creates a shifted and isearch command command."
  `(progn
     ,(if (eq 'backward-char command)
          `(defun ,(intern (concat "ergoemacs-isearch-" (symbol-name command))) (&optional arg)
             ,(format "Ergoemacs isearch movement command for `%s'.  Behviour controlled with `ergoemacs-isearch-backward-char-to-edit'.  A prefix command will temporarily toggle if the keyboard will edit the item." (symbol-name command))
             (interactive "^P")
             (if (or (and arg (not ergoemacs-isearch-backward-char-to-edit))
                     (and (not arg) ergoemacs-isearch-backward-char-to-edit))
                 (isearch-edit-string)
               (isearch-exit)
               (call-interactively ',command t)
               (setq this-command ',command)))
        `(defun ,(intern (concat "ergoemacs-isearch-" (symbol-name command))) (&optional arg)
           ,(format "Ergoemacs isearch movement command for `%s'." (symbol-name command))
           (interactive "^P")
           (isearch-exit)
           (call-interactively ',command t)
           (setq this-command ',command)))
     (defun ,(intern (concat "ergoemacs-" (symbol-name command))) (&optional arg)
       ,(format "Ergoemacs shifted movement command for `%s'." (symbol-name command))
       (interactive)
       (let ((active (mark)))
         (call-interactively ',command t)
         (setq this-command ',command)
         (unless active
           (deactivate-mark))))))


(mapc
 (lambda(x)
   (eval `(ergoemacs-create-movement-commands ,x)))
 ergoemacs-movement-functions)


;;; Ido-ergoemacs functional fixes
(defun ergoemacs-ido-c-o (arg)
  "Ergoemacs ido C-o command"
  (interactive "P")
  (cond
   ((memq ido-cur-item '(file dir))
    (ido-fallback-command))
   (t
    (minibuffer-keyboard-quit)
    (ido-find-file))))

(defun ergoemacs-ido-prev-match-dir ()
  "Call the correct function based on if we are completing directories or not"
  (interactive)
  (if (and (boundp 'item) item (eq item 'file))
      (ido-prev-match-dir)
    (previous-history-element)))

(defun ergoemacs-ido-next-match-dir ()
  "Call the correct function based on if we are completing directories or not."
  (interactive)
  (if (and (boundp 'item) item (eq item 'file))
      (ido-next-match-dir)
    (next-history-element)))

(defun ergoemacs-print-buffer-confirm ()
  "Print current buffer, but ask for confirmation first."
  (interactive)
  (when
      (y-or-n-p "Print current buffer?")
    (print-buffer)))

(defun ergoemacs-call-keyword-completion ()
  "Call the command that has keyboard shortcut M-TAB."
  (interactive)
  (call-interactively (key-binding (kbd "M-TAB"))))

(defun ergoemacs-describe-major-mode ()
  "Show inline doc for current major-mode."
  ;; code by Kevin Rodgers. 2009-02-25
  (interactive)
  (describe-function major-mode))

(defun ergoemacs-copy-all ()
  "Put the whole buffer content into the kill-ring.
If narrow-to-region is in effect, then copy that region only."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied copy-region-as-kill"))

(defun ergoemacs-cut-all ()
  "Cut the whole buffer content into the kill-ring.
If narrow-to-region is in effect, then cut that region only."
  (interactive)
  (kill-region (point-min) (point-max))
  (message "Buffer content cut"))

(defun ergoemacs-copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(defun ergoemacs-cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

;;; CURSOR MOVEMENT

(defun ergoemacs-forward-open-bracket ()
  "Move cursor to the next occurrence of left bracket or quotation mark."
  (interactive)
  (forward-char 1)
  (search-forward-regexp (regexp-opt '("\"" "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«")) nil t)
;;  (search-forward-regexp "\\s(\\|\\s\"\\|<\\|“\\|‘\\|‹") ; using syntax table
  (backward-char 1))

(defun ergoemacs-backward-open-bracket ()
  "Move cursor to the previous occurrence of left bracket or quotation mark.."
  (interactive)
  (search-backward-regexp (regexp-opt '("\"" "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«")) nil t))

(defun ergoemacs-forward-close-bracket ()
  "Move cursor to the next occurrence of right bracket or quotation mark."
  (interactive)
   (search-forward-regexp (regexp-opt '("\"" ")" "\\]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")) nil t)
;;  (search-forward-regexp "\\s)\\|\\s\"\\|>\\|”\\|’\\|›") ;using syntax table
 )

(defun ergoemacs-backward-close-bracket ()
  "Move cursor to the previous occurrence of right bracket or quotation mark."
  (interactive)
  (backward-char 1)
  (search-backward-regexp (regexp-opt '("\"" ")" "\\]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")) nil t)
  (forward-char 1))

(defun ergoemacs-forward-block ()
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive)
                                        ;  (skip-chars-forward "\n\t ")
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR")
      (progn (backward-char))
    (progn (goto-char (point-max)) )
    )
  )

(defun ergoemacs-backward-block ()
  "Move cursor backward to previous text block.
See: `ergoemacs-forward-block'"
  (interactive)
                                        ;  (skip-chars-backward "\n\t ")
  (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
      (progn
        ;; (goto-char (match-beginning 0))
        (skip-chars-backward "\n\t ")
        (forward-char 1)
        )
    (progn (goto-char (point-min)) )
    )
  )

;;; TEXT SELECTION RELATED

(defun ergoemacs-select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun ergoemacs-select-current-block ()
  "Select the current block of next between empty lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point) ) )
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point) ))
        (setq p2 (point) ) ) )
    (set-mark p1) ) )

(defun ergoemacs-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters:
 () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄
 For practical purposes, also: \"\", but not single quotes."
 (interactive)
 (let (p1)
   (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘⦅〚⦃\"")
   (setq p1 (point))
   (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙⦆〛⦄\"")
   (set-mark p1)))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun ergoemacs-semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq arg (1- arg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq arg (1+ arg) )))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun ergoemacs-extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (ergoemacs-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;;; TEXT TRANSFORMATION RELATED

(defun ergoemacs-kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending."
  (interactive)
  (if (looking-back "\n")
      (delete-char -1)
    (kill-line 0)))

(defun ergoemacs-move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))

(defun ergoemacs-move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))

(defun ergoemacs-unfill-paragraph ()
  "Replace newline char in current paragraph by space.
This command does the reverse of `fill-paragraph'.
See also: `compact-uncompact-block'"
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun ergoemacs-unfill-region (start end)
  "Replace newline char in region by space.
This command does the reverse of `fill-region'.
See also: `ergoemacs-compact-uncompact-block'"
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(defun ergoemacs-compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  
  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))
    
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )
      
      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )
      
      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defun ergoemacs-shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line does have visible chars, then shrink whitespace surrounding cursor to just one space.
If current line does not have visible chars, then shrink al neighboring blank lines to just one.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let (cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos)
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))
      
      (setq spaceTabNeighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)) t nil) )
      (goto-char cursor-point)
      
      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))
      
      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))
      
      (goto-char cursor-point)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point)))
    
    (if line-has-meat-p
        (let (deleted-text)
          (when spaceTabNeighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " "))))
      
      (progn
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n")
        (delete-blank-lines))
      ;; todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )))

(defun ergoemacs-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )
    
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )
    
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )) )

;;; FRAME

(defun ergoemacs-switch-to-next-frame ()
  "Select the next frame on current display, and raise it."
  (interactive)
  (other-frame 1))

(defun ergoemacs-switch-to-previous-frame ()
  "Select the previous frame on current display, and raise it."
  (interactive)
  (other-frame -1))

;;; BUFFER RELATED

(defun ergoemacs-next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun ergoemacs-previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun ergoemacs-next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer) )))

(defun ergoemacs-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun ergoemacs-new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.

(defun ergoemacs-open-in-external-app ()
  "Open the current file or dired marked files in external app.
Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))) ) ) )
    
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?") ) )
    
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
        )
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

(defun ergoemacs-open-in-desktop ()
  "Open the current file in desktop.
Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux") (shell-command "xdg-open ."))) )

(defvar ergoemacs-recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable recently-closed-buffers-max.")
(defvar ergoemacs-recently-closed-buffers-max 30 "The maximum length for recently-closed-buffers.")

(defun ergoemacs-close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• make sure the buffer shown after closing is a user buffer.
• if the buffer is a file, add the path to the list `recently-closed-buffers'.

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (emacsBuff-p isEmacsBufferAfter)
    
    (setq emacsBuff-p (if (string-match "^*" (buffer-name)) t nil) )
    
    (if (string= major-mode "minibuffer-inactive-mode")
        nil ; if minibuffer, do nothing
      (progn 
        ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not emacsBuff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil) 
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save?" (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        
        ;; save to a list of closed buffer
        (when (not (equal buffer-file-name nil))
          (setq ergoemacs-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) ergoemacs-recently-closed-buffers))
          (when (> (length ergoemacs-recently-closed-buffers) ergoemacs-recently-closed-buffers-max)
            (setq ergoemacs-recently-closed-buffers (butlast ergoemacs-recently-closed-buffers 1))))
        
        ;; close
        (kill-buffer (current-buffer))
        
        ;; if emacs buffer, switch to a user buffer
        (if (string-match "^*" (buffer-name))
            (setq isEmacsBufferAfter t)
          (setq isEmacsBufferAfter nil))
        (when isEmacsBufferAfter
          (ergoemacs-next-user-buffer) ) ))))

(defun ergoemacs-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop ergoemacs-recently-closed-buffers)) ) )

;;; Text scaling functions
(defun ergoemacs-text-scale-normal-size ()
  "Set the height of the default face in the current buffer to its default value."
  (interactive)
  (text-scale-increase 0))

;;; Ergoprog functions
(defun ergoemacs-is-text-mode ()
  (or (eq major-mode 'text-mode)
      (eq major-mode 'markdown-mode)))

(defun ergoemacs-beginning-of-block ()
  (interactive)
  (if (ergoemacs-is-text-mode)
      (backward-paragraph)
    (beginning-of-defun)))

(defun ergoemacs-end-of-block ()
  (interactive)
  (if (ergoemacs-is-text-mode)
      (forward-paragraph)
    (end-of-defun)))

(defun ergoemacs-switch-macro-recording ()
  (interactive)
  (if (not defining-kbd-macro)
      (kmacro-start-macro 0)
    (kmacro-end-macro 1)))

;; ==================================================
;; Camel Case
;; ==================================================

;; These functions were taken from:
;; http://www.emacswiki.org/emacs/CamelCase

(defun ergoemacs-un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".
    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun ergoemacs-mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun ergoemacs-camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))
(defun ergoemacs-camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (ergoemacs-mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

;; This is my camel-case switcher

(defun ergoemacs-toggle-camel-case ()
  (interactive)
  (let* ((bounds (progn (if (= (cdr (bounds-of-thing-at-point 'word))
                               (car (bounds-of-thing-at-point 'sexp)))
                            (backward-char))
                        (bounds-of-thing-at-point 'sexp)))
         (beg (car bounds))
         (end (cdr bounds))
         (rgn (filter-buffer-substring beg end))
         (case-fold-search nil))
    (delete-region beg end)
    (cond
     ((string-match "_" rgn)
      (insert (camelize-method rgn)))
     ((string-match "^[a-z]" rgn)
      (progn (insert (capitalize (substring rgn 0 1)))
             (insert (substring rgn 1))))
     (t
      (insert (un-camelcase-string rgn "_"))))))

;; ==================================================
;; PHP facilities
;; ==================================================

(defun ergoemacs-open-and-close-php-tag ()
  (interactive)
  (insert "<?php  ?>")
  (backward-char 3))

(defun ergoemacs-open-and-close-php-tag-with-echo ()
  (interactive)
  (insert "<?php echo ; ?>")
  (backward-char 4))

(provide 'ergoemacs-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-functions.el ends here
