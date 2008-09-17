; Patrick Thomson's .emacs file.
; Here be cargo cult science.

; Switching the newline/newline-and-indent characters.
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

; Enable the list of recent files...
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-c\C-r" 'recentf-open-files)

; A macro for adding a given directory to the user path.
(defmacro add-to-user-path (item)
  `(add-to-list 'load-path (expand-file-name
                            ,(format "~/emacs"))))

(add-to-user-path "")

; Ensure that abbrev mode is always on.
(setq-default abbrev-mode t)
(setq save-abbrevs)


(setq blink-matching-paren t)
; Making the expansion command more powerful.
(require 'hippie-exp)
(global-set-key [(control /)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))

;; M-g is now the same as M-x goto-line
(global-set-key "\eg" 'goto-line)
;; M-G is now the same as M-x what-line
(global-set-key "\eG" 'what-line)

; Ensuring Unicode compliance (may not be necessary)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; Avoiding the Alt key...
(global-set-key "\C-x\C-m" 'execute-extended-command)

; Highlighting the current region
(transient-mark-mode 1)

; I wonder if syntax highlighting will work?
(global-font-lock-mode 1)

; GAAAH! I WILL KILL TERMINAL.APP!!!
(global-set-key "\C-c\C-e" 'ESC-prefix)

(column-number-mode 1)

; BEGIN ABBREVIATIONS

; C abbreviations

(defun next-char-as-string ()
  (char-to-string (following-char)))

(defun source-escape-string ()
  (interactive)
  (when
      (or (equal (next-char-as-string) "\"")
          (equal (next-char-as-string) "'"))
      (forward-char))
  (insert-string ", "))

(global-set-key "\M-," 'source-escape-string)