; Patrick Thomson's .emacs file.
; Here be cargo cult science.

; Switching the newline/newline-and-indent characters.
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

; Enable the list of recent files...
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-c\C-r" 'recentf-open-files)

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

