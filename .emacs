; Patrick Thomson's .emacs file.

(mapcar 'require '(erlang recentf linum go-mode))

; Switching the newline/newline-and-indent characters.
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

; Enable the list of recent files...
(recentf-mode 1)
(global-set-key "\C-c\C-r" 'recentf-open-files)

;; M-g is now the same as M-x goto-line
(global-set-key "\eg" 'goto-line)
;; M-G is now the same as M-x what-line
(global-set-key "\eG" 'what-line)

; disable linum mode and echoing in shells
(defun my-comint-init ()
  (linum-mode 0)
  (hl-line-mode 0)
  (setq comint-process-echoes t))

(add-hook 'comint-mode-hook 'my-comint-init)

; spawn shell with C-c C-s
(global-set-key "\C-c\C-s" 'shell)

; fullscreen plz
(add-to-list 'default-frame-alist '(fullscreen . fullboth)) 

; Ensuring Unicode compliance (may not be necessary)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; Avoiding the Alt key...
(global-set-key "\C-x\C-m" 'execute-extended-command)

; execute erlang-mode when encountering .erl files
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))

; line numbers and line highlights
(global-linum-mode)
(global-hl-line-mode)
(setq linum-format "%d ")


(setq blink-matching-paren t)

(when window-system (set-frame-size (selected-frame) 105 55))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(global-hl-line-mode t)
 '(inhibit-startup-screen t)
 '(semantic-mode t)
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
