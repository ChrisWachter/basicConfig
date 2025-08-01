;;; .emacs --- Init file for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs (this is to shut up flycheck, btw)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))


(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(expand-region magit paredit python rainbow-mode rainbow-delimiters
    cyberpunk-theme undo-tree zenburn-theme smartparens company function-args
    company-c-headers clean-aindent-mode ws-butler move-text flycheck avy realgud
    goto-last-change
   )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (let ((all-here t))
    (dolist (pkg my-packages)
      (when (not (package-installed-p pkg)) (setf all-here nil)))
  all-here))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (pkg my-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;

(fset 'yes-or-no-p 'y-or-n-p)

(require 'delsel)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

(delete-selection-mode t)
(setq blink-cursor-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)
(line-number-mode 1)
(setq subword-mode t)
(setq inhibit-startup-message t)
(put 'narrow-to-region 'disabled nil)

(global-hl-line-mode +1)

(avy-setup-default)
(global-set-key (kbd "M-C-:") 'avy-goto-line)
(global-set-key (kbd "C-:") 'avy-goto-word-1)
(global-set-key (kbd "M-:") 'avy-goto-char)

(unless (fboundp 'cua-replace-region)
  (defun cua-replace-region ()
    "Replace the active region with the character you type."
    (interactive)
    (let ((not-empty (and cua-delete-selection (cua-delete-region))))
      (unless (eq this-original-command this-command)
        (let ((overwrite-mode
               (and overwrite-mode
                    not-empty
                    (not (eq this-original-command 'self-insert-command)))))
          (cua--fallback))))))


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq scroll-step 1)
(setq  scroll-conservatively 10000)
(setq  auto-window-vscroll nil)
;; (setq  scroll-margin 0)
;; (setq  next-line-add-newlines nil)
;; (setq  scroll-preserve-screen-position 1)

;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar, (hbar. HEIGHT) 
(setq read-only-color       "gray")
(setq read-only-cursor-type 'hbar)
(setq overwrite-color       "red")
(setq overwrite-cursor-type 'box)
(setq normal-color          "green")
(setq normal-cursor-type    'bar)

(defun set-cursor-according-to-mode ()
  (cond
    (buffer-read-only
      (set-cursor-color read-only-color)
      (setq cursor-type read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color overwrite-color)
      (setq cursor-type overwrite-cursor-type))
    (t 
      (set-cursor-color normal-color)
      (setq cursor-type normal-cursor-type))))

(add-hook 'post-command-hook 'set-cursor-according-to-mode)

(menu-bar-mode -1) 
(tool-bar-mode -1)
;;(toggle-scroll-bar 1)
(scroll-bar-mode -1)  ;; Just gross on WSL2
;;(setq scroll-bar-mode 'right) 
;;(menu-bar-mode -99)

(setq blink-matching-delay 0.025)

(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t) 

(setq transient-mark-mode 1)
(setq set-mark-command-repeat-pop 1)
(setq mark-ring-max 4)

(setq column-number-mode 1)

(setq visible-bell t)
(setq font-lock-maximum-decoration t)

(setq-default case-fold-search nil)

(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default tab-stop-list (number-sequence 4 200 4))

;; (require 'clean-aindent-mode)
;; (add-hook 'prog-mode-hook 'clean-aindent-mode)
;; (require 'ws-butler)
;; (add-hook 'c-mode-common-hook 'ws-butler-mode)

(defun how-many-region (begin end regexp &optional interactive)
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(setq make-backup-files nil)
(setq set-mark-command-repeat-pop t)

(setq global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
    jit-lock-defer-contextually t
    jit-lock-stealth-nice 0)

(global-subword-mode 1)

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-code-rigidly (region-beginning)
                               (region-end)
                               distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-code-rigidly (line-beginning-position)
                         (line-end-position)
                         distance)))

(defun shift-right ()
  (interactive)
  (shift-text c-basic-offset))

(defun shift-left ()
  (interactive)
  (shift-text (- c-basic-offset)))

(global-set-key [backtab] 'shift-left)
(global-set-key [tab] 'shift-right)

(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<M-return>") 'open-line-below)
(global-set-key (kbd "<M-S-return>") 'open-line-above)

(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))


(require 'goto-last-change)
(global-set-key (kbd "C-M-/") 'goto-last-change)

;;;;

(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; This is better than orderless, I think
(use-package prescient
  :ensure t
  :custom ())
(setq completion-preview-sort-function #'prescient-completion-sort)
(use-package vertico-prescient
  :ensure t
  :custom ()
  :init
  (vertico-prescient-mode))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;;;;;;

(use-package just-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

;;

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

;;;;

(use-package sqlite-mode 
  :config
  (defun ct/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name)))

  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . ct/sqlite-view-file-magically)))

(use-package sqlite-mode-extras
  :ensure t
  :hook ((sqlite-mode . sqlite-extras-minor-mode))
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("DEL" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(use-package ledger-mode
  :ensure t
  :custom
  ((ledger-binary-path "hledger")
   (ledger-mode-should-check-version nil)
   (ledger-report-auto-width nil)
   (ledger-report-links-in-register nil)
   (ledger-report-native-highlighting-arguments '("--color=always")))
  :mode ("\\.hledger\\'" "\\.ledger\\'" "\\.journal\\'")
  :init
    (setq ledger-binary-path "hledger.sh")
    (setq ledger-default-date-string "%Y-%m-%d"))

(use-package flycheck-ledger
  :ensure t
  :after ledger-mode flycheck
  ;; :demand t
  :custom
  (flycheck-hledger-strict t) 
  (flycheck-hledger-checks '("ordereddates" "recentassertions"))  ; extra checks from https://hledger.org/hledger.html#check: ordereddates, uniqueleafnames, payees, recentassertions, tags..
  (flycheck-hledger-executable "hledger"))


;;;;

(require 'cc-mode)
(setq c-basic-offset 4)
(setq-default c-electric-flag nil)

(require 'python)

(add-to-list 'c-mode-common-hook
  (lambda () (setq c-syntactic-indentation nil)))
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(add-hook 'c-mode-common-hook (lambda () (infer-indentation-style))) 


(add-hook 'python-mode-hook
  '(lambda ()
    (define-key python-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'python-mode-hook
  '(lambda ()
    (define-key python-mode-map (kbd "<backtab>") 'shift-left)))

(add-hook 'python-mode-hook (lambda () (infer-indentation-style))) 

(add-hook 'yaml-mode-hook (lambda () (infer-indentation-style))) 

(add-hook 'clojure-mode-hook
  '(lambda ()
    (define-key clojure-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'clojure-mode-hook
  '(lambda ()
    (define-key clojure-mode-map (kbd "<backtab>") 'shift-left)))

(add-hook 'clojure-mode-hook (lambda () (infer-indentation-style)))


;;;;

(global-prettify-symbols-mode 1) 

(load-theme 'zenburn t)
(setq my-cur-theme 'zenburn)

(use-package auto-dim-other-buffers
  :ensure t)

(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

(defun toggle-theme ()
 (interactive)
 (if buffer-read-only
     (progn
      (load-theme 'cyberpunk t)
      (setq my-cur-theme 'cyberpunk))
  (progn
   (load-theme 'zenburn t)
   (setq my-cur-theme 'zenburn))))

(defun toggle-theme-on-readonly-switch ()
  (interactive)
  (progn
    (disable-theme 'my-cur-theme)
    (if (equal buffer-read-only nil)
      (setq buffer-read-only t)
      (setq buffer-read-only nil))
    (toggle-theme)))
(global-set-key (kbd "C-x C-q") 'toggle-theme-on-readonly-switch)

(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 140))

(defun my-basic-faces-setup-hook ()
  (face-remap-add-relative 'variable-pitch :family "Hack Nerd Font" :height 110 :weight 'semi-bold)
  (face-remap-add-relative 'font-lock-comment-face :inherit 'variable-pitch :weight 'normal :slant 'italic)
  (face-remap-add-relative 'default :family "Hack Nerd Font Mono" :height 110 :weight 'medium)
  (face-remap-add-relative 'mode-line :family "Hack Nerd Font" :height 90 :weight 'semi-bold)
  (face-remap-add-relative 'mode-line-inactive :family "Hack Nerd Font" :height 90 :weight 'extra-light :slant 'italic))

(add-hook 'font-lock-mode-hook 'my-basic-faces-setup-hook)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-help-echo-function nil)
(setq flycheck-indication-mode 'right-fringe)

(custom-set-faces
 '(flycheck-info ((((class color)) (:underline (:color "#33AAAA" :style line)))))
 '(flycheck-error ((((class color)) (:underline (:color "#CC0000" :style wave)))))
 '(flycheck-warning ((((class color)) (:underline (:color "#FFFF00" :style wave)))))
 '(flycheck-fringe-info ((((class color)) (:background "#338888" :foreground "#000000"))))
 '(flycheck-fringe-error ((((class color)) (:background "#990000" :foreground "#CCCCCC"))))
 '(flycheck-fringe-warning ((((class color)) (:background "#CCCC00" :foreground "#000000")))))

(custom-set-faces
 '(my-space-face ((((class color)) (:foreground "#555555"))) t)
 '(my-tab-face ((((class color)) (:foreground "#308030"))) t))

(add-hook 'font-lock-mode-hook
 (function
  (lambda ()
   (setq font-lock-keywords
    (append font-lock-keywords
     '(
       ("\t" (0 'my-tab-face t))
       (" " (0 'my-space-face t))
       ("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):" (0 font-lock-warning-face t))
      ))))))

(global-whitespace-mode t)
(setq whitespace-style '(face empty spaces space-mark tabs tab-mark newline newline-mark trailing))
(setq whitespace-display-mappings ;; http://ergoemacs.org/emacs/whitespace-mode.html
 '(
   (space-mark 32 [183] [46])
   (newline-mark 10 [9166 10])
   (tab-mark 9 [9655 9] [92 9])
   ))

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-autoescape-string-quote nil)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
(add-hook 'sh-mode-hook 'rainbow-delimiters-mode)

(defun my-rainbow-delimiters-setup-hook ()
  (face-remap-add-relative 'rainbow-delimiters-unmatched-face :inherit 'error :weight 'bold :box "#00FF00" :foreground '"red" :strike-through t))
(add-hook 'font-lock-mode-hook 'my-rainbow-delimiters-setup-hook)

(defvar my-paren-dual-colors
  '("#f25e40" "#a7d52a"))
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (set-face-foreground
  (intern (format "rainbow-delimiters-depth-%d-face" index))
  (elt my-paren-dual-colors
       (if (cl-evenp index) 0 1))))

(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gud-gdb-command-name "gdb")
(setq gud-gdb-command-name "gdb -interpreter=mi")
(setq gdb-many-windows t)

(require 'company)
(add-to-list 'company-backends 'company-c-headers)
(setq company-c-headers-path-system '("/usr/include/c++/4.9/"))
(setq company-begin-commands '(self-insert-command))
(setq company-idle-delay 1.5)
(setq company-minimum-prefix-length 4)
(setq company-tooltip-limit 8)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

(setq dabbrev-case-replace nil)

(require 'magit)
(global-set-key (kbd "<f9>") 'magit-status)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(global-set-key (kbd "C-S-<left>")  'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)
(global-set-key (kbd "C-S-<up>")    'windmove-up)
(global-set-key (kbd "C-S-<down>")  'windmove-down)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setf dired-kill-when-opening-new-dired-buffer t)

(require 'org)

(global-set-key (kbd "C-x r C-y") `yank-rectangle)
(global-set-key (kbd "C-x r C-w") `kill-rectangle)
(global-set-key (kbd "C-x r M-w") `copy-rectangle-as-kill)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "H-1") `eval-and-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-default-style "stroustrup")
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(setq
  gdb-many-windows t
  gdb-show-main t)
(add-to-list 'company-backends 'company-c-headers)

(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(control tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

(defun xah-copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(defun goto-line-and-recenter ()
  (interactive)
  (unwind-protect
      (progn
        (global-display-line-numbers-mode 1)
        (goto-line (read-number "Recenter on which line? "))
        (recenter))
    (global-display-line-numbers-mode -1)))
(global-set-key (kbd "M-g") 'goto-line-and-recenter)

;;;;;;;;

(global-set-key (kbd "M-<up>") 'scroll-down-command)
(global-set-key (kbd "M-<down>") 'scroll-up-command)

(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-DEL") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)

(global-set-key (kbd "M-/") 'hippie-expand)

(define-key isearch-mode-map [next] 'isearch-repeat-forward)
(define-key isearch-mode-map [prior] 'isearch-repeat-backward)

(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "M-;") 'uncomment-region)

(defun jump-to-mark ()
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") `toggle-window-split)

(global-set-key (kbd "H-<right>") 'sp-forward-sexp)
(global-set-key (kbd "H-<left>") 'sp-backward-sexp)

(global-set-key (kbd "C-x b") 'ibuffer)    ;; yup
(global-set-key (kbd "C-x C-b") 'ibuffer)  ;; I am dumb
(global-set-key (kbd "C-x B") 'switch-to-buffer)

(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;


