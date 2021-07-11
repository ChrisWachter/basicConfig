(require 'package)
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(auctex clojure-mode clojure-mode-extra-font-locking
    deft expand-region gist haml-mode haskell-mode helm
    magit paredit
    python sass-mode rainbow-mode rainbow-delimiters scss-mode
    solarized-theme cyberpunk-theme multiple-cursors undo-tree
    yaml-mode zenburn-theme fill-column-indicator
    smartparens dash dash-functional cider
    helm-gtags helm-swoop sr-speedbar company function-args
    company-c-headers clean-aindent-mode ws-butler move-text
    glsl-mode dockerfile-mode flycheck avy realgud)
  "A list of packages to ensure are installed at launch.")


(require 'cl-lib)
(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

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
(global-hl-line-mode +1)
(setq subword-mode t)

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

(tool-bar-mode -1)
;(toggle-scroll-bar 1) 
(setq scroll-bar-mode 'right) 
(menu-bar-mode -99)

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

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)
(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(load-library "hideshow")
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)


(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
    Non-interactive arguments are Begin End Regexp"
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

(subword-mode 1)

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
(global-set-key [S-C-up] 'move-text-up)
(global-set-key [S-C-down] 'move-text-down)

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

(require 'cc-mode)
(setq c-basic-offset 4)
(setq-default c-electric-flag nil)

(require 'python)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yaml$" . yaml-mode))

(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'pkg-info)
(require 'dash)
(require 'dash-functional)
(require 'cider)



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

(setq inhibit-startup-message t)
(put 'narrow-to-region 'disabled nil)

;;(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

;;;;;;;;;;;;;;;;;;;;;;;;

(when (display-graphic-p)
    (require 'fill-column-indicator)
    (setq-default fci-rule-column 100)
    (setq fci-rule-width 2)
    (setq fci-rule-use-dashes 1)
    (setq fci-dash-pattern 0.6)
    (setq fci-rule-color "pink")
    (add-hook 'python-mode-hook 'fci-mode)
    (add-hook 'groovy-mode-hook 'fci-mode)
    (add-hook 'c-mode-common-hook 'fci-mode)
    (add-hook 'sh-mode-hook 'fci-mode)
    (add-hook 'emacs-lisp-mode-hook 'fci-mode))

(global-prettify-symbols-mode 1) 

(load-theme 'zenburn t)
(setq my-cur-theme 'zenburn)

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
    (toggle-read-only)
    (toggle-theme)))
(global-set-key (kbd "C-x C-q") 'toggle-theme-on-readonly-switch)

(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 140))

(set-face-attribute 'default nil :family "Source Code Pro" :height 100)
(set-face-attribute 'variable-pitch nil :family "Nimbus Sans" :height 105)

;; (set-face-attribute 'font-lock-string-face nil
;;                     :inherit 'variable-pitch)

(set-face-attribute 'font-lock-comment-face nil
                    :inherit 'variable-pitch
                    :slant 'italic)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-help-echo-function nil)
(setq flycheck-indication-mode 'right-fringe)

;; (custom-set-faces
;;  '(popup-face ((((class color)) (:background "#555555"))) t)) ;; this is broken???
(setq flymake-fringe-indicator-position 'right-fringe)
(custom-set-faces
 '(flycheck-info ((((class color)) (:underline (:color "#33AAAA" :style line)))))
 '(flycheck-error ((((class color)) (:underline (:color "#CC0000" :style wave)))))
 '(flycheck-warning ((((class color)) (:underline (:color "#FFFF00" :style wave)))))
 '(flycheck-fringe-info ((((class color)) (:background "#338888" :foreground "#000000"))))
 '(flycheck-fringe-error ((((class color)) (:background "#990000" :foreground "#CCCCCC"))))
 '(flycheck-fringe-warning ((((class color)) (:background "#CCCC00" :foreground "#000000")))))

(flymake-mode 0)

;; (setq flymake-fringe-indicator-position 'right-fringe)
;; (custom-set-faces
;;  '(flymake-errline ((((class color)) (:underline "#990000"))))
;;  '(flymake-warnline ((((class color)) (:underline "#999900")))))

(custom-set-faces
 '(my-space-face ((((class color)) (:foreground "#555555"))) t)
 '(my-tab-face ((((class color)) (:foreground "#308030"))) t))

(require 'whitespace)
(set-face-attribute 'whitespace-line nil
                    :box "#660000"
                    :foreground "#FFFFDD")

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

(setq whitespace-line-column 161)
(setq whitespace-style '(face space space-mark tab tab-mark newline newline-mark lines-tail))

(setq
 whitespace-display-mappings ;; http://ergoemacs.org/emacs/whitespace-mode.html
 '(
   (space-mark 32 [183] [46])
   (newline-mark 10 [9166 10])
   (tab-mark 9 [9655 9] [92 9])
   ))

(global-whitespace-mode t)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
;;;;(add-hook 'groovy-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
(add-hook 'sh-mode-hook 'rainbow-delimiters-mode)
(add-hook 'yaml-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-autoescape-string-quote nil)

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :weight 'bold
                    :box "#00FF00"
                    :foreground '"red"
                    :inherit 'error
                    :strike-through t)


(require 'cl-lib)

(defvar my-paren-dual-colors
  '("#f25e40" "#a7d52a"))
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (set-face-foreground
  (intern (format "rainbow-delimiters-depth-%d-face" index))
  (elt my-paren-dual-colors
       (if (cl-evenp index) 0 1))))

;;(require 'nyan-mode)
;;(nyan-mode t)
;;(setq nyan-wavy-trail 't
;;      nyan-animate-nyancat 't
;;      nyan-bar-length 20)
;;(nyan-start-animation)

(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

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
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/home/cwachter/emacs/ac-dict")
;; (ac-config-default)
;; (setq ac-delay 1.33)
;; (setq ac-auto-start 2)
;; (setq ac-auto-show-menu t)
;; (setq ac-use-fuzzy t)
;; (setq ac-menu-height 6)

;; (set-face-foreground 'ac-candidate-face "white")
;; (set-face-background 'ac-candidate-face "black")
;; (set-face-underline 'ac-candidate-face "darkgray")
;; (set-face-background 'ac-selection-face "orange")
;; (set-face-foreground 'ac-selection-face "black")
;; (set-face-foreground 'ac-completion-face "green")

(setq dabbrev-case-replace nil)

;; (require 'multi-term)
;; (setq multi-term-program "/bin/bash")

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

(global-set-key (kbd "C-M-s-<left>")  'windmove-left)
(global-set-key (kbd "C-M-s-<right>") 'windmove-right)
(global-set-key (kbd "C-M-s-<up>")    'windmove-up)
(global-set-key (kbd "C-M-s-<down>")  'windmove-down)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(require 'semantic/ia)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(semantic-mode 1)
(global-ede-mode 1)
(global-set-key [(M-mouse-1)] 'semantic-ia-fast-mouse-jump)

(require 'semantic/sb)

(defun my-c-mode-cedet-hook ()
 (add-to-list 'ac-sources 'ac-source-gtags)
 (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(defun my-python-mode-cedet-hook ()
 (add-to-list 'ac-sources 'ac-source-gtags)
 (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'python-mode-common-hook 'my-python-mode-cedet-hook)

(defun my-semantic-hook ()
 (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(setq path-to-ctags "TAGS")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
)

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
  (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(require 'linum)
(setq linum-format "%d")

;; (dolist (hook '(python-mode-hook
;;                 c-mode-common-hook
;;                 sh-mode-hook
;;                 groovy-mode-hook
;;                 emacs-lisp-mode-hook
;;                ))
;;   (add-hook hook (lambda () (linum-mode t)))
;; )


;; (setq flyspell-issue-message-flag nil)

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
;; (add-hook 'python-mode-hook 'flyspell-prog-mode)


(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "s-p") 'org-mark-ring-goto)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "H-SPC") 'mc/mark-all-like-this)

(global-set-key (kbd "s-y") `yank-rectangle)
(global-set-key (kbd "s-k") `kill-rectangle)
(global-set-key (kbd "s-w") `copy-rectangle-as-kill)

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

(require 'smartparens)
(require 'smartparens-config)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm!
(require 'helm-config)
(require 'helm-grep)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-candidate-number-limit 500 ; limit the number of displayed canidates
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers

 )

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)

(global-set-key (kbd "C-c h x") 'helm-register)
;; (global-set-key (kbd "C-x r j") 'jump-to-register)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

(define-key global-map [remap find-tag] 'helm-etags-select)

(define-key global-map [remap list-buffers] 'helm-buffers-list)


(require 'helm-swoop)
(global-set-key (kbd "<f1>") 'helm-swoop)
(global-set-key (kbd "<f2>") 'helm-multi-swoop)
(global-set-key (kbd "<f3>") 'helm-multi-swoop-all)
(define-key isearch-mode-map (kbd "<f4>") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "<f4>") 'helm-multi-swoop-all-from-helm-swoop)

(setq helm-multi-swoop-edit-save t)
(setq helm-swoop-split-with-multiple-windows t)
(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-speed-or-color t)

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; New c-lang stuff
(setq c-default-style "stroustrup")
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(setq
 gdb-many-windows t
 gdb-show-main t
 )
(add-to-list 'company-backends 'company-c-headers)

(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(control tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;;;;;;;;

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fx\\'" . glsl-mode))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode)

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

(global-set-key (kbd "M-<pause>") `toggle-window-split)

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
        (linum-mode 1)
        (goto-line (read-number "Recenter on which line? "))
        (recenter))
    (linum-mode -1)))
(global-set-key (kbd "M-g") 'goto-line-and-recenter)


(global-set-key (kbd "M-<up>") 'scroll-down-command)
(global-set-key (kbd "M-<down>") 'scroll-up-command)

(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-DEL") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)

(global-set-key (kbd "M-SPC") 'hippie-expand)

(define-key isearch-mode-map [next] 'isearch-repeat-forward)
(define-key isearch-mode-map [prior] 'isearch-repeat-backward)

(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "M-;") 'uncomment-region)

(global-set-key (kbd "<pause>") 'save-buffer)
(global-set-key (kbd "M-s-C-<pause>") 'kill-buffer)

(defun jump-to-mark ()
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)

(global-set-key (kbd "H-<right>") 'sp-forward-sexp)
(global-set-key (kbd "H-<left>") 'sp-backward-sexp)

(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;


