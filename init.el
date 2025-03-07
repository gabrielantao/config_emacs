;; ignore anoying default features of Emacs
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (setq inhibit-splash-screen t)
 (setq ring-bell-function 'ignore)

 ;; show the positions numbers and fringe
 (setq column-number-mode t)
 (global-hl-line-mode 0)
;;(setq switch-to-buffer-preserve-window-point nil)
 (setq display-line-numbers-grow-only t)
 (setq display-line-numbers-width-start t)
 (add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
 (add-hook 'conf-mode-hook (lambda () (setq truncate-lines t)))
 (add-hook 'yaml-mode-hook (lambda () (setq truncate-lines t)))
 ;; (defun my-no-wrap-for-config-files ()
 ;;   "Desativa quebras de linha automáticas para arquivos TOML, JSON e YAML."
 ;;   (when (derived-mode-p 'json-mode 'yaml-mode 'toml-mode)
 ;;     (setq truncate-lines t)
 ;;     (auto-fill-mode -1)))

 (add-hook 'prog-mode-hook 'display-line-numbers-mode)
 (add-hook 'conf-mode-hook 'display-line-numbers-mode)
 (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
 ;; (add-hook 'json-mode-hook #'my-no-wrap-for-config-files)
 ;; (add-hook 'yaml-mode-hook #'my-no-wrap-for-config-files)
 ;; (add-hook 'toml-mode-hook #'my-no-wrap-for-config-files)

 (setq left-fringe-width 10)

 ;; divider for the windows 
 (setq window-divider-default-bottom-width 4)
 (setq window-divider-default-right-width 4)
 (window-divider-mode 1)

 ;; scroll of window
 (scroll-bar-mode -1)
 (setq scroll-step 1)
 (setq scroll-margin 0)
 (setq scroll-conservatively 101)
 (setq next-screen-context-lines 0)

 ;; Use "y or n" instead of "yes or no" and better message of scratch buffer
 (fset 'yes-or-no-p 'y-or-n-p)
 (setq initial-scratch-message ";; This is the playground buffer to try elisp expressions ...\n\n")

 ;; backups and auto save
 (setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
 (setq make-backup-files t)
 (setq auto-save-default t)

 ;; highlight cursor
 (setq visible-cursor nil)

 ;; Use spaces instead of tabs
 (setq-default indent-tabs-mode nil)
 (setq-default tab-width 4)

 ;; window split
 (setq split-width-threshold 120)
 (setq split-height-threshold nil)

 ;; automatic files reload
 (global-auto-revert-mode 1)
 ;;(setq echo-keystrokes 0.5)

 ;; allow to delete the current selected region
 (delete-selection-mode 1)

 ;; unset the suspend-frame
 (global-unset-key (kbd "C-z"))
 (global-unset-key (kbd "C-x C-z"))

;; shift do not select, use hydras instead (cut/delete/copy/move operations)
(setq shift-select-mode nil)
(setq mouse-drag-copy-region nil)
(global-unset-key [down-mouse-1]) ;; Desativa a seleção com o botão esquerdo
(global-unset-key [drag-mouse-1]) ;; Remove a funcionalidade de arrastar para selecionar
;;(global-unset-key [mouse-1])      ;; Bloqueia o clique para iniciar seleção

;; define the straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; need to do this require just to avid Flycheck errors waterfall
(require 'straight)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defvar my-cursor-default-color "magenta"
  "Default color for the cursor.")

;; themes and icons
(straight-use-package 'ef-themes)
(straight-use-package 'catppuccin-theme)
(straight-use-package 'all-the-icons)
(straight-use-package 'nerd-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-moonlight t))

(set-face-attribute 'cursor nil :background  my-cursor-default-color)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)
(setq-default line-spacing 0.3)

;; configure better identation bars
;; TODO: configure this package, it is not showing anything
(use-package indent-bars)
  ;; :config
  ;; (setq
  ;;  indent-bars-color '(highlight :face-bg t :blend 0.2)
  ;;  indent-bars-pattern "."
  ;;  indent-bars-width-frac 0.1
  ;;  indent-bars-pad-frac 0.1
  ;;  indent-bars-zigzag nil
  ;;  indent-bars-color-by-depth nil
  ;;  indent-bars-highlight-current-depth nil
  ;;  indent-bars-display-on-blank-lines nil))


;; improve the start dashboard
(use-package dashboard
  :config
  ;;(setq dashboard-image-directory "~/.emacs.d/images/")
  (setq dashboard-banner-logo-title (format "Welcome to GNU Emacs v%s" emacs-version))
  (setq dashboard-startup-banner "~/.emacs.d/images/emacs-modern-logo.png")
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-item-shortcuts '((recents   . "r")
                                 (bookmarks . "b")
                                 (projects  . "p")
                                 (agenda    . "a")))
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)))
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  ;; for now not use the navigator widget
                                  ;;dashboard-insert-navigator
                                  ;;dashboard-insert-newline
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-newline
                                  dashboard-insert-footer))
  (setq dashboard-heading-shorcut-format " [%s]")
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)
  (dashboard-set-footer t)
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; configure the doom modelinew
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-buffer-encoding nil))

;; improve the aspect of compilation mode when show ansi colors
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; FIX: highlight colors are no being showed for parentesis-like chars
;; but when enter M-x menu it shows fine, investigate it.
(use-package smartparens
  :config
  ;;(require 'smartparens-config)  ;; Carrega a configuração padrão
  (smartparens-global-mode 1)    ;; Ativa o modo globalmente
  (show-smartparens-global-mode t)

  (custom-set-faces
   '(sp-show-pair-match-face
     ((t (:foreground "#f8f8f2" :background "#44475a" :weight bold))))
   '(sp-show-pair-mismatch-face
     ((t (:foreground "#faafff" :background "#ff0000" :weight bold))))))

(use-package
  multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

;; configure the to jump with avy
(use-package avy
  :straight t
  :bind (("M-g a" . avy-goto-char)
         ("M-g r" . avy-goto-line) ;; row
         ("M-g w" . avy-goto-word-1)
         ("M-g t" . avy-goto-char-timer))
  :config
  (setq avy-background t)
  (custom-set-faces
    ;; background color of the face in the windows
    '(avy-background-face ((t (:foreground "gray40"))))
    ;; letter to jump
    '(avy-lead-face ((t (:background "black" :foreground "yellow" :weight bold))))
    ;; letter with high priority
    '(avy-lead-face-0 ((t (:background "blue" :foreground "white"))))
    ;; letter with intermediate priority
    '(avy-lead-face-1 ((t (:background "green" :foreground "black")))))
  :custom
  (avy-timeout-seconds 1.0))

;; navigate easily through links
(use-package ace-link
  :config
  (ace-link-setup-default))

;; jump windows with ace window
(use-package ace-window)

;; pulsar used to pulse the line when the cursor make (movements) like jumps
(use-package pulsar
  :config
  (pulsar-global-mode 1) ;; Ativa o pulsar globalmente
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  ;; add hooks for the emacs builtin jump operations
  (dolist (hook '(other-window
                  goto-line
                  recenter-top-bottom
                  scroll-up
                  scroll-down
                  switch-to-buffer))
    (add-hook hook #'pulsar-pulse-line))
  )

;; keep the same position when scrolling
(use-package scroll-page-without-moving-point
  :straight (:host github :repo "tanrax/scroll-page-without-moving-point.el" :files ("scroll-page-without-moving-point.el"))
  :ensure t)

(defun my-pulsar-scroll-page-up (&optional n)
  "Scroll up N lines without moving point and pulse the current line."
  (interactive "p")
  (dotimes (_ (or n 1))
    (scroll-page-without-moving-point-up))
  (pulsar-pulse-line))

(defun my-pulsar-scroll-page-down (&optional n)
  "Scroll down N lines without moving point and pulse the current line."
  (interactive "p")
  (dotimes (_ (or n 1))
    (scroll-page-without-moving-point-down))
  (pulsar-pulse-line))

(defun my-pulsar-scroll-page-up-multi (&optional n)
  "Scroll up N lines without moving point. Pulse if N > 1."
  (interactive "p")
  (let ((num-lines (or n 1)))
    (dotimes (_ num-lines)
      (scroll-page-without-moving-point-up))
    (when (> num-lines 1)
      (pulsar-pulse-line))))

(defun my-pulsar-scroll-page-down-multi (&optional n)
  "Scroll down N lines without moving point. Pulse if N > 1."
  (interactive "p")
  (let ((num-lines (or n 1)))
    (dotimes (_ num-lines)
      (scroll-page-without-moving-point-down))
    (when (> num-lines 1)
      (pulsar-pulse-line))))

;; useful because projectile depends on it
(use-package rg)
(rg-enable-default-bindings)

;; TODO: check if will keep this or just rg with projectile
(use-package deadgrep
    :bind (:map deadgrep-mode-map
              ("l" . deadgrep-forward-match)
              ("j" . deadgrep-backward-match)
              ("k" . deadgrep-forward-filename)
              ("i" . deadgrep-backward-filename)
              ("r" . deadgrep-restart)
              ("s" . deadgrep-kill-process)
              ;; deactivate the original keybindings
              ("p" . nil) ;; previous
              ("n" . nil) ;; next
              ("g" . nil))) ;; restart

(use-package wgrep
  :after deadgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "e"))

(use-package wgrep-deadgrep
  :after deadgrep)

(use-package anzu
  :straight t
  :init
  (global-anzu-mode 1)
  :config
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-replace-to-string-separator " ~▶"))

(defun my/anzu-replace-in-buffer ()
  "Move para o topo do buffer antes de substituir com anzu."
  (interactive)
  (goto-char (point-min))
  (call-interactively 'anzu-query-replace))

(defun my/anzu-replace-regexp-in-buffer ()
  "Move para o topo do buffer antes de substituir com anzu."
  (interactive)
  (goto-char (point-min))
  (call-interactively 'anzu-query-replace-regexp))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point)))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.0)
  (setq which-key-idle-secondary-delay 0.05))

(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :init
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<"))

(use-package consult-dir)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; show active keybindings in current context
  :init
  (setq prefix-help-command #'embark-prefix-help-command))  ;; use embark in C-h

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; set dir-locals variables content always marked as safe
(setq enable-local-variables :all)

      ;; project management
      (use-package projectile
        :config
        (projectile-mode +1)
        (setq projectile-project-search-path '("~/Projects/" "~/Playground/"))
        (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
        (setq projectile-generic-command "rg --files --hidden")
      ;;(add-hook 'project-find-functions #'project-projectile)

      ;; use consult to help projectile experience
      (use-package consult-projectile
        :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

      ;; TODO: try again the perspective package in future
      ;; perspective to have a workspace-like features
      ;; (use-package perspective
      ;;   :init
      ;;   (persp-mode))

      (use-package magit
        :bind (("C-x g" . magit-status))
        :config
        (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

  (use-package treemacs
      :ensure t
      :defer t
      :config
      (treemacs-follow-mode t)
      (setq treemacs-theme 'icons)
      (setq treemacs-position 'left)
      (setq treemacs-width 40)
      (setq treemacs-indentation 2)
      (setq treemacs-show-hidden-files t)
      (setq treemacs-hide-dot-git-directory nil)
      (setq treemacs-show-workspace-sidebar t)
      (setq treemacs-space-between-root-nodes nil)
      (setq treemacs-move-files-by-mouse-dragging nil)
      (setq treemacs-persist-file (expand-file-name ".treemacs-workspaces" user-emacs-directory))
      (treemacs-filewatch-mode t)
      (treemacs-resize-icons 15))

    (use-package treemacs-projectile
      :after (treemacs projectile)
      :ensure t)

    (use-package treemacs-magit
      :after (treemacs magit)
      :ensure t)

    (use-package treemacs-icons-dired
      :hook (dired-mode . treemacs-icons-dired-enable-once)
      :ensure t)

    ;; configs of Dired
    (setq dired-kill-when-opening-new-dired-buffer t)
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)
    (setq ls-lisp-ignore-case t)
    (setq ls-lisp-dirs-first t)
    (setq dired-listing-switches "-Alh --group-directories-first --sort=version")
    ;;(defun my/dired-hide-cursor ()
    ;;"Hide the cursor for the dired mode."
    ;; (setq-local cursor-type nil))
    ;;(add-hook 'dired-mode-hook #'my/dired-hide-cursor)
    (add-hook 'dired-mode-hook 'auto-revert-mode)
    (add-hook 'dired-mode-hook 'hl-line-mode)
    (with-eval-after-load 'dired
      (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
      (define-key dired-mode-map (kbd "SPC") 'dired-create-empty-file))

    ;; add colors to Dired
    (use-package diredfl
      :hook (dired-mode . diredfl-mode))

    (use-package dired-git-info)
    (setq dgi-auto-hide-details-p nil)
    (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
    ;;(setq dired-git-info-mode t)

  (use-package dired-filter
  :after dired
  :config
  (define-key dired-mode-map (kbd "/ g") 'dired-filter-by-git-ignored)
  (define-key dired-mode-map (kbd "/ i g") nil))

(use-package dired-subtree
  :after dired
  :config
  (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle))

(use-package dired-narrow
  :after dired
  :config
  (define-key dired-mode-map (kbd "/ N") 'dired-narrow)
  (define-key dired-mode-map (kbd "/ R") 'dired-narrow-regexp)
  (define-key dired-mode-map (kbd "/ F") 'dired-narrow-fuzzy))

   ;; TODO: try and configure these dired hacks
     ;; 
 ;; (use-package dired-avfs)
    ;; (use-package dired-collapse
    ;;   :hook (dired-mode . dired-collapse-mode))
    ;; (use-package dired-rainbow
    ;;   :config
    ;;   (dired-rainbow-define html "#8b0000" "\\.html?$")
    ;;   (dired-rainbow-define media "#ff4500" "\\.mp3$|\\.mp4$|\\.avi$")
    ;;   (dired-rainbow-define log "#ff1493" "\\.log$"))
    ;; (use-package dired-open
    ;;   :config
    ;;   (setq dired-open-extensions '(("mp4" . "vlc")
    ;;                                 ("mkv" . "vlc")
    ;;                                 ("png" . "feh")
    ;;                                 ("jpg" . "feh"))))

    ;; load hydra to proper sort the files
    (use-package dired-quick-sort)

    ;; deal with todo list
    (use-package hl-todo
      :straight t
      :hook (prog-mode . hl-todo-mode)
      :config
      (setq hl-todo-highlight-punctuation ":"
            hl-todo-keyword-faces
            '(("TODO"   . "#FF4500")
              ("FIXME"  . "#FF0000")
              ("NOTE"   . "#1E90FF")
              ("HACK"   . "#8A2BE2")
              ("REVIEW" . "#FFD700"))))

    (use-package consult-todo
      :demand t
      :config
      (setq consult-todo-keywords '("TODO" "FIXME" "NOTE" "HACK" "REVIEW")))

(use-package vterm
:ensure t
:config
 (setq vterm-max-scrollback 10000)
 (setq vterm-shell "/bin/fish"))

(use-package org
  :ensure t
  :pin gnu
  :config
  (setq org-startup-indented t
        org-startup-folded t
        org-hide-leading-stars t
        org-ellipsis " ▼ "
        org-src-fontify-natively t
        ; org-log-done 'time
        org-log-into-drawer t)
  (setq org-directory "~/Documents/notes")         
  (setq org-agenda-files '("~/Documents/notes/agenda.org")))

;; configure better heading marks
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("⬘ " "⬗ " "⬙ " "⬖ " "●" "●" "●" "●")))

;; just let the package auto tangle my modifications
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; always start the editor with a org-mode buffer
(defun my-create-org-scratch-buffer ()
 "Create and show a org notes buffer."
 (let ((buf (get-buffer-create "notes-org")))
   (with-current-buffer buf
    (org-mode)
    (insert "#+TITLE !!! ORG NOTES BUFFER !!!\n\n"))))

 (add-hook 'emacs-startup-hook #'my-create-org-scratch-buffer)

;; completitions for the code and text
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (global-set-key (kbd "C-SPC") #'completion-at-point)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'lsp-completion-at-point))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-prompt-functions '(yas-completing-prompt)))

(use-package yasnippet-snippets)

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet))

(use-package flycheck
  :custom
  (flycheck-global-modes t) 
  (flycheck-highlighting-mode 'symbols)
  ;; do not flood the minibuffer with alerts
  (flycheck-auto-display-errors-after-checking nil)
  (flycheck-display-errors-function #'ignore)
  (flycheck-display-errors-delay 5))

 (use-package consult-flycheck)

;; remove the global keymap 
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c !") nil))

;; let the lsp-ui work together with flycheck to show the erros inline
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-update-mode 'point) ;; could also be line
  (lsp-ui-sideline-delay 1)
  (lsp-ui-sideline-diagnostic-max-lines 1)
  (lsp-ui-peek-enable nil)
  (lsp-ui-doc-enable nil))

;; TODO: add here the flyspell too

(defun my/setup-lsp-mode ()
    "Basic setup for the lsp-mode."
    (lsp-enable-which-key-integration)
    ;;(flycheck-mode 1)
    ;;(flyspell-prog-mode)
    ;;(yas-minor-mode-on)
    ;;(lsp-diagnostics-mode 1)
    ;;(lsp-completion-mode 1)
    )

  (use-package lsp-mode
    :init
    (setq lsp-keymap-prefix "C-c l")
    :commands (lsp lsp-deferred)
    :config
    (lsp-enable-which-key-integration t)
    (flycheck-mode 1)
    ;; (flyspell-prog-mode)
    ;; (yas-minor-mode-on)
    (lsp-diagnostics-mode 1)
    (lsp-completion-mode 1)
    :custom
    (lsp-modeline-code-actions-enable nil)
    (lsp-modeline-diagnostics-enable nil)
    ;; (lsp-log-io nil)
    ;; (lsp-print-performance nil)
    ;; (lsp-report-if-no-buffer nil)
    ;; (lsp-server-trace nil)
    ;; (lsp-keep-workspace-alive nil)
    (lsp-enable-snippet t)
    ;; (lsp-auto-guess-root t)
    ;; (lsp-restart 'iteractive)
    ;; (lsp-auto-configure nil)
    ;; (lsp-auto-execute-action nil)
    ;; (lsp-eldoce-render-all nil)
    (lsp-enable-completion-at-point t)
    (lsp-enable-xref t)
    (lsp-diagnostics-provider :flycheck)
    ;; (lsp-enable-indentation t)
    (lsp-enable-on-type-formatting nil)
    (lsp-before-save-edits nil)
    (lsp-enable-imenu t)
    (lsp-imenu-show-container-name t)
    (lsp-imenu-container-name-separator "//")
    (lsp-imenu-sort-methods '(kind name))
    (lsp-response-timeout 10)
    (lsp-enable-file-watchers nil)
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-semantic-highlighting t)
    ;; (lsp-signature-auto-activate t)
    ;; (lsp-signature-render-documentation nil)
    (lsp-enable-text-document-color nil)
    (lsp-completion-provider :none)
    (gc-cons-threshold 100000000)
    (read-process-output-max (* 3 1024 1024)))
  (add-hook 'before-save-hook #'lsp-format-buffer)

  ;;(use-package consult-lsp)

  ;; Python external dependencies (for LSP):
  ;; - python-lsp-server (pip install 'python-lsp-server[all]')
  ;; - python-debugpy
  (use-package python-mode
    :hook (python-mode . lsp-deferred)
    :config
  (setq python-shell-interpreter "cd ~/.config/pixi_envs && pixi run -e devenv-python python")
  ;; (setq lsp-pylsp-server-command "cd ~/.config/pixi_envs && pixi run -e devenv-python pylsp")
  ;; (setq lsp-pylsp-plugins-preload-enabled nil)
  ;; disable the default plugins
  ;; (setq lsp-pylsp-plugins-autopep8-enabled nil)
  ;; (setq lsp-pylsp-plugins-flake8-enabled nil)
  ;; (setq lsp-pylsp-plugins-isort-enabled nil)
  ;; (setq lsp-pylsp-plugins-pycodestyle-enabled nil)
  ;; (setq lsp-pylsp-plugins-pyflakes-enabled nil)
  ;; (setq lsp-pylsp-plugins-pylint-enabled nil)
  ;; (setq lsp-pylsp-plugins-yapf-enabled nil)
  ;; enable the tools used for the project
  ;; (setq lsp-pylsp-plugins-jedi-completion-enabled t)
  ;; (setq lsp-pylsp-plugins-jedi-environment "~/.config/pixi_envs/.pixi/envs/devenv-python")
  ;; (setq lsp-pylsp-plugins-rope-autoimport-enabled nil)
  ;; (setq lsp-pylsp-plugins-rope-completion-enabled t)
  ;; (setq lsp-pylsp-plugins-rope-autoimport-code-actions-enabled t)
  ;; (setq lsp-pylsp-plugins-rope-autoimport-completions-enabled t)
  ;; (setq lsp-pylsp-plugins-mccabe-enabled t)
  ;; for mypy remember to change also in the pyproject.toml file section
  ;; see the pylsp-mypy for more info
  ;; (setq lsp-pylsp-plugins-mypy-enabled nil) ;; disable for now
  ;; (setq lsp-pylsp-plugins-mypy-live-mode t)
  ;; (setq lsp-pylsp-plugins-ruff-executable "cd ~/.config/pixi_envs && pixi run -e devenv-python ruff")
  ;; (setq lsp-pylsp-plugins-ruff-enabled t)
  ;; (setq lsp-pylsp-plugins-ruff-config "~/.config/pixi_envs/ruff.toml")
  
  
  (setq python-pytest-executable "cd ~/.config/pixi_envs && pixi run -e devenv-python pytest")
  (setq python-pytest-unsaved-buffers-behavior 'save-all)
  (setq python-pytest-confirm nil)
  (setq dap-python-debugger "cd ~/.config/pixi_envs && pixi run -e devenv-python debugpy"))

(use-package lsp-jedi
  :ensure t)
  
  (use-package python-pytest
    :custom
    (python-pytest-confirm t)
    :config
    ;; just an extra `-y' after the `-x' suffix
    (transient-append-suffix
      'python-pytest-dispatch
      "-x"
      '("-P" "IPython Debugger" "--pdbcls=IPython.terminal.debugger:TerminalPdb")))

  ;; it needs dependency of taplo
  (use-package toml-mode
    :hook (toml-mode . lsp-deferred)
    :config
    (setq lsp-toml-command "cd ~/.config/pixi_envs && pixi run -e devenv-configs taplo"))

  (use-package yaml-mode
    ;; :mode "\\.ya?ml\\'"
    :hook (yaml-mode . lsp-deferred)
    :config
    (setq lsp-yaml-server-command "cd ~/.config/pixi_envs && pixi run -e devenv-configs yaml-language-server --stdio"))

  ;; it needs dependency of fortls
  (use-package fortran
    :straight nil
    :hook (fortran-mode . lsp-deferred)
    :config
    (setq lsp-clients-fortls-executable "cd ~/.config/pixi_envs && pixi run -e devenv-configs fortls"))
  (use-package f90
    :straight nil
    :hook (f90-mode . lsp-deferred))

  (use-package racket-mode
  :ensure t
  :hook (racket-mode . lsp-deferred)
  :config
  ;;(add-to-list 'exec-path "~/.config/pixi_envs/.pixi/envs/devenv-racket/bin")
  ;; (setq racket-program "cd ~/.config/pixi_envs && pixi run -e devenv-racket racket")
  (setq lsp-racket-langserver-command "cd ~/.config/pixi_envs && pixi run -e devenv-racket racket --lib racket-langserver")
  (setq racket-show-functions 't)) ;; Mostra informações ao passar o cursor


  (use-package dape
    ;; :preface
    ;; By default dape shares the same keybinding prefix as `gud'
    ;; If you do not want to use any prefix, set it to nil.
    ;; (setq dape-key-prefix "\C-x\C-a")

    ;; :hook
    ;; Save breakpoints on quit
    ;; (kill-emacs . dape-breakpoint-save)
    ;; Load breakpoints on startup
    ;; (after-init . dape-breakpoint-load)

    :config
    ;; Turn on global bindings for setting breakpoints with mouse
    ;; (dape-breakpoint-global-mode)

    ;; Info buffers to the right
    ;; (setq dape-buffer-window-arrangement 'right)

    ;; Info buffers like gud (gdb-mi)
    ;; (setq dape-buffer-window-arrangement 'gud)
    ;; (setq dape-info-hide-mode-line nil)

    ;; Pulse source line (performance hit)
    ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

    ;; Showing inlay hints
    ;; (setq dape-inlay-hints t)

    ;; Save buffers on startup, useful for interpreted languages
    ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

    ;; Kill compile buffer on build success
    ;; (add-hook 'dape-compile-hook 'kill-buffer)

    ;; Projectile users
    (setq dape-cwd-function 'projectile-project-root)
    ;;
    ;;(setq dape-python-command "pixi run -e=devnev python")
    )

   (add-to-list 'dape-configs
             `(debugpy-project
               modes (python-ts-mode python-mode)
               ensure dape-ensure-command
               command "~/config/pixi_envs/scripts/start_python_debug.bash"
               command-args ("--host" "0.0.0.0" "--port" :autoport)
               port :autoport
               :type "python"             
               :request "launch"
               :args []
               :stopOnEntry t
               :console "externalTerminal"
               :showReturnValue t
               :justMyCode nil
               :cwd dape-cwd-fn
               ;:env ()
               ))
  ;; (add-to-list 'dape-configs
  ;;           `(debugpy-custom
  ;;             modes (python-ts-mode python-mode)
  ;;             ensure dape-ensure-command
  ;;             command "/home/gabriel/Projects/Fluxus/scripts/start_debug.bash"
  ;;             ;;command "pixi"
  ;;             command-args ("--port" :autoport)
  ;;             host "0.0.0.0"
  ;;             port :autoport
  ;;             :type "python"             
  ;;             :request "launch"
  ;;             :args []
  ;;             :stopOnEntry t
  ;;             :console "externalTerminal"
  ;;             :showReturnValue t
  ;;             :justMyCode nil
  ;;             :cwd dape-cwd-fn
  ;;             ;; the pixi run script does not set the env variables, as workaround it just sets manually
  ;;             ;;:env (:PYTHONPATH "/home/gabriel/Projects/Fluxus"
  ;;               ;;                :PIXI_PROJECT_NAME "Fluxus"
  ;;                ;;               :SIMULATOR_SOURCE_PATH "/home/gabriel/Projects/Fluxus/simulator"
  ;;                 ;;              :CONTROLLER_PATH "/home/gabriel/Projects/Fluxus/controller"
  ;;                 ;;  :DEBUG "true"
  ;;                   ;;)
  ;;            ))
  ;; (defun my/dape-debug-buffer ()
  ;; "Inicia depuração do arquivo do buffer atual."
  ;; (interactive)
  ;; (dape-start
  ;;  `(:type "python"
  ;;    :request "launch"
  ;;    :name "Debug buffer"
  ;;    :program ,(buffer-file-name)
  ;;    :cwd ,(lsp-workspace-root)
  ;;  ;;  :console "integratedTerminal")
  ;;  )
  ;; )

  ;; Enable repeat mode for more ergonomic `dape' use
  (use-package repeat
    :config
    (repeat-mode))


  ;; (use-package dap-mode
  ;;   :after lsp-mode
  ;;   :hook ((lsp-mode . dap-mode)
  ;;          (lsp-mode . dap-ui-mode))
  ;;   ;; :hook (python-mode . dap-mode)
  ;;   :config
  ;;   (setq dap-python-debugger 'debugpy))

  ;; (use-package dap-mode
  ;; :hook ((lsp-mode . dap-mode)
  ;;        (lsp-mode . dap-ui-mode)))

  ;; configure the lsp-docker in order to run the LSP servers inside the containers
  ;; and then do not need to install anything directly in my machine
  ;; (use-package lsp-docker)
  ;; (setq lsp-docker-client-configs
  ;;       '((:server-id pylsp-docker ;; ID do servidor no Docker
  ;;          :docker-image-id "emacslsp/lsp-docker-langservers" ;; Imagem Docker
  ;;          :server-command "pylsp"))) ;; Comando para iniciar o pylsp
  ;; (lsp-docker-init-clients
  ;;  :path-mappings '(("/home/gabriel/Projects" . "/projects")) ;; Mapeamento de pastas
  ;;  :client-packages lsp-docker-client-packages
  ;;  :client-configs lsp-docker-client-configs)

  ;; use treemacs to help with the code data
  (use-package lsp-treemacs
    :after (lsp-mode treemacs)
    :config
    (lsp-treemacs-sync-mode 1))

(use-package hydra)
(defvar my-hydra-cut-or-copy 'copy
  "Define if the action in the Hydra should be 'cut' or 'copy' or 'move'.")

(defun my-hydra-action ()
"Perform the action of copy, cut, or move depending on `my-hydra-cut-or-copy`."
(interactive)
(cond
 ;; copy text under region
 ((eq my-hydra-cut-or-copy 'copy)
  (kill-ring-save (region-beginning) (region-end))
  (message "Region copied.")
  (deactivate-mark))
 ;; cut the text under region
 ((eq my-hydra-cut-or-copy 'cut)
  (kill-region (region-beginning) (region-end))
  (message "Region cut.")
  (deactivate-mark))
 ((eq my-hydra-cut-or-copy 'delete)
  (delete-region (region-beginning) (region-end))
  (message "Region deleted.")
  (deactivate-mark))
 ;; do nothing with region, just move pointer
 ((eq my-hydra-cut-or-copy 'move)
  (message "Region selected."))))

(defun my-hydra-deactivate-mark-and-quit ()
"Unmark current region and show a message."
  (interactive)
  (deactivate-mark)
  (message "Cursor moved."))

(defhydra my-hydra-copy-or-cut (:foreign-keys warn :columns 4)
  "Copy, cut a region or just navigate with cursor jumps."
  ("q" my-hydra-deactivate-mark-and-quit "Quit" :exit t) 
  ("a" my-hydra-action "Accept" :exit t)
  ("-" pulsar-pulse-line "Pulse")
  ("r" set-mark-command "Reset Mark") 
  ;; movements
  ("j" backward-char "← Char")
  ("k" next-line "↓ Line")
  ("i" previous-line "↑ Line")
  ("l" forward-char "→ Char")
  ("<left>" backward-char "← Char")
  ("<down>" next-line "↓ Line")    
  ("<up>" previous-line "↑ Line")  
  ("<right>" forward-char "→ Char")
  ("u" backward-word "← Word")
  ("o" forward-word "→ Word")
  ("U" sp-backward-symbol "← Symbol")
  ("O" sp-forward-symbol "→ Symbol")
  ("J" beginning-of-line "|← Line Start")
  ("L" end-of-line "→| Line End")
  ("I" beginning-of-buffer "↖ Buffer Start")
  ("K" end-of-buffer "↘ Buffer End"))

(defun my-hydra-setup (action)
  "Set up the Hydra with the correct action (copy or cut)."
  (setq my-hydra-cut-or-copy action)
  (my-hydra-copy-or-cut/body))

(defun my-hydra-copy ()
  "Activate the Hydra with copy action."
  (interactive)
  (set-mark (point))
  (my-hydra-setup 'copy))

(defun my-hydra-cut ()
  "Activate the Hydra with cut action."
  (interactive)
  (set-mark (point))
  (my-hydra-setup 'cut))

(defun my-hydra-delete ()
  "Activate the Hydra with delete action."
  (interactive)
  (set-mark (point))
  (my-hydra-setup 'delete))

(defun my-hydra-move-select ()
  "Activate the Hydra with move action."
  (interactive)
  (set-mark (point))
  (my-hydra-setup 'move))

    (defhydra hydra-text-zoom (:color pink :timeout 4)
      "Scale text font"
      ("i" text-scale-increase "in")
      ("k" text-scale-decrease "out")
      ("q" nil "quit" :color blue))

    (defhydra hydra-window-scroll (:hint nil :color red)
      "
      Scrolling and Navigation:
      [_j_] ← scroll left  [_l_] → scroll right
      [_i_] ↑ scroll up    [_k_] ↓ scroll down
      [_I_] ↑↑ page up     [_K_] ↓↓ page down
      [_c_] - recenter
      [_q_] quit
    "
      ("l" scroll-left)
      ("j" scroll-right)
      ;; option: simple scroll with static point
      ;; ("i" (lambda (n) (interactive "p") (dotimes (_ n) (scroll-page-without-moving-point-up))))
      ;; ("k" (lambda (n) (interactive "p") (dotimes (_ n) (scroll-page-without-moving-point-down))))
      ("i" my-pulsar-scroll-page-up-multi)
      ("k" my-pulsar-scroll-page-down-multi)
      ("K" (lambda () (interactive) (scroll-up-command) (pulsar-recenter-middle)))
      ("I" (lambda () (interactive) (scroll-down-command) (pulsar-recenter-middle)))
      ("c" pulsar-recenter-middle)
      ("q" nil))

    (defhydra hydra-window-move (:color pink :columns 4)
      "Window navigation and manipulation"
      ("j" windmove-left "← left")
      ("l" windmove-right "→ right")
      ("k" windmove-down "↓ down")
      ("i" windmove-up "↑ up")
      ("J" windmove-swap-states-left "←← swap left")
      ("L" windmove-swap-states-right "→→ swap right")
      ("K" windmove-swap-states-down "↓↓ swap down")
      ("I" windmove-swap-states-up "↑↑ swap up")
      ("t" enlarge-window-horizontally "←|→ enlarge horizontally")
      ("g" shrink-window-horizontally "→|← shrink horizontally")
      ("y" enlarge-window "←|→ enlarge vertically")
      ("h" shrink-window "→|← shrink vertically")
      ("a" split-window-vertically "== split in rows")
      ("s" split-window-horizontally "|| split in columns")
      ("d" delete-window "delete window")
      ("D" delete-other-windows "delete other windows")
      ("o" other-window "other window")
      ("c" pulsar-recenter-middle "center window")
      ("q" nil "quit"))

(use-package general)

;; TODO: create hydras for these functions
;; identation/aligns
;; folding
;; moving between symbols
;; move line or region to line X or above/below line

(defun my-insert-backslash ()
  "Insert a backslash (`\\`)."
  (interactive)
  (insert "\\"))

(defun my-kill-region-or-line ()
"Kill a region, or cut whole line if there is not active region."
(interactive)
(if (use-region-p)
    (kill-region (region-beginning) (region-end))
  (kill-whole-line)))

(defun my-delete-whole-line ()
  "Delete the whole line without puting in the kill ring."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (forward-line 1)
  (delete-backward-char 1))

(general-create-definer my/leader-key
  :keymaps 'override
  :prefix "\\"
  :global-prefix "C-\\")
(my/leader-key
  ;; base text operations
  "y" 'consult-yasnippet
  "Y" 'yas-expand
  "\\" 'my-insert-backslash
  "-" 'pulsar-pulse-line
  ";" 'comment-line
  "z" 'undo
  "Z" 'undo-redo
  "c" 'my-hydra-copy
  "C" 'duplicate-line
  "x" 'my-hydra-cut    
  "X" 'kill-whole-line
  "d" 'my-hydra-delete
  "D" 'my-delete-whole-line
  "v" 'yank ;; paste
  "V" 'consult-yank-replace ;; consult available paste list
  "n" 'hydra-text-navigation/body
  "?" 'general-describe-keybindings
  "m" 'my-hydra-move-select
  
  ;; commands to execute
  "e" '(:ignore t :which-key "execute")
  "e x" 'execute-extended-command
  "e a" 'embark-act
  "e b" 'embark-bindings
  "e e" 'eval-buffer
  "e R" 'restart-emacs
  "e Q" 'save-buffers-kill-terminal
  "e d" 'dired
  "e g" 'magit
  "e t" 'vterm              ;; terminal shell
  "e T" 'vterm-other-window ;; terminal shell other window
  "e s" 'shell-command
  "e S" 'async-shell-command
  
  ;; ace jump in visible area of buffers
  "j" '(:ignore t :which-key "jump")
  "j c" 'avy-goto-char
  "j w" 'avy-goto-word-1
  "j l" 'avy-goto-line ;; go to line using letters
  "j t" 'avy-goto-char-timer
  "j k" 'ace-link

  ;; bigger jumps throughout the buffers to specific points
  "g" '(:ignore t :which-key "goto")
  "g l" 'consult-goto-line ;; go to line using number
  "g s" 'consult-line ;; go to searched term
  "g S" 'consult-line-multi
  "g i" 'consult-imenu
  "g I" 'consult-imenu-multi
  "g o" 'consult-outline
  "g m" 'consult-mark
  "g M" 'consult-global-mark
  "g b" 'consult-bookmark
  
  ;; todo jump
  "g T" '(:ignore t :which-key "todo")
  "g T t" 'consult-todo     ;; check in the current buffer
  "g T a" 'consult-todo-all ;; check in all live buffers
  ;;"g T p" 'consult-todo-project ;; TODO: remove, it caches forever
  "g T d" 'consult-todo-dir ;; check in the current directory
  "g T r" 'hl-todo-rgrep    ;; check in any directory selecting a path
  "g T o" 'hl-todo-occur

  ;; TODO: add keybindings for org mode
  ;;("h" consult-org-heading)
  ;;("a" consult-org-agenda)

  ;; search and replace
  "s" '(:ignore t :which-key "search/replace")
  "s g" 'consult-ripgrep
  "s r" 'anzu-query-replace
  "s R" 'anzu-query-replace-regexp
  "s b" 'my/anzu-replace-in-buffer
  "s B" 'my/anzu-replace-in-buffer-regexp

  ;; windows management and movements
  "w" '(:ignore t :which-key "window")
  "w m" 'hydra-window-move/body
  "w s" 'hydra-window-scroll/body
  "w z" 'hydra-text-zoom/body
  "w c" 'pulsar-recenter-middle
  "w w" 'ace-window    ;; when there is only two windows this is the same of other-window command
  "w o" 'other-window         ;; move to other window
  "w q" 'delete-window        ;; quit windows
  "w Q" 'delete-other-windows ;; quit other windows

  ;; deal with files
  "f" '(:ignore t :which-key "files/dir")
  "f d" 'consult-dir
  "f o" 'find-file     ;; open file
  "f f" 'consult-fd    ;; find file with fd
  "f F" 'consult-find
  "f r" 'consult-recent-file

  ;; deal with buffer
  "b" '(:ignore t :which-key "buffers")
  "b s" 'save-buffer
  "b b" 'switch-to-buffer
  "b B" 'consult-buffer
  "b k" 'kill-buffer
  "b K" 'kill-current-buffer

  ;; manage keybindings for the project
  "p" '(:ignore t :which-key "project")
  "p d" 'treemacs               ;; directories tree
  "p D" 'projectile-dired
  "p t" 'projectile-run-vterm              ;; terminal shell
  "p T" 'projectile-run-vterm-other-window ;; terminal shell other window
  "p E" 'projectile-edit-dir-locals
  
  ;; project workspace management commands
  "p w" '(:ignore t :which-key "workspaces")
  "p w c" 'treemacs-create-workspace
  "p w C" 'treemacs-create-workspace-from-project
  "p w a" 'treemacs-projectile             ;; it adds a new project to the workspace
  "p w e" 'treemacs-edit-workspaces
  "p w r" 'treemacs-remove-project-from-workspace
  "p w s" 'treemacs-switch-workspace
  "p w o" 'projectile-switch-open-project
  "p w O" 'consult-projectile-switch-project

  ;; TODO: try in the future use perspective again
  ;; project perspectives (workspaces)
  ;; "p p" '(:ignore t :which-key "perspectives")
  ;; "p p c" 'persp-switch
  ;; "p p i" 'persp-ibuffer
  ;; "p p k" 'persp-kill
  ;; "p p s" 'persp-state-save
  ;; "p p l" 'persp-state-load
  ;; "p p r" 'persp-state-restore
  ;; "p p o" 'projectile-persp-switch-project
  
  ;; project file and directory management
  "p f" '(:ignore t :which-key "file/dir")
  "p f d" 'consult-projectile-find-dir    
  "p f o" 'consult-projectile-find-file
  "p f r" 'consult-projectile-recentf
  "p f t" 'projectile-find-test-file
  
  ;; project search and replace
  "p s" '(:ignore t :which-key "search")
  "p s f" 'projectile-find-references
  "p s g" 'projectile-ripgrep
  "p s r" 'projectile-replace
  "p s R" 'projectile-replace-regexp
  
  ;; buffers in this project
  "p b" '(:ignore t :which-key "buffers")
  "p b s" 'projectile-save-project-buffers
  "p b b" 'consult-projectile-switch-to-buffer
  "p b r" 'consult-projectile-recentf
  "p b i" 'projectile-ibuffer
  
  ;; execution commands for project
  "p e" '(:ignore t :which-key "execute")
  "p e C" 'projectile-configure-project
  "p e c" 'projectile-compile-project
  "p e t" 'projectile-test-project
  "p e r" 'projectile-run-project
  "p e P" 'projectile-package-project
  "p e I" 'projectile-install-project
  "p e s" 'projectile-run-shell-command-in-root
  "p e S" 'projectile-run-async-shell-command-in-root

  ;; code details navigation
  "p c" '(:ignore t :which-key "code")
  "p c e" 'consult-compile-error
  "p c g" 'consult-git-grep
  "p c s" 'lsp-treemacs-symbols
  "p c h" 'lsp-treemacs-code-hierarchy ;; LSP must support this
  "p c i" 'lsp-treemacs-implementations ;; LSP must support this

  ;; commands dedicated to the LSP tasks
  "p l" '(:ignore t :which-key "LSP")
  "p l a" 'lsp-execute-code-action
  ;"p l !" 'flycheck-list-errors ;; diagnostics list in other buffer
  "p l d" 'consult-flycheck ;; show diagnostics erros using consult
  "p l D" 'lsp-ui-flycheck-list ;; show for the whole workspace
  "p l !" 'flycheck-clear
  ;; "g d" 'consult-lsp-diagnostics
  ;; "g y" 'consult-lsp-file-symbols
  ;; "g Y" 'consult-lsp-symbols      
  )
