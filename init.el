(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; show the positions and line highlith
(setq column-number-mode t)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-hl-line-mode 0)
(setq left-fringe-width 10)
(setq window-divider-default-bottom-width 4)
(setq window-divider-default-right-width 4)
(window-divider-mode 1)

;; TODO: check because this function has problem whem jump to total number of digits
;; e.g. when the buffer goes to 100 to 99 (erase one line) or the oposite.
(defun my-update-line-number-width ()
  "Adjust the `display-line-numbers-width` to the max rows in buffer."
  (setq-local display-line-numbers-width
              (max 2 (length (number-to-string (line-number-at-pos (point-max)))))))

(add-hook 'find-file-hook #'my-update-line-number-width)
(add-hook 'after-change-major-mode-hook #'my-update-line-number-width)
(add-hook 'after-save-hook #'my-update-line-number-width)

;; scroll of window
(scroll-bar-mode -1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

(setq ring-bell-function 'ignore)
;;(setq inhibit-startup-message t)
;;(setq initial-scratch-message "")

;; Use "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; backups and auto save
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq make-backup-files t)
(setq auto-save-default t)

;; highlight cursor
(setq visible-cursor nil)

;; Melhora o comportamento do buffer (mais preditivo ao navegar entre buffers)
;;(setq switch-to-buffer-preserve-window-point 'already-displayed)

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

;; IMPROVED GENERAL ASPECT
;; Definição das cores do cursor
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
;; TODO: configure this package
;; (use-package indent-bars
;;   :config
;;   (setq
;;    indent-bars-color '(highlight :face-bg t :blend 0.2)
;;    indent-bars-pattern "."
;;    indent-bars-width-frac 0.1
;;    indent-bars-pad-frac 0.1
;;    indent-bars-zigzag nil
;;    indent-bars-color-by-depth nil
;;    indent-bars-highlight-current-depth nil
;;    indent-bars-display-on-blank-lines nil))


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
  (doom-modeline-height 20))

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

;; IMPROVED NAVIGATION EXPERIENCE
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

;; IMPROVE THE SEARCH/REPLACE SYSTEM
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
;;outro vanubi

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

;; BETTER ADIVISOR SYSTEMS
;; configure the helpful package
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point)))

;; configure the which key
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.0)
  (setq which-key-idle-secondary-delay 0.05))

;; vertigo
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

;; savehist
(use-package savehist
  :init
  (savehist-mode 1))

;; marginalia
(use-package marginalia
  :init
  (marginalia-mode 1))

;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; consult
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


;; IDE FEATURES CONFIGURED HERE...
;; project management
(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/Projects/" "~/Playground/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(setq projectile-generic-command "rg --files --hidden")

;; use consult to help projectile experience
(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(use-package perspective
  :straight t
  ;; :bind
  ;; ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  :init
  (persp-mode))

(use-package persp-projectile
  :straight t
  :after (perspective projectile)
  :bind ("C-c p p" . projectile-persp-switch-project))

;; configs of Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq ls-lisp-ignore-case t)
(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-Alh --group-directories-first --sort=version")
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory))


(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-follow-mode t)
  (setq treemacs-theme 'icons)  ;; Pode ser 'icons' ou 'arrow'
  (setq treemacs-position 'left)  ;; Posicionamento do treemacs (opções: 'left', 'right', 'top', 'bottom')
  (setq treemacs-width 40)  ;; Largura da janela do treemacs
  (setq treemacs-indentation 2)  ;; Indentação dos itens no treemacs
  (setq treemacs-show-hidden-files t)  ;; Mostrar arquivos ocultos (aqueles que começam com ponto)
  (setq treemacs-show-workspace-sidebar t)  ;; Mostrar barra lateral de workspace
  (setq treemacs-persist-file (expand-file-name ".treemacs-workspaces" user-emacs-directory))  ;; Armazenar as configurações de workspace
  (treemacs-resize-icons 15)
    :bind
  ("C-x t t" . treemacs)  ;; Atalho para abrir/fechar o Treemacsq
  ("C-x t d" . treemacs-select-directory)  ;; Abrir Treemacs para um diretório específico
  ("C-x t p" . treemacs-projectile)  ;; Abrir Treemacs com Projectile
  ("C-x t f" . treemacs-find-file))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; add colors to Dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; some of the dired-hacks utilities
(use-package dired-filter
  :after dired
  :bind (:map dired-mode-map
              ;; unbind original ones
              ("/ i g" . nil)
              ("/ A" . nil)
              ("/ D" . nil)
              ("/ L" . nil)
              ("/ S" . nil)
              ;; bind useful groups
              ("/ G" . dired-filter-by-git-ignored)
              ("/ F a" . dired-filter-add-saved-filters)
              ("/ F d" . dired-filter-delete-saved-filters)
              ("/ F l" . dired-filter-load-saved-filters)
              ("/ F s" . dired-filter-save-filters)))

(use-package dired-subtree
  :after dired
    :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/ i n" . dired-narrow)
              ("/ i r" . dired-narrow-regexp)
              ("/ i f" . dired-narrow-fuzzy)))

;; TODO: try and configure these dired hacks
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
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'symbols))


(defun my/setup-lsp-mode ()
  "Basic setup for the lsp-mode."
  (lsp-enable-which-key-integration)
  (flycheck-mode 1)
  ;;(flyspell-prog-mode)
  ;;(yas-minor-mode-on)
  (lsp-diagnostics-mode 1)
  (lsp-completion-mode 1))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  ;; (flycheck-mode 1)
  ;; (flyspell-prog-mode)
  ;; (yas-minor-mode-on)
  ;; (lsp-diagnostics-mode 1)
  ;; (lsp-completion-mode 1))
  :custom
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
  (lsp-completion-provider :capf)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 3 1024 1024)))

;; Python external dependencies (for LSP):
;; - python-lsp-server (pip install 'python-lsp-server[all]')
;; - python-debugpy
(use-package python-mode
  :hook (python-mode . lsp-deferred))


(use-package dap-mode
  :after lsp-mode
  :hook (python-mode . dap-mode)
  :config
  (require 'dap-python))


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

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))


;; HYDRAS!
(use-package hydra)
;; TODO: adjust the colors of hydras to have the proper behavior for the hydras


;; TODO: use this jumps the keybindings like [] () {} to do the jumps (think about it)
;; TODO: review if need these lambda interactive here
;; (defhydra hydra-sp-move (:exit nil)
;;   "Navegate with smartparens"
;;   ("f" (lambda () (interactive) (sp-forward-sexp)) "Avançar sexp (C-M-f)")
;;   ("b" (lambda () (interactive) (sp-backward-sexp)) "Retroceder sexp (C-M-b)")
;;   ("d" (lambda () (interactive) (sp-down-sexp)) "Descer sexp (C-M-d)")
;;   ("a" (lambda () (interactive) (sp-backward-down-sexp)) "Descer sexp (C-M-a)")
;;   ("e" (lambda () (interactive) (sp-up-sexp)) "Subir sexp (C-M-e)")
;;   ("u" (lambda () (interactive) (sp-backward-up-sexp)) "Subir sexp (C-M-u)")
;;   ("n" (lambda () (interactive) (sp-next-sexp)) "Próximo sexp (C-M-n)")
;;   ("p" (lambda () (interactive) (sp-previous-sexp)) "Anterior sexp (C-M-p)")
;;   ("D" (lambda () (interactive) (sp-beginning-of-sexp)) "Início do sexp (C-S-d)")
;;   ("A" (lambda () (interactive) (sp-end-of-sexp)) "Fim do sexp (C-S-a)")
;;   ;; TODO: Você pode adicionar os comandos que faltam aqui, se desejar, como:
;;   ;; ("N" (lambda () (interactive) (sp-beginning-of-next-sexp)) "Início do próximo sexp")
;;   ;; ("P" (lambda () (interactive) (sp-beginning-of-previous-sexp)) "Início do sexp anterior")
;;   ;; ("<" (lambda () (interactive) (sp-end-of-previous-sexp)) "Fim do sexp anterior")
;;   ;; (">" (lambda () (interactive) (sp-end-of-next-sexp)) "Fim do próximo sexp")
;;   ("q" nil "quit" :exit t :color blue))
;; ;;(global-set-key (kbd "C-c n") 'hydra-sp-nav/body) ;; Define a tecla de prefixo para a Hydra (C-c s n)


(defhydra hydra-text-zoom (:color pink :timeout 4)
  "Scale text font"
  ("i" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :color blue))
;;(global-set-key (kbd "C-c a") 'hydra-text-zoom/body)


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
;;(global-set-key (kbd "C-c v") 'hydra-window-scroll/body)


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
;;(global-set-key (kbd "C-c w") 'hydra-window-move/body)


;; MODAL EDITIONS
(use-package general)

  ;; TODO: create another general group for these (maybe)
  ;;("a" consult-org-agenda)
  ;;("h" consult-org-heading)
  ;;("e" consult-compile-error)
  ;;("c" consult-mode-command)
  ;;("x" consult-history)


;; TODO: create hydras for these functions
;; identation
;; folding
;; moving between symbols
;; move line or region to line X or above/below line

(defun my-insert-backslash ()
  "Insert a backslash (`\\`)."
  (interactive)
  (insert "\\"))

(general-create-definer my/leader-key
  :keymaps 'override ;; Garante que o atalho funcione globalmente
  :prefix "\\" ;; Defina a leader key como a contra barra
  :global-prefix "C-\\") ;; Alternativa para teclados sem tecla "SPC"
(my/leader-key
  ;; commands to execute
  "e" '(:ignore t :which-key "execute")
  "e x" 'execute-extended-command
  "e a" 'embark-act
  "e b" 'embark-bindings
  "e e" 'eval-buffer
  "e R" 'restart-emacs
  "e Q" 'save-buffers-kill-terminal
  "e g" 'magit
  
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
  "g B" 'consult-bookmark
  ;; todo jump
  "g T" '(:ignore t :which-key "todo")
  "g T t" 'consult-todo
  "g T p" 'consult-todo-project
  "g T a" 'consult-todo-all
  "g T d" 'consult-todo-dir
  
  ;; search and replace
  "s" '(:ignore t :which-key "search/replace")
  "s g" 'consult-ripgrep
  ;; TODO: remove it and use ripgrep directly
  ;;"s d" 'deadgrep
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
  "w o" 'other-window
  "w d" 'delete-window
  "w D" 'delete-other-windows

  ;; deal with files
  "f" '(:ignore t :which-key "files/dir")
  "f d" 'consult-dir
  "f s" 'find-file
  "f f" 'consult-fd    ;; find file with fd
  "f F" 'consult-find
  "f r" 'consult-recent-file

  ;; deal with buffer
  "b" '(:ignore t :which-key "buffers")
  "b s" 'save-buffer
  "b b" 'switch-to-buffer
  "b B" 'consult-buffer
  "b p" 'consult-project-buffer
  "b k" 'kill-buffer
  "b K" 'kill-this-buffer
 
  ;; manage keybindings for the project
  "p" '(:ignore t :which-key "project")
  "p m" '(:ignore t :which-key "management")
  "p m t" 'treemacs          ;; directories in a sidebar
  "p m T" 'treemacs-projectile
  "p m d" 'projectile-dired
  "p m o" 'projectile-switch-open-project
  "p m O" 'consult-projectile-switch-project
  ;; TODO: configure also the shell here ...
  ;; "p t" 'projectile-vterm
  ;; "p T" 'projectile-shell
  ;; project workspaces (perspectives)
  "p w" '(:ignore t :which-key "workspaces")
  "p w c" 'persp-switch
  "p w i" 'persp-ibuffer
  "p w k" 'persp-kill
  "p w s" 'persp-state-save
  "p w l" 'persp-state-load
  "p w r" 'persp-state-restore
  "p w o" 'projectile-persp-switch-project
  ;; projectile search and replace
  "p s" '(:ignore t :which-key "search")
  "p s f" 'consult-projectile-find-file
  "p s d" 'consult-projectile-find-dir
  "p s t" 'projectile-find-test-file
  "p s y" 'projectile-find-references
  "p s g" 'projectile-ripgrep
  "p s r" 'projectile-replace
  "p s R" 'projectile-replace-regexp
  ;; buffers in this project
  "p b" '(:ignore t :which-key "buffers")
  "p b s" 'projectile-save-project-buffers
  "p b b" 'consult-projectile-switch-to-buffer
  "p b r" 'consult-projectile-recentf
  "p b i" 'projectile-ibuffer
  ;; execution commands
  "p x" '(:ignore t :which-key "execute")
  "p x C" 'projectile-configure-project
  "p x c" 'projectile-compile-project
  "p x t" 'projectile-test-project
  "p x r" 'projectile-run-project
  "p x P" 'projectile-package-project
  "p x I" 'projectile-install-project
   
  ;; base text operations
  "y" 'consult-yasnippet
  "Y" 'yas-expand
  "\\" 'my-insert-backslash
  "-" 'pulsar-pulse-line
  ";" 'comment-line
  "z" 'undo
  "Z" 'undo-redo
  "c" 'kill-ring-save ;; copy
  "x" 'kill-region ;; cut region
  "X" 'kill-whole-line
  "v" 'yank ;; paste
  "V" 'consult-yank-replace ;; consult available paste list
  
  ;; TODO: add entry for the visual mode (ryo)
  ;; TODO: put the flycheck commands here
  
  )


;; TODO: change this to work as a selection (visual) mode only (OR this could be only a hydra)
;; check for functionalities in evil, vim, spacemacs (visual mode), and meow helix/kakoune
;; to do rich selections
(use-package ryo-modal
  :commands ryo-modal-mode
  :bind ("C-c SPC" . ryo-modal-mode) ;; TODO change to a better keybiind (maybe ESC)
  :config
  (setq ryo-modal-cursor-color "peach puff")
  (setq ryo-modal-cursor-type 'box)
  (defun my-show-ryo-keymap ()
    "Show the current ryo-modal-mode keybindings in a which-key popup."
    (interactive)
    (which-key-show-keymap 'ryo-modal-mode-map))
  
  (ryo-modal-keys
   ("," ryo-modal-repeat)
   ("q" ryo-modal-mode)
   ;; move one position
   ("j" backward-char)
   ("l" forward-char)
   ("k" next-line)
   ("i" previous-line)
   ;; move jump-like
   ("u" backward-word)
   ("o" forward-word)
   ("U" sp-backward-symbol)
   ("O" sp-forward-symbol)
   ("J" beginning-of-line)
   ("L" end-of-line)
   ("I" beginning-of-buffer)
   ("K" end-of-buffer)
   ;; TODO: review these jumps
   ;; TODO: add p to call hydra to jump with smartparens
   ("[" sp-beginning-of-previous-sexp)
   ("]" sp-beginning-of-next-sexp)
   ("{" sp-end-of-previous-sexp)
   ("}" sp-end-of-next-sexp)

   ;; TODO: still available keywords
   ;; Q wW eE rR tT pP |
   ;; aA sS Ff gG H :; '"
   ;; xX cC vV bB nN M ,< .> /?
   
   ;; base command section
   ("ESC" keyboard-quit)
   ("-" pulsar-pulse-line)
   ;; undo/redo commands
   ("z" undo)
   ("Z" undo-redo)
   ;; start a selection (region)
   ("m" set-mark-command)
   ;; TODO: add M to call hydra to advanced selection "submode"
   ;; (e.g. select current line, backward, forward, multicursor, regex, etc)
   ;; basic copy/cut/paste commands (kill/yank)
   ("c" kill-ring-save) ;; copy
   ;; TODO: add H for advanced the kill (hydra) advanced mode
   ;;  e.g. word, line paragraph, buffer, etc.
   ("C" kill-region) ;; cut region
   ("d" kill-whole-line) ;; cut line
   ("v" yank) ;; paste
   ("V" consult-yank-replace))


  (ryo-modal-keys
   ;; First argument to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9")))

