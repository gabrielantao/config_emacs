;; basic configs
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; show the positions and line highlith
(setq column-number-mode t)
(global-display-line-numbers-mode t)
(global-hl-line-mode 0)
(setq left-fringe-width 10)

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
;; themes and icons
(straight-use-package 'ef-themes)
(straight-use-package 'catppuccin-theme)
(straight-use-package 'all-the-icons)
(straight-use-package 'nerd-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-moonlight t))

(set-face-attribute 'cursor nil :background "magenta")
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)
(setq-default line-spacing 0.3)

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

;; IMPROVED NAVIGATION EXPERIENCE
;; configure the to jump with avy
(use-package avy
  :straight t
  :bind (("M-g a" . avy-goto-char)
         ("M-g l" . avy-goto-line)
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

;; TODO: DEPRACATED remove because is already using a hydra for windmove(ments)
;; fast movement ace-window
;; (use-package ace-window
;;   :bind ("M-g o" . ace-window))

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


;; BETTER ADIVISOR SYSTEMS
;; confugure the helpful package
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)   ;; Ajuda detalhada sobre funções
   ("C-h v" . helpful-variable)   ;; Ajuda detalhada sobre variáveis
   ("C-h k" . helpful-key)        ;; Ajuda detalhada sobre teclas
   ("C-h x" . helpful-command)    ;; Ajuda sobre comandos interativos
   ("C-c C-d" . helpful-at-point)) ;; Ajuda do que está sob o cursor
  :config
  ;; Faz os comandos `describe-*` padrões chamarem o Helpful
  (setq apropos-do-all t)
  (advice-add 'describe-function :override #'helpful-callable)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-key :override #'helpful-key))

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

;; TODO: configure proper the keybindings for consult
;; consult
(use-package consult
  :bind (("M-y" . consult-yank-pop)          ;; Histórico de copiar/colar (kill-ring)
         ("M-g -" . consult-goto-line))       ;; Ir para uma linha específica
         ;;("M-g i" . consult-imenu)           ;; Navegar por seções do código
         ;;("M-g o" . consult-outline))         ;; Outline para documentos estruturados
         ;;("M-s r" . consult-ripgrep)         ;; Busca por palavras no projeto (ripgrep)
         ;;("M-s l" . consult-line-multi)      ;; Busca em múltiplos buffers
         ;;("M-s g" . consult-git-grep)        ;; Busca dentro do repositório Git
         ;;("M-s d" . consult-find)            ;; Encontrar arquivos dentro de diretórios
         ;;("M-s m" . consult-mark)            ;; Navegar entre marcas no buffer
         ;;("M-s k" . consult-kmacro))         ;; Busca em macros gravadas
  :init
  (setq consult-preview-key 'any)           ;; Mostra preview de opções ao navegar
  (setq consult-narrow-key "<"))            ;; Permite filtrar por categoria rapidamente

(use-package embark
  :bind
  (("C-." . embark-act)         ;; Aciona o Embark no contexto atual
   ("C-;" . embark-dwim)        ;; Tenta a ação mais lógica
   ("C-h B" . embark-bindings)) ;; Mostra keybindings ativos no contexto
  :init
  (setq prefix-help-command #'embark-prefix-help-command))  ;; Usa embark no C-h

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))  ;; Preview ao selecionar no embark

(use-package corfu
  :init
  (global-corfu-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (global-set-key (kbd "TAB") #'completion-at-point)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'lsp-completion-at-point))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;; PROJECTE MANAGEMENT
;; ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings))

;; projectile
(use-package projectile
  :init
  (projectile-mode +1)
 ;; :bind (("C-c p f" . projectile-find-file)
 ;;        ("C-c p p" . projectile-switch-project)
 ;;        ("C-c p g" . projectile-grep)
 ;;        ("C-c p s" . projectile-ripgrep))
  :config
  ;;(setq projectile-project-search-path '("~/projects/" "~/code/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(setq projectile-generic-command "rg --files --hidden")


;; IDE features
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-diagnostics-provider :flycheck)
  ;; ;; specific configuration for the LSPs used
  ;; (with-eval-after-load 'pylsp
  ;;   (add-hook 'lsp-pylsp-before-initialize-hook
  ;;             (lambda ()
  ;;               (setq lsp-pylsp-plugins
  ;;                     '(( "pycodestyle" . t)  ;; activate the pycodestyle
  ;;                       ( "pylint" . t)     ;; activate the pylint
  ;;                       ;; TODO: add other plugins here...
  ;;                       )))))
  ;; (add-hook 'python-mode-hook #'lsp-deferred)
  
  ;; configure some keybindings
  ;; (define-key lsp-mode-map (kbd "C-c l i") 'lsp-identifier-definition)
  ;; (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename-symbol)

  ;; CLOSING lsp-mode block here...
  )

;; pip install 'python-lsp-server[all]'
;; OR use a docker image to run the server
(use-package python-mode
  :hook (python-mode . lsp-deferred))
;; TODO: checkout this custom configs here...
;;  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
;;  (dap-python-debugger 'debugpy)
;;  :config
;;  (require 'dap-python))


;; configure the lsp-docker in order to run the LSP servers inside the containers
;; and then do not need to install anything directly in my machine
(use-package lsp-docker)
;; (setq lsp-docker-client-configs
;;       '((:server-id pylsp-docker ;; ID do servidor no Docker
;;          :docker-image-id "emacslsp/lsp-docker-langservers" ;; Imagem Docker
;;          :server-command "pylsp"))) ;; Comando para iniciar o pylsp
;; (lsp-docker-init-clients
;;  :path-mappings '(("/home/gabriel/Projects" . "/projects")) ;; Mapeamento de pastas
;;  :client-packages lsp-docker-client-packages
;;  :client-configs lsp-docker-client-configs)

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; MODAL EDITIONS
;; configure the boon mode and use the qwert keybinds
;; (use-package boon
;;   :config
;;   (boon-mode 1))
;; TODO: move these configures to here and annotated better (Org)
;;(require 'boon-qwerty)
;;(use-package hydra)

;; (use-package modalka
;;   :config
;;   (modalka-mode 1)
;;   ;; Defina seus próprios atalhos aqui
;;   (define-key modalka-mode-map (kbd "C-c C-k") 'kill-line)
;;   (define-key modalka-mode-map (kbd "C-c C-y") 'yank))


;; HYDRAS!
(use-package hydra)
;; (require 'windmove)

(defhydra hydra-sp-move (:exit nil)
  "Navegate with smartparens"
  ("f" (lambda () (interactive) (sp-forward-sexp)) "Avançar sexp (C-M-f)")
  ("b" (lambda () (interactive) (sp-backward-sexp)) "Retroceder sexp (C-M-b)")
  ("d" (lambda () (interactive) (sp-down-sexp)) "Descer sexp (C-M-d)")
  ("a" (lambda () (interactive) (sp-backward-down-sexp)) "Descer sexp (C-M-a)")
  ("e" (lambda () (interactive) (sp-up-sexp)) "Subir sexp (C-M-e)")
  ("u" (lambda () (interactive) (sp-backward-up-sexp)) "Subir sexp (C-M-u)")
  ("n" (lambda () (interactive) (sp-next-sexp)) "Próximo sexp (C-M-n)")
  ("p" (lambda () (interactive) (sp-previous-sexp)) "Anterior sexp (C-M-p)")
  ("D" (lambda () (interactive) (sp-beginning-of-sexp)) "Início do sexp (C-S-d)")
  ("A" (lambda () (interactive) (sp-end-of-sexp)) "Fim do sexp (C-S-a)")
  ;; TODO: Você pode adicionar os comandos que faltam aqui, se desejar, como:
  ;; ("N" (lambda () (interactive) (sp-beginning-of-next-sexp)) "Início do próximo sexp")
  ;; ("P" (lambda () (interactive) (sp-beginning-of-previous-sexp)) "Início do sexp anterior")
  ;; ("<" (lambda () (interactive) (sp-end-of-previous-sexp)) "Fim do sexp anterior")
  ;; (">" (lambda () (interactive) (sp-end-of-next-sexp)) "Fim do próximo sexp")
  ("q" nil "quit" :exit t :color blue))
(global-set-key (kbd "C-c n") 'hydra-sp-nav/body) ;; Define a tecla de prefixo para a Hydra (C-c s n)


(defhydra hydra-window-nav (:color pink :columns 2)
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
  ("a" split-window-vertically "split vertically")
  ("s" split-window-horizontally "split horizontally")
  ("d" delete-window "delete window")
  ("D" delete-other-windows "delete other window")
  ("o" other-window "other window")
  ("q" nil "quit" :color blue))
(global-set-key (kbd "C-c w") 'hydra-window-nav/body)



;; TODO: criar outras hydras para outros movimentos com consult, etc.

