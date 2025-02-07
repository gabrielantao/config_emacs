;; basic configs
(tool-bar-mode -1)
(menu-bar-mode -1)

;; show the positions and line highlith
(setq column-number-mode t)
(setq line-number-mode t)
(global-display-line-numbers-mode t)
(global-hl-line-mode 0)
(setq left-fringe-width 10)


(defun my-update-line-number-width ()
  "Adjust the `display-line-numbers-width` to the max rows in buffer."
  (setq-local display-line-numbers-width
              (max 2 (length (number-to-string (line-number-at-pos (point-max)))))))

(add-hook 'find-file-hook #'my-update-line-number-width)
(add-hook 'after-change-major-mode-hook #'my-update-line-number-width)
(add-hook 'after-save-hook #'my-update-line-number-width)

(global-display-line-numbers-mode 1)

;; configure the parentesis-like closing and marks
;; TODO: make sure this config for the mismatch works right with lsp servers
(use-package paren
  :ensure nil
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1)
  (custom-set-faces
   '(show-paren-match ((t (:background "#44475a" :foreground "#f8f8f2" :weight bold))))
   '(show-paren-mismatch ((t (:background "red" :foreground "white" :weight bold))))))

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
(show-paren-mode 1)
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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; themes and icons
(straight-use-package 'ef-themes)
(straight-use-package 'catppuccin-theme)
(straight-use-package 'all-the-icons)
(straight-use-package 'nerd-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-moonlight t))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)
(setq-default line-spacing 0.3)

;; configure the boon mode and use the qwert keybinds
;; (use-package boon
;;   :config
;;   (boon-mode 1))
;; TODO: move these configures to here and annotated better (Org)
;;(require 'boon-qwerty)
;;(use-package hydra)

;; (defhydra hydra-move (:exit nil)
;;   "Movimentação do Cursor"
;;   ("h" backward-char "← Esquerda")
;;   ("l" forward-char "→ Direita")
;;   ("k" previous-line "↑ Cima")
;;   ("j" next-line "↓ Baixo")
;;   ("q" nil "Sair" :exit t))
;; (global-set-key (kbd "C-c m") 'hydra-move/body)

;; (use-package modalka
;;   :config
;;   (modalka-global-mode nil))
;; ;;(modalka-define-key (kbd "m") "C-c m")


;; fast movement ace-window
(use-package ace-window
  :bind ("M-o" . ace-window))

;; configure the avy to jump
(use-package avy
  :bind ("M-s" . avy-goto-char))

;; configure the doom modelinew
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 20))

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
         ("M-g g" . consult-goto-line)       ;; Ir para uma linha específica
         ("M-g i" . consult-imenu)           ;; Navegar por seções do código
         ("M-g o" . consult-outline))         ;; Outline para documentos estruturados
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


;; IDE features
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)

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
(setq lsp-diagnostics-provider :flycheck) 

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

