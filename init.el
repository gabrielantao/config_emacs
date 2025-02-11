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

;; TODO: configure proper the keybindings for consult
;; consult
(use-package consult
  :bind (("M-y" . consult-yank-pop)
         ("M-g -" . consult-goto-line))
  :init
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<"))

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

(use-package corfu
  :init
  (global-corfu-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (global-set-key (kbd "C-SPC") #'completion-at-point)
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



;; HYDRAS!
(use-package hydra)
;; TODO: adjust the colors of hydras to have the proper behavior for the hydras


;; TODO: use this jumps the keybindings like [] () {} to do the jumps (think about it)
;; TODO: review if need these lambda interactive here
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


(defhydra hydra-text-scale (:color pink :timeout 4)
  "Scale text font"
  ("i" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :color blue))
(global-set-key (kbd "C-c a") 'hydra-text-scale/body)

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
(global-set-key (kbd "C-c v") 'hydra-window-scroll/body)


(defhydra hydra-window-nav (:color pink :columns 4)
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
  ("a" split-window-vertically "|| split vertically")
  ("s" split-window-horizontally "== split horizontally")
  ("d" delete-window "delete window")
  ("D" delete-other-windows "delete other windows")
  ("o" other-window "other window")
  ("q" nil "quit"))
(global-set-key (kbd "C-c w") 'hydra-window-nav/body)

(defhydra hydra-ace-jump (:color blue)
  "Ace jump movements"
  ("a" avy-goto-char "goto char")
  ("w" avy-goto-word-1 "goto word")
  ("r" avy-goto-line "- goto row")
  ("t" avy-goto-char-timer "⏱ goto timer")
  ("l" ace-link "@ goto link")
  ("q" nil "quit"))
(global-set-key (kbd "M-g M-a") 'hydra-ace-jump/body)

(defhydra hydra-consult-goto (:hint nil :color blue)
   "
  Goto Navigation:
  [_l_] goto line        [_s_] search term        [_S_] multi search
  [_i_] imenu            [_I_] multi imenu        [_o_] outline
  [_m_] mark             [_M_] global mark        [_B_] bookmark
  [_q_] quit
  "
  ("l" consult-goto-line)
  ("s" consult-line)  ;; search regex term
  ("S" consult-line-multi)
  ("i" consult-imenu)
  ("I" consult-imenu-multi)
  ("o" consult-outline)  ;; travel headers
  ("m" consult-mark)
  ("M" consult-global-mark)
  ("B" consult-bookmark)
  ("q" nil))
(global-set-key (kbd "C-c g") 'hydra-consult-goto/body)


  ;; TODO: move to another hydra for file
  ;; ("f" consult-find) ;; find file
  ;; ("F" consult-fd)
  ;; ("r" consult-recent-file)
  ;; ("p" consult-project-buffer)
  ;; ("r" consult-ripgrep)
  ;;("b" consult-buffer)
  ;;("B" consult-bookmark)

  ;; TODO: create another hydra for these (maybe)
  ;;("a" consult-org-agenda)
  ;;("h" consult-org-heading)
  ;;("e" consult-compile-error)
  ;;("c" consult-mode-command)
  ;;("x" consult-history)


;; TODO: create hydras for these functions
;; search/replace
;; identation
;; folding
;; moving between symbols
;; comment code
;; move line or region to line X or above/below line


;; MODAL EDITIONS
;; TODO change the cursor when normal mode is active to something different (white? hollow?)
;; TODO: change to be active in all open buffers
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
   ;; Q W E rR tT Y pP \|
   ;; A S fF gG ;: '"
   ;; xX cC V B nN M ,< .> /?
   
   ;; base command section
   ("ESC" keyboard-quit)
   ;; TODO: this is a convinience keybind for now
   ;;        think if I could remove it later
   ("e" eval-buffer)
   ("s" save-buffer)
   ("a" hydra-ace-jump/body)
   ("w" hydra-window-nav/body)
   ("g" hydra-consult-goto/body)
   ("v" hydra-window-scroll/body)
   ("b" pulsar-pulse-line) ;; blink
   (";" comment-line)
   ("?" my-show-ryo-keymap)
   ;; undo/redo commands
   ("z" undo)
   ("Z" undo-redo)
   ;; start a selection (region)
   ("m" set-mark-command)
   ;; TODO: add M to call hydra to advanced selection "submode"
   ;; (e.g. select current line, backward, forward, multicursor, regex, etc)
   ;; basic copy/cut/paste commands (kill/yank)
   ("h" kill-ring-save) ;; copy
   ;; TODO: add H for advanced the kill (hydra) advanced mode
   ;;  e.g. word, line paragraph, buffer, etc.
   ("d" kill-region) ;; cut region
   ("D" kill-whole-line) ;; cut line
   ("y" yank) ;; paste
   ("Y" consult-yank-replace))
  
  ;; TODO: add entry here for the search/replace f/r
  
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

