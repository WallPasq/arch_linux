;; -*- lexical-binding: t; -*-

;; Ajustes na inicialização do Emacs
(setq native-comp-async-report-warnings-errors nil  ; Remove Warnings desnecessários
			inhibit-startup-message t											;	Remove a mensagem de boas-vindas
			auto-save-default nil													; Remove o save automático
			column-number-indicator-zero-based nil)     	; Garante que a coluna comece a contar do 1, ao invés do 0
(setq-default tab-width 2														; Ajusta o tamanho da identação
							cursor-type 'bar)				       				; Define o cursor como barra
(blink-cursor-mode 0)									       				; Desabilita o pisca-pisca do cursor
(tool-bar-mode -1)										       				; Remove a toolbar
(tooltip-mode -1) 										       				; Remove as tooltips
(set-fringe-mode 10) 									       				; Ajusta um "respiro" de 10 pixels nas bordas laterais
(menu-bar-mode -1) 										       				; Remove a barra de menu
(scroll-bar-mode -1) 									       				; Remove a barra de rolagem
(display-battery-mode 1)														; Mostra a bateria na modeline
(column-number-mode t)											        ; Insere o número da coluna na modeline	
(repeat-mode t) 							 											; Habilita a repetição atalhos (exemplo, C-x o para trocar de janela)
(kill-buffer "*scratch*") 									        ; Para de criar um buffer scratch

;; Ajustes nas performance de inicialização
	;; Reduz a frequência de coleção de lixo
(setq gc-cons-threshold (* 2 1000 1000))
	;; Para de guardar os arquivos de compilação no .emacs.d
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

	;; Mensagem de tempo de demora para carregar, e quantidade de lixo (em bytes) coletada
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Melhora a rolagem (scroll) do mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))	; Uma linha por vez
			mouse-wheel-progressive-speed nil							; Não acelera a rolagem
			mouse-wheel-follow-mouse 't										; Só rola na janela ativa
			scroll-step 1																	; A rolagem pelo teclado acontece uma linha por vez
			scroll-margin 7)															; Mostra as sete últimas linhas durante a rolagem

;; Ajusta a transparência da janela e maximiza a janela por padrão
(set-frame-parameter (selected-frame) 'alpha' (98 . 98))
	(add-to-list 'default-frame-alist' (alpha . (98 . 98)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
	(add-to-list 'default-frame-alist' (fullscreen . maximized))

;; Muda o lixo transitório  de ~/.emacs para ~/.cache/emacs
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

;; Instala e configura o Straight (gerenciador de pacotes)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

	;; Desabilidade o package.el em favor do straight.el
(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(use-package straight
	:custom (straight-use-package-by-default t))

	;; Inicializa as configurações com base no que está no shell
(use-package exec-path-from-shell
	:init
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))

;; Configurações do ibuffer
(use-package ibuffer
	:bind
	("C-x C-b" . ibuffer))

;; Ibuffer configuration
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1) (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-saved-filter-groups
      '(("default"
				 ("dired" (mode . dired-mode))
				 ("browser" (or
										 (name . "brave")
										 (name . "chrome")
										 (name . "firefox")))
				 ("elisp" (mode . emacs-lisp-mode))
				 ("org" (mode . org-mode))
				 ("python" (mode . python-mode))
				 ("web" (or
								 (mode . html-mode)
								 (mode . mhtml-mode)
								 (mode . css-mode)
								 (mode . js-mode)))
				 ("bash" (mode . sh-mode))
				 ("shell" (or
									 (mode . ansi-term-mode)
									 (mode . eshell-mode)
									 (mode . term-mode)
									 (mode . vterm-mode)
									 (mode . shell-mode)))
				 ("exwm" (mode . exwm-mode))
				 ("dashboard" (mode . dashboard-mode))
				 ("emacs" (name . "^[*].+[*]$"))))
      ibuffer-show-empty-filter-groups nil)

;; Insere o número da linha
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode doc-view-mode which-key-mode telega-chat-mode telega-root-mode)
  "Desabilita o display-line-numbers nos modos listados"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Ativa o display-line-numbers, exceto nos modos listados em `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))
(global-display-line-numbers-mode)

;; Define o tema Dracula como padrão
(use-package dracula-theme)
(load-theme 'dracula t)

;; Alerta quando algum comando não for possível de ser executado
(setq visible-bell t)

;; Instala e configura o EXWM (Window Manager)

	;; Coloca programas para rodar no background
(defun run-in-background (command)
	(let ((command-parts (split-string command "[ ]+")))
		(apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(run-in-background "pasystray")		;; System tray do Pavu Control
(run-in-background "nm-applet")		;; Permite acesso ao Network Manager
(run-in-background "dunst")				;; Programa de notificações
(run-in-background "caffeine")		;; Deixa a tela do computador ligada

	;; Transforma o workspace 0 no primário
(defun exwm-init-hook ()
	(exwm-workspace-switch-create 0))

	;; Renomeia o buffer com a classe dele
(defun exwm-update-class ()
	(exwm-workspace-rename-buffer exwm-class-name))

	;; Renomeia os browsers com seus títulos
(defun exwm-update-title ()
	(pcase exwm-class-name
		("Google-chrome" (exwm-workspace-rename-buffer (format "%s" exwm-title)))
		("firefox" (exwm-workspace-rename-buffer (format "%s" exwm-title)))))

	;; Configurações da Window Manager
(use-package exwm
	:config
	
	;; Criar 5 workspaces por padrão
	(setq exwm-workspace-number 5)

	;; Aplicando as funções criadas anteriormente
	(add-hook 'exwm-update-class-hook #'exwm-update-class)
	(add-hook 'exwm-update-title-hook #'exwm-update-title)
	(add-hook 'exwm-init-hook #'exwm-init-hook)

	;; Define a resolução da tela
	(require 'exwm-randr)
	(setq exwm-randr-workspace-output-plist '(0 "eDP-1"))
	(add-hook 'exwm-randr-screen-change-hook
						(lambda ()
							(start-process-shell-command
							 "xrandr" nil "xrandr --output HDMI-1 --left-of eDP-1 --auto")))
	(setq exwm-randr-workspace-monitor-plist '(1 "HDMI-1" 3 "HDMI-1" 5 "HDMI-1"
																						 7 "HDMI-1" 9 "HDMI-1"))
	(exwm-randr-enable)
	
		;; Define o Wallpaper
	(use-package wallpaper
		:hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
					 (after-init . wallpaper-cycle-mode))
		:custom ((wallpaper-cycle-single t)
						 (wallpaper-cycle-directory "~/Wallpaper/")))
	
	;; Configurações do System tray
	(require 'exwm-systemtray)
	(setq exwm-systemtray-height 16)
	(exwm-systemtray-enable)
	
	;; As keybings abaixo rodarão obrigatoriamente nos buffers do Emacs
	(setq exwm-input-prefix-keys
				'(?\C-x
					?\C-u
					?\C-h
					?\C-w
					?\M-x
					?\M-`
					?\M-&
					?\M-:
					?\C-\M-j
					?\C-\ ))

	;; C-q é habilitado para os atalhos dos softwares além do Emacs
	(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

	;; Define o Windows Espaço para lançar aplicativos
	(exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app)
	
	;; Define atalhos globais para o EXWM
	(setq exwm-input-global-keys
				`(
					([?\s-r] . exwm-reset)
					([?\s-&] . (lambda (command)
										 (interactive (list (read-shell-command "$ ")))
										 (start-process-shell-command command nil command)))
					([?\s-w] . exwm-workspace-switch)
					,@(mapcar (lambda (i)
											`(,(kbd (format "s-%d" i)) .
												(lambda ()
												(interactive)
												(exwm-workspace-switch-create ,i))))
										(number-sequence 0 9))))

	(exwm-enable))

;; Configurações do DE (Desktop Environment)
(use-package desktop-environment
	:after exwm
	:config
	(exwm-input-set-key (kbd "s-.") 'desktop-environment-brightness-increment)
	(exwm-input-set-key (kbd "s->") 'desktop-environment-brightness-increment-slowly)
	(exwm-input-set-key (kbd "s-,") 'desktop-environment-brightness-decrement)
	(exwm-input-set-key (kbd "s-<") 'desktop-environment-brightness-decrement-slowly)
	(exwm-input-set-key (kbd "s-m") 'desktop-environment-toggle-music)
	(exwm-input-set-key (kbd "s-s") 'desktop-environment-screenshot)
	(exwm-input-set-key (kbd "s-S") 'desktop-environment-screenshot-part)
	(exwm-input-set-key (kbd "s-l") 'desktop-environment-screenlock-command)
  (exwm-input-set-key (kbd "M-]") 'exwm-layout-shrink-window-horizontally)
  (exwm-input-set-key (kbd "M-[") 'exwm-layout-enlarge-window-horizontally)
  (exwm-input-set-key (kbd "s-]") 'exwm-layout-shrink-window)
  (exwm-input-set-key (kbd "s-[") 'exwm-layout-enlarge-window)
	:custom
	(desktop-environment-brightness-small-increment "1%+")
	(desktop-environment-brightness-small-decrement "1%-")
	(desktop-environment-brightness-normal-increment "5%+")
	(desktop-environment-brightness-normal-decrement "5%-")
	(desktop-environment-volume-small-increment "1%+")
	(desktop-environment-volume-small-decrement "1%-")
	(desktop-environment-volume-normal-increment "5%+")
	(desktop-environment-volume-normal-decrement "5%-")
	:init
	(desktop-environment-mode))

;; Habilita o TRAMP para fazer conexões SSH
(setq tramp-default-method "ssh")

;; Ajusta as cores dos brackets e também faz o pareamento automático deles
(use-package rainbow-delimiters
	:hook ((prog-mode . rainbow-delimiters-mode)
				 (text-mode . rainbow-delimiters-mode)
				 (org-mode . rainbow-delimiters-mode)))

(use-package smartparens
	:init (require 'smartparens-config)
	:hook ((prog-mode . smartparens-mode)
				 (text-mode . smartparens-mode)
         (org-mode . smartparens-mode)))

;; Instala e configura o Pinentry
(use-package pinentry
	:config
	(setq epg-pinentry-mode 'loopback))
(pinentry-start)

;; Habilita para o Emacs reconhecer o Unicode
  ;; Ativa o suporte adequado aos caracteres Unicode
(defun replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))
          
;; Instalação e configuração do which-key (mostra as key bindings disponíveis)
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

  ;; Insere a tecla ESC para cancelar
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Insere as configurações do Evil Mode (E*vim*l) no Emacs
(defun evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  term-mode))
		(add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
	(setq evil-want-keybinding nil
				evil-want-C-i-jump t
				evil-want-integration t
				evil-want-C-u-scroll t
				evil-respect-visual-line-mode t
				evil-move-beyond-eol t)
  :config
  (add-hook 'evil-mode-hook 'evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Define as configurações de movimentação do Emacs como iguais às do Vim
	;; j para próxima linha e k para linha anterior
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
  
  ;; Instala e configura o Evil Collection
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
	:bind ("C-;" . evilnc-comment-or-uncomment-lines))

;; Hydra
(use-package hydra
  :defer 1)

;; Configurações do dashboard
(use-package dashboard
	:config
	(setq dashboard-banner-logo-title "Welcome to Emacs, Wallacy"
				dashboard-startup-banner 'logo
				dashboard-set-init-info t
				dashboard-center-content t
				dashboard-items '((recents . 10)
													(agenda . 15)
													(bookmarks . 5)))
	(dashboard-setup-startup-hook))

;; Habilita data e horário na modeline
(setq display-time-format "%H:%M %d/%m/%Y"
      displat-time-default-load-average nil
      display-time-day-and-date t)
(display-time-mode 1)

;; Instala e ativa o Doom Modeline
(use-package all-the-icons)      
(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (minions-mode-line-lighter ""))

(use-package doom-modeline
  :custom
  (doom-modeline-height 20)
  (doom-modeline-bar-width 5)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-major-mode-icon t))
(doom-modeline-mode 1)

;; Instala e ativa o Treemacs no atalho Ctrl + \
(use-package treemacs
	:bind
	(:map global-map
					("C-\\" . treemacs)
					("C-<tab>" . treemacs-select-window))
	:config
	(setq treemacs-is-never-other-window t))

(use-package treemacs-evil
  :after (treemacs evil))

;; Instala o LSP e o Corfu
(use-package lsp-mode
	:init
	(setq lsp-keymap-prefix "C-c l")
	(defun dotfiles--lsp-deferred-if-supported ()
  "Habilita o `lsp-deferred' somente nos modos em que ele tem suporte."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (lsp-deferred)))
	(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-if-supported)
	:bind
	("C-c o" . lsp-org)
	:hook
	(lsp-mode . lsp-enable-which-key-integration)
	:commands
	(lsp lsp-deferred))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package corfu
	:config
	(setq corfu-popupinfo-delay t)
	:custom
	(corfu-cycle t)                ; Habilita o circuito de `corfu-next/previous'
  (corfu-auto t)                 ; Habilita auto completar
  (corfu-quit-no-match nil)      ; Não sai, mesmo que não tenha nenhuma correspondência
  (corfu-on-exact-match nil)     ; Configura o tratamento de correspondências exatas
  (corfu-scroll-margin 4)        ; Mostra as quatro primeiras opções correspondentes
	:init
  (global-corfu-mode)
	(corfu-popupinfo-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3	; Habilita o ciclo TAB se houver poucos candidatos
				tab-always-indent 'complete))

;; Instala o go-mode para a linguagem Go
(use-package go-mode)

;; Instala o julia-mode e o lsp-julia para a linguagem Julia
(use-package julia-mode)
(use-package lsp-julia)

;; Instala e configura o Vertico
(defun minibuffer-backward-kill (arg)
  "Se o minibuffer está completando o nome do arquivo, deleta a pasta pai, se não deleta a palavra"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Inspirado em https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (delete-word (- arg))))

(use-package vertico
	:init
	(vertico-mode)
	(setq vertico-count 10
				vertico-resize t
				vertico-cycle t)
	:bind
	(:map vertico-map
				("C-j" . vertico-next)
				("C-k" . vertico-previous)
	 :map minibuffer-local-map
				("M-h" . minibuffer-backward-kill)))

;; Instala e configura o Marginalia
(use-package marginalia
	:init
	(marginalia-mode))

;; Instala e configura o Orderless
(use-package orderless
	:init
	(setq completion-styles '(orderless)
				completion-category-defaults nil
				completion-category-overrides '((file (styles . (partial-completion))))))

;; Instala e configura o Consult
(use-package consult
	:bind
	(("C-s" . consult-line)
	 ("C-M-l" . consult-imenu)
	 :map minibuffer-local-map
	 ("C-r" . consult-history))
	:custom
	(completion-in-region-function #'consult-completion-in-region))

(use-package consult-dir
	:bind
	(("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))

;; Instala e configura o app-launcher
(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

;; Configurações do Dired
(use-package dired
	:ensure nil
	:straight nil
	:defer 1
	:commands (dired dired-jump)
	:config
	(setq dired-listing-switches "-agho --group-directories-first"
				dired-omit-files "^\\.[^.].*"
				diret-omit-verbose nil))

(autoload 'dired-omit-mode "dired-x")

(add-hook 'dired-mode-hook
					(lambda ()
						(interactive)
						(dired-omit-mode 1)))

(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook
					(lambda ()
						(interactive)
						(all-the-icons-dired-mode 1)
						(hl-line-mode 1)))

(add-hook 'dired-load-hook
					(lambda ()
						(interactive)
						(dired-collapse)))

(use-package dired-single
	:commands (dired dired-jump)
	:defer t)

(use-package dired-ranger
	:defer t)

(use-package dired-collapse
	:defer t)

	;; Key Bindings
(evil-collection-define-key 'normal 'dired-mode-map
	"h" 'dired-single-up-directory
	"H" 'dired-omit-mode
	"l" 'dired-single-buffer
	"y" 'dired-ranger-copy
	"n" 'dired-ranger-move
	"p" 'dired-ranger-paste)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
