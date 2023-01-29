;; Ajustes na inicialização do Emacs
(setq inhibit-startup-message t)	; Remove a mensagem de boas-vindas
(setq auto-save-default nil)			; Remove o save automático
(setq-default tab-width 2)				; Ajusta o tamanho da identação
(setq-default cursor-type 'bar)		; Define o cursor como barra
(blink-cursor-mode 0)							; Desabilita o pisca-pisca do cursor
(tool-bar-mode -1)								; Remove a toolbar
(tooltip-mode -1) 								; Remove as tooltips
(set-fringe-mode 10) 							; Ajusta um "respiro" de 10 pixels nas bordas laterais
(menu-bar-mode -1) 								; Remove a barra de menu
(scroll-bar-mode -1) 					 		; Remove a barra de rolagem
(kill-buffer "*scratch*") 				; Para de criar um buffer scratch

;; Ajustes nas performance de inicialização
	;; Reduz a frequência de coleção de lixo
(setq gc-cons-threshold (* 2 1000 1000))

	;; Mensagem de tempo de demora para carregar, e quantidade de lixo (em bytes) coletada
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Melhora a rolagem (scroll) do mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))	; Uma linha por vez
(setq mouse-wheel-progressive-speed nil)						; Não acelera a rolagem
(setq mouse-wheel-follow-mouse 't)									; Só rola na janela ativa
(setq scroll-step 1)

;; Ajusta a transparência da janela e maximiza a janela por padrão
(set-frame-parameter (selected-frame) 'alpha' (90 . 90))
	(add-to-list 'default-frame-alist' (alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
	(add-to-list 'default-frame-alist' (fullscreen . maximized))

;; Muda o lixo transitório  de ~/.emacs para ~/.cache/emacs
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

;; Inicializa as fontes do pacote
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
												 ("elpa" . "https://elpa.gnu.org/packages/")
												 ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

	;; Configurações do use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

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

;; Desabilita o mouse no Emacs
(use-package disable-mouse)
(global-disable-mouse-mode)

;; Define o tema Dracula como padrão
(use-package dracula-theme)
(load-theme 'dracula t)

;; Alerta quando algum comando gráfico for executado
(setq visible-bell t)

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
          
;; Instalação e configuração do general.el (key bindings)
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer c-keys
    :prefix "C-c"))

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
	(setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
	(setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-move-beyond-eol t)
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

;; Hydra
(use-package hydra
  :defer 1)

;; Configurações do dashboard
(use-package dashboard
	:config
	(setq dashboard-banner-logo-title "Welcome to Emacs, Wallacy")
	(setq dashboard-startup-banner 'logo)
	(setq dashboard-set-init-info t)
	(setq dashboard-center-content t)
	(setq dashboard-items '((recents . 10)
													(agenda . 15)
													(bookmarks . 5)))
	(dashboard-setup-startup-hook))

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

;; Configurações padrão do use-package (vem com a instalação do mesmo)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("12e12c708b0d968868435a6f1197d6e8e51828338566593a28804644c80f0c03" "9e721e03b78de19b5f5cbdb877de134d862f40a907e96964c4729658d1c6d94b" "21d7f1c3389d76b199fed33989fc7e13139c66e183436894a0f22aba82ff17c6" "7c284f499a1be8fcf465458f5250442ecbb26ce2fd8108abc89b241c93350004" default))
 '(package-selected-packages
	 '(disable-mouse dashboard hydra general dracula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
