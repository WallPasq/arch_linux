;; Ajustes na inicialização do Emacs
(setq inhibit-startup-message t)	; Remove a mensagem de boas-vindas
(setq-default tab-width 2)				; Ajusta o tamanho da identação
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

;; Melhora a rolagem
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))	; Uma linha por vez
(setq mouse-wheel-progressive-speed nil)						; Não acelear a rolagem
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
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
												 ("elpa" . "https://elpa.gnu.org/packages/")
												 ("melpa" . "https://melpa.org/packages/")
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

;; Define o tema Dracula como padrão
(use-package dracula-theme)
(load-theme 'dracula t)

;; Alerta quando algum comando gráfico for executado
(setq visible-bell t)

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

;; Configurações padrão do use-package (vem com a instalação do mesmo)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(general dracula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
