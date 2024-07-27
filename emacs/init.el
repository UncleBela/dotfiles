(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'ox-texinfo)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(set-face-attribute 'default nil :font "Iosevka-22")
(set-face-attribute 'default nil :foreground "white" :background "black")
(set-face-attribute 'region nil :background "#505050")
(set-face-attribute 'highlight nil :background "#303030")
(set-face-attribute 'fringe nil :background "black")
(set-face-foreground 'line-number-current-line "green") 
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")
(add-hook 'server-after-make-frame-hook
  (lambda ()
  (set-frame-font "Iosevka-22")))

(set-face-attribute 'font-lock-comment-face nil :foreground "#757575")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#A6E22E")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#F92672")
(set-face-attribute 'font-lock-string-face nil :foreground "#E6DB74")
(set-face-attribute 'font-lock-type-face nil :foreground "#66D9EF")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#FD971F")
(set-face-attribute 'font-lock-constant-face nil :foreground "#AE81FF")

(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line") 
  :config
  (setq lambda-line-position 'bottom)  ; Ensure the line is at the bottom

  ;; Customize the default mode line faces for your lambda line
  (set-face-attribute 'mode-line nil
		      :background "#0a0a0a"  ; Dark background color
		      :foreground "#b2b2b2"  ; Light text color
		      :box nil)  ; No border

  (set-face-attribute 'mode-line-inactive nil
		      :background "#0a0a0a"  ; Dark background color
		      :foreground "#b2b2b2"  ; Light text color
		      :box nil)  ; No border
  (lambda-line-mode 1))
(setq visible-bell nil)  ; Disable visual bell entirely
(setq ring-bell-function 'ignore)  ; Ignore the bell function to avoid any bell

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

(unless (package-installed-p 'evil-org)
  (package-refresh-contents)
  (package-install 'evil-org))

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)

;; Undo Redo system
(setq evil-undo-system 'undo-redo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-redo)
(define-key evil-visual-state-map (kbd "C-r") 'undo-redo)
(define-key evil-insert-state-map (kbd "C-r") 'undo-redo)

(use-package lsp-mode
  :ensure t
  :hook ((bash-mode . lsp)
	 (python-mode . lsp)
	 (julia-mode . lsp)
	 (html-mode . lsp)
	 (css-mode . lsp)
	 (js-mode . lsp)
	 (typescript-mode . lsp))
  :commands lsp)

;; Bash LSP setup
(use-package lsp-mode
  :ensure t
  :hook (sh-mode . lsp))

;; Python LSP setup
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp))))
(setq lsp-python-ms-auto-install-server t)

;; Julia LSP
  (use-package lsp-mode
  :init
  (setq lsp-julia-package-dir nil)
  :hook
  (julia-mode . lsp))

  (use-package julia-mode
  :ensure t
  :mode "\\.jl\\'"
  :hook
  (julia-mode . (lambda ()
		  (require 'lsp-julia)
		  (lsp))))

  (use-package lsp-julia
  :after julia-mode
  :custom
  (lsp-julia-default-environment "~/.julia/environments/v1.10"))  ; Adjust the path to your Julia environment

;; HTML, CSS, JavaScript, and TypeScript LSP setup
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.css?\\'" "\\.js?\\'" "\\.ts?\\'")
  :hook (web-mode . lsp))

(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(setq lsp-clients-clangd-executable "/usr/bin/clangd")

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq lsp-completion-provider :capf)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
:ensure t
:config
(setq company-idle-delay 0.2
  company-minimum-prefix-length 1)
(global-company-mode t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
    :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package org :ensure t)

(setq org-startup-folded t)

(setq org-adapt-indentation t)

(setq org-pretty-entities t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-descriptive-links t)

;; Replace "Table of Contents" text with "Contents"
 (defun replace-toc-title (backend)
  (when (org-export-derived-backend-p backend 'html)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "Table of Contents" nil t)
        (replace-match "Contents")))))

(add-hook 'org-export-before-processing-hook 'replace-toc-title)

(use-package gdscript-mode
  :straight (gdscript-mode
	     :type git
	     :host github
	     :repo "godotengine/emacs-gdscript-mode"))

(unless (package-installed-p 'ivy)
(package-refresh-contents)
(package-install 'ivy))

(unless (package-installed-p 'ivy-rich)
(package-refresh-contents)
(package-install 'ivy-rich))


(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 10)
(setq ivy-display-style 'fancy)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

;; Disabling ido mode to disable jankiness in buffer menus and file menus
(ido-mode -1)

;; Ivy-rich configuration
(require 'ivy-rich)
(ivy-rich-mode 1)

;; Use counsel for better integration
(require 'counsel)
(counsel-mode 1)

(setq ivy-format-function 'ivy-format-function-line)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package all-the-icons :ensure t)

(use-package windresize :ensure t)

(use-package websocket :ensure t)
(use-package simple-httpd :ensure t)
(use-package f :ensure t)
(use-package org-roam-ui :ensure t)
(use-package org-roam
:ensure t
:custom
(org-roam-directory (file-truename "/home/anon/Projects/Personal/org-files/"))
:bind (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n g" . org-roam-graph)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n c" . org-roam-capture)
       ;; Dailies
       ("C-c n j" . org-roam-dailies-capture-today))
:config
;; If you're using a vertical completion framework, you might want a more informative completion interface
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
(org-roam-db-autosync-mode)
;; If using org-roam-protocol
(require 'org-roam-protocol))

(use-package fireplace :ensure t)

(defun my-disable-line-numbers-in-fireplace ()
"Disable line numbers in Fireplace."
(when (eq major-mode 'fireplace-mode)
(display-line-numbers-mode -1)))

(use-package evil-mc :ensure t)
(global-evil-mc-mode t)

(global-set-key (kbd "C-<up>") 'evil-mc-make-cursor-move-prev-line)
(global-set-key (kbd "C-<down>") 'evil-mc-make-cursor-move-next-line)

(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6601")
  :bind
  ("s-m p" . emms)
  ("s-m b" . emms-smart-browse)
  ("s-m r" . emms-player-mpd-update-all-reset-cache)
  ("s-m r" . emms-player-mpd-update-all-reset-cache)


  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("<XF86AudioPlay>" . emms-pause)
  ("<XF86AudioStop>" . emms-stop))

  (defun open-pulsemixer ()
      "Open pulsemixer in st (suckless terminal)."
      (interactive)
      (start-process "st" nil "st" "-e" "pulsemixer"))
  (global-set-key (kbd "s-m v") 'open-pulsemixer)

  (defun my-disable-line-numbers-in-emms-playlist ()
  "Disable line numbers in EMMS playlist."
  (when (eq major-mode 'emms-playlist-mode)
  (display-line-numbers-mode -1)))

  (add-hook 'emms-playlist-mode-hook 'my-disable-line-numbers-in-emms-playlist)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
  (defun toggle-line-numbering-type ()
    "Toggle between relative and absolute line numbering."
    (interactive)
    (setq display-line-numbers-type
	  (if (eq display-line-numbers-type 'relative)
	      'absolute
	      'relative))
    (global-display-line-numbers-mode -1) ; Turn off line numbering
    (global-display-line-numbers-mode 1)) ; Turn it back on
  (global-set-key (kbd "M-g") 'toggle-line-numbering-type)

(defun copy-line-up ()
  "Copy the current line and paste it above the current line, and remain on the copied line."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    ;; Copy and paste the line immediately below
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (forward-line -1)))

  (defun copy-line-down()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

  (defun move-line-up ()
  "Move the current line up by one."
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

  (defun move-line-down ()
  "Move the current line down by one."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key (kbd "M-j") 'copy-line-down)
(global-set-key (kbd "M-k") 'copy-line-up)
(global-set-key (kbd "C-j") 'move-line-down)
(global-set-key (kbd "C-k") 'move-line-up)
(global-set-key (kbd "C-c C-v") 'eshell)
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-c C-<left>")  'windmove-left)
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<up>")    'windmove-up)
(global-set-key (kbd "C-c C-<down>")  'windmove-down)

(electric-pair-mode t)

(defun open-terminal-in-current-directory ()
    "Open a terminal in the current directory using `st`."
    (interactive)
    (let ((current-directory (expand-file-name default-directory)))
    (start-process "st" nil "st" "-e" "sh" "-c" (concat "cd " current-directory " && exec $SHELL"))))
(global-set-key (kbd "C-c t") 'open-terminal-in-current-directory)

(global-set-key (kbd "s-r") 'windresize)

(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
(setq make-backup-files nil) ; stop creating ~ files

(setq display-buffer-alist
      '((".*" (display-buffer-same-window))))

(global-set-key (kbd "<f6>") (lambda () (interactive)
(find-file "/home/anon/Projects/Personal/org-files/encyclopedia/encyclopedia.org")
(message: "Opened %s" (buffer-name))))

(global-set-key (kbd "<f5>") (lambda () (interactive)
(find-file "/home/anon/.emacs.d/init.org")
(message: "Opened %s" (buffer-name))))

(setq Info-default-directory-list
(append '("/usr/share/info")
Info-default-directory-list
'("~/.emacs.d/info")))

(org-babel-tangle-file "init.org" "init.el" "emacs-lisp")
