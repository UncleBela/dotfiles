(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

(require 'ox-texinfo)

(defun ub-lightmode ()
    "Activate Light Mode"
    (interactive)
    (set-face-attribute 'region nil :background "#b3b3b3")
    (set-face-attribute 'highlight nil :background "#dbdbdb")
    (set-face-attribute 'fringe nil :background "white")
    (set-face-background 'hl-line "#dbdbdb")
    (set-face-foreground 'font-lock-function-name-face "MediumVioletRed")
    (set-face-foreground 'font-lock-string-face "red")
    (set-face-foreground 'org-level-1 "DodgerBlue")
    (set-face-foreground 'org-level-2 "LimeGreen")
    (set-face-foreground 'org-level-3 "LightSeaGreen")
    (set-face-attribute 'default nil :foreground "black" :background "white")
    (set-frame-parameter nil 'alpha-background 100)
    (add-to-list 'default-frame-alist '(alpha-background . 100))

    ;; ---

    (set-face-foreground 'org-block "#00007f")
    (set-face-background 'org-block "#f8f8eb")

    (set-face-foreground 'org-block-begin-line "#545454")
    (set-face-background 'org-block-begin-line "#e0dfd1")

    (set-face-foreground 'org-block-end-line "#545454")
    (set-face-background 'org-block-end-line "#e0dfd1")
    
    ;; ----

    (set-face-attribute 'mode-line nil
                        :background "#334455") ; #334455: Dark Blue

    ;; Customize the default mode line faces for your lambda line
    (set-face-attribute 'mode-line nil
                        :background "#d1d1d1"  ; Dark background color
                        :foreground "#000000"  ; Light text color
                        )

  (set-face-attribute 'mode-line-inactive nil
                      :background "#d1d1d1"  ; Dark background color
                      :foreground "#000000") ; Light text color
)

(defun ub-darkmode ()
  "Activate Dark Mode"
  (interactive)
  (set-face-attribute 'region nil :background "#757474")
  (set-face-attribute 'highlight nil :background "#262626")
  (set-face-attribute 'fringe nil :background "black")
  (set-face-background 'hl-line "#454545")
  (set-face-attribute 'default nil :foreground "white" :background "black")
  (set-face-foreground 'org-level-1 "GreenYellow")
  (set-frame-parameter nil 'alpha-background 100)
  (add-to-list 'default-frame-alist '(alpha-background . 100))

  (set-face-attribute 'mode-line nil
                      :background "#000000"  ; Dark background color
                      :foreground "#ffffff"  ; Light text color
                      )


  (set-face-attribute 'mode-line-inactive nil
                      :background "#000000"  ; Dark background color
                      :foreground "#ffffff") ; Light text color

  ;; ---------x

  (set-face-foreground 'org-block "#ffffff")
  (set-face-background 'org-block "#0f0f0f")
  (set-face-foreground 'org-block-begin-line "#146dfc")
  (set-face-background 'org-block-begin-line "#141414")
  (set-face-foreground 'org-block-end-line "#146dfc")
  (set-face-background 'org-block-end-line "#141414")


  ;; --------x

  (set-face-attribute 'mode-line nil
                      :background "#334455") ; #334455: Dark Blue

  ;; Customize the default mode line faces for your lambda line
  (set-face-attribute 'mode-line nil
                      :background "#141414"  ; Dark background color
                      :foreground "#ffffff"  ; Light text color
                      )

(set-face-attribute 'mode-line-inactive nil
                    :background "#141414"  ; Dark background color
                    :foreground "#ffffff") ; Light text color
)

(defun ub-tp ()
  (interactive)
  (set-frame-parameter nil 'alpha-background 80)
  (add-to-list 'default-frame-alist '(alpha-background . 80))    
  )

(global-set-key (kbd "M-j") 'copy-line-down)
(global-set-key (kbd "M-k") 'copy-line-up)
(global-set-key (kbd "C-j") 'move-line-down)
(global-set-key (kbd "C-k") 'move-line-up)
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-u") 'undo-redo)
(global-set-key (kbd "C-x u") 'universal-argument)

(global-set-key (kbd "C-c q") 'indent-region)
(global-set-key (kbd "C-c c") 'comment-dwim)
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c r") 'replace-regexp)  

(global-set-key (kbd "C-c l") 'org-open-at-point)

(global-hl-line-mode 1)

(set-face-attribute 'default nil :font "DejaVu Sans Mono-18")

;;;; Light Mode
(ub-lightmode)

;; Dark Mode
;; (ub-darkmode)

;; Transparent Mode
;; (ub-tp)

(set-face-foreground 'line-number-current-line "green") 

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (set-frame-font "DejaVu Sans Mono-18")))

(set-face-attribute 'font-lock-comment-face nil :foreground "#757575")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#F92672")
(set-face-attribute 'font-lock-type-face nil :foreground "#66D9EF")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#FD971F")
(set-face-attribute 'font-lock-constant-face nil :foreground "#AE81FF")

(use-package lambda-line
    :straight (:type git :host github :repo "lambda-emacs/lambda-line") 
    :custom
    (lambda-line-icon-time t) ;; requires ClockFace font (see below)
    (lambda-line-clockface-update-fontset "⏰") ;; set clock icon
    (lambda-line-position 'bottom) ;; Set position of status-line 
    (lambda-line-abbrev t) ;; abbreviate major modes
    (lambda-line-hspace "  ")  ;; add some cushion
    (lambda-line-prefix t) ;; use a prefix symbol
    (lambda-line-prefix-padding nil) ;; no extra space for prefix 
    (lambda-line-status-invert nil)  ;; no invert colors
    (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
    (lambda-line-gui-mod-symbol " ⬤") 
    (lambda-line-gui-rw-symbol  " ◯") 
    (lambda-line-space-top +.20)  ;; padding on top and bottom of line
    (lambda-line-space-bottom -.20)
    (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
)

      (lambda-line-mode 1)  ; Enable lambda-line mode

      (setq visible-bell nil)  ; Turn off visual bell
      (setq ring-bell-function 'ignore)  ; Ignore the bell function

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((bash-mode . lsp)
         (python-mode . lsp)
         (julia-mode . lsp)
         (html-mode . lsp)
         (css-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (sh-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :capf)  ; Common setting for completion
  (lsp-clients-clangd-executable "/usr/bin/clangd"))

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

(use-package magit :ensure t)

(use-package org :ensure t)

(setq org-startup-folded t)

(setq org-adapt-indentation t)

(setq org-pretty-entities t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-descriptive-links t)
(global-set-key (kbd "C-c o") 'org-toggle-link-display)

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

(unless (package-installed-p 'counsel)
(package-refresh-contents)
(package-install 'counsel))

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
(use-package nerd-icons :ensure t)

(use-package all-the-icons-dired :ensure t)  
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq lsp-modeline-code-action-fallback-icon "💡")

(use-package writeroom-mode :ensure t)

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

(add-hook 'fireplace-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m <down>" . mc/mark-next-lines)
	 ("C-c m <up>" . mc/mark-previous-lines)
	 ("C-c m n" . mc/mark-next-like-this)
	 ("C-c m p" . mc/mark-previous-like-this)
	 ("C-c m a" . mc/mark-all-like-this)))

(setq mc/cmds-to-run-for-all nil)

(setopt indent-tabs-mode nil)
(setopt tab-width 4)
(setq-default indent-tabs-mode nil)
(setq standard-indent 4)
(setq c-basic-offset 4)

(use-package undo-tree :ensure t)

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

(global-set-key (kbd "C-c C-<left>")  'windmove-left)
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<up>")    'windmove-up)
(global-set-key (kbd "C-c C-<down>")  'windmove-down)

(electric-pair-mode t)

(defun open-terminal-in-current-directory ()
  "Open a terminal in the current directory using `st` and also disown it."
  (interactive)
  (let ((current-directory (expand-file-name default-directory)))
    (start-process-shell-command
     "st" nil
     (concat "setsid st -e sh -c 'cd " current-directory " && exec $SHELL'"))))
(global-set-key (kbd "C-c t") 'open-terminal-in-current-directory)

(global-set-key (kbd "s-r") 'windresize) ; This is nice.

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

(defun my-open-file-in-new-buffer ()
  "Open the file at point in a new buffer."
  (interactive)
  (let ((file (org-element-property :path (org-element-context))))
    (if file
    (find-file file)
  (message "No file at point"))))

(define-key org-mode-map (kbd "C-c C-o") 'my-open-file-in-new-buffer)

(global-unset-key (kbd "C-x C-z"))

(defun ivy-my-yasnippet ()
  "Custom yasnippet selection with ivy from ~/.emacs.d/snippets dir"
  (interactive)
  (let* ((yas-snippet-dirs '("~/.emacs.d/snippets")) ; Default *snippets* directory
         (choices (yas--all-templates (yas--get-snippet-tables)))
         (my-snippets (seq-filter
                       (lambda (template)
                         (let ((file (yas--template-load-file template)))
                           (and file
                                (cl-some (lambda (dir)
                                           (string-prefix-p (expand-file-name dir) (expand-file-name file)))
                                         yas-snippet-dirs))))
                       choices))
         (snippets (mapcar (lambda (template)
                             (cons (yas--template-name template) template))
                           my-snippets)))
    (ivy-read "Snippet: " (mapcar #'car snippets)
              :action (lambda (snippet-name)
                        (let ((template (cdr (assoc snippet-name snippets))))
                          (when template
                            (yas-expand-snippet (yas--template-content template))))))))
(global-set-key (kbd "C-c y") 'ivy-my-yasnippet)

;; Company mode setup
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))  ;; Show suggestions immediately

;; TypeScript specific setup
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Optional: setup lsp-ui for better UI
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t))

(defun my-filter-lsp-warnings (format-string &rest args)
  "Filter out specific lsp-mode warnings."
  (unless (string-match-p "Unknown request method: workspace/diagnostic/refresh" format-string)
    (apply #'message format-string args)))

(advice-add 'lsp-warn :override #'my-filter-lsp-warnings)

(use-package vterm :ensure t)

(use-package julia-mode
  :ensure t)

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)

  :init
  (setenv "JULIA_NUM_THREADS" "8")

  :config
  ;; Set the terminal backend
  (julia-repl-set-terminal-backend 'vterm)

  ;; Keybindings for quickly sending code to the REPL
  (define-key julia-repl-mode-map (kbd "<C-RET>") 'my/julia-repl-send-cell)
  (define-key julia-repl-mode-map (kbd "<M-RET>") 'julia-repl-send-line)
  (define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer))

(use-package lsp-julia
:config
(setq lsp-julia-default-environment "~/.julia/environments/v1.10"))

(add-hook 'julia-mode-hook #'lsp-mode)

(defun insert-navigation-links (html-file prev-html next-html)
  (with-current-buffer (find-file-noselect html-file)
    (goto-char (point-min))
    (when (re-search-forward "</header>" nil t)  ;; Assuming your template has a <header> tag
      (insert (format "<nav><a href='index.html'>Home Page</a> %s %s</nav>"
                      (if prev-html (format "<a href='%s'>Previous Page</a>" prev-html) "")
                      (if next-html (format "<a href='%s'>Next Page</a>" next-html) "")))
      (save-buffer)
      (kill-buffer))))

(defun my-blog-add-navigation ()
  (interactive)
  (let* ((files (directory-files-recursively "/path/to/org-files" "\\.org$"))
         (sorted-files (sort files #'string<)))
    (dolist (index (number-sequence 1 (1- (length sorted-files))))
      (let ((current-file (nth index sorted-files))
            (prev-file (if (> index 0) (nth (1- index) sorted-files) nil))
            (next-file (if (< index (1- (length sorted-files))) (nth (1+ index) sorted-files) nil)))
        (insert-navigation-links (replace-regexp-in-string "\\.org$" ".html" current-file)
                                 (and prev-file (replace-regexp-in-string "\\.org$" ".html" prev-file))
                                 (and next-file (replace-regexp-in-string "\\.org$" ".html" next-file)))))))

(use-package highlight-indent-guides :ensure t)
(setq highlight-indent-guides-auto-enabled nil)

;; Set the method to use character displays
(setq highlight-indent-guides-method 'character)

;; Enable the mode in programming modes and web-mode
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'web-mode-hook 'highlight-indent-guides-mode)

(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-delay 0)
(set-face-foreground 'highlight-indent-guides-character-face "black")
(set-face-foreground 'highlight-indent-guides-top-character-face "dimgray")

(defun my/org-html--format-image-caption (orig-func &rest args)
  (let ((caption (apply orig-func args)))
    (replace-regexp-in-string "Figure:" "Ábra:" caption)))

(advice-add 'org-html--format-caption :around #'my/org-html--format-image-caption)x

(org-babel-tangle-file "init.org" "init.el" "emacs-lisp")
