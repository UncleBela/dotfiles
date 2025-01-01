;;; Uncle Béla's Emacs config
;;; Author: Uncle Béla

(setq debug-on-error nil) ;; Change to t, when in doubt

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

(use-package "jupyter" :ensure t)

(require 'ob-jupyter)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((jupyter . t)  ;; Enable Jupyter support
   (emacs-lisp . t)
   (python . t)
   (julia . t)))  ;; Enable Julia language support

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
(setq writeroom-fullscreen-effect nil)

(use-package windresize :ensure t)

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
(defun toggle-line-numbering-type ()
  "Toggle between relative and absolute line numbering."
  (interactive)
  (setq display-line-numbers-type
        (if (eq display-line-numbers-type 'relative)
            'absolute
          'relative))
  (global-display-line-numbers-mode -1) ; Turn off
  (global-display-line-numbers-mode 1)) ; Turn on
(global-set-key (kbd "C-x a q") 'toggle-line-numbering-type)
(global-set-key (kbd "C-x a w") 'display-line-numbers-mode)

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
(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq display-buffer-alist
      '((".*" (display-buffer-same-window))))

(global-set-key (kbd "<f6>") (lambda () (interactive)
                               (find-file "/home/anon/Projects/Personal/org-files/index.org")
                               (message "Opened %s" (buffer-name))))

(global-set-key (kbd "<f5>") (lambda () (interactive)
                               (find-file "~/.emacs.d/init.org")
                               (message "Opened %s" (buffer-name))))

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
  (let* ((yas-snippet-dirs '("~/.emacs.d/snippets")) ; Default *snippets*
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

(defun gybfuns/pretty-funs ()
  (writeroom-mode 1)
  (display-line-numbers-mode -1)
  (visual-line-mode 1))

(defun gybfuns/dired-funs ()
  (dired-hide-details-mode t)
  (dired-omit-mode t))

(add-hook 'org-mode-hook 'gybfuns/pretty-funs)
(add-hook 'elfeed-new-entry-hook 'gybfuns/pretty-funs)

(add-hook 'dired-mode-hook 'gybfuns/pretty-funs)  
(add-hook 'dired-mode-hook 'gybfuns/dired-funs)

(add-hook 'html-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            (emmet-mode 1)
            (sgml-electric-tag-pair-mode 1)
            ))

(global-unset-key (kbd "C-<up>"))
(global-set-key (kbd "C-<up>") 'emmet-expand-line)
(add-hook 'html-mode
          (lambda ()
            (local-set-key (kbd "C-<up>") 'emmet-expand-line)
            ))

          (add-hook 'css-mode-hook
                    (lambda ()
                      (display-line-numbers-mode 1)
                      ))

          (add-hook 'c-mode-hook
                    (lambda ()
                      (display-line-numbers-mode 1)
                      ))
          (add-hook 'prog-mode-hook
                    (lambda ()
                      (display-line-numbers-mode 1)
                      ))

(setq org-latex-text-markup-alist
      '((bold . "\\textbf{%s}")
        (italic . "\\emph{%s}")
        (underline . "\\underline{%s}")
        (verbatim . "\\texttt{%s}")
        (strike-through . "\\sout{%s}")
        (code . verb)))

(setq org-export-with-LaTeX t)

(defun compile-xelatex ()
  "Compile current .tex file with xelatex in the background."
  (interactive)
  (let ((file (shell-quote-argument (buffer-file-name))))
    (start-process "xelatex-process" "*xelatex-output*" "xelatex" file)))

(global-set-key (kbd "<f1> <f3>") 'compile-xelatex)

(use-package helm :ensure t)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value. Magnificent!"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%s" value))))

(defun load-directory (dir)
  "This function loads all elisp files."
  (dolist (file (directory-files dir t "\\.el$"))
    (load file)))

(load-directory "~/.emacs.d/gyb-lisp/")
