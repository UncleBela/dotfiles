(setq package-archives
	  '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
		("MELPA"        . "https://melpa.org/packages/")
		("ORG"          . "https://orgmode.org/elpa/")
		("MELPA Stable" . "https://stable.melpa.org/packages/")
		("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
	  package-archive-priorities
	  '(("GNU ELPA"     . 20)
		("MELPA"        . 15)
		("ORG"          . 10)
		("MELPA Stable" . 5)
		("nongnu"       . 0)))

(package-initialize)

;; Org related
;; This sets descriptive links,
(setq org-descriptive-links t)
(global-set-key (kbd "C-c o") 'org-toggle-link-display)
(global-set-key (kbd "C-c l") 'org-open-at-point)

;; Disabling these
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(menu-bar-mode     -1)
(blink-cursor-mode -1)
(electric-pair-mode 1)

;; Setting variables
(setq org-latex-toc-command "\\clearpage  \\tableofcontents \\clearpage")
(setq disabled-command-function nil)

;; Keybinds
(defmacro defkeys (mapname &rest body)
  `(let ((defs '(,@body)))
     (while defs
       (define-key
        ,mapname
        (if (vectorp (car defs))
            (car defs)
          (read-kbd-macro (car defs)))
        (if (or (listp (cadr defs)) (functionp (cadr defs)))
            (cadr defs)
          (if `(keymapp (bound-and-true-p ,(cadr defs)))
              (eval (cadr defs)))))
       (setq defs (cddr defs)))))

(global-set-key (kbd "<f5>") (lambda () (interactive)
                               (find-file "~/.emacs.d/init.el")
                               (message "Opening %s..." (buffer-name))))
(global-set-key (kbd "<f6>") (lambda () (interactive)
                               (calfw-open-calendar-buffer)
                               (message "Opening Org Calendar.. ." (buffer-name))))
(global-set-key (kbd "<f7>") (lambda () (interactive)
                               (find-file "/mnt/usb/Notes")
                               (message "Opening %s..." (buffer-name))))

(defkeys global-map
;;		 [f5] a
		 )

;; Variables
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Stop creating ~ and # files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq make-backup-files nil) ; stop creating ~ files
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Opening st terminal FROM Emacs
(defun open-terminal-in-current-directory ()
  "Open a terminal in the current directory using `st` and also disown it."
  (interactive)
  (let ((current-directory (expand-file-name default-directory)))
    (start-process-shell-command
     "st" nil
     (concat "setsid st -e sh -c 'cd " current-directory " && exec $SHELL'"))))
(global-set-key (kbd "C-c t") 'open-terminal-in-current-directory)

;; Loading custom packages & customizing
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-calfw/")
(require 'calfw-org)
(setq org-agenda-files '("/mnt/drives/windowsdrive/Projects/OrgNotes/1 - Notes/calendar/people_days.org"
))

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'load-path "~/.emacs.d/plugins/visual-fill-column/")
(require 'visual-fill-column)

(add-to-list 'load-path "~/.emacs.d/plugins/writeroom-mode/")
(require 'writeroom-mode)

;; Modus Themes
(use-package modus-themes
  :ensure t
  :bind
  (("<f12>" . modus-themes-rotate)
   ("C-<f12>" . modus-themes-select)
   ("M-<f12>" . modus-themes-load-random))
  :config
  (modus-themes-load-theme 'modus-vivendi))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(julia-mode magit modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
