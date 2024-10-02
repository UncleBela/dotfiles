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
