(defun my-org-html-toc (depth info &optional scope)
  "Custom Table of Contents title in Hungarian."
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat "<div id=\"text-table-of-contents\" role=\"doc-toc\">"
                         (org-html--toc-text toc-entries)
                         "</div>\n")))
        (if scope toc
          (let ((outer-tag (if (org-html--html5-fancy-p info)
                               "nav"
                             "div")))
            (concat (format "<%s id=\"table-of-contents\" role=\"doc-toc\">\n" outer-tag)
                    (let ((top-level (plist-get info :html-toplevel-hlevel)))
                      (format "<h%d>%s</h%d>\n"
                              top-level
                              "Tartalomjegyzék" ;; YOU CHANGE THIS!
                              top-level))
                    toc
                    (format "</%s>\n" outer-tag))))))))

(advice-add 'org-html-toc :override #'my-org-html-toc)

(setq org-html-postamble "<footer><p>&copy; 2024 Gyenes Béla</p></footer>")
