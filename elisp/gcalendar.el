(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id ""
	org-gcal-client-secret ""
	org-gcal-file-alist '(("" .  "~/Dropbox/orgfiles/gcal.org"))))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-fetch) ))

(defun org-gcal--notify (title mes)
  (let ((file (expand-file-name (concat (file-name-directory
                                         (locate-library "org-gcal")) org-gcal-logo)))
        (mes mes)
        (title title))
    (if (eq system-type 'gnu/linux)
        (progn
          (if (not (file-exists-p file))
              (deferred:$
                (deferred:url-retrieve (concat "https://raw.githubusercontent.com/myuhe/org-gcal.el/master/" org-gcal-logo))
                (deferred:nextc it
                  (lambda (buf)
                    (with-current-buffer buf
                      (let ((tmp (substring (buffer-string) (+
                                                             (string-match "\n\n" (buffer-string)) 2))))
                        (erase-buffer)
                        (fundamental-mode)
                        (insert tmp)
                        (write-file file)))
                    (kill-buffer buf)))))
          (alert mes :title title :icon file))
      (alert mes :title title))
    ))
(provide 'gcalendar)
