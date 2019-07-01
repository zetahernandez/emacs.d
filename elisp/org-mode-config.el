(global-set-key (kbd "C-c c")
                'org-capture)

(setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
			     "~/Dropbox/orgfiles/i.org"
			     "~/Dropbox/orgfiles/schedule.org"))


(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/Dropbox/orgfiles/gcal.org" "Appointments")
	 "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
	("n" "Note" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Notes")
	 "* Note %?\n%T")
	("l" "Link" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	("b" "Blog idea" entry (file+headline "~/Dropbox/orgfiles/i.org" "Blog Topics:")
	 "* %?\n%T" :prepend t)
	("t" "To Do Item" entry (file+headline "~/Dropbox/orgfiles/i.org" "To Do Items")
	 "* %?\n%T" :prepend t)
	("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("s" "Screencast" entry (file "~/Dropbox/orgfiles/screencastnotes.org")
	 "* %?\n%i\n")))

(setq org-ditaa-jar-path "~/git/org-mode/contrib/scripts/ditaa.jar")
(setq org-plantuml-jar-path "~/Dropbox/plantuml.jar")
(setq org-log-done 'time)

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (sql . t)
         (shell . t)
         (python . t)
         (org . t)
         (plantuml . t)
         (restclient . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
;; ;; add additional languages with '((language . t)))

(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'org-mode-config)
