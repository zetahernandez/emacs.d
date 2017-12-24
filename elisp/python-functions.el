(require 'python)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\s-*" "import pdb; pdb.set_trace()" "$")
                               (point-max) t)
            (setq buffer-read-only nil)
            ;; set COMINT argument to `t'.
          (ad-set-arg 1 t))))))

(provide 'python-functions)
