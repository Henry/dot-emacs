;;; init-gnuplot.el --- Initialize gnuplot-mode
;; -----------------------------------------------------------------------------
(use-package gnuplot
  :mode ("\\.gp\\'" . gnuplot-mode)
  :config
  (defun my-gnuplot-mode-hook ()
    (font-lock-mode 1))
  (add-hook 'gnuplot-mode-hook 'my-gnuplot-mode-hook))

;; -----------------------------------------------------------------------------
;;; init-gnuplot.el ends here
