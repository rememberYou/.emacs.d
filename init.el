;;; init.el --- Emacs configuration.

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(if (file-exists-p (concat user-emacs-directory "config.el"))
    (load-file (concat user-emacs-directory "config.el"))
  (org-babel-load-file (concat user-emacs-directory "config.org")))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
