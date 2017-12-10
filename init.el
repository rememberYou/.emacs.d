;;; init.el --- Emacs configuration.

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(load-file (concat user-emacs-directory "config.el"))
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
