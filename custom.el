(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t t)
 '(TeX-auto-save t t)
 '(TeX-byte-compile t t)
 '(TeX-clean-confirm nil t)
 '(TeX-master (quote dwim) t)
 '(TeX-parse-self t t)
 '(TeX-source-correlate-mode t t)
 '(TeX-view-program-selection (quote ((output-pdf "Evince") (output-html "xdg-open"))) t)
 '(add-to-list (quote erc-modules) t)
 '(android-mode-sdk-dir "/opt/android-sdk")
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" default)))
 '(dump-jump-selector (quote ivy) t)
 '(eclim-eclipse-dirs "/opt/eclipse" t)
 '(eclim-executable "/opt/eclipse/eclim" t)
 '(eclimd-autostart t t)
 '(eclimd-default-workspace "~/Documents/Projects/Java/" t)
 '(elfeed-db-directory "~/Dropbox/shared/elfeed/db")
 '(epg-gpg-program "/usr/bin/gpg")
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "#android-dev" "#archlinux" "#bash" "#bitcoin" "#emacs" "#latex" "#python" "#sway"))) t)
 '(erc-autojoin-timing (quote ident) t)
 '(erc-fill-function (quote erc-fill-static) t)
 '(erc-fill-static-center 22 t)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")) t)
 '(erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")) t)
 '(erc-lurker-threshold-time 43200 t)
 '(erc-prompt-for-nickserv-password nil t)
 '(erc-server-reconnect-attempts 5 t)
 '(erc-server-reconnect-timeout 3 t)
 '(erc-services-mode 1 t)
 '(erc-track-exclude-types
   (quote
    ("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477")) t)
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")) t)
 '(help-at-pt-display-when-idle t nil (help-at-pt))
 '(help-at-pt-set-timer nil t)
 '(help-at-pt-timer-delay 0.1)
 '(httpd-root "/var/www/html" t)
 '(ivy-display-style (quote fancy))
 '(ivy-mode 1)
 '(ivy-use-virtual-buffers t)
 '(ledger-clear-whole-transactions 1 t)
 '(math-additional-units
   (quote
    ((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit"))) t)
 '(mu4e-alert-set-default-style (quote libnotify) t)
 '(muse-project-alist nil)
 '(org-agenda-files
   (quote
    ("~/personal/business.org" "~/personal/organizer.org" "~/personal/people.org" "~/personal/projects.org" "~/personal/routine.org" "~/personal/school.org")))
 '(org-journal-date-format "%e %b %Y (%A)")
 '(org-journal-dir "~/Dropbox/shared/.journal/2018/")
 '(org-journal-file-format "%Y%m%d")
 '(org-journal-time-format "")
 '(org-reveal-mathjax t t)
 '(org-reveal-root "file:///home/someone/Dropbox/shared/lib/reveal.js" t)
 '(org-reveal-transition "fade" t)
 '(package-selected-packages
   (quote
    (solarized-theme color-theme atomic-chrome noctilux-theme sokoban elfeed markdown-mode eclim yasnippet projectile flycheck expand-region company htmlize webpaste mu4e-alert mu4e alert helm-notmuch elfeed-goodies elfeed-org erc-youtube docker chess beacon ivy-hydra ivy-pass ivy-rich ipcalc abbrev company-auctex latex ercn erc-question erc-alert comint circle circe auth-password-store tex use-package which-key dumb-jump dump-jump counsel-projectile aggresive-indent hungry-delete counsel swipper councel try org-journal sql-mode ng2-mode gradle-mode company-ebdb helm-ebdb ebdb abbrev-mode powershell org-edna plantuml-mode engine-mode w3m lorem-ipsum skewer-mode company-lua ac-php spotify bbdb helm-spotify flycheck-ledger erc-image erc-hl-nicks company-tern xref-js2 js2-refactor company-anaconda paredit erefactor artbollocks-mode peep-dired switch-window eclimd auto-compile web-mode web-beautify undo-tree tern tabbar sql-indent smex smartscan smartparens skype scss-mode restclient requirejs-mode rainbow-mode pdf-tools ox-twbs ox-reveal ox-ioslide org-bullets nlinum-relative neotree multiple-cursors move-text miniedit memoize markdown-preview-mode lua-mode less-css-mode ledger-mode key-chord json-mode js-comint impatient-mode iedit hydra helm-swoop helm-projectile helm-descbinds guide-key github-clone git-timemachine git-messenger git-link git-gutter git gist emmet-mode crm-custom company-ycmd company-quickhelp company-emacs-eclim column-marker change-inner buffer-move browse-at-remote bm autopair auto-package-update auto-complete-c-headers auctex android-mode anaconda-mode aggressive-indent ace-window ace-popup-menu ac-emacs-eclim)))
 '(pdf-view-use-unicode-ligther nil)
 '(recentf-max-menu-items 15)
 '(recentf-max-saved-items 200)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(yas-installed-snippets-dir "~/.emacs.d/elisp/yasnippet-snippets" t)
 '(yas-snippet-dirs (quote ("~/.emacs.d/elisp/yasnippet-snippets"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))))
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue")))))
