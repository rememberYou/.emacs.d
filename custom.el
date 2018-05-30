(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t t nil "Customized with use-package tex")
 '(TeX-auto-save t t nil "Customized with use-package tex")
 '(TeX-byte-compile t t nil "Customized with use-package tex")
 '(TeX-clean-confirm nil t nil "Customized with use-package tex")
 '(TeX-master (quote dwim) t nil "Customized with use-package tex")
 '(TeX-parse-self t t nil "Customized with use-package tex")
 '(TeX-source-correlate-mode t t nil "Customized with use-package tex")
 '(TeX-view-program-selection (quote ((output-pdf "Evince") (output-html "xdg-open"))) t nil "Customized with use-package tex")
 '(add-to-list (quote erc-modules) t nil "Customized with use-package erc")
 '(aggressive-indent-comments-too nil)
 '(android-mode-sdk-dir "/opt/android-sdk")
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "/home/someone/.emacs.d/backups/"))))
 '(company-begin-commands (quote (self-insert-command)) nil nil "Customized with use-package company")
 '(company-idle-delay 0.1 nil nil "Customized with use-package company")
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t nil nil "Customized with use-package company")
 '(company-tooltip-limit 20 nil nil "Customized with use-package company")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
	("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" default)))
 '(delete-old-versions -1)
 '(dump-jump-selector (quote ivy) t nil "Customized with use-package dumb-jump")
 '(eclim-eclipse-dirs "/opt/eclipse" nil nil "Customized with use-package eclim")
 '(eclim-executable "/opt/eclipse/eclim" nil nil "Customized with use-package eclim")
 '(eclimd-autostart t nil nil "Customized with use-package eclim")
 '(eclimd-default-workspace "~/Documents/Projects/Java/" nil nil "Customized with use-package eclim")
 '(elfeed-db-directory "~/Sync/shared/elfeed/db" nil nil "Customized with use-package elfeed")
 '(epg-gpg-program "gpg")
 '(erc-autojoin-channels-alist
   (quote
	(("freenode.net" "#archlinux" "#bash" "#emacs" "#gentoo" "#i3" "#latex" "#org-mode" "#python" "#sway"))) nil nil "Customized with use-package erc")
 '(erc-autojoin-timing (quote ident) nil nil "Customized with use-package erc")
 '(erc-fill-function (quote erc-fill-static) nil nil "Customized with use-package erc")
 '(erc-fill-static-center 22 nil nil "Customized with use-package erc")
 '(erc-header-line-format "%n on %t (%m)")
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")) nil nil "Customized with use-package erc")
 '(erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")) nil nil "Customized with use-package erc")
 '(erc-lurker-threshold-time 43200 nil nil "Customized with use-package erc")
 '(erc-prompt-for-nickserv-password nil nil nil "Customized with use-package erc")
 '(erc-server-reconnect-attempts 5 nil nil "Customized with use-package erc")
 '(erc-server-reconnect-timeout 3 nil nil "Customized with use-package erc")
 '(erc-services-mode 1 nil nil "Customized with use-package erc")
 '(erc-track-exclude-types
   (quote
	("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477")) nil nil "Customized with use-package erc")
 '(fci-rule-color "#073642")
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")) nil nil "Customized with use-package find-dired")
 '(flyspell-lazy-window-idle-seconds 60 t)
 '(flyspell-mode t t)
 '(global-company-mode t)
 '(guess-language-languages (quote (en fr)))
 '(guess-language-min-paragraph-length 40)
 '(help-at-pt-display-when-idle t nil (help-at-pt) "Customized with use-package eclim")
 '(help-at-pt-set-timer nil t nil "Customized with use-package eclim")
 '(help-at-pt-timer-delay 0.1 nil nil "Customized with use-package eclim")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
	(solarized-color-blend it "#002b36" 0.25)
	(quote
	 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
	(("#073642" . 0)
	 ("#546E00" . 20)
	 ("#00736F" . 30)
	 ("#00629D" . 50)
	 ("#7B6000" . 60)
	 ("#8B2C02" . 70)
	 ("#93115C" . 85)
	 ("#073642" . 100))))
 '(hl-bg-colors
   (quote
	("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
	("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(httpd-root "/var/www/html" nil nil "Customized with use-package simple-httpd")
 '(ivy-display-style (quote fancy) nil nil "Customized with use-package ivy")
 '(ivy-mode 1 nil nil "Customized with use-package ivy")
 '(ivy-rich-path-style (quote abbrev))
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-set-display-transformer (quote ivy-switch-buffer) t)
 '(ivy-use-virtual-buffers t nil nil "Customized with use-package ivy")
 '(ivy-virtual-abbreviate (quote full))
 '(ledger-clear-whole-transactions 1 t nil "Customized with use-package ledger-mode")
 '(ledger-use-iso-dates t t)
 '(magit-diff-use-overlays nil)
 '(math-additional-units
   (quote
	((GiB "1024 * MiB" "Giga Byte")
	 (MiB "1024 * KiB" "Mega Byte")
	 (KiB "1024 * B" "Kilo Byte")
	 (B nil "Byte")
	 (Gib "1024 * Mib" "Giga Bit")
	 (Mib "1024 * Kib" "Mega Bit")
	 (Kib "1024 * b" "Kilo Bit")
	 (b "B / 8" "Bit"))) t nil "Customized with use-package calc")
 '(math-units-table nil t)
 '(mu4e-alert-set-default-style (quote libnotify) t nil "Customized with use-package mu4e-alert")
 '(muse-project-alist nil)
 '(nrepl-message-colors
   (quote
	("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files (quote ("~/.emacs.d/config.org")))
 '(org-crypt-key "E9AADC36E94A672D1A07D49B208FCDBB98190562")
 '(org-ditaa-jar-path "~/Sync/shared/lib/ditaa0_9.jar")
 '(org-journal-date-format "%e %b %Y (%A)" nil nil "Customized with use-package org-journal")
 '(org-journal-dir "~/Sync/shared/.journal/2018/" nil nil "Customized with use-package org-journal")
 '(org-journal-enable-encryption t)
 '(org-journal-file-format "%Y%m%d" nil nil "Customized with use-package org-journal")
 '(org-journal-time-format "" nil nil "Customized with use-package org-journal")
 '(org-plantuml-jar-path "/home/someone/Sync/shared/lib/plantuml.jar")
 '(org-reveal-mathjax t nil nil "Customized with use-package ox-reveal")
 '(org-reveal-root "file:///home/someone/Dropbox/shared/lib/reveal.js" nil nil "Customized with use-package ox-reveal")
 '(org-reveal-transition "fade" nil nil "Customized with use-package ox-reveal")
 '(org-tags-exclude-from-inheritance (quote ("crypt")))
 '(package-selected-packages
   (quote
	(dockerfile-mode toc-org erc-button smart-mode-line simple-httpd swiper ivy magit-popup files org-babel guess-language org-crypt diminish auto-fill-mode smart-mode-line-powerline-theme simple async dashboard ob-org mybindings mybinding lol js2-mode ob-java ob-C company-box emacs-async org-plus-contrib counsel-org-clock jinja2-mode yaml-mode magit color-theme-sanityinc-solarized color-theme-solarized erc-yank solarized-theme color-theme atomic-chrome noctilux-theme sokoban elfeed markdown-mode eclim yasnippet projectile flycheck expand-region company htmlize webpaste mu4e-alert mu4e alert helm-notmuch elfeed-goodies elfeed-org erc-youtube docker chess beacon ivy-hydra ivy-pass ivy-rich ipcalc abbrev company-auctex latex ercn erc-question erc-alert comint circle circe auth-password-store tex use-package which-key dumb-jump dump-jump counsel-projectile aggresive-indent hungry-delete counsel swipper councel try org-journal sql-mode ng2-mode gradle-mode company-ebdb helm-ebdb ebdb abbrev-mode powershell org-edna plantuml-mode engine-mode w3m lorem-ipsum skewer-mode company-lua ac-php spotify bbdb helm-spotify flycheck-ledger erc-image erc-hl-nicks company-tern xref-js2 js2-refactor company-anaconda paredit erefactor artbollocks-mode peep-dired switch-window eclimd auto-compile web-mode web-beautify undo-tree tern tabbar sql-indent smex smartscan smartparens skype scss-mode restclient requirejs-mode rainbow-mode pdf-tools ox-twbs ox-reveal ox-ioslide org-bullets nlinum-relative neotree multiple-cursors move-text miniedit memoize markdown-preview-mode lua-mode less-css-mode ledger-mode key-chord json-mode js-comint impatient-mode iedit hydra helm-swoop helm-projectile helm-descbinds guide-key github-clone git-timemachine git-messenger git-link git-gutter git gist emmet-mode crm-custom company-ycmd company-quickhelp company-emacs-eclim column-marker change-inner buffer-move browse-at-remote bm autopair auto-package-update auto-complete-c-headers auctex android-mode anaconda-mode aggressive-indent ace-window ace-popup-menu ac-emacs-eclim)))
 '(pdf-view-use-unicode-ligther nil nil nil "Customized with use-package pdf-tools")
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(recentf-max-menu-items 15 nil nil "Customized with use-package recentf")
 '(recentf-max-saved-items 200 nil nil "Customized with use-package recentf")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(undo-tree-visualizer-diff t nil nil "Customized with use-package undo-tree")
 '(undo-tree-visualizer-timestamps t nil nil "Customized with use-package undo-tree")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#dc322f")
	 (40 . "#ff7f00")
	 (60 . "#ffbf00")
	 (80 . "#b58900")
	 (100 . "#ffff00")
	 (120 . "#ffff00")
	 (140 . "#ffff00")
	 (160 . "#ffff00")
	 (180 . "#859900")
	 (200 . "#aaff55")
	 (220 . "#7fff7f")
	 (240 . "#55ffaa")
	 (260 . "#2affd4")
	 (280 . "#2aa198")
	 (300 . "#00ffff")
	 (320 . "#00ffff")
	 (340 . "#00ffff")
	 (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vc-make-backup-files t)
 '(version-control t)
 '(weechat-color-list
   (quote
	(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
 '(yas-installed-snippets-dir "~/.emacs.d/snippets" t nil "Customized with use-package yasnippet")
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")) nil nil "Customized with use-package yasnippet"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black")))))
