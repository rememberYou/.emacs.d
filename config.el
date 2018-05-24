(setq user-full-name "Terencio Agozzino"
      user-mail-address "terencio.agozzino@gmail.com")

(setq inhibit-startup-message t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

(add-to-list 'load-path "~/elisp")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)
(setq vc-follow-symlinks nil)

(setq auth-sources '("~/Sync/shared/.authinfo.gpg"
                     "~/.authinfo.gpg"
                     "~/.authinfo"
                     "~/.netrc"))

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'libnotify))

(use-package auto-compile
  :defer 5
  :config (auto-compile-on-load-mode))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq x-select-enable-clipboard-manager nil)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :init (dumb-jump-mode)
  :custom (dump-jump-selector 'ivy))

(use-package dashboard
  :preface
  (defun my/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
  time and garbage collections."""
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :config
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))

(use-package async)
(use-package org
  :defer 1
  :hook (after-save . my/config-tangle)
  :config
  (defvar *config-file* "~/.emacs.d/config.org"
    "The configuration file.")

  (defvar *config-last-change* (nth 5 (file-attributes *config-file*))
    "Last modification time of the configuration file.")

  (defvar *show-async-tangle-results* nil
    "Keep *emacs* async buffers around for later inspection.")

  (defun my/config-updated ()
    "Check if the configuration file has been updated since the last time."
    (time-less-p *config-last-change*
                 (nth 5 (file-attributes *config-file*))))

  (defun my/config-tangle ()
    "Tangle the org file asynchronously."
    (when (my/config-updated)
      (setq *config-last-change*
            (nth 5 (file-attributes *config-file*)))
      (my/async-babel-tangle *config-file*)))

  (defun my/async-babel-tangle (org-file)
    "Tangle the org file asynchronously."
    (let ((init-tangle-start-time (current-time))
          (file (buffer-file-name))
          (async-quiet-switch "-q"))
      (async-start
       `(lambda ()
          (require 'org)
          (org-babel-tangle-file ,org-file))
       (unless *show-async-tangle-results*
         `(lambda (result)
            (if result
                (message "SUCCESS: %s successfully tangled (%.2fs)."
                         ,org-file
                         (float-time (time-subtract (current-time)
                                                    ',init-tangle-start-time)))
              (message "ERROR: %s as tangle failed." ,org-file))))))))

(global-hl-line-mode)

(setq savehist-file "~/.emacs.d/history")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Dired" (mode . dired-mode))
                 ("Org" (name . "^.*org$"))
                 ("Web" (or (mode . web-mode) (mode . js2-mode)))
                 ("Shell" (or (mode . eshell-mode) (mode . shell-mode)))
                 ("Programming" (or
                                 (mode . python-mode)))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 ))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default"))))

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(defun my/french ()
  "Set up French words for Abbrev and dictionary for ispell"
  (interactive)
  (setq ispell-dictionary "french")
  (setq abbrev-file-name "~/.emacs.d/.abbrev__french"
        save-abbrevs 'silently))

(defun my/english ()
  "Set up English words for Abbrev and dictionary for ispell"
  (interactive)
  (setq ispell-dictionary "english")
  (setq abbrev-file-name "~/.emacs.d/.abbrev_defs"
        save-abbrevs 'silently))

(use-package lorem-ipsum
  :defer 10
  :bind (("C-c C-v s" . lorem-ipsum-insert-sentences)
         ("C-c C-v p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-v l" . lorem-ipsum-insert-list)))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "C-x R") 'revert-buffer)

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(column-number-mode 1)
(show-paren-mode 1)

(defvar *afilename-cmd*
  '(("/home/someone/.Xresources" . "xrdb -merge ~/.Xresources")
    ("/home/someone/.xbindkeysrc" . "xbindkeys -p"))
  "File association list with their respective command.")

(defun my/cmd-after-saved-file ()
  "Execute a command after saved a specific file."
  (let* ((match (assoc (buffer-file-name) *afilename-cmd*)))
    (when match
      (shell-command (cdr match)))))

(add-hook 'after-save-hook 'my/cmd-after-saved-file)

(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t)
  :config
  (use-package smart-mode-line
    :config
    (setq sml/theme 'respectful)
    (sml/setup)))

(display-time-mode 1)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(use-package webpaste
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)))

(add-hook 'after-init-hook 'auto-fill-mode)
(setq-default fill-column 80)
(setq default-major-mode 'text-mode)
(setq text-mode-hook 'turn-on-auto-fill)

(use-package diminish
  :diminish abbrev-mode
  :diminish auto-fill-mode
  :diminish org-indent-mode
  :diminish visual-line-mode)

(setq browse-url-browser-function 'browse-url-chromium)

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(use-package engine-mode
  :defer 10
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  (engine-mode t))

(use-package find-dired
  :defer 20
  :custom (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(defvar my/refile-map (make-sparse-keymap))

(defmacro my/defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key my/refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))

(my/defshortcut ?I "~/.config/i3/config")
(my/defshortcut ?S "~/.config/sway/config")
(my/defshortcut ?X "~/.Xresources")
(my/defshortcut ?b "~/Sync/shared/.personal/various/buy.org")
(my/defshortcut ?c "~/.emacs.d/config.org")
(my/defshortcut ?e "~/Sync/shared/elfeed/elfeed.org")
(my/defshortcut ?i "~/.emacs.d/init.el")
(my/defshortcut ?m "~/Sync/shared/.personal/various/movies.org")
(my/defshortcut ?o "~/Sync/shared/.personal/organizer.org")
(my/defshortcut ?p "~/Sync/shared/.personal/people.org")
(my/defshortcut ?r "~/Sync/shared/.personal/routine.org")
(my/defshortcut ?s "~/Sync/shared/.personal/school.org")
(my/defshortcut ?t "~/Sync/shared/.personal/tfe.org")

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(use-package recentf
  :defer 2
  :bind ("C-c r" . recentf-open-files)
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15))

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

(use-package windmove
  :bind (("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up)
         ("C-c l" . windmove-right)))

(setq org-cycle-include-plain-lists 'integrate
      org-startup-folded nil
      org-startup-indented t
      org-yank-adjusted-subtrees t)

(add-hook 'org-mode-hook #'visual-line-mode)

(with-eval-after-load 'org
  (bind-key "C-c a" 'org-agenda)
  (bind-key "C-c J" 'org-clock-goto)
  (bind-key "C-c K" 'org-cut-subtree org-mode-map)
  (bind-key "C-c c" 'org-capture)
  (bind-key "C-c s" 'org-store-link)
  (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
  (bind-key "C-c C-w" 'org-refile)
  (bind-key "C-c L" 'org-insert-link-global)
  (bind-key "C-c O" 'org-open-at-point-global)
  (bind-key "C-c R" 'org-reveal org-mode-map)
  (bind-key "C-TAB" 'org-cycle org-mode-map)
  (bind-key "C-M-w" 'append-next-kill org-mode-map))

(use-package org
  :init
  (use-package ob-python
    :ensure org-plus-contrib
    :commands (org-babel-execute:python))
  (require' ob-C)
  (require' ob-css)
  (require' ob-dot)
  (require' ob-ditaa)
  (require' ob-emacs-lisp)
  (require' ob-gnuplot)
  (require' ob-java)
  (require' ob-js)
  (require' ob-latex)
  (require' ob-plantuml)
  (require' ob-makefile)
  (require' ob-org)
  (require' ob-ruby))

(setq org-plantuml-jar-path (expand-file-name "~/dropbox/shared/lib/plantuml.jar"))
(setq org-ditaa-jar-path "~/Dropbox/shared/lib/ditaa0_9.jar")

(setq org-modules '(org-info
                    org-crypt
                    org-habit
                    org-irc
                    org-mouse
                    org-protocol))
'(org-load-modules-maybe t)
(setq org-export-backends '(ascii beamer html icalendar latex man md org texinfo))

(defun header-org-mode ()
  (interactive)
  (insert "#+TITLE: " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n")
  (insert "#+AUTHOR: " (user-full-name) "\n")
  (insert "#+EMAIL: " "terencio.agozzino@gmail.com" "\n")
  (insert "#+OPTIONS: H:2 num:t toc:nil\n")
  (insert "#+OPTIONS: ^:nil\n")
  (insert "#+OPTIONS: <:nil todo:nil *:t ^:{} @:t ::t |:t TeX:t\n"))

(setq org-use-effective-time t)

(defun my/org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
(setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)

(with-eval-after-load 'org
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree)))

(setq org-goto-interface 'outline
      org-goto-max-level 10)

(setq org-startup-folded nil)
(setq org-cycle-include-plain-lists 'integrate)
(setq org-yank-adjusted-subtrees t)

(defun my/org-move-line-to-destination ()
  "Moves the current list item to <<destination>> in the current buffer.
If no <<destination>> is found, move it to the end of the list
and indent it one level."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((string
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))
            found)
        (delete-region (line-beginning-position) (1+ (line-end-position)))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "<<destination>>" nil t)
            (insert "\n" (make-string (- (match-beginning 0) (line-beginning-position)) ?\ ) (s-trim string))
            (setq found t)))
        (unless found
          (org-end-of-item-list)
          (insert string "\n"))))))

(require 'org-agenda)
(setq org-directory "~/Sync/shared/personal")
(setq org-default-notes-file "~/Sync/shared/.personal/organizer.org")

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/Sync/shared/.personal/101things.org"
                      "~/Sync/shared/.personal/business.org"
                      "~/Sync/shared/.personal/decisions.org"
                      "~/Sync/shared/.personal/learning.org"
                      "~/Sync/shared/.personal/organizer.org"
                      "~/Sync/shared/.personal/people.org"
                      "~/Sync/shared/.personal/projects.org"
                      "~/Sync/shared/.personal/routine.org"
                      "~/Sync/shared/.personal/school.org"))))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(defun my/org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n\n" new-date))))

(setq org-agenda-span 2)
(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-time-grid
      '((daily today require-timed)))
(setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")
;; (setq org-agenda-time-grid
;;       '((daily today require-timed)
;;         "----------------"
;;         (800 1000 1200 1400 1600 1800)))

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

(setq org-agenda-start-on-weekday 6)

(defun my/org-archive-done-tasks ()
  "Archive finished or cancelled tasks."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

(defun my/org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
        If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (if (gnus-alive-p)
                (gnus-with-article-headers
                 (mail-extract-address-components
                  (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defvar my/org-basic-task-template "* TODO %^{Task}
  :PROPERTIES:
  :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
  :END:
  Captured %<%Y-%m-%d %H:%M>
  %?

  %i
  " "Basic task data")

(defvar my/org-basic-trade-template "* Trade
  Previous Balance: %^{PreviousBalance}
  Next Balance: %^{NextBalance}
  Captured %<%Y-%m-%d>
  %?

  %i
  " "Basic trade data")

(setq org-capture-templates
      `(("b" "Buy task" checkitem (file+headline "~/Sync/shared/.personal/various/buy.org" "To Buy")
         "- [ ] %^{Task}"
         :immediate-finish t)
        ("i" "Interrupting task" entry (file+headline "~/Sync/shared/.personal/organizer.org" "Inbox")
         "* STARTED %^{Task}"
         :clock-in :clock-resume)
        ("m" "Movie task" checkitem (file+headline "~/Sync/shared/.personal/various/movies.org" "To Watch")
         "- [ ] %^{Task}"
         :immediate-finish t)
        ("p" "People task" entry (file+headline "~/Sync/shared/.personal/people.org" "Tasks"),
         my/org-basic-task-template
         :immediate-finish t)
        ("s" "School task" entry (file+headline "~/Sync/shared/.personal/school.org" "Tasks"),
         my/org-basic-task-template
         :immediate-finish t)
        ("t" "Trading" entry (file+headline "~/Sync/shared/.personal/trading.org" "Trades/Day"),
         my/org-basic-trade-template
         :immediate-finish t)
        ("T" "Tasks" entry (file+headline "~/Sync/shared/.personal/organizer.org" "Tasks"),
         my/org-basic-task-template
         :immediate-finish t)
        ("F" "TFE Tasks" entry (file+headline "~/Sync/shared/.personal/tfe.org" "Tasks"),
         my/org-basic-task-template
         :immediate-finish t)))

(defun my/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(define-key org-agenda-mode-map "N" 'my/org-agenda-new)

(defvar my/org-agenda-contexts
  '((tags-todo "+@phone")
    (tags-todo "+@work")
    (tags-todo "+@love")
    (tags-todo "+@coding")
    (tags-todo "+@writing")
    (tags-todo "+@computer")
    (tags-todo "+@home")
    (tags-todo "+@school")
    (tags-todo "+@errands"))
  "Usual list of contexts.")
(bind-key "<apps> a" 'org-agenda)

(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)

(setq org-tags-exclude-from-inheritance '("project"))

(add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("W" widen))

(defmacro my/org-with-current-task (&rest body)
  "Execute BODY with the point at the subtree of the current task."
  `(if (derived-mode-p 'org-agenda-mode)
       (save-window-excursion
         (org-agenda-switch-to)
         ,@body)
     ,@body))

(defun my/org-agenda-for-subtree ()
  (interactive)
  (when (derived-mode-p 'org-agenda-mode) (org-agenda-switch-to))
  (my/org-with-current-task
   (let ((org-agenda-view-columns-initially t))
     (org-agenda nil "t" 'subtree))))
(add-to-list 'org-speed-commands-user '("T" my/org-agenda-for-subtree))

(defun my/org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))

(defun my/org-agenda-project-agenda ()
  "Return the project headline and up to `my/org-agenda-limit-items' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))

(defun my/org-agenda-projects-and-tasks (match)
  "Show TODOs for all `org-agenda-files' headlines matching MATCH."
  (interactive "MString: ")
  (let ((todo-only nil))
    (if org-agenda-overriding-arguments
        (setq todo-only (car org-agenda-overriding-arguments)
              match (nth 1 org-agenda-overriding-arguments)))
    (let* ((org-tags-match-list-sublevels
            org-tags-match-list-sublevels)
           (completion-ignore-case t)
           rtn rtnall files file pos matcher
           buffer)
      (when (and (stringp match) (not (string-match "\\S-" match)))
        (setq match nil))
      (when match
        (setq matcher (org-make-tags-matcher match)
              match (car matcher) matcher (cdr matcher)))
      (catch 'exit
        (if org-agenda-sticky
            (setq org-agenda-buffer-name
                  (if (stringp match)
                      (format "*Org Agenda(%s:%s)*"
                              (or org-keys (or (and todo-only "M") "m")) match)
                    (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
        (org-agenda-prepare (concat "TAGS " match))
        (org-compile-prefix-format 'tags)
        (org-set-sorting-strategy 'tags)
        (setq org-agenda-query-string match)
        (setq org-agenda-redo-command
              (list 'org-tags-view `(quote ,todo-only)
                    (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
        (setq files (org-agenda-files nil 'ifmode)
              rtnall nil)
        (while (setq file (pop files))
          (catch 'nextfile
            (org-check-agenda-file file)
            (setq buffer (if (file-exists-p file)
                             (org-get-agenda-file-buffer file)
                           (error "No such file %s" file)))
            (if (not buffer)
                ;; If file does not exist, error message to agenda
                (setq rtn (list
                           (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                      rtnall (append rtnall rtn))
              (with-current-buffer buffer
                (unless (derived-mode-p 'org-mode)
                  (error "Agenda file %s is not in `org-mode'" file))
                (save-excursion
                  (save-restriction
                    (if org-agenda-restrict
                        (narrow-to-region org-agenda-restrict-begin
                                          org-agenda-restrict-end)
                      (widen))
                    (setq rtn (org-scan-tags 'my/org-agenda-project-agenda matcher todo-only))
                    (setq rtnall (append rtnall rtn))))))))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n")
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-warning))
          (setq pos (point))
          (unless org-agenda-multi
            (insert "Press `C-u r' to search again with new search string\n"))
          (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (mapconcat 'identity rtnall "\n") ""))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties (point-min) (point-max)
                             `(org-agenda-type tags
                                               org-last-args (,todo-only ,match)
                                               org-redo-cmd ,org-agenda-redo-command
                                               org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-blank-before-new-entry nil)

(defun my/org-refile-and-jump ()
      (interactive)
      (if (derived-mode-p 'org-capture-mode)
        (org-capture-refile)
        (call-interactively 'org-refile))
      (org-refile-goto-last-stored))
;;    (eval-after-load 'org-capture
  ;;    '(bind-key "C-c C-r" 'my/org-refile-and-jump org-capture-mode-map))

(defun my/org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))

;; Example: (org-refile 4 nil (my/org-refile-get-location-by-substring "Other Emacs"))
(defun my/org-refile-get-location-by-substring (regexp &optional file)
  "Return the refile location identified by REGEXP."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find regexp org-refile-target-table
           :test
           (lambda (a b)
             (and
            (string-match a (car b))
            (or (null file)
                  (string-match file (elt b 1)))))))
(defun my/org-refile-subtree-to (name)
  (org-refile nil nil (my/org-refile-get-location-exact name)))

(defun my/org-refile-get-location-exact (name &optional file)
  "Return the refile location identified by NAME."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find name org-refile-target-table
           :test (lambda (a b)
                 (and (string-equal a (car b))
                      (or (null file)
                          (string-match file (elt b 1)))))))
;; Example: (my/org-clock-in-refile "Off my computer")
(defun my/org-clock-in-refile (location &optional file)
  "Clocks into LOCATION.
LOCATION and FILE can also be regular expressions for `my/org-refile-get-location-by-substring'."
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
    (if (stringp location) (setq location (my/org-refile-get-location-by-substring location file)))
    (org-refile 4 nil location)
    (org-clock-in))))

(defun my/org-finish-previous-task-and-clock-in-new-one (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (org-clock-goto)
    (org-todo 'done))
  (my/org-clock-in-and-track-by-name location file))

(defun my/org-clock-in-and-track-by-name (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
    (if (stringp location) (setq location (my/org-refile-get-location-exact location file)))
    (org-refile 4 nil location)
    (my/org-clock-in-and-track))))
(defun my/org-off-computer (category)
  (interactive "MCategory: ")
  (my/org-clock-in-refile "Off my computer")
  (quantified-track category))

(setq org-tag-alist '(("@work" . ?b)
                      ("@home" . ?h)
                      ("@writing" . ?w)
                      ("@errands" . ?e)
                      ("@love" . ?d)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("@school" . ?s)
                      ("crypt" . ?C)
                      ("fuzzy" . ?0)
                      ("highenergy" . ?1)))

(progn
  (setq org-expiry-inactive-timestamps t)
  (setq org-clock-idle-time nil)
  (setq org-log-done 'time)
  (setq org-clock-continuously nil)
  (setq org-clock-persist t)
  (setq org-clock-in-switch-to-state "STARTED")
  (setq org-clock-in-resume nil)
  (setq org-show-notification-handler 'message)
  (setq org-clock-report-include-clocking-task t))
(org-clock-persistence-insinuate)

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)

(defun my/org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (my/org-with-current-task
   (org-clock-in)
   ;;(call-interactively 'my/org-quantified-track)
   (when (org-entry-get (point) "AUTO")
     (org-open-link-from-string (org-entry-get (point) "AUTO")))))

(add-to-list 'org-global-properties
             '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

(add-hook 'org-clock-in-prepare-hook
          'my/org-mode-ask-effort)

(defun my/org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(defun my/compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
              (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                 (org-hh:mm-string-to-minutes estimated)))
    ""))

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "STARTED(s)"
         "WAITING(w@/!)"
         "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
        (sequence "TOBUY" "TOSHRINK" "TOCUT"  "TOSEW" "|" "DONE(x)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))

(setq org-log-done 'time)

(defun my/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'my/org-agenda-done)

(defun my/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
 Creates it at the same level as the previous task, so it's better to use
 this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "X" 'my/org-agenda-mark-done-and-add-followup)

(use-package org-journal
  :bind (("C-c t" . org-journal-new-entry)
         ("C-c y" . journal-file-yesterday))
  :custom
  (org-journal-dir "~/Sync/shared/.journal/2018/")
  (org-journal-file-format "%Y%m%d")
  (org-journal-date-format "%e %b %Y (%A)")
  (org-journal-time-format "")
  (org-journal-enable-encryption t)
  :preface
  (defun get-journal-file-yesterday ()
    "Gets filename for yesterday's journal entry."
    (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y%m%d" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))

  (defun journal-last-year-file ()
    "Gets the string corresponding to the journal entry that
happened 'last year' at this same time (meaning on the same day
of the week)."
    (let* ((last-year-seconds (- (float-time) (* 365 24 60 60)))
           (last-year (seconds-to-time last-year-seconds))
           (last-year-dow (nth 6 (decode-time last-year)))
           (this-year-dow (nth 6 (decode-time)))
           (difference (if (> this-year-dow last-year-dow)
                           (- this-year-dow last-year-dow)
                         (- last-year-dow this-year-dow)))
           (target-date-seconds (+ last-year-seconds (* difference 24 60 60)))
           (target-date (seconds-to-time target-date-seconds)))
      (format-time-string "%Y%m%d" target-date)))

  (defun journal-last-year ()
    "Loads last year's journal entry, which is not necessary the
same day of the month, but will be the same day of the week."
    (interactive)
    (let ((journal-file (concat org-journal-dir (journal-last-year-file))))
      (find-file journal-file))))

(use-package epa
  :custom
  (epg-gpg-program "gpg"))

(use-package org
  :bind ("C-c d" . org-decrypt-entry)
  :init (org-crypt-use-before-save-magic)
  :custom
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  (org-crypt-key "E9AADC36E94A672D1A07D49B208FCDBB98190562")
  (auto-save-default nil))

(use-package ox-reveal
  :defer 2
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t))

(use-package htmlize
  :defer 2)

(use-package aggressive-indent
  :defer 2
  :diminish
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package atomic-chrome
  :defer 2
  :hook (atomic-chrome-edit-mode . flyspell-mode)
  :init
  (defun atomic-chrome-server-running-p ()
    (cond ((executable-find "lsof")
           (zerop (call-process "lsof" nil nil nil "-i" ":64292")))
          ((executable-find "netstat") ; Windows
           (zerop (call-process-shell-command "netstat -aon | grep 64292")))))

  (if (atomic-chrome-server-running-p)
      (message "Can't start atomic-chrome server, because port 64292 is already used")
    (atomic-chrome-start-server)))

(use-package calc
  :defer t
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")))
  :config
  (setq math-units-table nil))

(use-package company
  :diminish
  :defer 2
  :custom
  (company-tooltip-align-annotations 't)
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-begin-commands '(self-insert-command))
  (company-show-numbers t)
  (global-company-mode t))

(use-package company-box
  :diminish
  :after company
  :hook (company-mode . company-box-mode))

(use-package docker
  :defer 15
  :diminish
  :config
  (require 'docker-images)
  (require 'docker-containers)
  (require 'docker-volumes)
  (require 'docker-networks)
  (docker-global-mode))

(use-package magit
  :bind (("C-c m c" . magit-commit)
         ("C-c m a" . magit-stage)
         ("C-c m s" . magit-status)
         ("C-c m u" . magit-unstage)
         ("C-c m U" . magit-unstage-all)
         ("C-c m p" . magit-push)))

(use-package git-gutter
  :defer 2
  :diminish
  :init
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :defer 3
  :diminish)

(use-package which-key
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))

(use-package expand-region
  :defer 2
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :defer 2
  :diminish (flycheck-mode)
  :init (global-flycheck-mode t)
  :config
  (flycheck-define-checker my-php
    "A PHP syntax checker using the PHP command line interpreter.
    See URL `http://php.net/manual/en/features.commandline.php'."
    :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
              "-d" "log_errors=0" source)
    :error-patterns
    ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
            (message) " in " (file-name) " on line " line line-end))
    :modes (php-mode php+-mode web-mode)))

(use-package hungry-delete
  :defer 2
  :diminish
  :config
  (global-hungry-delete-mode))

(use-package iedit
  :defer t)

(use-package ipcalc
  :commands ipcalc)

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :custom
  (ledger-clear-whole-transactions 1)
  ;; https://xkcd.com/1179/
  (ledger-use-iso-dates t))

  (use-package flycheck-ledger
    :after ledger-mode)

(use-package pdf-tools
  :defer 2
  :init
  (pdf-tools-install)
  :custom
  ;; Disable unicode support in mode line for more speed.
  (pdf-view-use-unicode-ligther nil))

(use-package projectile
  :defer 5
  :diminish
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after (counsel projectile)
  (counsel-projectile-on))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package skewer-mode
  :hook ((js2-mode . skewer-mode)
        (css-mode . skewer-css-mode)
        (html-mode . skewer-html-mode)
        (web-mode . skewer-html-mode)))

(use-package ivy
  :defer 5
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-mode 1)
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy))

(use-package ivy-pass
  :after ivy
  :commands ivy-pass)

(use-package ivy-rich
  :defer 6
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package counsel
  :after ivy
  :bind ("M-y" . counsel-yank-pop))

(use-package simple-httpd
  :defer 4
  :custom
  (httpd-root "/var/www/html"))

(use-package impatient-mode
  :after simple-httpd
  :hook ((web-mode . httpd-start)
         (web-mode . impatient-mode)
         (css-mode . httpd-start)))

(use-package smartparens
  :defer 5)

(use-package spotify
  :defer 5
  :config
  (spotify-enable-song-notifications))

(use-package try
  :defer 5)

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C--" . undo-tree-redo)
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

(use-package web-beautify
  :disabled
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'json-mode
    '(add-hook 'json-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'sgml-mode
    '(add-hook 'html-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-css-buffer t t)))))

(use-package web-mode
  :commands web-mode
  :hook ((css-mode web-mode) . rainbow-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php$" . my/php-setup))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package key-chord
  :commands key-chord-mord)

(use-package winner :defer 5)

(use-package yasnippet
  :defer 2
  :diminish yas-minor-mode
  :bind ("C-c i" . yas-insert-snippet)
  :init
  (yas-global-mode t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-installed-snippets-dir "~/.emacs.d/snippets"))

(use-package "eldoc"
  :diminish
  :commands turn-on-eldoc-mode
  :hook ((abbrev-mode . emacs-lisp-mode)
         (abbrev-mode . lisp-interaction-mode)
         (abbrev-mode . ielm-mode)))

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

(defun my/sort-sexps-in-region (beg end)
  "Can be handy for sorting out duplicates.
Sorts the sexps from BEG to END. Leaves the point at where it
couldn't figure things out (ex: syntax errors)."
  (interactive "r")
  (let ((input (buffer-substring beg end))
        list last-point form result)
    (save-restriction
      (save-excursion
        (narrow-to-region beg end)
        (goto-char (point-min))
        (setq last-point (point-min))
        (setq form t)
        (while (and form (not (eobp)))
          (setq form (ignore-errors (read (current-buffer))))
          (when form
            (add-to-list
             'list
             (cons
              (prin1-to-string form)
              (buffer-substring last-point (point))))
            (setq last-point (point))))
        (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
        (delete-region (point-min) (point))
        (insert (mapconcat 'cdr list "\n"))))))

'(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

(use-package emmet-mode
  :defer 10
  :hook (sgml-mode css-mode web-mode))

(use-package less-css-mode
  :mode "\\.less\\'"
  :interpreter ("less" . less-css-mode))

(use-package eclim
  :defer t
  :hook (java-mode . eclim-mode)
  :custom
  (eclimd-autostart t)
  (eclimd-default-workspace '"~/Documents/Projects/Java/")
  (eclim-eclipse-dirs '"/opt/eclipse")
  (eclim-executable '"/opt/eclipse/eclim")
  (help-at-pt-display-when-idle t)
  (help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer))

(use-package company-emacs-eclim
  :after eclim
  :commands company-emacs-eclim-setup)

(use-package gradle-mode
  :mode "\\.gradle\\'"
  :interpreter ("gradle" . gradle-mode))

(use-package js2-mode
  :defer 40
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
  ;; Better imenu
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)

  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package xref-js2 :defer 40)

(use-package tern
  :defer 30
  :config
  (bind-key "C-c C-c" 'compile tern-mode-keymap)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode)
                             (company-mode)))

  ;; Disable completion keybindings, as we use xref-js2 instead
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

(use-package company-tern
  :after (company tern)
  :config (add-to-list 'company-backends 'company-tern))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . flyspell-mode)
         (LaTeX-mode . reftex-mode))
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-view-program-selection '((output-pdf "Evince")
                                (output-html "xdg-open")))
  (TeX-source-correlate-mode t))

(setq-default TeX-engine 'xetex)

(use-package company-auctex
  :after (auctex company)
  :config
  (company-auctex-init))

(use-package reftex
  :after auctex)

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package markdown-preview-mode
  :after markdown-mode)

(defun my/php-setup ()
  (web-mode)

  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  (use-package ac-php
    :config
    (auto-complete-mode t)
    (setq ac-sources '(ac-source-php))
    (yas-global-mode 1)
    (ac-php-core-eldoc-setup))

  (flycheck-select-checker 'my-php)
  (flycheck-mode t))

(use-package anaconda-mode
  :after python
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :after (anaconda-mode company)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package sql-indent
  :mode "\\.sql\\'"
  :interpreter ("sql" . sql-mode))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :interpreter ("yml" . yml-mode))

(use-package erc
  :defer 10
  :bind (("C-c e" . my/erc-start-or-switch)
         ("C-c n" . my/erc-count-users))
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#archlinux" "#bash" "#bitcoin"
                                  "#emacs" "#gentoo" "#i3" "#latex" "#org-mode"  "#python" "#sway")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-prompt-for-nickserv-password nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")))
  (erc-lurker-threshold-time 43200)

  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc)

(defun my/erc-count-users ()
  "Displays the number of users connected on the current channel."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (let ((channel (erc-default-target)))
        (if (and channel (erc-channel-p channel))
            (message "%d users are online on %s"
                     (hash-table-count erc-channel-users)
                     channel)
          (user-error "The current buffer is not a channel")))
    (user-error "You must first start ERC")))

(defun my/erc-notify (nickname message)
  "Displays a notification message for ERC."
  (let* ((channel (buffer-name))
         (nick (erc-hl-nicks-trim-irc-nick nickname))
         (title (if (string-match-p (concat "^" nickname) channel)
                    nick
                  (concat nick " (" channel ")")))
         (msg (s-trim (s-collapse-whitespace message))))

    (alert (concat nick ": " msg) :title title)))

(add-hook 'ercn-notify-hook 'my/erc-notify)

(defun my/erc-preprocess (string)
  "Avoids channel flooding"
  (setq str
        (string-trim
         (replace-regexp-in-string "\n+" " " str))))

(add-hook 'erc-send-pre-hook 'my/erc-preprocess)

(defun my/erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.freenode.net" :port 6667 :nick "rememberYou"))))

(use-package elfeed
  :defer 2
  :bind (("C-x e" . elfeed)
         :map elfeed-search-mode-map
         ("q" . elfeed-save-db-and-bury)
         ("Q" . elfeed-save-db-and-bury)
         ("m" . elfeed-toggle-star)
         ("M" . elfeed-toggle-star)
         ("j" . mz/make-and-run-elfeed-hydra)
         ("J" . mz/make-and-run-elfeed-hydra))
  :custom (elfeed-db-directory "~/Sync/shared/elfeed/db"))

  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  (defun elfeed-mark-all-as-read ()
    "Mark all feeds as read"
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defun z/hasCap (s) ""
         (let ((case-fold-search nil))
           (string-match-p "[[:upper:]]" s)))

  (defun z/get-hydra-option-key (s)
    "returns single upper case letter (converted to lower) or first"
    (interactive)
    (let ( (loc (z/hasCap s)))
      (if loc
          (downcase (substring s loc (+ loc 1)))
        (substring s 0 1))))

  (defun mz/make-elfeed-cats (tags)
    "Returns a list of lists. Each one is line for the hydra configuration in the form
       (c function hint)"
    (interactive)
    (mapcar (lambda (tag)
              (let* (
                     (tagstring (symbol-name tag))
                     (c (z/get-hydra-option-key tagstring))
                     )
                (list c (append '(elfeed-search-set-filter) (list (format "@6-months-ago +%s" tagstring) ))tagstring  )))
            tags))

  (defmacro mz/make-elfeed-hydra ()
    `(defhydra mz/hydra-elfeed ()
       "filter"
       ,@(mz/make-elfeed-cats (elfeed-db-get-all-tags))
       ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
       ("M" elfeed-toggle-star "Mark")
       ("A" (elfeed-search-set-filter "@6-months-ago") "All")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("Q" elfeed-save-db-and-bury "Quit Elfeed" :color blue)
       ("q" nil "quit" :color blue)))

  (defun mz/make-and-run-elfeed-hydra ()
    ""
    (interactive)
    (mz/make-elfeed-hydra)
    (mz/hydra-elfeed/body))

(use-package hydra
  :defer 2)

(global-set-key
 (kbd "C-x t")
 (defhydra toggle (:color blue)
   "toggle"
   ("a" abbrev-mode "abbrev")
   ("s" flyspell-mode "flyspell")
   ("d" toggle-debug-on-error "debug")
   ("w" whitespace-mode "whitespace")
   ("q" nil "cancel")))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(use-package elfeed-org
  :after elfeed
  :defer 2
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Sync/shared/elfeed/elfeed.org")))

(use-package elfeed-goodies
  :after elfeed
  :defer 2
  :config
  (elfeed-goodies/setup))

(require 'mu4e)

(setq mu4e-maildir "~/Maildir"
      mu4e-drafts-folder "/gmail/Drafts"
      mu4e-sent-folder "/gmail/Sent Mail"
      mu4e-trash-folder "/gmail/Trash"
      mu4e-refile-folder "/gmail/Archive"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300 ;; second
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-attachment-dir "~/Downloads"
      mu4e-use-fancy-chars t)

(setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX" . ?i)
        ("/gmail/All Mail" . ?a)
        ("/gmail/Deleted Items" . ?d)
        ("/gmail/Drafts" . ?D)
        ("/gmail/Important" . ?i)
        ("/gmail/Sent Mail" . ?s)
        ("/gmail/Starred" . ?S)))

(use-package mu4e-alert
  :defer 1
  :hook ((after-init . mu4e-alert-enable-notifications)
         (after-init . mu4e-alert-enable-mode-line-display))
  :custom (mu4e-alert-set-default-style 'libnotify))

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465)

(use-package ebdb
  :commands ebdb)

(use-package helm-ebdb
  :after ebdb)

(use-package company-ebdb
  :after ebdb)
