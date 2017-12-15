(setq user-full-name "Terencio Agozzino"
      user-mail-address "terencio.agozzino@gmail.com")

(setq inhibit-startup-message t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

(add-to-list 'load-path "~/elisp")

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(require 'use-package)

(setq load-prefer-newer t)
(setq vc-follow-symlinks nil)

;; (abbrev-mode t)
  (setq abbrev-file-name "~/.emacs.d/.abbrev_defs"
        save-abbrevs 'silently)
  (quietly-read-abbrev-file)

(use-package auto-compile
  :defer 5
  :config (auto-compile-on-load-mode))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :defer 5
  :hook (before-save-hook . whitespace-cleanup)
  :custom (whitespace-style '(face empty tabs lines-tail trailing)))

(setq x-select-enable-clipboard-manager nil)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :init (dumb-jump-mode)
  :custom (dump-jump-selector 'ivy))

(use-package org
  :defer 2
  :config
  (defvar *config-file* "~/.emacs.d/config.org"
    "The configuration file.")

  (defvar *config-last-change* (nth 5 (file-attributes *config-file*))
    "Last modification time of the configuration file.")

  (defun my/config-updated-p ()
    "Check if the configuration file has been updated since the last time."
    (time-less-p *config-last-change*
                 (nth 5 (file-attributes *config-file*))))

  (defun my/config-update ()
    "Compile the configuration file."
    (when (my/config-updated-p)
      (setq *config-last-change*
            (nth 5 (file-attributes *config-file*)))
      (org-babel-tangle)))

  (add-hook 'kill-emacs-hook 'my/config-update))

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

(global-set-key (kbd "C-x C-b") 'ibuffer)
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
            (ibuffer-switch-to-saved-filter-groups "default")))

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
  (setq filenames (mapcar 'car *afilename-cmd*))
  (dolist (file filenames)
    (let ((cmd (cdr (assoc file *afilename-cmd*))))
      (if (file-exists-p file)
          (when (equal (buffer-file-name) file)
            (shell-command cmd))
        (error "No such file %s" file)))))

(add-hook 'after-save-hook 'my/cmd-after-saved-file)

(use-package color-theme
  :init
  (set-face-foreground 'secondary-selection "darkblue")
  (set-face-background 'font-lock-doc-face "black")
  (set-face-foreground 'font-lock-doc-face "wheat")
  (set-face-background 'font-lock-string-face "black"))

(use-package color-theme-solarized
  :init
  (color-theme-solarized)
  (set-face-foreground 'org-todo "green")
  (set-face-background 'org-todo "black"))

(display-time-mode 1)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(add-hook 'after-init-hook 'auto-fill-mode)
(setq-default fill-column 80)
(setq default-major-mode 'text-mode)
(setq text-mode-hook 'turn-on-auto-fill)

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

(my/defshortcut ?P "~/personal/people.org")
(my/defshortcut ?S "~/.config/sway/config")
(my/defshortcut ?X "~/.Xresources")
(my/defshortcut ?a "~/.config/awesome/rc.lua")
(my/defshortcut ?b "~/personal/business.org")
(my/defshortcut ?c "~/.emacs.d/config.org")
(my/defshortcut ?d "~/personal/decisions.org")
(my/defshortcut ?i "~/.emacs.d/init.el")
(my/defshortcut ?l "~/personal/learning.org")
(my/defshortcut ?o "~/personal/organizer.org")
(my/defshortcut ?p "~/personal/projects.org")
(my/defshortcut ?r "~/personal/routine.org")
(my/defshortcut ?s "~/personal/school.org")

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

(require 'ob-plantuml)

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (css . t)
                             (dot . t)
                             (emacs-lisp t)
                             (gnuplot . t)
                             (java . t)
                             (js . t)
                             (latex . t)
                             (plantuml . t)
                             (makefile . t)
                             (org . t)
                             (python . t)
                             (ruby . t)
                             (shell . t)))

(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))

(setq org-modules '(org-info
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
(setq org-directory "~/personal")
(setq org-default-notes-file "~/personal/organizer.org")

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/personal/101things.org"
                      "~/personal/business.org"
                      "~/personal/decisions.org"
                      "~/personal/learning.org"
                      "~/personal/organizer.org"
                      "~/personal/people.org"
                      "~/personal/projects.org"
                      "~/personal/routine.org"
                      "~/personal/school.org"))))
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
(setq org-capture-templates
      '(("T" "Quick task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* TODO %^{Task}\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         ,my/org-basic-task-template)
        ("i" "Interrupting task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* STARTED %^{Task}"
         :clock-in :clock-resume)
        ("p" "People task" entry
         (file+headline "~/personal/people.org" "Tasks")
         ,my/org-basic-task-template)
        ("q" "Quick note" item
         (file+headline "~/personal/organizer.org" "Quick notes")
         ,my/org-basic-task-template)
        ("t" "Tasks" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         ,my/org-basic-task-template)
        ("j" "Journal Note" entry
         (file (get-journal-file-today))
         "* %?\n\n  %i\n\n  From: %a" :empty-lines 1)
        ))

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
                      ("crypt" . ?C)
                      ("quantified" . ?q)
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

(defun my/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

(defvar my/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")
;;  (eval-after-load 'org
    '(defadvice org-agenda-finalize-entries (around sacha activate)
       (if my/org-agenda-limit-items
           (progn
             (setq list (mapcar 'org-agenda-highlight-todo list))
             (setq ad-return-value
                   (subseq list 0 my/org-agenda-limit-items))
             (when org-agenda-before-sorting-filter-function
               (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
             (setq ad-return-value
                   (mapconcat 'identity
                              (delq nil
                                    (subseq
                                     (sort list 'org-entries-lessp)
                                     0
                                     my/org-agenda-limit-items))
                              "\n")))
         ad-do-it));;)

(use-package org-journal
  :init
  (setq org-journal-dir "~/.journal/")
  (setq org-journal-file-format "%Y%m%d")
  (setq org-journal-date-format "%e %b %Y (%A)")
  (setq org-journal-time-format "")
  :config
  (defun get-journal-file-today ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y%m%d")))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-today ()
    "Create and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

  (defun get-journal-file-yesterday ()
    "Return filename for yesterday's journal entry."
    (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y%m%d" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))

  (global-set-key (kbd "C-c t") 'journal-file-today)
  (global-set-key (kbd "C-c y") 'journal-file-yesterday)

  (defun journal-last-year-file ()
    "Returns the string corresponding to the journal entry that
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

(require 'epa)
(custom-set-variables '(epg-gpg-program  "/usr/bin/gpg"))
(epa-file-enable)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)
(setq auto-save-default nil)

(global-set-key (kbd "C-c d") 'org-decrypt-entry)

(use-package artbollocks-mode
  :defer 5
  :load-path  "~/elisp/artbollocks-mode"
  :custom
  (artbollocks-weasel-words-regex
   (concat "\\b" (regexp-opt
                  '("one of the"
                    "should"
                    "just"
                    "sort of"
                    "a lot"
                    "probably"
                    "maybe"
                    "perhaps"
                    "I think"
                    "really"
                    "pretty"
                    "nice"
                    "action"
                    "utilize"
                    "leverage") t) "\\b"))
  ;; Don't show the art critic words, or at least until I figure
  ;; out my own jargon
  (artbollocks-jargon nil))

(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-limit 20)
  (company-tooltip-align-annotations 't)
  (company-idle-delay .1)
  (company-begin-commands '(self-insert-command)))

(use-package which-key
  :defer 2
  :config
  (which-key-mode))

(use-package expand-region
  :init (global-set-key (kbd "C-=") 'er/expand-region))

(use-package flycheck
  :defer t
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
  :config
  (global-hungry-delete-mode))

(use-package iedit)

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :custom
  (ledger-clear-whole-transactions 1))

(use-package flycheck-ledger
  :after ledger-mode)

(pdf-tools-install)

(use-package projectile
  :defer 5
  :diminish
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-on))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package skewer-mode
  :defer t
  :hook ((js2-mode-hook . skewer-mode)
        (css-mode-hook . skewer-css-mode)
        (html-mode-hook . skewer-html-mode)
        (web-mode-hook . skewer-html-mode)))

(use-package ivy
  :diminish (ivy-mode)
  :bind ("C-x b" . ivy-switch-buffer)
  :custom
  (ivy-mode 1)
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

  (use-package counsel
    :bind ("M-y" . counsel-yank-pop)))

(use-package simple-httpd
  :defer t
  :config
  (setq httpd-root "/var/www/html")

  (use-package impatient-mode
    :defer t
    :hook ((web-mode-hook . httpd-start)
           (web-mode-hook . impatient-mode)
           (css-mode-hook . httpd-start))))

(use-package smartparens
  :defer 5
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management

    (define-key sp-keymap (kbd "C-c s r n") 'sp-narrow-to-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    ;; (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (define-key sp-keymap (kbd "C-c s t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "C-c s p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "C-c s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "C-c s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "C-c s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "C-c s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "C-c s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "C-c s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "C-c s s") 'sp-split-sexp)

  ;;;;;;;;;;;;;;;;;;
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))

  ;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  ;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

  ;;; html-mode
    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))

  ;;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))

(use-package spotify
  :defer 5
  :config
  (spotify-enable-song-notifications))

(use-package try :defer 5)

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
  :hook ((css-mode-hook . rainbow-mode)
         (web-mode-hook . rainbow-mode))
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

;; (use-package key-chord
;;   :init
;;   (progn
;;     (setq key-chord-one-key-delay 0.16)
;;     (key-chord-mode 1)
;;     (key-chord-define-global "yy" 'my/window-movement/body)))

;; (use-package hydra
;;   :defer t
;;   :config
;;   (defhydra my/window-movement ()
;;     ("h" windmove-left)
;;     ("l" windmove-right)
;;     ("j" windmove-down)
;;     ("k" windmove-up)
;;     ("y" other-window "other")
;;     ("f" find-file "file")
;;     ("F" find-file-other-window "other file")
;;     ("v" (progn (split-window-right) (windmove-right)))
;;     ("o" delete-other-windows :color blue)
;;     ("a" ace-window)
;;     ("s" ace-swap-window)
;;     ("d" delete-window "delete")
;;     ("D" ace-delete-window "ace delete")
;;     ("i" ace-delete-other-windows)
;;     ("b" helm-buffers-list)
;;     ("q" nil))
;;   (defhydra join-lines ()
;;     ("n" join-line)
;;     ("p" (join-line 1))))

(use-package winner :defer 5)

(use-package yasnippet
  :defer 2
  :diminish yasnippet-mode
  :bind ("C-c i" . yas-insert-snippet)
  :init
  (yas-global-mode t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/yasnippet-snippets")
  :custom
  (yas-snippet-dirs '("~/.emacs.d/elisp/yasnippet-snippets"))
  (yas-installed-snippets-dir "~/.emacs.d/elisp/yasnippet-snippets"))

(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :hook ((emacs-lisp-mode-hook . turn-on-eldoc-mode)
         (lisp-interaction-mode-hook . turn-on-eldoc-mode)
         (ielm-mode-hook . turn-on-eldoc-mode)))

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
  :hook (sgml-mode-hook css-mode-hook web-mode-hook))

(use-package less-css-mode
  :mode "\\.less\\'"
  :interpreter ("less" . less-css-mode))

(use-package eclim
  :defer t
  :hook (java-mode-hook . eclim-mode)
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
  :config
  ;; Better imenu
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :defer t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
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

;; (use-package tex-site
;;   :ensure auctex
;;   :config
    ;; (use-package tex
    ;; :custom
    ;;    (TeX-auto-save t)
    ;;    (TeX-byte-compile t)
    ;;    (TeX-clean-confirm nil)
    ;;    (TeX-master 'dwim)
    ;;    (TeX-parse-self t)
    ;;    (TeX-view-program-selection '((output-pdf "Evince")
    ;;                                  (output-html "xdg-open")))
    ;;    (Tex-PDF-mode t)
    ;;    (TeX-source-correlate-mode t))

  ;; (use-package latex
  ;;     :requires (flyspell reftex)
  ;;     :custom
  ;;     (LaTeX-default-style "scrartcl")
  ;;     (LaTeX-default-options "version=last, paper=A4, parskip=half")
  ;;     (LaTeX-default-author user-full-name)
  ;;     :hook (LaTeX-mode . (LaTeX-math-mode flyspell-mode reftex-mode))))

(setq Tex-PDF-mode t)

(setq TeX-auto-save t
      TeX-parse-self t)

(setq-default TeX-engine 'xetex)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(setq TeX-source-correlate-method (quote synctex))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-selection '(((output-pdf "PDF Tools")
                                    ((output-dvi has-no-display-manager)
                                     "dvi2tty")
                                    ((output-dvi style-pstricks)
                                     "dvips and gv")
                                    (output-dvi "xdvi")
                                    (output-(point)df "Evince")
                                    (output-html "xdg-open"))))

(setq latex-run-command "xelatex -synctex=1 -interaction=nonstopmode --shell-escape")
(setq LaTeX-command "latex -synctex=1 -interaction=nonstopmode --shell-escape")

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
  :defer 2
  :hook ((python-mode-hook . anaconda-mode)
         (python-mode-hook . (lambda ()
                               (add-to-list 'company-backends 'company-anaconda)))))

(use-package company-anaconda
  :after anaconda-mode
  :hook (python-mode-hook . anaconda-mode))

(use-package sql-indent
  :mode "\\.sql\\'"
  :interpreter ("sql" . sql-mode))

(use-package erc
  :defer 10
  :bind ("C-c e" . erc-start-or-switch)
  :custom
  (erc-autojoin-channels-alist '(("freenode.net"
                                  "#android-dev" "#archlinux" "#bitcoin"
                                  "#bitcoin-pricetalk" "#emacs" "#latex"
                                  "#python" "#sway"
                                  )))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-prompt-for-nickserv-password nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  (erc-services-mode 1)
  (erc-track-mode t)
  (add-to-list 'erc-modules 'spelling)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'hl-nicks)
  (add-to-list 'erc-modules 'image))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image 
  :after erc)

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.freenode.net" :port 6667 :nick "rememberYou"))))

(defun my/display-four-channels ()
  "Lazy function permit to execute a bunch of commands
          to display four IRC channels in the same time."
  (interactive)
  (if (not (get-buffer "irc.freenode.net:6667"))
      (erc-start-or-switch)
    (delete-other-windows)
    (switch-to-buffer "#latex")
    (split-window-right)
    (windmove-right)
    (switch-to-buffer "#emacs")
    (split-window-below)
    (windmove-down)
    (switch-to-buffer "#python")
    (windmove-left)
    (split-window-below)
    (switch-to-buffer "#archlinux")))

(autoload 'notmuch "notmuch" "notmuch mail" t)
(use-package helm-notmuch
  :defer 5
  :bind ("C-x M-e" . helm-notmuch))

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
