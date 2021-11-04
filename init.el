(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/11:/opt/homebrew/lib/gcc/11/gcc/aarch64-apple-darwin20/11.1.0")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering)

(setq custom-file "~/.emacs.d/custom.el")
 (if (file-exists-p custom-file)
     (message (concat  "File " (concat custom-file " already exists")))
   (with-temp-buffer (write-file custom-file)))
(load custom-file)

(menu-bar-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(toggle-frame-maximized)
(set-fringe-mode 10)

(column-number-mode t)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :family "Iosevka Term" :height 170)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Fixed" :height 170)
(set-face-attribute 'variable-pitch nil :family "Iosevka" :height 170)

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el" :files ("*.el" "*"))
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 20))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package solarized-theme)
(setq solarized-use-more-italic t)
(setq solarized-scale-markdown-headlines t)
(load-theme 'solarized-gruvbox-light)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package magit)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package hydra)
(defhydra hydra-text-scale (global-map "<f2>")
  "scale text"
  ("C-p" text-scale-increase "in")
  ("C-n" text-scale-decrease "out"))

(use-package company
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(global-company-mode t)

(defun text-mode-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)

  ;; company-ispell is the plugin to complete words
  (add-to-list 'company-backends 'company-ispell)
  )

(add-hook 'text-mode-hook 'text-mode-hook-setup)
(global-set-key (kbd "C-:") #'ispell)

(use-package yasnippet)
(yas-global-mode 1)

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(use-package whole-line-or-region
  :straight (whole-line-or-region :type git :host github :repo "purcell/whole-line-or-region" :files ("*.el" "*")))
(whole-line-or-region-global-mode t)

(use-package project)

(defun phl/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . phl/org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("~/Documents/ok/agenda/Tasks.org" "~/Documents/ok/agenda/Habits.org"))
  (setq org-ellipsis " ⤵"
	org-hide-emphasis-markers t)
  (setq org-todo-keywords
    '((sequence "BACKLOG(b)" "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  
  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  :bind ("C-c a" . org-agenda))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/ok")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
(global-set-key (kbd "C-c n I") #'org-roam-node-insert-immediate)

;; Automatically tangle our Emacs.org config file when we save it
(defun phl/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'phl/org-babel-tangle-config)))

(use-package eglot)

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
	      ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (setq flutter-sdk-path "/Users/100phlecs/packages/flutter/"))

(use-package dart-mode)

(use-package activity-watch-mode)
(global-activity-watch-mode)

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil)
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))
