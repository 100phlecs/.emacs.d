(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/11:/opt/homebrew/lib/gcc/11/gcc/aarch64-apple-darwin20/11.1.0")
(menu-bar-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(toggle-frame-maximized)
(set-fringe-mode 10)
(setq ns-auto-hide-menu-bar nil)
(set-face-attribute 'default nil :family "Iosevka Term" :height 170)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Fixed" :height 170)
(set-face-attribute 'variable-pitch nil :family "Iosevka" :height 170)

(fset 'yes-or-no-p 'y-or-n-p)                                   ; substitute y-or-n for yes-or-no

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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Dont start searches with ^

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

(use-package solarized-theme)
(setq solarized-use-more-italic t)
(setq solarized-scale-markdown-headlines t)
(load-theme 'solarized-gruvbox-light)

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


(column-number-mode t)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 20))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package ivy
  :bind (("C-s" . swiper))
  :config (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package hydra)

(defhydra hydra-text-scale (global-map "<f2>")
  "scale text"
  ("C-p" text-scale-increase "in")
  ("C-n" text-scale-decrease "out"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq proejectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))


(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; need to set up
;; to browse issues & PRs
(use-package forge)



(defun efs/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 1)
  (visual-line-mode 1))
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("~/Documents/ok/agenda/Tasks.org" "~/Documents/ok/agenda/Habits.org"))
  (setq org-ellipsis " ⤵"
	org-hide-emphasis-markers t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  
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


(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package dart-mode)
(use-package lsp-dart)
(use-package dap-mode)
(use-package flycheck)
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-treemacs)
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))
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


 
(setq lsp-dart-sdk-dir "/Users/100phlecs/packages/flutter/bin/cache/dart-sdk")
(setq lsp-dart-flutter-sdk-dir "/Users/100phlecs/packages/flutter")
(add-hook 'dart-mode-hook 'lsp)
(setq lsp-dart-dap-flutter-hot-reload-on-save t)
(setq lsp-dart-enable-sdk-formatter t)
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defcustom asdf-path "$HOME/.asdf"
  "Path to asdf directory."
  :type 'string
  :group 'asdf)

(defcustom asdf-binary "$HOME/.asdf/bin/asdf"
  "Path to asdf binary."
  :type 'string
  :group 'asdf)

(defun asdf-enable ()
  "Setup asdf for environment."
  (interactive)
  (let ((path (substitute-env-vars (concat asdf-path "/shims:" asdf-path "/bin:$PATH"))))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(asdf-enable)

(set-exec-path-from-shell-PATH)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil)

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
	      ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Users/100phlecs/packages/flutter/"))

(use-package yasnippet)
(yas-global-mode 1)

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(use-package whole-line-or-region
  :straight (whole-line-or-region :type git :host github :repo "purcell/whole-line-or-region" :files ("*.el" "*")))
(whole-line-or-region-global-mode t)

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

(use-package sly)
(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")

(use-package activity-watch-mode)
(global-activity-watch-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
