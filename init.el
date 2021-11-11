;; -*- lexical-binding: t -*-

(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/11:/opt/homebrew/lib/gcc/11/gcc/aarch64-apple-darwin20/11.1.0")

(setq gc-cons-threshold (* 50 1000 1000))
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

(add-hook 'emacs-startup-hook
        (lambda ()
          (message "Emacs ready in %s with %d garbage collections."
                   (format "%.2f seconds"
                           (float-time
                            (time-subtract after-init-time before-init-time)))
                   gcs-done)))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package diminish)

(menu-bar-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(toggle-frame-maximized)
(set-fringe-mode 10)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)


(setq ring-bell-function 'ignore)

(column-number-mode t)
(setq display-line-numbers-type 'relative)

(add-hook 'prog-mode-hook (lambda()
                            (display-line-numbers-mode)
                            ))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 24)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package solarized-theme
  :after moody
  :config
  (setq solarized-use-more-italic t)
  (setq solarized-scale-markdown-headlines t))

(defun phl-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'solarized-gruvbox-light t))
    ('dark (load-theme 'solarized-gruvbox-dark t)))
  ;; preserve syntax highlighting
  (set-face-background 'region (face-attribute 'highlight :background))
  (set-face-foreground 'region nil)
  (setq moody-line (face-attribute 'mode-line :underline))
  (set-face-attribute 'mode-line          nil :overline   moody-line)
  (set-face-attribute 'mode-line-inactive nil :overline   moody-line)
  (set-face-attribute 'mode-line-inactive nil :underline  moody-line)
  (setq show-paren-priority -50)
  (set-face-attribute 'mode-line          nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :box        nil)
  (phl-fix-bookmark))

(defun phl-fix-bookmark ()
  "Set bookmark appearance after load"
  (set-face-foreground 'bookmark-face (face-attribute 'default :foreground))
  (set-face-background 'bookmark-face (face-attribute 'default :background)))

(add-hook 'bookmark-load-hook #'phl-fix-bookmark)
(add-hook 'ns-system-appearance-change-functions #'phl-apply-theme)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package magit)

(use-package expand-region
  :bind(
  ("C-=" . er/expand-region)))

(global-set-key (kbd "C-x M-k") #'kill-this-buffer)
(global-set-key (kbd "C-c s") #'ispell)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package hydra)
(defhydra hydra-text-scale (global-map "<f2>")
  "scale text"
  ("C-p" text-scale-increase "in")
  ("C-n" text-scale-decrease "out"))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

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

(use-package yasnippet
  :init (yas-global-mode 1))
(use-package doom-snippets
:after yasnippet
:straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(use-package whole-line-or-region
  :straight (whole-line-or-region :type git :host github :repo "purcell/whole-line-or-region" :files ("*.el" "*")))
(whole-line-or-region-global-mode t)

(use-package project
  :after magit
  :init
  (setq project-switch-commands
    '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-find-dir "Find directory" nil)
     (project-vc-dir "VC-Dir" nil)
     (project-eshell "Eshell" nil)
     (magit-status "Magit" ?m))))

(use-package popper
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*pomidor\\*"
          "\\*Backtrace\\*"
          pomidor-mode
          "\\*Warnings\\*"
          "^\\*eshell.*\\*$"
          eshell-mode
          flutter-mode
          helpful-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  (setq popper-echo-dispatch-keys
        '("C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9" "C-0"))

  (defun phl-popper-kill-buffer ()
    "Kill selected popper buffer without closing popper."
    (interactive)
    (popper-kill-latest-popup)
    (popper-toggle-latest))

  (defun phl-popper-maximize-buffer ()
    "Maximize selected popper buffer within frame."
    (interactive)
    (popper-toggle-type)
    (maximize-window))

  :bind (("M-`"   . popper-toggle-latest)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         ("C-^" . phl-popper-maximize-buffer)
         ("C-~" . phl-popper-kill-buffer))
  )

(use-package vertico
  :init
  (vertico-mode)
  (defun phl-minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word."
    (interactive "p")
    (if minibuffer-completing-file-name
        (if (string-match-p "/." (minibuffer-contents))
            (zap-up-to-char (- arg) ?/)
          (delete-minibuffer-contents))
      (delete-word (- arg))))

  :bind (:map vertico-map
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-DEL" . phl-minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  (custom-set-faces '(vertico-current ((t (:background "#3a3f5a"))))))

(use-package orderless
:init
;; Configure a custom style dispatcher (see the Consult wiki)
;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;       orderless-component-separator #'orderless-escapable-split-on-space)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion))))))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  ;;:hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
    ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
    ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
    ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package consult-yasnippet
  :bind ("C-x C-y" . consult-yasnippet))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :bind (("M-o" . embark-act)
         ("M-C-o" . embark-export))
  :config
  (setq embark-cycle-key (kbd "O"))
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (define-key embark-command-map "f" #'helpful-function)
  )

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
    The which-key help message will show the type and value of the
    current target followed by an ellipsis if there are further
    targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))


(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
  :demand
  :bind (("C-;" . avy-goto-char-timer)
         ("C-:" . avy-isearch)))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)
(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
(setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
(setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
      (alist-get ?w avy-dispatch-alist) 'avy-action-copy
      (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
      (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
(setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)
(setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)
(setf (alist-get ?o avy-dispatch-alist) 'avy-action-embark)

(defun phl-org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . phl-org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ⤵"
  org-hide-emphasis-markers t)
  (setq org-todo-keywords
    '((sequence "BACKLOG(b)" "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "HABIT(h)" "|" "CHECKED(c)")))

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
  :demand
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/notes")
  (org-roam-completion-everywhere t)

  (defun phl-org-roam-rg ()
    "Search across the content of the root org dir."
    (interactive)
    (consult-ripgrep org-roam-directory))

  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))


  (org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain (file ,(concat org-roam-directory "/Templates/BookTemplate.org"))
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("a" "design notes" plain
      (file ,(concat org-roam-directory "/Templates/DesignAnalysisTemplate.org"))
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     )
   )

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . phl-org-roam-rg)
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

(use-package org-make-toc)

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
(global-set-key (kbd "C-c n I") #'org-roam-node-insert-immediate)

(org-babel-do-load-languages
 'org-babel-load-languagesp
 '((emacs-lisp . t)
   (python . t)))
(setq org-src-tab-acts-natively t)
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our .org config file when we save it
(defun phl-org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/README.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'phl-org-babel-tangle-config)))

(defun phl-org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun phl-org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (phl-org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun phl-org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (append (phl-org-roam-list-notes-by-tag "Project")
                                 '("~/Documents/notes/agenda/Tasks.org"
                                   "~/Documents/notes/agenda/Habits.org"))
        )
  )

;; Build the agenda list the first time for the session
(phl-org-roam-refresh-agenda-list)

(defun phl-org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
         capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'phl-org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun phl-org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'phl-org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (phl-org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun phl-org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun phl-org-roam-capture-project-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'phl-org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (phl-org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun phl-org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (phl-org-roam-copy-todo-to-today))))

(global-set-key (kbd "C-c n t") #'phl-org-roam-capture-project-task)
(global-set-key (kbd "C-c n n") #'phl-org-roam-capture-inbox)
(global-set-key (kbd "C-c n p") #'phl-org-roam-find-project)

(defun phl-start-new-eshell ()
  "Spawn a new eshell always."
  (interactive)
  (eshell)
  (rename-uniquely))

(global-set-key (kbd "C-c e") #'phl-start-new-eshell)

(defun phl-configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . phl-configure-eshell))

(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :init
  (setq lsp-keymap-prefix "C-c l")

  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
        ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (setq flutter-sdk-path "/Users/100phlecs/packages/flutter/"))

;; [[file:README.org::*dart-mode][dart-mode:1]]
(use-package dart-mode
  :hook (dart-mode . lsp))

;; UI and such, sine they're dependences of lsp-dart
(use-package flycheck)
(use-package treemacs)
(use-package lsp-treemacs)
(use-package lsp-dart
  :init
  (setq lsp-dart-sdk-dir "/Users/100phlecs/packages/flutter/bin/cache/dart-sdk")
  (setq lsp-dart-flutter-sdk-dir "/Users/100phlecs/packages/flutter")
  (setq lsp-dart-enable-sdk-formatter t))
;; dart-mode:1 ends here

(use-package sly)
(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")

(use-package lispy)
(setq lispy-use-sly t)

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c r" . rustic-compile))
  :config
  (progn
  ;; (setq rustic-lsp-setup-p nil)
    (setq rustic-lsp-server 'rust-analyzer)
    (setq rustic-format-on-save nil)
    (setq rustic-indent-offset 2)
    (electric-pair-mode 1)))

(use-package esup
  :config
  (setq esup-depth 0))

(use-package activity-watch-mode
  :init
   (global-activity-watch-mode)
  :diminish activity-watch-mode)

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
(setq gc-cons-threshold (* 2 1000 1000))
