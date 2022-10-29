;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Wenxiang Sun"
      user-mail-address "swx@tsgzj.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family  "Cascadia Code PL"
                            :size 15
                            :weight 'light
                            :width 'narrow))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Don't confirm to kill emacs
(setq confirm-kill-emacs nil)

;; (add-hook 'emacs-startup-hook 'eshell)
(display-time-mode 1)
(nano-modeline-mode t)
(setq default-frame-alist
      (append (list
               '(height     . 45)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 8)
               '(left-fringe    . 1)
               '(right-fringe   . 1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))
(setq doom-theme 'doom-one-light)

(setq leetcode-prefer-language "rust")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/Git/leetcode_rust")
(setq rustic-lsp-server 'rust-analyzer)

(after! haskell
    (setq lsp-haskell-formatting-provider "brittany"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section contains configuration for org mode ;;
;; 1. GTD                                           ;;
;; 2. Zettlekasten                                  ;;
;; * org-roam                                       ;;
;; * deft                                           ;;
;; 3. Babel
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-inbox "~/org/inbox.org")

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/Zettelkasten/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :bind (("C-c n i" . vulpea-insert)
         ("C-c n f" . vulpea-find)))

(after! (org-agenda org-roam)
  (defun vulpea-task-p ()
    "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-task-update-tag ()
    "Update task tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-task-p)
              (setq tags (cons "task" tags))
            (setq tags (remove "task" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-task-files ()
    "Return a list of note files containing 'task' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (or (like tag (quote "%\"task\"%"))
                   (like tag (quote "%\"schedule\"%")))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-task-files)))

  (add-hook 'find-file-hook #'vulpea-task-update-tag)
  (add-hook 'before-save-hook #'vulpea-task-update-tag)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update))

;; Ignoring popup rule
(after! org
  (setq org-src-window-setup 'current-window)
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (setq org-todo-keywords
  '((sequence
     "TODO(t!)" ; Initial creation
     "STR(s@)"; Work in progress
     "WAIT(w@)" ; My choice to pause task
     "BLOCKED(b@)" ; Not my choice to pause task
     "REVIEW(r!)" ; Inspect or Share Time
     "|" ; Remaining close task
     "DONE(d@)" ; Normal completion
     "CANCELED(c@)" ; Not going to od it
     "DUPLICATE(p@)" ; Already did it
     ))))

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(after! org-modern
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

(setq deft-directory "~/org/Zettelkasten")

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 20)
   (internal-border-width . 20)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis ""

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?-
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ????? " "???????????????")
 org-agenda-current-time-string
 "? now -------------------------------------------------")

(global-org-modern-mode)
(evil-set-initial-state 'eshell 'normal)

;; haskell symbols
(global-pretty-mode t)
(pretty-deactivate-groups
 '(:punctuation :arrows :set :logic :arrows-twoheaded :ordering :equality))
(pretty-activate-groups
 '(:greek :arithmetic-nary :sub-and-superscripts))
(add-hook 'haskell-mode-hook
          (lambda ()
            (mapc (lambda (pair) (push pair prettify-symbols-alist))
                  doom/haskell-pretty-alist)))

(add-hook 'dap-stopped-hook (lambda () (call-interactively #'dap-hydra)))

(defvar doom/haskell-pretty-alist
  '(
    ;; Type
                                        ;("Bool" .  #x1D539)
    ("Integer" . #x2124)
    ;; True/False
    ("True" . #x27D9)
    ("False" . #x27D8)
    ;; Rterun/Join
    ("return" . #x03B7)
    ("join" . #x03BC)
    ;; Quantifier
    ("forall" . #x2200)
    ;; Set
    ("elem" . #x2208)
    ("notElem" . #x2209)
    ("isSubsetOf" . #x2287)
    ("union" . #x222A)
    ;; Ring
    (" \. " . (?\s (Br . Bl) ?\s (Bc . Bc) ?\u2218))
    )) ;; need to add more space
;;
;; dap mode
;; forcing load dap-go which is not automatically loaded after go-mode
(add-hook 'go-mode-hook
          (lambda ()
            (require 'dap-go)))
;; launch dap-hydra
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;; Geiser chez
(setq geiser-chez-binary
      (if (eq system-type 'darwin)
          "chez"
        "chez-scheme"))

;; enable 3 state org-cycle
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc"
     "425cf02839fa7c5ebd6cb11f8074f6b8463ae6ed3eeb4cf5a2b18ffc33383b0b"
     "25f1b2ace87d23d803b42267fafdc38b31472e444c2aaa9069aa2c06be8955b2"
     "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1"
     "9f15d03580b08dae41a1e5c1f00d1f1aa99fea121ca32c28e2abec9563c6e32c"
     "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e"
     "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7"
     "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086"
     "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
