;;; ~/.doom.d/+misc.el -*- lexical-binding: t; -*-

(setq counsel-search-engine 'google)

(def-package! counsel-tramp
  :commands (counsel-tramp))

;; use for sync my code
(add-hook 'after-save-hook (lambda () (call-process-shell-command "rc" nil 0)))

;; Use chrome to browse
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (cond (IS-MAC "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
            ((executable-find "/opt/google/chrome/chrome") "/opt/google/chrome/chrome")
            ((executable-find "google-chrome") "google-chrome")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make--ssh "argo-desktop" "ztlevi-5820")
;; (make--shell "argo-desktop" "ztlevi-5820")

(after! ssh-deploy
  (setq ssh-deploy-automatically-detect-remote-changes 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-cross-lines t
      evil-split-window-below t
      evil-vsplit-window-right t)

(use-package! evil-nerd-commenter :defer t)


(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))


(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer)
  (push 'prodigy-mode evil-snipe-disabled-modes))


(use-package! imenu-list
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))


(after! nav-flash
  ;; (defun nav-flash-show (&optional pos end-pos face delay)
  ;; ...
  ;; (let ((inhibit-point-motion-hooks t))
  ;; (goto-char pos)
  ;; (beginning-of-visual-line) ; work around args-out-of-range error when the target file is not opened
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

;; Use ) key to toggle it
(after! dired
  (remove-hook 'dired-after-readin-hook '+dired-enable-git-info-h))

(use-package! ranger
  :config
  (setq ranger-hide-cursor t
        ranger-show-hidden 'format
        ranger-deer-show-details nil)

  (defun ranger-close-and-kill-inactive-buffers ()
    "ranger close current buffer and kill inactive ranger buffers"
    (interactive)
    (ranger-close)
    (ranger-kill-buffers-without-window))
  ;; do not kill buffer if exists in windows
  (defun ranger-disable ()
    "Interactively disable ranger-mode."
    (interactive)
    (ranger-revert)))


(after! all-the-icons-dired
  (advice-add 'wdired-change-to-wdired-mode :before (λ! (all-the-icons-dired-mode -1)))
  (advice-add 'wdired-finish-edit :after (λ! (all-the-icons-dired-mode +1))))


(after! dash-docs
  (setq dash-docs-use-workaround-for-emacs-bug nil)
  (setq dash-docs-browser-func 'browse-url-generic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! ivy
  (after! ivy-prescient
    (setq ivy-prescient-retain-classic-highlighting t)))


(after! ivy-posframe
  ;; Lower internal-border-width on MacOS
  (when IS-MAC
    (setq ivy-posframe-border-width 5))

  ;; Use minibuffer to display ivy functions
  ;; (dolist (fn '(+ivy/switch-workspace-buffer
  ;;               ivy-switch-buffer))
  ;;   (setf (alist-get fn ivy-posframe-display-functions-alist) #'ivy-display-function-fallback))
  )


(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable
        counsel-rg-base-command "rg -zS --no-heading --line-number --max-columns 1000 --color never %s ."
        counsel-grep-base-command counsel-rg-base-command))


(use-package! counsel-etags
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (map!
   :nv "ge"  #'counsel-etags-find-tag-at-point)

  (setq counsel-etags-update-interval 60)
  (add-to-list 'counsel-etags-ignore-directories "build"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUICKRUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  ;; quickrun--language-alist
  (when IS-LINUX
    (quickrun-set-default "c++" "c++/g++")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! projectile
  (setq compilation-read-command nil)   ; no prompt in projectile-compile-project
  ;; . -> Build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s"
                                    :compile "cmake --build Debug"
                                    :test "ctest")

  ;; set projectile-known-projects after magit
  (after! magit
    (update-projectile-known-projects))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (after! magit
;;   (setq magit-repository-directories '(("~/Developer" . 2))
;;         magit-save-repository-buffers nil
;;         git-commit-style-convention-checks nil
;;         magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;;   (magit-wip-after-apply-mode t)
;;   (magit-wip-before-change-mode t))


(use-package! magit-todos
  :init
  (setq magit-todos-ignored-keywords nil)
  :config
  (setq magit-todos-exclude-globs '("third-party/*" "third_party/*")))


;; magit-todos uses hl-todo-keywords
(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("HACK"  . ,(face-foreground 'warning))
          ("TEMP"  . ,(face-foreground 'warning))
          ("DONE"  . ,(face-foreground 'success))
          ("NOTE"  . ,(face-foreground 'success))
          ("DONT"  . ,(face-foreground 'error))
          ("DEBUG"  . ,(face-foreground 'error))
          ("FAIL"  . ,(face-foreground 'error))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("XXXX"  . ,(face-foreground 'error)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-formatter! 'shfmt "shfmt -i=2")

(after! eshell
  ;; eshell-mode imenu index
  (add-hook! 'eshell-mode-hook (setq-local imenu-generic-expression '(("Prompt" " λ \\(.*\\)" 1))))

  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
  (defun eshell/ft (&optional arg) (treemacs arg))

  (defun eshell/up (&optional pattern)
    (let ((p (locate-dominating-file
              (f-parent default-directory)
              (lambda (p)
                (if pattern
                    (string-match-p pattern (f-base p))
                  t)))
             ))
      (eshell/pushd p)))
  )


(after! term
  ;; term-mode imenu index
  (add-hook! 'term-mode-hook (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1)))))


(use-package! vterm-toggle
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Youdao
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe
             youdao-dictionary-search-at-point
             youdao-dictionary-search-at-point+
             youdao-dictionary-search-from-input
             youdao-dictionary-search-and-replace
             youdao-dictionary-play-voice-at-point
             youdao-dictionary-play-voice-from-input)
  :init
  (map!
   (:when (display-graphic-p)
     :leader
     :prefix ("l" . "lookup")
     :desc "Lookup Youdao"
     :n "y"  #'youdao-dictionary-search-at-point-posframe)
   (:when (not (display-graphic-p))
     :leader
     :prefix ("l" . "lookup")
     :desc "Lookup Youdao"
     :n "y"  #'youdao-dictionary-search-at-point+)
   :leader
   :prefix ("l" . "lookup")
   :desc "Lookup Youdao Input"
   :n "Y"  #'youdao-dictionary-search-from-input)
  :config
  (setq url-automatic-caching t
        youdao-dictionary-search-history-file (concat doom-cache-dir "youdao.cache")
        youdao-dictionary-use-chinese-word-segmentation t)
  (map!
   :map youdao-dictionary-mode-map
   :n "q" #'quit-window
   :n "p" #'youdao-dictionary-play-voice-of-current-word
   :n "y" #'youdao-dictionary-play-voice-at-point))
