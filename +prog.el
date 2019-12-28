;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BETTER EDIT EXP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smart kill parens
(use-package! elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Hungry Delete
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))


(def-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

;; (use-package! company-tabnine
;;   :after company
;;   :config
;;   (cl-pushnew 'company-tabnine (default-value 'company-backends)))

(after! company
  ;; (company-flx-mode +1)
  ;; (global-company-fuzzy-mode 1)
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t
        ;; company-global-modes '(not eshell-mode)
        company-tooltip-minimum-width 80
        company-tooltip-minimum 5
        company-quickhelp-delay nil
        company-frontends '(
                            ;; company-tng-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend
                            company-echo-metadata-frontend
                            ))
  (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  )

;; Set specific company-backends for python and tex
(add-hook  'python-mode-hook
          (lambda ()
            (setq company-backends '(company-files
                                     company-dabbrev
                                     company-anaconda))
            )
          )
(setq +lsp-company-backend '(company-lsp company-tabnine))
;; (setq +company-backend-alist
;;   '((text-mode company-tabnine company-dabbrev company-yasnippet company-ispell)
;;     (prog-mode company-capf company-yasnippet)
;;     (conf-mode company-capf company-dabbrev-code company-yasnippet)))
(set-company-backend! 'text-mode 'company-tabnine 'company-yasnippet 'company-ispell 'company-dabbrev)
;; (add-hook 'TeX-mode-hook
;;           (lambda ()
;;             (setq company-backends '(company-reftex-labels company-reftex-citations
;;                                      (+latex-symbols-company-backend company-auctex-macros company-auctex-environments)
;;                                      company-tabnine company-yasnippet company-ispell company-dabbrev company-capf))
;;             )
;;           )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar cspell-base-program "cspell")
;; (defvar cspell-config-file-path (concat "'" (expand-file-name  "~/Dotfiles/misc/apps/.cspell.json") "'"))
;; (defvar cspell-args (string-join `("--config" ,cspell-config-file-path) " "))
;; (defun cspell-check-buffer ()
;;   (interactive)
;;   (if cspell-base-program
;;       (let* ((file-name (concat "'" (file-name-nondirectory (buffer-file-name)) "'"))
;;              (command (string-join `(,cspell-base-program ,cspell-args ,file-name) " ")))
;;         (compilation-start command 'grep-mode))
;;     (message "Cannot find cspell, please install with `npm install -g csepll`")
;;     ))

;; (defun cspell-check-directory ()
;;   (interactive)
;;   (if cspell-base-program
;;       (let* ((project-root (doom-project-root))
;;              (default-directory
;;                (if (string-match-p "av/detection" project-root)
;;                    (expand-file-name "~/av")
;;                  project-root))
;;              (command (string-join `("git diff --name-only origin/develop | xargs -I{}" ,cspell-base-program ,cspell-args "'{}'") " ")))
;;         (compilation-start command 'grep-mode))
;;     (message "Cannot find cspell, please install with `npm install -g csepll`")))


(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  json-jsonlist json-python-json
                  c/c++-clang c/c++-cppcheck c/c++-gcc
                  python-pylint python-pycompile
                  ))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; ======================== Python ========================
  (require 'flycheck-mypy)

  ;; ======================== CC ========================
  (require 'flycheck-google-cpplint)
  (setq flycheck-c/c++-googlelint-executable "cpplint")
  (flycheck-add-next-checker 'c/c++-gcc '(t . c/c++-googlelint))

  (setq flycheck-c/c++-gcc-executable "gcc-7"
        flycheck-gcc-include-path '("/usr/local/inclue"))

  (add-hook! c++-mode-hook
    (setq flycheck-gcc-language-standard "c++11"
          flycheck-clang-language-standard "c++11"))
  )

(defun disable-flycheck-mode ()
  (flycheck-mode -1))
;; (add-hook! (emacs-lisp-mode) 'disable-flycheck-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! bazel-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("BUILD\\(\\.bazel\\)?\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode)) )

(add-to-list 'auto-mode-alist '("\\.inl\\'" . +cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

(after! cc-mode
  (c-add-style
   "my-cc" '("user"
             (c-basic-offset . 2)
             (c-offsets-alist
              . ((innamespace . 0)
                 (access-label . -)
                 (case-label . 0)
                 (member-init-intro . +)
                 (topmost-intro . 0)
                 (arglist-cont-nonempty . +)))))
  (setq c-default-style "my-cc")

  (setq-default c-basic-offset 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq lsp-python-ms-nupkg-channel "beta")
(setq lsp-python-ms-dir "~/.local/mspyls")
(define-derived-mode asl-mode
  python-mode "ARGO Schema Language Mode"
  "Major mode for asl file."
  (flycheck-mode -1))
(add-to-list 'auto-mode-alist '("\\.asl\\'" . asl-mode))

(after! python
  (setq python-indent-offset 4
        importmagic-python-interpreter "python3"
        python-shell-interpreter "python3"
        pippel-python-command "python3"
        conda-env-home-directory (expand-file-name "~/.conda")
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8"))

  ;; if you use pyton2, then you could comment the following 2 lines
  ;; (setq python-shell-interpreter "python2"
  ;;       python-shell-interpreter-args "-i")

  ;; ignore some linting info
  ;; (if (featurep! :tools lsp)
  ;;     (setq lsp-pyls-plugins-pycodestyle-ignore '("E501")
  ;;           lsp-pyls-plugins-pylint-args [ "--errors-only" ]))
  ;; )


(after! lsp-python-ms
  (setq lsp-python-ms-python-executable-cmd "python3"))


(use-package! py-isort
  :defer t
  :init
  (setq python-sort-imports-on-save t)
  (defun +python/python-sort-imports ()
    (interactive)
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook #'+python/python-sort-imports nil t))
  )


(use-package! importmagic
  :defer t
  :hook (python-mode . importmagic-mode)
  :commands (importmagic-fix-imports importmagic-fix-symbol-at-point)
  :config
  (dolist (func '(importmagic-fix-imports importmagic-fix-symbol-at-point))
    (advice-add func :before #'revert-buffer-no-confirm)))


(after! pipenv
  (setq pipenv-with-projectile t)
  ;; Override pipenv--clean-response to trim color codes
  (defun pipenv--clean-response (response)
    "Clean up RESPONSE from shell command."
    (replace-regexp-in-string "\n\\[0m$" "" (s-chomp response)))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pipenv-activate pipenv-deactivate))
    (advice-add func :after #'reset-flycheck)))


;; (after! conda
;;   (when IS-LINUX
;;     ;; Ubuntu anaconda
;;     (setq conda-anaconda-home "~/anaconda3"))

;;   ;; restart flycheck-mode after env activate and deactivate
;;   (dolist (func '(conda-env-activate conda-env-deactivate))
;;     (advice-add func :after #'reset-flycheck)))


;; For pytest-mode
(set-evil-initial-state! '(comint-mode) 'normal)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP & DAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! lsp-mode
  (setq lsp-use-native-json t
        lsp-print-io nil)
  (dolist (dir '("[/\\\\]\\.ccls-cache$"
                 "[/\\\\]\\.mypy_cache$"
                 "[/\\\\]\\.pytest_cache$"
                 "[/\\\\]\\.cache$"
                 "[/\\\\]\\.clwb$"
                 "[/\\\\]_build$"
                 "[/\\\\]__pycache__$"
                 "[/\\\\]bazel-bin$"
                 "[/\\\\]bazel-code$"
                 "[/\\\\]bazel-genfiles$"
                 "[/\\\\]bazel-out$"
                 "[/\\\\]bazel-testlogs$"
                 "[/\\\\]third_party$"
                 "[/\\\\]third-party$"
                 ))
    (push dir lsp-file-watch-ignored))
  )


(add-hook 'lsp-ui-mode-hook (λ!! #'lsp-ui-doc-mode -1))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100
        lsp-ui-doc-position 'at-point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))


(after! realgud (advice-remove #'realgud:terminate #'+debugger--cleanup-after-realgud-a))


(defun +my/dap-start ()
  (interactive)
  (dap-mode 1)
  (call-interactively #'dap-debug))

(add-hook! dap-mode-hook ((dap-tooltip-mode 1) (tooltip-mode 1)))

(after! dap-mode
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! latex
  :init
  :config
  (setq TeX-PDF-mode t)

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf -pvc -view=none %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  (eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Viewer"))))
  )
