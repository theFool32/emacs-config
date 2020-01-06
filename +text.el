;;;  -*- lexical-binding: t; -*-

(add-hook! 'text-mode-hook (setq-local truncate-lines nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.ratpoisonrc$" . sh-mode))

(setq org-directory (expand-file-name "~/Dropbox/org-notes")
      org-agenda-files (list org-directory)
      org-ellipsis " ▼ "
      org-babel-python-command "python3"
      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Markdown #-marks for headlines are more elegant.
      org-bullets-bullet-list '("#"))

(after! org
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("j" "Journal" entry
           (file+headline +org-capture-journal-file "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("p" "Paper" entry
           (file+headline "~/Dropbox/org-notes/papers.org" "Papers")
           "* %u %?\n%i" :prepend t :kill-buffer t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; ("p" "Templates for projects")
          ("pt" "Project todo" entry    ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry   ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i" :prepend t :kill-buffer t)))

  (setq org-log-into-drawer "LOGBOOK")


  ;; Schedule/deadline popup with default time
  (defvar org-default-time "10:30"
    "The default time for deadlines.")

  (defun advise-org-default-time (func arg &optional time)
    (let ((old-time (symbol-function #'org-read-date)))
      (cl-letf (((symbol-function #'org-read-date)
                 #'(lambda (&optional a b c d default-time f g)
                     (let ((default-time (or default-time
                                             org-default-time)))
                       (apply old-time a b c d f default-time g)
                       ))))
        (apply func arg time))))

  (advice-add #'org-deadline :around #'advise-org-default-time)
  (advice-add #'org-schedule :around #'advise-org-default-time)


  (setq bibtex-completion-bibliography '( "~/Dropbox/Paper/egbib.bib" ) ;the major bibtex file
        bibtex-completion-library-path "~/Dropbox/org-notes/reference/pdf/" ;the directory to store pdfs
        bibtex-completion-notes-path "~/Dropbox/org-notes/ref.org" ;the note file for reference notes
        ;; org-directory "~/Dropbox/org"
        org-ref-default-bibliography '( "~/Dropbox/Paper/egbib.bib" )
        org-ref-bibliography-notes "~/Dropbox/org-notes/ref.org"
        org-ref-pdf-directory "~/Dropbox/org-notes/reference/pdf/"
        )


  ;; org 截图
  ;; https://pengpengxp.github.io/archive/before-2018-11-10/2017-02-23-org-mode-screenshot-management.html
  (defcustom peng-org-screenshot-dir-name  "img"
    "default image directory name for org screenshot"
    :type 'string
    )

  (defun peng-org-screenshot ()
    (interactive)
    "Take a screenshot into a user specified file in the current
       buffer file directory and insert a link to this file."
    (let* ((img-dir peng-org-screenshot-dir-name))
      (progn
        (if (file-exists-p img-dir)
            (print "yes")
          (mkdir img-dir))
        (let ((temp-name (ivy-read "please selete a image name"
                                   (delete ".." (delete "." (directory-files img-dir))))))
          (setq filename (concat img-dir "/" (file-name-base temp-name) ".png"))
          (call-process-shell-command "screencapture" nil nil nil nil "-i" (concat
                                                                            "\"" filename "\"" ))
          (insert (concat "[[./" filename "]]"))))))

  (defun peng-find-org-link-begin-and-end (plist string)
    "find link from plist whose link is equal to string, return a
  list just like `((name begin-position end-position))'"
    (let ((return-list '()))
      (progn
        (while plist
          (progn
            (if (string-equal (car (car plist))
                              string)
                (add-to-list 'return-list (cdr (car plist))))
            (setq plist (cdr plist))))
        return-list)))

  (defun peng-do-delete-link-function (be-list)
    "goto the begining of link and delete it, be-list is a list
  just like `((name begin-position end-position))'"
    (while be-list
      (progn
        (goto-char (car (car be-list)))
        (delete-char (- (car (cdr (car be-list)))
                        (car (car be-list))))
        (setq be-list (cdr be-list)))))

  (defun peng-delete-org-screenshot-image-file-and-link ()
    (interactive)
    (let* ((link-list (org-element-map (org-element-parse-buffer) 'link
                        (lambda (link)
                          (when (string= (org-element-property :type link) "file")
                            (list (org-element-property :path link)
                                  (org-element-property :begin link)
                                  (org-element-property :end link))))))
           (img-dir peng-org-screenshot-dir-name)
           (temp-name (concat "./" img-dir "/"
                                   (ivy-read "please selete a image name you want to delete"
                                             (delete ".." (delete "." (directory-files img-dir))))))
           (begin-end-list (peng-find-org-link-begin-and-end link-list temp-name)))
      (progn
        (if (yes-or-no-p "Do you really want to delete the image file? This can't be revert!!")
            (delete-file temp-name))
        (if (yes-or-no-p "Do you also want to delete the image links?")
            (peng-do-delete-link-function begin-end-list)))))
  )


(use-package! org-wild-notifier
  :defer t
  :init
  (add-hook 'doom-after-init-modules-hook #'org-wild-notifier-mode t)
  :config
  (setq org-wild-notifier-alert-time 5
        alert-default-style (if IS-MAC 'osx-notifier 'libnotify)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(remove-hook 'text-mode-hook #'auto-fill-mode)


(use-package! edit-indirect :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! youdao-dictionary
  :defer t
  :config
  ;; Enable Cache
  (setq url-automatic-caching t
        ;; Set file path for saving search history
        youdao-dictionary-search-history-file
        (concat doom-cache-dir ".youdao")
        ;; Enable Chinese word segmentation support
        youdao-dictionary-use-chinese-word-segmentation t))

(use-package! tldr
  :defer t
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t)
  )

(use-package! link-hint :defer t)

(use-package! symbol-overlay :defer t)
