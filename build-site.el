;;; build-site.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Martin Rodriguez
;;
;; Author: Martin Rodriguez <mtrpdx@gmail.com>
;; Maintainer: Martin Rodriguez <mtrpdx@gmail.com>
;; Created: May 03, 2024
;; Modified: May 03, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/mtrpdx/mtrpdx.github.io
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; Initialize package sources
(require 'package)

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.

(setq package-user-dir (expand-file-name "./.packages"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Install dependencies
(require 'vc-git)
(require 'ox-publish)
(require 'subr-x)
(require 'cl-lib)

(use-package esxml
  :pin "melpa-stable"
  :ensure t)

(use-package htmlize
  :ensure t)

(setq user-full-name "Martin Rodriguez")
(setq user-mail-address "mtrpdx@gmail.com")

(defvar mtr/site-url (if (string-equal (getenv "CI") "true")
                        "" ;; Don't hardcode the domain
                      "http://localhost:8080")
  "The URL for the site being generated.")


;; Customize the HTML output
;; (setq org-html-validation-link nil
;;       org-html-head-include-scripts nil
;;       org-html-head-include-default-style nil
;;       org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

(defun mtr/site-header ()
  (list `(header (@ (class "site-header"))
                 (div (@ (class "container"))
                      (div (@ (class "site-title"))))
                 (div (@ (class "site-masthead"))
                      (div (@ (class "container"))
                           (nav (@ (class "nav"))
                                (a (@ (class "nav-link") (href "/")) "Home") " "
                                (a (@ (class "nav-link") (href "/")) "About") " "
                                (a (@ (class "nav-link") (href "/projects/")) "Projects") " "
                                (a (@ (class "nav-link") (href "/readinglist/")) "Reading List")))))))

(defun mtr/site-footer ()
  (list `(footer (@ (class "site-footer"))
                 (div (@ (class "container"))
                      (div (@ (class "row"))
                           (div (@ (class "column"))
                                (div (@ (class "site-footer-line"))
                                     " · "
                                     " · "
                                (div (@ (class "site-footer-line"))
                                     "© 2024 · mtrpdx"))))))))

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org\\|\\/404.org$" org-file)
        pub-dir
      (progn
        (unless (file-directory-p article-dir)
          (make-directory article-dir t))
        article-dir))))

(defun mtr/get-commit-hash ()
  "Get the short hash of the latest commit in the current repository."
  (string-trim-right
   (with-output-to-string
     (with-current-buffer standard-output
       (vc-git-command t nil nil "rev-parse" "--short" "HEAD")))))


(cl-defun mtr/generate-page (title
                             content
                             info
                             &key
                             (publish-date)
                             (head-extra)
                             (pre-content)
                             (exclude-header)
                             (exclude-footer))
  (concat
   "<!-- Generated from " (mtr/get-commit-hash)  " on " (format-time-string "%Y-%m-%d @ %H:%M") " with " org-export-creator-string " -->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
      (head
       (meta (@ (charset "utf-8")))
       (meta (@ (author "mtrpdx - Martin Rodriguez")))
       (meta (@ (name "viewport")
                (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
       (link (@ (rel "icon") (type "image/png") (href "/img/favicon.png")))
       (link (@ (rel "stylesheet") (href ,(concat mtr/site-url "/css/site.css"))))
       ;; Empty string to cause a closing </script> tag
       "")
      (body ,@(mtr/site-header))))))

(defun mtr/org-html-template (contents info)
  (mtr/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))


(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . mtr/org-html-template)))
    ;; (link . dw/org-html-link)
    ;; (src-block . dw/org-html-src-block)
    ;; (special-block . dw/org-html-special-block)
    ;; (headline . dw/org-html-headline))
  ;; :options-alist
  ;; '((:video "VIDEO" nil nil)
  ;;   (:page-type "PAGE-TYPE" nil nil)))

(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 ;; The 404 page is a special case, it must be named "404.html"
                 (concat article-path
                         (if (string= (file-name-nondirectory filename) "404.org") "404" "index")
                         extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-html-htmlize-output-type 'css
      org-html-prefer-user-labels t
      org-html-link-home mtr/site-url
      org-html-link-use-abs-url t
      org-html-link-org-files-as-html t
      org-html-html5-fancy t
      org-html-self-link-headlines t
      org-export-with-toc nil
      make-backup-files nil)

;; Define the publishing project
(setq org-publish-project-alist
      (list '("mtrpdx:main"
              :base-directory "./content"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-author nil
              :with-creator t
              :with-toc t
              :section-numbers nil
              :time-stamp-file nil)
            '("mtrpdx:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./public"
              :recursive t
              :publishing-function org-publish-attachment)
            '("mtrpdx:about"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-author nil
              :with-creator t
              :with-toc nil
              :section-numbers nil
              ;; :auto-sitemap t
              ;; :sitemap-filename "../about.org"
              :with-title nil
              :time-stamp-file nil
              :with-timestamps nil)
            '("mtrpdx:projects"
              :base-directory "./content/projects"
              :base-extension "org"
              :publishing-directory "./public/projects"
              :publishing-function org-html-publish-to-html
              :with-author nil
              :with-creator t
              :with-toc nil
              :section-numbers nil
              ;; :auto-sitemap t
              ;; :sitemap-filename "../projects.org"
              :with-title nil
              :time-stamp-file nil
              :with-timestamps nil)
            '("mtrpdx:readinglist"
              :base-directory "./content/readinglist"
              :base-extension "org"
              :publishing-directory "./public/readinglist"
              :publishing-function org-html-publish-to-html
              :with-author nil
              :with-creator t
              :with-toc nil
              :section-numbers nil
              ;; :auto-sitemap t
              ;; :sitemap-filename "../readinglist.org"
              :with-title nil
              :time-stamp-file nil
              :with-timestamps nil)))


;; Generate the site output
(org-publish-remove-all-timestamps)
(org-publish-all t)

(message "Build complete!")

(provide 'build-site)
;; ;;; build-site.el ends here
