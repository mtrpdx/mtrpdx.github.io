;;; build-site.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Martin Rodriguez
;;
;; Author: Martin Rodriguez <mtrpdx@gmail.com>
;; Maintainer: Martin Rodriguez <mtrpdx@gmail.com>
;; Created: May 03, 2024
;; Modified: May 03, 2024
;; Version: 0.0.1
;; Keywords: website
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
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
      ;; org-html-head "<link rel=\"stylesheet\" href=\"assets/css/style.css\" />")

;; (defun mtr/site-header ()
;;   (list `(header (@ (class "site-header"))
;;                  (div (@ (class "container"))
;;                       (div (@ (class "site-title"))
;;                            (img (@ (class "logo")
;;                                    (src ,(concat mtr/site-url "/img/mtr_egg.png"))
;;                                    (alt "> mtrpdx/")))))
;;                  (div (@ (class "site-masthead"))
;;                       (div (@ (class "container"))
;;                            (div (@ (class "nav-wrapper"))
;;                                 (ul (@ (class "list list-main"))
;;                                      (a (@ (class "nav-link") (href "/")) "Home") " ")
;;                                 (ul (@ (class "list list-secondary"))
;;                                      (a (@ (class "nav-link") (href "/about/")) "About") " "
;;                                      (a (@ (class "nav-link") (href "/projects/")) "Projects") " "
;;                                      (a (@ (class "nav-link") (href "/readinglist/")) "Reading List"))))))))
(defun mtr/site-header ()
  (list `(header (@ (class "site-header"))
                 (div (@ (class "site-masthead"))
                      (div (@ (class "container"))
                           (nav (@ (class "nav"))
                                (div (@ (class "row"))
                                     (div (@ (class "column"))
                                          (a (@ (class "nav-link") (href "/")) "> mtrpdx/") " ")
                                     (div (@ (class "column align-right"))
                                          (a (@ (class "nav-link") (href "/about/index.html")) "About") " "
                                          (a (@ (class "nav-link") (href "/projects/index.html")) "Projects") " "
                                          (a (@ (class "nav-link") (href "/readinglist/index.html")) "Reading List") " "))))))))

;; (defun mtr/site-header ()
;;   (list `(header (@ (class "site-header"))
;;                  (div (@ (class "container"))
;;                       (div (@ (class "site-masthead"))
;;                         (nav (@ (class "nav"))
;;                 (a (@ (class "nav-link") (href "/")) "> mtrpdx/") " "))
;;                 (a (@ (class "nav-link") (href "/about/")) "About") "    |    "
;;                         (a (@ (class "nav-link") (href "/projects/")) "Projects") "    |    "
;;                         (a (@ (class "nav-link") (href "/readinglist/")) "Reading List"))))))

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
       (link (@ (rel "icon") (type "image/png") (href ,(concat mtr/site-url "/img/favicon.png"))))
       (link (@ (rel "stylesheet") (href ,(concat mtr/site-url "/css/style.css"))))

       ,(when head-extra head-extra)
       (title ,(concat title " Martin Rodriguez - mtrpdx")))
      (body ,@(unless exclude-header
                (mtr/site-header))
            (div (@ (class "container"))
                 (div (@ (class "site-post"))
                      (h1 (@ (class "site-post-title center"))
                         ,title)
                      ,(when publish-date
                         `(p (@ (class "site-post-meta center")) ,publish-date))
                      ,(when pre-content pre-content)
                      (div (@ (id "content"))
                           ,content)))
                 ,@(unless exclude-footer
                     (mtr/site-footer)))))))

(defun mtr/org-html-link (link contents info)
  "Removes file extension and changes the path into lowercase file:// links."
  (when (and (string= 'file (org-element-property :type link))
             (string= "org" (file-name-extension (org-element-property :path link))))
    (org-element-put-property link :path
                              (downcase
                               (file-name-sans-extension
                                (org-element-property :path link)))))

  (let ((exported-link (org-export-custom-protocol-maybe link contents 'html info)))
    (cond
     (exported-link exported-link)
     ((and (null contents)
           (not (org-export-inline-image-p link)))
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              (org-element-property :raw-link link)))
     ((string-prefix-p "/" (org-element-property :raw-link link))
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              contents))
     (t (org-export-with-backend 'html link contents info)))))

(defun mtr/make-heading-anchor-name (headline-text)
  (thread-last headline-text
               (downcase)
               (replace-regexp-in-string " " "-")
               (replace-regexp-in-string "[^[:alnum:]_-]" "")))

(defun mtr/org-html-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info))
         (level (min 7 (when level (1+ level))))
         (anchor-name (mtr/make-heading-anchor-name text))
         (attributes (org-element-property :ATTR_HTML headline))
         (container (org-element-property :HTML_CONTAINER headline))
         (container-class (and container (org-element-property :HTML_CONTAINER_CLASS headline))))
    (when attributes
      (setq attributes
            (format " %s" (org-html--make-attribute-string
                           (org-export-read-attribute 'attr_html `(nil
                                                                   (attr_html ,(split-string attributes))))))))
    (concat
     (when (and container (not (string= "" container)))
       (format "<%s%s>" container (if container-class (format " class=\"%s\"" container-class) "")))
     (if (not (org-export-low-level-p headline info))
         ;; (format "<h%d%s><a id=\"%s\" class=\"anchor\" href=\"#%s\">¶</a>%s</h%d>%s"
         (format "<h%d%s><a id=\"%s\" class=\"anchor\" href=\"#%s\"></a>%s</h%d>%s"
                 level
                 (or attributes "")
                 anchor-name
                 anchor-name
                 text
                 level
                 (or contents ""))
       (concat
        (when (org-export-first-sibling-p headline info) "<ul>")
        (format "<li>%s%s</li>" text (or contents ""))
        (when (org-export-last-sibling-p headline info) "</ul>")))
     (when (and container (not (string= "" container)))
       (format "</%s>" (cl-subseq container 0 (cl-search " " container)))))))

(defun mtr/org-html-template (contents info)
  (mtr/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))

(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . mtr/org-html-template)
    (link . mtr/org-html-link)
    (headline . mtr/org-html-headline)))

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
      org-attach-id-dir "./assets/"
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


(defun simendsjo/org-publish-include-attachments (plist)
  "Fix published html for org-attach attached files.

- Walks all html files
- Copies attached files it finds to a local .attach folder
- Fixes all src links to point to this new location"
  (let ((pattern (concat "src=\"file://\\(" (regexp-quote org-attach-id-dir) "\\)/\\([^\"]*\\)"))
        (pub-dir (plist-get plist :publishing-directory)))
    (dolist (file (directory-files-recursively pub-dir "\.html$" t))
      (let ((buffer (find-file-noselect file)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (let* ((attach-part (match-string 1))
                   (file-part (match-string 2))
                   (srcfile (f-join attach-part file-part))
                   (dstfile-rel (f-join ".attach" file-part))
                   (dstfile (f-join pub-dir dstfile-rel)))
              ;; Make sure the directory exists as copy/symlink assumes it.
              (let ((dir (file-name-directory dstfile)))
                (unless (f-directory-p dir)
                  (message "Attachment directory %s missing, creating it" dir)
                  (make-directory dir t)))
              ;; Copy/symlink attachment
              (if IS-WINDOWS
                  (copy-file srcfile dstfile)
                (make-symbolic-link srcfile dstfile t))
              ;; Replace link to relative file
              ;; I assume the .attach folder is added at the root, and thus add
              ;; the / at the beginning
              (replace-match (concat "src=\"/" dstfile-rel "\"")))))))))


;; Define the publishing project
(setq org-publish-project-alist
      (list '("mtrpdx:main"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-author nil
              :with-creator t
              ;; :with-toc t
              :section-numbers nil
              :time-stamp-file nil)
            '("mtrpdx:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./public/assets"
              :recursive t
              :publishing-function org-publish-attachment)
            '("mtrpdx:about"
              :base-directory "./content/about"
              :base-extension "org"
              :publishing-directory "./public/about"
              :publishing-function org-html-publish-to-html
              :with-author nil
              :with-creator t
              :with-toc nil
              :section-numbers nil
              ;; :auto-sitemap t
              :sitemap-filename "../about.org"
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
              :sitemap-filename "../projects.org"
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
              :sitemap-filename "../readinglist.org"
              :with-title nil
              :time-stamp-file nil
              :with-timestamps nil)))

(defun mtr/build-site ()
  ;; Generate the site output
  (interactive)

  (org-publish-remove-all-timestamps)
  (org-publish-all (string-equal (or (getenv "FORCE")
                                     (getenv "CI"))
                                 "true"))
  (simendsjo/org-publish-include-attachments '(:publishing-directory "./public/assets"))
  (message "Build complete!"))

(provide 'build-site)
;; ;;; build-site.el ends here
