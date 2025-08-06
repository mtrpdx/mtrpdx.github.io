;;; build-site.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Martin Rodriguez
;;
;; Author: Martin Rodriguez <mtrpdx@gmail.com>
;; Maintainer: Martin Rodriguez <mtrpdx@gmail.com>
;; Created: May 03, 2024
;; Modified: May 03, 2024
;; Version: 0.0.1
;; ;; Keywords: hypermedia, blog, feed
;; ;; Homepage: https://github.com/mtrpdx/mtrpdx.github.io
;; Package-Requires: ((emacs "29.1"))
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

(defun use-package-require (name &optional no-require body)
  (if use-package-expand-minimally
      (use-package-concat
       (unless no-require
         (list (use-package-load-name name)))
       body)
    (if no-require
        body
      (use-package-with-elapsed-timer
          (format "Loading package %s" name)
        `((if (not ,(use-package-load-name name))
              (display-warning 'use-package
                               (format "Cannot load %s" ',name)
                               :error)
            ,@body))))))

;; Install dependencies
(require 'vc-git)
;;(require 'ox-html)
(require 'ox-publish)
;;(require 'nxml-mode)
(require 'subr-x)
(require 'cl-lib)

(use-package esxml
  :pin "melpa-stable"
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package parsebib
  :pin "melpa-stable"
  :ensure t)

(use-package citeproc
  :pin "melpa-stable"
  :ensure t)

(setq user-full-name "Martin Rodriguez")
(setq user-mail-address "mtrpdx@gmail.com")

(defvar mtr/site-url (if (string-equal (getenv "CI") "true")
                        "" ;; Don't hardcode the domain
                      "http://localhost:8080")
  "The URL for the site being generated.")

(defvar javascript-format
  (concat "<script type=\"text/javascript\">"
          "  src="
          mtr/site-url
          "/assets/js/main.js"
          "</script>"))

(defvar svg-format
  "<svg class=\"theme-toggle\" width=\"24\" height=\"24\" viewBox=\"0 0 48 48\" fill=\"none\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"M22 41C32.4934 41 41 32.4934 41 22C41 11.5066 32.4934 3 22 3C11.5066 3 3 11.5066 3 22C3 32.4934 11.5066 41 22 41ZM7 22C7 13.7157 13.7157 7 22 7V37C13.7157 37 7 30.2843 7 22Z\"/></svg>")

;; Customize the HTML output
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
                      ;; "<script type=\"text/javascript\" src=\"0.0.0.0:8080/assets/js/main.js\" />"))
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
                                          (a (@ (class "nav-link") (href "/about/")) "About") " "
                                          (a (@ (class "nav-link") (href "/posts/")) "Posts") " "
                                          (a (@ (class "nav-link") (href "/projects/")) "Projects") " "
                                          (a (@ (class "nav-link") (href "/readinglist/")) "Reading List") " "))))))))
                                          ;; (div (@ (class "vertical-line")) "") " "
                                          ;; (div (@ (class "container"))
                                          ;; ,svg-format))))))))
                                          ;;
                                          ;; (span (@ (class "theme-toggle"))
                                          ;; (svg (@ (class "theme-toggler")
                                          ;;         (width "24")
                                          ;;         (height "24")
                                          ;;         (viewBox "0 0 48 48")
                                          ;;         (fill "none")
                                          ;;         (xmlns "http://www.w3.org/2000/svg"))
                                          ;;      (path (@ (d "M22 41C32.4934 41 41 32.4934 41 22C41 11.5066 32.4934 3 22
                                          ;;                3C11.5066 3 3 11.5066 3 22C3 32.4934 11.5066 41 22 41ZM7 22C7
                                          ;;                13.7157 13.7157 7 22 7V37C13.7157 37 7 30.2843 7 22Z")))))))))))))

;; (defun mtr/site-header ()
;;   (list `(header (@ (class "site-header"))
;;                  (div (@ (class "container"))
;;                       (div (@ (class "site-masthead"))
;;                         (nav (@ (class "nav"))
;;                 (a (@ (class "nav-link") (href "/")) "> mtrpdx/") " "))
;;                 (a (@ (class "nav-link") (href "/about/")) "About") "    |    "
;;                         (a (@ (class "nav-link") (href "/projects/")) "Projects") "    |    "
;;                         (a (@ (class "nav-link") (href "/readinglist/")) "Reading List"))))))

;; (defun mtr/link-row ()
;;   "Create row for displaying social links. This is the old function that uses pngs."
;;   (list `(div (@ (class "link-row"))
;;           (div (@ (class "container") (align "center"))
;;                (a (@ (href "https://github.com/mtrpdx"))
;;                   (img (@ (src ,(concat mtr/site-url "/assets/icons/8666686_github_icon_64.png"))
;;                           (style "width: 24px")
;;                           (alt "Github link")))) "  "
;;                (a (@ (href "https://gitlab.com/mtrpdx"))
;;                   (img (@ (src ,(concat mtr/site-url "/assets/icons/8666646_gitlab_icon_64.png"))
;;                           (style "width: 24px")
;;                           (alt "Gitlab link")))) "  "
;;                (a (@ (href "https://www.linkedin.com/in/martintrodriguez/"))
;;                   (img (@ (src ,(concat mtr/site-url "/assets/icons/8666770_linkedin_social_icon_64.png"))
;;                           (style "width: 24px")
;;                           (alt "LinkedIn link")))) "  "
;;                (a (@ (href "mailto:mtrpdx@gmail.com"))
;;                   (img (@ (src ,(concat mtr/site-url "/assets/icons/8666723_mail_icon_64.png"))
;;                           (style "width: 24px")
;;                           (alt "Email link")))) "  "
;;                (a (@ (href "https://soundcloud.com/teensbeans"))
;;                   (img (@ (src ,(concat mtr/site-url "/assets/icons/8666763_headphones_music_icon_64.png"))
;;                           (style "width: 24px")
;;                           (alt "Soundcloud link"))))))))

(defun mtr/link-row ()
  "Create row for displaying social links. This version uses svgs."
  (list `(div (@ (class "link-row"))
          (div (@ (class "container") (align "center"))
               (a (@ (href "https://github.com/mtrpdx"))
                  (svg (@ (class "feather")
                          (xmlns "http://www.w3.org/2000/svg"))
                          (path (@ (d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0
                                       0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44
                                       0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16
                                       2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07
                                       5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3
                                       6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22"))))) " "
               (a (@ (href "https://gitlab.com/mtrpdx"))
                  (svg (@ (class "feather")
                          (xmlns "http://www.w3.org/2000/svg"))
                          (path (@ (d "M22.65 14.39L12 22.13 1.35 14.39a.84.84 0 0
                                       1-.3-.94l1.22-3.78 2.44-7.51A.42.42 0 0 1 4.82
                                       2a.43.43 0 0 1 .58 0 .42.42 0 0 1 .11.18l2.44
                                       7.49h8.1l2.44-7.51A.42.42 0 0 1 18.6 2a.43.43 0 0 1
                                       .58 0 .42.42 0 0 1 .11.18l2.44 7.51L23 13.45a.84.84
                                       0 0 1-.35.94z"))))) " "
               (a (@ (href "https://www.linkedin.com/in/martintrodriguez/"))
                  (svg (@ (class "feather")
                          (xmlns "http://www.w3.org/2000/svg"))
                          (path (@ (d "M16 8a6 6 0 0 1 6 6v7h-4v-7a2 2 0 0 0-2-2 2 2 0 0
                                       0-2 2v7h-4v-7a6 6 0 0 1 6-6z")))
                          (rect (@ (x "2") (y "9") (width "4") (height "12")))
                          (circle (@ (cx "4") (cy "4") (r "2"))))) " "
               (a (@ (href "mailto:mtrpdx@gmail.com"))
                  (svg (@ (class "feather")
                          (xmlns "http://www.w3.org/2000/svg"))
                          (path (@ (d "M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1
                                       0-2-.9-2-2V6c0-1.1.9-2 2-2z")))
                          (polyline (@ (points "22,6 12,13 2,6"))))) " "
               (a (@ (href "https://soundcloud.com/teensbeans"))
                  (svg (@ (class "feather")
                          (xmlns "http://www.w3.org/2000/svg"))
                          (path (@ (d "M3 18v-6a9 9 0 0 1 18 0v6")))
                          (path (@ (d "M21 19a2 2 0 0 1-2 2h-1a2 2 0 0 1-2-2v-3a2 2 0 0 1
                                       2-2h3zM3 19a2 2 0 0 0 2 2h1a2 2 0 0 0 2-2v-3a2 2 0 0 0-2-2H3z")))))))))

               ;; (a (@ (href "https://github.com/mtrpdx"))
                  ;; (object (@ (class "feather-icons")
                  ;;         (data ,(concat mtr/site-url "/assets/icons/github.svg"))
                  ;;         (type "image/svg+xml")))))))

               ;; (a (@ (href "https://github.com/mtrpdx"))
               ;;    (svg (@ (image (@ (href ,(concat mtr/site-url "/assets/icons/github.svg"))
               ;;            (style "width: 24px")
               ;;            (alt "Github link")))))) "  "
               ;; (a (@ (href "https://gitlab.com/mtrpdx"))
               ;;    (img (@ (src ,(concat mtr/site-url "/assets/icons/8666646_gitlab_icon_64.png"))
               ;;            (style "width: 24px")
               ;;            (alt "Gitlab link")))) "  "
               ;; (a (@ (href "https://www.linkedin.com/in/martintrodriguez/"))
               ;;    (img (@ (src ,(concat mtr/site-url "/assets/icons/8666770_linkedin_social_icon_64.png"))
               ;;            (style "width: 24px")
               ;;            (alt "LinkedIn link")))) "  "
               ;; (a (@ (href "mailto:mtrpdx@gmail.com"))
               ;;    (img (@ (src ,(concat mtr/site-url "/assets/icons/8666723_mail_icon_64.png"))
               ;;            (style "width: 24px")
               ;;            (alt "Email link")))) "  "
               ;; (a (@ (href "https://soundcloud.com/teensbeans"))
               ;;    (img (@ (src ,(concat mtr/site-url "/assets/icons/8666763_headphones_music_icon_64.png"))
               ;;            (style "width: 24px")
               ;;            (alt "Soundcloud link"))))))))

(defun mtr/site-footer ()
  (list `(footer (@ (class "site-footer"))
                 (div (@ (class "container"))
                      (div (@ (class "row"))
                           (div (@ (class "column"))
                          ,@(mtr/link-row)))
                      (div (@ (class "row"))
                          (div (@ (class "column"))
                                ;; (div (@ (class "site-footer-line"))
                                ;;      " · "
                                ;;      " · "
                                (div (@ (class "site-footer-line"))
                                     "© 2024 · mtrpdx")))))))

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
       (meta (@ (name "theme-color")
                (content "{{ .Site.Params.themeColor }}")))
       (link (@ (rel "icon") (type "image/png") (href ,(concat mtr/site-url "/assets/img/favicon.png"))))
       (link (@ (rel "stylesheet") (href ,(concat mtr/site-url "/assets/css/code.css"))))
       (link (@ (rel "stylesheet") (href ,(concat mtr/site-url "/assets/css/style.css"))))
       (script (@ (type "text/javascript") (src ,(concat mtr/site-url "/assets/js/main.js")))
                    ;; Empty string to cause a closing </script> tag
                    "")


       ,(when head-extra head-extra)
       (title ,(concat title " - mtrpdx")))
      (body ,@(unless exclude-header
                (mtr/site-header))
            (div (@ (class "container"))
                 (div (@ (class "site-post"))
                      (h1 (@ (class "site-post-title"))
                         ,title)
                      ,(when publish-date
                         `(p (@ (class "site-post-meta")) ,publish-date))
                      ;; (div (@ (a ) (href))
                      ,(when pre-content pre-content)
                      (div (@ (id "content"))
                           ,content)))
                 ,@(unless exclude-footer
                     (mtr/site-footer)))))))

(defun mtr/org-html-link (link contents info)
  "Remove file extension and changes the path into lowercase file:// links."
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

(defun mtr/org-html-src-block (src-block _contents info)
  (let* ((lang (org-element-property :language src-block))
           (code (org-html-format-code src-block info)))
    (format "<pre>%s</pre>" (string-trim code))))

(defun mtr/org-html-template (contents info)
  (mtr/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))

(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . mtr/org-html-template)
    (link . mtr/org-html-link)
    (src-block . mtr/org-html-src-block)
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
      org-export-with-section-numbers t
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

(defun mtr/format-post-entry (entry style project)
  "Format posts with author and published data in the index page."
  (cond ((not (directory-name-p entry))
         (format "[[file:%s][%s]] - %s · %s"
                 entry
                 (org-publish-find-title entry project)
                 (car (org-publish-find-property entry :author project))
                 (format-time-string "%B %d, %Y"
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun mtr/post-sitemap (title files)
  (format "#+title: %s\n\n%s"
          title
          (mapconcat (lambda (file)
                       (format "- %s\n" file))
                     (cadr files)
                     "\n")))

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
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf\\|svg\\|map\\|csl"
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
            '("mtrpdx:posts"
              :base-directory "./content/posts"
              :base-extension "org"
              :publishing-directory "./public/posts"
              :publishing-function org-html-publish-to-html
              :recursive t
              :with-author t
              :with-creator t
              :with-toc toc
              :section-numbers nil
              :auto-sitemap t
              :sitemap-filename "../posts.org"
              :sitemap-title "Posts"
              :sitemap-format-entry mtr/format-post-entry
              :sitemap-style list
              ;; :sitemap-function mtr/post-sitemap
              :sitemap-sort-files anti-chronologically
              :with-title nil
              :with-timestamps t)
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
