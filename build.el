(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'org)
  (package-install 'org))

(require 'org)
(require 'org-id)
(require 'ox-publish)

;; Set org-roam directory
(setq org-roam-directory "./notes")
(setq org-id-track-globally t)
(setq org-id-locations-file "./.org-id-locations")

;; Build ID database
(org-id-update-id-locations 
 (directory-files-recursively "./notes" "\\.org$"))

;; === FIXED SITEMAP FUNCTION ===
(defun my/org-simple-sitemap (title sitemap)
  "Generate simple hierarchical sitemap. TITLE is sitemap title, SMAP is list of files."
  (with-temp-buffer
    (insert (format "* %s\n\n** Notes\n" title))
    (dolist (entry (sort sitemap #'string<))
      (let* ((file (car entry))
             (filename (file-name-sans-extension (file-name-nondirectory file)))
             (title (or (cdr entry) filename)))
        (insert (format "- [[file:%s][%s]]\n" filename title))))
    (buffer-string)))

;; === PUBLISHING PROJECTS ===
(setq org-publish-project-alist
      '(("org-roam-notes"
         :base-directory "./notes"
         :base-extension "org"
         :publishing-directory "./public"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Knowledge Base"
         :sitemap-function my/org-simple-sitemap  ;; ✅ FIXED
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"index-style.css\"/>
                     <link rel=\"stylesheet\" type=\"text/css\" href=\"gray-theme.css\"/>"
         :html-head-include-default-style nil
         :html-link-org-files-as-html t
         :section-numbers nil
         :with-toc nil  ;; Disable TOC for cleaner index
         :html-postamble nil)
        ("static"
         :base-directory "./notes"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg"
         :publishing-directory "./public"
         :recursive t
         :publishing-function org-publish-attachment)
        ("website" :components ("org-roam-notes" "static"))))

(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

;; Publish!
(org-publish "website" t)
