(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure org is available
(unless (package-installed-p 'org)
  (package-install 'org))

;; Load org and org-id
(require 'org)
(require 'org-id)
(require 'ox-publish)

;; Set org-roam directory for batch mode
(setq org-roam-directory "./notes")

;; Build org-id database from all files in notes/
(setq org-id-track-globally t)
(setq org-id-locations-file "./.org-id-locations")

;; Scan notes directory and build ID database
(org-id-update-id-locations 
 (directory-files-recursively "./notes" "\\.org$"))

;; Simple publishing configuration
(setq org-publish-project-alist
      '(("org-roam-notes"
         :base-directory "./notes"
         :publishing-directory "./public/notes"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Knowledge Base"
         :sitemap-sort-files anti-chronologically
         :html-link-home "/Org-Blog-Forge/notes/"
         :html-link-use-abs-url t
         :html-link-org-files-as-html t)
        ("static"
         :base-directory "./notes"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
         :publishing-directory "./public/notes"
         :recursive t
         :publishing-function org-publish-attachment)
        ("website" :components ("org-roam-notes" "static"))))





(defun my/org-id-to-html-link (link desc info)
  (let* ((id (org-element-property :path link))
         (file (org-id-find-id-file id)))
    (when file
      (let ((html-file (concat (file-name-sans-extension 
                                (file-relative-name file 
                                 (plist-get info :base-directory))) 
                               ".html")))
        (format "<a href=\"%s\">%s</a>" html-file (or desc "link"))))))

(add-to-list 'org-export-filter-link-functions 'my/org-id-to-html-link)
