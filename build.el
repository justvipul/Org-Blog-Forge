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

;; === CUSTOM FUNCTIONS (MUST COME FIRST) ===
(defun my/org-simple-sitemap (project &optional opts)
  "Simple sitemap with titles."
  (let ((files (org-publish-get-base-files project opts)))
    (with-temp-buffer
      (insert "* Knowledge Base\n\n")
      (insert "** Notes\n")
      (dolist (file (sort files #'string<))
        (let ((title (org-publish-find-title file project)))
          (insert (format "- [[file:%s][%s]]\n" 
                          (file-name-sans-extension (file-name-nondirectory file))
                          title))))
      (buffer-string))))

;; === PUBLISHING CONFIGURATION ===
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
         :sitemap-sort-files anti-chronologically
         :sitemap-function my/org-simple-sitemap    ;; ← ADD THIS LINE
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"index-style.css\"/>"
         :html-head-include-default-style nil
         :html-link-org-files-as-html t
         :section-numbers nil
         :with-toc t
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
