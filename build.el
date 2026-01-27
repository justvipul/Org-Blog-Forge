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

;; Org-roam setup
(setq org-roam-directory "./notes")
(setq org-id-track-globally t)
(setq org-id-locations-file "./.org-id-locations")

;; Build ID database
(org-id-update-id-locations 
 (directory-files-recursively "./notes" "\\.org$"))

;; === PERFECT SITEMAP FUNCTION ===
(defun my/org-simple-sitemap (title sitemap)
  "Generate clean hierarchical sitemap from TITLE and SMAP (list of strings)."
  (with-temp-buffer
    (insert (format "* %s\n\n** All Notes\n" title))
    ;; Sort filenames alphabetically (extract from [[file:xxx][title]])
    (let ((entries (sort sitemap 
                         (lambda (a b) 
                           (string< (my/extract-filename a) 
                                    (my/extract-filename b))))))
      (dolist (entry entries)
        (let ((filename (my/extract-filename entry))
              (title-str (my/extract-title entry)))
          (insert (format "- [[file:%s][%s]]\n" filename title-str)))))
    (buffer-string)))

(defun my/extract-filename (entry)
  "Extract filename from [[file:filename][title]] string."
  (if (string-match "\\[\\[file:([^]]+)\\]\\[" entry)
      (match-string 1 entry)
    (file-name-sans-extension (file-name-nondirectory entry))))

(defun my/extract-title (entry)
  "Extract title from [[file:filename][title]] string."
  (if (string-match "\\]\\[\\([^]]+\\)\\]\\]" entry)
      (match-string 1 entry)
    "Untitled"))

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
         :sitemap-function my/org-simple-sitemap
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"index-style.css\">
                     <link rel=\"stylesheet\" type=\"text/css\" href=\"gray-theme.css\">"
         :html-head-include-default-style nil
         :html-link-org-files-as-html t
         :section-numbers nil
         :with-toc nil
         :html-postamble nil)
        ("static"
         :base-directory "./notes"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg"
         :publishing-directory "./public"
         :recursive t
         :publishing-function org-publish-attachment)
        ("website" :components ("org-roam-notes" "static"))))

(setq org-html-head-include-default-style nil)

;; Publish everything!
(org-publish "website" t)
