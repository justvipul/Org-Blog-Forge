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


;; Enhanced sitemap with search box
(defun my/org-publish-sitemap-entry (entry style project)
  "Custom sitemap entry with search-friendly structure."
  (cond ((not (directory-name-p entry))
         (format "<li><a href=\"%s.html\">%s</a></li>"
                 (file-name-sans-extension (file-name-nondirectory entry))
                 (org-publish-find-title entry project)))
        ((eq style 'tree)
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

;; Add this BEFORE your org-publish-project-alist
(unless (package-installed-p 'ox-rss)
  (package-install 'ox-rss))
(require 'ox-rss)

;; Publishing configuration
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
         :sitemap-format-entry my/org-publish-sitemap-entry
         :html-head "<script>
function kbSearch() {
  const q = document.getElementById('kb-search').value.toLowerCase();
  const items = document.querySelectorAll('#kb-list li');
  items.forEach(li => {
    const text = li.textContent.toLowerCase();
    li.style.display = text.includes(q) ? '' : 'none';
  });
}
</script>
<style>
#kb-search { width: 100%; padding: 8px; margin-bottom: 1em; }
#kb-list { list-style: none; padding: 0; }
#kb-list li { margin: 0.5em 0; }
</style>"
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

   ;; NEW RSS PROJECT
("rss-feed"
 :base-directory "./notes"
 :publishing-directory "./public"
 :publishing-function org-rss-publish-to-rss)
       
        ;; Update this to include RSS
        ("website" :components ("org-roam-notes" "rss-feed" "static"))
        ))
	
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)


(defun my/org-html-link-target-blank (link backend info)
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<a " "<a target=\"_blank\" " link)))
(add-to-list 'org-export-filter-link-functions #'my/org-html-link-target-blank)
