(require 'ttrss)

(defvar helm-ttrss-source
  '((name . "TTRSS starred articles")
    (init . helm-ttrss-init)
    (candidates . helm-ttrss-candidates)
    (action . (("Unstar" . helm-ttrss-unstar)
	       ("Open" . helm-ttrss-open))))
  "Source for starred articles in TTRSS.")

(defun helm-ttrss-init ()
  ())

(defun helm-ttrss-candidates ()
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password))
	 (headlines (ttrss-get-headlines ttrss-address ttrss-sid :feed_id -1 :limit 100)))
    (mapcar (lambda (x) (plist-get x :title)) headlines))) 

(defun helm-ttrss-unstar (_)
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password)))
    (dolist (i (helm-marked-candidates))
      (let* ((search-string i)
	     (article-id
	      (plist-get 
	       (first (ttrss-get-headlines ttrss-address ttrss-sid :feed_id -1 :search search-string))
	       :id)))
	(ttrss-update-article ttrss-address ttrss-sid article-id :mode 0 :field 0)))))

(defun helm-ttrss-open (_)
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password))
	 (search-string (first (helm-marked-candidates)))
	 (article-id
	  (plist-get
	   (first (ttrss-get-headlines ttrss-address ttrss-sid :feed_id -1 :search search-string)) :id)))
    (with-temp-buffer
      (let ((article (first (ttrss-get-article ttrss-address ttrss-sid article-id))))
	(insert (concat "<h1>" (plist-get article :title) "</h1>"))
	(insert "\n")
	(insert (plist-get article :content))
	(shr-render-buffer (current-buffer))))))

(defun helm-ttrss-starred ()
  (interactive)
  (helm :sources '(helm-ttrss-source)))

(provide 'helm-ttrss)
