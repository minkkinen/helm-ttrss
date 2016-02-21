;;; helm-ttrss.el --- Interacting with Tiny Tiny RSS from Emacs through helm

;; Copyright (C) 2016 Matti Minkkinen

;; Author: Matti Minkkinen <matti.minkkinen@iki.fi>

(require 'ttrss)

(defvar helm-ttrss-headlines nil
  "List of articles downloaded from TTRSS.")

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
	 (headlines (ttrss-get-headlines ttrss-address ttrss-sid :feed_id -1)))
    (mapcar (lambda (x)
	      (concat "|" (number-to-string (plist-get x :id)) "| "
		      (plist-get x :title)))
	    headlines)))

(defun helm-ttrss-unstar (_)
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password)))
    (dolist (i (helm-marked-candidates))
      (let* ((id-match (string-match "^|\\([0-9]+\\)|" i))
	     (article-id (match-string-no-properties 1 i)))
	(ttrss-update-article ttrss-address ttrss-sid article-id :mode 0 :field 0))))
  (helm-refresh))

(defun helm-ttrss-open (_)
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password))
	 (first-candidate (first (helm-marked-candidates)))
	 (id-match (string-match "^|\\([0-9]+\\)|" first-candidate))
	 (article-id (string-to-number (match-string-no-properties 1 first-candidate))))
    (with-temp-buffer
      (let ((article (first (ttrss-get-article ttrss-address ttrss-sid article-id))))
	(insert (concat
		 "<h1>" (plist-get article :title) "</h1>\n"
		 "<h2>" (plist-get article :author) "</h2>\n\n"))
	(insert (concat "<a href=\""
			(plist-get article :link) "\">"
			(plist-get article :link) "</a>\n"))
	(insert (plist-get article :content))
	(shr-render-buffer (current-buffer))))))

(defun helm-ttrss-starred ()
  (interactive)
  (helm :sources '(helm-ttrss-source)
	:full-frame t))


(provide 'helm-ttrss)
