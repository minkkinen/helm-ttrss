;;; helm-ttrss.el --- Interacting with Tiny Tiny RSS from Emacs through helm

;; Copyright (C) 2016 Matti Minkkinen

;; Author: Matti Minkkinen <matti.minkkinen@iki.fi>

(require 'ttrss)

(defvar helm-ttrss-headlines nil
  "List of articles downloaded from TTRSS.")

(defvar helm-ttrss-starred-source
  '((name . "TTRSS starred articles")
    (init . helm-ttrss-init)
    (candidates . helm-ttrss-starred-candidates)
    (action . (("Unstar" . helm-ttrss-unstar)
	       ("Open" . helm-ttrss-open))))
  "Source for starred articles in TTRSS.")

(defvar helm-ttrss-source
  '((name . "TTRSS starred articles")
    (init . helm-ttrss-init)
    (candidates . helm-ttrss-candidates)
    (action . (("Unstar" . helm-ttrss-unstar)
	       ("Open" . helm-ttrss-open))))
  "Source for starred articles in TTRSS.")

(defun helm-ttrss-init ()
  ())

(defun helm-ttrss-starred-candidates ()
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password))
	 (headlines (ttrss-get-headlines ttrss-address ttrss-sid :feed_id -1)))
    (mapcar (lambda (x)
	      (concat "|" (number-to-string (plist-get x :id)) "| "
		      (plist-get x :title) " (" (plist-get x :feed_title) ")"))
	    headlines)))

(defun helm-ttrss-candidates ()
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password))
	 (headlines (ttrss-get-headlines ttrss-address ttrss-sid :feed_id -3)))
    (mapcar (lambda (x)
	      (concat "|" (number-to-string (plist-get x :id)) "| "
		      (plist-get x :title) " (" (plist-get x :feed_title) ")"))
	    headlines)))

(defun helm-ttrss-unstar (_)
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password)))
    (dolist (i (helm-marked-candidates))
      (let* ((id-match (string-match "^|\\([0-9]+\\)|" i))
	     (article-id (match-string-no-properties 1 i)))
	(ttrss-update-article ttrss-address ttrss-sid article-id :mode 0 :field 0))))
  (when helm-alive-p
    (helm-refresh)
    (helm-unmark-all)))

(defun helm-ttrss-open (_)
  (let* ((ttrss-sid (ttrss-login ttrss-address ttrss-user ttrss-password)))
    (dolist (i (helm-marked-candidates))
      (let* ((id-match (string-match "^|\\([0-9]+\\)|" i))
	     (article-id (string-to-number (match-string-no-properties 1 i)))
	     (article (first (ttrss-get-article ttrss-address ttrss-sid article-id))))
	(get-buffer-create (plist-get article :title))
	(switch-to-buffer (plist-get article :title))
	(insert (concat
		 "<h1>" (plist-get article :title) "</h1>\n"
		 "<h2>" (plist-get article :author) "</h2>\n\n"
		 ;; convert UNIX epoch time string to date and time
		 ;; (stolen from
		 ;; http://nullman.net/tutorial/emacs-files/.emacs.d/kyle-modules/epoch.el.html)
		 "<h2>" (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time (plist-get article :updated)) t) "</h2>\n\n"))
	(insert (concat "<a href=\""
			(plist-get article :link) "\">"
			(plist-get article :link) "</a>\n"))
	(insert (plist-get article :content))
	(shr-render-region (point-min) (point-max))
	(goto-char (point-min))
	(text-mode)
	(view-mode 1)))))

(defun helm-ttrss-starred ()
  (interactive)
  (helm :sources '(helm-ttrss-starred-source)
	:full-frame t))

(defun helm-ttrss ()
  (interactive)
  (helm :sources '(helm-ttrss-source)
	:full-frame t))

(provide 'helm-ttrss)
