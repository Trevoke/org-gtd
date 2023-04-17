;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(defun ogt-add-calendar-item (label year month day)
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd (format "C-c c c %s-%s-%s RET TAB RET" year month day))))

(describe
 "A calendar item"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "has a specific property with the active timestamp"
     (let* ((timestamp (decode-time))
            (year (nth 5 timestamp))
            (month (nth 4 timestamp))
            (day (nth 3 timestamp)))
       (ogt-add-calendar-item "Yowza" year month day)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
                 :to-match (format "%s-%#02d-%#02d" year month day))
         )))
 (describe
  "compatibility with orgzly"
  (it "has a copy of the active timestamp in the body"
      (let* ((timestamp (decode-time))
            (year (nth 5 timestamp))
            (month (nth 4 timestamp))
            (day (nth 3 timestamp)))
       (ogt-add-calendar-item "Yowza" year month day)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (org-end-of-meta-data t)
         (expect (buffer-substring (point) (point-max))
                 :to-match
                 (format "<%s-%#02d-%#02d>" year month day)))))))