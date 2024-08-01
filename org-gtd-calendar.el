(setq org-gtd-calendared
      '((metadata . ((ID . org-gtd-id-get-create)
                     (ORG_GTD . "Calendar")))
        (definitions . ((properties . ((ORG_GTD_TIMESTAMP . ((type . active-timestamp)
                                                            (prompt . "Timestamp: ")))))))
        (views . (engage . ((ORG_GTD_TIMESTAMP . :today))))))


(setq my-inputs
      '(('string (read-string (format "%s: " prompt)))
        ('active-timestamp (org-gtd-prompt-for-active-date prompt))
        ('active-timestamp-with-repeater (org-gtd-prompt-for-date-with-repeater))))

(setq user-inputs nil)

(defun what-gets-used ()
  (concat user-inputs my-inputs))

(defun org-gtd-prompt-for-property (property-data)
  "Prompt the user for a value based on PROPERTY-DATA."
  (interactive)
  (let ((type (alist-get 'type property-data))
        (prompt (alist-get 'prompt property-data)))
    (pcase type
      ('string (read-string (format "%s: " prompt)))
      ('active-timestamp (org-gtd-prompt-for-active-date prompt))
      ('active-timestamp-with-repeater (org-gtd-prompt-for-date-with-repeater))
      (_ (error "%s is not a known property type" type)))))

(defun org-gtd-prompt-for-date-with-repeater ()
  (interactive)
  (let ((start-date (org-read-date nil nil nil "When do you want this repeating event to start?"))
        (repeater (read-from-minibuffer "How do you want this to repeat? ")))
    (format "<%s %s>" today repeater)))

(defun org-gtd-prompt-for-active-date (prompt)
  (interactive)
  (let ((date (org-read-date nil nil nil (format "!! %s !!" prompt))))
    (format "<%s>" date)))

(defun org-gtd-set-property (property-value)
  (if (functionp property-value)
      (funcall property-value)
    property-value))

(defun org-gtd-make-new-heading (action-alist &optional epom)
  "Create a new org heading with the information from ACTION-ALIST at EPOM."
  (interactive)
  (let* ((epom (or epom (org-element-at-point)))
         (metadata (alist-get 'metadata action-alist))
         (definitions (alist-get 'definitions action-alist))
         (properties (alist-get 'properties definitions))
         (keyword (alist-get 'keyword definitions)))
    (save-excursion
      (goto-char (org-element-property :begin epom))

      (dolist (metadatum metadata)
        (let ((property-name (symbol-name (car metadatum)))
              (property-value (cdr metadatum)))
          (org-set-property property-name (org-gtd-set-property property-value))))

      (dolist (property properties)
        (let ((property-name (symbol-name (car property)))
              (property-data (cdr property)))
          (org-set-property property-name (org-gtd-prompt-for-property property-data))))
      ;; Set keyword if present
      (when keyword
        (org-todo keyword)))))


(defun org-gtd-generate-engage-query (alist)
  "Generate an org-ql query from the provided ALIST.
NOTE THAT ENGAGE IS HARDCODED RIGHT NOW."
  (let* ((engage (alist-get 'engage (alist-get 'views alist)))
         (keyword (cdr (assoc 'keyword alist)))
        (properties (cdr (assoc 'properties alist))))
    (delq nil
          (append
           '(and)
           (when keyword
             `((todo ,keyword)))
           (mapcar (lambda (property)
                     (let ((name (symbol-name (car property)))
                           (value (cdr (assoc 'value (cdr property)))))
                       (when (and value (member name '("ORG_GTD" "ORG_GTD_TIMESTAMP" "style")))
                         `(property ,name ,value t))))
                   properties)))))

;; (org-ql-search (org-agenda-files) (generate-org-ql-query action))
;; (org-ql-search (org-agenda-files) '(todo "NEXT"))
;; (org-ql-search (org-agenda-files) '(and (property "ORG_GTD" "Calendar" t)))

(provide 'org-gtd-calendar)
