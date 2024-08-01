(setq action
      '((keyword . "NEXT")
        (properties . ((ORG_GTD . ((value . "Actions")
                                   (selectable . nil)))
                       (id . ((value . org-gtd-id-get-create)
                              (selectable . nil)))))))

(setq calendared-draft-one
      '((properties . ((id . ((value . org-gtd-id-get-create)
                              (selectable . nil)))
                       (ORG_GTD . ((value . "Calendar")
                                   (selectable . nil)))
                       (ORG_GTD_TIMESTAMP . ((type . 'active-timestamp)
                                             (prompt . "Timestamp: ")
                                             (selectable . t)))))))


(setq calendared
      '((metadata . ((id . org-gtd-id-get-create)
                     (ORG_GTD . "Calendar")))
        (definition . (properties . ((ORG_GTD_TIMESTAMP . ((type . 'active-timestamp)
                                                           (prompt . "Timestamp: "))))))
        (views . (engage . ((ORG_GTD_TIMESTAMP . :today))))))

(setq habits
      '((keyword . "SCHEDULED")
        (properties . ((id . ((value . org-gtd-id-get-create)
                              (selectable . nil)))
                       (ORG_GTD . ((value . "Habits")
                                   (selectable . nil)))
                       (style . ((value . "habit")
                                 (selectable . nil)))))))

(setq incubated
      '((properties . ((id . ((value . org-gtd-id-get-create)
                              (selectable . nil)))
                       (ORG_GTD . ((value . "incubated")
                                   (selectable . nil)))
                       (ORG_GTD_TIMESTAMP . ((type . 'active-timestamp)
                                             (prompt . "Timestamp: ")
                                             (selectable . t)))))))

(setq project
      '((properties . ((id . ((value . org-gtd-id-get-create)
                              (selectable . nil)))
                       (ORG_GTD . ((value . "project")
                                   (selectable . nil)))
                       (trigger . ((value . "org-gtd-next-project-action org-gtd-update-project-task!")
                                   (selectable . nil)))
                       (first_action . ((type . 'string)
                                        (value . org-gtd-id-get-create)
                                        (selectable . t)))))))

(setq project-action
      '((keyword . "TODO")
        (properties . ((ORG_GTD . ((value . "actions")
                                   (selectable . nil)))
                       (id . ((value . org-gtd-id-get-create)
                              (selectable . nil)))
                       (parent_id . ((type . 'string)
                                     (value . org-gtd-id-get-create)
                                     (selectable . t)))
                       (following-action . ((type . 'string)
                                            (value . "")
                                            (selectable . t)))))))

(setq delegated
      '((keyword . "WAIT")
        (properties . ((id . ((value . org-gtd-id-get-create)
                              (selectable . nil)))
                       (ORG_GTD_TIMESTAMP . ((type . 'active-timestamp)
                                             (prompt . "When to check in on this? ")
                                             (selectable . t)))
                       (ORG_GTD . ((value . "actions")
                                   (selectable . nil)))))))

(defun prompt-for-property (property-data)
  "Prompt the user for a value based on PROPERTY-DATA if it is selectable."
  (interactive)
   (let ((type (cdr (assoc 'type property-data)))
        (prompt (cdr (assoc 'prompt property-data)))
        (selectable (cdr (assoc 'selectable property-data)))
        (value (cdr (assoc 'value property-data))))
    (if selectable
        (pcase type
          ('string (read-string (format "%s: " prompt)))
          ('active-timestamp (prompt-for-active-date prompt))
          ('active-timestamp-with-repeater (prompt-for-date-with-repeater))
          (_ (read-string (format "%s: " prompt))))
      (if (functionp value)
          (funcall value)
        value))))

(defun prompt-for-date-with-repeater ()
  (interactive)
  (let ((start-date (org-read-date nil nil nil "When do you want this repeating event to start?"))
        (repeater (read-from-minibuffer "How do you want this to repeat? ")))
    (format "<%s %s>" today repeater)))

(defun prompt-for-active-date (prompt)
  (interactive)
  (let ((date (org-read-date nil nil nil prompt)))
    (format "<%s>" date)))

(defun make-new-heading (action-alist &optional epom)
  "Create a new org heading with the information from ACTION-ALIST at EPOM."
  (interactive)
  (let ((epom (or epom (org-element-at-point)))
        (properties (cdr (assoc 'properties action-alist)))
        (keyword (cdr (assoc 'keyword action-alist))))
    ;; Navigate to EPOM
    (goto-char (org-element-property :begin epom))

    ;; Iterate over properties and set them
    (dolist (property properties)
      (let ((property-name (symbol-name (car property)))
            (property-data (cdr property)))
        (org-set-property property-name (prompt-for-property property-data))))
    ;; Set keyword if present
    (when keyword
      (org-todo keyword))))


(defun generate-org-ql-query (alist)
  "Generate an org-ql query from the provided ALIST, focusing on ORG_GTD, ORG_GTD_TIMESTAMP, and style."
  (let ((keyword (cdr (assoc 'keyword alist)))
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


(org-ql-search (org-agenda-files) (generate-org-ql-query action))
(org-ql-search (org-agenda-files) '(todo "NEXT"))
(org-ql-search (org-agenda-files) '(and (property "ORG_GTD" "Calendar" t)))
