;;; org-gtd-core.el --- Core code for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Core logic for org-gtd
;; Creating this file because straight.el seems unhappy.
;;
;;; Code:

;;;; Requirements

(require 'f)
(require 'org-agenda-property)

(require 'org-gtd-backward-compatibility)

;;;; Customization

(defgroup org-gtd nil
  "Customize the org-gtd package."
  :group 'org
  :link '(url-link "https://github.com/Trevoke/org-gtd.el")
  :package-version '(org-gtd . "0.1"))

(defcustom org-gtd-canceled "CNCL"
  "The `org-mode' keyword for a canceled task.

 See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-directory "~/gtd/"
  "Directory for org-gtd.

The package will use this directory for all its functionality, whether it is
building the agenda or refiling items.  This is the directory where you will
find the default org-gtd file, and it is the directory where you should place
your own files if you want multiple refile targets (projects, etc.)."
  :group 'org-gtd
  :package-version '(org-gtd . "0.1")
  :type 'directory)

(defcustom org-gtd-done "DONE"
  "The `org-mode' keyword for a finished task.

 See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)


(defcustom org-gtd-next "NEXT"
  "The `org-mode' keyword for an action ready to be done.  Just the word."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-todo "TODO"
  "The `org-mode' keyword for an upcoming action (not yet ready, not blocked).

See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-wait "WAIT"
  "The `org-mode' keyword when an action is blocked/delegated.

See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

;;;; Constants

(defconst org-gtd-timestamp "ORG_GTD_TIMESTAMP"
  "Org property storing timestamps for `org-gtd' logic.")

;;;;; Private

(define-error
  'org-gtd-error
  "Something went wrong with `org-gtd'"
  'user-error)

;;;; Footer

(provide 'org-gtd-core)

;;; org-gtd-core.el ends here
