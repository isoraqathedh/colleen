#|
 This file is a part of Colleen
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.dictionary
  (:nicknames :co-dictionary)
  (:use :cl :colleen :events :alexandria))
(in-package :org.tymoonnext.colleen.mod.dictionary)

(define-module dictionary () ()
  (:documentation "A general purpose dictionary provider."))

(defmethod stop ((dictionary dictionary))
  (unless (eq (hash-table-test (storage dictionary)) 'equalp)
    (setf (storage dictionary) (make-hash-table :test 'equalp))))

(defun link-p (definition)
  (and definition (< 2 (length definition))
       (string= definition "=>" :end1 2)
       (string-trim " " (subseq definition 2))))

(defun about-term (module event term &optional target)
  (with-module-storage (module)
    (let* ((term (string-trim " " term))
           (definition (uc:config-tree term)))
      (when-let ((link (link-p definition)))
        (setf definition (uc:config-tree link)
              term (format NIL "~a: ~a" term link)))
      (respond event (format-message event (format NIL "~@[~a, look at ~]~a: ~:[Unknown term.~;~:*~a~]" target term definition))))))

(defun define-term (module event term definition)
  (with-module-storage (module)
    (let ((term (string-trim " " term))
          (definition (string-trim " " definition))
          (old-definition (uc:config-tree term)))
      (when-let ((link (link-p definition)))
        (let ((definition (uc:config-tree link)))
          (when-let ((definition (and definition (link-p definition))))
            (respond event "Cannot link to a link. Linking to ~a instead." definition)
            (setf term definition))))
      (setf (uc:config-tree term) definition)
      (respond event "~:[New~;Old~] term ~:*~:[~;re~]defined." old-definition))))

(define-handler (privmsg-event event) ()
  (let ((regex (format NIL "(?i)^~a: (define (.*?):(.*)|(tell me |tell ([^ ]+)|define |explain |do )?(\\s?about )?(.*))" (nick (server event)))))
    (cl-ppcre:register-groups-bind (NIL to-define definition action target NIL term) (regex (message event))
      (cond
        (to-define (define-term module event to-define definition))
        ((string= action "do ") (relay-command event term))
        (target (about-term module event term target))
        (term (about-term module event term))))))

(define-group dictionary :documentation "Manage the general purpose dictionary.")

(define-command (dictionary about) (&rest term) (:documentation "Look up a term.")
  (about-term module event (format NIL "~{~a~^ ~}" term)))

(define-command (dictionary define) (&rest definition) (:documentation "Define a new term, a semicolon separates term name and definition.")
  (cl-ppcre:register-groups-bind (term definition) ("(.*?):(.*)" definition)
    (define-term module event term definition)))

(define-command (dictionary remove) (&rest term) (:documentation "Remove a term.")
  (let ((term (format NIL "~{~a~^ ~}" term)))
    (remhash term (storage module))
    (respond event "Term removed.")))

(define-command (dictionary size) () (:documentation "Returns the number of known terms.")
  (respond event "Dictionary size: ~d terms." (hash-table-count (storage module))))

(define-command (dictionary search) (&rest term) (:documentation "Search for matching terms.")
  (let ((term (format NIL "~{~a~^ ~}" term)))
    (if (< (length term) 1)
        (respond event "Search term too short.")
        (respond event "Matching terms: ~{~a~^, ~}"
                 (loop for item being the hash-keys of (storage module)
                       if (search term item :test #'equalp)
                         collect item)))))

(define-command (dictionary random) () (:documentation "Show a random entry from the dictionary.")
  (let ((key (random-elt (hash-table-keys (storage module)))))
    (about-term module event key)))

(define-command (dictionary link) (from to) (:documentation "Create a link from one term to another.")
  (define-term module event from (format NIL "=> ~a" to)))

(define-command (dictionary is-link) (term) (:documentation "If the term is a link, returns the original term or otherwise NIL.")
  (respond event "~a" (link-p (uc:config-tree term))))
