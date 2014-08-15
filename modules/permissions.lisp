#|
This file is a part of Colleen
(c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.permissions
  (:use :cl :colleen :events)
  (:nicknames :co-perms :co-permissions))
(in-package :org.tymoonnext.colleen.mod.permissions)

(colleen:define-module permissions () ()
  (:documentation "Manage permissions (limitations of command-use in certain commands."))


(define-matcher space (is #\Space))

(defmacro force-exist-symbol (name package)
  (let ((name-sym (gensym))
        (pack-sym (gensym)))
    `(let ((,name-sym ,name)
           (,pack-sym ,package))
       (or (find-symbol (string-upcase ,name-sym)
                        (string-upcase ,pack-sym))
           (intern (string-upcase ,name-sym)
                   (string-upcase ,pack-sym))))))

(define-matcher :string-end (and (is #\") (prev (not (is #\\)))))

(defun read-name ()
  (consume-until (make-matcher (any #\Space #\" #\( #\)))))

(defun read-val ()
  (case (peek)
    (#\" (consume)
     (prog1
         (cl-ppcre:regex-replace-all "\\\\\"" (consume-until (make-matcher :string-end)) "\"")
       (consume)))
    (#\( (consume)
     (let ((name (read-name)))
       (prog1 (cons name (read-key-val #\)))
         (consume))))
    (T (read-name))))

(defun read-key ()
  (let ((char (consume)))
    (unless (char= char #\:)
      (error "Expected a key, got ~a." char)))
  (let ((name (read-name)))
    (or (find-symbol (string-upcase name) "KEYWORD")
        (error "Unknown keyword ~a." name))))

(defun read-key-val (&optional terminator)
  (consume-until (make-matcher (not (is #\Space))))
  (loop with plist = ()
        until (eql (peek) terminator)
        do (push (read-key) plist)
           (consume-until (make-matcher (not (is #\Space))))
           (push (read-val) plist)
           (consume-until (make-matcher (not (is #\Space))))
        finally (return (nreverse plist))))
