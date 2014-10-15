#|
  This file is a part of Colleen
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.colleen)
(defpackage org.tymoonnext.colleen.mod.auth
  (:nicknames :co-auth)
  (:use :cl :colleen :events))
(in-package :org.tymoonnext.colleen.mod.auth)

(define-module auth () ())

(define-handler (nick-event event) ()
  (let ((prevnick (nick event))
        (newnick (new-nick event)))
    (when (auth-p prevnick)
      (v:info (name (server event)) "Changing ~a to ~a in authenticated list due to NICK." prevnick newnick)
      (setf (auth-users (server event))
            (cons newnick (delete prevnick (auth-users (server event)) :test #'string-equal))))))

(define-handler (part-event event) ()
  (when (auth-p (nick event))
    (remove-from-auth (nick event) "PART")))

(define-handler (quit-event event) ()
  (when (auth-p (nick event))
    (remove-from-auth (nick event) "QUIT")))

(define-handler (kick-event event) ()
  (when (auth-p (nick event))
    (remove-from-auth (nick event) "KICK")))

(define-command logout () ()
  (if (auth-p (nick event))
      (progn
        (remove-from-auth (nick event) "logout command")
        (respond event (fstd-message event :auth-out)))
      (respond event (fstd-message event :auth-fail))))

(defvar *pending-nickserv-auth* ())
(defvar *nickserv-status-regex* (cl-ppcre:create-scanner "STATUS (.+) ([0-3])"))

(define-command login (&rest pw) ()
  (handler-case 
      (if (auth-p (nick event))
          (respond event (fstd-message event :auth-already))
          (let ((logininfo (uc:config-tree :logins (intern (string-upcase (nick event)) "KEYWORD"))))
            (if pw
                (if (string= (format nil "~{~a~^ ~}" pw) logininfo)
                    (progn (add-to-auth (nick event) "Password auth.")
                           (respond event (fstd-message event :auth)))
                    (progn (v:warn :auth "User (~a)~a tried to authenticate with ~a but failed." (name (server event)) (nick event) pw)
                           (respond event (fstd-message event :auth-fail))))
                (if logininfo
                    (progn
                      (v:debug :auth "Requesting NickServ status for ~a" (nick event))
                      (irc:privmsg "NickServ" (format nil "STATUS ~a" (nick event)))
                      (push event *pending-nickserv-auth*)
                      (respond event (fstd-message event :auth-wait)))
                    (progn
                      (v:warn :auth "User ~a tried to authenticate through NickServ, but isn't on ident list!" (nick event))
                      (respond event (fstd-message event :auth-fail)))))))
    (error (err)
      (v:warn :auth "Error during login attempt: ~a" err)
      (respond event (fstd-message event :auth-fail)))))

(define-group auth :documentation "Regulate authentication.")

(define-command (auth add) (username password) (:authorization T :documentation "Add a user to the auth login system.")
  (setf (uc:config-tree :logins (intern (string-upcase username) "KEYWORD")) password)
  (respond event "User ~a added to logins." username))

(define-command (auth remove) (username) (:authorization T :documentation "Remove a user from the auth login system.")
  (remhash (intern (string-upcase username) "KEYWORD") (uc:config-tree :logins))
  (respond event "User ~a removed from logins." username))

(define-command (auth change-password) (new-password) (:authorization T :documentation "Change the password.")
  (setf (uc:config-tree :logins (intern (string-upcase (nick event)) "KEYWORD")) new-password)
  (respond event "Password changed."))

(define-handler notice-event ()
  (when (string-equal "NickServ" (nick notice-event))
    (cl-ppcre:register-groups-bind (user level) (*nickserv-status-regex* (message notice-event))
      (let ((event (loop for pending in *pending-nickserv-auth*
                      do (if (string-equal user (nick pending)) (return pending)))))
        (if event
            (if (string= level "3")
                (progn
                  (add-to-auth user "NickServ IDENT.")
                  (setf *pending-nickserv-auth*
                        (delete event *pending-nickserv-auth*))
                  (respond event (fstd-message event :auth)))
                (progn
                  (v:warn :auth "Received bad NickServ STATUS for user ~a: ~d" user level)
                  (respond event (fstd-message event :auth-fail))))
            (v:warn :auth "Received NickServ STATUS for user (~a)~a without pending request!" (name (server notice-event)) user))))))
