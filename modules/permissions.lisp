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
