
(in-package :weblocks)

(export '(weblocks-acceptor *weblocks-default-app-name* *weblocks-default-app-name-fn*))

(defclass weblocks-acceptor (#.(if (find-class 'easy-acceptor nil)
                                 'easy-acceptor
                                 'acceptor))
  ((session-cookie-name :type string :accessor session-cookie-name
                        :initarg :session-cookie-name
                        :initform (format nil "weblocks-~(~A~)" (gensym)))))

(defmethod initialize-instance :after ((inst weblocks-acceptor) &rest initargs)
  "Set the session secret to prevent a Hunchentoot warning emitted upon
  starting the acceptor."
  (unless (boundp 'hunchentoot:*session-secret*)
    (hunchentoot:reset-session-secret)))

(defmethod process-connection ((acceptor weblocks-acceptor) socket)
  ;; CCL uses predictable random states for new threads
  #+ccl(setf *random-state* (make-random-state t))
  (let ((*print-readably* nil))
    (call-next-method)))

(defvar *weblocks-default-app-name* 'weblocks-default)

(defvar *weblocks-default-app-name-fn* (lambda (request)
                                         (declare (ignore request))
                                         *weblocks-default-app-name*))


(defmethod acceptor-dispatch-request ((acceptor weblocks-acceptor) request)
  "Like easy-acceptor's method, but in the end call/start our default app"
  (loop for dispatcher in *dispatch-table*
        for action = (funcall dispatcher request)
        when action return (funcall action)
          finally
             (if (null (tokenize-uri (script-name*) nil))
                 (let* ((webapp-name (funcall *weblocks-default-app-name-fn* request))
                        (webapp (or (get-webapp webapp-name nil)
                                    (start-webapp webapp-name))))
                   (redirect (weblocks-webapp-prefix webapp)))
                 (call-next-method))))



