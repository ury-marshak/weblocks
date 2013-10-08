
(in-package :weblocks)

(export '(weblocks-acceptor
	  weblocks-ssl-acceptor
	  ssl-redirect-acceptor
          *weblocks-default-app-name* *weblocks-default-app-name-fn*))

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

(defmethod acceptor-status-message :around ((acceptor weblocks-acceptor) (http-status-code (eql hunchentoot:+http-internal-server-error+)) &key &allow-other-keys)
  nil)

(defmethod acceptor-status-message :around ((acceptor weblocks-acceptor) (http-status-code (eql hunchentoot:+http-not-found+)) &key &allow-other-keys)
  nil)

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


;;; To support both http: and https:, call START-WEBLOCKS twice, once with
;;; :ACCEPTOR-CLASS 'WEBLOCKS-SSL-ACCEPTOR, once using the default acceptor.
;;; To force https:, call START-WEBLOCKS with :ACCEPTOR-CLASS 'WEBLOCKS-SSL-ACCEPTOR,
;;; and also do
;;;
;;;   (hunchentoot:start (make-instance 'ssl-redirect-acceptor))
;;;

(defclass weblocks-ssl-acceptor (weblocks-acceptor ssl-acceptor)
    ())

(defclass ssl-redirect-acceptor (acceptor)
    ((ssl-port :reader ssl-redirect-acceptor-ssl-port
	       :initarg :ssl-port
	       :initform 443
	       :documentation
	       "The port used by the SSL acceptor."))
  (:documentation
    "A very simple acceptor for handling non-SSL requests and redirecting them
to the SSL port."))

(defmethod acceptor-dispatch-request ((acceptor ssl-redirect-acceptor) request)
  (hunchentoot:redirect (request-uri* request)
			:protocol ':https
			:port (ssl-redirect-acceptor-ssl-port acceptor)
			:add-session-id nil))

