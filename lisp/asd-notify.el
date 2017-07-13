;;; asd-notify.el --- Notifications via. dbus
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; Display notifications using dbus.
;;
;; Supports easy (hopefully) specification of actions. For example, if
;; we want to display the message "Click me to do stuff" with the
;; title "Hello", we can write
;;
;; (notify "Click me to do stuff" :summary "Hello"
;;                                :action (((lambda () (message "action clicked!")) "some action")))
;;
;; When the notification is display, it can be clicked and the lambda
;; will be executed.
;;
;;; Code:

(require 'dbus)
(require 'cl-macs)

(eval-and-compile ; shuts up complaints about void definition in the two macros below.
  (defvar notify/service "org.freedesktop.Notifications")
  (defvar notify/interface "org.freedesktop.Notifications")
  (defvar notify/path "/org/freedesktop/Notifications")
  (defvar notify/on-close-events nil)
  (defvar notify/on-close-handler-registered nil)
  (defvar notify/actions nil)
  (defvar notify/action-handler-registered nil))

;; A lot of parameter re-use goes into `dbus-call-method' and
;; `dbus-register-signal', which these two macros deal with.
(defmacro notify/call-method (&rest args)
  (declare (indent defun))
  `(dbus-call-method :session ,notify/service ,notify/path ,notify/interface ,@args))

(defmacro notify/register-signal (&rest args)
  (declare (indent defun))
  `(dbus-register-signal :session ,notify/service ,notify/path ,notify/interface ,@args))

(defsubst notify/capabilities ()
  (notify/call-method "GetCapabilities"))

(defun notify/get-server-information ()
  (let ((server-info (notify/call-method "GetServerInformation")))
    (cl-destructuring-bind (name vendor version . other) server-info
      (list :name name :vendor vendor :version version :additionl-info other))))

(defun notify/signal-handler (notification-id action-id)
  (let ((action (assoc action-id notify/actions)))
    (pcase action
      (`(,name ,fun . ,args)
       (progn
	 (message "(notify) action %s invoking %s on %s" name fun args)
	 (apply fun args)))
      (_ (message "(notify) signal was invoked for unknown action: %s %s" notification-id action-id)))))

(defun notify/setup-action-handler ()
  (unless notify/action-handler-registered
    (notify/register-signal "ActionInvoked" #'notify/signal-handler)
    (setq notify/action-handler-registered t)))

(defun notify/close-notification-handler (notification-id reason-num)
  (let ((event-fun (assoc notification-id notify/on-close-events)))
    (when event-fun
      (let ((reason (pcase reason-num
		      (1 :expired)
		      (2 :dismissed)
		      (3 :closed)
		      (4 :undefined))))
	(funcall (cdr event-fun) reason))
      (setq notify/on-close-events (assq-delete-all notification-id notify/on-close-events)))))

(defun notify/setup-on-close-event-handler ()
  (unless notify/on-close-handler-registered
    (notify/register-signal "NotificationClosed" #'notify/close-notification-handler)
    (setq notify/on-close-handler-registered t)))

;;;###autoload
(defun notify (msg &rest args)
  (let ((summary (or (plist-get args :summary) "Hello from Emacs"))
	(app-name (or (plist-get args :app-name) "asd/notify"))
	(action-list (plist-get args :action))
	(replace-id (or (plist-get args :replace) 0))
	(on-close-event (plist-get args :on-close))
	(actions '(:array))
	(timeout (or (plist-get args :timeout) -1))
	(location (plist-get args :location))
	(urgency (plist-get args :urgency))
	hints)
    ;;; Register the action handler
    (notify/setup-action-handler)
    ;;; Set location (dunno if it works)
    (when location
      (if (listp location)
	  (cl-destructuring-bind (x . y) location
	    (push `(:dict-entry "x" (:variant :int32 ,x)) hints)
	    (push `(:dict-entry "y" (:variant :int32 ,y)) hints))
	(error "(notify) location not of the form '(x . y)")))
    ;;; Set urgency
    (when urgency
      (push `(:dict-entry "urgency"
			  (:variant :byte ,(pcase urgency
					     ('low 0)
					     ('normal 1)
					     ('critical 2)
					     (_ 1))))
	    hints))
    ;;; If no hints was set, set to default (i.e., empty dict)
    (unless hints
      (setq hints '(:array :signature "{sv}")))
    ;; process action(s)
    (dolist (action action-list)
      (cl-destructuring-bind (fun msg . args) action
	(let ((action-name (md5 (format "%s" (random)))))
	  (add-to-list 'notify/actions
		       (cons action-name (cons fun args)))
	  (setq actions (append actions (list action-name msg))))))
    ;; fire the notification and setup the on-close-event, if supplied.
    (let ((notification-id (notify/call-method "Notify" app-name replace-id "" summary msg actions hints :int32 timeout)))
      (when (and on-close-event (functionp on-close-event))
	(notify/setup-on-close-event-handler)
	(push (cons notification-id on-close-event) notify/on-close-events))
      notification-id)))

(with-eval-after-load "asd-notify"
  (unless (dbus-ping :session notify/service)
    (error "dbus seems to be dead... Notifications might not work :-(")))

(provide 'asd-notify)
;;; asd-notify.el ends here
