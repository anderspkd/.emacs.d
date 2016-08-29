(require 'json)
(require 'user-secrets)


(defmacro nicwh-curl-arglist (method &rest args)
  `(list "nicwh-curl" " nicwh-output"
         "curl" "-X" ,method "-H" "Content-Type: application/json" ,@args))

(defmacro nicwh-curl-post (url data)
  `(start-process ,@(nicwh-curl-arglist "POST" url "-d" data)))

(defmacro nicwh-curl-get (url)
  `(start-process ,@(nicwh-curl-arglist "GET" url)))

(defmacro nicwh-endpoint (ep)
  `(when (boundp ,secret/pi-uri)
     `(concat ,secret/pi-uri "/" ,ep)))

(defun nicwh--play (url &optional f)
  (let ((d (json-encode (list :url url :fetch (if f "True" "False")))))
    (nicwh-curl-post (nicwh-endpoint "play") d)))

(defun nicwh--stop ()
  (nicwh-curl-get (nicwh-endpoint "stop")))

(defun nicwh--pause ()
  (nicwh-curl-get (nicwh-endpoint "pause")))

(defun nicwh--resume ()
  (nicwh-curl-get (nicwh-endpoint "resume")))

(provide 'nicwh)
