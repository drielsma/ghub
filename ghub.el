;;; ghub.el --- minuscule client for the Github API  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GPL see https://www.gnu.org/licenses/gpl.txt.

;;; Commentary:

;; A minuscule client for the Github API.

;; This library just provides the HTTP methods.
;; See https://developer.github.com/v3 for valid requests.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-auth)

(eval-when-compile (require 'subr-x))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defconst ghub-base-url "https://api.github.com")

;;; Request
;;;; API

(defvar ghub-response-headers nil)

(defun ghub-get (resource &optional params data headers unpaginate
                          noerror reader username auth base)
  "Make a `GET' request for RESOURCE, optionally sending PARAMS and/or DATA.
Like calling `ghub-request' (which see) with \"GET\" as METHOD."
  (ghub-request "GET" resource params data headers unpaginate
                noerror reader username auth base))

(defun ghub-put (resource &optional params data headers unpaginate
                          noerror reader username auth base)
  "Make a `PUT' request for RESOURCE, optionally sending PARAMS and/or DATA.
Like calling `ghub-request' (which see) with \"PUT\" as METHOD."
  (ghub-request "PUT" resource params data headers unpaginate
                noerror reader username auth base))

(defun ghub-head (resource &optional params data headers unpaginate
                           noerror reader username auth base)
  "Make a `HEAD' request for RESOURCE, optionally sending PARAMS and/or DATA.
Like calling `ghub-request' (which see) with \"HEAD\" as METHOD."
  (ghub-request "HEAD" resource params data headers unpaginate
                noerror reader username auth base))

(defun ghub-post (resource &optional params data headers unpaginate
                           noerror reader username auth base)
  "Make a `POST' request for RESOURCE, optionally sending PARAMS and/or DATA.
Like calling `ghub-request' (which see) with \"POST\" as METHOD."
  (ghub-request "POST" resource params data headers unpaginate
                noerror reader username auth base))

(defun ghub-patch (resource &optional params data headers unpaginate
                            noerror reader username auth base)
  "Make a `PATCH' request for RESOURCE, optionally sending PARAMS and/or DATA.
Like calling `ghub-request' (which see) with \"PATCH\" as METHOD."
  (ghub-request "PATCH" resource params data headers unpaginate
                noerror reader username auth base))

(defun ghub-delete (resource &optional params data headers unpaginate
                             noerror reader username auth base)
  "Make a `DELETE' request for RESOURCE, optionally sending PARAMS and/or DATA.
Like calling `ghub-request' (which see) with \"DELETE\" as METHOD."
  (ghub-request "DELETE" resource params data headers unpaginate
                noerror reader username auth base))

(define-error 'ghub-error "Ghub Error")
(define-error 'ghub-auth-error "Auth Error" 'ghub-error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)
(define-error 'ghub-301 "Moved Permanently" 'ghub-http-error)
(define-error 'ghub-400 "Bad Request" 'ghub-http-error)
(define-error 'ghub-401 "Unauthorized" 'ghub-http-error)
(define-error 'ghub-403 "Forbidden" 'ghub-http-error)
(define-error 'ghub-404 "Not Found" 'ghub-http-error)
(define-error 'ghub-422 "Unprocessable Entity" 'ghub-http-error)

;; TODO maybe change to: (method resource &optional auth ...)
(defun ghub-request (method resource &optional params data headers unpaginate
                            noerror reader username auth base)
  "Make a request for RESOURCE using METHOD."
  (let ((p (and params (concat "?" (ghub--url-encode-params params))))
        (d (and data   (encode-coding-string (json-encode-list data) 'utf-8)))
        (url (if (string-prefix-p "https://" resource)
                 resource
               (concat (or base ghub-base-url) resource))))
    (with-current-buffer
        (let ((url-request-extra-headers
               `(("Content-Type" . "application/json")
                 ,@(and (not (eq auth 'none))
                        (list (cons "Authorization"
                                    (ghub--auth url username auth))))
                 ,@headers))
              (url-request-method method)
              (url-request-data d))
          (url-retrieve-synchronously (concat url p)))
      (set-buffer-multibyte t)
      (let (link body)
        (goto-char (point-min))
        (let (headers)
          (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                                    url-http-end-of-headers t)
            (push (cons (match-string 1)
                        (match-string 2))
                  headers))
          (and (setq link (cdr (assoc "Link" headers)))
               (setq link (car (rassoc (list "rel=\"next\"")
                                       (mapcar (lambda (elt) (split-string elt "; "))
                                               (split-string link ",")))))
               (string-match "[?&]page=\\([^&>]+\\)" link)
               (setq link (match-string 1 link)))
          (setq ghub-response-headers (nreverse headers)))
        (goto-char (1+ url-http-end-of-headers))
        (setq body (funcall (or reader 'ghub--read-json-response)))
        (unless (or noerror (= (/ url-http-response-status 100) 2))
          (let ((data (list method resource p d body)))
            (pcase url-http-response-status
              (301 (signal 'ghub-301 data))
              (400 (signal 'ghub-400 data))
              (401 (signal 'ghub-401 data))
              (403 (signal 'ghub-403 data))
              (404 (signal 'ghub-404 data))
              (422 (signal 'ghub-422 data))
              (_   (signal 'ghub-http-error
                           (cons url-http-response-status data))))))
        (if (and link unpaginate)
            (nconc body
                   (ghub-request method url
                                 (cons (cons 'page link)
                                       (cl-delete 'page params :key #'car))
                                 data headers t noerror reader username auth base))
          body)))))

(defun ghub-wait (resource &optional username auth base)
  "Busy-wait until RESOURCE becomes available."
  (with-local-quit
    (let ((for 0.5)
          (total 0))
      (while (not (ignore-errors (ghub-get resource nil nil nil nil nil nil
                                           username auth base)))
        (setq for (truncate (* 2 for)))
        (setq total (+ total for))
        (when (= for 128)
          (signal 'ghub-error
                  (list (format "Github is taking too long to create %s"
                                resource))))
        (message "Waiting for %s (%ss)..." resource total)
        (sit-for for)))))

;;;; Internal

(defun ghub--read-json-response ()
  (and (not (eobp))
       (let ((json-object-type 'alist)
             (json-array-type  'list)
             (json-key-type    'symbol)
             (json-false       nil)
             (json-null        nil))
         (json-read-from-string (ghub--read-raw-response)))))

(defun ghub--read-raw-response ()
  (and (not (eobp))
       (decode-coding-string
        (buffer-substring-no-properties (point) (point-max))
        'utf-8)))

(defun ghub--url-encode-params (params)
  (mapconcat (lambda (param)
               (concat (url-hexify-string (symbol-name (car param))) "="
                       (url-hexify-string (cdr param))))
             params "&"))

;;; Authentication
;;;; Internal

(defun ghub--auth (url username auth)
  (encode-coding-string
   (if (eq auth 'basic)
       (ghub--basic-auth url)
     (concat "token "
             (if (stringp auth)
                 auth
               (ghub--token url username (and (symbolp auth) auth)))))
   'utf-8))

(defun ghub--basic-auth (url)
  (let ((url (url-generic-parse-url url)))
    (setf (url-user url)
          (ghub--username url))
    (url-basic-auth url t)))

(defun ghub--token (url username package)
  (let* ((host (ghub--hostname url))
         (user (ghub--ident (or username (ghub--username url host))
                            package)))
    (or (ghub--auth-source-get :secret :host host :user user)
        (signal 'ghub-auth-error '("Token not found")))))

(defun ghub--hostname (url)
  (save-match-data
    (if (string-match "\\`https?://\\([^/]+\\)" url)
        (match-string 1 url)
      (signal 'ghub-auth-error (list (format "Invalid url %s" url))))))

(defun ghub--username (url &optional hostname)
  (let ((var (if (string-prefix-p "https://api.github.com" url)
                 "github.user"
               (format "github.%s.user" (or hostname (ghub--hostname url))))))
    (condition-case nil
        (car (process-lines "git" "config" var))
      (error
       (signal 'ghub-auth-error (list (format "%s is undefined" var)))))))

(defun ghub--ident (username package)
  (if (memq package '(nil ghub))
      username
    (format "%s^{%s}" username package)))

(defun ghub--auth-source-get (key/s &rest spec)
  (declare (indent 1))
  (let* ((plist  (car (apply #'auth-source-search spec)))
         (values (mapcar (lambda (key)
                           (let ((value (plist-get plist key)))
                             (if (functionp value)
                                 (funcall value)
                               value)))
                         (if (listp key/s) key/s (list key/s)))))
    (if (listp key/s) values (car values))))

;;; ghub.el ends soon
(provide 'ghub)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ghub.el ends here
