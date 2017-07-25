;;; ghub.el --- minuscule client for the Github API  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/ghub
;; Keywords: tools
;; Package-Requires: ((emacs "25"))

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

;; Initial configuration
;; ---------------------
;;
;;   $ git config --global github.user <username>
;;   $ emacs ~/.authinfo.gpg
;;   # -*- epa-file-encrypt-to: ("A.U.Thor@example.com") -*-
;;   machine api.github.com login <login> password <token>
;;
;; To acquire a token, go to https://github.com/settings/tokens.  Note
;; that currently the same token is shared by all Emacs packages that
;; use `ghub.el'.

;; Usage examples
;; --------------
;;
;; Getting details about a repository:
;;
;;   (ghub-get "/repos/tarsius/ghub")
;;
;; Listing names of all repositories of a user:
;;
;;   (--keep (cdr (assq 'name it))
;;           (let ((ghub-unpaginate t))
;;             (ghub-get "/users/tarsius/repos")))
;;
;; Making an unauthenticated request:
;;
;;   (let ((ghub-authenticate nil))
;;     (ghub-get "/orgs/magit/repos"))
;;
;; Making a request using basic authentication:
;;
;;   (let ((ghub-authenticate 'basic))
;;     (ghub-get "/orgs/magit/repos"))

;; Github Enterprise support
;; -------------------------
;;
;; Initial configuration:
;;
;;   $ git config --global github.gh.example.com.user employee
;;   $ emacs ~/.authinfo.gpg
;;   # -*- epa-file-encrypt-to: ("employee@example.com") -*-
;;   machine gh.example.com login employee password <token>
;;
;; Making a request:
;;
;;   (let ((ghub-base-url "https://gh.example.com"))
;;     (ghub-get "/users/employee/repos"))

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-auth)

(eval-when-compile (require 'subr-x))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar ghub--token-scopes)

(defvar ghub-base-url "https://api.github.com")
(defvar ghub-authenticate t)
(defvar ghub-token nil)
(defvar ghub-package nil)
(defvar ghub-username nil)
(defvar ghub-unpaginate nil)
(defvar ghub-create-token nil)

(defun ghub-get (resource &optional params data noerror)
  "Make `GET' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "GET" resource params data noerror))

(defun ghub-put (resource &optional params data noerror)
  "Make `PUT' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "PUT" resource params data noerror))

(defun ghub-head (resource &optional params data noerror)
  "Make `HEAD' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "HEAD" resource params data noerror))

(defun ghub-post (resource &optional params data noerror)
  "Make `POST' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "POST" resource params data noerror))

(defun ghub-patch (resource &optional params data noerror)
  "Make `PATCH' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class;
unless optional NOERROR is non-nil, in which case return nil."
  (ghub--request "PATCH" resource params data noerror))

(defun ghub-delete (resource &optional params data noerror)
  "Make `DELETE' request for RESOURCE, optionally sending PARAMS and/or DATA.
Signal an error if the status code isn't in the 2xx class; unless
optional NOERROR is non-nil, in which case return nil."
  (ghub--request "DELETE" resource params data noerror))

(define-error 'ghub-error "Ghub Error")
(define-error 'ghub-auth-error "Auth Error" 'ghub-error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)
(define-error 'ghub-301 "Moved Permanently" 'ghub-http-error)
(define-error 'ghub-400 "Bad Request" 'ghub-http-error)
(define-error 'ghub-404 "Not Found" 'ghub-http-error)
(define-error 'ghub-422 "Unprocessable Entity" 'ghub-http-error)

(defun ghub--request (method resource &optional params data noerror)
  "Make a request using METHOD for RESOURCE.
METHOD is a `HTTP' request method, a string.  If non-nil, send
PARAMS and/or DATA in the request.  Signal an error if the status
code isn't in the 2xx class; unless optional NOERROR is non-nil,
in which case return nil."
  (let* ((p (and params (concat "?" (ghub--url-encode-params params))))
         (d (and data   (encode-coding-string (json-encode-list data) 'utf-8)))
         (url-request-extra-headers
          `(("Content-Type"  . "application/json")
            ,@(and ghub-authenticate
                   `(("Authorization"
                      . ,(if (eq ghub-authenticate 'basic)
                             (ghub--basic-auth)
                           (concat "token "
                                   (encode-coding-string
                                    (ghub--token) 'utf-8))))))))
         (url-request-method method)
         (url-request-data d))
    (with-current-buffer
        (url-retrieve-synchronously (concat ghub-base-url resource p))
      (set-buffer-multibyte t)
      (let (link body)
        (goto-char (point-min))
        (save-restriction
          (narrow-to-region (point) url-http-end-of-headers)
          (and (setq link (mail-fetch-field "Link"))
               (setq link (car (rassoc (list "rel=\"next\"")
                                       (mapcar (lambda (elt) (split-string elt "; "))
                                               (split-string link ",")))))
               (string-match "[?&]page=\\([^&>]+\\)" link)
               (setq link (match-string 1 link))))
        (goto-char (1+ url-http-end-of-headers))
        (setq body (ghub--read-response))
        (unless (or noerror (= (/ url-http-response-status 100) 2))
          (pcase url-http-response-status
            (301 (signal 'ghub-301 (list method resource p d body)))
            (400 (signal 'ghub-400 (list method resource p d body)))
            (404 (signal 'ghub-404 (list method resource p d body)))
            (422 (signal 'ghub-422 (list method resource p d body)))
            (_   (signal 'ghub-http-error
                         (list url-http-response-status
                               method resource p d body)))))
        (if (and link ghub-unpaginate)
            (nconc body
                   (ghub--request method resource
                                  (cons (cons 'page link)
                                        (cl-delete 'page params :key #'car))
                                  data noerror))
          body)))))

(defun ghub--read-response ()
  (and (not (eobp))
       (let ((json-object-type 'alist)
             (json-array-type  'list)
             (json-key-type    'symbol)
             (json-false       nil)
             (json-null        nil))
         (json-read-from-string
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8)))))

(defun ghub--url-encode-params (params)
  (mapconcat (pcase-lambda (`(,key . ,val))
               (concat (url-hexify-string (symbol-name key)) "="
                       (url-hexify-string val)))
             params "&"))

(defun ghub--basic-auth ()
  (let ((url (url-generic-parse-url ghub-base-url)))
    (setf (url-user url)
          (ghub--username))
    (url-basic-auth url t)))

(defun ghub--hostname ()
  (save-match-data
    (if (string-match "\\`https?://\\([^/]+\\)" ghub-base-url)
        (match-string 1 ghub-base-url)
      (signal 'ghub-auth-error '("Invalid value for ghub-base-url")))))

(defun ghub--token ()
  "Return the configured token.
Use `auth-source-search' to get the token for the user returned
by `ghub--username' and a host based on `ghub-base-url'.  When
`ghub-token' is non-nil, then return its value instead."
  (or ghub-token
      (let ((secret (plist-get (car (auth-source-search
                                     :max 1
                                     :user (ghub--username)
                                     :host (ghub--hostname)))
                               :secret)))
        (or (if (functionp secret)
                (funcall secret)
              secret)
            (and ghub-create-token
                 (ghub-create-token ghub-package
                                    (completing-read-multiple
                                     "Token scopes: " ghub--token-scopes)))
            (signal 'ghub-auth-error '("Token not found"))))))

(defun ghub--username ()
  "Return the configured username.
For Github.com get the value of the Git variable `github.user'.
For Github enterprise instances, get the value of the Git
variable `github.HOST.user'.  When `ghub-package' is non-nil,
then return (concat USERNAME \":\" PACKAGE)."
  (concat
   (or ghub-username
       (let ((var (if (string-equal ghub-base-url "https://api.github.com")
                      "github.user"
                    (format "github.%s.user" (ghub--hostname)))))
         (condition-case nil
             (car (process-lines "git" "config" var))
           (error
            (signal 'ghub-auth-error (list (format "%s is undefined" var)))))))
   (and ghub-package (concat ":" ghub-package))))

(defun ghub-wait (resource)
  "Busy-wait until RESOURCE becomes available."
  (with-local-quit
    (let ((for 0.5)
          (total 0))
      (while (not (ignore-errors (ghub-get resource)))
        (setq for (truncate (* 2 for)))
        (setq total (+ total for))
        (when (= for 128)
          (signal 'ghub-error
                  (list (format "Github is taking too long to create %s"
                                resource))))
        (message "Waiting for %s (%ss)..." resource total)
        (sit-for for)))))

(defun ghub-create-token (package scopes &optional note)
  "Create and save a new token for PACKAGE with access to SCOPES.
If PACKAGE is nil, then create a token that can be shared by
multiple Emacs packages.  Optional NOTE specifies a note saved
for the token; if it is nil, then \"Emacs package PACKAGE\" is
used, or if PACKAGE is nil, then \"Emacs library ghub.el\"."
  (interactive
   (let ((pkg (or ghub-package
                  (read-string
                   "Create new token for package (default: generic token): "))))
     (when (equal pkg "")
       (setq pkg nil))
     (list pkg
           (completing-read-multiple "Token scopes: " ghub--token-scopes)
           (read-string "Token description: "
                        (if pkg
                            (concat "Emacs package " pkg)
                          "Emacs library ghub.el")))))
  (let* ((ghub-authenticate 'basic)
         (ghub-package package)
         (token
          (cdr (assq 'token
                     (ghub-post
                      "/authorizations" nil
                      `((scopes . ,scopes)
                        (note . ,(cond (note)
                                       (package
                                        (concat "Emacs package " package))
                                       (t
                                        "Emacs library ghub.el"))))))))
         (file (convert-standard-filename "~/.authinfo.gpg")))
    (with-current-buffer (find-file-noselect file)
      (let* ((host (ghub--hostname))
             (user (ghub--username))
             (key  (format "machine %s login %s password " host user)))
        (goto-char (point-min))
        (cond ((re-search-forward (concat "^" (regexp-quote key)) nil t)
               (kill-line)
               (insert token))
              (t
               (goto-char (point-max))
               (insert key token "\n"))))
      (save-buffer)
      (kill-buffer))
    ;; (auth-source-forget+ '(:host host :user user))
    token))

(defconst ghub--token-scopes
  '((user             . "\
Grants read/write access to profile info only. \
Note that this scope includes `user:email' and `user:follow'.")
    (user:email       . "Grants read access to a user's email addresses.")
    (user:follow      . "Grants access to follow or unfollow other users.")
    (public_repo      . "\
Grants read/write access to code, commit statuses, collaborators, \
and deployment statuses for public repositories and organizations. \
Also required for starring public repositories.")
    (repo             . "\
Grants read/write access to code, commit statuses, invitations, \
collaborators, adding team memberships, and deployment statuses \
for public and private repositories and organizations.")
    (repo_deployment  . "\
Grants access to deployment statuses for public and private repositories. \
This scope is only necessary to grant other users or services access to \
deployment statuses, without granting access to the code.")
    (repo:status      . "\
Grants read/write access to public and private repository commit statuses. \
This scope is only necessary to grant other users or services access to \
private repository commit statuses without granting access to the code.")
    (delete_repo      . "\
Grants access to delete adminable repositories.")
    (notifications    . "\
Grants read access to a user's notifications. \
`repo' also provides this access.")
    (gist             . "\
Grants write access to gists.")
    (read:repo_hook   . "\
Grants read and ping access to hooks in public or private repositories.")
    (write:repo_hook  . "\
Grants read, write, and ping access to hooks \
in public or private repositories.")
    (admin:repo_hook  . "\
Grants read, write, ping, and delete access \
to hooks in public or private repositories.")
    (admin:org_hook   . "\
Grants read, write, ping, and delete access to organization hooks. \
Note: OAuth tokens will only be able to perform these actions on \
organization hooks which were created by the OAuth App. \
Personal access tokens will only be able to perform these actions \
on organization hooks created by a user.")
    (read:org         . "\
Read-only access to organization, teams, and membership.")
    (write:org        . "Publicize and unpublicize organization membership.")
    (admin:org        . "Fully manage organization, teams, and memberships.")
    (read:public_key  . "List and view details for public keys.")
    (write:public_key . "Create, list, and view details for public keys.")
    (admin:public_key . "Fully manage public keys.")
    (read:gpg_key     . "List and view details for GPG keys.")
    (write:gpg_key    . "Create, list, and view details for GPG keys.")
    (admin:gpg_key    . "Fully manage GPG keys."))
  "Last updated on 2017-07-02 from source:
https://developer.github.com/apps/building-integrations/\
setting-up-and-registering-oauth-apps/about-scopes-for-oauth-apps/")

;;; ghub.el ends soon
(provide 'ghub)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ghub.el ends here
