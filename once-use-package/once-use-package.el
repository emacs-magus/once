;;; once-use-package.el --- Once.el use-package keywords -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once
;; Created: May 06, 2022
;; Keywords: convenience dotemacs startup config
;; TODO add once; currently will break tests since not packaged though
;; Package-Requires: ((emacs "26.1") (use-package "2.4.4"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; once.el keywords for use-package.el.
;;

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)
(require 'once)
(require 'once-incrementally)
(require 'use-package)

;; * Settings
(defvar once-use-package-keyword-aliases nil
  "Plist to rename the keywords provided by once-use-package.
For example:
\(list \":once-x-require\" \":require-once\")

You should confirm there are no conflicts with existing keywords before removing
the \"once-\" prefix.

Note that this must be set before loading once-use-package.")

(defvar once-use-package--keyword-names
  '(":once" ":once-x-require" ":once-require-incrementally")
  "List of the default names of the keywords that once-use-package creates.")

;; * :once
(defun once-use-package--non-list-condition-p (form)
  "Return whether FORM (a form passed to a macro) is a non-list once condition.
Return non-nil if FORM is an atom or if it is a quoted or sharp-quoted atom,
i.e. it looks like condition-in-var, \\='some-hook, #\\='some-func, or \"some
file\".  This is used to match shorthand to the :once use-package keyword."
  ;; not `lambda' since you don't advise anonymous functions
  (or (atom form)
      (and (memq (car form) '(quote function))
           ;; this should not be possible, e.g. won't ever do :once '(foo...),
           ;; but it doesn't hurt
           (atom (cadr form)))))

(defun use-package-normalize/:once (name _keyword args)
  "Normalize package NAME's :once ARGS by converting each to a `once' arglist."
  (let ((mode-name `(function ,(use-package-as-mode name))))
    (cl-loop for arg in args
             collect (cond ((once-use-package--non-list-condition-p arg)
                            (list arg mode-name))
                           ((= (length arg) 1)
                            (append arg (list mode-name)))
                           ((memq nil arg)
                            (cl-substitute mode-name nil arg))
                           ((memq t arg)
                            (cl-substitute mode-name t arg))
                           (t
                            arg)))))

(defun use-package-handler/:once (name _keyword arglists rest state)
  "Use-package handler for :once.
NAME, REST, and STATE are used to handle remaining keywords.  ARGLISTS should be
valid `once' arglists."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(cl-loop for arglist in arglists
                collect `(once ,@arglist)))))

;; * :once-x-require
(defun use-package-normalize/:once-x-require (name _keyword args)
  "Normalize package NAME's :once-x-require ARGS.
Convert each to a valid `once-x-require' arglist."
  (let ((package `',name))
    (cl-loop for arg in args
             collect (cond ((once-use-package--non-list-condition-p arg)
                            (list arg package))
                           ((= (length arg) 1)
                            (append arg (list package)))
                           ((memq nil arg)
                            (cl-substitute package nil arg))
                           ((memq t arg)
                            (cl-substitute package t arg))
                           (t
                            arg)))))

(defun use-package-handler/:once-x-require (name _keyword arglists rest state)
  "Use-package handler for :once-x-require.
NAME, REST, and STATE are used to handle remaining keywords.  ARGLISTS should be
valid `once-x-require' arglists."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(cl-loop for arglist in arglists
                collect `(once-x-require ,@arglist)))))

;; * :once-require-incrementally
(defun use-package-normalize/:once-require-incrementally (name _keyword args)
  "Normalize package NAME's :once-require-incrementally ARGS.
Convert each to a valid `once-require-incrementally' arglist."
  (cl-loop for arg in args
           collect (cond ((once-use-package--non-list-condition-p arg)
                          (if (memq arg '(nil t))
                              (list name)
                            (list arg)))
                         ((memq nil arg)
                          (cl-substitute name nil arg))
                         ((memq t arg)
                          (cl-substitute name t arg))
                         (t
                          arg))))

(defun use-package-handler/:once-require-incrementally (name _keyword arglists rest state)
  "Use-package handler for :once-require-incrementally.
NAME, REST, and STATE are used to handle remaining keywords.  ARGLISTS should be
valid `once-require-incrementally' arglists."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(cl-loop for arglist in arglists
                collect `(once-require-incrementally ,@arglist)))))

;; * Add Keywords and Create Aliases
(defvar once-use-package--insert-after-keyword :commands
  "Default `use-package' keyword to insert once keywords after.")

(defun once-use-package--keyword (name)
  "Return the once-use-package keyword for NAME.
If NAME appears in `once-use-package-keyword-aliases', use the user-defined
alias.  Otherwise return NAME as a symbol."
  (intern (or (plist-get once-use-package-keyword-aliases name #'string=)
              name)))

(defun once-use-package-setup ()
  "Add once keywords to `use-package-keywords'.
This is called automatically when requiring once-use-package."
  (let ((once-keywords (mapcar #'once-use-package--keyword
                               once-use-package--keyword-names)))
    (setq use-package-keywords
          (cl-loop for keyword in use-package-keywords
                   if (eq keyword once-use-package--insert-after-keyword)
                   collect keyword and append once-keywords
                   else
                   ;; don't add duplicates
                   unless (memq keyword once-keywords)
                   collect keyword)))
  ;; create aliases
  (cl-loop for (name alias) on once-use-package-keyword-aliases by #'cddr
           do
           (progn
             (defalias (intern (format "use-package-normalize/%s" alias))
               (intern (format "use-package-normalize/%s" name)))
             (defalias (intern (format "use-package-handler/%s" alias))
               (intern (format "use-package-handler/%s" name))))))
(once-use-package-setup)

(provide 'once-use-package)
;;; once-use-package.el ends here
