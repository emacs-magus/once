;;; once-setup.el --- Once.el keyword for setup.el -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once
;; Created: May 06, 2022
;; Keywords: convenience dotemacs startup config
;; NOTE setup.el depends on 26.1
;; TODO add once; currently will break tests since not packaged though
;; Package-Requires: ((emacs "26.1") (setup "1.2.0"))
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
;; once.el setup.el integration.
;;

;; For more information see the README in the online repository.

;;; Code:
(require 'setup)
(require 'once)
(require 'once-incrementally)

(defvar once-setup-keyword-aliases nil
  "Plist to rename the keywords provided by once-setup.
For example:
\(list :once-x-require :require-once)

You should confirm there are no conflicts with existing keywords before removing
the \"once-\" prefix.

Note that this must be set before loading once-setup.")

(defun once-setup--keyword (default-name)
  "Return DEFAULT-NAME or its value in `once-setup-keyword-aliases'."
  (or (plist-get once-setup-keyword-aliases default-name)
      default-name))

(setup-define (once-setup--keyword :once)
  (lambda (condition &rest body)
    (let ((body (if body
                    (mapcar
                     (lambda (item)
                       (if (memq item '(nil t))
                           `#',(setup-get 'mode)
                         item))
                     body)
                  (list `#',(setup-get 'mode)))))
      `(once ,condition
         ,@body)))
  :documentation "When CONDITION is met for the first time, execute BODY.
This is then same as `once' except if BODY is left unspecified, the function to
use will be inferred from the feature name (e.g. for a feature \\='foo or
\\='foo-mode, #'foo-mode would be used).  If any item in BODY is nil, it will
also be converted to the inferred mode name."
  :indent 1
  :debug '(form body))

(setup-define (once-setup--keyword :once-x-require)
  (lambda (condition &rest features)
    (let ((features (if features
                        (mapcar
                         (lambda (feature)
                           (if (memq feature '(nil t))
                               `',(setup-get 'feature)
                             feature))
                         features)
                      (list `',(setup-get 'feature)))))
      `(once-x-require ,condition ,@features)))
  :documentation "Once CONDITION is met the first time, require FEATURES.
This is the same as `once-x-require' except if FEATURES are unspecified, the
feature to require will be inferred (e.g. if in a (setup foo), require \\='foo).
If any item in FEATURES is nil or t, it will also be converted to the inferred
feature name."
  :indent 1
  :debug '(form body))

(setup-define (once-setup--keyword :once-require-incrementally)
  (lambda (&rest features)
    (let ((features (if features
                        (mapcar
                         (lambda (feature)
                           (if (memq feature '(nil t))
                               (setup-get 'feature)
                             feature))
                         features)
                      (list (setup-get 'feature)))))
      `(once-require-incrementally ,@features)))
  :documentation "Require FEATURES incrementally after idle time.
This is the same as `once-require-incrementally' except if FEATURES are
unspecified, the feature to require will be inferred (e.g. if in a (setup foo),
require \\='foo).  If any item in FEATURES is nil or t, it will also be
converted to the inferred feature name."
  :indent 0
  :debug '(body))

(provide 'once-setup)
;;; once-setup.el ends here
