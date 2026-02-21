;;; once-incrementally.el --- Incrementally run code during idle time -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once
;; Created: June 07, 2022

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
;; Provides helpers for incrementally deferring code to run spread out during
;; idle time like Doom's :defer-incrementally.  This package is based on Doom's
;; code but does not depend on Doom and provides additional functionality such
;; as allowing giving packages priority (as a number like `add-hook') and
;; incrementally running any code not just requiring packages (which could
;; potentially be useful over just using `run-with-idle-timer').

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)

(require 'once-core)

;; * Settings
(defcustom once-idle-timer 2.0
  "How long (in idle seconds) to wait before running code.
This applies just after startup and after incremental loading is interrupted by
user input.  When nil, do not incrementally load or run anything.  When 0,
immediately run all code after startup.  For example, you could set this to
\"(if (daemonp) 0 2.0)\" to immediately run all code when starting the Emacs
daemon."
  :type '(choice number (const nil))
  :group 'once)

(defcustom once-incremental-run-interval 0.75
  "How long in seconds to wait between incrementally running code.
After `once-idle-timer' seconds of idle time have passed, stored code will
incrementally run with `once-incremental-run-interval' seconds in between each
item until user input."
  :type 'number
  :group 'once)

(defvar once--incremental-code nil
  "List of functions to run and packages to load incrementally after startup.
Each item can either be a function to run or feature to require.  Functions
should be in the form (:function <function>) and features should be in the
form (:feature <feature>).")

;; * Helpers
(defun once--load-feature (feature)
  "Load FEATURE and return t."
  (message "Once loading %s" feature)
  ;; if `default-directory' doesn't exist or is unreadable, Emacs throws file
  ;; errors
  (let ((default-directory user-emacs-directory)
        (inhibit-message t)
        (file-name-handler-alist
         (delq nil (list (rassq 'jka-compr-handler file-name-handler-alist)))))
    (require feature nil t)
    t))

(defun once--run (item)
  "Run the next function or load the next package and return t.
ITEM should be in the format (:feature <feature>) or (:function <function>)."
  (let ((type (car item))
        (code (cadr item)))
    (if (eq type :feature)
        (once--load-feature code)
      (message "Once running %s" code)
      (funcall code)
      t)))

(defun once--run-incrementally ()
  "Incrementally run code in `once--incremental-code' until it is empty.
When failing to load a package or run some code, continue trying to run the
remaining code after `once-incremental-run-interval'."
  (let ((gc-cons-threshold most-positive-fixnum)
        item
        type
        code
        idle-time)
    ;; get the item function to run or feature that has not yet been loaded
    (while (and once--incremental-code
                (progn (setq item (pop once--incremental-code))
                       (setq type (car item))
                       (setq code (cadr item))
                       (and (eq type :feature)
                            (featurep code))))
      (message "Once already loaded %s" code)
      (setq item nil))
    (when item
      (condition-case-unless-debug e
          (when (or (null (setq idle-time (current-idle-time)))
                    (< (float-time idle-time) once-idle-timer)
                    (not (while-no-input (once--run item))))
            ;; interrupted, run code later
            (push item once--incremental-code))
        (error
         (message "Error: once failed to incrementally %s %S because: %s"
                  (if (eq type :feature)
                      "load"
                    "run")
                  code
                  e))))
    (if (null once--incremental-code)
        (message "Once finished incrementally running code")
      (run-at-time (if idle-time
                       once-idle-timer
                     once-incremental-run-interval)
                   nil
                   #'once--run-incrementally))))


(defun once--begin-incremental-loading ()
  "Start the incremental loading process."
  (when (numberp once-idle-timer)
    (if (zerop once-incremental-run-interval)
        (mapc #'once--run once--incremental-code)
      (run-with-idle-timer once-idle-timer
                           nil #'once--run-incrementally))))

;; * Main
;;;###autoload
(defun once-enable-incremental-loading ()
  "Enable incrementally running deferred code after startup.
This applies to code deferred with `once-incrementally' and
`once-require-incrementally'."
  (add-hook 'emacs-startup-hook #'once--begin-incremental-loading))

(defun once--make-singular (symbol)
  "Remove the s from the end of SYMBOL if there is one."
  (intern (string-trim-right (symbol-name symbol) "s")))

;;;###autoload
(defun once-incrementally (&rest entries)
  "Register code to be executed incrementally after idle time.
Each item in ENTRIES can be one of the following:
- A number - Changes the depth to add all following entries at
- :features or :functions - Signals that the following entries are features to
  load or functions call
- A feature or function to defer

Depth determines the order code will run in.  For example, a function with depth
-90 would run before one with depth 0.  This corresponds to the depth argument
to `add-hook' (see its documentation for more information).  When no depth is
specified, code runs in the order it is registered (default depth 90).

See `once-idle-timer' and `once-incremental-run-interval' for more information
on customizing when code will be run."
  ;; depth 90 (same as t) to append by default
  (let ((depth 90)
        type)
    (dolist (entry entries)
      (cond ((keywordp entry)
             (setq type entry))
            ((numberp entry)
             (setq depth entry))
            (t
             (let ((item (list (once--make-singular type)
                               entry)))
               (add-hook 'once--incremental-code item depth)))))))

;;;###autoload
(defmacro once-call-incrementally (&rest body)
  "Register code to be executed incrementally after idle time.
Each item in BODY should be a function to run or a depth to start following
functions at.  For example, a function with depth -90 would run before one with
depth 0.  This corresponds to the depth argument to `add-hook' (see its
documentation for more information).  When no depth is specified, code runs in
the order it is registered (default depth 90).

Items in BODY that could be functions will be added as-is:
\(once-call-incrementally #\\='foo \\='bar some-func-in-var (lambda ()))

Items in BODY that are in the form (fun args...) will be considered to be
function bodies and wrapped in lambdas:
\(once-call-incrementally
  (foo)
  (bar)
  (baz))

Syntax can be mixed between depths, functions and forms.  Adjacent forms will be
grouped into a single lambda.
\(once-call-incrementally
  10
  #\\='foo
  #\\='bar
  20
  (baz)
  (qux)
  \\='quux
  30
  corge)

See `once-idle-timer' and `once-incremental-run-interval' for more information
on customizing when code will be run."
  (declare (indent 0))
  (let (args
        wrap-forms)
    (dolist (form body)
      (if (and (listp form) (not (once--function-p form)))
          (push form wrap-forms)
        (when wrap-forms
          (push `(lambda () ,@(nreverse wrap-forms)) args)
          (setq wrap-forms nil))
        (push form args)))
    (when wrap-forms
      (push `(lambda () ,@(nreverse wrap-forms)) args))
    `(once-incrementally :functions ,@(nreverse args))))

;;;###autoload
(defmacro once-require-incrementally (&rest features)
  "Register FEATURES to be required incrementally after idle time.
Each item in FEATURES should be a feature symbol or a depth to store the
following features at.  For example, a feature with depth -90 would load before
one with depth 0.  This corresponds to the depth argument to `add-hook' (see its
documentation for more information).  When no depth is specified, features are
loaded in the order they are registered (default depth 90).

Note that FEATURES should not be quoted:
\(once-require-incrementally 10 evil -90 magit)

See `once-idle-timer' and `once-incremental-run-interval' for more information
on customizing when features will be loaded."
  (declare (indent 0))
  `(once-incrementally :features ,@(cl-loop for feature in features
                                            collect `',feature)))

(provide 'once-incrementally)
;;; once-incrementally.el ends here
