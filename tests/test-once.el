;;; test-once.el --- Tests for once.el -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "26.1") (buttercup "1.38")) (undercover "0.8.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Tests for once.el
;;

;;; Code:
;; * Setup
(load-file "./tests/undercover-init.el")

(require 'buttercup)
(require 'once)
(require 'once-incrementally)

(setq once-shorthand t)

(defvar test-once-counter 0)

(defvar test-once-hook nil)

(defvar test-once-functions nil)

(defvar test-once-run-now nil)

(defvar test-once-watched nil)

(defun test-once-dummy-fn (&rest _)
  "Dummy function to advise.")

;; will need to update this if anything changes
(defun test-once-advised-p (fun)
  "Return whether FUN has advice."
  (advice--p (advice--symbol-function fun)))

(defun test-once-incf-counter ()
  "Increment `test-once-counter'."
  (cl-incf test-once-counter))

(defun test-once-run-idle-timers ()
  "Run all idle timers (from `timer-idle-list').
This is `ert-run-idle-timers'"
  (dolist (timer (copy-sequence timer-idle-list))
    (timer-event-handler timer)))

(defun test-once-run-timers ()
  "Run all timers (from `timer-list')."
  (dolist (timer (copy-sequence timer-list))
    (timer-event-handler timer)))

(defun test-once-throw-error ()
  "Throw an error."
  (error "Oh no"))

;; * once-eval-after-load
(describe "eval-after-load"
  (before-each
    (setq test-once-counter 0
          after-load-alist nil)
    (when (featurep 'test-once-dummy)
      (unload-feature 'test-once-dummy)))
  (it "should run code after a file/feature loads"
    (eval-after-load 'test-once-dummy (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1))
  (it "should run code every time a file/feature loads"
    (eval-after-load 'test-once-dummy (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (unload-feature 'test-once-dummy)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 2)))

(describe "once-eval-after-load"
  (before-all
    (setq after-load-alist nil))
  (before-each
    (setq test-once-counter 0)
    (when (featurep 'test-once-dummy)
      (unload-feature 'test-once-dummy)))
  (it "should run code after a file/feature loads"
    (once-eval-after-load 'test-once-dummy (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1))
  (it "should run code immediately if the file/feature already loaded"
    (require 'test-once-dummy)
    (once-eval-after-load 'test-once-dummy (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 1))
  (it "should not add to `after-load-alist' if the file/feature already loaded"
    (require 'test-once-dummy)
    (once-eval-after-load 'test-once-dummy (lambda () (cl-incf test-once-counter)))
    (expect after-load-alist :to-be nil))
  (it "should run code only the first time a file/feature loads"
    (once-eval-after-load 'test-once-dummy (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (unload-feature 'test-once-dummy t)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1))
  (it "should remove anything added to `after-load-alist' after running"
    (once-eval-after-load 'test-once-dummy (lambda () (cl-incf test-once-counter)))
    (expect after-load-alist :not :to-be nil)
    (require 'test-once-dummy)
    (expect after-load-alist :to-be nil)))

;; * once-with-eval-after-load
(describe "once-with-eval-after-load"
  (it "should expand to once-eval-after-load with the forms wrapped in a lambda"
    (expect (macroexpand-1 '(once-with-eval-after-load 'file (foo) (bar)))
            :to-equal '(once-eval-after-load 'file (lambda nil (foo) (bar))))))

;; * once-x-call
(describe "once-x-call"
  (before-each
    (setq test-once-counter 0
          test-once-hook nil
          test-once-run-now nil
          after-load-alist nil
          test-once-watched nil)
    (when (featurep 'test-once-dummy)
      (unload-feature 'test-once-dummy)))

  (it "should run code when a hook runs"
    (once-x-call (list :hooks 'test-once-hook)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1))

  (it "should run code only the first time a hook runs"
    (once-x-call (list :hooks 'test-once-hook)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (run-hooks 'test-once-hook)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should run code when a function runs"
    (once-x-call (list :before #'test-once-dummy-fn)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (test-once-dummy-fn)
    (expect test-once-counter :to-be 1))

  (it "should run code only the first time a function runs"
    (once-x-call (list :before #'test-once-dummy-fn)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (test-once-dummy-fn)
    (test-once-dummy-fn)
    (expect test-once-counter :to-be 1)
    (expect (not (test-once-advised-p #'test-once-dummy-fn))))

  (it "should run code when a file/feature loads"
    (once-x-call (list :packages 'test-once-dummy)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1))

  (it "should run code only the first time a file/feature loads"
    (once-x-call (list :packages 'test-once-dummy)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)
    (require 'test-once-dummy)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (expect after-load-alist :to-be nil))

  (it "should support :files as an alias for :packages"
    (once-x-call (list :files 'test-once-dummy)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1))

  (it "should run code immediately if a file/feature already loaded"
    (require 'test-once-dummy)
    (once-x-call (list :packages 'test-once-dummy)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 1)
    (expect after-load-alist :to-be nil))

  (it "should run code when a variable is set"
    (once-x-call (list :variables 'test-once-watched)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))
    (setq test-once-watched t)
    (expect test-once-counter :to-be 1)
    (expect (not (get-variable-watchers 'test-once-watched))))

  (it "should run code only the first time a variable is set"
    (once-x-call (list :variables 'test-once-watched)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))
    (setq test-once-watched t)
    (setq test-once-watched 2)
    (expect test-once-counter :to-be 1)
    (expect (not (get-variable-watchers 'test-once-watched))))

  (it "should support :vars as an alias for :variables"
    (once-x-call (list :vars 'test-once-watched)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))
    (setq test-once-watched t)
    (expect test-once-counter :to-be 1)
    (expect (not (get-variable-watchers 'test-once-watched))))

  (it "should support a combination of all when the hook runs first"
    (once-x-call (list :hooks 'test-once-hook
                       :before #'test-once-dummy-fn
                       :packages 'test-once-dummy
                       :variables 'test-once-watched)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (expect after-load-alist :not :to-be nil)
    (expect (get-variable-watchers 'test-once-watched))

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)

    (expect test-once-hook :to-be nil)
    (expect (not (test-once-advised-p #'test-once-dummy-fn)))
    (expect after-load-alist :to-be nil)
    (expect (not (get-variable-watchers 'test-once-watched)))

    (test-once-dummy-fn)
    (require 'test-once-dummy)
    (setq test-once-watched t)
    (expect test-once-counter :to-be 1))

  (it "should support a combination of all when the function runs first"
    (once-x-call (list :hooks 'test-once-hook
                       :before #'test-once-dummy-fn
                       :packages 'test-once-dummy
                       :variables 'test-once-watched)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (expect after-load-alist :not :to-be nil)
    (expect (get-variable-watchers 'test-once-watched))

    (test-once-dummy-fn)
    (expect test-once-counter :to-be 1)

    (expect test-once-hook :to-be nil)
    (expect (not (test-once-advised-p #'test-once-dummy-fn)))
    (expect after-load-alist :to-be nil)
    (expect (not (get-variable-watchers 'test-once-watched)))

    (run-hooks 'test-once-hook)
    (require 'test-once-dummy)
    (setq test-once-watched t)
    (expect test-once-counter :to-be 1))

  (it "should support a combination of all when the file/feature loads first"
    (once-x-call (list :hooks 'test-once-hook
                       :before #'test-once-dummy-fn
                       :packages 'test-once-dummy
                       :variables 'test-once-watched)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (expect after-load-alist :not :to-be nil)
    (expect (get-variable-watchers 'test-once-watched))

    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)

    (expect test-once-hook :to-be nil)
    (expect (not (test-once-advised-p #'test-once-dummy-fn)))
    (expect after-load-alist :to-be nil)
    (expect (not (get-variable-watchers 'test-once-watched)))

    (run-hooks 'test-once-hook)
    (test-once-dummy-fn)
    (setq test-once-watched t)
    (expect test-once-counter :to-be 1))

  (it "should support a combination of all when the variable changes first"
    (once-x-call (list :hooks 'test-once-hook
                       :before #'test-once-dummy-fn
                       :packages 'test-once-dummy
                       :variables 'test-once-watched)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (expect after-load-alist :not :to-be nil)
    (expect (get-variable-watchers 'test-once-watched))

    (setq test-once-watched t)
    (expect test-once-counter :to-be 1)

    (expect test-once-hook :to-be nil)
    (expect (not (test-once-advised-p #'test-once-dummy-fn)))
    (expect after-load-alist :to-be nil)
    (expect (not (get-variable-watchers 'test-once-watched)))

    (run-hooks 'test-once-hook)
    (test-once-dummy-fn)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1))

  (it "should support a :check that is initially false"
    (once-x-call (list :hooks 'test-once-hook
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (setq test-once-run-now t)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support a :check that is initially true"
    (setq test-once-run-now t)
    (once-x-call (list :hooks 'test-once-hook
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support a :check that is initially false with advice"
    (once-x-call (list :before #'test-once-dummy-fn
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (test-once-dummy-fn)
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (setq test-once-run-now t)
    (test-once-dummy-fn)
    (expect test-once-counter :to-be 1)
    (expect (not (test-once-advised-p #'test-once-dummy-fn))))

  (it "should support a :check that is initially false with :packages"
    (once-x-call (list :packages 'test-once-dummy
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (require 'test-once-dummy)
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (unload-feature 'test-once-dummy)
    (setq test-once-run-now t)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (expect after-load-alist :to-be nil))

  (it "should support an initially false :check even with a loaded package"
    (require 'test-once-dummy)
    (once-x-call (list :packages 'test-once-dummy
                       :hooks 'test-once-hook
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    ;; package already had its chance; hook is now the trigger; there may not be
    ;; a great use case for this, but the behavior should be consistent
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :to-be nil)
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (setq test-once-run-now t)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support an :initial-check that fails"
    (once-x-call (list :hooks 'test-once-hook
                       :initial-check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (setq test-once-run-now t)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support a failed :initial-check even with a loaded package"
    (require 'test-once-dummy)
    (once-x-call (list :packages 'test-once-dummy
                       :hooks 'test-once-hook
                       :initial-check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    ;; package already had its chance; hook is now the trigger; there may not be
    ;; a great use case for this, but the behavior should be consistent
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :to-be nil)
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (setq test-once-run-now t)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support an :initial-check that succeeds"
    (setq test-once-run-now t)
    (once-x-call (list :hooks 'test-once-hook
                       :initial-check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support a :check and a failed :initial-check"
    (setq test-once-run-now t)
    (once-x-call (list :hooks 'test-once-hook
                       :initial-check (lambda () (eq test-once-run-now 'yes))
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (setq test-once-run-now nil)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (setq test-once-run-now 'yes)
    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support a :check and a successful :initial-check"
    (setq test-once-run-now t)
    (once-x-call (list :hooks 'test-once-hook
                       :initial-check (lambda () test-once-run-now)
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support specific checks for hooks run with args"
    (once-x-call (list :hooks (list 'test-once-hook
                                    (lambda (arg) arg)))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hook-with-args 'test-once-hook nil)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hook-with-args 'test-once-hook t)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support specific checks for functions run with args"
    (once-x-call (list :before (list 'test-once-dummy-fn
                                     (lambda (arg) arg)))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (test-once-dummy-fn nil)
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (test-once-dummy-fn t)
    (expect test-once-counter :to-be 1)
    (expect (not (test-once-advised-p #'test-once-dummy-fn))))

  (it "should support specific checks for packages"
    (once-x-call (list :packages (list 'test-once-dummy
                                       (lambda () test-once-run-now)))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (require 'test-once-dummy)
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (unload-feature 'test-once-dummy)
    (setq test-once-run-now t)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (expect after-load-alist :to-be nil))

  (it "should support specific checks for variables"
    (once-x-call (list :variables (list 'test-once-watched
                                        (lambda (_symbol newval operation _where)
                                          (and (eq operation 'set)
                                               (eq newval t)))))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))

    (setq test-once-watched 1)
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))

    (setq test-once-watched t)
    (expect test-once-counter :to-be 1)
    (expect (not (get-variable-watchers 'test-once-watched))))

  (it "should support :check and a specific check for a hooks run with args"
    (once-x-call (list :hooks (list 'test-once-hook
                                    (lambda (arg) arg))
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hook-with-args 'test-once-hook t)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (setq test-once-run-now t)
    (run-hook-with-args 'test-once-hook nil)
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hook-with-args 'test-once-hook t)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support :check and a specific check for a functions run with args"
    (once-x-call (list :before (list 'test-once-dummy-fn
                                     (lambda (arg) arg))
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (test-once-dummy-fn t)
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (setq test-once-run-now t)
    (test-once-dummy-fn nil)
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (test-once-dummy-fn t)
    (expect test-once-counter :to-be 1)
    (expect (not (test-once-advised-p #'test-once-dummy-fn))))

  (it "should support :check and a specific checks for a package"
    (once-x-call (list :packages (list 'test-once-dummy
                                       (lambda () (eq test-once-run-now 'yes)))
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (require 'test-once-dummy)
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (unload-feature 'test-once-dummy)
    (setq test-once-run-now t)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (unload-feature 'test-once-dummy)
    (setq test-once-run-now 'yes)
    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (expect after-load-alist :to-be nil))

  (it "should support :check and a specific check for variables"
    (once-x-call (list :variables (list 'test-once-watched
                                        (lambda (_symbol newval operation _where)
                                          (and (eq operation 'set)
                                               (eq newval t))))
                       :check (lambda () test-once-run-now))
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))

    (setq test-once-watched 1)
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))

    (setq test-once-watched t)
    (expect test-once-counter :to-be 0)
    (expect (get-variable-watchers 'test-once-watched))

    (setq test-once-run-now t)
    (setq test-once-watched t)
    (expect test-once-counter :to-be 1)
    (expect (not (get-variable-watchers 'test-once-watched))))

  (it "should support list shorthand for hooks that end in -hook"
    (once-x-call (list 'test-once-hook)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support symbol shorthand for hooks that end in -hook"
    (once-x-call 'test-once-hook
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil))

  (it "should support list shorthand for hooks that end in -functions"
    (once-x-call (list 'test-once-functions)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-functions :not :to-be nil)

    (run-hooks 'test-once-functions)
    (expect test-once-counter :to-be 1)
    (expect test-once-functions :to-be nil))

  (it "should support symbol shorthand for hooks that end in -functions"
    (once-x-call 'test-once-functions
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-functions :not :to-be nil)

    (run-hooks 'test-once-functions)
    (expect test-once-counter :to-be 1)
    (expect test-once-functions :to-be nil))

  (it "should support list shorthand for functions to advise"
    (once-x-call (list 'test-once-dummy-fn)
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (test-once-dummy-fn)
    (expect test-once-counter :to-be 1)
    (expect (not (test-once-advised-p #'test-once-dummy-fn))))

  (it "should support symbol shorthand for functions to advise"
    (once-x-call 'test-once-dummy-fn
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect (test-once-advised-p #'test-once-dummy-fn))

    (test-once-dummy-fn)
    (expect test-once-counter :to-be 1)
    (expect (not (test-once-advised-p #'test-once-dummy-fn))))

  (it "should support list shorthand for packages"
    (once-x-call (list "test-once-dummy")
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (expect after-load-alist :to-be nil))

  (it "should support string shorthand for packages"
    (once-x-call (list "test-once-dummy")
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect after-load-alist :not :to-be nil)

    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (expect after-load-alist :to-be nil))

  (it "should support combining shorthand with a hook that runs first"
    (once-x-call (list 'test-once-hook
                       #'test-once-dummy-fn
                       "test-once-dummy")
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (expect after-load-alist :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil)
    (expect (not (test-once-advised-p #'test-once-dummy-fn)))
    (expect after-load-alist :to-be nil))

  (it "should support combining shorthand with a function that runs first"
    (once-x-call (list 'test-once-hook
                       #'test-once-dummy-fn
                       "test-once-dummy")
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (expect after-load-alist :not :to-be nil)

    (test-once-dummy-fn)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil)
    (expect (not (test-once-advised-p #'test-once-dummy-fn)))
    (expect after-load-alist :to-be nil))

  (it "should support combining shorthand with a package that loads first"
    (once-x-call (list 'test-once-hook
                       #'test-once-dummy-fn
                       "test-once-dummy")
      (lambda () (cl-incf test-once-counter)))
    (expect test-once-counter :to-be 0)
    (expect test-once-hook :not :to-be nil)
    (expect (test-once-advised-p #'test-once-dummy-fn))
    (expect after-load-alist :not :to-be nil)

    (require 'test-once-dummy)
    (expect test-once-counter :to-be 1)
    (expect test-once-hook :to-be nil)
    (expect (not (test-once-advised-p #'test-once-dummy-fn)))
    (expect after-load-alist :to-be nil))

  (describe "should support a mix of shorthand and longhand"
    (it "where the shorthand triggers first"
      (once-x-call (list "test-once-dummy"
                         :hooks 'test-once-hook
                         :before #'test-once-dummy-fn)
        (lambda () (cl-incf test-once-counter)))
      (expect test-once-counter :to-be 0)
      (expect test-once-hook :not :to-be nil)
      (expect (test-once-advised-p #'test-once-dummy-fn))
      (expect after-load-alist :not :to-be nil)

      (require 'test-once-dummy)
      (expect test-once-counter :to-be 1)
      (expect test-once-hook :to-be nil)
      (expect (not (test-once-advised-p #'test-once-dummy-fn)))
      (expect after-load-alist :to-be nil))
    (it "where the longhand triggers first"
      (once-x-call (list "test-once-dummy"
                         :hooks 'test-once-hook
                         :before #'test-once-dummy-fn)
        (lambda () (cl-incf test-once-counter)))
      (expect test-once-counter :to-be 0)
      (expect test-once-hook :not :to-be nil)
      (expect (test-once-advised-p #'test-once-dummy-fn))
      (expect after-load-alist :not :to-be nil)

      (run-hooks 'test-once-hook)
      (expect test-once-counter :to-be 1)
      (expect test-once-hook :to-be nil)
      (expect (not (test-once-advised-p #'test-once-dummy-fn)))
      (expect after-load-alist :to-be nil))))

;; * once
(describe "once"
  (describe "should expand to once-x-call as-is when the first function argument"
    (it "is a sharp-quoted symbol"
      (expect (macroexpand-1 '(once condition #'foo #'bar))
              :to-equal '(once-x-call condition #'foo #'bar)))
    (it "is a quoted symbol"
      (expect (macroexpand-1 '(once condition 'foo #'bar))
              :to-equal '(once-x-call condition 'foo #'bar)))
    (it "is an unquoted symbol (possible variable)"
      (expect (macroexpand-1 '(once condition fun-in-var #'bar))
              :to-equal '(once-x-call condition fun-in-var #'bar)))
    (it "is a lambda"
      (expect (macroexpand-1 '(once condition (lambda nil) #'bar))
              :to-equal '(once-x-call condition (lambda nil) #'bar))))
  (describe "should wrap the body in a lambda when the first function argument"
    (it "is a form/list"
      (expect (macroexpand-1 '(once condition (foo) (bar)))
              :to-equal '(once-x-call condition

                           (lambda nil (foo) (bar)))))))

;; * once-x-require
(describe "once-x-require"
  (before-each
    (when (featurep 'test-once-dummy)
      (unload-feature 'test-once-dummy))
    (when (featurep 'test-once-dummy-too)
      (unload-feature 'test-once-dummy-too)))
  (it "should require the feature once the condition is met"
    (once-x-require (list :hooks 'test-once-hook)
      'test-once-dummy)
    (expect (not (featurep 'test-once-dummy)))
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect (featurep 'test-once-dummy))
    (expect test-once-hook :to-be nil))
  (it "should support requiring multiple features"
    (once-x-require (list :hooks 'test-once-hook)
      'test-once-dummy
      'test-once-dummy-too)
    (expect (not (featurep 'test-once-dummy)))
    (expect (not (featurep 'test-once-dummy-too)))
    (expect test-once-hook :not :to-be nil)

    (run-hooks 'test-once-hook)
    (expect (featurep 'test-once-dummy))
    (expect (featurep 'test-once-dummy-too))
    (expect test-once-hook :to-be nil)))

;; * once-incrementally
(defun test-once-fake-idle-time ()
  "Return 10."
  10)

(describe "once-incrementally"
  (before-each
    (advice-add 'current-idle-time :override #'test-once-fake-idle-time)
    (setq once--incremental-code nil
          test-once-counter 0)
    (when (featurep 'test-once-dummy)
      (unload-feature 'test-once-dummy)))
  (after-each
    (advice-remove 'current-idle-time #'test-once-fake-idle-time))
  (it "should support adding functions to once--incremental-code"
    (once-incrementally :functions #'test-once-incf-counter)
    (expect once--incremental-code
            :to-equal '((:function test-once-incf-counter))))
  (it "should support adding features to once--incremental-code"
    (once-incrementally :features 'foo)
    (expect once--incremental-code
            :to-equal '((:feature foo))))
  (it "should append by default"
    (once-incrementally :features 'foo)
    (once-incrementally :features 'bar)
    (expect once--incremental-code
            :to-equal '((:feature foo) (:feature bar))))
  (it "should support a manually specified depth"
    (once-incrementally :features 'foo)
    (once-incrementally 0 :features 'bar)
    (once-incrementally :features -10 'baz)
    (expect once--incremental-code
            :to-equal '((:feature baz) (:feature bar) (:feature foo))))
  (it "should support adding multiple entries at once"
    (once-incrementally :features 'foo 'bar)
    (expect once--incremental-code
            :to-equal '((:feature foo) (:feature bar))))
  (it "should support mixing depths, features, and functions"
    (once-incrementally :features 'foo :functions 'bar 'baz 0 :features 'qux)
    (expect once--incremental-code
            :to-equal
            '((:feature qux) (:feature foo) (:function bar) (:function baz))))
  (it "should run functions during idle time"
    (once-incrementally :functions #'test-once-incf-counter)
    (once--begin-incremental-loading)
    (expect once--incremental-code
            :to-equal '((:function test-once-incf-counter)))
    (expect test-once-counter :to-be 0)
    (test-once-run-idle-timers)
    (expect once--incremental-code :to-be nil)
    (expect test-once-counter :to-be 1))
  (it "should require features during idle time"
    (once-incrementally :features 'test-once-dummy)
    (once--begin-incremental-loading)
    (expect once--incremental-code
            :to-equal '((:feature test-once-dummy)))
    (expect (not (featurep 'test-once-dummy)))
    (test-once-run-idle-timers)
    (expect once--incremental-code :to-be nil)
    (expect (featurep 'test-once-dummy)))
  (it "should add back code when interrupted before running it"
    (once-incrementally :features 'test-once-dummy)
    (once--begin-incremental-loading)
    (expect once--incremental-code
            :to-equal '((:feature test-once-dummy)))
    (expect (not (featurep 'test-once-dummy)))
    (advice-remove 'current-idle-time #'test-once-fake-idle-time)
    (test-once-run-idle-timers)
    (expect once--incremental-code
            :to-equal '((:feature test-once-dummy))))
  (it "should automatically schedule the next piece of code to run"
    (once-incrementally :functions #'test-once-incf-counter
                        :features 'test-once-dummy)
    (once--begin-incremental-loading)
    (expect once--incremental-code
            :to-equal '((:function test-once-incf-counter)
                        (:feature test-once-dummy)))
    (expect test-once-counter :to-be 0)
    (expect (not (featurep 'test-once-dummy)))

    (test-once-run-idle-timers)
    (expect once--incremental-code
            :to-equal '((:feature test-once-dummy)))
    (expect test-once-counter :to-be 1)
    (expect (not (featurep 'test-once-dummy)))

    (test-once-run-timers)
    (expect once--incremental-code :to-be nil)
    (expect (featurep 'test-once-dummy)))
  ;; TODO need to figure out how to test this correctly
  (xit "should skip code that errors and continue"
    (once-incrementally :functions #'test-once-throw-error
                        :features 'test-once-dummy)
    (once--begin-incremental-loading)
    (expect once--incremental-code
            :to-equal '((:function test-once-throw-error)
                        (:feature test-once-dummy)))
    (expect (not (featurep 'test-once-dummy)))

    (test-once-run-idle-timers)
    (expect once--incremental-code
            :to-equal '((:feature test-once-dummy)))
    (expect (not (featurep 'test-once-dummy)))

    (test-once-run-timers)
    (expect once--incremental-code :to-be nil)
    (expect (featurep 'test-once-dummy)))
  (it "should do nothing when once-idle-timer is nil"
    (let (once-idle-timer)
      (once-incrementally :functions #'test-once-incf-counter)
      (once--begin-incremental-loading)
      (expect once--incremental-code
              :to-equal '((:function test-once-incf-counter)))
      (expect test-once-counter :to-be 0)
      (test-once-run-idle-timers)
      (expect once--incremental-code
              :to-equal '((:function test-once-incf-counter)))
      (expect test-once-counter :to-be 0)))
  (it "should run all code immediately when once-idle-timer is 0"
    (let ((once-idle-timer 0))
      (once-incrementally :functions #'test-once-incf-counter)
      (expect once--incremental-code
              :to-equal '((:function test-once-incf-counter)))
      (expect test-once-counter :to-be 0)
      (once--begin-incremental-loading)
      (expect once--incremental-code
              :to-equal nil)
      (expect test-once-counter :to-be 1))))

;; * once-call-incrementally
(describe "once-call-incrementally"
  (describe "should expand to once-incrementally as-is for when the argument"
    (it "is a sharp-quoted symbol"
      (expect (macroexpand-1 '(once-call-incrementally #'foo))
              :to-equal '(once-incrementally :functions #'foo)))
    (it "is a quoted symbol"
      (expect (macroexpand-1 '(once-call-incrementally 'foo))
              :to-equal '(once-incrementally :functions 'foo)))
    (it "is an unquoted symbol (variable containing function)"
      (expect (macroexpand-1 '(once-call-incrementally fun-in-var))
              :to-equal '(once-incrementally :functions fun-in-var)))
    (it "is a lambda"
      (expect (macroexpand-1 '(once-call-incrementally (lambda nil)))
              :to-equal '(once-incrementally :functions (lambda nil)))))
  (it "should wrap the body in a lambda when the argument is a form/list"
    (expect (macroexpand-1 '(once-call-incrementally (foo)))
            :to-equal '(once-incrementally :functions (lambda nil (foo)))))
  (it "should correctly handle a mix of depths, functions, and forms"
    (expect (macroexpand-1 '(once-call-incrementally (foo) 10 #'bar (baz) (qux)))
            :to-equal '(once-incrementally
                        :functions
                        (lambda nil (foo))
                        10
                        #'bar
                        (lambda nil (baz) (qux))))
    (expect (macroexpand-1 '(once-call-incrementally
                              10 20 (foo) #'bar (baz) (qux) corge 30 'thud
                              (lambda ())))
            :to-equal '(once-incrementally
                        :functions
                        10
                        20
                        (lambda nil (foo))
                        #'bar
                        (lambda nil (baz) (qux))
                        corge
                        30
                        'thud
                        (lambda nil)))))

;; * once-require-incrementally
(describe "once-require-incrementally"
  (it "should expand to once-incrementally, quoting all arguments"
    (expect (macroexpand-1 '(once-require-incrementally 10 foo bar 20 baz qux))
            :to-equal '(once-incrementally :features '10 'foo 'bar '20 'baz 'qux))))

;;; test-once.el ends here
