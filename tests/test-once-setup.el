;;; test-once-setup.el --- Tests for once-setup.el -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "26.1") (buttercup "1.25")) (undercover "0.8.0"))

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
;;  Tests for once-setup.el
;;

;;; Code:
;; * Setup
(when (require 'undercover nil t)
  (undercover "once-setup/*.el"
              (:exclude "test-*.el")
              (:report-format 'codecov)
              (:send-report nil)))

(require 'buttercup)
(require 'once-setup)

;; * :once
(describe "The :once setup.el keyword"
  (describe "should behave like once with"
    (it "a function argument"
      (expect (macroexpand-all '(setup foo
                                  (:once condition #'foo #'bar)))
              :to-equal '(once-x-call condition #'foo #'bar)))
    (it "a quoted symbol"
      (expect (macroexpand-all '(setup foo
                                  (:once condition 'foo #'bar)))
              :to-equal '(once-x-call condition 'foo #'bar)))
    (it "an unquoted symbol (possible variable)"
      (expect (macroexpand-all '(setup foo
                                  (:once condition fun-in-var #'bar)))
              :to-equal '(once-x-call condition fun-in-var #'bar)))
    (it "a lambda"
      (expect (macroexpand-all '(setup foo
                                  (:once condition #'(lambda nil) #'bar)))
              :to-equal '(once-x-call condition #'(lambda nil) #'bar)))
    (it "forms as the body"
      (expect (macroexpand-all '(setup foo
                                  (:once condition (foo) (bar))))
              :to-equal '(once-x-call condition
                           #'(lambda nil (foo) (bar))))))
  (it "should infer the function to run when no body is specified"
    (expect (macroexpand-all '(setup foo
                                (:once condition)))
            :to-equal '(once-x-call condition #'foo-mode))))

;; * :once-x-require
(describe "The :once-x-require setup.el keyword"
  (it "should expand to once-x-require"
    (expect (macroexpand-all '(setup foo
                                (:once-x-require condition 'bar)))
            :to-equal '(once-x-require condition 'bar)))
  (it "should infer the feature when none are specified"
    (expect (macroexpand-all '(setup foo
                                (:once-x-require condition)))
            :to-equal '(once-x-require condition 'foo)))
  (it "should infer the feature when nil is specified in the feature list"
    (expect (macroexpand-all '(setup foo
                                (:once-x-require condition 'a nil 'bar)))
            :to-equal '(once-x-require condition 'a 'foo 'bar))))

;;; test-once-setup.el ends here
