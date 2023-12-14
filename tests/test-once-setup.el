;;; test-once-setup.el --- Tests for once-setup.el -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once
;; Package-Requires: ((emacs "26.1") (buttercup "1.25") (undercover "0.8.0"))
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
;; Tests for once-setup.el
;;

;; TODO test aliases

;;; Code:
;; * Setup
(load-file "./tests/undercover-init.el")

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
            :to-equal '(once-x-call condition #'foo-mode)))
  (describe "should infer the function when given an arglist containing"
    (it "t as a placeholder"
      (expect (macroexpand-all '(setup foo
                                  (:once condition t #'foo-2-mode)))
              :to-equal '(once-x-call condition #'foo-mode #'foo-2-mode)))
    (it "nil as a placeholder"
      (expect (macroexpand-all '(setup foo
                                  (:once condition nil #'foo-2-mode)))
              :to-equal '(once-x-call condition #'foo-mode #'foo-2-mode)))))

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
  (describe "should infer the function when given an arglist containing"
    (it "t as a placeholder"
      (expect (macroexpand-all '(setup foo
                                  (:once-x-require condition 'a t 'bar)))
              :to-equal '(once-x-require condition 'a 'foo 'bar)))
    (it "nil as a placeholder"
      (expect (macroexpand-all '(setup foo
                                  (:once-x-require condition 'a nil 'bar)))
              :to-equal '(once-x-require condition 'a 'foo 'bar)))))

;; * :once-require-incrementally
(describe "The :once-require-incrementally setup.el keyword"
  (it "should expand to once-incrementally"
    (expect (macroexpand-all '(setup foo
                                (:once-require-incrementally bar)))
            :to-equal '(once-incrementally :features 'bar)))
  (it "should infer the feature when none are specified"
    (expect (macroexpand-all '(setup foo
                                (:once-require-incrementally)))
            :to-equal '(once-incrementally :features 'foo)))
  (describe "should infer the function when given an arglist containing"
    (it "t as a placeholder"
      (expect (macroexpand-all '(setup foo
                                  (:once-require-incrementally a t bar)))
              :to-equal '(once-incrementally :features 'a 'foo 'bar)))
    (it "nil as a placeholder"
      (expect (macroexpand-all '(setup foo
                                  (:once-require-incrementally a nil bar)))
              :to-equal '(once-incrementally :features 'a 'foo 'bar)))))

;;; test-once-setup.el ends here
