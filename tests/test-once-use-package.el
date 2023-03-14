;;; test-once-use-package.el --- Tests for once-use-package.el -*- lexical-binding: t; -*-

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
;; Tests for once-use-package.el
;;

;; For more information see the README in the online repository.

;;; Code:
;; * Setup
(when (require 'undercover nil t)
  (undercover "once-setup/*.el"
              (:exclude "test-*.el")
              (:report-format 'codecov)
              (:send-report nil)))

(require 'buttercup)
(require 'once-use-package)

(setq use-package-always-defer t)

(defun test-once-use-package-compare (expansion expected-expansion)
  "Compare EXPANSION to EXPECTED-EXPANSION.
Remove most of the `use-package' boilerplate from EXPANSION."
  (let ((expansion-without-error-handling (nth 2 (nth 2 expansion))))
    (expect expansion-without-error-handling
            :to-equal expected-expansion)))

;; * :once
(describe "The :once use-package keyword"
  (describe "should expand to once when given"
    (it "a once arglist"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once ((list :files "bar") #'foo-mode)))
       '(once (list :files "bar") #'foo-mode)))
    (it "multiple once arglist"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once
                         ("bar" #'foo-mode)
                         ("baz" #'foo-mode2)))
       '(progn (once "bar" #'foo-mode)
               (once "baz" #'foo-mode2)))))
  (describe "should support shorthand and infer the function for"
    (it "a file name"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once "bar"))
       '(once "bar" #'foo-mode)))
    (it "a hook or quoted function"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once 'bar-hook))
       '(once 'bar-hook #'foo-mode))
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once 'bar))
       '(once 'bar #'foo-mode)))
    (it "a sharp-quoted function"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once #'bar))
       '(once #'bar #'foo-mode)))
    (it "a variable holding a condition"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once bar-condition))
       '(once bar-condition #'foo-mode))))
  (it "should infer the function when given an incomplete arglist"
    (test-once-use-package-compare
     (macroexpand-1 '(use-package foo
                       :once ((list :files "bar"))))
     '(once (list :files "bar") #'foo-mode)))
  (describe "should infer the function when given an arglist containing"
    (it "t as a placeholder"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once ((list :files "bar") t #'foo-extra-mode)))
       '(once (list :files "bar") #'foo-mode #'foo-extra-mode)))
    (it "nil as a placeholder"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once ((list :files "bar") nil #'foo-extra-mode)))
       '(once (list :files "bar") #'foo-mode #'foo-extra-mode)))))

;; * :once-x-require
(describe "The :once-x-require use-package keyword"
  (describe "should expand to once-x-require when given"
    (it "a once-x-require arglist"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require ((list :files "bar") 'foo)))
       '(once-x-require (list :files "bar") 'foo)))
    (it "multiple once-x-require arglists"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require
                         ("bar" 'foo)
                         ("baz" 'foo2)))
       '(progn (once-x-require "bar" 'foo)
               (once-x-require "baz" 'foo2)))))
  (describe "should support shorthand and infer the package for"
    (it "a file name"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require "bar"))
       '(once-x-require "bar" 'foo)))
    (it "a hook or quoted function"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require 'bar-hook))
       '(once-x-require 'bar-hook 'foo))
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require 'bar))
       '(once-x-require 'bar 'foo)))
    (it "a sharp-quoted function"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require #'bar))
       '(once-x-require #'bar 'foo)))
    (it "a varibale holding a condition"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require bar-condition))
       '(once-x-require bar-condition 'foo))))
  (it "should infer the package when given an incomplete arglist"
    (test-once-use-package-compare
     (macroexpand-1 '(use-package foo
                       :once-x-require ((list :files "bar"))))
     '(once-x-require (list :files "bar") 'foo)))
  (describe "should infer the function when given an arglist containing"
    (it "t as a placeholder"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require ((list :files "bar") t 'foo-extra)))
       '(once-x-require (list :files "bar") 'foo 'foo-extra)))
    (it "nil as a placeholder"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-x-require ((list :files "bar") nil 'foo-extra)))
       '(once-x-require (list :files "bar") 'foo 'foo-extra)))))

;; * :once-require-incrementally
(describe "the :once-require-incrementally use-package keyword"
  (describe "should expand to once-require-incrementally when given"
    (it "a once-require-incrementally arglist"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally (foo)))
       '(once-require-incrementally foo))
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally (foo bar)))
       '(once-require-incrementally foo bar)))
    (it "an unquoted symbol"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally foo))
       '(once-require-incrementally foo)))
    (it "multiple unquoted symbols"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally foo bar))
       '(progn (once-require-incrementally foo)
               (once-require-incrementally bar)))))
  (describe "should infer the package when"
    (it "t appears as a placeholder symbol"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally t bar))
       '(progn (once-require-incrementally foo)
               (once-require-incrementally bar))))
    (it "t appears as a placeholder in an arglist"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally (t bar)))
       '(once-require-incrementally foo bar)))
    (it "nil appears as a placeholder symbol"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally nil bar))
       '(progn (once-require-incrementally foo)
               (once-require-incrementally bar))))
    (it "nil appears as a placeholder in an arglist"
      (test-once-use-package-compare
       (macroexpand-1 '(use-package foo
                         :once-require-incrementally (nil bar)))
       '(once-require-incrementally foo bar)))))

;; * once-use-package-keyword-aliases
(describe "once-use-package-keyword-aliases"
  (before-all
    (setq once-use-package-keyword-aliases
          '(":once-require-incrementally" ":require-incrementally"))
    (once-use-package-setup))
  (after-all
    (setq once-use-package-keyword-aliases nil)
    (once-use-package-setup))
  (it "should allow defining an alias for a keyword"
    (expect (functionp 'use-package-normalize/:require-incrementally))
    (test-once-use-package-compare
     (macroexpand-1 '(use-package foo
                       :require-incrementally t))
     '(once-require-incrementally foo))))

;;; test-once-use-package.el ends here
