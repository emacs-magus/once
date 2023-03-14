;;; test-once-setup-aliases.el --- Test aliases for once-setup.el -*- lexical-binding: t; -*-

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
;; Test aliases for once-setup.el
;;

;; For more information see the README in the online repository.

;;; Code:
(require 'buttercup)

(defvar once-setup-keyword-aliases)
(setq once-setup-keyword-aliases
      '(":once-require-incrementally" ":require-incrementally"))
(require 'once-setup)

(describe "once-setup-keyword-aliases"
  (it "should allow defining an alias for a keyword"
    (test-once-use-package-compare
     (macroexpand-1 '(setup foo
                       (:require-incrementally t)))
     '(once-require-incrementally foo))))

;;; test-once-setup-aliases.el ends here
