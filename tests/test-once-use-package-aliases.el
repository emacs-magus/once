;;; test-once-use-package-aliases.el --- Test aliases for once-use-package -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once
;; Package-Requires: ((emacs "26.1") (buttercup "1.38") (undercover "0.8.0"))
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
;; Test aliases for once-use-package.el
;;
;; For more information see the README in the online repository.

;;; Code:
(load-file "./tests/undercover-init.el")

(require 'buttercup)

;; aliases must be set before loading once-use-package
(defvar once-use-package-keyword-aliases)
(setq once-use-package-keyword-aliases
      '(:once-require-incrementally :require-incrementally))
(require 'once-use-package)

(setq use-package-always-defer t)

(defun test-once-use-package-compare (expansion expected-expansion)
  "Compare EXPANSION to EXPECTED-EXPANSION.
Remove most of the `use-package' boilerplate from EXPANSION."
  (let ((expansion-without-error-handling (nth 2 (nth 2 expansion))))
    (expect expansion-without-error-handling
            :to-equal expected-expansion)))

(describe "once-use-package-keyword-aliases"
  (it "should allow defining an alias for a keyword"
    (expect (functionp 'use-package-normalize/:require-incrementally))
    (expect (functionp 'use-package-handler/:require-incrementally))
    (test-once-use-package-compare
     (macroexpand-1 '(use-package foo
                       :require-incrementally t))
     '(once-require-incrementally foo))))

;;; test-once-use-package-aliases.el ends here
