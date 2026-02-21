;;; test-once-setup-aliases.el --- Test aliases for once-setup.el -*- lexical-binding: t; -*-

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
;; Test aliases for once-setup.el
;;

;; For more information see the README in the online repository.

;;; Code:
(load-file "./tests/undercover-init.el")

(require 'buttercup)

(defvar once-setup-keyword-aliases)
(setq once-setup-keyword-aliases
      '(":once-require-incrementally" ":require-incrementally"))
(require 'once-setup)

(describe "once-setup-keyword-aliases"
  (it "should allow defining an alias for a keyword"
    (expect (macroexpand-1 '(setup foo
                              (:require-incrementally t)))
            :to-equal (macroexpand-1 '(once-require-incrementally foo)))))

(makunbound 'once-setup-keywoard-aliases)
(unload-feature 'once-setup)

;;; test-once-setup-aliases.el ends here
