;;; once-core.el --- Shared once.el code -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once
;; Created: April 14, 2022

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
;; once-core.el provides the group definition and globally shared utility
;; functions.  Package dependencies and keywords are defined in once-pkg.el.

;; For more information see the info manual or README in the online repository.

;;; Code:
(defgroup once nil
  "Provides extra deferred evaluation init.el utilities."
  :group 'convenience)

(defun once--function-p (form)
  "Return whether FORM (a form passed to a macro) is a function.
FORM is considered a function if it looks like \\='foo, #\\='foo, or (lambda ()
...)."
  (or (symbolp form)
      (and (listp form)
           (memq (car form) '(lambda function quote)))))

(provide 'once-core)
;; TODO https://github.com/alphapapa/makem.sh/issues/7#issuecomment-1141748201
;; LocalWords: arg args satch el uninterned init magit newval corge arglist
;; LocalWords: arglists normalizers
;;; once-core.el ends here
