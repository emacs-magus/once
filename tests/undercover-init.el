;;; undercover-init.el --- Setup for undercover.el -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noctuid@pir-hana.cafe>
;; URL: https://github.com/emacs-magus/once

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
;; Setup for undercover.el.
;;

;; For more information see the README in the online repository.

;;; Code:
(when (require 'undercover nil t)
  (undercover "*.el" "once-setup/*.el" "once-use-package/*.el"
              (:exclude "test-*.el")
              (:report-format 'codecov)
              (:send-report nil)))

;;; undercover-init.el ends here
