;;; vimish-fold-test.el --- Tests for Vimish Fold -*- lexical-binding: t; -*-
;;
;; Copyright © 2018 Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'undercover)

(undercover "vimish-fold.el")

(require 'cl-lib)
(require 'vimish-fold)

(defmacro with-test-file (&rest body)
  "Run BODY in a temporary buffer with test file loaded."
  `(with-temp-buffer
     (insert-file-contents "test/test.txt")
     ,@body))

(ert-deftest vimish-fold-vimish-fold/successful ()
  (with-test-file
   (vimish-fold 666 1324)
   (let* ((os (vimish-fold--folds-in (point-min) (point-max)))
          (o  (car os)))
     (should-not (null o))
     (should (eq (overlay-get o 'type)
                 'vimish-fold--folded))
     (should (= (overlay-start o) 635))
     (should (= (overlay-end   o) 1323)))))

(provide 'vimish-fold-test)

;;; vimish-fold-test.el ends here
