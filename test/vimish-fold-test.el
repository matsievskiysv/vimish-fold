;;; vimish-fold-test.el --- Tests for Vimish Fold -*- lexical-binding: t; -*-
;;
;; Copyright © 2018–present Mark Karpov <markkarpov92@gmail.com>
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

(require 'cl-lib)
(require 'vimish-fold)

(defmacro with-test-file (file &rest body)
  "Run BODY in a temporary buffer with test FILE loaded."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents ,file)
     ,@body))

(defun compare-regions (region1 region2)
  "Helper function to compare REGION1 to REGION2."
  (cl-destructuring-bind (beg1 . end1) region1
    (cl-destructuring-bind (beg2 . end2) region2
      (and (= beg1 beg2) (= end1 end2)))))

(ert-deftest vimish-fold-vimish-fold/test-correct-region ()
  "Test `vimish-fold--correct-region' function."
  (with-test-file "test/fold.txt"
   (should (compare-regions (vimish-fold--correct-region 1 1) '(1 . 76)))
   (should (compare-regions (vimish-fold--correct-region 10 80) '(1 . 152)))
   (should (compare-regions (vimish-fold--correct-region 550 600) '(528 . 633)))
   (should (compare-regions (vimish-fold--correct-region 550 634) '(528 . 633)))
   (should (compare-regions (vimish-fold--correct-region 633 633) '(599 . 633)))
   (should (compare-regions (vimish-fold--correct-region 634 634) '(634 . 634)))
   (should (compare-regions (vimish-fold--correct-region 1324 2432) '(1324 . 2431))))
  (with-test-file
   "test/marks.py"
   (should (compare-regions (vimish-fold--correct-region 1 1) '(1 . 22)))
   (should (compare-regions (vimish-fold--correct-region 10 38) '(1 . 37)))
   (should (compare-regions (vimish-fold--correct-region 116 132) '(116 . 131)))
   (should (compare-regions (vimish-fold--correct-region 116 133) '(116 . 133)))))

(ert-deftest vimish-fold-vimish-fold/test-fold ()
  "Test `vimish-fold' function."
  (with-test-file "test/fold.txt"
    (vimish-fold 666 1324)
    (let* ((os (vimish-fold--folds-in (point-min) (point-max)))
           (o  (car os)))
      (should-not (null o))
      (should (eq (overlay-get o 'type)
                  'vimish-fold--folded))
      (should (= (overlay-start o) 635))
      (should (= (overlay-end   o) 1323)))))

(ert-deftest vimish-fold-vimish-fold/test-marks ()
  "Test `vimish-fold-from-marks' function."
  (with-test-file "test/marks.py"
    (setq vimish-fold-marks '("{{{" . "}}}"))
    (vimish-fold-delete-all)
    (vimish-fold-from-marks)
    (should (equal (cl-sort (cl-mapcar 'overlay-start (vimish-fold--folds-in (point-min) (point-max))) '<) '(24 59)))
    (should (equal (cl-sort (cl-mapcar 'overlay-end (vimish-fold--folds-in (point-min) (point-max))) '<) '(57 114)))
    (should (cl-reduce '(lambda (x o) (and (eq (overlay-get o 'type) 'vimish-fold--unfolded) x)) (vimish-fold--folds-in (point-min) (point-max)) :initial-value t))
    (vimish-fold-refold-all)
    (should (cl-reduce '(lambda (x o) (and (eq (overlay-get o 'type) 'vimish-fold--folded) x)) (vimish-fold--folds-in (point-min) (point-max)) :initial-value t))
    (setq vimish-fold-marks '("<<<" . ">>>"))
    (vimish-fold-delete-all)
    (vimish-fold-from-marks)
    (should (equal (cl-sort (cl-mapcar 'overlay-start (vimish-fold--folds-in (point-min) (point-max))) '<) '(116)))
    (should (equal (cl-sort (cl-mapcar 'overlay-end (vimish-fold--folds-in (point-min) (point-max))) '<) '(267)))
    (should (cl-reduce '(lambda (x o) (and (eq (overlay-get o 'type) 'vimish-fold--unfolded) x)) (vimish-fold--folds-in (point-min) (point-max)) :initial-value t))
    (vimish-fold-refold-all)
    (should (cl-reduce '(lambda (x o) (and (eq (overlay-get o 'type) 'vimish-fold--folded) x)) (vimish-fold--folds-in (point-min) (point-max)) :initial-value t))))

;;; vimish-fold-test.el ends here
