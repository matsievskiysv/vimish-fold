;;; vimish-fold.el --- Fold text like in Vim -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/vimish-fold
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (f "0.18.0"))
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

;; This is package to do text folding like in Vim. It has the following
;; features:
;;
;; * it works on regions you select;
;;
;; * it's persistent: when you close file your folds don't disappear;
;;
;; * it's obvious which parts of text are folded;
;;
;; * it doesn't break anything.

;;; Code:

(require 'cl-lib)
(require 'f)

;; TODO write the package here

(provide 'vimish-fold)

;;; vimish-fold.el ends here
