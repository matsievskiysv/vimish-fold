;;; vimish-fold.el --- Fold text like in Vim -*- lexical-binding: t; -*-
;;
;; Copyright © 2020–present Sergey Matsievskiy <matsievskiysv@gmail.com>
;; Copyright © 2015–2020 Mark Karpov <markkarpov92@gmail.com>
;; Copyright © 2012–2013 Magnar Sveen <magnars@gmail.com>
;;
;; Author: Sergey Matsievskiy <matsievskiysv@gmail.com>
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; Author: Magnar Sveen <magnars@gmail.com>
;; URL: https://github.com/mrkkrp/vimish-fold
;; Version: 0.4.0
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5") (f "0.18.0"))
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

;; This is a package to perform text folding like in Vim.  It has the
;; following features:
;;
;; * folding of active regions;
;;
;; * good visual feedback: it's obvious which part of text is folded;
;;
;; * persistence by default: when you kill a buffer your folds don't
;;   disappear;
;;
;; * persistence scales well, you can work on hundreds of files with lots of
;;   folds without adverse effects;
;;
;; * it does not break indentation;
;;
;; * folds can be toggled from folded state to unfolded and back very
;;   easily;
;;
;; * quick navigation between existing folds;
;;
;; * you can use mouse to unfold folds (good for beginners and not only for
;;   them);
;;
;; * for fans of `avy' package: you can use `avy' to fold text with minimal
;;   number of key strokes!

;;; Code:

(require 'cl-lib)
(require 'f)

(defgroup vimish-fold nil
  "Fold text like in Vim"
  :group  'text
  :tag    "Vimish Fold"
  :prefix "vimish-fold-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/vimish-fold"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functionality

(defface vimish-fold-overlay
  '((t (:inherit highlight)))
  "Face used to highlight the fold overlay.")

(defface vimish-fold-mouse-face
  '((t (:inherit highlight :weight bold)))
  "Face to use when mouse hovers over folded text.")

(defface vimish-fold-fringe
  '((t (:inherit font-lock-function-name-face)))
  "Face used to indicate folded text on fringe.")

(defcustom vimish-fold-indication-mode 'left-fringe
  "The indication mode for folded text areas.

This variable may have one of the following values:
`left-fringe', `right-fringe', or NIL.

If set to `left-fringe' or `right-fringe', indicate folded text
via icons in the left and right fringe respectively.

If set to NIL, do not indicate folded text, just highlight it."
  :tag "Indication on folded text"
  :type '(choice (const :tag "Indicate in the left fringe" left-fringe)
                 (const :tag "Indicate in the right fringe" right-fringe)
                 (const :tag "Do not indicate" nil)))

(defcustom vimish-fold-blank-fold-header "<blank fold>"
  "The string is used as fold header when it consists of blank characters."
  :tag  "Header of Blank Fold"
  :type 'string)

(defcustom vimish-fold-header-width nil
  "Width of header of folded region.

This can be a number or NIL.  If it's NIL value returned of
`window-width' will be used."
  :tag  "Width of header of folded region"
  :type '(choice (const   :tag "use window width")
                 (integer :tag "width of fold header"))
  :package-version '(vimish-fold . "0.4.0"))

(defcustom vimish-fold-show-lines t
  "Whether to show number of lines folded in fold header."
  :tag  "Show number of lines folded"
  :type 'boolean)

(defcustom vimish-fold-include-last-empty-line nil
  "Whether to include last empty line in selection into created fold."
  :tag  "Include last empty line into created fold"
  :type 'boolean
  :package-version '(vimish-fold . "0.2.1"))

(defcustom vimish-fold-persist-on-saving t
  "Whether to save folds on buffer saving.

Other than on saving, folds are also saved on buffer killing and
when user quits Emacs.  Turn this option off if the additional
overhead is undesirable."
  :tag "Save folds on buffer saving."
  :type 'boolean
  :package-version '(vimish-fold . "0.2.3"))

(defvar vimish-fold-folded-keymap (make-sparse-keymap)
  "Keymap which is active when point is placed on folded text.")

(defvar vimish-fold-unfolded-keymap (make-sparse-keymap)
  "Keymap which is active when point is placed on unfolded text.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <<< Helper functions

(defun vimish-fold--vimish-overlay-folded-p (overlay)
  "Detect if given OVERLAY is folded."
  (eq (overlay-get overlay 'type) 'vimish-fold--folded))

(defun vimish-fold--vimish-overlay-unfolded-p (overlay)
  "Detect if given OVERLAY is folded."
  (eq (overlay-get overlay 'type) 'vimish-fold--unfolded))

(defun vimish-fold--vimish-overlay-p (overlay)
  "Detect if given OVERLAY is created by this package."
  (memq (overlay-get overlay 'type)
        '(vimish-fold--folded
          vimish-fold--unfolded)))

(cl-defun vimish-fold--folds-in (beg end &optional (type 'all))
  "Return all folds exiting between BEG and END in current buffer.

Set TYPE to folded or unfolded to return only folded or unfolded
overlays.
When TYPE is omitted function returns all fold overlays."
  (overlay-recenter end)               ; speedup overlay lookup near end
  (cl-remove-if-not
   (cl-case type
     ('folded #'vimish-fold--vimish-overlay-folded-p)
     ('unfolded #'vimish-fold--vimish-overlay-unfolded-p)
     (t #'vimish-fold--vimish-overlay-p))
   (overlays-in beg end)))

(defun vimish-fold--read-only (on beg end)
  "If ON is non-NIL, make text between BEG and END read-only.

If ON is NIL, make the text editable again."
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (if on
          (add-text-properties beg end (list 'read-only on))
        (remove-text-properties beg end (list 'read-only on))))))

(defun vimish-fold--correct-region (beg end)
  "Return a cons of corrected BEG and END.

We only support folding by whole lines, so we should make sure
that beginning and end positions are correct.  Also, sometimes
users select region including last newline into it, they don't
really want to include it, we correct this here."
  (cl-destructuring-bind (beg . end)    ; sort BEG and END
      (if (>= end beg)
          (cons beg end)
        (cons end beg))
    (save-excursion
      (save-restriction
        (let ((beg* (progn (goto-char beg) ; Expand BEG and END to whole lines
                           (line-beginning-position)))
              (end* (progn (goto-char end)
                           (line-end-position))))
          (when (or vimish-fold-include-last-empty-line
                    (not (eq beg* end*)))
            (narrow-to-region beg* end*)
            (goto-char (point-min))
            ;; Iterate over strings in region and find empty ones
            (let* ((empty-lines (reverse (cl-loop if (progn (goto-char (line-beginning-position)) (looking-at "$"))
                                                  collect (line-number-at-pos)
                                                  end
                                                  until (progn (goto-char (line-end-position)) (eobp))
                                                  do (forward-line 1))))
                   (empty-lines-num (length empty-lines)))
              ;; Remove last line only if it is preceded by non-empty line
              ;; https://github.com/seregaxvm/vimish-fold/issues/55
              (cl-case empty-lines-num
                (0)
                (1 (when (eq (line-number-at-pos (point-max)) (car empty-lines))
                     (setq end* (1- end*))))
                (t (when (and (eq (line-number-at-pos (point-max)) (car empty-lines))
                              (> (- (car empty-lines) (cadr empty-lines)) 1))
                     (setq end* (1- end*)))))))
          (cons beg* end*))))))

(defun vimish-fold--get-header (beg end &optional buffer-or-name)
  "Extract folding header from region between BEG and END in BUFFER.

If BUFFER-OR-NAME is nil, current buffer is used."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (let ((info (when vimish-fold-show-lines
                  (format "    %d lines" (count-lines beg end)))))
      (save-excursion
        (goto-char beg)
        (re-search-forward "^\\([[:blank:]]*.*\\)$")
        (concat
         (truncate-string-to-width
          (if (and (>= (match-beginning 1) beg)
                   (<= (match-end 1)       end))
              (match-string-no-properties 1)
            vimish-fold-blank-fold-header)
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41537
          ;; when applying folds on newly created buffer,
          ;; `line-number-display-width' fails because window is not ready yet.
          ;; In this case assume 5 line number chars + 2 padding chars = 7 chars
          (- (or vimish-fold-header-width
                 (- (window-body-width)
                    (if (and (get-buffer-window (current-buffer))
                               line-number-mode)
                        (+ (line-number-display-width) 2)
                      7)))
             (length info))
          nil
          32 ; space
          "…")
         info)))))

(defun vimish-fold--setup-fringe (overlay &optional prefix)
  "Setup fringe for OVERLAY according to user settings.

If PREFIX is not NIL, setup fringe for every line."
  (when vimish-fold-indication-mode
    (unless (memq vimish-fold-indication-mode
                  '(left-fringe right-fringe))
      (error "Invalid fringe side: %S"
             vimish-fold-indication-mode))
    (overlay-put overlay (if prefix 'line-prefix 'before-string)
                 (propertize "…" 'display
                             (list vimish-fold-indication-mode
                                   'empty-line
                                   'vimish-fold-fringe)))))

(defun vimish-fold--apply-cosmetic (overlay header)
  "Make OVERLAY look according to user's settings displaying HEADER.

This includes fringe bitmaps and faces."
  (overlay-put overlay 'display
               (propertize header 'face 'vimish-fold-overlay))
  (overlay-put overlay 'pointer 'hand)
  (overlay-put overlay 'mouse-face 'vimish-fold-mouse-face)
  (overlay-put overlay 'help-echo "Click to unfold the text")
  (vimish-fold--setup-fringe overlay))

(defun vimish-fold--update-headers (frame)
  "Update headers"
  (dolist (buffer (buffer-list frame))
    (let ((window (get-buffer-window)))
      (when (and (not (eq (window-pixel-width-before-size-change window)
                          (window-pixel-height window))))
        ;; run for all buffers with changed width
        (with-current-buffer buffer
          (dolist (overlay (vimish-fold--folds-in (point-min)
                                                  (point-max)
                                                  'folded))
            (vimish-fold--apply-cosmetic overlay
                                         (vimish-fold--get-header
                                          (overlay-start overlay)
                                          (overlay-end overlay)))))))))

;; >>>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <<< Core functions

;;;###autoload
(defun vimish-fold (beg end)
  "Fold active region staring at BEG, ending at END."
  (interactive "r")
  (deactivate-mark)
  (cl-destructuring-bind (beg . end) (vimish-fold--correct-region beg end)
    (when (< (count-lines beg end) 2)
      (error "Nothing to fold"))
    (dolist (overlay (vimish-fold--folds-in beg end))
        (goto-char (overlay-start overlay))
        (error "Fold already exists here"))
    (vimish-fold--read-only t (max 1 (1- beg)) end)
    (let ((overlay (make-overlay beg end nil t nil)))
      (overlay-put overlay 'type 'vimish-fold--folded)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'keymap vimish-fold-folded-keymap)
      (vimish-fold--apply-cosmetic overlay (vimish-fold--get-header beg end)))
    (goto-char beg)))

(defun vimish-fold--unfold (overlay)
  "Unfold fold found by its OVERLAY type `vimish-fold--folded'."
  (when (vimish-fold--vimish-overlay-folded-p overlay)
    (let ((beg (overlay-start overlay))
          (end (overlay-end   overlay)))
      (vimish-fold--read-only nil (max 1 (1- beg)) end)
      (delete-overlay overlay)
      (let ((unfolded (make-overlay beg end nil t nil)))
        (overlay-put unfolded 'type 'vimish-fold--unfolded)
        (overlay-put unfolded 'evaporate t)
        (overlay-put unfolded 'keymap vimish-fold-unfolded-keymap)
        (vimish-fold--setup-fringe unfolded t)
        unfolded))))

;;;###autoload
(defun vimish-fold-unfold ()
  "Delete all `vimish-fold--folded' overlays at point."
  (interactive)
  (mapc #'vimish-fold--unfold (overlays-at (point))))

(define-key vimish-fold-folded-keymap (kbd "<mouse-1>") #'vimish-fold-unfold)
(define-key vimish-fold-folded-keymap (kbd "C-`")       #'vimish-fold-unfold)

(defun vimish-fold--refold (overlay)
  "Refold fold found by its OVERLAY type `vimish-fold--unfolded'."
  (when (vimish-fold--vimish-overlay-unfolded-p overlay)
    (when (buffer-narrowed-p)
      (error "Cannot operate on narrowed buffers"))
    (let ((beg (overlay-start overlay))
          (end (overlay-end   overlay)))
      (delete-overlay overlay)
      (vimish-fold beg end))))

;;;###autoload
(defun vimish-fold-refold ()
  "Refold unfolded fold at point."
  (interactive)
  (mapc #'vimish-fold--refold (overlays-at (point))))

(define-key vimish-fold-unfolded-keymap (kbd "C-`") #'vimish-fold-refold)

(defun vimish-fold--delete (overlay)
  "Internal function used to delete folds represented by OVERLAY.

If OVERLAY does not represent a fold, it's ignored."
  (when (vimish-fold--vimish-overlay-p overlay)
    (when (vimish-fold--vimish-overlay-folded-p overlay)
      (vimish-fold--read-only
       nil
       (max 1 (1- (overlay-start overlay)))
       (overlay-end overlay)))
    (delete-overlay overlay)))

;;;###autoload
(defun vimish-fold-delete ()
  "Delete fold at point."
  (interactive)
  (mapc #'vimish-fold--delete (overlays-at (point))))

;; >>>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <<< Extra features

;;;###autoload
(defun vimish-fold-unfold-all ()
  "Unfold all folds in current buffer."
  (interactive)
  (mapc #'vimish-fold--unfold
        (vimish-fold--folds-in
         (point-min)
         (point-max))))

;;;###autoload
(defun vimish-fold-refold-all ()
  "Refold all closed folds in current buffer."
  (interactive)
  (save-excursion ; after folding cursor jumps to beginning of fold
    (mapc #'vimish-fold--refold
          (vimish-fold--folds-in
           (point-min)
           (point-max)))))

;;;###autoload
(defun vimish-fold-delete-all ()
  "Delete all folds in current buffer."
  (interactive)
  (mapc #'vimish-fold--delete
        (vimish-fold--folds-in
         (point-min)
         (point-max))))

(defun vimish-fold--toggle (overlay)
  "Unfold or refold fold represented by OVERLAY depending on its type."
  (when (vimish-fold--vimish-overlay-p overlay)
    (save-excursion
      (goto-char (overlay-start overlay))
      (if (vimish-fold--vimish-overlay-folded-p overlay)
          (vimish-fold-unfold)
        (vimish-fold-refold)))))

;;;###autoload
(defun vimish-fold-toggle ()
  "Toggle fold at point."
  (interactive)
  (mapc #'vimish-fold--toggle (overlays-at (point))))

;;;###autoload
(defun vimish-fold-toggle-all ()
  "Toggle all folds in current buffer."
  (interactive)
  (mapc #'vimish-fold--toggle
        (vimish-fold--folds-in
         (point-min)
         (point-max))))

(declare-function avy-goto-line "ext:avy")

;;;###autoload
(defun vimish-fold-avy ()
  "Fold region of text between point and line selected with avy.

This feature needs `avy' package."
  (interactive)
  (if (require 'avy nil t)
      (let ((beg (point))
            (end (let (avy-all-windows)
                   (ignore avy-all-windows)
                   (call-interactively #'avy-goto-line)
                   (point))))
        (vimish-fold beg end))
    (message "Package ‘avy’ is unavailable")))

;;;###autoload
(defun vimish-fold-next-fold ()
  "Jump to next folded region in current buffer."
  (interactive)
  (let ((folds-after-point
         (cl-nset-difference
          (vimish-fold--folds-in (point) (point-max))
          (overlays-at (point)))))
    (if folds-after-point
        (goto-char
         (cl-reduce
          #'min
          (mapcar
           #'overlay-start
           folds-after-point)))
      (message "No more folds after point"))))

;;;###autoload
(defun vimish-fold-previous-fold ()
  "Jump to previous folded region in current buffer."
  (interactive)
  (let ((folds-before-point
         (cl-nset-difference
          (vimish-fold--folds-in (point-min) (point))
          (overlays-at (point)))))
    (if folds-before-point
        (goto-char
         (cl-reduce
          #'max
          (mapcar
           #'overlay-start
           folds-before-point)))
      (message "No more folds before point"))))

;;;###autoload
(defun vimish-fold-narrow-to-fold ()
  "Narrow to fold at current position."
  (interactive)
  (cl-loop for overlay in (overlays-at (point))
           if (vimish-fold--vimish-overlay-p overlay)
           do
           (when (vimish-fold--vimish-overlay-folded-p overlay)
             (setq overlay (vimish-fold--unfold overlay)))
           (narrow-to-region (overlay-start overlay)
                             (overlay-end overlay))
           end
           until (vimish-fold--vimish-overlay-p overlay)))

(define-key vimish-fold-folded-keymap (kbd "C-x n f") #'vimish-fold-narrow-to-fold)
(define-key vimish-fold-unfolded-keymap (kbd "C-x n f") #'vimish-fold-narrow-to-fold)


;; >>>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <<< Folding marks

(defcustom vimish-fold-marks '("{{{" . "}}}")
  "Strings marking beginning and end of folding region.

Automatically create folds from regions marked by `vimish-fold-marks' strings."
  :tag "Fold marks."
  :type '(cons :tag "Configure marks" string string)
  :package-version '(vimish-fold . "0.3.0")
  :safe #'vimish-fold--check-marks)
(make-variable-buffer-local 'vimish-fold-marks)

(defcustom vimish-fold-find-marks-on-open t
  "Whether to search for marks when opening buffer."
  :tag "Search for marks on open."
  :type 'boolean
  :package-version '(vimish-fold . "0.3.0"))

(defun vimish-fold--check-marks (arg)
  "Check that ARG is a valid `vimish-fold-marks' value."
  (and (listp arg)
       (stringp (car arg))
       (stringp (cdr arg))))

(defun vimish-fold--find-marks-in-region (beg end)
  "Find folding symbols in region between BEG and END.

Returns list of regions between marks in form
  (BEG END UNFOLDED)."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (start end)
        (cl-loop
         do
         (setq start (search-forward (car vimish-fold-marks) nil t)
               end (search-forward (cdr vimish-fold-marks) nil t))
         while (and start end)
         unless (= (line-number-at-pos start)
                   (line-number-at-pos end))
         collect (list start end 'UNFOLDED))))))

(defun vimish-fold--find-gaps ()
  "Find gaps between folds."
  (let ((overlays (vimish-fold--folds-in (point-min) (point-max)))
        positions)
    (setq positions
          (append (list (point-min))
                  (cl-mapcan #'list
                             (cl-sort (cl-mapcar 'overlay-start overlays) '<)
                             (cl-sort (cl-mapcar 'overlay-end overlays) '<))
                  (list (point-max))))
    (cl-remove-if
     (lambda (x) (let ((beg (line-number-at-pos (car x)))
                  (end (line-number-at-pos (cadr x))))
              (> 2 (- end beg))))
     (cl-loop while positions
              collect (list (pop positions) (pop positions))))))

;;;###autoload
(defun vimish-fold-from-marks ()
  "Create folds from folding symbols.

Mark strings are controlled by `vimish-fold-marks' customize variable."
  (interactive)
  (vimish-fold--restore-from
   (cl-mapcan (lambda (x) (apply 'vimish-fold--find-marks-in-region x))
              (vimish-fold--find-gaps))))

;; >>>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <<< Persistence

(defcustom vimish-fold-dir
  (file-name-as-directory (f-expand "vimish-fold" user-emacs-directory))
  "The directory where Vimish Fold keeps its files.

The string should end with a slash.  If it doesn't exist, it will
be created automatically."
  :tag   "Directory for Folding Info"
  :type  'directory)

(defun vimish-fold--make-file-name (file)
  "Return path to file where information about folding in FILE is written."
  (f-expand
   (replace-regexp-in-string
    (regexp-opt (list (f-path-separator) ":")) "!" file)
   vimish-fold-dir))

(defun vimish-fold--restore-from (list)
  "Restore folds in current buffer form LIST.

Elements of LIST should be of the following form:

  (BEG END &optional UNFOLDED)"
  (save-excursion
     (dolist (item list)
      (cl-destructuring-bind (beg end . rest) item
        (vimish-fold beg end)
        (when (car rest)
          (goto-char beg)
          (vimish-fold-unfold))))))

(defun vimish-fold--save-folds (&optional buffer-or-name)
  "Save folds in BUFFER-OR-NAME, which should have associated file.

BUFFER-OR-NAME defaults to current buffer."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (let ((filename (buffer-file-name))
          regions)
      (when (and vimish-fold-mode filename)
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (vimish-fold--vimish-overlay-p overlay)
            (push (list (overlay-start overlay)
                        (overlay-end   overlay)
                        (vimish-fold--vimish-overlay-unfolded-p overlay))
                  regions)))
        (let ((fold-file (vimish-fold--make-file-name (f-canonical filename))))
          (if regions
              (with-temp-buffer
                (pp regions (current-buffer))
                (let ((version-control 'never))
                  (condition-case nil
                      (progn
                        (apply #'f-mkdir (f-split vimish-fold-dir))
                        (write-region (point-min) (point-max) fold-file)
                        (message nil))
                    (file-error
                     (message "Vimish Fold: can't write %s" fold-file)))
                  (kill-buffer (current-buffer))
                  (message "Saved folds... %s" fold-file)
                  (message nil)))
            (when (f-exists? fold-file)
              (f-delete fold-file))))))))

(defun vimish-fold--restore-folds (&optional buffer-or-name)
  "Restore folds in BUFFER-OR-NAME, if they have been saved.

BUFFER-OR-NAME defaults to current buffer.

Return T is some folds have been restored and NIL otherwise."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (let ((filename (buffer-file-name)))
      (when (and filename
                 (null (vimish-fold--folds-in
                        (point-min)
                        (point-max))))
        (let ((fold-file (vimish-fold--make-file-name (f-canonical filename))))
          (when (and fold-file (f-readable? fold-file))
            (message "Restoring folds... %s" (buffer-file-name))
            (message nil)
            (vimish-fold--restore-from
             (with-temp-buffer
               (insert-file-contents fold-file)
               (read (buffer-string))))))))))

(defun vimish-fold--kill-emacs ()
  "Traverse all buffers and try to save their folds."
  (mapc #'vimish-fold--save-folds (buffer-list)))

;; >>>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <<< Mode definition

;;;###autoload
(define-minor-mode vimish-fold-mode
  "Toggle `vimish-fold-mode' minor mode.

With a prefix argument ARG, enable `vimish-fold-mode' mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode sets hooks so when you `find-file' it calls
`vimish-fold--restore-folds' and when you kill a file it calls
`vimish-fold--save-folds'.

For globalized version of this mode see `vimish-fold-global-mode'."
  :global nil
  (when (buffer-file-name)
    (if vimish-fold-mode
        (progn
          (vimish-fold--restore-folds)
          (when vimish-fold-find-marks-on-open
            (vimish-fold-from-marks)))
      (vimish-fold-delete-all))))

;;;###autoload
(define-globalized-minor-mode vimish-fold-global-mode
  vimish-fold-mode vimish-fold-mode)

(add-hook 'kill-buffer-hook #'vimish-fold--save-folds)
(when vimish-fold-persist-on-saving
  (add-hook 'before-save-hook #'vimish-fold--save-folds))
(add-hook 'kill-emacs-hook  #'vimish-fold--kill-emacs)
(unless vimish-fold-header-width
  (add-hook 'window-size-change-functions
            #'vimish-fold--update-headers))

;; >>>

(provide 'vimish-fold)

;; Local Variables:
;; vimish-fold-marks: ("<<<" . ">>>")
;; End:
;;; vimish-fold.el ends here
