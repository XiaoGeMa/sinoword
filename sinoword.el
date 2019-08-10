;;; sinoword.el  --- Use nodesinoword chinese segmentation in Emacs  -*- lexical-binding: t -*-

;; Author: XiaoGeMa <coolmaq@gmail.com>
;; URL: http://github.com/sinoword.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2") (jsonrpc "1.0.7"))
;; Keywords: chinese

;; This file is NOT a part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'thingatpt)

(require 'sinoword-friso-api)

(eval-when-compile
  (require 'cl-lib))

;;; Customize

(defgroup sinoword ()
  ""
  :group 'chinese
  :prefix "sinoword-")

;;; Utils

(defun sinoword--current-dir ()
  (let* ((this-file (cond
                     (load-in-progress load-file-name)
                     ((and (boundp 'byte-compile-current-file)
                           byte-compile-current-file)
                      byte-compile-current-file)
                     (t (buffer-file-name))))
         (dir (file-name-directory this-file)))
    dir))

;;; Backend Access API

(cl-defgeneric sinoword-do-split (backend str))

(cl-defgeneric sinoword-load-dict (backend dicts))

;;; Data Cache

(defvar sinoword--cache (make-hash-table :test #'equal))

(defun sinoword--cache-gc ())

(cl-defmethod sinoword-do-split :around ((_backend t) string)
  "Access cache if used."
  (let ((not-found (make-symbol "hash-not-found"))
        result)
    (if (not sinoword-use-cache)
        (cl-call-next-method)
      (setq result (gethash string sinoword--cache not-found))
      (if (eq not-found result)
          (prog1 (setq result (cl-call-next-method))
            (puthash string result sinoword--cache))
        result))))


;;; Export function

(defvar sinoword--single-chinese-char-re "\\cC")

(defun sinoword-split-chinese-word (str)
  (sinoword-split-words str))
  ;; (sinoword-split-words str))
  ;; (sinoword-word--split-by-friso str))

(defsubst sinoword-chinese-word? (s)
  "Return t when S is a real chinese word (All its chars are chinese char.)"
  (and (string-match-p (format "%s\\{%d\\}"
                               sinoword--single-chinese-char-re
                               (length s)) s)
       t))

(defalias 'sinoword-chinese-word-p 'sinoword-chinese-word?)

;;;###autoload
(defun sinoword-chinese-word-atpt-bounds ()
  ;; (sinoword--assert-server)
  (pcase (bounds-of-thing-at-point 'word)
    (`(,beg . ,end)
     (let ((word (buffer-substring-no-properties beg end)))
       (if (sinoword-chinese-word? word)
         (let ((cur (point))
               (index beg)
               (old-index beg))
           (cl-block retval
             (mapc (lambda (x)
                     (cl-incf index (length x))
                     (cond
                      ((or (< old-index cur index)
                           (= old-index cur))
                       (cl-return-from retval (cons old-index index)))
                      ((= index end)
                       (cl-return-from retval (cons old-index index)))
                      (t
                       (setq old-index index))))
                   (sinoword-split-chinese-word word))))
         (cons beg  end))))))


(defun sinoword--move-chinese-word (backward?)
  (cl-labels
      ((find-dest (backward?)
                  (pcase (sinoword-chinese-word-atpt-bounds)
                    (`(,beg . ,end)
                     (if backward? beg end))))

       (try-backward-move (backward?)
                          (let (pnt beg)
                            (save-excursion
                              (if backward? (backward-char) (forward-char))
                              (setq pnt (point))
                              (setq beg (find-dest backward?)))
                            (goto-char pnt)
                            (when (or (null beg)
                                      (not (= beg pnt)))
                              (sinoword--move-chinese-word backward?)))))

    (let* ((dest (find-dest backward?))
           (cur (point)))
      (cond
       ((null dest)
        (if backward?
            (if (looking-back sinoword--single-chinese-char-re
                              (car (bounds-of-thing-at-point 'word)))
                (try-backward-move backward?)
              (backward-word))
          ;; (skip-chars-forward "\n\r\t\f ")
          (skip-chars-forward "^[:word:]")
          ;; (forward-word)
          (sinoword--move-chinese-word backward?)
          ))
       ((= dest cur)
        (try-backward-move backward?))
       (t
        (goto-char dest)
        (skip-chars-forward "\n\r\t\ ")
        )))))

;;;###autoload
(defun sinoword-forward-word (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (let ((backward? (< arg 0)))
    (dotimes (_ (abs arg))
      (sinoword--move-chinese-word backward?))))

;;;###autoload
(defun sinoword-backward-word (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (sinoword-forward-word (- arg)))

;;;###autoload
(defun sinoword-kill-word (arg)
  (interactive "p")
  (kill-region (point) (progn (sinoword-forward-word arg) (point))))

;;;###autoload
(defun sinoword-backward-kill-word (arg)
  (interactive "p")
  (sinoword-kill-word (- arg)))

;;;###autoload
(defun sinoword-mark-word ()
  (interactive)
  (end-of-thing 'sinoword-chinese-word)
  (set-mark (point))
  (beginning-of-thing 'sinoword-chinese-word))

;;; Minor mode

;;;###autoload
(defvar sinoword-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] #'sinoword-forward-word)
    ;; (define-key evil-motion-state-map "w" 'sinoword-forward-word)
    ;; (define-key evil-motion-state-map "b" 'sinoword-backward-word)
    (evil-define-minor-mode-key '(normal visual) 'sinoword-mode "w" 'sinoword-forward-word)
    (evil-define-minor-mode-key '(normal visual) 'sinoword-mode "e" 'sinoword-forward-word)
    (evil-define-minor-mode-key '(normal visual) 'sinoword-mode "b" 'sinoword-backward-word)
    (define-key map [remap backward-word] #'sinoword-backward-word)
    (define-key map [remap kill-word] #'sinoword-kill-word)
    (define-key map [remap backward-kill-word] #'sinoword-backward-kill-word)
    map))

;;;###autoload
(define-minor-mode sinoword-mode
  ""
  :global nil
  :keymap sinoword-mode-map
  :lighter " Sinoword"
  )

(provide 'sinoword)

;; Define text object
(put 'sinoword-chinese-word
     'bounds-of-thing-at-point 'sinoword-chinese-word-atpt-bounds)

;; (cl-eval-when (load eval)
;;   (require 'sinoword-node))

;;; sinoword.el ends here
