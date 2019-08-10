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

(defvar sinoword-dict nil)

(defvar sinoword-dict-path nil)

(defvar sinoword--root
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar sinoword--module-file
  (concat sinoword--root "build/libefriso" module-file-suffix))

(defun sinoword--config ()
  (unless sinoword-dict-path
    (setq sinoword-dict-path (concat sinoword--root "friso/friso.ini"))))

(defun sinoword--load()
  (unless (featurep 'sinoword)
    (load-file sinoword--module-file))
  ;; (unless (featurep 'sinoword)
  ;;   (t (error "cannot load libfriso")))
  (sinoword--config)
  (sinoword--load-dict sinoword-dict-path))

;; (defun sinoword-load-dict()
;;   (let ((dicts (mapcar (lambda (s)
;;                          (expand-file-name s sinoword-dict-path))
;;                        '("jieba.dict.utf8" "hmm_model.utf8"
;;                          "user.dict.utf8"
;;                          "idf.utf8"
;;                          "stop_words.utf8"))))
;;     (when (reduce (lambda (a b) (and (file-exists-p a) b))
;;                   (append dicts '(t)))

;;       (setq sinoword-dict
;;             (apply 'sinoword-make-dict dicts)))))

(defun sinoword-split-words (str)
  (sinoword--split-words sinoword-dict str))

(sinoword--load)
;; (defun sinoword-p (o)
;;   (sinoword-p o))

;; (defun sinoword-insert-user-word (word)
;;   (sinoword--insert-user-word sinoword-dict word))

(provide 'sinoword-friso-api)
