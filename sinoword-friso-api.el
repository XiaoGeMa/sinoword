;; (require 'sinoword)

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; (add-subdirs-to-load-path "~/Github/lazycat-emacs/site-lisp/sdcv-dict")
;; (add-subdirs-to-load-path "~/Github/lazycat-emacs/site-lisp/extensions")
;; (add-subdirs-to-load-path "~/elisp")
(add-subdirs-to-load-path "~/Github/sino-word/")
;; (require 'libefriso)

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
