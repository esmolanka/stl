;;; stl-mode.el --- Major mode for the Structural Types Language -*- lexical-binding: t -*-

;; Author: Eugene Smolanka <esmolanka@gmail.com>
;; Version: 0.0.0
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This library provides support for STL language.

;;; Code:

(defconst stl-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; (modify-syntax-entry ?# "<"  table)
    ;; (modify-syntax-entry ?\n ">"  table)
    ;; (modify-syntax-entry ?' "\"" table)
    ;; (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for STL files.")

(defvar stl-font-lock-keywords
  `(;; keywords
    ,(rx symbol-start
         (or
          "forall"
          "exists"
          "type"
          "mutual"
          "return"
          "module"
          "import")
         symbol-end)
    ;; functions
    ;; (,(rx symbol-start
    ;;       "fun" (1+ space) (group (1+ word)))
    ;;  (1 font-lock-function-name-face))
    ;; types
    (,(rx symbol-start
          "type" (1+ space) (group (1+ word))
          symbol-end) . (1 font-lock-type-face))))

;;;###autoload
(define-derived-mode stl-mode prog-mode "STL"
  "Major mode for Stl"
  :syntax-table stl-mode-syntax-table
  (setq-local font-lock-defaults
              '(stl-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "--+\\s-* "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stl\\'" . stl-mode))

;; (add-to-list 'eglot-server-programs '(stl-mode . ("/Users/eugene/sandbox/stl/bin/stl" "-lsp-debug" "/Users/eugene/sandbox/stl/debug.log")))

(provide 'stl-mode)
;;; stl-mode.el ends here
