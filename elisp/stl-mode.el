;;; stl-mode.el --- Major mode for the Structural Types Language -*- lexical-binding: t -*-

;; Author: Eugene Smolanka <esmolanka@gmail.com>
;; Version: 0.0.0
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This library provides support for STL language.

;;; Code:

(load "flycheck" t t)

(defgroup stl nil
  "Mode for STL scripts"
  :group 'languages)

(defcustom stl-mode-exe "stl"
  "Path to `stl` executable."
  :group 'stl
  :type 'string)


(defconst stl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\- ". 12" table)
    (modify-syntax-entry ?\n ">" table)

    (modify-syntax-entry ?\  " "  table)
    (modify-syntax-entry ?\t " "  table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?_ "_"   table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; (modify-syntax-entry ?\< "(>" table)
    ;; (modify-syntax-entry ?\> ")>" table)

    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
            "!@#$%^&*+|/:.,~<>")
    table)
  "Syntax table for STL files.")

(defconst stl-keyword-keywords
  '("forall"
    "exists"
    "type"
    "mutual"
    "return"
    "module"
    "import"))

(defconst stl-keyword-hints
  '("#check"
    "#eval"))

(defconst stl-keyword-operators
  '("\\="
    "\\:"
    "\\-\\>"
    "\|"
    "\\."
    "\\,"
    "\\<\\:"
    "\\+"))

(defconst stl-keyword-constants
  '("Unit"
    "Void"
    "Integer"
    "Double"
    "String"
    "List"
    "Dictionary"
    "Natural"
    "Array"))

(defconst stl-keyword-kinds
  '("Type"
    "Row"
    "Nat"))

(defvar stl-font-lock-keywords
  `(
    (,(rx symbol-start (eval `(or ,@stl-keyword-keywords)) symbol-end) .
     (0 font-lock-keyword-face))
    (,(rx (eval `(or ,@stl-keyword-hints)) symbol-end) .
     (0 font-lock-keyword-face))
    (,(rx symbol-start (eval `(or ,@stl-keyword-constants)) symbol-end) .
     (0 font-lock-constant-face))
    (,(rx symbol-start (eval `(or ,@stl-keyword-kinds)) symbol-end) .
     (0 font-lock-type-face))
    ;; (,(rx (eval `(or ,@stl-keyword-operators))) .
    ;;  (0 font-lock-operator-face))
    (,(rx symbol-start "type" (1+ space) (group (1+ (or word "_"))) symbol-end) .
     (1 font-lock-type-face))
    ))

;;;###autoload
(define-derived-mode stl-mode prog-mode "STL"
  "Major mode for Stl"
  :syntax-table stl-mode-syntax-table
  (setq-local font-lock-defaults '(stl-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "--+\\s-* "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stl\\'" . stl-mode))

(when (featurep 'flycheck)
  (flycheck-define-checker stl
      "Checker for STL"
      :command ("stl" source-original)
      :error-patterns
      ( (error
         line-start
         (file-name (+ not-newline) ".stl") ":"
         line ":"
         column "-"
         (+ digit) ":"
         (+ digit) ":"
         (* " ") "error:"
         (message (* not-newline)
                  (* (seq "\n"
                          (optional
                           (+ " ")
                           (+ not-newline))))))
        (info
         line-start
         (file-name (+ not-newline) ".stl") ":"
         line ":"
         column "-"
         (+ digit) ":"
         (+ digit) ":"
         (* " ")
         (message (* not-newline)
                  (* (seq "\n"
                          (optional
                           (+ " ")
                           (+ not-newline)))))))
      :modes (stl-mode))
  (add-to-list 'flycheck-checkers 'stl))

;; (add-to-list 'eglot-server-programs '(stl-mode . (stl-mode-exe "--lsp")))

(provide 'stl-mode)
;;; stl-mode.el ends here
