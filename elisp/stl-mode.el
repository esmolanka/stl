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

    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
            "!@#$%^&*+|/:.,~<>")
    table)
  "Syntax table for STL files.")

(defconst stl-keyword-keywords
  '("forall"
    "exists"
    "type"
    "with"
    "provide"
    "module"
    "import"))

(defconst stl-keyword-hints
  '("#check" "#eval"))

(defconst stl-keyword-operators
  '("∀" "∃"))

(defconst stl-rx-constructor-name
  '(upper (* (in alnum digit ?_))
   (* ?. upper (* (in alnum digit ?_)))))

(defconst stl-rx-upper-name
  '(upper (* (in alnum digit ?_))))

(defconst stl-rx-lower-name
  '((or (seq (in lower ?_) (* (in alnum digit ?_)))
       (seq "'" (in lower ?_) (* (in alnum digit ?_)) "'"))))

(defvar stl-font-lock-keywords
  `(
    ;; Record labels
    (,(rx (or "," "{") (* space) (eval `(group ,@stl-rx-lower-name)) (* space) (? "?" (* space)) ":") .
     (1 font-lock-string-face))

    ;; Variant labels
    (,(rx (any "|" "<") (* space) (eval `(group-n 1 ,@stl-rx-upper-name)) symbol-end)
     (1 font-lock-string-face))

    ;; Keywords
    (,(rx symbol-start (eval `(or ,@stl-keyword-keywords)) symbol-end) .
     (0 font-lock-keyword-face))

    (,(rx (eval `(or ,@stl-keyword-hints)) symbol-end) .
     (0 font-lock-keyword-face))

    (,(rx (eval `(or ,@stl-keyword-operators)) (not word-boundary)) .
     (0 font-lock-keyword-face))

    ;; Type declarations
    (,(rx symbol-start (or "type" "with") (1+ space) (group upper (* (in alnum digit ?_)))) .
     (1 font-lock-type-face))

    ;; Module declarations
    (,(rx symbol-start "module" (1+ space) (eval `(group ,@stl-rx-constructor-name))) .
     (1 font-lock-constant-face))

    ;; Imports
    (,(rx symbol-start "import" (1+ space) (eval `(group ,@stl-rx-constructor-name)) (? (1+ space) (group "as") (1+ space) (eval `(group ,@stl-rx-constructor-name))))
     (1 font-lock-constant-face)
     (2 font-lock-keyword-face)
     (3 font-lock-constant-face))

    ;; Operators
    (,(rx (or bol (in space alnum digit ?_)) (group (or "->" "<:" ":" "=" "|" "?" "?:" "⊆" "→"))) .
     (1 font-lock-variable-name-face))))

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

(provide 'stl-mode)
;;; stl-mode.el ends here
