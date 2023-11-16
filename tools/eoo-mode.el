;;; eoo-mode.el -*- lexical-binding: t -*-

(require 'compile)
(require 'mcore-mode)

;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep this list sorted
(defvar eoo--keywords
      '(
        "model"
        "output"
        "def"
        "end"
        "type"
        "initial"
        "connect"
        "to"
        "in"
        "with"
        "across"
        "through"
        "equation"
        "var"
        "node"
        "let"
        "main"
        "match"
        "if"
        "then"
        "else"
        "begin"
        ))

(defvar eoo--operators
      '(
        "+"
        "*"
        "-"
        "/"
        ";"
        "::"
        "'"
        ))

(defvar eoo--keywords-regexp (regexp-opt eoo--keywords 'symbols))
(defvar eoo--operators-regexp (regexp-opt eoo--operators 'symbols))

(defvar eoo-font-lock-keywords
    `(
         (,eoo--operators-regexp . font-lock-builtin-face)
         (,eoo--keywords-regexp  . font-lock-keyword-face)
         )
    "List of font lock keyword specifications to use in `eoo-mode'.")

(defvar eoo-mode-syntax-table
    (let ((table (make-syntax-table)))
        ;; Inline comment "-- ..."
        ;; Block comment "/- ... -/"
        (modify-syntax-entry ?- ". 123" table)
        (modify-syntax-entry ?/ ". 14cn" table)
        (modify-syntax-entry ?\n "> " table)
        ;; (modify-syntax-entry ?' "\"" table)
        table)
    "Syntax table for `eoo-mode'.")

;;;;;;;;;;;;;;;;;
;; compilation ;;
;;;;;;;;;;;;;;;;;

(defun eoo--setup-compile ()
  (mcore-setup-error-regexp)            ; We have the same error format
  ;; Set default compile command
  (set (make-local-variable 'compile-command)
       (concat "eoo " (buffer-name))))

;;;;;;;;;;;;;;;;;;;;;
;; mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode eoo-mode prog-mode "eoo"
    "Major mode for editing EOO code."
    (setq-local font-lock-defaults '(eoo-font-lock-keywords))
    (setq-local comment-start "--")
    (setq-local comment-end "")
    (eoo--setup-compile))

;; Open “*.syn” in eoo-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eoo\\'" . eoo-mode))

(provide 'eoo-mode)
;;; eoo-mode.el ends here
