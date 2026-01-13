;;; walnut.el --- Manipulate textobjects with tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.
;; Author: Adran Carnavale <adran.carnavale@gmail.com>
;; Version: 0.1

(require 'treesit)
(require 'transient)

;;; Functions

;;;###autoload
(defun walnut-select-inside-quotes ()
  "Select the content inside a Python string using the string_content node."
  (interactive)
  (let* ((query "((string (string_content) @content))")
         (node (treesit-node-at (point)))
         (string-node (treesit-parent-until
                       node
                       (lambda (n) (member (treesit-node-type n) '("string"))))))
    (if (not string-node)
        (user-error "Not inside a string")
      (let* ((captures (treesit-query-capture string-node query))
             (content-node (cdr (assoc 'content captures))))
        (if content-node
            (let ((beg (treesit-node-start content-node))
                  (end (treesit-node-end content-node)))
              (goto-char beg)
              (push-mark end t t))
          ;; If there's no string_content node, the string is likely empty ("")
          (message "String is empty"))))))

;;;###autoload
(defun walnut-select-around-quotes ()
  "Select the entire Python string, including the quotes."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (string-node (treesit-parent-until
                       node
                       (lambda (n) (member (treesit-node-type n) '("string"))))))
    (if (not string-node)
        (user-error "Not inside a string")
      (goto-char (treesit-node-start string-node))
      (push-mark (treesit-node-end string-node) t t))))

;;;###autoload
(defun walnut-select-inside-brackets ()
  "Select inside brackets. Falls back to point-between-brackets if empty."
  (interactive)
  (let* ((query "([ (list \"[\" @open \"]\" @close)
                    (tuple \"(\" @open \")\" @close)
                    (dictionary \"{\" @open \"}\" @close)
                    (subscript \"[\" @open \"]\" @close)
                    (list \"[\" . (_) @start (_) @end . \"]\")
                    (tuple \"(\" . (_) @start (_) @end . \")\")
                    (dictionary \"{\" . (_) @start (_) @end . \"}\")
                    (subscript \"[\" . (_) @start (_) @end . \"]\") ])")
         (node (treesit-node-at (point)))
         (container (treesit-parent-until
                     node
                     (lambda (n) (member (treesit-node-type n)
                                         '("list" "tuple" "dictionary" "subscript"))))))
    (if (not container)
        (user-error "Not inside brackets")
      (let* ((captures (treesit-query-capture container query))
             (start (cdr (assoc 'start captures)))
             (end (cdr (assoc 'end captures)))
             (open (cdr (assoc 'open captures))))
        (cond ((and start end)
               (goto-char (treesit-node-start start))
               (push-mark (treesit-node-end end) t t))
              (open
               (goto-char (treesit-node-end open))
               (message "Empty collection")))))))

;;;###autoload
(defun walnut-select-around-brackets ()
  "Select the entire Python collection, including the brackets."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (container (treesit-parent-until
                     node
                     (lambda (n) (member (treesit-node-type n)
                                         '("list" "tuple" "dictionary" "subscript"))))))
    (if (not container)
        (user-error "Not inside brackets")
      (goto-char (treesit-node-start container))
      (push-mark (treesit-node-end container) t t))))

;;; UI and Dispatcher

;;;###autoload
(transient-define-prefix walnut-dispatch ()
  "Dispatch a walnut command."
  ["Textobjects"
   ("q" "Inside Quotes" walnut-select-inside-quotes)
   ("Q" "Around Quotes" walnut-select-around-quotes)
   ("b" "Inside Brackets" walnut-select-inside-brackets)
   ("B" "Around Brackets" walnut-select-around-brackets)])

;;; Initialization

(defvar walnut-prefix-key "C-c w"
  "Prefix key for Walnut commands.")

;;;###autoload
(progn
  (keymap-global-set walnut-prefix-key #'walnut-dispatch))

(provide 'walnut)

;;; walnut.el ends here
