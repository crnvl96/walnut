;;; walnut.el --- Advanced Tree-sitter Selection Engine -*- lexical-binding: t -*-

(require 'treesit)
(require 'transient)
(require 'cl-lib)

(defvar walnut-queries-file (expand-file-name "queries.scm" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the master queries.scm file.")

(defvar walnut--mode-to-lang-map
  '((python-ts-mode . python)
    (python-mode    . python))
  "Map major modes to tree-sitter language symbols.")

;; --- UTILITIES ---

(defun walnut--get-all-queries-for-lang (lang type)
  "Extract all query patterns for LANG and TYPE from queries.scm."
  (with-temp-buffer
    (insert-file-contents walnut-queries-file)
    (goto-char (point-min))
    (let ((header (format "[%s :%s]" (symbol-name lang) (substring (symbol-name type) 1)))
          (start nil)
          (patterns '()))
      (when (search-forward header nil t)
        (forward-line 1)
        (setq start (point))
        ;; Read until the next header or end of file
        (while (and (not (eobp)) (not (looking-at "\n;; \\[")))
          (forward-sexp 1)
          (push (buffer-substring-no-properties start (point)) patterns)
          (skip-chars-forward " \t\n")
          (setq start (point))))
      (mapconcat #'identity (nreverse patterns) "\n"))))

;; --- THE ENGINE ---

(defun walnut--select-text-object (type scope)
  "Universal selector. TYPE is :quotes, :brackets, etc. SCOPE is :inside or :around."
  (let* ((lang (or (alist-get major-mode walnut--mode-to-lang-map) (user-error "Mode not mapped")))
         (query-str (walnut--get-all-queries-for-lang lang type))
         (curr-node (treesit-node-at (point)))
         (found-node nil)
         (relevant-captures nil)
         (target-capture-name (if (eq scope :inside) "content" "around")))

    (if (string-empty-p query-str)
        (message "Walnut: No queries found for %s" type)

      ;; Search up the tree
      (while (and curr-node (not found-node))
        (let* ((captures (ignore-errors (treesit-query-capture curr-node lang query-str)))
               ;; Filter for the specific capture name (content vs around)
               (matches (cl-remove-if-not
                         (lambda (c) (string= (symbol-name (car c)) target-capture-name))
                         captures)))
          (if matches
              (setq found-node curr-node
                    relevant-captures matches)
            (setq curr-node (treesit-node-parent curr-node)))))

      (if found-node
          (let* ((beg (treesit-node-start (cdr (car relevant-captures))))
                 (end (treesit-node-end (cdr (car (last relevant-captures))))))
            (deactivate-mark)
            (goto-char beg)
            (push-mark end t t)
            (activate-mark)
            (message "Walnut: Selected %s %s (%s)" type scope (treesit-node-type found-node)))
        (message "Walnut: No matching %s found." type)))))

;; --- COMMANDS ---

(defun walnut-select-inside-quotes () (interactive) (walnut--select-text-object :quotes :inside))
(defun walnut-select-around-quotes () (interactive) (walnut--select-text-object :quotes :around))
(defun walnut-select-inside-brackets () (interactive) (walnut--select-text-object :brackets :inside))
(defun walnut-select-around-brackets () (interactive) (walnut--select-text-object :brackets :around))
(defun walnut-select-inside-block () (interactive) (walnut--select-text-object :blocks :inside))

;; --- MENU ---

(transient-define-prefix walnut-dispatch ()
  "Walnut: Tree-sitter Text Objects"
  [["Quotes"
    ("q" "Inside" walnut-select-inside-quotes)
    ("Q" "Around" walnut-select-around-quotes)]
   ["Brackets/Collections"
    ("b" "Inside" walnut-select-inside-brackets)
    ("B" "Around" walnut-select-around-brackets)]
   ["Blocks (Fn/Class)"
    ("k" "Inside" walnut-select-inside-block)]]
  [("q" "Quit" transient-quit-one)])

(keymap-global-set "C-c s" #'walnut-dispatch)
(provide 'walnut)
;;; walnut.el ends here
