;;; dendroam.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Rodriguez
;;
;; Author: Victor Rodriguez <https://github.com/vrodriguez>
;; Maintainer: Victor Rodriguez <vrodriguez@confluent.io>
;; Created: April 26, 2021
;; Modified: April 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/vrodriguez/dendroam
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(provide 'dendroam)

(require 's)
(require 'org-roam-node) ;; Needed for node functions like org-roam-node-file, etc.

(defvar dendroam-capture-templates
  '(("t" "Time note" entry
     "* %?"
     :if-new (file+head "${current-file}.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n"))
    ("s" "Scratch note" entry
     "* %?"
     :if-new (file+head "scratch.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n")))

  "Some utils templates for different type of notes such us time notes
or sratch notes")

;;Node custom getters
(cl-defmethod org-roam-node-current-file (node)
  "Gets node file-name-base by file name"
  (file-name-base (org-roam-node-file node)))

(cl-defmethod org-roam-node-hierarchy-title (node)
  "Gets node title excluding the hierarchy and capitalize it"
  (capitalize
   (car
    (last
     (split-string
      (org-roam-node-title node)
      "\\.")))))

(defun dendroam-format-hierarchy (file)
  "Formats node's path, to get the hierarchy whithout the title
where title will be the last child of the hierarchy:
from the filename this.is.a.hierarchy.note-title.org
returns this.is.a.hierarchy"
  (let* ((base-name (file-name-base file))
         (hierarchy-no-title (file-name-base base-name)))
    hierarchy-no-title))

(cl-defmethod org-roam-node-hierarchy (node)
  "Gets node hierarchy by file name"
  (funcall 'dendroam-format-hierarchy (org-roam-node-file node)))

(cl-defmethod org-roam-node-current-file (node)
  (file-name-base (buffer-file-name)))

;; Refactor functions

(defun dendroam-fetch-same-hierarchy-files (hierarchy)
  "Gets all the nodes that share the same HIERARCHY totally or parcially"
  (let ((files
         (mapcar #'car (org-roam-db-query [:select [file]
                                           :from nodes
                                           :where (like file $r1)]
                                          (concat "%" hierarchy "%")))))
    files))

(defun dendroam-refactor-hierarchy (&optional current)
  "Prompts the user to change the hierarchy of the current file
node and updates its hierarchy and the hierarchy of all the nodes
that have it if CURRENT is t the list of updated files is just
the current file"
  (interactive)
  (let*
      ((initial-file
        (file-name-nondirectory (buffer-file-name)))
       (initial-slug
        (file-name-base initial-file))
       (new-slug (file-name-base
                  (read-string "Refactor: " initial-slug)))
       (initial-slug-no-title
        (file-name-base initial-slug))
       (files-to-upd (if current
                         `(,initial-file)
                       (dendroam-fetch-same-hierarchy-files
                        initial-slug-no-title))))

    (dolist (file files-to-upd)
      (let ((new-file
             (replace-regexp-in-string initial-slug-no-title new-slug file)))
        (rename-file file new-file)
        (if (equal buffer-file-name file)
            (progn
              (kill-current-buffer)
              (find-file new-file)))))))

(defun dendroam-refactor-file ()
  (interactive)
  (let* ((initial-file (buffer-file-name))
         (initial-slug (file-name-base initial-file))
         (new-slug (read-string "Refactor: " initial-slug))
         (new-file (concat
                    (expand-file-name new-slug org-roam-directory)
                    ".org")))
    (rename-file initial-file new-file)
    (kill-current-buffer)
    (find-file new-file)))

;; Useful notes functions
(defun dendroam-insert-time-note(&optional goto)
  "Creates a time note in the current level of the hierarchy.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `dendroam-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates dendroam-capture-templates
                     :keys "t"
                     :props (list :default-time (current-time))))

(defun dendroam-insert-scratch-note(&optional goto)
  "Creates a time note in the current level of the hierarchy.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `dendroam-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates dendroam-capture-templates
                     :keys "s"
                     :props (list :default-time (current-time))))

;; Org roam overrides to allow these features
(eval-after-load "org-roam"
  '(cl-defmethod org-roam-node-slug ((node org-roam-node))
     "Override org-roam-node-slug to generate a dendron-like slug.
Handles Unicode by removing non-spacing marks (like accents).
Replaces spaces and most non-alphanumeric chars (except '.') with hyphens,
and cleans up resulting hyphens. Preserves case initially, then downcases.
Example: ' Lang.Elisp.What_Is Ã©? ' -> 'lang.elisp.what-is-e'"
     (let ((title (org-roam-node-title node)))
       ;; --- Helper Functions ---
       (cl-flet* (;; Checks if a character is a non-spacing mark (e.g., accent)
                  (nonspacing-mark-p (char)
                    (eq 'Mn (get-char-code-property char 'general-category)))

                  ;; Removes non-spacing marks from a string using Unicode normalization
                  (strip-nonspacing-marks (s)
                    (if (string-empty-p s)
                        s
                      (ucs-normalize-NFC-string ; Recompose characters
                       (apply #'string
                              (seq-remove #'nonspacing-mark-p ; Remove the separated marks
                                          (ucs-normalize-NFD-string s)))))) ; Decompose char + mark

                  ;; Helper to apply a single regex replacement rule
                  (apply-replacement (current-slug rule)
                    (replace-regexp-in-string (car rule) (cdr rule) current-slug t t)))

         ;; --- Slug Generation Logic ---
         (let* (
                ;; 1. Strip accents and similar marks from the title
                (slug (strip-nonspacing-marks title))

                ;; 2. Define sequence of regex replacements for slug cleanup
                (replacement-rules
                 '(;; Replace most non-alphanumeric chars (EXCEPT '.') with a hyphen.
                   ;; Includes replacing underscores.
                   ("[^[:alnum:][:digit:].]+" . "-")
                   ;; Collapse consecutive hyphens into one.
                   ("--+" . "-")
                   ;; Remove hyphens immediately before or after a dot.
                   ("-\\." . ".")
                   ("\\.-" . ".")
                   ;; Remove leading hyphens. (Use -+ to catch multiples)
                   ("^-+" . "")
                   ;; Remove trailing hyphens. (Use -+ to catch multiples)
                   ("-+$" . "")))

                ;; 3. Apply all replacement rules sequentially
                (slug (-reduce-from #'apply-replacement slug replacement-rules)))

           ;; 4. Convert the final slug to lowercase
           (downcase slug))))))

;; Add hierarchy list to roam buffer
(defun dendroam--get-relevant-unique-hierarchies (node)
  "Query DB for relevant hierarchies based on NODE and return sorted unique list.
Queries files matching the node's top-level hierarchy prefix for efficiency.
If NODE has no hierarchy, fetches all nodes and filters for top-level in Elisp."
  (require 'org-roam-db)
  (let* (;; Determine current hierarchy context
         (current-file (when node (org-roam-node-file node)))
         (current-hierarchy (if current-file (dendroam-format-hierarchy current-file) nil))
         (current-top-level (when current-hierarchy
                              (car (s-split "\\." current-hierarchy)))) ; Get part before first dot
         ;; Variables to hold results
         files-from-db)
    (if current-top-level
        ;; --- Optimized Case: Node is in a hierarchy ---
        (progn
          ;; Query only files matching the top-level prefix (e.g., "project%")
          (setq files-from-db (condition-case nil
                                  (org-roam-db-query [:select [file title id] :from nodes :where (like file $s1)]
                                                                  (concat "%" current-top-level "%"))
                                (error (message "Dendroam: Error querying relevant hierarchies from DB.") nil)))
          ;; Process only the relevant files
;;          (setq relevant-hierarchies (mapcar #'dendroam-format-hierarchy files-from-db))
          )

      ;; --- Fallback Case: Node is root or has no hierarchy ---
      (progn
        ;; I don't think this will ever be used?
        ;; Query all files (less efficient, but only for this case)
        (setq files-from-db (condition-case nil
                                (mapcar #'car (org-roam-db-query [:select [file] :from nodes]))
                              (error (message "Dendroam: Error querying all hierarchies from DB.") nil)))
        ;; Process all filenames but filter for top-level only (no dots)
        (setq relevant-hierarchies
              (seq-filter (lambda (h) (and h (not (s-contains? "." h))))
                          (mapcar #'dendroam-format-hierarchy files-from-db)))))
    files-from-db))

;; Helper function for linking (keep this from previous step)
(defun dendroam--get-hierarchy-link (hierarchy)
  "Generate an Org link for HIERARCHY. Links to exact node match or searches."
  (org-link-make-string (car hierarchy) (nth 1 hierarchy)))

(defun my-hierarchical-sort-comparator (node1 node2)
  "Comparator function for sorting dot-separated hierarchy strings.
Returns t if STR1 should come before STR2, nil otherwise."
  (let ((parts1 (split-string (replace-regexp-in-string "\\.org\\'" "" (car node1)) "\\."))
        (parts2 (split-string (replace-regexp-in-string "\\.org\\'" "" (car node2)) "\\.")))
    (catch 'done
      (let ((len1 (length parts1))
            (len2 (length parts2)))
        (dotimes (i (min len1 len2))
          (let ((part1 (nth i parts1))
                (part2 (nth i parts2)))
            (let ((cmp (string< part1 part2)))
              (when (not (eq cmp ())) ; If not equal
                (throw 'done cmp)))))
        ;; If we've compared all common parts, the shorter one comes first
        (< len1 len2)))))

(defun sort-hierarchical-nodes (node-list)
  "Sorts a list of dot-separated hierarchy strings hierarchically."
  (sort node-list #'my-hierarchical-sort-comparator))

;; Updated section function using the optimized hierarchy getter
(defun dendroam-insert-hierarchy-section (node)
  "Insert the Dendroam hierarchies section using optimized DB query."
  ;; Get only relevant hierarchies using the optimized function
  (let* ((relevant-hierarchies (dendroam--get-relevant-unique-hierarchies node))
         ;; Determine heading based on context
         (current-file (when node (org-roam-node-file node)))
         (current-hierarchy (if current-file (dendroam-format-hierarchy current-file) nil))
         (current-top-level (when current-hierarchy
                              (car (s-split "\\." current-hierarchy))))
         (heading (if current-top-level
                      (format "Hierarchies (%s)" current-top-level)
                    "Hierarchies (Top Level)")))

    (when relevant-hierarchies
      (setq sorted-hierarchies (sort-hierarchical-nodes relevant-hierarchies))
      (let ((inhibit-read-only t))
        (insert (format "* %s\n\n" heading))
        (dolist (hierarchy sorted-hierarchies)
          (let* ((level (length (s-split "\\." (car hierarchy))))
                 (indent-level (max 0 (- level 1)))
                 (indent-string (s-repeat (* indent-level 2) " ")))
            (insert indent-string "- ")
            (insert-button
             (nth 1 hierarchy)
             'action (lambda (_btn) (interactive) (org-roam-node-visit (org-roam-node-from-id (nth 2 hierarchy))))
;;             'help-echo (format "Find notes under the '%s' hierarchy" (hierarchy)
             'face 'link)
            (insert "\n")))
        (insert "\n")))))

;; --- Add Section to Org-roam Buffer using the correct variable ---
(with-eval-after-load 'org-roam-mode ;; The customization var is in org-roam-mode.el
  (add-to-list 'org-roam-mode-sections #'dendroam-insert-hierarchy-section))
;;; dendroam.el ends here
