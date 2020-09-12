;;; backlinks.el --- Minor mode to display backlinks in new buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  Brandon Webster <branweb1@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Finds backlinks to an org file in a flat notes directory.
;; If you are at file A, files B and C are backlinks if they link to A.
;; Works by creating a graph of backlinks and writing it to a text file.
;; File loaded into memory on first request for backlinks, and subsequent
;; requests read from memory.  If notes have TITLE keyword, it's value is
;; dispayed in the backlinks buffer.  Otherwise it falls back to the file
;; name.

;;; Code:
(require 'seq)

;; TODO auto-create nonexisting links
;; TOOD add text preview to backlinks buffer
;; variables
(defgroup backlinks nil
  "Finder of backlinks for org files"
  :group 'org)

(defcustom backlinks-notes-directory (concat (getenv "HOME") "/notes")
  "The notes directory, a flat directory of org files."
  :type 'string
  :group 'backlinks)
(defcustom backlinks-data-file (concat (getenv "HOME") "/.emacs.d/backlinks.txt")
  "The location of the text file where the backlinks graph is saved."
  :type 'string
  :group 'backlinks)
(defvar backlinks-graph nil)
(defvar backlinks-titles nil)

;; filesystem
(defun backlinks--dotfile-p (filename)
  "Predicate to determine if FILENAME is a dotfile."
  (string= "." (substring filename 0 1)))

(defun backlinks--orgfile-p (filename)
  "Predicate to determine if FILENAME is an org file."
  (string= "org" (downcase (file-name-extension filename))))

(defun backlinks--has-extension-p (str)
  "Predicate to determine if STR ends in a file extension."
  (file-name-extension str))

(defun backlinks--ensure-trailing-slash (dirname)
  "Add trailing / to DIRNAME if it is not there."
  (concat
   dirname
   (when (not (string= "/" (substring dirname -1))) "/")))

(defun backlinks--relpath->abspath (filename)
  "Convert FILENAME to an absolute path."
  (concat (backlinks--ensure-trailing-slash backlinks-notes-directory) filename))

(defun backlinks--get-orgfiles ()
  "Retrieve all org files in notes directory."
  (seq-filter
   (lambda (filename)
     (and (not (backlinks--dotfile-p filename))
          (backlinks--orgfile-p filename)))
   (directory-files backlinks-notes-directory nil)))

;; parsing
(defun backlinks--extract-title-from-ast (ast &optional fallback)
  "Extract title keyword from AST if there.  Otherwise return FALLBACK."
  (or (org-element-map  ast 'keyword
        (lambda (kw)
          (when (string= "TITLE" (org-element-property :key kw))
            (org-element-property :value kw))) nil t)
      fallback))

(defun backlinks--extract-org-links-from-ast (ast)
  "Extract all links to org files from AST."
  (seq-filter
   (lambda (link)
     (and (backlinks--has-extension-p link)
          (backlinks--orgfile-p link)))
   (backlinks--extract-links-from-ast ast)))

(defun backlinks--extract-links-from-ast (ast)
  "Extract all links from AST."
  (org-element-map ast 'link
    (lambda (link) (org-element-property :path link))))

(defun backlinks--file-filter-p (filename)
  "Return t if FILENAME ends in org and is not a dotfile."
  (and (backlinks--orgfile-p filename) (not (backlinks--dotfile-p filename))))

(defun backlinks--get-open-buffers ()
  "Get a list of currently open org-buffers."
  (mapcar 'file-name-nondirectory
          (seq-filter
           (lambda (fp)
             (and fp (backlinks--file-filter-p fp)))
           (mapcar 'buffer-file-name (buffer-list)))))

(defun backlinks--create-backlinks-graph ()
  "Generate backlinks graph and file title hash."
  (let ((backlinks-ht (make-hash-table :test 'equal))
        (title-ht (make-hash-table :test 'equal))
        (open-buffer-filepaths (backlinks--get-open-buffers)))
    (dolist (filename (backlinks--get-orgfiles))
      (with-current-buffer
          (find-file-noselect (backlinks--relpath->abspath filename))
        (let ((ast (org-element-parse-buffer)))
          (dolist (link (backlinks--extract-org-links-from-ast ast))
            (puthash
             link
             (cons filename (gethash link backlinks-ht))
             backlinks-ht))
          (puthash
           filename
           (backlinks--extract-title-from-ast ast filename)
           title-ht))
        ;; don't kill if it was already open when we ran this
        (when (not (member filename open-buffer-filepaths))
          (kill-buffer))))
    (values backlinks-ht title-ht)))

;; I/O
(defun backlinks--write-to-file (filename data)
  "Write DATA to FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun backlinks--read-from-file (filename)
  "Read contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
   (read (current-buffer))))

(defun backlinks--valid-file-p (filename)
  "Predicate to check that FILENAME exists and is not empty."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (> 0 (buffer-size (current-buffer))))))

;; TODO remove the messages here
(defun backlinks--load-backlinks-data ()
  "Try loading backlinks data from memory, then from file."
  (cond ((and backlinks-graph backlinks-titles)
         (message "loading backlinks data from memory")
         (values backlinks-graph backlinks-titles))
        ((backlinks--valid-file-p backlinks-data-file)
         (let ((hashes (backlinks--read-from-file backlinks-data-file)))
           (setq backlinks-graph (car hashes))
           (setq backlinks-titles (cadr hashes))
           (message "loading backlinks data from file")
           hashes))
        (t (message "generating backlinks data from filesystem")
           (backlinks--refresh-backlinks)
           (values backlinks-graph backlinks-titles))))

;; rendering
(defun backlinks--render-backlink (backlink)
  "Take cons-cell BACKLINK and render it as a clickable link."
  (insert-button
   (car backlink)
   'action (lambda (_)
             (let ((buff (current-buffer)))
               (find-file (cdr backlink))
               (kill-buffer buff)))))

;; API
;;;###autoload
(defun backlinks--refresh-backlinks ()
  "Generate backlinks data structures by crawling nodes dir."
  (interactive)
  (let ((graphs (backlinks--create-backlinks-graph)))
    (setq backlinks-graph (car graphs))
    (setq backlinks-titles (cadr graphs))
    (backlinks--write-to-file backlinks-data-file graphs)
    (message "Backlinks refreshed.")))

;;;###autoload
(defun backlinks--show-backlinks ()
  "Display backlinks."
  (interactive)
  (let* ((hashes (backlinks--load-backlinks-data))
         (backlinks (car hashes))
         (titles (cadr hashes))
         (bl-for-buffer
          (gethash (buffer-name) backlinks))
         (links
          (mapcar (lambda (blfb) (cons (gethash blfb titles) blfb))
                  bl-for-buffer))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "Backlinks")
      (erase-buffer)
      (help-mode)
      (insert "BACKLINKS\n")
      (insert (make-string 50 ?\=))
      (insert "\n")
      (dolist (bl links)
        (backlinks--render-backlink bl)
        (insert "\n\n"))
      (switch-to-buffer (current-buffer)))))


;;;###autoload
(define-minor-mode backlinks-mode
  "Finder of backlinks"
  :lighter " BL"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c b l") 'backlinks--show-backlinks)
            (define-key map (kbd "C-c b r") 'backlinks--refresh-backlinks)
            map))

;;;###autoload
(add-hook 'org-mode 'backlinks-mode)

(provide 'backlinks)
;;; backlinks.el ends here
