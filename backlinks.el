;;; backlinks.el --- Minor mode to display backlinks to current buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <branweb1@gmail.com>
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

;;

;;; Code:
(require 'seq)

;; TODO auto-create nonexisting links
;; TODO find emacs dir for backlinks-data-file
;; TODO find empty files
;; TOOD add text preview to backlinks buffer
;; variables
(defcustom backlinks-notes-directory (concat (getenv "HOME") "/notes")
  "The notes directory, a flat directory of org files."
  :type 'string)
(defcustom backlinks-data-file (concat (getenv "HOME") "/.emacs.d/backlinks.txt")
  "The location of the text file where the backlinks graph is saved."
  :type 'string)
(defvar backlinks-graph nil)
(defvar backlinks-titles nil)

;; filesystem
(defun backlinks--dotfile-p (fname)
  "Predicate to determine if FNAME is a dotfile."
  (string= "." (substring fname 0 1)))

(defun backlinks--orgfile-p (fname)
  "Predicate to determine if FNAME is an org file."
  (string= "org" (downcase (file-name-extension fname))))

(defun backlinks--ensure-trailing-slash (dirname)
  "Add trailing / to DIRNAME if it is not there."
  (concat
   dirname
   (when (not (string= "/" (substring dirname -1))) "/")))

(defun backlinks--relpath->abspath (fname)
  "Convert FNAME to an absolute path."
  (concat (backlinks--ensure-trailing-slash backlinks-notes-directory) fname))

(defun backlinks--get-orgfiles ()
  "Retrieve all org files in notes directory."
  (seq-filter
   (lambda (fname)
     (and (not (backlinks--dotfile-p fname))
          (backlinks--orgfile-p fname)))
   (directory-files backlinks-notes-directory nil)))

;; parsing
(defun backlinks--extract-title-from-ast (ast &optional fallback)
  "Extract title keyword from AST if there.  Otherwise return FALLBACK."
  (or (org-element-map  ast 'keyword
        (lambda (kw)
          (when (string= "TITLE" (org-element-property :key kw))
            (org-element-property :value kw))) nil t)
      fallback))

;; TODO filter out non-org links?
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
    (dolist (fname (backlinks--get-orgfiles))
      (with-current-buffer
          (find-file-noselect (backlinks--relpath->abspath fname))
        (let ((ast (org-element-parse-buffer)))
          (dolist (link (backlinks--extract-links-from-ast ast))
            (puthash
             link
             (cons fname (gethash link backlinks-ht))
             backlinks-ht))
          (puthash
           fname
           (backlinks--extract-title-from-ast ast fname)
           title-ht))
        ;; don't kill if it was already open when we ran this
        (when (not (member fname open-buffer-filepaths))
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
    (backlinks--write-to-file backlinks-data-file graphs)))

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
