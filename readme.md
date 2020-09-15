

# Backlinks

Finds backlinks to an org file in a flat notes directory.

If you are at file A, files B and C are backlinks if they link to A.

Works by creating a graph of backlinks and writing it to a text file.
File loaded into memory on first request for backlinks, and subsequent
requests read from memory.  If notes have TITLE keyword, it's value is
dispayed in the backlinks buffer.  Otherwise it falls back to the file


## Setup

    (add-to-list 'load-path "path/to/backlinks")
    (require 'backlinks)
    (setq backlinks-notes-directory "path/to/notes_directory")
    (define-key backlinks-key-map (kbd "C-c b") backlinks-command-map)

**Note**: The keymap prefix of `C-c b` is just a suggestion.


## Usage

When viewing an org file (assuming you are using the default keymap prefix of `C-c b`:

-   **`C-c b r`:** generate backlinks for all files
-   **`C-c b l`:** view generated backlinks for file

