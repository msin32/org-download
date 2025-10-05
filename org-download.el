;;; org-download.el --- Image drag-and-drop for Org-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Free Software Foundation, Inc.

;; Author: Oleh Krehel
;; Maintainer: (your name)
;; Version: 0.2.2
;; Package-Requires: ((emacs "24.3") (async "1.2"))
;; Keywords: multimedia, images, screenshots, download

;;; Commentary:
;;
;; Drag-and-drop images (single or multiple) into an Org buffer.
;; No automatic preview – use Org’s own `C-c C-x C-v’ if/when you want it.
;;

;;; Code:
(require 'cl-lib)
(require 'async)
(require 'url-parse)
(require 'url-http)
(require 'org)
(require 'org-attach)
(require 'org-element)


;;; User options
(defgroup org-download nil
  "Image drag-and-drop for Org mode."
  :group 'org
  :prefix "org-download-")

(defcustom org-download-method 'directory
  "How images should be stored."
  :type '(choice
          (const :tag "Directory" directory)
          (const :tag "Attachment" attach)
          (function :tag "Custom function")))

(defcustom org-download-image-dir nil
  "Directory for images (nil = current directory)."
  :type '(choice (const :tag "Default" nil) (string :tag "Directory")))
(make-variable-buffer-local 'org-download-image-dir)

(defcustom org-download-heading-lvl 0
  "Heading level used for sub-directories."
  :type '(choice integer (const :tag "None" nil)))
(make-variable-buffer-local 'org-download-heading-lvl)

(defcustom org-download-annotate-p t
  "When nil, never insert the `#+DOWNLOADED: …' line."
  :type 'boolean
  :group 'org-download)

(defcustom org-download-backend t
  "Download backend."
  :type '(choice
          (const :tag "wget" "wget \"%s\" -O \"%s\"")
          (const :tag "curl" "curl \"%s\" -o \"%s\"")
          (const :tag "url-retrieve" t)))

(defcustom org-download-timestamp "%Y-%m-%d_%H-%M-%S_"
  "Format-time-string appended to file name (set to \"\" to disable)."
  :type 'string)

(defcustom org-download-screenshot-method "gnome-screenshot -a -f %s"
  "Screenshot utility."
  :type '(choice
          (const "gnome-screenshot -a -f %s")
          (const "scrot -s %s")
          (const "flameshot gui --raw > %s")
          (const "gm import %s")
          (const "magick import %s")
          (const "screencapture -i %s")
          (const "spectacle -br -o %s")
          (const "grim -g \"$(slurp)\" %s")
          (function :tag "Custom function")))

(defcustom org-download-screenshot-basename "screenshot.png"
  "Default basename for screenshots."
  :type 'string)

(defcustom org-download-screenshot-file
  (expand-file-name org-download-screenshot-basename temporary-file-directory)
  "Temporary file used for screenshots."
  :type 'string)

(defcustom org-download-image-html-width 0
  "When non-zero add #+attr_html: :width tag."
  :type 'integer)

(defcustom org-download-image-latex-width 0
  "When non-zero add #+attr_latex: :width tag."
  :type 'integer)

(defcustom org-download-image-org-width 0
  "When non-zero add #+attr_org: :width tag."
  :type 'integer)

(defcustom org-download-image-attr-list nil
  "Extra attribute lines to insert."
  :type '(repeat string))

(defcustom org-download-delete-image-after-download nil
  "Non-nil means delete local file after copying it."
  :type 'boolean)


;;; Internal variables
(defvar org-download-path-last-file nil
  "Full path of the last downloaded file.")

(defvar org-download--file-content nil
  "When non-nil, an already-downloaded file to use.")


;;; Utilities
(defun org-download-org-mode-p ()
  "Return non-nil if we are in an Org buffer."
  (derived-mode-p 'org-mode))

(defun org-download-get-heading (lvl)
  "Return heading text of the parent at level LVL."
  (save-excursion
    (let ((cur (org-current-level)))
      (when cur
        (unless (= cur (1+ lvl)) (org-up-heading-all (- cur lvl 1)))
        (let ((txt (nth 4 (org-heading-components))))
          (if txt (replace-regexp-in-string " " "_" txt) ""))))))

(defun org-download--dir-1 ()
  (or org-download-image-dir "."))

(defun org-download--dir-2 ()
  (when org-download-heading-lvl (org-download-get-heading org-download-heading-lvl)))

(defun org-download--dir ()
  "Return directory where images should be stored, creating it if needed."
  (if (org-download-org-mode-p)
      (let* ((p1 (org-download--dir-1))
             (p2 (org-download--dir-2))
             (dir (if p2 (expand-file-name p2 p1) p1)))
        (unless (file-exists-p dir) (make-directory dir t))
        dir)
    default-directory))


;;; File naming
(defvar org-download-file-format-function #'org-download-file-format-default)

(defun org-download-file-format-default (filename)
  (concat (format-time-string org-download-timestamp) filename))

(defun org-download--fullname (link &optional ext)
  "Return target file name for LINK."
  (let* ((base (replace-regexp-in-string
                "%20" " "
                (file-name-nondirectory
                 (car (url-path-and-query (url-generic-parse-url link))))))
         (dir  (org-download--dir)))
    (when ext (setq base (concat (file-name-sans-extension base) "." ext)))
    (abbreviate-file-name
     (expand-file-name (funcall org-download-file-format-function base) dir))))


;;; Download / copy
(defun org-download--image (link filename)
  "Save LINK to FILENAME."
  (when (string= "file" (url-type (url-generic-parse-url link)))
    (setq link (url-unhex-string (url-filename (url-generic-parse-url link)))))
  (cond
   ((and (not (file-remote-p link)) (file-exists-p link))
    (copy-file link (expand-file-name filename)))
   (org-download--file-content
    (copy-file org-download--file-content (expand-file-name filename))
    (setq org-download--file-content nil))
   ((eq org-download-backend t)
    (org-download--image/url-retrieve link filename))
   (t
    (org-download--image/command org-download-backend link filename))))

(defun org-download--image/command (cmd link filename)
  (async-start
   `(lambda () (shell-command ,(format cmd link (expand-file-name filename))))
   (lambda (_) nil)))                 ; no preview

(defun org-download--image/url-retrieve (link filename)
  (url-retrieve
   link
   (lambda (status filename _buffer)
     (org-download--write-image status filename))
   (list (expand-file-name filename) (current-buffer))
   nil t))

(defun org-download--write-image (status filename)
  (let ((err (plist-get status :error)))
    (when err (error "HTTP error %s" (downcase (nth 2 (assq (nth 2 err) url-http-codes))))))
  (delete-region (point-min) (progn (re-search-forward "\n\n" nil 'move) (point)))
  (let ((coding-system-for-write 'no-conversion))
    (write-region nil nil filename nil nil nil 'confirm)))


;;; Insertion
(defvar org-download-link-format "[[file:%s]]\n")

(defun org-download-link-format-function-default (filename)
  (if (and (>= (string-to-number org-version) 9.3)
           (eq org-download-method 'attach))
      (format "[[attachment:%s]]\n"
              (org-link-escape (file-relative-name filename (org-attach-dir))))
    (format org-download-link-format
            (org-link-escape (abbreviate-file-name filename)))))

(defcustom org-download-link-format-function
  #'org-download-link-format-function-default
  "Function that turns a file name into an Org link."
  :type 'function)

(defun org-download-annotate-default (link)
  (if org-download-annotate-p
      (format "#+DOWNLOADED: %s @ %s\n"
              (if (equal link org-download-screenshot-file) "screenshot" link)
              (format-time-string "%Y-%m-%d %H:%M:%S"))
    ""))

(defvar org-download-annotate-function #'org-download-annotate-default)

(defun org-download-insert-link (link filename)
  "Insert link (and annotation) for FILENAME at point."
  (let* ((beg      (point))
         (line-beg (line-beginning-position))
         (indent   (- beg line-beg))
         (in-item  (org-in-item-p))
         str)
    (if (looking-back "^[ \t]+" line-beg)
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (insert (funcall org-download-annotate-function link))
    (dolist (attr org-download-image-attr-list) (insert attr "\n"))
    (when (> org-download-image-html-width 0)
      (insert (format "#+attr_html: :width %dpx\n" org-download-image-html-width)))
    (when (> org-download-image-latex-width 0)
      (insert (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width)))
    (when (> org-download-image-org-width 0)
      (insert (format "#+attr_org: :width %dpx\n" org-download-image-org-width)))
    (insert (funcall org-download-link-format-function filename))
    (setq str (buffer-substring-no-properties line-beg (point)))
    (when in-item (indent-region line-beg (point) indent))
    (goto-char beg)                     ; stay where dropped
    str))


;;; High-level entry points
(defun org-download-image (link)
  "Download image at LINK and insert link at point."
  (interactive "sURL: ")
  (let* ((link-and-ext (org-download--parse-link link))
         (filename
          (cond
           ((and (org-download-org-mode-p) (eq org-download-method 'attach))
            (let ((org-download-image-dir (org-attach-dir t))
                  org-download-heading-lvl)
              (apply #'org-download--fullname link-and-ext)))
           ((fboundp org-download-method)
            (funcall org-download-method link))
           (t
            (apply #'org-download--fullname link-and-ext)))))
    (setq org-download-path-last-file filename)
    (org-download--image link filename)
    (when (org-download-org-mode-p)
      (when (eq org-download-method 'attach)
        (org-attach-attach filename nil 'none))
      (org-download-insert-link link filename))
    (when (and org-download-delete-image-after-download
               (not (file-remote-p link)))
      (delete-file link delete-by-moving-to-trash))))

(defun org-download-yank ()
  "Download image from `kill-ring'."
  (interactive)
  (let ((k (current-kill 0)))
    (unless (url-type (url-generic-parse-url k))
      (user-error "Not a URL: %s" k))
    (org-download-image (replace-regexp-in-string "\n+$" "" k))))

(defun org-download-screenshot (&optional basename)
  "Capture screenshot and insert it."
  (interactive)
  (let* ((dir  (file-name-directory org-download-screenshot-file))
         (file (if basename (concat dir basename) org-download-screenshot-file)))
    (make-directory dir t)
    (if (functionp org-download-screenshot-method)
        (funcall org-download-screenshot-method file)
      (shell-command-to-string (format org-download-screenshot-method file)))
    (when (file-exists-p file)
      (org-download-image (concat "file:" file))
      (delete-file file))))


;;; Drag-and-drop (single + multiple files)
(defun org-download-dnd-fallback (uri action)
  "Let default dnd mechanism handle URI/ACTION."
  (let ((dnd-protocol-alist
         (rassq-delete-all 'org-download-dnd (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

(defun org-download-dnd (uri action)
  "Drag-and-drop handler for Org buffers."
  (cond
   ;; multiple files (modern file-managers send a list)
   ((and (listp uri) (cl-every #'stringp uri))
    (if (org-download-org-mode-p)
        (dolist (f uri)
          (condition-case nil
              (org-download-image (if (string-prefix-p "file:" f)
                                      f
                                    (concat "file:" f)))
            (error nil)))
      (org-download-dnd-fallback uri action)))
   ;; single URI
   ((org-download-org-mode-p)
    (condition-case nil
        (org-download-image uri)
      (error (org-download-dnd-fallback uri action))))
   ((eq major-mode 'dired-mode)
    (org-download-dired uri))
   (t
    (org-download-dnd-fallback uri action))))

(defun org-download-dired (uri)
  "Download URI to `default-directory'."
  (raise-frame)
  (org-download-image uri))


;;; Base64 drop (browser → Emacs)
(defun org-download-dnd-base64 (uri _action)
  (when (and (org-download-org-mode-p)
             (string-match "^data:image/\\(png\\|jpg\\|jpeg\\);base64," uri))
    (let* ((ext (match-string 1 uri))
           (data-start (match-end 0))
           (fname (org-download--fullname (substring-no-properties uri data-start (+ data-start 10)) ext)))
      (with-temp-buffer
        (insert (base64-decode-string (substring uri data-start)))
        (write-file fname))
      (org-download-insert-link fname fname))))


;;; Parse link / detect type
(defun org-download--parse-link (link)
  (cond ((image-type-from-file-name link) (list link nil))
        ((string-match "^file:/+" link) (list link nil))
        (t
         (let ((buffer (url-retrieve-synchronously link t)))
           (org-download--detect-ext link buffer)))))

(defun org-download--detect-ext (link buffer)
  (with-current-buffer buffer
    (let (ext)
      (cond
       ((let ((regexes org-download-img-regex-list) lnk)
          (while (and (not lnk) regexes)
            (goto-char (point-min))
            (when (re-search-forward (pop regexes) nil t)
              (backward-char)
              (setq lnk (read (current-buffer)))))
          (when lnk (setq link lnk))))
       ((progn (goto-char (point-min))
               (when (re-search-forward "^Content-Type: image/\\(.*\\)$" nil t)
                 (setq ext (match-string 1)))))
       ((progn (goto-char (point-min))
               (when (re-search-forward "^Content-Type: application/pdf" nil t)
                 (setq ext "pdf"))
               (re-search-forward "^%PDF")
               (beginning-of-line)
               (write-region (point) (point-max)
                             (setq org-download--file-content "/tmp/org-download.pdf"))
               t))
       (t (error "Link %s does not point to an image; unaliasing failed" link)))
      (list link ext))))

(defvar org-download-img-regex-list
  '("<img +src=\"" "<img +\\(class=\"[^\"]+\"\\)? *src=\"")
  "Regexes to extract real image URL from HTML wrapper.")


;;; Enable / disable
;;;###autoload
(defun org-download-enable ()
  "Enable org-download."
  (unless (assoc "^\\(https?\\|ftp\\|file\\|nfs\\):" dnd-protocol-alist)
    (setq dnd-protocol-alist
          `(("^\\(https?\\|ftp\\|file\\|nfs\\):" . org-download-dnd)
            ("^data:" . org-download-dnd-base64)
            ,@dnd-protocol-alist))))

(defun org-download-disable ()
  "Disable org-download."
  (setq dnd-protocol-alist
        (rassq-delete-all 'org-download-dnd dnd-protocol-alist)))

(org-download-enable)

(provide 'org-download)

;;; org-download.el ends here
