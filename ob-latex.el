;;; ob-latex.el --- Babel Functions for LaTeX        -*- lexical-binding: t; -*-

;;; Rewrite for something I can use in a modern LaTeX setup.
;;; stripped out for now...

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-macs)

(declare-function org-create-formula-image "org" (string tofile options buffer &optional type))
(declare-function org-latex-compile "ox-latex" (texfile &optional snippet))
(declare-function org-latex-guess-inputenc "ox-latex" (header))
(declare-function org-splice-latex-header "org" (tpl def-pkg pkg snippets-p &optional extra))
(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-next-visible-heading "org" (arg))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("latex" . "tex"))

(defvar org-format-latex-header)	  ; From org.el
(defvar org-format-latex-options)	  ; From org.el
(defvar org-latex-default-packages-alist) ; From org.el
(defvar org-latex-packages-alist)	  ; From org.el
(defvar org-preview-latex-process-alist)  ; From org.el

(defvar org-babel-default-header-args:latex
  '((:results . "latex") (:exports . "results"))
  "Default arguments to use when evaluating a LaTeX source block.")

(defconst org-babel-header-args:latex
  '((border	  . :any)
    (fit          . :any)
    (imagemagick  . ((nil t)))
    (iminoptions  . :any)
    (imoutoptions . :any)
    (packages     . :any)
    (pdfheight    . :any)
    (pdfpng       . :any)
    (pdfwidth     . :any)
    (headers      . :any)
    (buffer       . ((yes no))))
  "LaTeX-specific header arguments.")


(defcustom org-babel-latex-preamble
  (lambda (_)
    "\\documentclass[preview]{standalone}
\\def\\pgfsysdriver{pgfsys-tex4ht.def}
")
  "Closure which evaluates at runtime to the LaTeX preamble.

It takes 1 argument which is the parameters of the source block."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-latex-begin-env
  (lambda (_)
    "\\begin{document}")
  "Function that evaluates to the begin part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal LaTeX source."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-latex-end-env
  (lambda (_)
    "\\end{document}")
  "Closure which evaluates at runtime to the end part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal LaTeX source."
  :group 'org-babel
  :type 'function)


(defcustom org-babel-latex-process-alist
  `((svg :programs ("latexmk" "dvisvgm")
         :descriptiom "dvi > svg"
         :message "you need to install latexmk and dvisvgm!"
         :image-input-type "dvi"
         :image-output-type "svg"
         :image-size-adjust (1.0 . 1.0)
         :latex-compiler
         ("latexmk -f -latex -interaction=nonstopmode -output-directory=%o %f")
         :image-converter
         ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")))
  
  "Definitions of external processes for LaTeX result generation.
See `org-preview-latex-process-alist' for more details.

The following process symbols are recognized:
- `svg' :: Process used to produce .svg output."
  :group 'org-babel
  :package-version '(Org . "9.8")
  :type '(alist :tag "LaTeX to image backends"
		:value-type (plist)))


(defun org-babel-expand-body:latex (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapc (lambda (pair) ;; replace variables
          (setq body
                (replace-regexp-in-string
                 (regexp-quote (format "%S" (car pair)))
                 (if (stringp (cdr pair))
                     (cdr pair) (format "%S" (cdr pair)))
                 body t t)))
	(org-babel--get-vars params))
  (let ((prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (org-trim
     (concat
      (and prologue (concat prologue "\n"))
      body
      (and epilogue (concat "\n" epilogue "\n"))))))

(defun headers-to-lines (headers)
  "Headers seem to be anything so make sure we can process them."
  (cond
   ((stringp headers)
    (mapconcat
     #'identity
     (string-split headers nil t (rx (one-or-more whitespace)))
     "\n"))
   ((listp headers)
    (mapconcat #'identity headers "\n"))
   (t "")))
        
;; the MVP
(defun org-babel-execute:latex (body params)
  "Execute LaTeX BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (setq body (org-babel-expand-body:latex body params))
  (if (cdr (assq :file params))
      (let* ((out-file (cdr (assq :file params)))
	     (extension (file-name-extension out-file))
	     (headers (cdr (assq :headers params)))
             (in-buffer (not (string= "no" (cdr (assq :buffer params)))))
	     (org-latex-packages-alist
	      (append (cdr (assq :packages params)) org-latex-packages-alist)))
        (cond
         ((string= "svg" extension)
          ;; overide by lisp dynamic bindings of the header and processing list
          ;; as required and call the image maker
          (let ((org-format-latex-header
                 (concat
                  org-format-latex-header "\n"
                  (headers-to-lines headers)))
                ;; use our version of the process-alist I guess we
                ;; can then customize it above.
                (org-preview-latex-process-alist org-babel-latex-process-alist))
            (org-create-formula-image
             body out-file org-format-latex-options in-buffer 'svg)))
         ;;
         (t (message "only SVG here!")))
        nil) ;; signal that output has already been written to file
    body))


(provide 'ob-latex)

;;; ob-latex.el ends here
