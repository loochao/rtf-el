;;; rtf.el --- Emacs reader/writer for RTF files.

;; Copyright (C) 2010 Eduard Wiebe <pusto@web.de>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'format)
(require 'table)
(require 'hex-util)

(eval-when-compile
  (require 'cl))

(defgroup rtf nil
  "Read and save files in text/rtf format."
  :group  'wp
  :prefix 'rtf-)

;;;  Options
(defcustom rtf-invert-foreground-color t
  "Invert foreground color if it the same as background."
  :type  'boolean
  :group 'rtf)

;;;  Version
(defconst rtf-version "0.1"
  "The version number of the rtf.el package.")

(defun rtf-version ()
  (interactive)
  (message "rtf.el version %s" rtf-version))

;;;  Utils
(defconst rtf-mm-per-twips
  (/ 25.4 (* 20 72)))

(defconst rtf-pixel-per-mm
  (let ((mm-width (display-mm-width)))
    (and mm-width (/ (display-pixel-width) mm-width))))

(defun rtf-twips-to-width (twips)
  (if rtf-pixel-per-mm
      (round (/ (* rtf-pixel-per-mm (* twips rtf-mm-per-twips))
		(frame-char-width)))
    (round (/ twips display-pixels-per-inch 20))))

(defun rtf-twips-to-height (twips)
  (if rtf-pixel-per-mm
      (round (/ (* rtf-pixel-per-mm (* twips rtf-mm-per-twips))
		(frame-char-height)))
    (round (/ twips display-pixels-per-inch 20))))


(defsubst rtf-make-properties-stack ()
  "Create stack of text properties. Initialy the are no
properties and fresh mark is false."
  ;; 0. fresh mark,  1. list of current text properties
  (vector nil (list)))

(defsubst rtf-fresh-properties-p (stack)
  "Return TRUE if there are fresh text properties on the top of the STACK."
  (aref stack 0))

(defun rtf-pop-properties (stack)
  "Pop current properties from STACK and mark restored properties as fresh."
  (aset stack 0 t)
  (aset stack 1 (cdr (aref stack 1))))

(defun rtf-push-properties (stack)
  "Push current text properties on STACK."
  ;; There is no need to mark current stack state as fresh, because we
  ;; only duplicate text properties on top of stack.
  (let ((props (aref stack 1)))
    (aset stack 1 (cons (copy-tree (car props)) props))))

(defun rtf-reset-properties (stack)
  "Clear STACK and mark the current state as fresh."
  (aset stack 0 t)
  (setcar (aref stack 1) nil))

(defun rtf-set-property (stack prop &rest values)
  "Set PROP to VALUES on the top of the STACK and mark them as fresh."
  (aset stack 0 t)
  (let* ((props (aref stack 1))
	 (entry (assq prop (car props))))
    (if entry
	(setcdr entry values)
      (push (cons prop values) (car props)))))

(defun rtf-get-property (stack prop)
  (assq prop (car (aref stack 1))))

(defun rtf-toggle-property (stack flag prop onval offval)
  (rtf-set-property stack prop (if (= flag 0) offval onval)))

(defun rtf-apply-properties (stack fun &optional len)
  "Apply the text properties from top of STACK and remove the fresh mark."
  (or len (setq len 1))
  (funcall fun (car (aref stack 1)) len)
  (aset stack 0 nil))


;;;  Control Table
(defvar rtf-control-table nil)

(defmacro rtf-with-group (&rest body)
  (declare (indent 0))
  `(while (not (eq (following-char) ?}))
     ,@body))

(defmacro rtf-define-special (name &rest body)
  (declare (indent 1) (debug t))
  (and (stringp name) (setq name (intern name)))
  `(push '(,name nil
		 (lambda (name param)
		   ,@body)
		 special)
	 rtf-control-table))

(defmacro rtf-define-control (name defpar &rest body)
  (declare (indent 2) (debug t))
  `(push '(,name ,defpar
		 (lambda (name param)
		   ,@body))
	 rtf-control-table))

(defmacro rtf-defvar (var &optional init-value permanent doc)
  (declare (indent 3) (debug t))
  `(progn
     (defvar ,var ,init-value ,doc)
     (make-variable-buffer-local ',var)
     (put ',var 'permanent-local ,permanent)))

(defmacro rtf-define-controls (namelst defpar &rest body)
  (declare (indent 2) (debug t))
  (let ((name  (car namelst))
	(rest  (cdr namelst)))
    (if (not (null name))
	`(progn
	   (rtf-define-control  ,name ,defpar ,@body)
	   (rtf-define-controls ,rest ,defpar ,@body)))))

(defmacro rtf-define-destination (name &rest body)
  (declare (indent 1) (debug t))
  `(push '(,name nil
		 (lambda (name param)
		   (let ((rtf-inside-destination t))
		     ,@body)))
	 rtf-control-table))

(defun rtf-inside-destination-p ()
  (and (boundp 'rtf-inside-destination) rtf-inside-destination))

(defmacro rtf-write-as-group (&rest body)
  (declare (indent 0))
  `(progn (insert ?{) ,@body (insert ?})))

;;;  Document Properties
(unless (coding-system-p 'symbol)
  (define-coding-system 'symbol
    "Coding system for the symbol charset."
    :mnemonic ??
    :coding-type 'charset
    :charset-list '(symbol)))

(defconst rtf-font-charset-alist
  '((0   . cp1252)
    (2   . symbol)
    (77  . mac-roman)
    (128 . cp932)
    (129 . cp949)
    (134 . gb2312)
    (135 . cp950)
    (136 . big5)
    (161 . cp1253)
    (162 . cp1254)
    (254 . cp437)))

(defconst rtf-language-alist
  '((#x400 . nil)
    (#x408 . cp1253)
    (#x40D . cp1255)
    (#x409 . cp1251)
    (#x411 . cp932)
    (#x412 . cp949)
    (#x419 . cp1251)
    (#x804 . gb2312)))

(defconst rtf-coding-system-spec
  '((cp949  . 2)
    (cp932  . 2)
    (cp950  . 2)
    (gb2312 . 2)
    (big5   . 2)))

(defvar rtf-default-lang nil)
(make-variable-buffer-local 'rtf-default-lang)
(put 'rtf-default-lang 'permanent-local t)

(defvar rtf-coding-system-stack (list nil))
(make-variable-buffer-local 'rtf-coding-system-stack)
(put 'rtf-coding-system-stack 'permanent-local t)

(defun rtf-set-font-charset (num)
  (setcar rtf-coding-system-stack (cdr (assq num rtf-font-charset-alist))))

(defun rtf-set-language (num)
  (setcar rtf-coding-system-stack (cdr (assq num rtf-language-alist))))

;;;  RTF Version
(rtf-define-control rtf  1
  (unless (= param 1)
    (error "Wrong RTF version %d, expected 1." param)))

;;;  Character Set
(defvar rtf-coding-system-alist
  '((ansi . cp1252)
    (mac  . mac-roman)
    (pc   . cp437)
    (pca  . cp850)))

(defvar rtf-coding-system 'ansi)
(make-variable-buffer-local 'rtf-coding-system)
(put 'rtf-coding-system 'permanent-local t)

(rtf-define-control ansi nil
  (setq rtf-coding-system 'ansi)
  (set-buffer-file-coding-system 'cp1252))

(rtf-define-control mac nil
  (setq rtf-coding-system 'mac)
  (set-buffer-file-coding-system 'mac-roman))

(rtf-define-control pc nil
  (setq rtf-coding-system 'pc)
  (set-buffer-file-coding-system 'cp437))

(rtf-define-control pca nil
  (setq rtf-coding-system 'pca)
  (set-buffer-file-coding-system 'cp850))

;;;  Unicode
(defvar rtf-ansi-codepage nil)
(make-variable-buffer-local 'rtf-ansi-codepage)
(put 'rtf-ansi-codepage 'permanent-local t)

(rtf-define-control ansicpg 0
  (setq rtf-ansi-codepage param)
  (let ((cs (intern (format "cp%d" param))))
    (and (coding-system-p cs)
	 (set-buffer-file-coding-system cs))))

(rtf-define-destination upr
  (let ((rtf-control-table rtf-control-table))
    (rtf-define-control ud nil)
    ;; XXX: we should record ANSI representation text
    ;; for output routine
    (rtf-read-group #'rtf-discard t)
    (rtf-read-group #'rtf-reinsert-formatted)))

(defvar rtf-none-unicode-bytes (list 1))
(make-variable-buffer-local 'rtf-none-unicode-bytes)
(put 'rtf-none-unicode-bytes 'permanent-local t)

(rtf-define-control uc 1
  (setcar rtf-none-unicode-bytes param))

(defun rtf-insert-unicode-char (code)
  (rtf-insert-formatted
   (decode-char 'ucs
		(cond ((< code 0)     (+ 65536 code))
		      ((> code 32767) (- 65536 code))
		      (t code))))
  ;; remove following ansi characters
  (dotimes (i (car rtf-none-unicode-bytes))
    (cond ((eq (following-char) ?\\)
	   (rtf-perform-control-action)
	   (delete-char -1))
	  (t (delete-char 1)))))

(rtf-define-control u 0
  (rtf-insert-unicode-char param))

;;;  Default Font
(defvar rtf-default-font 0)
(make-variable-buffer-local 'rtf-default-font)
(put 'rtf-default-font 'permanent-local t)

(rtf-define-control deff 0
  (setq rtf-default-font param))

(defun rtf-write-prolog (buffer)
  (let ((coding-system (buffer-local-value 'rtf-coding-system buffer))
	(ansi-codepage (buffer-local-value 'rtf-ansi-codepage buffer))
	(default-font  (buffer-local-value 'rtf-default-font buffer))
	(default-lang  (buffer-local-value 'rtf-default-lang buffer))
	(default-tab-width (buffer-local-value 'rtf-default-tab-width buffer)))
    (insert (format "{\\rtf1\\%s" coding-system))
    (and ansi-codepage (insert (format "\\ansicpg%d" ansi-codepage)))
    (and default-lang  (insert (format "\\deflang%d" default-lang)))
    (insert (format "\\deff%d" default-font)
	    (format "\\deftab%d" default-tab-width))
    ;; XXX: fix \uc handling
    (insert "\\uc0\n")))

;;;  Special Characters and Formatting
(defun rtf-insert-char-by-hex ()
  (let* ((cs    (car rtf-coding-system-stack))
	 (bytes (cdr (assoc cs rtf-coding-system-spec))))
    (rtf-insert-formatted
     (decode-coding-string
      (rtf-read-char-as-hex bytes)
      (or cs buffer-file-coding-system)))))

(rtf-define-special \'
  (rtf-insert-char-by-hex))

(rtf-define-special ~
  (rtf-insert-formatted " "))

(rtf-define-special \\
  (rtf-insert-formatted ?\\))

(rtf-define-special {
  (rtf-insert-formatted ?{))

(rtf-define-special }
  (rtf-insert-formatted ?}))

(rtf-define-control tab nil
  (rtf-insert-tab))

(rtf-define-control ldblquote nil
  (rtf-insert-formatted ?\"))

(rtf-define-control rdblquote nil
  (rtf-insert-formatted ?\"))

(rtf-define-control lquote nil
  (rtf-insert-formatted ?\`))

(rtf-define-control rquote nil
  (rtf-insert-formatted ?\'))

(rtf-define-control bullet nil
  (rtf-insert-formatted ?����))

(rtf-define-control endash nil
  (rtf-insert-formatted "--"))

(rtf-define-control emdash nil
  (rtf-insert-formatted "---"))

(rtf-define-control emspace nil
  (rtf-insert-formatted " "))

(rtf-define-control enspace nil
  (rtf-insert-formatted " "))

(rtf-define-control qmspace nil
  (rtf-insert-formatted " "))

(defun rtf-insert-hard-newline ()
  (rtf-insert-formatted "\n")
  (put-text-property (1- (point)) (point) 'hard t))

(rtf-define-control line nil
  (rtf-insert-hard-newline))

(rtf-define-special "\n"
  (rtf-insert-hard-newline))

(rtf-define-special "\r"
  (rtf-insert-hard-newline))

(rtf-define-control chdate nil
  (rtf-insert-formatted (format-time-string "%e.%m.%y")))

(rtf-define-control chldpl nil
  (rtf-insert-formatted (format-time-string "%a, %b, %y")))

(rtf-define-control chadpa nil
  (rtf-insert-formatted (format-time-string "%a, %b, %y")))

(rtf-define-control chtime nil
  (rtf-insert-formatted (format-time-string "%h:%m:%s")))

;;;  Default Fonts

;;;  Font Table
(defvar rtf-font-table nil)
(make-variable-buffer-local 'rtf-font-table)
(put 'rtf-font-table 'permanent-local t)

(defstruct rtf-font
  themefont
  num
  family
  charset
  pitch
  panose
  nontagged
  type
  codepage
  name
  altname)

(rtf-define-destination fonttbl
  (let* ((rtf-control-table nil)
	 (beg (point))
	 (font (make-rtf-font)))
    (rtf-define-controls
	(flomajor fhimajor fdbmajor fbimajor flominor fhiminor fdbminor fbiminor)
	nil (setf (rtf-font-themefont font) name))
    (rtf-define-controls (fnil froman fswiss fmodern fscript fdecor ftech) nil (setf (rtf-font-family font) name))
    (rtf-define-control f 0 (setf (rtf-font-num font) param))
    (rtf-define-control fcharset 0 (setf (rtf-font-charset font) param))
    (rtf-define-control fprq 0 (setf (rtf-font-pitch font) param))
    (rtf-define-control nontagged nil (setf (rtf-font-nontagged font) t))
    (rtf-define-control cpg 0 (setf (rtf-font-codepage font) param))
    (rtf-define-destination panose (setf (rtf-font-panose font) (rtf-read-group-contents)))
    (rtf-with-group
      (rtf-dispatch #'rtf-read-over)
      (when (eq (preceding-char) ?\;)
	(delete-char -1)
	(setf (rtf-font-name font) (rtf-read-region beg (point)))
	(push (cons (rtf-font-num font) font) rtf-font-table)
	(setq font (make-rtf-font))
	(setq beg (point)))))
  (setq rtf-font-table (nreverse rtf-font-table))
  (rtf-set-font rtf-default-font))

(defun rtf-write-font-table (buffer)
  (let ((rtf-font-table (buffer-local-value 'rtf-font-table buffer)))
    (rtf-write-as-group
      (insert "\\fonttbl")
      (dolist (font rtf-font-table)
	(setq font (cdr font))
	(insert "\n")
	(rtf-write-as-group
	  (when (rtf-font-themefont font)
	    (insert (format "\\%s" (rtf-font-themefont font))))
	  (insert (format"\\f%d" (rtf-font-num font)))
	  (when (rtf-font-family font)
	    (insert (format "\\%s" (rtf-font-family font))))
	  (when (rtf-font-charset font)
	    (insert (format "\\fcharset%d" (rtf-font-charset font))))
	  (when (rtf-font-pitch font)
	    (insert (format "\\fprq%d" (rtf-font-pitch font))))
	  (when (rtf-font-panose font)
	    (insert " ")
	    (rtf-write-as-group
	      (insert "\\*\\panose " (rtf-font-panose font))))
	  (when (rtf-font-nontagged font)
	    (insert "\\*\\fname"))
	  (when (rtf-font-codepage font)
	    (insert (format "\\cpg%d" (rtf-font-codepage font))))
	  (insert " " (rtf-font-name font))
	  (when (rtf-font-altname font)
	    (rtf-write-as-group
	      (insert "\\*\\falt " (rtf-font-altname font))))
	  (insert ";"))))
    (insert "\n")))

(rtf-define-control f 0
  (rtf-set-font param))

(defun rtf-set-font (num)
  (let ((font (cdr (assq num rtf-font-table))))
    ;; Some strange rtf documents have \f primitive without a declared
    ;; font table. We should a bit tolerant on this point.
    (when font 
      (rtf-set-property rtf-char-props :family (rtf-font-name font))
      (rtf-set-font-charset (rtf-font-charset font)))))

(rtf-define-control fs 24
  ;; Emacs expects a integer in 1/10 points, but PARAM is in
  ;; half-points, so 5 is the correction factor.
  (rtf-set-property rtf-char-props :height (* 5 param)))

;;;  File Table
(defvar rtf-file-table nil)
(make-variable-buffer-local 'rtf-file-table)
(put 'rtf-file-table 'permanent-local t)

(defstruct rtf-file
  id
  relative
  osnum
  sources
  name)

(rtf-define-destination filetbl
  (let ((rtf-control-table nil))
    (rtf-define-destination file
      (setf (rtf-file-name file) (rtf-read-group-contents))
      (push file rtf-file-table)
      (setq file (make-rtf-file)))
    (rtf-define-control fid       0 (setf (rtf-file-id file) param))
    (rtf-define-control frelative 0 (setf (rtf-file-relative file) param))
    (rtf-define-control osnum     0 (setf (rtf-file-osnum file) param))
    (rtf-define-controls
	(fvalidmac fvaliddos fvalidntfs fvalidhpfs fnetwork fnonfilesys) nil
      (push (rtf-file-sources file) name))
    (rtf-with-group
      (rtf-dispatch #'rtf-discard)))
  (setq rtf-file-table (nreverse rtf-file-table)))

(defun rtf-write-file-table (buffer)
  (let ((rtf-file-table (buffer-local-value 'rtf-file-table buffer)))
    (when rtf-file-table
      (rtf-write-as-group
	(insert "\\*\\filetbl")
	(dolist (file rtf-file-table)
	  (rtf-write-as-group
	    (insert
	     (format "\\file\\fid%d" (rtf-file-id file)))
	    (when (rtf-file-relative file)
	      (insert (format "\\frelative%d" (rtf-file-relative file))))
	    (when (rtf-file-osnum file)
	      (insert (format "\\osnum%d" (rtf-file-osnum file))))
	    (dolist (res (rtf-file-sources file))
	      (insert "\\%s" res))
	    (insert (format " %s\n" (rtf-file-name file)))))))))

;;;  Color Table
(defvar rtf-color-table nil)
(make-variable-buffer-local 'rtf-color-table)
(put 'rtf-color-table 'permanent-local t)

(rtf-define-destination colortbl
  (let ((rtf-control-table rtf-control-table)
	theme red green blue)
    (rtf-define-control red   0 (setq red param))
    (rtf-define-control green 0 (setq green param))
    (rtf-define-control blue  0 (setq blue param))
    (rtf-with-group
     (rtf-dispatch #'rtf-read-over)
     (when (eq (preceding-char) ?\;)
       (delete-char -1)
       (push (cond ((or red green blue)
		    (or red   (setq red   0))
		    (or green (setq green 0))
		    (or blue  (setq blue  0))
		    (format "#%02x%02x%02x" red green blue))
		   (t
		    "default"))
	     rtf-color-table)
       (setq red nil green nil blue nil))))
  (setq rtf-color-table (nreverse rtf-color-table)))

(defun rtf-write-color-table (buffer)
  (let ((rtf-color-table (buffer-local-value 'rtf-color-table buffer)))
    (when rtf-color-table
      (rtf-write-as-group
	(insert "\\colortbl")
	(dolist (c rtf-color-table)
	  (unless (string= c "default")
	    (insert (format "\n\\red%d\\green%d\\blue%d"
			    (string-to-number (substring c 1 3) 16)
			    (string-to-number (substring c 3 5) 16)
			    (string-to-number (substring c 5)   16))))
	  (insert ?\;))))
    (insert ?\n)))

(defun rtf-color-index (c)
  (unless (eq ?# (aref c 0))
    (setq c (apply #'format "#%02x%02x%02x"
		   (mapcar (lambda (x) (/ x 256)) (color-values c)))))
  (let ((i 0) (pos nil))
    (setq pos (catch 'found
		(dolist (col rtf-color-table)
		  (if (string-equal c col)
		      (throw 'found i)
		    (setq i (1+ i))))))
    (unless pos
      (setq rtf-color-table (append rtf-color-table (list c)))
      (setq pos i))
    pos))

;;;  Default Properties
(defvar rtf-default-char-props nil)
(make-variable-buffer-local 'rtf-default-char-props)
(put 'rtf-default-char-props 'permanent-local t)

(defvar rtf-default-paragraph-props nil)
(make-variable-buffer-local 'rtf-default-paragraph-props)
(put 'rtf-default-paragraph-props 'permanent-local t)

;;;  Style Sheet
(defstruct rtf-style-sheet
  type
  formatting
  name
  additive
  based
  next
  autoupdate
  hidden
  link
  locked
  personal
  compose
  reply
  rsid
  semihidden
  qformat
  priority
  unhideused)

(defvar rtf-style-sheets nil)
(make-variable-buffer-local 'rtf-style-sheets)
(put 'rtf-style-sheets 'permanent-local t)

(rtf-define-destination stylesheet
  (let ((rtf-control-table nil)
	(style (make-rtf-style-sheet))
	(beg   (point))
	(rtf-default-control-action
	 (lambda (name param)
	   (setf (rtf-style-sheet-formatting style)
		 (concat (rtf-style-sheet-formatting style)
			 (format "\\%s%s" name (if param param "")))))))
    (rtf-define-special \' (rtf-insert-char-by-hex))
    (rtf-define-control u (rtf-insert-unicode-char param))
    (rtf-define-controls (cs s ds ts tsrowd) 0 (setf (rtf-style-sheet-type style) (cons name param)))
    (rtf-define-control additive    nil (setf (rtf-style-sheet-additive style) t))
    (rtf-define-control sbasedon      0 (setf (rtf-style-sheet-based style) param))
    (rtf-define-control snext         0 (setf (rtf-style-sheet-next style) param))
    (rtf-define-control autoupd     nil (setf (rtf-style-sheet-autoupdate style) t))
    (rtf-define-control shidden     nil (setf (rtf-style-sheet-hidden style) t))
    (rtf-define-control link          0 (setf (rtf-style-sheet-link style) param))
    (rtf-define-control slocked     nil (setf (rtf-style-sheet-locked style) t))
    (rtf-define-control spersonal   nil (setf (rtf-style-sheet-personal style) t))
    (rtf-define-control compose     nil (setf (rtf-style-sheet-compose style) t))
    (rtf-define-control reply       nil (setf (rtf-style-sheet-reply style) t))
    (rtf-define-control styrsid       0 (setf (rtf-style-sheet-rsid style) param))
    (rtf-define-control ssemihidden nil (setf (rtf-style-sheet-semihidden style) param))
    (rtf-define-control sqformat    nil (setf (rtf-style-sheet-qformat style) param))
    (rtf-define-control spriority   nil (setf (rtf-style-sheet-priority style) param))
    (rtf-define-control sunhideused nil (setf (rtf-style-sheet-unhideused style) param))
    (rtf-with-group
      (rtf-dispatch #'rtf-read-over)
      (when (eq (preceding-char) ?\;)
	(delete-char -1)
	(setf (rtf-style-sheet-name style) (rtf-read-region beg (point)))
	(push (cons (cdr (rtf-style-sheet-type style)) style) rtf-style-sheets)
	(setq style (make-rtf-style-sheet))
	(setq beg (point)))))
  (setq rtf-style-sheets (nreverse rtf-style-sheets)))

(defun rtf-write-stylesheets (buffer)
  (let ((rtf-style-sheets (buffer-local-value 'rtf-style-sheets buffer)))
    (when rtf-style-sheets
      (rtf-write-as-group
	(insert "\\stylesheet")
	(dolist (style rtf-style-sheets)
	  (setq style (cdr style))
	  (insert "\n")
	  (rtf-write-as-group
;; 	    (rtf-cond-prog
;; 	     ((rtf-style-sheet-type style)
;; 	      (and (eq (car it) 'cs) (insert "\\*"))
;; 	      (insert (format "\\%s" (car it)))
;; 	      (and (cdr type) (insert (number-to-string (cdr it)))))
;; 	     ((rtf-style-sheet-formatting style)
;; 	      (insert it))
;; 	     ((rtf-style-sheet-additive style)
;; 	      (insert it))
;; 	     ((rtf-style-sheet-base style)
;; 	      (insert (format "" it)))
;; 	     ((rtf-style-sheet-next style)
;; 	      (insert (format "\\snext%d" it))))


	    (let ((type (rtf-style-sheet-type style)))
	      (when type
		(and (eq (car type) 'cs) (insert "\\*"))
		(insert (format "\\%s" (car type)))
		(and (cdr type) (insert (number-to-string (cdr type))))))
	    (and (rtf-style-sheet-formatting style)
		 (insert (rtf-style-sheet-formatting style)))
	    (and (rtf-style-sheet-additive style)
		 (insert "\\additive"))
	    (and (rtf-style-sheet-based style)
		 (insert (format "\\sbasedon%d" (rtf-style-sheet-based style))))
	    (and (rtf-style-sheet-next style)
		 (insert (format "\\snext%d" (rtf-style-sheet-next style))))
	    (insert " " (rtf-style-sheet-name style) ";"))))
      (insert "\n"))))

(rtf-define-controls (cs s ds ts tsrowd) 0
  (let ((style (cdr (assq param rtf-style-sheets))))
    (when style
      (let ((formatting (rtf-style-sheet-formatting style)))
	(and formatting (rtf-insert-and-eval (concat formatting " ")))))))


;;  List Tables

;;  Paragraph Group Properties

;;  Track Changes (revision marks)

;;  RSID

;;;  User Protection Info
(rtf-defvar rtf-user-list nil t
  "Lists of users, which granted exceptions to the document protection.")

(defvar rtf-user-list nil)
(make-variable-buffer-local 'rtf-user-list)
(put 'rtf-user-list 'permanent-local t)

(rtf-define-destination protusertbl
  (let ((beg (point)))
    (rtf-with-group
      (rtf-dispatch #'rtf-reinsert-formatted)
      (and (< beg (point))
	   (push (rtf-read-region beg (point)) rtf-user-list))))
  (setq rtf-user-list (nreverse rtf-user-list)))

(defun rtf-write-protection-info (buffer)
  (let ((rtf-user-list (buffer-local-value 'rtf-user-list buffer)))
    (when rtf-user-list
      (insert "\n")
      (rtf-write-as-group
	(insert "\\*\\protusertbl")
	(dolist (user rtf-user-list)
	  (insert "\n")
	  (rtf-write-as-group (insert user)))))))

(defun rtf-show-protection-info ()
  (interactive)
  (if rtf-user-list
      (message "Allowed by %s." (mapconcat #'identity rtf-user-list ", "))
    (message "Not restricted.")))

;;;  Generator
(defvar rtf-generator-data nil)
(make-local-variable 'rtf-generator-data)
(put 'rtf-generator-data 'permanent-local t)

(rtf-define-control generator nil
  (setq rtf-generator-data
	(substring (rtf-read-group-contents) 0 -1)))

(defun rtf-write-generator-data (buffer)
  (let ((rtf-generator-data (buffer-local-value 'rtf-generator-data buffer)))
    (when rtf-generator-data
      (rtf-write-as-group
	(insert "\\*\\generator " rtf-generator-data ";\n"))
      (insert "\n"))))

(defun rtf-show-generator-data ()
  (interactive)
  (message
   (if rtf-generator-data rtf-generator-data
     "No generator info available.")))

;;;  Information Group
(defstruct rtf-info
  title
  subject
  author
  manager
  company
  operator
  category
  keywords
  comment
  version
  doccomm
  hlinkbase)

(defvar rtf-info-data (make-rtf-info))
(make-local-variable 'rtf-info-data)
(put 'rtf-generator-data 'permanent-local t)

(defmacro rtf-define-info-subcontrol (name)
  (let ((getter (intern (format "rtf-info-%s" name))))
    `(rtf-define-destination ,name
       (setf (,getter rtf-info-data) (rtf-read-group-contents)))))

(rtf-define-destination info
  (let ((rtf-control-table nil))
    (rtf-define-info-subcontrol title)
    (rtf-define-info-subcontrol subject)
    (rtf-define-info-subcontrol author)
    (rtf-define-info-subcontrol manager)
    (rtf-define-info-subcontrol company)
    (rtf-define-info-subcontrol operator)
    (rtf-define-info-subcontrol category)
    (rtf-define-info-subcontrol comment)
    (rtf-define-info-subcontrol version)
    (rtf-define-info-subcontrol doccomm)
    (rtf-define-info-subcontrol hlinkbase)
    (rtf-with-group
      (rtf-dispatch #'rtf-discard))))

(defun rtf-write-info-group (buffer)
  (let ((rtf-info-data (buffer-local-value 'rtf-info-data buffer)))
    (when rtf-info-data
      (rtf-write-as-group
	(insert "\\info")
	(dolist (field  '(title subject author manager company
			  operator category keywords comment
			  version doccomm hlinkbase))
	  (let ((content
		 (funcall (intern (format "rtf-info-%s" field))
			  rtf-info-data)))
	    (when content
	      (insert "\n")
	      (rtf-write-as-group
		(insert (format "\\%s %s" field content))))))
	;; at last insert current time as last revision time
	(insert "\n")
	(rtf-write-as-group
	  (insert
	   (format-time-string
	    "\\revtim\\yr%Y\\mo%m\\dy%d\\hr%H\\min%M\\sec%S"))))
      (insert "\n"))))


(defun rtf-show-info-group (info)
  )

;;;  Footnotes
(defvar rtf-page-footnotes nil)
(make-variable-buffer-local 'rtf-page-footnotes)
(put 'rtf-page-footnotes 'permanent-local t)

(defvar rtf-footnote-number 0)
(make-variable-buffer-local 'rtf-footnote-number)
(put 'rtf-footnote-number 'permanent-local t)

(rtf-define-control ftnstart 1
  (and (> rtf-footnote-number 1)
       (error "! (ftnstart): internal error"))
  (setq rtf-footnote-number param))

(defvar rtf-footnote-numbering 'arabic)
(make-variable-buffer-local 'rtf-footnote-numbering)
(put 'rtf-footnote-numbering 'permanent-local t)

(rtf-define-control ftnnar nil
  (setq rtf-footnote-numbering 'arabic))

(rtf-define-control ftnnalc nil
  (setq rtf-footnote-numbering 'alpha-lowercase))

(rtf-define-control ftnnauc nil
  (setq rtf-footnote-numbering'alpha-uppercase))

(rtf-define-control fntnrlc nil
  (setq rtf-footnote-numbering'roman-lowercase))

(rtf-define-control fntnruc nil
  (setq rtf-footnote-numbering'roman-uppercase))

(rtf-define-control fntnchi nil
  (setq rtf-footnote-numbering'chicago))

(defun rtf-format-footnote-number (num)
  (case rtf-footnote-numbering
    ((arabic) (number-to-string num))
    ((alpha-uppercase) (format "%c" (+ ?A (1- num))))
    ((alpha-lowercase) (format "%c" (+ ?a (1- num))))
    (t (number-to-string num))))

(rtf-defvar rtf-footnote-restart 'continue t
  "Restart footnode mode style. Possible values are 'continue
  'page and 'section.")

(defun rtf-reset-footnote-number (kind)
  (and (eq kind rtf-footnote-restart)
       (setq rtf-footnote-number 0)))

(rtf-define-control fntrstcont nil
  (setq rtf-footnote-restart 'continue))

(rtf-define-control fntrstpage nil
  (setq rtf-footnote-restart 'page))

(rtf-define-control fntrestart nil
  (setq rtf-footnote-restart 'section))

(rtf-define-control chftn 0
  (or (boundp 'rtf-inside-footnote)
      (setq rtf-footnote-number (1+ rtf-footnote-number)))
  (rtf-insert-formatted
   (rtf-format-footnote-number rtf-footnote-number)))

(rtf-define-destination footnote
  (let ((rtf-inside-footnote t)
	(beg (point))
	(mark (preceding-char)))
    (rtf-reset-properties rtf-char-props)
    (rtf-reset-properties rtf-paragraph-props)
    (rtf-open-paragraph)
    (insert "\n")
    (rtf-with-group
      (rtf-dispatch #'rtf-reinsert-formatted))
    (rtf-close-paragraph)
    (add-to-list 'rtf-page-footnotes
		 (cons mark (rtf-read-region beg (point)))
		 'append)))

;; XXX: interactive function for changing footnote numbering style in
;; whole document


;;;  Picture
(rtf-define-destination shppict
  ;; XXX:
  )

(rtf-define-destination nonshppict
  ;; XXX:
  )

(rtf-define-destination pict
  (let ((rtf-control-table nil)
	(beg (point))
	(type nil)
	(x-scale 100)
	(y-scale 100)
	(hex-to-byte
	 (lambda ()
	   (insert (rtf-read-char-as-hex 1)))))
    ;; define some subcontrols
    (rtf-define-control pngblip  nil   (setq type 'png))
    (rtf-define-control jpegblip nil   (setq type 'jpeg))
    (rtf-define-control bin        0   (forward-char param))
    (rtf-define-control picscalex  100 (setq x-scale param))
    (rtf-define-control picscaley  100 (setq y-scale param))

    ;;  Read image data
    (rtf-with-group
      (rtf-dispatch hex-to-byte))
    ;;  Display image if image type already known, otherwise ignore
    ;;  all the bytes and bits and insert a placeholder.
    (cond ((memq type '(png jpeg))
	   (insert-image
	    (create-image
	     (string-make-unibyte (rtf-read-region beg (point))) type t)))
	  (t (delete-region beg (point))))
    ))

(defun rtf-write-image (image)
  (rtf-write-as-group
    (insert "\\pict ")
    (mapc
     (lambda (c)
       (and (= (current-column) 64) (insert "\n"))
       (insert (format "%02x" c)))
     (plist-get (cdr image) :data))))


;;;  Header and Footer
(defvar rtf-doc-header nil)
(make-variable-buffer-local 'rtf-doc-header)
(put 'rtf-doc-header 'permanent-local t)

(rtf-define-destination header
  (setq rtf-doc-header (rtf-read-group-contents-nonexpanded)))

(defvar rtf-doc-footer nil)
(make-variable-buffer-local 'rtf-doc-footer)
(put 'rtf-doc-footer 'permanent-local t)

(rtf-define-destination footer
  (setq rtf-doc-footer (rtf-read-group-contents-nonexpanded))
  (rtf-insert-and-eval rtf-doc-footer))


;;;  Character Formatting
(defvar rtf-char-props (rtf-make-properties-stack))
(make-variable-buffer-local 'rtf-char-props)
(put 'rtf-char-props 'permanent-local t)

(defun rtf-add-char-properties (props len)
  (put-text-property (- (point) len) (point) 'face (copy-tree props)))

(rtf-define-control plain nil
  (rtf-reset-properties rtf-char-props)
  (rtf-set-language rtf-default-lang))

(rtf-define-control b 1
  (rtf-toggle-property rtf-char-props param :weight 'bold 'normal))

(rtf-define-control ul 1
  (rtf-toggle-property rtf-char-props param :underline t nil))

(rtf-define-control ulnone nil
  (rtf-set-property rtf-char-props :underline nil))

(rtf-define-control ulc 0
  (rtf-set-property rtf-char-props :underline (nth param rtf-color-table)))

(rtf-define-control i 1
  (rtf-toggle-property rtf-char-props param :slant 'italic 'normal))

(rtf-define-control strike 1
  (rtf-toggle-property rtf-char-props param :strike-through t nil))

(rtf-define-control cf 0
  (let ((col    (nth param rtf-color-table))
	(defcol (face-attribute 'default :foreground)))
    (rtf-set-property rtf-char-props
		      :foreground
		      (cond ((string= col "default")
			     defcol)
			    ((and
			      rtf-invert-foreground-color
			      (string= col "#000000")
			      (not (color-values defcol)))
			     "#ffffff")
			    (t
			     col)))))

(rtf-define-control cb 0
  (let ((col (nth param rtf-color-table)))
    (rtf-set-property rtf-char-props
		      :background
		      (if (string= col "default")
			  (face-attribute 'default :background)
			col))))

(rtf-define-control shad 0
  (rtf-set-property rtf-char-props :foreground "#696969"))



;;;  Display Properties
(defvar rtf-display-props (rtf-make-properties-stack))
(make-variable-buffer-local 'rtf-display-props)
(put 'rtf-display-props 'permanent-local t)

(defun rtf-apply-display-properties (props len)
  (put-text-property (- (point) len) (point) 'display (copy-tree props)))

(rtf-define-control super nil
  (rtf-set-property rtf-display-props 'raise 0.5)
  (rtf-set-property rtf-display-props 'height 0.5))

(rtf-define-control sub nil
  (rtf-set-property rtf-display-props 'raise -0.5)
  (rtf-set-property rtf-display-props 'height 0.5))

(rtf-define-control up 6
  ;; XXX: calculate raise face properly
  (rtf-set-property rtf-display-props 'raise (* 0.1 param)))

(rtf-define-control dn 6
  ;; XXX: calculate raise factor properly
  (rtf-set-property rtf-display-props 'raise (- (* 0.1 param))))

(rtf-define-control nosupersub nil
  (rtf-set-property rtf-display-props 'raise nil)
  (rtf-set-property rtf-display-props 'height nil))

;; 
(rtf-defvar rtf-visible-props (rtf-make-properties-stack) t
  "Properties stack for invisible face attribute.")

(defun rtf-apply-visible-properties (props len)
  (dolist (p (copy-tree props))
    (put-text-property (- (point) len) (point) (car p) (cadr p))))

(rtf-define-control v 1
  (rtf-toggle-property rtf-visible-props param 'invisible t nil)
  (rtf-toggle-property rtf-visible-props param 'intangible t nil))


;;;  Section Formatting
(defvar rtf-section-props (rtf-make-properties-stack))
(make-variable-buffer-local 'rtf-section-props)
(put 'rtf-section-props 'permanent-local t)

(defun rtf-apply-section-properties (props len)
  )

(defun rtf-open-section ()
  (rtf-open-paragraph))

(defun rtf-close-section ()
  ;; (rtf-apply-properties rtf-section-props #'rtf-section-format-last)
  (let ((cols  (cadr (rtf-get-property rtf-section-props 'columns)))
	(brk   (cadr (rtf-get-property rtf-section-props 'break))))

    ;; set default values
    (or cols (setq cols 1))

    ;;     (when (> cols 1)
    ;;       (message "===== COLS: %d" cols)
    ;;       (dolist (start rtf-paragraphs-start)
    ;; 	(message "ON: %S" start)))

    (rtf-reset-footnote-number 'section)))

(rtf-define-control sectd nil
  (rtf-reset-properties rtf-section-props)
  (rtf-set-property rtf-section-props 'columns 1))

(rtf-define-control sect nil
  (rtf-close-section)
  (rtf-open-section))

(rtf-define-control sbknone nil
  (rtf-set-property rtf-section-props 'break 'none))

(rtf-define-control sbkcol nil
  (rtf-set-property rtf-section-props 'break 'newcolumn))

(rtf-define-control sbkpage nil
  (rtf-set-property rtf-section-props 'break 'newpage))

(rtf-define-control sbkeven nil
  (rtf-set-property rtf-section-props 'break 'evenpage))

(rtf-define-control sbkodd nil
  (rtf-set-property rtf-section-props 'break 'oddpage))

(rtf-define-control cols 1
  (rtf-set-property rtf-section-props 'columns param))


;;;  Paragraph Formatting

;; (defstruct rtf-par
;;   "Paragraph formatting properties."
;;   style
;;   hyphenation
;;   in-table
;;   keep
;;   widow-control
;;   keep-next
;;   level
;;   no-line-numbering
;;   outline-level
;;   pagebreak-before
;;   justification
;;   first-line-indent
;;   left-indent
;;   right-indent
;;   space-before
;;   space-after
;;   line-spacing
;;   tab-list
;;   )

(defvar rtf-paragraph-props (rtf-make-properties-stack))
(make-variable-buffer-local 'rtf-paragraph-props)
(put 'rtf-paragraph-props 'permanent-local t)

(defvar rtf-paragraphs-start nil)
(make-variable-buffer-local 'rtf-paragraphs-start)
(put 'rtf-paragraphs-start 'permanent-local t)

(defun rtf-apply-paragraph-properties (props len)
  (let ((pos           (set-marker (make-marker) (point)))
	(first-indent  (cadr (assq 'first-indent  props)))
	(right-indent  (cadr (assq 'right-indent  props)))
	(left-indent   (cadr (assq 'left-indent   props)))
	(justify       (cadr (assq 'justification props)))
	(space-before  (cadr (assq 'space-before  props)))
	(space-after   (cadr (assq 'space-after   props)))
	(in-table      (cadr (assq 'in-table      props)))
	(columns       (cadr (rtf-get-property rtf-section-props 'columns)))
	(fill-column   fill-column)
	(last-par-start (car rtf-paragraphs-start)))

    ;; set default margin values
    (or left-indent  (setq left-indent  0))
    (or right-indent (setq right-indent 0))
    (or space-after  (setq space-after  0))
    (or space-before (setq space-before 0))

    (if in-table
	(rtf-register-table-start last-par-start)
      (rtf-format-last-table))

    (when (> (point) last-par-start)
      (goto-char last-par-start)
      (rtf-insert-space 'vertical space-before)
      (when first-indent
	(and (< first-indent 0)
	     (setq first-indent (+ first-indent left-indent)))
	(indent-to (+ (current-column) first-indent))
	(forward-char))
      (set-left-margin  (point) pos left-indent)
      (set-right-margin (point) pos right-indent)
      (fill-region (point) pos justify t))
    (goto-char pos)
    (rtf-insert-space 'vertical space-after)))

(defun rtf-open-paragraph ()
  (push (point) rtf-paragraphs-start))

(defun rtf-close-paragraph ()
  (rtf-apply-properties rtf-paragraph-props #'rtf-apply-paragraph-properties)
  (insert "\n")
  (pop rtf-paragraphs-start))

(rtf-define-control par nil
  (rtf-close-paragraph)
  (rtf-open-paragraph))

(rtf-define-control pard nil
  (rtf-reset-properties rtf-paragraph-props))

(rtf-define-control outlinelevel 1
  (rtf-set-property rtf-paragraph-props 'outlevel param))

(rtf-define-control intbl nil
  (rtf-set-property rtf-paragraph-props 'in-table t))

(rtf-define-control qc nil
  (rtf-set-property rtf-paragraph-props 'justification 'center))

(rtf-define-control ql nil
  (rtf-set-property rtf-paragraph-props 'justification 'left))

(rtf-define-control qr nil
  (rtf-set-property rtf-paragraph-props 'justification 'right))

(rtf-define-control qj nil
  (rtf-set-property rtf-paragraph-props 'justification 'full))

(rtf-define-control fi 0
  (rtf-set-property rtf-paragraph-props
		    'first-indent (rtf-twips-to-width param)))

(rtf-define-control li 0
  (rtf-set-property rtf-paragraph-props
		    'left-indent (rtf-twips-to-width param)))

(rtf-define-control ri 0
  (rtf-set-property rtf-paragraph-props
		    'right-indent (rtf-twips-to-width param)))

(rtf-define-control sa 0
  (rtf-set-property rtf-paragraph-props
		    'space-after (rtf-twips-to-height param)))

(rtf-define-control sb 0
  (rtf-set-property rtf-paragraph-props
		    'space-before (rtf-twips-to-height param)))

;;;  Tables
(defvar rtf-table-props (rtf-make-properties-stack))
(defvar rtf-cell-props  (rtf-make-properties-stack))
(defvar rtf-cell-pos-stack nil)
(defvar rtf-table-start nil)
(defconst rtf-table-cell-sep "==CELL==")
(defconst rtf-table-row-sep "==ROW==")

(defun rtf-last-paragraph-is-table ()
  (rtf-get-property rtf-paragraph-props 'table-inside))

(defun rtf-register-table-start (pos)
  (or rtf-table-start (setq rtf-table-start pos)))

(defun rtf-format-last-table ()
  (when rtf-table-start
    (save-excursion
      (dolist (pos rtf-cell-pos-stack)
	(goto-char pos)
	(newline))

;;       (and
;;        (re-search-backward rtf-table-row-sep rtf-table-start 'noerror)
;;        (goto-char (match-end 0)))
;;       (table-capture rtf-table-start
;; 		     (point)
;; 		     rtf-table-cell-sep rtf-table-row-sep))
;;     ;; reset table aux data
    (setq rtf-cell-pos-stack nil)
    (setq rtf-table-start nil))))

(rtf-define-control trowd nil
  ;; (rtf-reset-properties rtf-table-props)
  (rtf-register-table-start (point))
  (push (point) rtf-cell-pos-stack))

(rtf-define-control cell nil
  ;; (rtf-reset-properties rtf-cell-props)
  ;; (push (point) rtf-cell-pos-stack)
  (rtf-register-table-start (car rtf-paragraphs-start))
  (push (point) rtf-cell-pos-stack))

(rtf-define-control row nil
  ;; (push (point) rtf-cell-pos-stack)
  ;; (push (point) (car rtf-cell-pos-stack))
  (push (point) rtf-cell-pos-stack))

;;;  Document Formatting
(rtf-define-control deflang 0
  (setq rtf-default-lang param))

(rtf-define-control lang 0
  (rtf-set-language param))


;;;  Bookmarks
(defstruct rtf-bookmark
  name
  content
  pos
  first-col
  last-col)

(defvar rtf-bookmark-alist nil)
(make-variable-buffer-local 'rtf-bookmark-alist)
(put 'rtf-bookmark-alist 'permanent-local t)

(rtf-define-destination bkmkstart
  (let ((rtf-color-table nil)
	(bkm (make-rtf-bookmark)))
    (rtf-define-control bkmkcolf 0 (setf (rtf-bookmark-first-col bkm) param))
    (rtf-define-control bkmkcoll 0 (setf (rtf-bookmark-last-col  bkm) param))
    (setf (rtf-bookmark-pos bkm) (point))
    (push (cons (rtf-read-group-contents) bkm) rtf-bookmark-alist)))

(rtf-define-destination bkmkstop
  (let* ((name (intern (rtf-read-group-contents)))
	 (bkm  (cdr (assoc name rtf-bookmark-alist))))
    (and bkm
	 (setf (rtf-bookmark-content bkm)
	       (rtf-read-region (rtf-bookmark-pos bkm)
				rtf-last-bookmark-start)))))

(defun rtf-write-bookmarks (buffer)
  (let ((bookmarks (buffer-local-value 'rtf-bookmark-alist buffer)))
    (save-excursion
      (dolist (bkmk bookmarks)
	;; XXX: we need a kind of check to see that bookmark position
	;; is valid
	(setq bkmk (cdr bkmk))
	(goto-char (rtf-bookmark-pos bkmk))
	(rtf-write-as-group
	  (insert "\\bkmkstart")
	  (and (rtf-bookmark-first-col bkmk) (insert "\\bkmkcolf"))
	  (and (rtf-bookmark-last-col  bkmk) (insert "\\bkmkcoll"))
	  (insert " " (rtf-bookmark-name bkmk)))
	(insert (rtf-bookmark-content bkmk))
	(rtf-write-as-group
	  (insert "\\bkmkstop " (rtf-bookmark-name bkmk)))))))

(defun rtf-add-bookmark ()
  )

;;;  Comments (Annotations)

;;;  Fields
;; Liste of known / seen fields types:
;; INCLUDEFILE, INCLIDEPICTURE, HYPERLINK, REF, SYMBOL, REFNOT, TOC, PAGEREF, PAGE
;; QUOTE, NOTEREF, FORMTEXT

(defstruct rtf-field
  modifier
  instruction
  result)

(rtf-defvar rtf-fields nil t
  "List of document fields.")

(defun rtf-install-field-REF (field beg end)
  (make-text-button
   beg end
   'follow-link t
   'help-echo (concat "Go to: " (cadr (rtf-field-instruction field)))
   'action #'rtf-field-goto))

(defun rtf-install-field-HYPERLINK (field beg end)
  (let ((inst (rtf-field-instruction field)))
    (make-text-button 
     beg end
     'follow-link t
     'help-echo (concat "Link: " (if (string= "\\l" (cadr inst)) (caddr inst) (cadr inst)))
     'action #'rtf-field-goto)))

(defun rtf-field-goto (button)
  (let* ((beg (button-start button))
	 (field (get-text-property beg 'rtf-field))
	 inst url local)
    ;; Search for HYPERLINK or REF field, when this is embedded inside
    ;; another field, e.g. TOC etc.
    (while (and field (not (member (car (rtf-field-instruction field)) '("HYPERLINK" "REF"))))
      (setq field (rtf-field-result field)))
    
    (when field
      (setq inst (rtf-field-instruction field))
      (setq url (cadr inst))
      (and (string= url "\\l") (setq url (caddr inst)) (setq local t))
      (and (not local) (equal "REF" (car inst)) (setq local t))
      (cond (local
	     (goto-char (rtf-bookmark-pos (cdr (assoc url rtf-bookmark-alist)))))
	    (t 
	     (browse-url (cadr (rtf-field-instruction field))))))))

(rtf-define-destination fldinst
  (setf (rtf-field-instruction (car rtf-fields))
	(mapcar (lambda (entry) (replace-regexp-in-string "\"" "" entry))
		(split-string (rtf-read-group-contents) nil t))))

(rtf-define-destination field
  (let ((beg (point))
	(field (make-rtf-field)))
    (push field rtf-fields)
    (rtf-with-group (rtf-dispatch #'rtf-reinsert-formatted))
    (let ((result-fields (get-text-property beg 'rtf-field)))
      (and result-fields (setf (rtf-field-result field) result-fields))
      (put-text-property beg (point) 'rtf-field field))
    (rtf-install-field-handler field beg (point))))

(defun rtf-install-field-handler (field beg end)
  (let ((field-install (intern (concat "rtf-install-field-" (car (rtf-field-instruction field))))))
    (and (fboundp field-install)
	 (funcall field-install field beg end))))

;;;  Index Entries


;;;  Table of Contents



;;;  Tabs
(defstruct rtf-tab
  width					; in twips
  position				; in twips from left margin
  kind					; decimal, centered, flush-right
  leader)

(rtf-defvar rtf-default-tab-width 720 t)

(rtf-define-control deftab 720
  (setq rtf-default-tab-width param))

(defvar rtf-current-tab (make-rtf-tab))

(rtf-define-control tx 0
  (setf (rtf-tab-position rtf-current-tab) param))

(rtf-define-control tqr nil
  (setf (rtf-tab-kind rtf-current-tab) 'flush-right))

(rtf-define-control tqc nil
  (setf (rtf-tab-kind rtf-current-tab) 'centered))

(rtf-define-control tqdec nil
  (setf (rtf-tab-kind rtf-current-tab) 'decimal))

(rtf-define-control tldot nil
  (setf (rtf-tab-leader rtf-current-tab) ?.))

(rtf-define-control tlmdot nil
  (setf (rtf-tab-leader rtf-current-tab) ?·))

(rtf-define-control tlhyph nil
  (setf (rtf-tab-leader rtf-current-tab) ?-))

(rtf-define-control tlul nil
  (setf (rtf-tab-leader rtf-current-tab) ?_))

(rtf-define-control tleq nil
  (setf (rtf-tab-leader rtf-current-tab) ?=))

;; \tbN A bar tab position in twips from the left margin.

(defun rtf-insert-tab ()
  ;; XXX: must be improved
  (rtf-insert-formatted ?\t))

(defun rtf-write-tab (tab)
  ;; XXX:
  )


;;;  Insertion
(defun rtf-insert-space (dir size)
  (unless (zerop size)
    (cond ((eq dir 'vertical)
	   ;; XXX convert twips to colums
	   (insert "\n"))
	  ((eq dir 'horizontal)
	   ;; XXX convert twips to chars
	   (insert " ")))))

(defconst rtf-none-special-regexp "[^{}\\\r\n\t]+")

(defun rtf-discard ()
  (delete-region (point) (re-search-forward rtf-none-special-regexp)))

(defun rtf-read-over ()
  (re-search-forward rtf-none-special-regexp))

(defun rtf-reinsert-formatted ()
  (rtf-insert-formatted
   (rtf-read-region (point) (re-search-forward rtf-none-special-regexp))))

(defun rtf-insert-formatted (arg)
  (insert-and-inherit arg)
  (let ((len (and (stringp arg) (length arg))))
    (and (rtf-fresh-properties-p rtf-visible-props)
	 (rtf-apply-properties rtf-visible-props
			       #'rtf-apply-visible-properties
			       len))
    
    (and (rtf-fresh-properties-p rtf-char-props)
	 (rtf-apply-properties rtf-char-props
			       #'rtf-add-char-properties
			       len))

    (and (rtf-fresh-properties-p rtf-display-props)
	 (rtf-apply-properties rtf-display-props
			       #'rtf-apply-display-properties
			       len))))


(defun rtf-insert-empty-page ()
  (insert "\n\f\n"))

(defun rtf-insert-anno (anno)
  (message "RTF anno at %d with value %S" (point) anno)
  (put-text-property (point) (1+ (point)) 'rtf-anno anno))

(defconst rtf-footnote-separator "--------------")

(defun rtf-insert-page-footnotes ()
  (when rtf-page-footnotes
    (insert rtf-footnote-separator)
    (dolist (note rtf-page-footnotes)
      (insert (cdr note)))
    (setq rtf-page-footnotes nil)))

(defun rtf-insert-page-header ()
  (when rtf-doc-header
    (rtf-open-paragraph)
    (rtf-insert-and-eval rtf-doc-header 'as-group)
    (rtf-close-paragraph)))

(defun rtf-insert-page-footer ()
  (when rtf-doc-footer
    (rtf-open-paragraph)
    (rtf-insert-and-eval rtf-doc-footer 'as-group)
    (rtf-close-paragraph)))

;;;  Page
(defvar rtf-current-page-number 0)
(make-variable-buffer-local 'rtf-current-page-number)
(put 'rtf-current-page-number  'permanent-local t)

(defstruct rtf-page
  (width         12240)
  (height        15840)
  (margin-left   1880)
  (margin-right  1880)
  (margin-top    1440)
  (margin-bottom 1440))

(rtf-defvar rtf-page (make-rtf-page) t
  "Describes page layout of current document.")

(defun rtf-write-page-info (origbuf)
  (let ((rtf-page (buffer-local-value 'rtf-page origbuf)))
    (when rtf-page
      (insert
       (format "\\paperw%d" (rtf-page-width rtf-page))
       (format "\\paperh%d" (rtf-page-height rtf-page))
       (format "\\margl%d"  (rtf-page-margin-left rtf-page))
       (format "\\margr%d"  (rtf-page-margin-right rtf-page))
       (format "\\margt%d"  (rtf-page-margin-top rtf-page))
       (format "\\margb%d"  (rtf-page-margin-bottom rtf-page))
       "\n"))))

(rtf-define-control paperw 12240
  (setf (rtf-page-width rtf-page) param))

(rtf-define-control paperh 15840
  (setf (rtf-page-height rtf-page) param))

(rtf-define-control margl 1800
  (setf (rtf-page-margin-left rtf-page) param))

(rtf-define-control margr 1800
  (setf (rtf-page-margin-right rtf-page) param))

(rtf-define-control margt 1440
  (setf (rtf-page-margin-top rtf-page) param))

(rtf-define-control margb 1440
  (setf (rtf-page-margin-bottom rtf-page) param))

(defun rtf-open-page ()
  (rtf-insert-page-header)
  (rtf-open-paragraph))

(defun rtf-close-page (&optional no-form-feed)
  (rtf-close-paragraph)
  (rtf-break-page no-form-feed))

(defun rtf-break-page (&optional no-form-feed)
  (rtf-insert-page-footnotes)
  (rtf-insert-page-footer)
  (or no-form-feed (insert "\f"))
  (insert "\n")
  (setq rtf-current-page-number (1+ rtf-current-page-number)))

(rtf-define-control page nil
  (rtf-close-page)
  (rtf-open-page))

(rtf-define-control chpgn nil
  (insert (number-to-string rtf-current-page-number)))

(rtf-define-control pagebb nil
  (rtf-set-property rtf-paragraph-props 'break-page t))

;;;  Reader
(defun rtf-read (regexp)
  (when (looking-at regexp)
    (prog1 (match-string-no-properties 1)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun rtf-read-region (beg end)
  (prog1 (buffer-substring beg end)
    (delete-region beg end)))

(defun rtf-read-char-as-hex (bytes)
  (or bytes (setq bytes 1))
  (let ((str (make-string bytes ?\ ))
	(i 0))
    (while (< i bytes)
      (aset str i (string-to-number (rtf-read-region (point) (+ 2 (point))) 16))
      (and (< (1+ i) bytes)
	   (looking-at "\\(\n\\|\r\\)*\\\\'")
	   (delete-region (match-beginning 0) (match-end 0)))
      (setq i (1+ i)))
    str))

(defun rtf-read-group-contents ()
  (let ((beg (point)))
    (rtf-with-group
      (rtf-dispatch #'rtf-read-over))
    (rtf-read-region beg (point))))

(defun rtf-read-group-contents-nonexpanded ()
  (let ((beg (point)))
    (parse-partial-sexp (point) (point-max) -1)
    (backward-char)
    (rtf-read-region beg (point))))

(defun rtf-read-control-word ()
  (let ((control-regexp "\\([-_'*{}\\\n\r:|~ ]\\|[[:alpha:]]+\\)"))
    (intern (rtf-read control-regexp))))

(defvar rtf-default-control-action nil)

(defun rtf-perform-control-action (&optional skip)
  (delete-char 1)
  (unless skip
    (let* ((param-regexp "\\(-?[[:digit:]]+\\)")
	   (word (rtf-read-control-word))
	   (optional (string= word "*"))
	   control defparam param action special)
      ;; remove whitespace and reread control word,
      ;; if get '*' as control
      (when optional
	(and (eq (following-char) ?\ ) (delete-char 1))
	(rtf-read "[\r\n]+")
	(delete-char 1)
	(setq word (rtf-read-control-word)))

      ;; get control table entry
      (setq control  (cdr (assq word rtf-control-table)))
      (setq defparam (nth 0 control))
      (setq action   (nth 1 control))
      (setq special  (nth 2 control))

      ;; read parameter for control word
      (cond ((not action)
	     (setq action rtf-default-control-action)
	     (setq param (rtf-read param-regexp)))
	    (defparam
	      (setq param (or (rtf-read param-regexp) defparam))))

      (and (stringp param)
	   (setq param (string-to-number param)))
      ;; delete optional trailing whitespace,
      ;; because this is not a part of control word
      (unless special
	(and (eq (following-char) ?\ ) (delete-char 1)))

      (if (and optional (not action))
	  (rtf-with-group
	    (rtf-dispatch #'rtf-discard t))
	(and action (funcall action word param))))))

(defun rtf-open-group ()
  (rtf-push-properties rtf-char-props)
  (rtf-push-properties rtf-display-props)
  (rtf-push-properties rtf-section-props)
  (rtf-push-properties rtf-paragraph-props)
  (rtf-push-properties rtf-visible-props)
  (push (car rtf-none-unicode-bytes) rtf-none-unicode-bytes)
  (push (car rtf-coding-system-stack) rtf-coding-system-stack))

(defun rtf-close-group ()
  (rtf-pop-properties rtf-char-props)
  (rtf-pop-properties rtf-display-props)
  (rtf-pop-properties rtf-section-props)
  (rtf-pop-properties rtf-paragraph-props)
  (rtf-pop-properties rtf-visible-props)
  (pop rtf-none-unicode-bytes)
  (pop rtf-coding-system-stack))

(defun rtf-read-group (action &optional skip)
  ;; match opening brace, push properties
  (delete-char 1)
  (rtf-open-group)
  ;; read inside group, until closing brace
  (rtf-with-group
    (rtf-dispatch action skip))
  ;; match closing brace and pop some properties
  (delete-char 1)
  (rtf-close-group))

(defun rtf-dispatch (default-action &optional skip)
  (let ((c (following-char)))
    (cond ((eq c ?\r) (delete-char 1))
	  ((eq c ?\n) (delete-char 1))
	  ((eq c ?\t) (delete-char 1) (rtf-insert-tab))
	  ((eq c ?{)  (rtf-read-group default-action skip))
	  ((eq c ?})  (error "internal error!"))
	  ((eq c ?\\) (rtf-perform-control-action skip))
	  (t (funcall default-action)))))

;;;  File Encoding Interface
(add-to-list 'file-coding-system-alist
	     (cons "\\.\\(rtf\\|doc\\)\\'" #'rtf-find-file-coding-system))

;;;###autoload
(defun rtf-find-file-coding-system (arg-list)
  (if (eq (car arg-list) 'insert-file-contents)
      (save-excursion
	(goto-char (point-min))
	(let ((cs 'cp1252)
	      (limit 100))
	  (when (looking-at "{\\\\rtf")
	    (cond ((re-search-forward "\\\\mac" limit t) (setq cs 'mac-roman))
		  ((re-search-forward "\\\\pc"  limit t) (setq cs 'cp437))
		  ((re-search-forward "\\\\pca" limit t) (setq cs 'cp850))))
	  (when (re-search-forward
		 "[^\\\\]\\\\ansicpg\\([[:digit:]]+\\)" limit t)
	    (let ((cp (intern
		       (format "cp%d" (string-to-number (match-string 1))))))
	      (and (coding-system-p cp)
		   (setq cs cp))))
	  cs))))


(defun rtf-encode-non-ascii-characters (buffer)
  (let ((cs (buffer-local-value 'rtf-coding-system buffer)))
    (setq cs (cdr (assq (or cs 'ansi) rtf-coding-system-alist)))
    (while (not (eobp))
      (let* ((c (following-char))
	     (charset (or (get-char-property (point) 'charset)
			  (char-charset c))))
	(cond ((> c 127)
	       (delete-char 1)
	       (insert "\\")
	       (let ((code (encode-coding-char c cs charset)))
		 (if code
		     (insert "'" (encode-hex-string code))
		   (setq code (encode-char c 'ucs))
		   (and (> code 32767) (setq code (- code 65536)))
		   (insert (format "u%d " code)))))
	      (t (forward-char)))))))

(defun rtf-encode-all-images (buffer)
  (save-excursion
    (goto-char (point-min))
    (let ((prop nil)
	  (pos (next-single-property-change (point) 'display)))
      (while pos
	(goto-char pos)
	(setq prop (get-text-property (point) 'display))
	(cond ((or (eq (car prop) 'image)
		   (and (listp (car prop)) (eq (caar prop) 'slice)))
	       (delete-char 1)
	       (rtf-write-image prop))
	      (t (forward-char)))
	(setq pos (next-single-property-change (point) 'display))))))


;;;   Format Interface
(add-to-list 'format-alist
	     '(text/rtf "doc" "{\\\\rtf" rtf-decode rtf-encode t nil nil))

(defvar rtf-trans
  '((face (nil rtf-ann-unknown))
    (left-margin  (4  "tab"))
    (right-margin (4  ""))
    (justification (none        "")
		   (right  "flushright")
		   (left   "flushleft")
		   (full   "flushboth")
		   (center "center"))
    (unknown  (nil rtf-ann-unknown))
    ))

(defun rtf-ann-unknown (old new)
  (message "==   OLD:  %s   NEW: %s" old new)
  (cons (and old '( "*X*"))
	(and new '("*Y*"))))

(defun rtf-make-ann (interann pos)
  (if pos (format "{\\%s " interann) "}"))

(defun rtf-insert-and-eval (content &optional as-group)
  (let ((pos (point)))
    (and as-group (insert ?{))
    (insert content)
    (and as-group (insert ?}))
    (rtf-decode-region pos (point))))

(defun rtf-decode-region (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (not (eobp))
      (rtf-dispatch #'rtf-reinsert-formatted))))

(defun rtf-decode (beg end)
  (save-excursion
    (save-restriction
      (let ((text-property-default-nonsticky '((display . nil)))
	    (fill-column 78))
	(narrow-to-region beg end)
	(goto-char beg)
	(use-hard-newlines 1 'always)
	(rtf-open-page)
	(rtf-read-group #'rtf-reinsert-formatted)
	(rtf-close-page 'no-form-feed)
	;; Ignore rest of document.
	(delete-region (point) (point-max))))
    (point-max)))

(defvar rtf-default-par-formatting "\\pard")

(defun rtf-insert-properties (props)
  ;; (message "plist: %S" props)
  (insert ?,))

(defun rtf-encode-paragraph (beg end &optional formatting)
  (insert "\\par\n")
  (goto-char end)
  (forward-line -1)
  (end-of-line)
  (while (< beg (point))
    (end-of-line)
    (insert "\\line")
    (forward-line -1))
  (insert (if formatting formatting rtf-default-par-formatting) " ")
  (goto-char beg))

(defun rtf-encode (beg end origbuf)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (delete-to-left-margin)
      (unjustify-region)

      (format-replace-strings
       '(("\\" . "\\\\")
	 ("{"  . "\\{")
	 ("}"  . "\\}")
	 ("����"  . "\\bullet")))

      ;; write annotations
      (goto-char (point-min))
      (while (not (eobp))
	(rtf-insert-properties (text-properties-at (point)))
	(goto-char (or (next-property-change (point)) (point-max))))

      ;; encode all paragraphs
      (goto-char (point-max))
      (let ((pos (point)))
	(while (not (bobp))
	  (backward-paragraph)
	  (rtf-encode-paragraph (point) pos)
	  (setq pos (point))))

      ;; first of all write static data
      (goto-char (point-min))
      (rtf-write-prolog origbuf)
      (rtf-write-font-table origbuf)
      (rtf-write-file-table origbuf)
      (rtf-write-color-table origbuf)
      (rtf-write-stylesheets origbuf)
      (rtf-write-info-group  origbuf)
      (rtf-write-generator-data origbuf)
      (rtf-write-page-info origbuf)
      ;; finally insert data of on variable positions
      (rtf-write-bookmarks origbuf)
      (rtf-encode-non-ascii-characters origbuf)
      (rtf-encode-all-images origbuf)
      (goto-char (point-max))
      (insert "}")))
  (point-max))

;;;###autoload
(define-minor-mode rtf-mode
  "Minor mode for editing text/rtf files."
  :group   'rtf
  :lighter " RTF"
  (cond ((null rtf-mode))
	(t)))
(put 'rtf-mode 'permanent-local t)

;;; Font Lock Support
(defvar rtf-package-keywords
  '(("\\<rtf-\\(with\\|write-as\\)-group\\>" . 0)
    ("(\\(rtf-define-control\\)\\s+\\(\\w+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))))

(font-lock-add-keywords nil rtf-package-keywords)

(provide 'rtf)

;; eof
