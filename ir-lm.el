;;; ir-lm.el ---  Basic Mixed Language Model for Information Retrieval
					;by Andrey Kotlarski m00naticus@gmail.com

;;; Commentary:


;;; History:
;; 5.VIII.2009 - Version 1.8
;; Abstracting away file-postings
;;  structure
;; 31.VII.2009 - Version 1.7
;; Generating word processing function
;;  on the fly, thus optimizing
;;  depending on whether stop words or
;;  stemmer are loaded
;; 18.VII.2009 - Version 1.6
;; highlighting of search words
;; minor bugfixes;
;; 15.VII.2009 - Version 1.5
;; bulgarian stemmer added
;; stop-word and stemmer files are now stored in separate directories
;;  which are recursively processed
;; added stemming parameter
;; many corrections in merging
;; 14.VII.2009 - Version 1.4
;; correctly merge postings and info
;;  on load or index (no duplicates,
;;  no loading of older than index files)
;; added globs for filtering file types
;; 13.VII.2009 - Version 1.3
;; remembering encoding for individual files
;; prune non-existing files on load
;; 12.VII.2009 - Version 1.2
;; new command `ir-lm' giving a unified
;;  interface of files and commands
;; command to change lambda
;; full cleaning of data
;; minor bugfixes
;; 10.VII.2009 - Version 1.1
;; added minumim possible score for query
;;  so that irrelevant results are discarded
;; a bit of code refactoring and cleaning

;; 09.VII.2009 - Version 1.0

;;; Code:
(defconst *ir-dir*
  (eval-when-compile
    (if (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))
	"C:/ir/"
      "~/.ir/"))
  "Directory for auxiliary files.")

;; *ir-hashes* structure is ((file-path encoding time (point-in-file total-words-in-paragraph
;;   distinct-words-in-paragraph hash-of-word-counts) ...) ...)
(defvar *ir-hashes* nil "List of postings grouped in files.")
(defvar *ir-global-hash* nil "Global hash table of words and their count.")
(defvar *ir-total-count* 0 "Count of all words in index.")
(defvar *ir-words-count* 0 "Count of all distinct words in index.")
(defvar *ir-word-cache* nil "Cache of raw word -> transformation.")
(defvar *ir-stop* nil "Hash table of stop words.")
(defvar *ir-stem* nil "Hash table of stemmer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup ir-lm nil
  "Basic mixed language model for information retrieval mode."
  :group 'applications)

;;;###autoload
(defcustom ir-lm-lambda 0.5
  "Lambda parameter in the mixed language model."
  :group 'ir-lm
  :type 'number)

;;;###autoload
(defcustom ir-lm-max-results 30
  "Maximum number of search results."
  :group 'ir-lm
  :type 'integer)

;;;###autoload
(defcustom ir-lm-stem-level 1
  "Stemming level."
  :group 'ir-lm
  :type 'integer)

;;;###autoload
(defcustom ir-lm-min-words 20
  "Minimal number of words in paragraph."
  :group 'ir-lm
  :type 'integer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; *ir-hashes* selectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-getter (getter-name)
  "Create a macro for writing getters with name MAKE- GETTER-NAME -GETTER and argument GETTER-NAME."
  (let ((getter-name-str (symbol-name getter-name)))
    `(defmacro ,(intern (concat "make-" getter-name-str "-getter"))
       (name &rest body)
       ,(concat "Create a selector for `*ir-hashes*' named "
		getter-name-str "- NAME and BODY.
This selector has one argument with structure as `*ir-hashes*'
named `" getter-name-str "'.
Do not use symbol `bla-arg' in the body.")
       (let ((bla-arg ',getter-name))
	 `(defun ,(intern (concat ,getter-name-str "-"
				  (symbol-name name)))
	    (,bla-arg)
	    ,@body)))))

;; getters for file structures
(make-getter ir-file)

(make-ir-file-getter name (car ir-file))
(make-ir-file-getter encoding (cadr ir-file))
(make-ir-file-getter time (car (cddr ir-file)))
(make-ir-file-getter paragraphs (cdr (cddr ir-file)))

;; getters for paragraph structures
(make-getter ir-paragraph)

(make-ir-paragraph-getter point (car ir-paragraph))
(make-ir-paragraph-getter total-words (cadr ir-paragraph))
(make-ir-paragraph-getter distinct-words (car (cddr ir-paragraph)))
(make-ir-paragraph-getter hash (cadr (cddr ir-paragraph)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; visualisation and set-er commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-link (text cmd &optional file point underline-p encoding query)
  "Return a TEXT propertized as a link that invokes CMD when clicked.
FILE is to be opened and cursor moved to position POINT.
UNDERLINE-P determines wether text should be underlined.
If ENCODING is nil, use default encoding when loading result file.
QUERY is list of search terms."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] cmd)
    (define-key map (kbd "RET") cmd)
    (propertize text
		'keymap map
		'face (when underline-p
			'((:foreground "green") (:underline t)))
		'mouse-face 'highlight
		'rear-nonsticky t
		'read-only t
		'file file
		'point point
		'encoding encoding
		'query query)))

(defun ir-file-words (paragraphs)
  "Get total count of words for file by summing count in its PARAGRAPHS."
  (apply '+ (mapcar (lambda (sexp)
		      (ir-paragraph-total-words sexp))
		    paragraphs)))

(defun ir-list-index ()
  "List all files currently in index."
  (dolist (file *ir-hashes*)
    (let ((file-path (ir-file-name file)))
      (when (file-exists-p file-path)
	(insert "\n" (make-link file-path 'ir-lm-jump-to-result
				file-path 1 nil
				(ir-file-encoding file))
		(format " [%d]"	(ir-file-words (ir-file-paragraphs
						file))))))))

(defun ir-refresh-view ()
  "Refresh file names in current index."
  (ignore-errors
    (with-current-buffer "*Information retrieval*"
      (goto-char (point-min))
      (forward-line 14)
      (setq inhibit-read-only t)
      (let ((start (point)))
	(forward-line 5)
	(delete-region start (line-end-position)))
      (insert
       "maximum results = " (format "%d\n" ir-lm-max-results)
       "minimum number of words in paragraph = "
       (format "%d\n" ir-lm-min-words)
       "lambda = " (format "%f\n" ir-lm-lambda)
       "stemming level = " (format "%d\n" ir-lm-stem-level)
       "total words in texts = " (format "%d\n" *ir-total-count*)
       "words in index = " (format "%d" *ir-words-count*))
      (forward-line 2)
      (delete-region (point) (point-max))
      (ir-list-index)
      (setq inhibit-read-only nil)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (forward-line 3))))

(defun ir-lm-change-lambda (symbol new)
  "Set NEW value of the `lambda' parameter."
  (interactive
   (list 'ir-lm-lambda
	 (read-number "New value for lambda (0 < lambda < 1) = ")))
  (if (or (<= new 0) (>= new 1))
      (message "Incorrect value for lambda.")
    (set-default symbol new)
    (ir-refresh-view)))

(defun ir-lm-change-stem-level (symbol new)
  "Set NEW value of the stemming parameter."
  (interactive
   (list 'ir-lm-stem-level
	 (read-number "New level for stemming (> 0) = ")))
  (if (< new 1)
      (message "Incorrect value for stemming.")
    (set-default symbol new)
    (ir-refresh-view)
    (ir-load-auxiliary t)))

(defun ir-lm-change-max-results (symbol new)
  "Set NEW value for maximum number of search results."
  (interactive
   (list 'ir-lm-max-results
	 (read-number "Maximum number of search results = ")))
  (set-default symbol new)
  (ir-refresh-view))

(defun ir-lm-change-min-words (symbol new)
  "Set NEW minimum number of words for paragraph."
  (interactive
   (list 'ir-lm-min-words
	 (read-number "Minumun number of words in paragraph = ")))
  (set-default symbol new)
  (ir-refresh-view))

(defun ir-clear (&optional all)
  "Clear global hashes and reset global variables.
If ALL is non-nil - ask to clear words' cache as well."
  (interactive
   (list t))
  (setq *ir-hashes* nil
	*ir-global-hash* nil
	*ir-total-count* 0
	*ir-words-count* 0)
  (when all
    (and (or *ir-word-cache* *ir-stem* *ir-stop*)
	 (y-or-n-p "Clear auxiliary caches as well? ")
	 (setq *ir-stop* nil
	       *ir-stem* nil
	       *ir-word-cache* nil))
    (message "Index cleared.")
    (ir-refresh-view))
  (garbage-collect))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-fn (fn lst)
  "Return first item that satisfies FN in LST.  Nil if no such."
  (catch 'out
    (dolist (item lst)
      (when (funcall fn item)
	(throw 'out item)))))

(defun delete-fn (lst fn)
  "Destructively delete first element of LST for which FN is non-nil."
  (if (funcall fn (car lst))
      (cdr lst)
    (let ((prev lst)
	  (curr (cdr lst)))
      (catch 'out
	(while curr
	  (if (not (funcall fn (car curr)))
	      (setq prev curr
		    curr (cdr curr))
	    (setcdr prev (cdr curr))
	    (throw 'out nil))))
      lst)))

(defun get-next-word ()
  "Get next word \(including hyphens and carrige return\) after position."
  (when (forward-word)
    (let ((word (current-word t t)))
      (while (equal (char-to-string (following-char)) "-")
	(when (forward-word)
	  (setq word (concat word (if (equal (char-to-string
					      (following-char)) "\n")
				      ""
				    "-")
			     (current-word t t)))))
      word)))

(defmacro dowords (vars &rest body)
  "Bind VARS to consecutive words and execute BODY."
  (if (listp vars)
      `(let ,(mapcar (lambda (var)
		       `(,var (get-next-word)))
		     vars)
	 (while ,(car vars)
	   ,@body
	   (setq ,@(apply 'nconc
			  (mapcar (lambda (var)
				    `(,var (get-next-word)))
				  vars)))))
    `(let ((,vars (get-next-word)))
       (while ,vars
	 ,@body
	 (setq ,vars (get-next-word))))))

(defun replace-regex-str (word regex str)
  "In WORD replace REGEX with STR."
  (mapconcat 'identity (split-string word regex) str))

(defun glob-to-regex (glob)
  "Turn a GLOB to a reg-exp."
  (replace-regex-str
   (replace-regex-str (replace-regex-str glob "\\." "\\.")
		      "?" ".")
   "\\*" ".*"))

(defun filter-name (file-name patterns)
  "Check whether FILE-NAME is fully matched by any of the PATTERNS."
  (when patterns
    (let ((match (string-match (car patterns) file-name)))
      (or (and match (= 0 match))
	  (filter-name file-name (cdr patterns))))))

(defun maprdir (fn dir &optional file-types subdir-p)
  "Apply FN over all files in DIR and its subdirectories.
FILE-TYPES determines file name patterns for calling FN upon.
Default is all files.  If SUBDIR-P is nil,
we are in the top level directory, otherwize we are lower.
This is used when recursing, when calling, must be nil."
  (or subdir-p		 ;executed only once, in top directory
      (setq file-types (mapcar 'glob-to-regex
			       (split-string (or file-types "*")
					     nil t))))
  (dolist (file (directory-files dir))
    (let ((file-full (concat dir file)))
      (or (equal "." file)
	  (equal ".." file)
	  (if (file-directory-p file-full)
	      (maprdir fn (concat file-full "/") file-types t)
	    (when (filter-name file file-types)
	      (funcall fn file-full)))))))

(defun inc-hash-value (key h-table &optional value)
  "Increment value for KEY in H-TABLE with VALUE.
If VALUE is nil, use 1.
If KEY doesn't exist, set initial value to VALUE.
If end value of KEY is <=0, remove key.
Return new val if key is added/changed, nil if key is removed."
  (let ((end-val (+ (or value 1) (gethash key h-table 0))))
    (if (> end-val 0)
	(puthash key end-val h-table)
      (remhash key h-table))))

(defun hash-to-assoc (h-table)
  "Turn a H-TABLE to assoc-list."
  (let ((a-list nil))
    (maphash (lambda (key val)
	       (push (cons key val) a-list))
	     h-table)
    a-list))

(defun ir-pair-to-global-hash (key value)
  "Add KEY VALUE to `*ir-global-hash*' and adjust global count of words."
  (or (gethash key *ir-global-hash* nil)
      (setq *ir-words-count* (1+ *ir-words-count*)))
  (inc-hash-value key *ir-global-hash* value))

(defun ir-assoc-to-hash (a-list &optional size use-global-hash-p
				parent-hash-p)
  "Turn A-LIST to a hash-table with size SIZE.
If USE-GLOBAL-HASH-P, add to `*ir-global-hash*', return nil.
If PARENT-HASH-P, create new hash and add both to it
and `*ir-global-hash*', adjusting global counts,
return the newly created one."
  (if (not use-global-hash-p)
      (let ((h-table (make-hash-table :test 'equal :size size)))
	(if parent-hash-p
	    (dolist (cell a-list h-table)
	      (let ((key (car cell))
		    (val (cdr cell)))
		(ir-pair-to-global-hash key val)
		(inc-hash-value key h-table val)))
	  (dolist (cell a-list h-table)
	    (inc-hash-value (car cell) h-table (cdr cell)))))
    (or *ir-global-hash*		;else use global, return nil
	(setq *ir-global-hash* (make-hash-table
				:test 'equal :size size)))
    (if parent-hash-p
	(dolist (cell a-list)
	  (ir-pair-to-global-hash (car cell) (cdr cell)))
      (dolist (cell a-list)
	(inc-hash-value (car cell) *ir-global-hash* (cdr cell))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Word processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bg-stem (word)
  "Return stemmed version of WORD."
  (if (string-match "\\(.*?[аъоуеийюя]\\)\\(.*\\)" word)
      (let ((prefix (match-string-no-properties 1 word))
	    (suffix (match-string-no-properties 2 word)))
	(if (and prefix suffix)
	    (catch 'out
	      (dotimes (i (length suffix) word)
		(let ((stem-suf (gethash (substring suffix i)
					 *ir-stem* nil)))
		  (when stem-suf
		    (throw 'out (concat prefix (substring suffix 0 i)
					stem-suf))))))
	  word))
    word))

(defun ir-process-new-word (word)
  "Return processed WORD.  This is default implementation.
After loading stop-words and stemmers will be fset."
  word)

(defmacro ir-build-word-processor (&optional stop-p stem-p)
  "Build optimized word processing function.
STOP-P determines whether stop words should be checked.
STEM-P determines whether stemming should be applied."
  `(lambda (word)
     ,(if stop-p
	  `(if (gethash word *ir-stop* nil)
	       ""			;stop words are marked as ""
	     ,(if stem-p
		  '(bg-stem word)
		'word))
	(if stem-p
	    '(bg-stem word)
	  'word))))

(defun ir-get-word-processor (stop-p stem-p)
  "Return optimized word processing function.
STOP-P determines whether stop words should be checked.
STEM-P determines whether stemming should be applied."
  (cond
   ((and stop-p stem-p) (ir-build-word-processor t t))
   (stop-p (ir-build-word-processor t))
   (stem-p (ir-build-word-processor nil t))
   (t (ir-build-word-processor))))

(defun ir-process-word (word)
  "Return hashed processed value for WORD.
If no such is found, process and cache."
  (let ((hash-check (gethash word *ir-word-cache* nil)))
    (or hash-check
	(setq hash-check (puthash word (ir-process-new-word word)
				  *ir-word-cache*)))
    (unless (equal "" hash-check) hash-check))) ;if not a stop word

(defun ir-load-stop-words (file)
  "Load stop-words from FILE to the global hash *ir-stop*."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (dowords word (puthash word "1" *ir-stop*))))

;; (defun ir-load-stemmer-bg (file)		;freezes compilation
;;   "Load stem entries from FILE to the global hash *ir-stem*."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (goto-char (point-min))
;;     (dowords (w1 w2 w3)		;does not byte compile!
;; 	     (when w3
;; 	       (setq w3 (car (read-from-string w3)))
;; 	       (and (numberp w3)
;; 		    (>= w3 ir-lm-stem-level)
;; 		    (puthash w1 w2 *ir-stem*))))))

(defun ir-load-stemmer-bg (file)
  "Load stem entries from FILE to the global hash *ir-stem*."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (dowords w1
	     (let ((w2 (get-next-word))
		   (w3 (get-next-word)))
	       (when w3
		 (setq w3 (car (read-from-string w3)))
		 (and (numberp w3)
		      (>= w3 ir-lm-stem-level)
		      (puthash w1 w2 *ir-stem*)))))))

(defun ir-load-auxiliary (&optional force)
  "Load auxiliary files to hashes if not already done.
When FORCE is non-nil, re-fill."
  (message "Loading auxiliary hashes...")
  (let ((stop-dir (concat *ir-dir* "stop-words/")))
    (when (and (file-exists-p stop-dir)
	       (or force (null *ir-stop*)))
      (setq *ir-stop* (make-hash-table :test 'equal :size 1000))
      (maprdir 'ir-load-stop-words stop-dir)))
  (let ((stem-dir-bg (concat *ir-dir* "stem-rules/bg/")))
    (when (and (file-exists-p stem-dir-bg)
	       (or force (null *ir-stem*)))
      (setq *ir-stem* (make-hash-table :test 'equal :size 130514))
      (maprdir 'ir-load-stemmer-bg stem-dir-bg)))
  (fset 'ir-process-new-word (ir-get-word-processor *ir-stop*
						    *ir-stem*))
  (message "Auxiliary hashes loaded."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; File processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assess-paragraph ()
  "Remove a bit of boiler-plate from `ir-lm-extract-words'."
  '(if (>= paragraph-total-count ir-lm-min-words)
       (push (list paragraph-start paragraph-total-count
		   paragraph-words-count paragraph)
	     acc)
     (setq *ir-total-count*	   ;if paragraph is too short, discard
	   (- *ir-total-count* paragraph-total-count))
     (maphash (lambda (wrd cnt)		;and remove word counts
		(or (inc-hash-value wrd *ir-global-hash* (- cnt))
		    (setq *ir-words-count* (1- *ir-words-count*))))
	      paragraph)))

(defun ir-lm-extract-words (full-file-name &optional encoding)
  "Process paragraphs of current buffer holding FULL-FILE-NAME.
Save ENCODING for further operations."
  (let ((prev (point-min))
	(acc (list (current-time) encoding full-file-name)))
    (let ((paragraph-start prev)
	  (paragraph-total-count 0)
	  (paragraph-words-count 0)
	  (paragraph (make-hash-table :test 'equal)))
      (goto-char prev)
      (dowords word
	       (setq word (ir-process-word (downcase word)))
	       (let ((curr (line-beginning-position)))
		 (when (string-match "\n.*\n" ;detect just ended paragraph
				     (buffer-substring-no-properties
				      prev curr))
		   (assess-paragraph)
		   (setq paragraph (make-hash-table :test 'equal)
			 paragraph-total-count 0
			 paragraph-words-count 0
			 paragraph-start curr))
		 (when word
		   (setq paragraph-total-count
			 (1+ paragraph-total-count)
			 *ir-total-count* (1+ *ir-total-count*))
		   (when (= 1 (inc-hash-value word paragraph)) ;new paragraph word
		     (setq paragraph-words-count
			   (1+ paragraph-words-count)))
		   (when (= 1 (inc-hash-value word *ir-global-hash*)) ;new global word
		     (setq *ir-words-count* (1+ *ir-words-count*))))
		 (setq prev curr)))
      (kill-buffer (current-buffer))
      (assess-paragraph))
    (when acc (push (nreverse acc) *ir-hashes*))))

(defun ir-remove-post (post &optional save-globals-p)
  "Subtract from global words hash key-values corresponding in POST.
SAVE-GLOBALS-P determines whether global indexes shouldn't be touched."
  (setq *ir-total-count* (- *ir-total-count*
			    (ir-paragraph-total-words post)))
  (if save-globals-p
      (maphash (lambda (key val)
		 (inc-hash-value key *ir-global-hash* (- val)))
	       (ir-paragraph-hash post))
    (maphash (lambda (key val)
	       (or (inc-hash-value key *ir-global-hash* (- val))
		   (setq *ir-words-count* (1- *ir-words-count*))))
	     (ir-paragraph-hash post))))

(defun ir-remove-postings (file &optional save-globals-p)
  "Clean all info for FILE in hashes.
SAVE-GLOBALS-P determines whether global indexes shouldn't be touched."
  (let ((file-posts (ir-file-paragraphs
		     (find-fn (lambda (post)
				(equal file (ir-file-name post)))
			      *ir-hashes*))))
    (dolist (post file-posts)
      (ir-remove-post post save-globals-p))
    (setq *ir-hashes* (delete-fn *ir-hashes*
				 (lambda (file-post)
				   (equal file (ir-file-name
						file-post)))))))

(defun ir-lm-process-paragraphs (file &optional encoding)
  "Load FILE to temp buffer and process its words.
If ENCODING is nil, use default encoding when loading FILE."
  (ir-remove-postings file)
  (with-temp-buffer
    (let ((coding-system-for-read encoding))
      (insert-file-contents file))
    (ir-lm-extract-words file encoding)))

(defun print-posting (lst)
  "Get printed representation for posting for paragraph LST."
  (princ "\n" (current-buffer))
  (prin1 (nconc (list (ir-file-name lst) (ir-file-encoding lst)
		      (ir-file-time lst))
		(mapcar (lambda (sublst)
			  (nconc
			   (list (ir-paragraph-point sublst)
				 (ir-paragraph-total-words sublst)
				 (ir-paragraph-distinct-words sublst))
			   (hash-to-assoc (ir-paragraph-hash
					   sublst))))
			(ir-file-paragraphs lst)))
	 (current-buffer)))

(defun ir-lm-write-index (file)
  "Write current index info to FILE."
  (interactive
   (list (read-file-name "Index file: " nil ".irlm" nil ".irlm")))
  (message "Writing...")
  (with-temp-file file
    (prin1 (nconc (list *ir-total-count* *ir-words-count*) ;firstly write the global hash
		  (hash-to-assoc *ir-global-hash*))
	   (current-buffer))
    (mapc 'print-posting *ir-hashes*))	;write all postings
  (message "Index written."))

;;;###autoload
(defun ir-lm-index (dir &optional file-types encoding append-p)
  "Recursivelly process directory DIR and index all files.
FILE-TYPES determines file name patterns for indexing.
If ENCODING is nil, use default \(utf-8\) encoding for files.
If APPEND-P is non-nil, merge to the current index."
  (interactive
   (list
    (read-directory-name "Top directory: " nil default-directory t)
    (read-string "File names to be indexed: " "*.txt" nil "*.txt")
    (unless (y-or-n-p "Use default encoding? ")
      (read-coding-system "Choose encoding: " 'cp1251))
    (when *ir-global-hash*
      (y-or-n-p "Add to existing configuration? "))))
  (or *ir-global-hash*
      (setq append-p nil))
  (or *ir-word-cache*
      (setq *ir-word-cache* (make-hash-table :test 'equal)))
  (unless append-p
    (ir-clear)
    (setq *ir-global-hash* (make-hash-table :test 'equal)))
  (ir-load-auxiliary)
  (message "Indexing...")
  (maprdir (lambda (file)
	     (ir-lm-process-paragraphs file encoding))
	   dir file-types)
  (message "Files successfully indexed.")
  (ir-refresh-view))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load existing index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ir-lm-get-file-posting (post &optional inc-globals-p)
  "Convert file saved POST info to actually used structures.
INC-GLOBALS-P determines whether global word counts should be adjusted."
  (nconc (list (ir-file-name post) (ir-file-encoding post)
	       (ir-file-time post))
	 (mapcar (lambda (subpost)
		   (let ((total-words (ir-paragraph-total-words
				       subpost))
			 (index-words (ir-paragraph-distinct-words
				       subpost)))
		     (when inc-globals-p
		       (setq *ir-total-count*
			     (+ *ir-total-count* total-words)))
		     (list (ir-paragraph-point subpost)
			   total-words index-words
			   (ir-assoc-to-hash (cdr (cddr subpost))
					     index-words nil
					     inc-globals-p))))
		 (ir-file-paragraphs post))))

(defun ir-lm-load-file-posting (post &optional inc-globals-p)
  "Get file saved POST.  If newer posting already exists, discard.
INC-GLOBALS-P determines whether global word counts should be adjusted."
  (let* ((file-path (ir-file-name post))
	 (existing-file-time
	  (ir-file-time (find-fn (lambda (post)
				   (equal file-path
					  (ir-file-name post)))
				 *ir-hashes*))))
    (if existing-file-time	    ;check if file is already in index
	(if (file-exists-p file-path)
	    (when (time-less-p existing-file-time
			       (ir-file-time post)) ;if post is newer
	      (ir-remove-postings file-path (not inc-globals-p)) ;remove old posting from *ir-hashes*
	      (ir-lm-get-file-posting post inc-globals-p))
	  ;;discard posting and remove existing from *ir-hashes*
	  (ir-remove-postings file-path (not inc-globals-p)) ;housekeeping
	  nil)
      (when (file-exists-p file-path)	;load only existing files
	(ir-lm-get-file-posting post inc-globals-p)))))

(defun ir-lm-load-index-from-file (file)
  "Load existing index from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((dont-inc-globals-p (null *ir-global-hash*)))
      (when dont-inc-globals-p ;need global hash from file only if current is cleared
	(let ((global-hash (read-from-whole-string
			    (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position)))))
	  (setq *ir-total-count* (car global-hash)
		*ir-words-count* (cadr global-hash))
	  (ir-assoc-to-hash (cddr global-hash) *ir-words-count* t)))
      (let ((point-max (point-max)))
	(while (and (= 0 (forward-line 1))
		    (< (point) point-max))
	  (let ((file-sexp (ir-lm-load-file-posting
			    (read-from-whole-string
			     (buffer-substring-no-properties
			      (line-beginning-position)
			      (line-end-position)))
			    (not dont-inc-globals-p))))
	    (when file-sexp (push file-sexp *ir-hashes*))))))
    (kill-buffer (current-buffer))))

;;;###autoload
(defun ir-lm-load-index (file &optional append-p)
  "Load existing index FILE.
If APPEND-P is non-nil, keep previous index loaded as well."
  (interactive
   (list (read-file-name "Index file: " nil ".irlm" nil ".irlm")
	 (when *ir-global-hash*
	   (y-or-n-p
	    "Add to existing configuration or overwrite? "))))
  (when (file-exists-p file)
    (or (and *ir-global-hash* append-p)
	(ir-clear))
    (ir-load-auxiliary)
    (message "Loading...")
    (ir-lm-load-index-from-file file)
    (message "Index loaded.")
    (ir-refresh-view)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ir-lm-posting-score (hash base words &optional lambda)
  "Get score from paragraph represented as HASH.
BASE is the total number of words in the paragraph.
WORDS is list of words in query.
LAMBDA is LM parameter between 0 and 1."
  (or lambda
      (setq lambda 0.5))
  (let ((result
	 (apply '*
		(mapcar (lambda (word)
			  (let ((global-count
				 (gethash word *ir-global-hash* 0)))
			    (if (> global-count 0)
				(+ (* lambda
				      (/ (float (gethash word hash 0))
					 base))
				   (* (- 1 lambda)
				      (/ (float global-count)
					 *ir-total-count*)))
			      1)))
			words))))
    (if (= result 1) 0 result)))

(defun ir-lm-posting-min-score (words &optional lambda)
  "Get minimum score possible for a paragraph.
WORDS is list of words in query.
LAMBDA is LM parameter between 0 and 1."
  (or lambda (setq lambda 0.5))
  (apply '* (mapcar (lambda (word)
		      (let ((global-count
			     (gethash word *ir-global-hash* 0)))
			(if (> global-count 0)
			    (* (- 1 lambda)
			       (/ (float global-count)
				  *ir-total-count*))
			  1)))
		    words)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ir-lm-insert-post (new best cnt)
  "Insert NEW post based on score into BEST array with CNT elements."
  (let ((new-val (aref new 0))
	(place (1+ cnt)))
    (when (> new-val 0)
      (while (and (>= place 1)
		  (> new-val (aref (aref best (1- place)) 0)))
	(setq place (1- place)))
      (while (> cnt place)
	(aset best cnt (aref best (1- cnt)))
	(setq cnt (1- cnt)))
      (when (>= cnt place) (aset best place new))))
  best)

(defun ir-lm-get-best-scores (query cnt)
  "For QUERY which is list of search terms find best CNT results.
Return vector of vectors with info for best paragraphs."
  (let ((best (make-vector cnt [0 "" -1 nil nil]))
	(min-score (ir-lm-posting-min-score query ir-lm-lambda)))
    (dolist (file *ir-hashes*)
      (let ((file-path (ir-file-name file)))
	(when (file-exists-p file-path)
	  (dolist (post (ir-file-paragraphs file))
	    (let ((score
		   (ir-lm-posting-score (ir-paragraph-hash post)
					(ir-paragraph-total-words
					 post)
					query ir-lm-lambda)))
	      (when (> score min-score)
		(setq best (ir-lm-insert-post
			    (vector score file-path
				    (ir-paragraph-point post)
				    (ir-file-encoding file))
			    best (1- cnt)))))))))
    best))

(defun highlight-search (pos query)
  "Highlight words from POS on to the end of paragraph corresponding to QUERY."
  (catch 'out
    (let ((prev pos))
      (dowords word
	       (let ((curr (point)))
		 (when (string-match "\n.*\n" ;detect just ended paragraph
				     (buffer-substring-no-properties
				      prev curr))
		   (throw 'out nil))
		 (when (member (ir-process-word (downcase word))
			       query)
		   (delete-char (- (length word)))
		   (insert
		    (propertize word
				'face '((:foreground "green")))))
		 (setq prev curr))))))

(defun ir-lm-jump-to-result (file pos &optional encoding query)
  "Open FILE and go to particular position POS.
If ENCODING is nil, use default encoding when loading result file.
QUERY is list of current search terms."
  (interactive
   (let ((point (point)))
     (list (get-text-property point 'file)
	   (get-text-property point 'point)
	   (get-text-property point 'encoding)
	   (get-text-property point 'query))))
  (let ((jump-buffer (generate-new-buffer
		      (car (nreverse (split-string file "/"))))))
    (set-buffer jump-buffer)
    (let ((coding-system-for-read encoding))
      (insert-file-contents file))
    (goto-char pos)
    (when query				;highlight search terms
      (highlight-search pos query)
      (goto-char pos))
    (switch-to-buffer jump-buffer)))

(defun ir-lm-insert-results (best query)
  "Insert in current buffer BEST results.
QUERY is list of current search terms."
  (catch 'end-results
    (mapc (lambda (post)
	    (let ((score (aref post 0))
		  (file (aref post 1))
		  (marker (aref post 2))
		  (encoding (aref post 3))
		  (preview ""))
	      (if (<= score 0)
		  (throw 'end-results nil) ;premature end of meaningful results
		(insert "\n")
		(insert (make-link (car (nreverse
					 (split-string file "/")))
				   'ir-lm-jump-to-result file marker t
				   encoding query))
		(insert (format " [%f]" (* score 1000000)))
		(when (number-or-marker-p marker)
		  (with-temp-buffer
		    (let ((coding-system-for-read encoding))
		      (insert-file-contents file))
		    (goto-char marker)
		    (setq preview
			  (buffer-substring-no-properties marker
							  (line-end-position)))
		    (kill-buffer (current-buffer)))
		  (insert "\n")
		  (insert (make-link preview 'ir-lm-jump-to-result
				     file marker nil encoding
				     query))))))
	  best)))

(defun ir-lm-search (query-str &optional cnt)
  "For QUERY-STR find best CNT results."
  (interactive
   (list (read-string "Search for: " nil t) nil))
  (or cnt (setq cnt ir-lm-max-results))
  (if (null *ir-global-hash*)
      (message "No index loaded.")
    (or *ir-word-cache*
	(setq *ir-word-cache* (make-hash-table :test 'equal)))
    (let ((results (generate-new-buffer "*Search results*")))
      (set-buffer results)
      (local-set-key (kbd "<M-down>")
		     (lambda () (interactive) (forward-line 2)))
      (local-set-key (kbd "<M-up>")
		     (lambda () (interactive) (forward-line -2)))
      (local-set-key "q" (lambda () (interactive) (kill-buffer)))
      (switch-to-buffer results)
      (insert "Results for: " query-str)
      (let ((query (delete nil
			   (mapcar (lambda (word)
				     (ir-process-word
				      (downcase word)))
				   (split-string query-str)))))
	(ir-lm-insert-results (ir-lm-get-best-scores query cnt)
			      query))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (forward-line))
    (ignore-errors (kill-buffer "*Quail Completions*"))
    (message (concat "Results for: " query-str))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Visualisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ir-lm-set-keys ()
  "Set key bindings in the IR buffer."
  (local-set-key "i" 'ir-lm-index)
  (local-set-key "l" 'ir-lm-load-index)
  (local-set-key "w" 'ir-lm-write-index)
  (local-set-key "f" 'ir-lm-search)
  (local-set-key "c" 'ir-clear)
  (local-set-key "m" 'ir-lm-change-max-results)
  (local-set-key "p" 'ir-lm-change-min-words)
  (local-set-key "b" 'ir-lm-change-lambda)
  (local-set-key "s" 'ir-lm-change-stem-level)
  (local-set-key "q" (lambda () (interactive) (kill-buffer)))
  (local-set-key "r" (lambda () (interactive) (ir-refresh-view))))

;;;###autoload
(defun ir-lm ()
  "Create buffer with information and shortcuts."
  (interactive)
  (let ((ir-buffer (get-buffer-create "*Information retrieval*")))
    (set-buffer ir-buffer)
    (switch-to-buffer ir-buffer)
    (insert
     (propertize "Information Retrieval - Basic Mixed Language Model"
		 'face '((:foreground "green") (:underline t)))
     "\n\nOptions:\n"
     (make-link "i -> index new directory"
		'ir-lm-index)
     "\n"
     (make-link "l -> load existing index from file"
		'ir-lm-load-index)
     "\n"
     (make-link "w -> write current index\(es\) to file"
		'ir-lm-write-index)
     "\n"
     (make-link "f -> search in current loaded index\(es\)"
		'ir-lm-search)
     "\n"
     (make-link "c -> clear current index\(es\)"
		'ir-clear)
     "\n"
     (make-link "m -> change maximum search results"
		'ir-lm-change-max-results)
     "\n"
     (make-link "p -> change minimum number of words in paragraph"
		'ir-lm-change-min-words)
     "\n"
     (make-link "b -> change lambda"
		'ir-lm-change-lambda)
     "\n"
     (make-link "s -> change stemming level"
		'ir-lm-change-stem-level)
     "\n"
     (make-link "q -> quit \(without clearing\)"
		(lambda () (interactive) (kill-buffer)))
     "\n\n"
     "maximum results = " (format "%d\n" ir-lm-max-results)
     "minimum number of words in paragraph = "
     (format "%d\n" ir-lm-min-words)
     "lambda = " (format "%f\n" ir-lm-lambda)
     "stemming level = " (format "%d\n" ir-lm-stem-level)
     "total words in texts = " (format "%d\n" *ir-total-count*)
     "words in index = " (format "%d\n" *ir-words-count*)
     "Currently indexed files [total words]:\n")
    (ir-lm-set-keys)
    (ir-list-index)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (forward-line 3)))


(provide 'ir-lm)

;;; ir-lm.el ends here
