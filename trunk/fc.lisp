(defpackage :feedcircuit
  (:use :common-lisp)
  (:export :sync :new-ebook :list-ebooks))

(in-package :feedcircuit)

(defmacro printing-errors (on-error &rest forms)
  (let ((tag (gensym)))
    `(block ,tag
            (handler-bind
              ((error #'(lambda (c) (format t "~a~%" c)
                          #+sbcl (format t "~a~%" (sb-debug:backtrace))
                          (return-from ,tag ,on-error))))
              ,@forms))))

(defun base-36 (value) (let ((*print-base* 36)) (string-downcase (write-to-string value))))

(defparameter *uid-base*
  (base-36 (- (get-universal-time) (encode-universal-time 0 0 11 7 2 2011))))

(defparameter *uid-counter* 0)

(defun new-uid () (format nil "~a-~a" *uid-base* (base-36 (incf *uid-counter*))))

(defun lhtml-collect-text (html)
  (if (consp html)
    (format nil "~{~a~^ ~}"
            (remove nil (loop for i in (cddr html) collect (lhtml-collect-text i))))
    (if (stringp html) html)))

(defun lhtml-process (html fn)
  (let ((html (funcall fn html)))
    (if (consp html)
      (append (list (first html) (second html))
              (loop for node in (cddr html)
                    when (lhtml-process node fn) collect it))
      html)))

(defun lhtml-find-all (html findp)
  (append
    (if (consp html) (loop for i in (cddr html) append (lhtml-find-all i findp)))
    (if (funcall findp html) (list html))))

(defun lhtml-find-first (html findp)
  (if (funcall findp html)
    html
    (if (consp html) (loop for i in (cddr html) thereis (lhtml-find-first i findp))))) 

(defun lhtml-test-tag (elem tag) (and (consp elem) (eq (car elem) tag)))

(defun lhtml-get-tag-p (tag) #'(lambda (x) (lhtml-test-tag x tag)))

(defun lhtml-find-tag (html tag) (lhtml-find-first html (lhtml-get-tag-p tag)))

(defun lhtml-get-link-p (names)
  #'(lambda (x)
      (if (lhtml-test-tag x :a)
        (let ((str (lhtml-collect-text x)))
          (loop for name in names thereis (search name str))))))

(defun lhtml-get-attr (tag attr)
  (loop for a in (second tag) thereis (and (eq (first a) attr) a)))

(defun lhtml-get-attr-value (tag attr) (second (lhtml-get-attr tag attr)))

(defun lhtml-test-tag-and-attr (html tag attr attr-val)
  (and (lhtml-test-tag html tag) (equalp (lhtml-get-attr-value html attr) attr-val)))

(defun lhtml-find-tag-and-attr (html tag attr attr-val)
  (lhtml-find-first html #'(lambda (x) (lhtml-test-tag-and-attr x tag attr attr-val))))

(defun lhtml-set-attr (tag attr attr-val)
  (let ((new-attrs
          (loop for (n v) in (second tag) when (not (eq n attr)) collect (list n v))))
    (append (list (first tag) (append new-attrs (list (list attr attr-val)))) (cddr tag))))

(defun lhtml-update-attr (tag attr fn)
  (append (list (first tag)
                (loop for (n v) in (second tag)
                      collect (list n (if (eq n attr) (funcall fn v) v))))
          (cddr tag)))

(defun lhtml-delete-duplicated-attrs (tag)
  (if (consp tag)
    (setf (second tag)
          (delete-duplicates (second tag) :test #'(lambda (x y) (equalp (first x) (first y)))))) 
  tag)

(defmacro lhtml-apply (html tag &rest ops)
  (let ((conds 
          (loop for (cnd op) in ops
                when (keywordp cnd)
                  collect `((lhtml-test-tag ,tag ,cnd) ,op)
                when (and (consp cnd) (consp (second cnd)))
                  collect `((lhtml-test-tag-and-attr ,tag ,(car cnd) ,(caadr cnd) ,(cadadr cnd)) ,op)
                when (and (consp cnd) (not (consp (second cnd)))) 
                  collect `((and (consp ,tag) (find (first ,tag) (list ,@cnd))) ,op))))
    `(lhtml-process ,html #'(lambda (,tag) (cond ,@conds (t ,tag))))))

(defun lhtml-calc-criterion (tag self)
  (loop for i in (cddr tag)
        when (stringp i) sum (length i)
        when (listp i) sum (/ (funcall self i self) 2)))

(defun lhtml-detect-content (html)
  (let ((result (cons nil 0)))
    (flet ((fn (tag self)
               (if (find (car tag) '(:a :script))
                 0
                 (let ((cr (lhtml-calc-criterion tag self)))
                   (if (> cr (cdr result)) (setf result (cons tag cr)))
                   cr))))
      (fn html #'fn)
      (car result))))

(defparameter *uri-octets-to-escape*
  (let* ((lst '(#\< #\> #\" #\space #\{ #\} #\| #\\ #\^ #\[ #\] #\` #\Rubout))
         (table (make-hash-table :size (length lst))))
    (dolist (ch lst) (setf (gethash (char-code ch) table) t))
    table))

(defun escape-uri-octet-p (octet)
  (or (< octet #x20) (> octet #x7F) (gethash octet *uri-octets-to-escape*)))

(defun escape-uri (uri)
  (with-output-to-string (str)
    (loop for octet across (babel:string-to-octets uri :encoding :utf-8) do
          (if (escape-uri-octet-p octet)
            (format str "%~2,'0x" octet)
            (write-char (code-char octet) str)))))

(defun norm-uri (uri) (escape-uri (string-trim '(#\Space #\Tab #\Newline) uri)))

(defun abs-uri (uri base-uri)
  (handler-case
    (puri:render-uri (puri:merge-uris (norm-uri uri) (norm-uri base-uri)) nil)
    (error () uri)))

(defun lhtml-find-link (html names)
  (if names (lhtml-get-attr-value (lhtml-find-first html (lhtml-get-link-p names)) :href))) 

(defun preprocess-content (html base-uri &optional strip-images)
  (let ((doc-base-uri 
          (or (lhtml-get-attr-value (lhtml-find-tag html :base) :href) base-uri))
        (html (lhtml-process html #'lhtml-delete-duplicated-attrs)))
    (flet ((fn (u) (abs-uri u doc-base-uri)))
      (lhtml-apply html tag
                   ((:script :object :applet :embed) nil)
                   (:a (lhtml-update-attr tag :href #'fn))
                   (:img (if (not strip-images) (lhtml-update-attr tag :src #'fn)))
                   (:form (lhtml-update-attr tag :action #'fn))
                   ((:iframe :input) (lhtml-update-attr tag :src #'fn))))))

(defun download (uri &key referer bin)
  (format t "downloading ~a~%" uri)
  (let ((headers-out (if referer (list (cons "Referer" referer))))) 
    (multiple-value-bind (body s h-in real-uri)
      (drakma:http-request uri :additional-headers headers-out :force-binary bin
                           :redirect t :auto-referer t :external-format-in nil)
      (declare (ignore s h-in))
      (values body (puri:render-uri real-uri nil)))))

(defun download-html (uri &optional referer) 
  (multiple-value-bind (html base-uri) (download uri :referer referer)
    (list (preprocess-content (chtml:parse html (chtml:make-lhtml-builder)) base-uri) base-uri))) 

(defun follow-link (html&uri names)
  (let ((uri (lhtml-find-link (first html&uri) names)))
    (if uri (download-html uri (second html&uri)))))

(defparameter *content-tag* nil)

(defun extract-content (html&uri) 
  (list
    (cond 
      (*content-tag* 
        (destructuring-bind (ctag cattr cval) *content-tag*
          (lhtml-find-first 
            (first html&uri) 
            #'(lambda (tag) (lhtml-test-tag-and-attr tag ctag cattr cval)))))
      (t (lhtml-detect-content (first html&uri))))
    (second html&uri)))

(defun create-page (template title body)
  (lhtml-apply 
    (lhtml-apply template tag (:title nil)) tag
    (:body (append tag body))
    (:head (append tag
                   '((:meta ((:http-equiv "Content-Type") (:content "text/html;charset=utf-8"))))
                   (if title (list (list :title nil title))))))) 

(defparameter *print-version-strs* '("Print version" "Версия для печати"))

(defparameter *single-page-strs* '("Single page" "Одной страницей"))

(defparameter *next-page-strs* '("Next page" "Следующая страница"))

(defun select-content (uri)
  (let ((html&uri (download-html uri)))
    (list 
      (lhtml-collect-text (lhtml-find-tag (first html&uri) :title))
      (cond
        ((follow-link html&uri *print-version-strs*))
        ((let ((html&uri (follow-link html&uri *single-page-strs*))) 
           (if html&uri (extract-content html&uri))))
        ((loop for page = html&uri then
               (follow-link page *next-page-strs*)
               while page collect (extract-content page))))))) 

(defparameter *cache* (make-hash-table :test #'equal))

(defparameter *root-dir* (make-pathname))

(defparameter *cache-dir* ".cache")

(defun make-path (name type &optional cache-id)
  (let ((dir (if cache-id (list :relative *cache-dir* cache-id))))
    (merge-pathnames (make-pathname :name name :type type :directory dir) *root-dir*)))

(defun make-local-uri (path rel)
  (if rel (format nil "~a/~a/~a.~a" *cache-dir* rel (pathname-name path) (pathname-type path))
          (format nil "~a.~a" (pathname-name path) (pathname-type path))))

(defun queue-uri (uri referrer cache-id)
  (let ((cached (gethash uri *cache*)))
    (car (or cached
             (setf (gethash uri *cache*)
                   (list (make-path (new-uid) (pathname-type (puri:uri-path (puri:parse-uri uri))) cache-id)
                         referrer))))))

(defun download-queued ()
  (flet ((mapfn (k v)
                (when (> (length v) 1)
                  (printing-errors nil
                    (let ((res (download k :referer (second v))))
                      (with-open-file (f (ensure-directories-exist (first v))
                                         :direction :output
                                         :if-exists :supersede
                                         :element-type '(unsigned-byte 8))
                        (write-sequence res f)))
                    (setf (gethash k *cache*) (list (car v)))))))
    (maphash #'mapfn *cache*)))

(defparameter *attrs-to-cache* '(:img :src :input :src))

(defun process-block (bl referrer cache-id &optional rel)
  (let ((bl (if (eq (car bl) :html) (lhtml-find-tag bl :body) bl)))
    (flet ((fn (u) (make-local-uri (queue-uri u referrer cache-id) rel)))
      (let ((bl (lhtml-apply bl tag ((:img :input) (lhtml-update-attr tag :src #'fn)))))
        (cond
          ((find (first bl) '(:table :dir :menu :dl :ul :ol)) (list bl))
          ((find (first bl) '(:tr :caption :colgroup :thead :tfoot :tbody)) (list (append '(:table nil) bl)))
          (t (cddr bl)))))))

(defvar *page-template*)

(defun process-content (uri cache-id) 
  (destructuring-bind (title blocks) (select-content uri)
    (let ((blocks (if (keywordp (caar blocks)) (list blocks) blocks)))
      (values
        (create-page
          *page-template*
          title
          (loop for (bl uri) in blocks append (process-block bl uri cache-id)))
        title)))) 

(defparameter *xhtml* nil)

(defun lhtml-save (html filepath)
  (flet ((make-sink (file) 
                    (if *xhtml* (cxml:make-octet-stream-sink file :encoding "utf-8" :indentation 2)
                                (chtml:make-octet-stream-sink file :encoding "utf-8"))))
    (with-open-file (f (ensure-directories-exist filepath) 
                       :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (chtml:serialize-lhtml html (make-sink f)))))

(defun cache-content (uri cache-id)
  (let ((filepath (make-path (new-uid) "html" cache-id)))
    (multiple-value-bind (content title) (process-content uri cache-id)
      (lhtml-save content filepath) 
      (download-queued) 
      (values filepath title))))

(defun fix-timezone (str)
  "Makes time string HTTP compatible so drakma could parse it."
  (let ((tail (subseq str (- (length str) 5))))
    (if (find (char tail 0) '(#\+ #\-))
      (if (loop for ch across (subseq tail 1) always (digit-char-p ch))
        (concatenate 'string (subseq str 0 (- (length str) 5))
                     "GMT" (subseq tail 0 3) ":" (subseq tail 3))))))

(defun parse-date-time-str (str)
  "Parses either rfc 1123 or rfc 3339 string"
  (handler-case
    (local-time:timestamp-to-universal (local-time:parse-timestring str))
    (error () (let ((fixed (or (fix-timezone str) str))) (drakma:parse-cookie-date fixed)))))

(defstruct item title uri description date)

(defun dom-map-if (node fn pred)
  (if (dom:element-p node)
    (if (funcall pred node) 
      (list (funcall fn node))
      (loop for sub across (dom:child-nodes node)
            when (dom-map-if sub fn pred) nconc it))))

(defun dom-test-tag (node name)
  (and (dom:element-p node) (equalp (dom:tag-name node) name))) 

(defun dom-node-text (node)
  (let ((text (loop for sub across (dom:child-nodes node)
                   thereis (if (dom:text-node-p sub) sub))))
    (if text (dom:node-value text))))

(defun feed-parse-item (node base-uri)
  (apply #'make-item 
           (loop for sub across (dom:child-nodes node) when
                 (or
                   (if (dom-test-tag sub "title")
                     (list :title (dom-node-text sub)))
                   (if (dom-test-tag sub "description")
                     (list :description (dom-node-text sub)))
                   (if (dom-test-tag sub "content")
                     (list :description (dom-node-text sub)))
                   (if (dom-test-tag sub "pubdate")
                     (list :date (parse-date-time-str (dom-node-text sub))))
                   (if (dom-test-tag sub "published")
                     (list :date (parse-date-time-str (dom-node-text sub))))
                   (if (and (dom-test-tag sub "link") (dom:has-attribute sub "href")) 
                     (list :uri (abs-uri (dom:get-attribute sub "href") base-uri))) 
                   (if (dom-test-tag sub "link")
                     (list :uri (abs-uri (dom-node-text sub) base-uri)))) append it)))

(defun download-feed (uri)
  (multiple-value-bind (body real-uri) (download uri :bin t)
    (list 
      (nreverse (dom-map-if 
        (dom:document-element (cxml:parse body (cxml-dom:make-dom-builder))) 
        #'(lambda (node) (feed-parse-item node real-uri))
        #'(lambda (node) (or (equalp (dom:tag-name node) "item")
                             (equalp (dom:tag-name node) "entry"))))) real-uri)))

(defvar *item-template*)

(defun create-item (title body uri id)
  (let ((attrs (append (if id (list (list :id id))) '((:class "feedcircuit-item")))))
    (let ((template (append (list :div attrs) *item-template*)))
      (lhtml-apply template tag
                   ((:a (:class "feedcircuit-item-link")) (append (lhtml-set-attr tag :href uri) (list title)))
                   ((:div (:class "feedcircuit-item-body")) (append tag body))))))

(defun process-item (item base-uri cache cache-id store-ids strip-feed-ads) 
  (format t "Processing ~a~%" (item-title item))
  (printing-errors nil 
    (let* ((uri (item-uri item))
           (local-uri (if cache (make-local-uri (cache-content uri cache-id) cache-id) uri))
           (body (process-block (preprocess-content (chtml:parse
                                                      (or (item-description item) "") 
                                                      (chtml:make-lhtml-builder)) 
                                                    base-uri strip-feed-ads)
                                base-uri cache-id cache-id)))
      (download-queued)
      (create-item (item-title item) body local-uri (if store-ids uri)))))

(defun test-item (item test-time inc exc) 
  (and (or (null (item-date item)) (null test-time) (> (item-date item) test-time))
       (or (null inc) (find-if #'(lambda (s) (or (search s (item-title item))
                                                 (search s (item-uri item)))) inc))
       (or (null exc) (not (find-if #'(lambda (s) (or (search s (item-title item))
                                                      (search s (item-uri item)))) exc)))))

(defvar *feed-template*)

(defun load-feed-html (name title)
  (let ((filename (make-path name "html")))
    (if (probe-file filename)
      (chtml:parse filename (chtml:make-lhtml-builder))
      (create-page *feed-template* title nil))))

(defun extract-index (html)
  (let ((tags (lhtml-find-all html #'(lambda (x) (lhtml-test-tag-and-attr x :div :class "feedcircuit-item"))))
        (table (make-hash-table :test #'equal)))
    (loop for tag in tags do (setf (gethash (lhtml-get-attr-value tag :id) table) t))
    table))

(defun load-index (index-file feed-uri)
  (if index-file
    (let ((index (make-hash-table :test #'equal)))
      (if (probe-file index-file)
        (let ((uris (with-open-file (f index-file)
                      (second (find-if #'(lambda (x) (equal (first x) feed-uri)) (read f))))))
          (loop for uri in uris do (setf (gethash uri index) t))))
      index)))

(defun update-index (index-file feed-uri uris)
  (if index-file
    (let* ((index (if (probe-file index-file) (with-open-file (f index-file) (read f))))
           (entry (find-if #'(lambda (x) (equal (first x) feed-uri)) index)))
      (if entry (setf (second entry) uris)
                (push (list feed-uri uris) index))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (pprint index f)))))

(defun create-feed-html (html new-items sync-id del-time)
  (let ((id-attr (if del-time (list (list :id (write-to-string (list sync-id del-time)))))))
    (let ((div (append (list :div (append id-attr '((:class "feedcircuit-sync")))) new-items)))
      (push div (cddr (lhtml-find-tag html :body)))
      html)))

(defun remove-old-items (html)
  (lhtml-apply html tag
               ((:div (:class "feedcircuit-sync"))
                (let ((id-attr (lhtml-get-attr-value tag :id)))
                  (destructuring-bind (sync-id del-time) (if id-attr (read-from-string id-attr)
                                                                     (list nil nil)) 
                    (if (or (null del-time) (> del-time (get-universal-time))) 
                      tag
                      (progn (cl-fad:delete-directory-and-files
                               (make-path nil nil sync-id) :if-does-not-exist :ignore) nil))))))) 

(defparameter *default-page-template* "<html><head/><body/></html>")
(defparameter *default-item-template* "<b><a class=\"feedcircuit-item-link\"/></b><div class=\"feedcircuit-item-body\"></div><br>")

(defun load-template (name type default &key body common)
  (let ((html (chtml:parse
                (or
                  (probe-file (make-path (format nil "~a-~a-template" name type) "html"))
                  (probe-file (make-path (format nil "~a-template" type) "html"))
                  (if common (probe-file (make-path "template" "html"))) 
                  default) 
                  (chtml:make-lhtml-builder))))
    (if body (cddr (lhtml-find-tag html :body)) html)))

(defun load-templates (name)
  (list
    (load-template name "feed" *default-page-template* :common t) 
    (load-template name "page" *default-page-template* :common t)
    (load-template name "item" *default-item-template* :body t)))

(defun create-toc (title path html)
  (let ((items 
          (loop for tag in
                (lhtml-find-all html #'(lambda (x) (lhtml-test-tag-and-attr
                                                     x :div :class "feedcircuit-item")))
                collect (let ((link (lhtml-find-tag-and-attr
                                      tag :a :class "feedcircuit-item-link")))
                          (list (third link) (lhtml-get-attr-value link :href))))))
    (if items (append (list title (file-namestring path)) 
                      (remove-if #'(lambda (x) (puri:uri-host (puri:parse-uri (second x)))) items)))))

(defun cache-single-item (url work-dir)
  (printing-errors nil
    (let ((puri:*strict-parse* nil)
          (chunga:*accept-bogus-eols* t)
          (*cache* (make-hash-table :test #'equal))
          (*root-dir* (pathname work-dir))
          (*cache-dir* "")
          (*page-template* (chtml:parse *default-page-template* (chtml:make-lhtml-builder)))) 
    (multiple-value-bind (path title) (cache-content url "")
      (list title (file-namestring path))))))

(defun sync-feed (&key (name "index") (title "feedcircuit") 
                       uri cache include exclude days
                       print-version-strs no-print-version
                       single-page-strs no-single-page
                       next-page-strs no-next-page index-file strip-feed-ads) 
  (printing-errors nil
    (let ((puri:*strict-parse* nil)
          (chunga:*accept-bogus-eols* t)
          (sync-id (new-uid))
          (test-time (if days (- (get-universal-time) (* days 24 60 60))))
          (del-time (if days (+ (get-universal-time) (* days 24 60 60))))
          (*cache* (make-hash-table :test #'equal))
          (*print-version-strs* (or print-version-strs
                                    (if (not no-print-version) *print-version-strs*)))
          (*single-page-strs* (or single-page-strs
                                  (if (not no-single-page) *single-page-strs*)))
          (*next-page-strs* (or next-page-strs
                                (if (not no-next-page) *next-page-strs*))))
      (format t "Grabbing ~a~%" (or title name))
      (destructuring-bind (*feed-template* *page-template* *item-template*) (load-templates name)
        (let* ((feed-html (load-feed-html name title))
               (index (or (load-index index-file uri) (extract-index feed-html)))
               new-index)
          (destructuring-bind (items real-uri) (download-feed uri)
            (let ((path (make-path name "html"))
                  (new-items (loop for item in items
                                   when (test-item item test-time include exclude)
                                     when (gethash (item-uri item) index)
                                       collect (item-uri item) into idx 
                                     else
                                       when (process-item item real-uri cache sync-id (null index-file) strip-feed-ads)
                                         collect it and collect (item-uri item) into idx
                                     finally (setf new-index idx))))
              (let ((html (if new-items
                            (remove-old-items
                              (create-feed-html feed-html new-items sync-id del-time))
                            feed-html)))
                (when new-items (lhtml-save html path) (update-index index-file uri new-index))
                (create-toc title path html)))))))))

(defun sync (&key (root-dir "") (cache-dir *cache-dir*) index-file feeds)
  (let ((*root-dir* (pathname root-dir))
        (*cache-dir* cache-dir))
    (remove-duplicates
      (loop for feed-cfg in feeds
            when (apply #'sync-feed (append feed-cfg (list :index-file index-file)))
              collect it)
      :test #'(lambda (x y) (equalp (second x) (second y))))))

;; here goes epub related stuff

(defun make-uri (path)
  (format nil "~{~a/~}~a.~a" 
          (cdr (pathname-directory path)) (pathname-name path) (pathname-type path)))

(defun xml-id (&optional uri)
  (let ((base (if uri (substitute #\. #\/ uri) (new-uid))))
    (format nil "id-~a" base)))

(defun mime-type (path)
  (if (equalp (pathname-type path) "html")
    "application/xhtml+xml"
    (let ((signature (make-array 8 :element-type '(unsigned-byte 8))))
      (with-open-file (f path :element-type '(unsigned-byte 8))
        (read-sequence signature f :end 8))
      (cond
        ((eql (search #(137 80 78 71 13 10 26 10) signature) 0) "image/png")
        ((eql (search #(71 73 70) signature) 0) "image/gif")
        ((eql (search #(255 216) signature) 0) "image/jpeg")
        ((eql (search #(60 63) signature) 0) "image/svg+xml")))))

(defun write-xml (xml dir name)
  (let ((path (ensure-directories-exist (merge-pathnames name dir))))
    (with-open-file (f path :direction :output :element-type '(unsigned-byte 8))
      (cxml-xmls:map-node (cxml:make-octet-stream-sink f :indentation 2)
                          xml :include-namespace-uri nil))))

(defun list-files (root)
  (let ((pattern (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                                 :name :wild :type :wild) root)))
    (mapcar #'(lambda (p) (list (pathname (enough-namestring p root)) (mime-type p)))
            (remove-if #'(lambda (p) (null (pathname-name p))) (directory pattern)))))

(defvar *play-order*)

(defun create-nav-points (toc)
  (loop for item in toc collect
        (append (list "navPoint" (list (list "id" (xml-id))
                                       (list "playOrder" (format nil "~a" (incf *play-order*))))
                      (list "navLabel" nil (list "text" nil (first item)))
                      (list "content" (list (list "src" (second item)))))
                (create-nav-points (cddr item)))))

(defun create-ncx (toc)
  (let ((*play-order* 0))
    (list "ncx" '(("xmlns" "http://www.daisy.org/z3986/2005/ncx/") ("version" "2005-1"))
          (list "head" nil
                (list "meta" (list (list "content" (xml-id)) '("name" "dtb:uid"))))
          '("docTitle" nil ("text" nil "Feedcircuit"))
          (append (list "navMap" nil) (create-nav-points toc)))))

(defun create-spine (toc)
  (loop for item in toc
        collect (list "itemref" (list (list "idref" (xml-id (second item)))))
        append (create-spine (cddr item))))

(defun now-str ()
  (multiple-value-bind (s m h d mo y) (get-decoded-time) (declare (ignore s))
    (format nil "~2,'0d-~2,'0d-~a ~2,'0d:~2,'0d" d mo y h m)))

(defun create-opf (toc files)
  (list "package" '(("version" "2.0") ("xmlns" "http://www.idpf.org/2007/opf") ("unique-identifier" "package-id"))
        (list "metadata" '(("xmlns:dc" "http://purl.org/dc/elements/1.1/") ("xmlns:opf" "http://www.idpf.org/2007/opf"))
              (list "dc:identifier" '(("id" "package-id")) (new-uid))
              (list "dc:title" nil (format nil "Feedcircuit ~a" (now-str)))
              '("dc:language" nil "en")
              '("dc:language" nil "ru"))
        (append '("manifest" nil)
                '(("item" (("id" "toc.ncx") ("href" "toc.ncx") ("media-type" "application/x-dtbncx+xml"))))
                (loop for (path m-type) in files collect
                      (list "item" (list (list "id" (xml-id (make-uri path)))
                                         (list "href" (make-uri path))
                                         (list "media-type" m-type)))))
        (append '("spine" (("toc" "toc.ncx"))) (create-spine toc))))

(defun write-mimetype (ebook-root)
  (let ((path (ensure-directories-exist (merge-pathnames "mimetype" ebook-root))))
    (with-open-file (f path :direction :output :if-exists :supersede)
      (write-string "application/epub+zip" f))))

(defun write-container (ebook-root)
  (let ((path (ensure-directories-exist 
                (merge-pathnames (make-pathname :directory '(:relative "META-INF") :name "container" :type "xml")
                                 ebook-root))))
    (with-open-file (f path :direction :output :if-exists :supersede)
      (write-string
"<?xml version=\"1.0\"?>
<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">
  <rootfiles>
    <rootfile full-path=\"OEBPS/root.opf\" media-type=\"application/oebps-package+xml\" />
  </rootfiles>
</container>"
        f))))

(defun pack-item (zf base path &optional do-not-compress)
  (let ((abspath (merge-pathnames path base)))
    (if (fad:directory-pathname-p abspath)
      (dolist (sub (fad:list-directory abspath))
        (pack-item zf base (enough-namestring sub base)))
      (with-open-file (f abspath :element-type '(unsigned-byte 8))
        (format t "Packing ~a~%" path)
        (zip:write-zipentry zf path f :do-not-compress do-not-compress)))))

(defun pack-ebook (path &optional cmd)
  (multiple-value-bind (v1 v2 v3 d m y) (get-decoded-time) (declare (ignore v1 v2 v3))
    (let ((epub
            (loop for n = 1 then (1+ n)
                  thereis (let ((fpath (merge-pathnames
                                         (make-pathname :directory '(:relative :up)
                                                        :name (format nil "fc-~a-~a-~a-~a" y m d n)
                                                        :type "epub") path))) 
                            (if (null (probe-file fpath)) fpath))))) 
      (if cmd
        #+sbcl (sb-ext:run-program cmd (list (namestring path) (namestring epub)))
        (zip:with-output-to-zipfile (zf epub)
          (pack-item zf path "mimetype" t)
          (pack-item zf path "META-INF/")
          (pack-item zf path "OEBPS/"))) 
      (file-namestring epub))))

(defun generate-ebook-name (dir)
  (multiple-value-bind (v1 v2 v3 d m y) (get-decoded-time) (declare (ignore v1 v2 v3)) 
    (loop for n = 1 then (1+ n)
                    thereis (let ((fpath (merge-pathnames
                                           (format nil "fc-~a-~a-~a-~a.epub" y m d n) dir))) 
                              (if (null (probe-file fpath)) fpath)))))

(defun list-ebooks (dir)
  (let ((pattern (merge-pathnames (make-pathname :name :wild :type "epub") dir)))
    (loop for epub in (directory pattern)
          when (eql (search "fc" (pathname-name epub)) 0) collect epub)))

(defun get-rid-of-old-ebooks (dir days)
  (let ((tm (- (get-universal-time) (* days 24 60 60)))) 
    (loop for epub in (list-ebooks dir) do
          (if (< (file-write-date epub) tm) (delete-file epub)))))

(defun make-ebook (work-dir epub prepare-fn)
  (let* ((*xhtml* t)
         (ebook-root (merge-pathnames (make-pathname :directory (list :relative (new-uid)))
                                      (merge-pathnames work-dir)))
         (html-root (merge-pathnames (make-pathname :directory '(:relative "OEBPS")) ebook-root)))
    (unwind-protect
      (let* ((toc (funcall prepare-fn html-root))
             (files (list-files html-root)))
        (when toc
          (write-xml (create-ncx toc) html-root "toc.ncx")
          (write-xml (create-opf toc files) html-root "root.opf")
          (write-mimetype ebook-root)
          (write-container ebook-root)
          (zip:with-output-to-zipfile (zf epub)
            (pack-item zf ebook-root "mimetype" t)
            (pack-item zf ebook-root "META-INF/")
            (pack-item zf ebook-root "OEBPS/"))  
          (file-namestring epub)))
      (cl-fad:delete-directory-and-files ebook-root :if-does-not-exist :ignore))))

(defun new-ebook (&key (root-dir "") (cache-dir *cache-dir*) (days 8) pack-cmd feeds)
  (let* ((*xhtml* t)
         (ebook-root (merge-pathnames (make-pathname :directory (list :relative (new-uid)))
                                      (merge-pathnames root-dir)))
         (html-root (merge-pathnames (make-pathname :directory '(:relative "OEBPS")) ebook-root)))
    (unwind-protect
      (let* ((index-file (merge-pathnames ".index" (merge-pathnames root-dir)))
             (toc (sync :root-dir (namestring html-root) :cache-dir cache-dir :feeds feeds :index-file index-file)) 
             (files (list-files html-root))) 
        (when toc
          (get-rid-of-old-ebooks root-dir days)
          (write-xml (create-ncx toc) html-root "toc.ncx")
          (write-xml (create-opf toc files) html-root "root.opf")
          (write-mimetype ebook-root)
          (write-container ebook-root)
          (pack-ebook ebook-root pack-cmd)))
      (cl-fad:delete-directory-and-files ebook-root :if-does-not-exist :ignore))))

(defun ebook-from-urls (urls &optional (work-dir ""))
  (make-ebook work-dir (generate-ebook-name work-dir)
              #'(lambda (dir) (loop for url in urls collect (cache-single-item url dir)))))

;(let ((config (with-open-file (f "config.2") (read f)))) (apply #'sync config))
;(let ((config (with-open-file (f "config") (read f)))) (apply #'new-ebook config))
;(sync-feed :title "Компьютерра" :name "index" :uri "http://www.computerra.ru/rss-sub-sgolub.xml" :days 8 :cache t)
;(sync-feed :title "Башорг" :name "bashorg" :uri "http://bash.org.ru/rss" :days 2)
