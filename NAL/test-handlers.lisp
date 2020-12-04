(in-package :nal)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

(defun menu-link ()
  (with-html-output (*standard-output*)
    (:p (:hr
         (:a :href "/NAL-Reasoner" "Back to menu")))))

(defmacro with-lisp-output ((var) &body body)
  `(let ((*package* (find-package :nal-user)))
     (with-output-to-string (,var #+:lispworks nil
                                  #+:lispworks :element-type
                                  #+:lispworks 'lw:simple-char)
       ,@body)))

(defmacro info-table (&rest forms)
  (let ((=value= (gensym))
        (=first= (gensym)))
    `(with-html-output (*standard-output*)
       (:p (:table :border 1 :cellpadding 2 :cellspacing 0
            (:tr (:td :colspan 2
                  "Some Information "
                  " provides about this request:"))
            ,@(loop for form in forms
                    collect `(:tr (:td :valign "top"
                                   (:pre :style "padding: 0px"
                                    (esc (with-lisp-output (s) (pprint ',form s)))))
                              (:td :valign "top"
                               (:pre :style "padding: 0px"
                                (esc (with-lisp-output (s)
                                       (loop for ,=value= in (multiple-value-list ,form)
                                             for ,=first= = t then nil
                                             unless ,=first=
                                             do (princ ", " s)
                                             do (pprint ,=value= s))))))))))
       (menu-link))))

;;Envío de archivos - inicio
(defvar *tmp-test-files* nil)

(defvar *tmp-test-directory*
    #+(or :win32 :mswindows) #p"c:\\NAL-Reasoner\\"
    #-(or :win32 :mswindows) #p"/tmp/NAL-Reasoner/")


(let ((counter 0))
  (defun handle-file (post-parameter)
    (when (and post-parameter
               (listp post-parameter))
      (destructuring-bind (path file-name content-type)
          post-parameter
        (let ((new-path (make-pathname :name (format nil "hunchentoot-~A"
                                                     (incf counter))
                                       :type nil
                                       :defaults *tmp-test-directory*)))
          ;; strip directory info sent by Windows browsers
          (when (search "Windows" (user-agent) :test 'char-equal)
            (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
          (rename-file path (ensure-directories-exist new-path))
          (push (list new-path file-name content-type) *tmp-test-files*))))))

(defun clean-tmp-dir ()
  (loop for (path . nil) in *tmp-test-files*
        when (probe-file path)
        do (ignore-errors (delete-file path)))
  (setq *tmp-test-files* nil))

(defun upload-test ()
  (let (post-parameter-p)
    (when (post-parameter "file1")
      (handle-file (post-parameter "file1"))
      (setq post-parameter-p t))
    (when (post-parameter "file2")
      (handle-file (post-parameter "file2"))
      (setq post-parameter-p t))
    (when (post-parameter "clean")
      (clean-tmp-dir)
      (setq post-parameter-p t)))
  (no-cache)
  (with-html
    (:html
     (:head (:title "Hunchentoot file upload test"))
     (:body
      (:h2 "File upload test")
      (:form :method :post :enctype "multipart/form-data"
       (:p "First file: "
        (:input :type :file
         :name "file1"))
       (:p "Second file: "
        (:input :type :file
         :name "file2"))
       (:p (:input :type :submit)))
      (when *tmp-test-files*
        (htm
         (:p
          (:table :border 1 :cellpadding 2 :cellspacing 0
           (:tr (:td :colspan 3 (:b "Uploaded files")))
           (loop for (path file-name nil) in *tmp-test-files*
                 for counter from 1
                 do (htm
                     (:tr (:td :align "right" (str counter))
                      (:td (:a :href (format nil "files/~A?path=~A"
                                             (url-encode file-name)
                                             (url-encode (namestring path)))
                            (esc file-name)))
                      (:td :align "right"
                       (str (ignore-errors
                              (with-open-file (in path)
                                (file-length in))))
                       "&nbsp;Bytes"))))))
         (:form :method :post
          (:p (:input :type :submit :name "clean" :value "Delete uploaded files")))))
      (menu-link)))))

;;Envío de archivos - fin

;;Parenscript - inicio
(define-easy-handler (ejemplojs :uri "/NAL-Reasoner/ejemplojs.html") ()
  (with-html
    (:html
     (:head
      (:title "Parenscript tutorial"))
     (:body
      (:h2 "Parenscript tutorial")
      (:a :href "#" :onclick (ps (alert "Hello World"))
          "Hello World")))))
;;Parenscript - fin

;;Envío de datos - inicio
(define-easy-handler (example :uri "/NAL-Reasoner/example.html"
                                :default-request-type :post)
    (first-name last-name
                (age :parameter-type 'integer)
                (implementation :parameter-type 'keyword)
                (meal :parameter-type '(hash-table boolean))
                (team :parameter-type 'list))
  (with-html
    (:html
     (:head (:title "Hunchentoot \"easy\" handler example"))
     (:body.png.png
      (:h2 "Example")
      (:p (:form :method :post
           (:table :border 1 :cellpadding 2 :cellspacing 0
            (:tr
             (:td "First Name:")
             (:td (:input :type :text
                   :name "first-name"
                   :value (or first-name "Donald"))))
            (:tr
             (:td "Last name:")
             (:td (:input :type :text
                   :name "last-name"
                   :value (or last-name "Duck"))))
            (:tr
             (:td "Age:")
             (:td (:input :type :text
                   :name "age"
                   :value (or age 42))))
            (:tr
             (:td "Implementation:")
             (:td (:select :name "implementation"
                   (loop for (value option) in '((:lispworks "LispWorks")
                                                 (:allegro "AllegroCL")
                                                 (:cmu "CMUCL")
                                                 (:sbcl "SBCL")
                                                 (:openmcl "OpenMCL"))
                         do (htm
                             (:option :value value
                              :selected (eq value implementation)
                              (str option)))) ) ))
            (:tr
             (:td :valign :top "Meal:")
             (:td (loop for choice in '("Burnt weeny sandwich")
                        do (htm
                            (:input :type "checkbox"
                             :name (format nil "meal{~A}" choice)
                             :checked (gethash choice meal)
                             (esc choice))
                            (:br)))))
            (:tr
             (:td :valign :top "Team:")
             (:td (loop for player in '("Beckenbauer"
                                        "Cruyff"
                                        "Maradona"
                                        ;; without accent (for SBCL)
                                        "Pele"
                                        "Zidane")
                        do (htm
                            (:input :type "checkbox"
                             :name "team"
                             :value player
                             :checked (member player team :test 'string=)
                             (esc player))
                            (:br)))))
            (:tr
             (:td :colspan 2
              (:input :type "submit"))))))
      (info-table first-name
                  last-name
                  age
                  implementation
                  (loop :for choice :being :the :hash-keys :of meal :collect choice)
                  (gethash "Yellow snow" meal)
                  team)))))

;;Envío de datos - fin

(defun menu ()
  (with-html
    (:html
     (:head
      (:link :rel "shortcut icon"
       :href "/NAL-Reasoner/favicon.ico" :type "image/x-icon")
      (:title "Menu"))
     (:body
      (:h2 "Menu")
      (:table :border 0 :cellspacing 4 :cellpadding 4
       (:tr (:td (:a :href "/NAL-Reasoner/index.html"
                  "NAL")))
       (:tr (:td (:a :href "/NAL-Reasoner/upload.html"
                  "File uploads")))
       (:tr (:td (:a :href "/NAL-Reasoner/example.html"
                  "Example")))
       (:tr (:td (:a :href "/NAL-Reasoner/ejemplojs.html"
                  "ExampleJS"))))))))

(setq *dispatch-table*
      (nconc
       (list 'dispatch-easy-handlers
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/pushbar.css"
              (make-pathname :name "pushbar" :type "css" :version nil
                             :defaults *this-file*)
              "text/css")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/estilo.css"
              (make-pathname :name "estilo" :type "css" :version nil
                             :defaults *this-file*)
              "text/css")
             
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/funciones.js"
              (make-pathname :name "funciones" :type "js" :version nil
                             :defaults *this-file*)
              "text/javascript")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/pushbar.js"
              (make-pathname :name "pushbar" :type "js" :version nil
                             :defaults *this-file*)
              "text/javascript")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/fondo3.jpg"
              (make-pathname :name "fondo3" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/jpeg")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/fondo4.jpg"
              (make-pathname :name "fondo4" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/jpeg")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/fondo5.jpg"
              (make-pathname :name "fondo5" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/jpeg")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/fondo6.jpg"
              (make-pathname :name "fondo6" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/jpeg")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/fondo7.jpg"
              (make-pathname :name "fondo7" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/jpeg")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/fondo8.jpg"
              (make-pathname :name "fondo8" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/jpeg")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/logo_cic3.png"
              (make-pathname :name "logo_cic3" :type "png" :version nil
                             :defaults *this-file*)
              "image/jpg")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/logo_escom.jpg"
              (make-pathname :name "logo_escom" :type "jpg" :version nil
                             :defaults *this-file*)
              "image/png")
             (create-static-file-dispatcher-and-handler
              "/NAL-Reasoner/logo_ipn3.png"
              (make-pathname :name "logo_ipn3" :type "png" :version nil
                             :defaults *this-file*)
              "image/png"))
       (mapcar (lambda (args)
                 (apply 'create-prefix-dispatcher args))
               '(("/NAL-Reasoner/upload.html" upload-test)
                ("/NAL-Reasoner" menu)))))
