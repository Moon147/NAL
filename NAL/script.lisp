(in-package :nal)

(defun file-length-string (pathname)
  (with-open-file (f pathname)
    (princ-to-string (file-length f))))

(defun say (fmt &rest args)
  (format t "; ")
  (apply #'format t fmt args)
  (terpri))

(defun hm (base-url &key (make-cookie-jar
                                        (lambda ()
                                          (make-instance 'drakma:cookie-jar))))
  "Runs the built-in confidence test.  BASE-URL is the base URL to use
for testing, it should not have a trailing slash.  The keyword
arguments accepted are for future extension and should not currently
be used.

The script expects the Hunchentoot example test server to be running
at the given BASE-URL and retrieves various pages from that server,
expecting certain responses."
  (with-script-context (:base-url (format nil "~A/NAL-Reasoner" base-url))

    (say "Request home page")
    (http-request "")
    (http-assert 'status-code 200)
    (http-assert-header :content-type "^text/html")

    (say "Test cookies")
    (let ((cookie-jar (funcall make-cookie-jar)))
      (http-request "cookie.html" :cookie-jar cookie-jar)
      (http-request "cookie.html" :cookie-jar cookie-jar)
      (http-assert-body "(?msi)COOKIE-IN &quot;pumpkin&quot;.*&quot;barking&quot;"))

    (say "Test session variables")
    (let ((cookie-jar (funcall make-cookie-jar)))
      (http-request "session.html" :cookie-jar cookie-jar
                    :method :post :parameters '(("new-foo-value" . "ABC") ("new-bar-value" . "DEF")))
      (http-request "session.html" :cookie-jar cookie-jar)
      ;; These assertions assume that SESSION-VALUE returns the found alist value as second value
      (http-assert-body "(?i)\(HUNCHENTOOT-TEST::FOO . &quot;ABC&quot;\)")
      (http-assert-body "(?i)\(HUNCHENTOOT-TEST::BAR . &quot;DEF&quot;\)"))

    (say "Test malformed session cookie validation")
    (dolist (session-id '("" "invalid-session-id" ":invalid-session-id" "invalid:session-id"))
      (http-request "session.html"
                    :additional-headers (acons "Cookie" (format nil "hunchentoot-session=~A" session-id) nil))
      (http-assert 'status-code 200)
      ;; session is empty
      (http-assert-body "(?i)\(HUNCHENTOOT-TEST::FOO\)"))

    (say "Test GET parameters with foreign characters (Latin-1)")
    (http-request "parameter_latin1_get.html"
                  :external-format-out :iso-8859-1
                  :parameters (list (cons "foo" (format nil "H~Chner" #.(code-char 252))))
                  :additional-headers '(("Content-Type" . "text/plain; charset=iso-8859-1")))
    (http-assert-header :content-type "(?i)text/html; charset=ISO-8859-1")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "(?i)&quot;H&#xFC;hner&quot;")

    (say "Test POST parameters with foreign characters (Latin-1)")
    (http-request "parameter_latin1_post.html"
                  :external-format-out :iso-8859-1
                  :method :post :parameters (list (cons "foo" (format nil "H~Chner" #.(code-char 252)))))
    (http-assert-header :content-type "(?i)text/html; charset=ISO-8859-1")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "(?i)&quot;H&#xFC;hner&quot;")

    (say "Test GET parameters with foreign characters (UTF-8)")
    (http-request "parameter_utf8_get.html"
                  :external-format-out :utf-8
                  :parameters (list (cons "foo" (format nil "H~Chner" #.(code-char 252)))))
    (http-assert-header :content-type "(?i)text/html; charset=UTF-8")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "(?i)&quot;H&#xFC;hner&quot;")

    (say "Test POST parameters with foreign characters (UTF-8)")
    (http-request "parameter_utf8_post.html"
                  :method :post
                  :external-format-out :utf-8
                  :parameters (list (cons "foo" (format nil "H~Chner" #.(code-char 252)))))
    (http-assert-header :content-type "(?i)text/html; charset=UTF-8")
    (http-assert-body "(72 252 104 110 101 114)")
    (http-assert-body "(?i)&quot;H&#xFC;hner&quot;")

    (say "Test redirection")
    (http-request "redir.html")
    (http-assert 'uri (lambda (uri)
                        (matches (princ-to-string uri) "info.html\\?redirected=1")))

    (say "Test authorization")
    (http-request "authorization.html")
    (http-assert 'status-code 401)
    (http-request "authorization.html"
                  :basic-authorization '("nanook" "igloo"))
    (http-assert 'status-code 200)

    (say "Request the Zappa image")
    (http-request "image.jpg")
    (http-assert-header :content-length (file-length-string #P"fz.jpg"))
    (http-assert-header :content-type "image/jpeg")
    (http-assert 'body (complement #'mismatch) (file-contents #P"fz.jpg"))

    (say "Request the Zappa image from RAM")
    (http-request "image-ram.jpg")
    (http-assert-header :content-length (file-length-string #P"fz.jpg"))
    (http-assert-header :content-type "image/jpeg")
    (http-assert 'body (complement #'mismatch) (file-contents #P"fz.jpg"))

    (say "Upload a file")
    (http-request "upload.html"
                  :method :post :parameters '(("clean" . "doit")))
    (http-request "upload.html"
                  :method :post :parameters '(("file1" #P"fz.jpg")))
    (http-request "upload.html")
    (http-assert-body (format nil "fz.jpg.*>~A&nbsp;Bytes" (file-length-string #P"fz.jpg")))

    (say "Range tests")
    (say " Upload file")
    (let* ((range-test-file-size (* 256 1024))  ; large enough to have hunchentoot use multiple buffers when reading back data, should be aligned to 1024
           (range-test-buffer (make-array range-test-file-size :element-type '(unsigned-byte 8)))
           (uploaded-file-url "files/?path=user-stream")) ; The uploaded file will appear under the name "user-stream" in hunchentoot

      (dotimes (i range-test-file-size)
         (setf (aref range-test-buffer i) (random 256)))

      (flex:with-input-from-sequence (upload-stream range-test-buffer)
        (http-request "upload.html"
                      :method :post :parameters `(("file1" ,upload-stream))))

      (say " Request the uploaded file, verify contents")
      (http-request uploaded-file-url)
      (http-assert-header :content-length (princ-to-string range-test-file-size))
      (http-assert 'body (complement #'mismatch) range-test-buffer)

      (say " Verify responses to partial requests")

      (say " Request just one byte")
      (http-request uploaded-file-url :range '(0 0))
      (http-assert 'status-code 206)
      (http-assert 'body 'equalp (subseq range-test-buffer 0 1))
      (http-assert-header :content-range (format nil "bytes 0-0/~D" range-test-file-size))

      (say " End out of range")
      (http-request uploaded-file-url :range (list 0 range-test-file-size))
      (http-assert 'status-code 416)
      (http-assert-header :content-range (format nil "bytes 0-~D/~A" (1- range-test-file-size) range-test-file-size))

      (say " Request whole file as partial")
      (http-request uploaded-file-url :range (list 0 (1- range-test-file-size)))
      (http-assert 'status-code 206)
      (http-assert 'body 'equalp range-test-buffer)
      (http-assert-header :content-range (format nil "bytes 0-~D/~D" (1- range-test-file-size) range-test-file-size))

      (say " Request something in the middle")
      (let ((start-offset (/ range-test-file-size 4))
            (length (/ range-test-file-size 2)))
        (http-request uploaded-file-url :range (list start-offset (1- length)))
        (http-assert 'status-code 206)
        (http-assert 'body 'equalp (subseq range-test-buffer start-offset length))
        (http-assert-header :content-range (format nil "bytes ~D-~D/~D" start-offset (1- length) range-test-file-size))))


    (values)))

