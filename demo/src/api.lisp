;; Copyright (c) 2022 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :jingle.demo.api
  (:use :cl)
  (:import-from :jingle)
  (:import-from :jonathan)
  (:import-from :local-time)
  (:import-from :cl-ppcre)
  (:export
   :register-urls))
(in-package :jingle.demo.api)

(define-condition api-error (jingle:base-http-error)
  ()
  (:documentation "Represents a condition which will be signalled on API errors"))

(defmethod jingle:handle-error ((error api-error))
  "Handles the error and sets up the HTTP error response to be sent to
clients"
  (with-accessors ((code jingle:http-error-code)
                   (body jingle:http-error-body)) error
    (jingle:set-response-status code)
    (jingle:set-response-header :content-type "application/json")
    (jonathan:to-json (list :|error| body))))

(defun throw-not-found-error (message)
  "Throws a 404 (Not Found) HTTP response"
  (error 'api-error :code :not-found :body message))

(defun throw-bad-request-error (message)
  "Throws a 400 (Bad Request) HTTP response"
  (error 'api-error :code :bad-request :body message))

(defun get-int-param (params name &optional default)
  "Gets the NAME parameter from PARAMS and parses it as an integer.
In case of invalid input it will signal a 400 (Bad Request) error"
  (let ((raw (jingle:get-request-param params name default)))
    (typecase raw
      (number raw)
      (null (throw-bad-request-error (format nil "missing value for `~A` param" name)))
      (string (let ((parsed (parse-integer raw :junk-allowed t)))
                (unless parsed
                  (throw-bad-request-error (format nil "invalid value for `~A` param" name)))
                parsed))
      (t (throw-bad-request-error (format nil "unsupported value for `~A` param" name))))))

(defun decode-json-or-throw-bad-request (content)
  "Decodes the given CONTENT or signals a Bad Request condition on errors"
  (handler-case (jonathan:parse content)
    (error ()
      (throw-bad-request-error "invalid JSON content"))))

(defparameter *products*
  '((:|id| 1 :|name| "foo")
    (:|id| 2 :|name| "bar")
    (:|id| 3 :|name| "baz")
    (:|id| 4 :|name| "qux"))
  "The `database' used by our API")

(defun find-product-by-id (id)
  "Finds a product by id"
  (find id
        *products*
        :key (lambda (item) (getf item :|id|))
        :test #'=))

(defun take (items from to)
  "A helper function to return the ITEMS between FROM and TO range"
  (let* ((len (length items))
         (to (if (>= to len) len to)))
    (if (>= from len)
        nil
        (subseq items from to))))

(defun get-product-by-id-handler (params)
  "Handles requests for the /api/v1/product/:id endpoint"
  (jingle:with-json-response
    (let* ((id (get-int-param params :id))
           (product (find-product-by-id id)))
      (unless product
        (throw-not-found-error "product not found"))
      product)))

(defun get-products-page-handler (params)
  "Handles requests for the /api/v1/product endpoint"
  (jingle:with-json-response
    (let ((from (get-int-param params "from" 0))
          (to (get-int-param params "to" 2)))
      (when (or (minusp from) (minusp to))
        (throw-bad-request-error "`from` and `to` must be positive"))
      (take
       (setf *products*
             (sort *products* #'< :key (lambda (item) (getf item :|id|))))
       from to))))

(defclass ping-response ()
  ((message
    :initarg :message
    :initform "pong"
    :documentation "Message to send as part of the response")
   (timestamp
    :initarg :timestamp
    :initform (local-time:now)
    :documentation "Timestamp of the message"))
  (:documentation "A response sent as part of a PING request"))

(defmethod jonathan:%to-json ((object ping-response))
  (with-slots (message timestamp) object
    (jonathan:with-object
      (jonathan:write-key-value "message" message)
      (jonathan:write-key-value "timestamp" timestamp))))

(defun ping-handler (params)
  "Handles requests for /api/v1/ping"
  (declare (ignore params))
  (jingle:with-json-response
    (make-instance 'ping-response)))

(defun delete-product-by-id-handler (params)
  "Handles requests for DELETE /api/v1/product/:id endpoint.
Note that this handler is not thread-safe, since that is outside of
the scope for the demo."
  (jingle:with-json-response
    (let* ((id (get-int-param params :id))
           (product (find-product-by-id id)))
      (unless product
        (throw-not-found-error "product not found"))
      (setf *products* (remove id *products* :test #'= :key (lambda (item) (getf item :|id|))))
      product)))

(defun create-product-handler (params)
  "Handles requests for POST /api/v1/product endpoint. Note that this
handler is not thread-safe, since that is outside of the scope for the
demo."
  (declare (ignore params))
  (jingle:with-json-response
    (flet ((product-exists-p (name)
             (find name *products* :test #'string= :key (lambda (item) (getf item :|name|))))
           (get-next-id ()
             (if (null *products*)
                 1
                 (1+ (apply #'max (mapcar (lambda (item) (getf item :|id|)) *products*))))))
      (let* ((content (babel:octets-to-string (jingle:request-content jingle:*request*)))
             (body (decode-json-or-throw-bad-request content))
             (name (getf body :|name|))
             (new-item (list :|id| (get-next-id) :|name| name)))
        (unless name
          (throw-bad-request-error "must provide product name"))
        (when (product-exists-p name)
          (throw-bad-request-error "product already exists"))
        (push new-item *products*)
        new-item))))

(defun restart-server-handler (&key)
  (list :|message| "Server restart requested"
        :|instructions| "Please run: (restart-server)"
        :|note| "Or use the REPL restart sequence"
        :|timestamp| (local-time:now)))

;; Create wrapper functions that jingle can call with params
(defun echo-wrapper (params)
  (jingle:with-json-response
    (let ((extracted-params (extract-parameters '(:query ((:name string) (:email string))) params)))
      (apply #'echo-handler extracted-params))))

(defun greet-wrapper (params)
  (jingle:with-json-response
    (let ((extracted-params (extract-parameters '(:path ((:name string))
                                                  :query ((:greeting string :optional t :default "Hello"))) params)))
      (apply #'greet-handler extracted-params))))

(defun runbook-read-wrapper (params)
  (jingle:with-json-response
    (let ((extracted-params (extract-parameters '(:body ((:query string)
                                                          (:channel string)
                                                          (:thread-ts string)
                                                          (:token string))) params)))
      (apply #'runbook-read-handler extracted-params))))

(defun calculate-wrapper (params)
  (jingle:with-json-response
    (let ((extracted-params (extract-parameters '(:body ((:a integer)
                                                          (:b integer)
                                                          (:operation string))) params)))
      (apply #'calculate-handler extracted-params))))

(defun fortune-wrapper (params)
  (jingle:with-json-response
    (let ((extracted-params (extract-parameters '(:query ((:category string :optional t :default "general"))) params)))
      (apply #'fortune-handler extracted-params))))

(defun refresh-routes-wrapper (params)
  (declare (ignore params))
  (jingle:with-json-response
    (refresh-routes-handler)))

(defun restart-server-wrapper (params)
  (declare (ignore params))
  (jingle:with-json-response
    (restart-server-handler)))

;; Define parameter-aware handler functions
(defun echo-handler (&key name email)
  (let ((analysis (format nil "Processing ~A at ~A" name email)))
    (list :|received| (list :|name| name :|email| email)
          :|analysis| analysis
          :|timestamp| (local-time:now)
          :|status| "processed")))

(defun greet-handler (&key name greeting)
  (let ((message (format nil "~A, ~A!" greeting name)))
    (list :|message| message
          :|timestamp| (local-time:now))))

(defun runbook-read-handler (&key query channel thread-ts token)
  (let ((slack-url (format nil "https://slack.com/~A" channel))
        (analysis (format nil "Analyzing query: ~A" query)))
    (list :|status| 200
          :|body| (list :|query| query
                        :|channel| channel
                        :|thread-ts| thread-ts
                        :|slack-url| slack-url
                        :|analysis| analysis))))

(defun calculate-handler (&key a b operation)
  (let ((result (case (intern (string-upcase operation) :keyword)
                  (:add (+ a b))
                  (:subtract (- a b))
                  (:multiply (* a b))
                  (:divide (if (zerop b) 
                             (error 'api-error :code :bad-request :body "division by zero")
                             (/ a b)))
                  (t (error 'api-error :code :bad-request :body "unsupported operation")))))
    (list :|a| a
          :|b| b
          :|operation| operation
          :|result| result
          :|timestamp| (local-time:now))))

(defun fortune-handler (&key category)
  (let ((fortunes (case (intern (string-upcase category) :keyword)
                    (:wisdom '("The journey of a thousand miles begins with one step."
                              "Knowledge is power, but practice is mastery."
                              "A wise person learns from the mistakes of others."))
                    (:humor '("I told my wife she was drawing her eyebrows too high. She looked surprised."
                             "Why don't scientists trust atoms? Because they make up everything!"
                             "I'm reading a book about anti-gravity. It's impossible to put down!"))
                    (:programming '("There are only two hard things in Computer Science: cache invalidation and naming things."
                                   "Code never lies, comments sometimes do."
                                   "Any fool can write code that a computer can understand. Good programmers write code that humans can understand."))
                    (t '("Today is a good day to try something new."
                        "Fortune favors the bold."
                        "Every expert was once a beginner.")))))
    (list :|fortune| (nth (random (length fortunes)) fortunes)
          :|category| category
          :|timestamp| (local-time:now))))

;; Force redefinition of *urls* 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (boundp '*urls*)
    (makunbound '*urls*)))

(defparameter *urls*
  `((:method :GET    :path "/api/v1/ping"        :handler ,#'ping-handler                 :name "ping")
    (:method :GET    :path "/api/v1/product"     :handler ,#'get-products-page-handler    :name "get-products-page")
    (:method :GET    :path "/api/v1/product/:id" :handler ,#'get-product-by-id-handler    :name "get-product-by-id")
    (:method :POST   :path "/api/v1/product"     :handler ,#'create-product-handler       :name "create-product")
    (:method :DELETE :path "/api/v1/product/:id" :handler ,#'delete-product-by-id-handler :name "delete-product-by-id")
    
    ;; DRY parameter-aware routes using wrapper functions
    (:method :POST :path "/api/v1/echo" :handler ,#'echo-wrapper :name "echo"
     :parameters (:query ((:name string) (:email string))))
    
    (:method :GET :path "/api/v1/greet/:name" :handler ,#'greet-wrapper :name "greet"
     :parameters (:path ((:name string))
                  :query ((:greeting string :optional t :default "Hello"))))
    
    (:method :POST :path "/api/v1/runbook-read" :handler ,#'runbook-read-wrapper :name "runbook-read"
     :parameters (:body ((:query string)
                         (:channel string)
                         (:thread-ts string)
                         (:token string))))
    
    (:method :POST :path "/api/v1/calculate" :handler ,#'calculate-wrapper :name "calculate"
     :parameters (:body ((:a integer)
                         (:b integer)
                         (:operation string))))
    
    (:method :GET :path "/api/v1/fortune" :handler ,#'fortune-wrapper :name "fortune"
     :parameters (:query ((:category string :optional t :default "general"))))
    
    (:method :POST :path "/api/v1/refresh-routes" :handler ,#'refresh-routes-wrapper :name "refresh-routes"
     :parameters ())
    
    (:method :POST :path "/api/v1/restart-server" :handler ,#'restart-server-wrapper :name "restart-server"
     :parameters ()))
  "The URLs map of our API")

(defun test-endpoint-handler (params)
  "Handles requests for the test endpoint"
  (jingle:with-json-response
    (let* ((content (babel:octets-to-string (jingle:request-content jingle:*request*)))
           (body (when (and content (> (length content) 0))
                   (decode-json-or-throw-bad-request content)))
           (message (getf body :|message|)))
      (list :|received| message
            :|timestamp| (local-time:now)
            :|status| "success"))))

(defun swagger-json-handler (params)
  "Dynamic swagger.json handler that generates from route definitions"
  (declare (ignore params))
  (jingle:set-response-header :content-type "application/json")
  (jingle:set-response-header :access-control-allow-origin "*")
  (jingle:set-response-header :access-control-allow-methods "GET, POST, PUT, DELETE, OPTIONS")
  (jingle:set-response-header :access-control-allow-headers "Content-Type, Authorization")
  (format t "~%DEBUG: *urls* length in handler: ~A~%" (length *urls*))
  (format t "DEBUG: *urls* routes: ~{~A~^, ~}~%" 
          (mapcar (lambda (route) (getf route :path)) *urls*))
  (let ((spec (generate-dynamic-swagger-spec)))
    (format t "DEBUG: Generated spec paths: ~A~%" (length (getf spec :|paths|)))
    (jonathan:to-json spec)))

(defun swagger-json-simple-handler (params)
  "Swagger.json with only simple operations (no parameters/requestBody)"
  (declare (ignore params))
  (jingle:set-response-header :content-type "application/json")
  (let ((simple-spec (generate-simple-swagger-spec)))
    (jonathan:to-json simple-spec)))

(defun swagger-json-minimal-handler (params)
  "Minimal swagger.json for testing"
  (declare (ignore params))
  (jingle:set-response-header :content-type "application/json")
  (jonathan:to-json 
    (list :|openapi| "3.0.3"
          :|info| (list :|title| "Test API" :|version| "1.0.0")
          :|paths| (list :|/api/v1/ping| 
                         (list :|get| 
                               (list :|summary| "Ping endpoint"
                                     :|operationId| "ping"
                                     :|responses| (list :|200| (list :|description| "Success"))))))))

(defun generate-simple-swagger-spec ()
  "Generates swagger spec with only simple operations"
  (let ((paths-alist '()))
    ;; Only process routes without :parameters
    (loop for route in *urls*
          for method = (getf route :method)
          for path = (getf route :path)
          for name = (getf route :name)
          for param-spec = (getf route :parameters)
          for swagger-path = (convert-jingle-path-to-openapi path)
          when (not param-spec) ; Only simple routes
          do (let ((existing-path (assoc swagger-path paths-alist :test #'string=)))
               (if existing-path
                   ;; Add method to existing path
                   (setf (cdr existing-path)
                         (append (cdr existing-path)
                                 (list (intern (string-downcase (symbol-name method)) :keyword)
                                       (generate-operation-spec name nil)))) ; nil for no params
                   ;; Create new path entry
                   (push (cons swagger-path
                               (list (intern (string-downcase (symbol-name method)) :keyword)
                                     (generate-operation-spec name nil)))
                         paths-alist))))
    
    ;; Convert alist to plist for JSON
    (let ((paths-plist '()))
      (dolist (path-entry paths-alist)
        (push (intern (car path-entry) :keyword) paths-plist)
        (push (cdr path-entry) paths-plist))
      
      ;; Build the complete OpenAPI spec
      (list :|openapi| "3.0.3"
            :|info| (list :|title| "Common Lisp jingle demo - Simple"
                          :|description| "Only simple operations without parameters"
                          :|version| "1.0.0"
                          :|contact| (list :|email| "dnaeon@gmail.com"))
            :|paths| (nreverse paths-plist)
            :|components| (list :|schemas| (list :|ApiError| (list :|type| "object"
                                                                   :|properties| (list :|error| (list :|type| "string")))))))))

(defun generate-dynamic-swagger-spec ()
  "Generates swagger spec from *urls* with parameter specs"
  (let ((paths-alist '()))
    ;; Process each route in *urls*
    (loop for route in *urls*
          for method = (getf route :method)
          for path = (getf route :path)
          for name = (getf route :name)
          for param-spec = (getf route :parameters)
          for swagger-path = (convert-jingle-path-to-openapi path)
          do (let ((existing-path (assoc swagger-path paths-alist :test #'string=)))
               (if existing-path
                   ;; Add method to existing path
                   (setf (cdr existing-path)
                         (append (cdr existing-path)
                                 (list (intern (string-downcase (symbol-name method)) :keyword)
                                       (generate-operation-spec name param-spec))))
                   ;; Create new path entry
                   (push (cons swagger-path
                               (list (intern (string-downcase (symbol-name method)) :keyword)
                                     (generate-operation-spec name param-spec)))
                         paths-alist))))
    
    ;; Convert alist to plist for JSON
    (let ((paths-plist '()))
      (dolist (path-entry paths-alist)
        (push (intern (car path-entry) :keyword) paths-plist)
        (push (cdr path-entry) paths-plist))
      
      ;; Build the complete OpenAPI spec
      (list :|openapi| "3.0.3"
            :|info| (list :|title| "Common Lisp jingle demo - Dynamic"
                          :|description| "Auto-generated from route definitions with parameter specs"
                          :|version| "1.0.0"
                          :|contact| (list :|email| "dnaeon@gmail.com"))
            :|paths| (nreverse paths-plist)
            :|components| (list :|schemas| (list :|ApiError| (list :|type| "object"
                                                                   :|properties| (list :|error| (list :|type| "string")))))))))

(defun convert-jingle-path-to-openapi (path)
  "Converts /api/v1/product/:id to /api/v1/product/{id}"
  (ppcre:regex-replace-all ":([^/]+)" path "{\\1}"))

(defun generate-operation-spec (name param-spec)
  "Generates OpenAPI operation from name and parameter spec"
  (let ((operation (list :|summary| (format nil "~A endpoint" (string-capitalize name))
                         :|operationId| name)))
    
    (when param-spec
      ;; Add parameters and requestBody based on spec
      (let ((openapi-params (process-parameter-spec param-spec)))
        (when (getf openapi-params :parameters)
          (setf operation (append operation (list :|parameters| (getf openapi-params :parameters)))))
        (when (getf openapi-params :requestBody)
          (setf operation (append operation (list :|requestBody| (getf openapi-params :requestBody)))))))
    
    ;; Add default responses
    (setf operation (append operation (list :|responses| (list :|200| (list :|description| "Successful operation")
                                                                :|400| (list :|description| "Bad Request"
                                                                             :|content| (list :|application/json| 
                                                                                              (list :|schema| (list :|$ref| "#/components/schemas/ApiError"))))))))
    operation))

(defun process-parameter-spec (param-spec)
  "Converts parameter spec to OpenAPI parameters and requestBody"
  (let ((parameters '())
        (request-body nil))
    
    (when param-spec
      (loop for i from 0 below (length param-spec) by 2
            for location = (nth i param-spec)
            for params = (nth (1+ i) param-spec)
            do (case location
                 ((:path :query)
                  (loop for param-def in params
                        for param-name = (first param-def)
                        for param-type = (second param-def)
                        for optional-p = (member :optional param-def)
                        for default-val = (getf param-def :default)
                        do (let ((param-spec (list :|name| (string-downcase (string param-name))
                                                   :|in| (string-downcase (string location))
                                                   :|required| (if optional-p :false t)
                                                   :|schema| (list :|type| (type-to-openapi-type param-type)))))
                             (when default-val
                               (setf (getf (getf param-spec :|schema|) :|default|) default-val))
                             (push param-spec parameters))))
                 (:body
                  (let ((properties (make-hash-table :test 'equal))
                        (required '()))
                    (loop for param-def in params
                          for param-name = (first param-def)
                          for param-type = (second param-def)
                          for optional-p = (member :optional param-def)
                          do (setf (gethash (string-downcase (string param-name)) properties)
                                   (list :|type| (type-to-openapi-type param-type)))
                             (unless optional-p
                               (push (string-downcase (string param-name)) required)))
                    (setf request-body
                          (list :|content| (list :|application/json|
                                                 (list :|schema| (list :|type| "object"
                                                                       :|properties| (hash-table-to-plist properties)
                                                                       :|required| (nreverse required)))))))))))
    
    (list :parameters (nreverse parameters) :requestBody request-body)))

(defun type-to-openapi-type (lisp-type)
  "Converts Lisp type symbols to OpenAPI type strings"
  (case lisp-type
    (string "string")
    (integer "integer")
    (number "number")
    (boolean "boolean")
    (t "string"))) ; default

(defun hash-table-to-plist (hash-table)
  "Converts hash table to plist for JSON serialization"
  (let ((result '()))
    (maphash (lambda (key value)
               (push (intern key :keyword) result)
               (push (if (hash-table-p value)
                         (hash-table-to-plist value)
                         value)
                     result))
             hash-table)
    (nreverse result)))

(defun extract-parameters (param-spec raw-params)
  "Extracts parameters according to specification and returns as plist"
  (let ((result '()))
    (when param-spec
      (loop for i from 0 below (length param-spec) by 2
            for location = (nth i param-spec)
            for params = (nth (1+ i) param-spec)
            do (loop for param-def in params
                     for param-name = (first param-def)
                     for param-type = (second param-def)
                     for optional-p = (member :optional param-def)
                     for default-val = (getf param-def :default)
                     for value = (case location
                                   (:path (jingle:get-request-param raw-params param-name))
                                   (:query (jingle:get-request-param raw-params (string param-name) default-val))
                                   (:body (extract-body-param param-name param-type)))
                     do (when (or value (not optional-p))
                          (push (parse-param-value value param-type) result)
                          (push param-name result)))))
    (nreverse result)))

(defun extract-body-param (param-name param-type)
  "Extracts parameter from request body JSON"
  (let* ((content (babel:octets-to-string (jingle:request-content jingle:*request*)))
         (body (when (and content (> (length content) 0))
                 (decode-json-or-throw-bad-request content))))
    (getf body (intern (string-upcase param-name) :keyword))))

(defun parse-param-value (value param-type)
  "Parses parameter value according to type"
  (case param-type
    (integer (if (stringp value)
                 (or (parse-integer value :junk-allowed t)
                     (throw-bad-request-error (format nil "invalid integer: ~A" value)))
                 value))
    (string (if value (string value) value))
    (t value)))

(defun create-parameter-aware-handler (route-spec)
  "Creates a handler that extracts and passes parameters as specified"
  (let ((param-spec (getf route-spec :parameters))
        (handler-fn (getf route-spec :handler)))
    (lambda (raw-params)
      ;; Extract parameters according to spec and call handler with keyword args
      (let ((extracted-params (extract-parameters param-spec raw-params)))
        (jingle:with-json-response
          (apply handler-fn extracted-params))))))

(defparameter *current-app* nil "Store reference to current app for dynamic updates")

(defun refresh-routes-handler ()
  "Refreshes all routes from *urls* without server restart"
  (let ((app *current-app*)
        (registered-count 0))
    (when app
      (loop :for route-spec :in *urls*
            :for path = (getf route-spec :path)
            :for method = (getf route-spec :method)
            :for name = (getf route-spec :name)
            :for handler = (if (getf route-spec :parameters)
                               (create-parameter-aware-handler route-spec)
                               (getf route-spec :handler)) :do
              (setf (jingle:route app path :method method :identifier name) handler)
              (incf registered-count)))
    (list :|message| "Routes refreshed successfully"
          :|registered_count| registered-count
          :|total_routes| (length *urls*)
          :|timestamp| (local-time:now))))

(defun restart-server (&optional (port 8080))
  "Restarts the server on the given port (default 8080)"
  (format t "~%Restarting server...~%")
  
  ;; Stop current server if running
  (when *current-app*
    (handler-case
        (progn
          (jingle:stop *current-app*)
          (format t "Stopped existing server.~%"))
      (error (e)
        (format t "Warning: Error stopping server: ~A~%" e))))
  
  ;; Reload code to pick up any changes
  (format t "Reloading code...~%")
  (load (merge-pathnames "demo/src/api.lisp" 
                         (asdf:system-source-directory :jingle.demo)))
  
  ;; Create and start new server
  (format t "Starting new server on port ~A...~%" port)
  (setf *current-app* (jingle:make-app :port port))
  (register-urls *current-app*)
  (jingle:start *current-app*)
  
  (format t "~%✅ Server restarted successfully!~%")
  (format t "📋 Swagger UI: http://localhost:~A/api/docs/~%" port)
  (format t "🔧 API JSON: http://localhost:~A/api/v1/swagger.json~%" port)
  (format t "📊 Total routes: ~A~%~%" (length *urls*))
  
  *current-app*)

(defun quick-restart ()
  "Quick server restart - just calls restart-server"
  (restart-server))

(defun register-urls (app)
  "Registers the API endpoints with the provided jingle app"
  ;; Store app reference for dynamic updates
  (setf *current-app* app)
  
  (let ((swagger-ui-dist (asdf:system-relative-pathname :jingle.demo "swagger-ui")))
    (jingle:serve-directory app "/api/docs" swagger-ui-dist)
    (jingle:redirect-route app "/" "/api/docs/")
    
    ;; Add swagger.json endpoint
    (setf (jingle:route app "/api/v1/swagger.json" :method :get) #'swagger-json-handler)
    (setf (jingle:route app "/api/v1/swagger-minimal.json" :method :get) #'swagger-json-minimal-handler)
    (setf (jingle:route app "/api/v1/swagger-simple.json" :method :get) #'swagger-json-simple-handler)
    (setf (jingle:route app "/api/v1/test" :method :post) #'test-endpoint-handler)
    
    ;; Add refresh endpoint
    (setf (jingle:route app "/api/v1/refresh-routes" :method :post) #'refresh-routes-handler)
    
    ;; Register routes from *urls*
    (loop :for route-spec :in *urls*
          :for path = (getf route-spec :path)
          :for method = (getf route-spec :method)
          :for name = (getf route-spec :name)
          :for handler = (getf route-spec :handler) :do
            (setf (jingle:route app path :method method :identifier name) handler))
    t))
