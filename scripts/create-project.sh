#!/bin/bash

# Project generator script for cl-jingle
# Creates a new project based on the demo template structure

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <project-name>"
    echo "Example: $0 my-awesome-api"
    exit 1
fi

PROJECT_NAME="$1"

# Validate project name
if [[ ! "$PROJECT_NAME" =~ ^[a-z][a-z0-9-]*$ ]]; then
    echo "Error: Project name must start with a lowercase letter and contain only lowercase letters, numbers, and hyphens"
    exit 1
fi

PROJECT_DIR="$PROJECT_NAME"

# Check if project directory already exists
if [ -d "$PROJECT_DIR" ]; then
    echo "Error: Directory '$PROJECT_DIR' already exists"
    exit 1
fi

echo "Creating project: $PROJECT_NAME"

# Create project directory structure
mkdir -p "$PROJECT_DIR/src"
mkdir -p "$PROJECT_DIR/bin"
mkdir -p "$PROJECT_DIR/swagger-ui"

# Copy swagger-ui files from demo
cp -r demo/../swagger-ui/* "$PROJECT_DIR/swagger-ui/"

# Generate project.asd file
cat > "$PROJECT_DIR/$PROJECT_NAME.asd" << EOF
;;;; $PROJECT_NAME.asd

(defpackage :$PROJECT_NAME-system
  (:use :cl :asdf))
(in-package :$PROJECT_NAME-system)

(defsystem "$PROJECT_NAME"
  :name "$PROJECT_NAME"
  :long-name "$(echo $PROJECT_NAME | sed 's/-/ /g' | sed 's/\b\w/\U&/g')"
  :description "API server with DRY parameter system and Clojure reitit-style inline lambda handlers"
  :version "0.1.0"
  :author "Generated from cl-jingle template"
  :license "BSD 2-Clause"
  :depends-on (:jingle
               :clack-handler-hunchentoot
               :lack-middleware-accesslog
               :local-time
               :jonathan
               :dexador
               :quri
               :babel
               :cl-ppcre)
  :components ((:module "swagger-ui-dist"
                :pathname #P"swagger-ui/"
                :components ((:static-file "favicon-16x16.png")
                             (:static-file "favicon-32x32.png")
                             (:static-file "index.css")
                             (:static-file "index.html")
                             (:static-file "oauth2-redirect.html")
                             (:static-file "swagger-initializer.js")
                             (:static-file "swagger-ui-bundle.js")
                             (:static-file "swagger-ui-bundle.js.map")
                             (:static-file "swagger-ui.css")
                             (:static-file "swagger-ui.css.map")
                             (:static-file "swagger-ui-es-bundle-core.js")
                             (:static-file "swagger-ui-es-bundle-core.js.map")
                             (:static-file "swagger-ui-es-bundle.js")
                             (:static-file "swagger-ui-es-bundle.js.map")
                             (:static-file "swagger-ui.js")
                             (:static-file "swagger-ui.js.map")
                             (:static-file "swagger-ui-standalone-preset.js")
                             (:static-file "swagger-ui-standalone-preset.js.map")))
               (:module "src"
                :pathname #P"src/"
                :depends-on ("swagger-ui-dist")
                :serial t
                :components ((:file "api")
                             (:file "server"))))
  :in-order-to ((test-op (test-op "$PROJECT_NAME.test"))))
EOF

# Generate api.lisp file (simplified version of demo/src/api.lisp)

cat > "$PROJECT_DIR/src/api.lisp" << EOF
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
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) \`\`AS IS'' AND ANY EXPRESS OR
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
(defpackage :$PROJECT_NAME.api
  (:use :cl)
  (:import-from :jingle)
  (:import-from :jonathan)
  (:import-from :local-time)
  (:import-from :cl-ppcre)
  (:export
   :register-urls
   :restart-server
   :reload-code
   :quick-restart
   :refresh-routes-handler
   :*current-app*))
(in-package :$PROJECT_NAME.api)

(define-condition api-error (jingle:base-http-error)
  ()
  (:documentation "Represents a condition which will be signalled on API errors"))

(defmethod jingle:handle-error ((error api-error))
  "Handles the error and sets up the HTTP error response to be sent to clients"
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

(defun decode-json-or-throw-bad-request (content)
  "Decodes the given CONTENT or signals a Bad Request condition on errors"
  (handler-case (jonathan:parse content)
    (error ()
      (throw-bad-request-error "invalid JSON content"))))

(defparameter *urls*
  \`((:method :GET :path "/api/v1/ping" :name "ping"
      :handler (lambda (&key)
                 (list :|message| "pong"
                       :|timestamp| (local-time:now))))
    
    (:method :POST :path "/api/v1/echo" :name "echo"
      :handler (lambda (&key)
                 (let* ((content (babel:octets-to-string (jingle:request-content jingle:*request*)))
                        (body (when (and content (> (length content) 0))
                                (decode-json-or-throw-bad-request content)))
                        (message (getf body :|message|)))
                   (list :|echo| message
                         :|received_at| (local-time:now)
                         :|original_body| body)))))
  "The URLs map of our API")

(defvar *current-app* nil "Store reference to current app for dynamic updates")

(defun create-parameter-aware-handler (route-spec)
  "Creates a handler that extracts and passes parameters as specified"
  (let ((param-spec (getf route-spec :parameters))
        (handler-fn (getf route-spec :handler)))
    (lambda (raw-params)
      (jingle:with-json-response
        (cond
          ((functionp handler-fn)
           (apply handler-fn '()))
          ((symbolp handler-fn)
           (apply handler-fn '()))
          ((and (listp handler-fn) (eq (first handler-fn) 'lambda))
           (apply (eval handler-fn) '()))
          (t
           (error "Handler must be a function, symbol, or lambda expression")))))))

(defun refresh-routes-handler ()
  "Refreshes all routes from *urls* without server restart"
  (let ((app *current-app*)
        (registered-count 0))
    (when app
      (loop :for route-spec :in *urls*
            :for path = (getf route-spec :path)
            :for method = (getf route-spec :method)
            :for name = (getf route-spec :name)
            :for handler = (create-parameter-aware-handler route-spec) :do
              (setf (jingle:route app path :method method :identifier name) handler)
              (incf registered-count)))
    (list :|message| "Routes refreshed successfully"
          :|registered_count| registered-count
          :|total_routes| (length *urls*)
          :|timestamp| (local-time:now))))

(defun reload-code ()
  "Reloads the API code without restarting the server"
  (format t "~%Reloading code...~%")
  (handler-case
      (progn
        (let ((saved-app *current-app*))
          (when (boundp '*urls*)
            (makunbound '*urls*))
          (load (merge-pathnames "src/api.lisp" 
                                 (asdf:system-source-directory :$PROJECT_NAME)))
          (setf *current-app* saved-app))
        (format t "✅ Code reloaded successfully!~%")
        (when *current-app*
          (let ((result (refresh-routes-handler)))
            (format t "📊 ~A~%" (getf result :|message|))))
        t)
    (error (e)
      (format t "❌ Error reloading code: ~A~%" e)
      nil)))

(defun restart-server (&optional (port 8080))
  "Restarts the server on the given port (default 8080)"
  (format t "~%Restarting server...~%")
  
  (when *current-app*
    (handler-case
        (progn
          (jingle:stop *current-app*)
          (format t "Stopped existing server.~%"))
      (error (e)
        (format t "Warning: Error stopping server: ~A~%" e))))
  
  (setf *current-app* nil)
  (reload-code)
  
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

(defun swagger-json-handler (params)
  "Simple swagger.json handler"
  (declare (ignore params))
  (jingle:set-response-header :content-type "application/json")
  (jingle:set-response-header :access-control-allow-origin "*")
  (jingle:set-response-header :access-control-allow-methods "GET, POST, PUT, DELETE, OPTIONS")
  (jingle:set-response-header :access-control-allow-headers "Content-Type, Authorization")
  (jonathan:to-json 
    (list :|openapi| "3.0.3"
          :|info| (list :|title| "$PROJECT_NAME API" :|version| "1.0.0")
          :|paths| (list :|/api/v1/ping| 
                         (list :|get| 
                               (list :|summary| "Ping endpoint"
                                     :|operationId| "ping"
                                     :|responses| (list :|200| (list :|description| "Success"))))
                         :|/api/v1/echo|
                         (list :|post|
                               (list :|summary| "Echo endpoint"
                                     :|operationId| "echo"
                                     :|requestBody| (list :|content| (list :|application/json|
                                                                           (list :|schema| (list :|type| "object"
                                                                                                 :|properties| (list :|message| (list :|type| "string"))))))
                                     :|responses| (list :|200| (list :|description| "Success"))))))))

(defun register-urls (app)
  "Registers the API endpoints with the provided jingle app"
  (setf *current-app* app)
  
  (let ((swagger-ui-dist (asdf:system-relative-pathname :$PROJECT_NAME "swagger-ui")))
    (jingle:serve-directory app "/api/docs" swagger-ui-dist)
    (jingle:redirect-route app "/" "/api/docs/")
    
    (setf (jingle:route app "/api/v1/swagger.json" :method :get) #'swagger-json-handler)
    
    (loop :for route-spec :in *urls*
          :for path = (getf route-spec :path)
          :for method = (getf route-spec :method)
          :for name = (getf route-spec :name)
          :for handler = (create-parameter-aware-handler route-spec) :do
            (setf (jingle:route app path :method method :identifier name) handler))
    t))
EOF

# Generate README.org from template
PROJECT_TITLE=$(echo "$PROJECT_NAME" | sed 's/-/ /g' | sed 's/\b\w/\U&/g')
PACKAGE_NAME="$PROJECT_NAME"
DATE=$(date '+%Y-%m-%d')

sed -e "s/{{PROJECT_NAME}}/$PROJECT_NAME/g" \
    -e "s/{{PROJECT_TITLE}}/$PROJECT_TITLE/g" \
    -e "s/{{PACKAGE_NAME}}/$PACKAGE_NAME/g" \
    -e "s/{{DATE}}/$DATE/g" \
    "$(dirname "$0")/README.org.template" > "$PROJECT_DIR/README.org"

# Generate start-server.sh from template
sed -e "s/{{PROJECT_NAME}}/$PROJECT_NAME/g" \
    -e "s/{{PACKAGE_NAME}}/$PACKAGE_NAME/g" \
    "$(dirname "$0")/start-server.sh.template" > "$PROJECT_DIR/bin/start-server.sh"

chmod +x "$PROJECT_DIR/bin/start-server.sh"

# Generate server.lisp file
cat > "$PROJECT_DIR/src/server.lisp" << EOF
;;;; $PROJECT_NAME Server

(in-package :cl-user)
(defpackage :$PROJECT_NAME.server
  (:use :cl)
  (:import-from :$PROJECT_NAME.api
                :register-urls
                :restart-server
                :*current-app*)
  (:export
   :start-server
   :stop-server
   :restart-server))
(in-package :$PROJECT_NAME.server)

(defun start-server (&optional (port 8080))
  "Start the $PROJECT_NAME server"
  (setf *current-app* (jingle:make-app :port port))
  (register-urls *current-app*)
  (jingle:start *current-app*)
  (format t "
✅ $PROJECT_NAME server started on port ~A
" port)
  (format t "📋 Swagger UI: http://localhost:~A/api/docs/
" port)
  (format t "🔧 API JSON: http://localhost:~A/api/v1/swagger.json

" port)
  *current-app*)

(defun stop-server ()
  "Stop the $PROJECT_NAME server"
  (when *current-app*
    (jingle:stop *current-app*)
    (setf *current-app* nil)
    (format t "
Server stopped.
")))

(defun main ()
  "Main entry point"
  (start-server))
EOF

echo "✅ Project structure created successfully!"
echo "📁 Project directory: $PROJECT_DIR"
echo "📋 Next steps:"
echo "   1. cd $PROJECT_DIR"
echo "   2. ./bin/start-server.sh (starts the Swagger server)"
echo "   3. Open http://localhost:8080/api/docs/"