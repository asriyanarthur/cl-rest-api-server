(defsystem "my-server"
  :depends-on ("ningle" "clack" "cl-json" "local-time" "parse-number" "hunchentoot" "clack-handler-hunchentoot")
  :components ((:file "my-server")))

(defsystem "my-server/executable"
  :build-operation program-op
  :build-pathname "my-server" ;; shell name
  :entry-point "my-server::my-server-start" ;; thunk
  :depends-on ("my-server")
  :components ((:file "my-server")))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

;; sbcl --eval "(push (truename #P\"~/zmygit/cl-rest-api-server/\") asdf:*central-registry*)" --eval "(asdf:make :my-server/executable)"

