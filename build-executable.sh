sbcl --eval "(push (truename #P\"~/zmygit/cl-rest-api-server/cl-rest-api-server/\") asdf:*central-registry*)" --eval "(asdf:make :my-server/executable)"
