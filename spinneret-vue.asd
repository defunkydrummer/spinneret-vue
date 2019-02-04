(in-package :cl-user)
(defpackage spinneret-vue-asd
  (:use :cl :asdf))
(in-package :spinneret-vue-asd)

(defsystem spinneret-vue
  :version "0.1"
  :author "Flavio Egoavil"
  :license "MIT"
  :depends-on (:spinneret)     
  :serial T
  :components ((:file "spinneret-vue-patcher"))

  :description "Patches Spinneret 2.3 (and similar) to allow free tags and better vue.js compatibility"
  :long-description "No long description!")

