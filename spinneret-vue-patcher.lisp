(in-package :spinneret) ;NOTE!

;; SPINNERET patcher for VUE

;; adds a free-form tag for inserting vue components.
;; usage: (:free-tag :name <component name> [other attributes] <body>)
;; free tag can have ANY name and can have ANY attributes.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;safety checks:
  (unless (and (packagep (find-package :spinneret.tag))
               (>= *version* 2.3)
               (boundp '*html5-elements*)
               (boundp '*unvalidated-attribute-prefixes*)
               ;; fboundp check for needed functions
               (loop for i in
                     (mapcar #'fboundp
                             '(format-attributes-plain
                               format-attributes-pretty/block
                               valid-attribute?
                               global-attribute?
                               aria-attribute?
                               permitted-attributes))
                     always T))
    (error "spinneret-vue: Can't patch Spinneret. Missing functions or symbols/incorrect version. This package was intended for spinneret version 2.3 and any version which doesn't deviate too much from it."))
  (declaim (notinline spinneret.tag::free-tag))
  (declaim (ftype (function (list function t t) (values)) spinneret.tag::free-tag))
  (defun spinneret.tag::free-tag (attrs body pre? empty?)
    (declare ;(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
     (type function body)
     (type list attrs))
    (let ((html *html*)
          (pretty *print-pretty*)
          (*pre* pre?)
          (*depth* (1+ *depth*))
          (*html-path* (cons :free-tag spinneret::*html-path*))
          ;; component name: "name" property
          (component-name (getf attrs :name)))
      (declare (dynamic-extent spinneret::*html-path*))
      (unless component-name (error "Component :name is required."))
      ;; remove component name from properties, leave the rest
      (remf attrs :name)
      (when pretty (fresh-line html))
      (write-string (format nil "<~a" component-name) html)
      (when attrs
        (eif pretty (format-attributes-pretty/block attrs html)
             (format-attributes-plain attrs html)))
      (write-char #\> html)
      (unless empty?
        (when pretty (elastic-newline html))
        (let ((*indent* (1+ *depth*)))
          (without-trailing-space
            (funcall body)))
        (when pretty (terpri html)))
      (write-string (format nil "</~a>" component-name) html)
      (values)))
  ;; push it to valid elements...
  (pushnew :free-tag spinneret::*html5-elements*)
  ;;    add :name tag as permitted for :free-tag
  ;;    (pushnew '(:free-tag :name) *permitted-attributes*)
  ;; Modify spinneret so it doesn't validate attributes on :free-tag tag.
  (defun valid-attribute? (tag name)
    (or (unvalidated-attribute? name)
        (eql name :attrs)
        (eql tag :free-tag)             ; modification here.
        (global-attribute? name)
        (aria-attribute? name)
        (let ((permitted (permitted-attributes tag)))
          (or (find name permitted :test #'string=)
              (find '* permitted)))))
  ;;---------------- :free-tag patch ready! ----------------------
  ;; define tag macro: component :name <component-name>  [attrs] <body>
  ;; if "bind" parameter is present, do a v-bind:xxx on all the parameters to bind.
  ;; I.e.:
  ;; (component :bind (|var1| "value1" |var2| "value2") :name "MyComponent"  "CONTENT")
  ;; -->
  ;; <MyComponent v-bind:var2=value2 v-bind:var1=value1>
  ;; CONTENT
  ;; </MyComponent>

  ;; other usage: :bind-same attr
  ;; (component :bind-same (|var1| |var2|) :name "MyComponent"  "CONTENT"))

  ;;  <MyComponent v-bind:var2=var2 v-bind:var1=var1>
  ;;   CONTENT
  ;;  </MyComponent>
  
  (deftag component (body attrs)
    (cond
      ((not (or (getf attrs :bind)
                (getf attrs :bind-same)))
       ;; no bind attr, no bind-same attr
       `(:free-tag ,@attrs ,@body))
      ((getf attrs :bind)
       ;; use the bind list
       (progn
         (let ((a attrs))               ;modified attrs
           (loop for (key value) on (getf attrs :bind) by #'cddr
                 do
                 (push value a)
                 (push (intern (format nil "v-bind:~a" key) :keyword) ;create keyword
                       a))
           (remf a :bind)      ;remove :bind prop on our property list
           
           `(:free-tag ,@a 
                       ,@body))))
      ((getf attrs :bind-same)
       ;; bind to vars of the same name.
       (progn
         (let ((a attrs))               ;modified attrs
           (loop for key in (getf attrs :bind-same) 
                 do
                 (push (symbol-name key) a)
                 (push (intern (format nil "v-bind:~a" key) :keyword) ;create keyword
                       a))
           (remf a :bind-same) 
           
           `(:free-tag ,@a 
                       ,@body)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Modify spinneret html valid attributes so any attribute that starts with v- is valid
  ;; for any tag. 
  (pushnew "v-"                         ; v-bind, v-on, v-if, etcetera
           spinneret::*unvalidated-attribute-prefixes*
           :test #'equal)

  ;; VUE: slot 
  ;; to be deprecated on vue3
  (pushnew "slot"                        
           spinneret::*unvalidated-attribute-prefixes*
           :test #'equal)

                                        ; @ prefix 
  (pushnew "@"                         ; equal to v-on
           spinneret::*unvalidated-attribute-prefixes*
           :test #'equal)

  (pushnew ":"                         ; shorthand for v-bind
           spinneret::*unvalidated-attribute-prefixes*
           :test #'equal))
