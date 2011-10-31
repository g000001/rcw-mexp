;;;; rcw-mexp.asd

(cl:in-package :asdf)

(defsystem :rcw-mexp
  :serial t
  :components ((:file "package")
               (:file "rcw-mexp")))

(defmethod perform ((o test-op) (c (eql (find-system :rcw-mexp))))
  (load-system :rcw-mexp)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :rcw-mexp-internal :rcw-mexp))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

