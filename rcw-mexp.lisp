;;;; rcw-mexp.lisp
;;
;; Richard C. Waters' macroexpand-all
;; Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.
;;
;; Permission to use, copy, modify, and distribute this software and its
;; documentation for any purpose and without fee is hereby granted,
;; provided that this copyright and permission notice appear in all
;; copies and supporting documentation, and that the name of M.I.T. not
;; be used in advertising or publicity pertaining to distribution of the
;; software without specific, written prior permission. M.I.T. makes no
;; representations about the suitability of this software for any
;; purpose.  It is provided "as is" without express or implied warranty.
;;
;;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;    SOFTWARE.

#-(or gcl)
(cl:in-package :rcw-mexp-internal)

#-(or gcl)
(def-suite rcw-mexp)

#-(or gcl)
(in-suite rcw-mexp)

#+gcl
(progn
  (in-package :mexp)
  (export '(macroexpand-all)) )

(defmacro grab-env (fn x &environment env)
  `',(funcall fn x env))

(defun aug-env (env form fn x)
  #+:gcl (evalhook `(,@form (grab-env ,fn ,x))
                   nil
                   nil
                   env)
  #+:sbcl (sb-int:eval-in-lexenv `(,@form (grab-env ,fn ,x)) env))

(defun convert-env (env) env)

(defun with-env (env form fn x)
  (aug-env (convert-env env) form fn x))

#+*noenv* (defun with-env (env form fn x)
            (declare (ignore form))
            (funcall fn x env))

(defun macroexpand-all
       (f &optional (env (or #+:sbcl (sb-kernel::make-null-lexenv))))
  (mexp (copy-tree f) env) )

(defun mexp (f env &aux (flag t) m)
  (loop
    (cond ((atom f)
           (return f))
          ((not (symbolp (car f)))
           (return (all-mexp f env)))
          ((setq m (get (car f) 'mexp))
           (return (funcall m f env)))
          ((not flag)
           (return (funcall-mexp f env))))
    (multiple-value-setq (f flag)
                         (macroexpand-1 f env))))

(defun all-mexp (list env)
  (do ((f list (cdr f))
       (r () (cons (mexp (car f) env) r)) )
      ((atom f) (nreconc r f)) ))

(defun funcall-mexp (f env)
  `(,(car f) ,@(all-mexp (cdr f) env)) )

(defun quote-mexp (f env)
  (declare (ignore env))
  f )

(defun block-mexp (f env)
  `(,(car f)
     ,(cadr f)
     ,@(all-mexp (cddr f) env)))

(defun let-mexp (f env)
  `(,(car f)
     ,(mapcar #'(lambda (p)
                  (bind-mexp p env))
              (cadr f))
     ,@(all-mexp (cddr f) env)))

(defun bind-mexp (p env)
  (if (and (consp p) (consp (cdr p)))
      (list (car p) (mexp (cadr p) env))
      p ))

(defun lambda-mexp (f env)
  `(,(car f)
     ,(mapcar #'(lambda (p)
                  (arg-mexp p env) )
              (cadr f) )
     ,@(all-mexp (cddr f) env) ))

(defun arg-mexp (arg env)
  (if (and (consp arg) (consp (cdr arg)))
      `(,(car arg)
         ,(mexp (cadr arg) env)
         ,@(cddr arg) )
      arg ))

(defun get-var (b)
  (if (consp b) (car b) b) )

(defun get-val (b)
  (eval (if (consp b) (cadr b) nil)))

(defun compiler-let-mexp (f env)
  (progv (mapcar #'get-var (cadr f))
         (mapcar #'get-val (cadr f))
    (mexp
     (if (null (cdddr f))
         (caddr f)
         `(let nil ,@(cddr f)) )
     env )))

(defun macrolet-mexp (f env)
  (with-env env `(macrolet ,(cadr f))
            #'mexp
            (if (null (cdddr f))
                (caddr f)
                `(let nil ,@(cddr f)) )))

(defun flet-mexp (f env)
  `(flet
       ,(all-lambda-mexp (cadr f) env)
     ,@(with-env env `(flet ,(cadr f))
                 #'all-mexp
                 (cddr f) )))

(defun labels-mexp (f env)
  (with-env env `(labels ,(cadr f))
            #'labels-mexp-2 f))

(defun labels-mexp-2 (f env)
  `(labels
       ,(all-lambda-mexp (cadr f) env)
     ,@(all-mexp (cddr f) env) ))

(defun all-lambda-mexp (list env)
  (mapcar #'(lambda (f)
              (lambda-mexp f env) )
          list ))

(defun named-lambda-mexp (f env)
  `(,(car f)
     ,(cadr f)
     ,(mapcar #'(lambda (p)
                  (arg-mexp p env) )
              (caddr f) )
     ,@(all-mexp (cdddr f) env) ))

(mapc #'(lambda (x)
          (setf (get (car x) 'mexp)
                (eval (cadr x)) ))
      '((block #'block-mexp )
        (catch #'funcall-mexp )
        (compiler-let #'compiler-let-mexp)
        (declare #'quote-mexp )
        (eval-when #'block-mexp )
        (flet #'flet-mexp )
        (function #'funcall-mexp )
        (go #'quote-mexp )
        (if #'funcall-mexp )
        (labels #'labels-mexp )
        (lambda #'lambda-mexp )
        (let #'let-mexp )
        (let* #'let-mexp )
        (macrolet #'macrolet-mexp )
        (multiple-value-call #'funcall-mexp)
        (multiple-value-prog1 #'funcall-mexp)
        (progn #'funcall-mexp )
        (progv #'funcall-mexp )
        (quote #'quote-mexp )
        (return-from #'block-mexp )
        (setq #'funcall-mexp )
        (tagbody #'funcall-mexp )
        (the #'block-mexp )
        (throw #'funcall-mexp )
        (unwind-protect #'funcall-mexp)
        #+sbcl (sb-int:named-lambda #'named-lambda-mexp)
        ))


#-(or gcl)
(test macroexpand-all
  (is (eq (macroexpand-all '(macrolet ((hello () :hello-outer))
                             (macrolet ((hello () :hello-inner))
                               (hello))))
          :hello-inner))
  (is (equal (macroexpand-all '(macrolet ((hello () :hello-outer))
                                (flet ((hello () :hello-inner))
                                  (hello))))
             '(FLET ((HELLO ()
                      :HELLO-INNER))
               (HELLO)))))

;; eof