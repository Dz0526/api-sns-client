(in-package :cl-user)
(ql:quickload :dexador)
(ql:quickload :jonathan)

"https://....", (($orderby . _created_at ...) ($limit . 20))
(defun compose (fn &rest functions)
  (reduce (lambda (f g)
            (lambda (&rest args)
              (funcall f (apply g args))))
          functions
          :initial-value fn))

(defun urlparam (url param)
  (flet ((encode (chr)
           (format nil "%~x" (char-code chr))))
    (mapcar 
    (compose 
      (lambda (lis)
        (apply #'concatenate 'string)) 
      (lambda (str)
        (map 'list
             (lambda (chr)
               (if (string= chr #\Space)
                   (encode chr)
                   (string chr)))
             str))
      (lambda (lis)
              (format nil "~a=~a&" (car lis) (cdr lis))))
      param)))

(urlparam "https://" '(("$orderby" . "_created_at ") ("unti" . "kami")))
(concatenate 'string (values-list '("a" "i")))
(apply #'concatenate 'string '("a" "i"))


(defun getpost ()
  (let ((post (jojo:parse (dex:get "https://versatileapi.herokuapp.com/api/text/all?$orderby=_created_at%20desc&$limit=20&")) ))
    (car post)))

(getpost)

(cdr (cons "ai" "ue"))
(format nil "%~x" (char-code #\Space))
(string= #\a #\a)

(position #\a "baobab" :start 0)
