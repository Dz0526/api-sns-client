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
    (format nil "~a?~{~A~}"
            url
            (mapcar 
            (compose 
              (lambda (lis)
                (apply #'concatenate 'string lis)) 
              (lambda (str)
                (map 'list
                     (lambda (chr)
                       (if (string= chr #\Space)
                           (encode chr)
                           (string chr)))
                     str))
              (lambda (lis)
                      (format nil "~a=~a&" (car lis) (cdr lis))))
      param))))

(jojo:parse (dex:get (urlparam "https://versatileapi.herokuapp.com/api/text/all" '(("$orderby" . "_created_at desc") ("$limit" . "20"))) ) )
(concatenate 'string (values-list '("a" "i")))
(apply #'concatenate 'string '("a" "i"))

(defun getext (num)
  (let ((url (urlparam "https://versatileapi.herokuapp.com/api/text/all" `(("$orderby" . "_created_at desc") ("$limit" . ,(write-to-string num)))) ))
    (jojo:parse (dex:get url))))

(defun getext-id (id)
  (let ((url (format nil "~a~a" "https://versatileapi.herokuapp.com/api/text/" id) ) )
    (jojo:parse (dex:get url))))

(defun postext (text))

(defun printext (hashs)
  (loop for table
        in hashs
        do (format t "?: ~a" (getf table ':|text|))
        do (princ #\newline)  
        do (if (getf table ':|in_reply_to_text_id|)
               (progn (format t "replyed ?: ~a" (getf (getext-id (getf table ':|in_reply_to_text_id|)) ':|text|))
                      (princ #\newline)))
        do (princ "-----------------------------------------------------------------------")
        do (princ #\newline)))

(gethash 'text table)

(getext 3)
(getext-id "826e43e4-b248-4470-bc21-482a93879c94") 
(printext (getext 10))

