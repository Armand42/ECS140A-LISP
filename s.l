


(defun isLL2 (q) 
  (and (listp q) (equal (length q) 2))          ;; and takes 2 boolean arguments: is list and length = 2 == true ?
)


(defun isLL2a (q)
  (and (listp q) (equal (length q) 2) (listp (cadr q))) ;;and takes 3 boolean arguments: is list and length = 2 and second one is a list == true ?
)


(defun isLpairs-recursive (q)
  (cond 
        ((not (listp q)) nil)
        ((null q) t)
        ((isLL2 (car q)) (isLpairs-recursive (cdr q)))
  ) 
)

(defun isLpairs-iterative (q)
    (if (not (listp q)) nil
    (do 
        ((x q (cdr x))
        (y t (setq y (isLL2 (car x)))))
     ((or (not y) (endp x)) y))
    )
)

(defun isLpairs-every (q)
    (if (not (listp q)) nil (every #'isLL2 q))
)


(defun isLpairs1st-recursive (q) 
    (cond 
        ((not (isLpairs-recursive q)) nil)
        ((null q) t)
        ((= (length q) 1) t)
        ((equal (car (car q)) (car (cadr q))) (isLpairs1st-recursive (cdr q)))
    )
)

(defun isLpairs1st-iterative (q) 
    (defun helper (l)
        (setq f (car (car l)))
        (setq s (cdr l))
        (do 
            ((x s (cdr x))
            (y t (setq y (equal (car (car x)) f))))
            ((or (not y) (endp x)) y)
        )
    )
    (cond 
        ((not (isLpairs-iterative q)) nil)
        ((null q) t)
        ((= (length q) 1) t)
        ((helper q) t)
    )
)

(defun isLpairs1st-every (q)
    (defun helper (l)
        (setq f (car (car l)))
        (setq s (cdr l))
        (defun helper2 (x) 
            (equal f (car x))
        )
        (every #'helper2 s)
    )
    (cond 
        ((not (isLpairs-every q)) nil)
        ((null q) t)
        ((= (length q) 1) t)
        ((helper q) t)
    )
)


(defun distribute-recursive (q)
    (cond 
        ((not (isLL2a q)) nil) 
        ((null (cadr q)) nil)
        (t 
            (cons 
                (list (car q) (car (cadr q))) ; (1 a)
                (distribute-recursive (list (car q) (cdr (cadr q))))
            )
        )
    )
)

(defun distribute-iterative (q)
    (cond
        ((not (isLL2a q)) nil)
        (t 
            (setq head (car q))  ; head = 1
            (setq tail (cadr q))  ; tail = (a b c)
            (do 
                (
                    (x tail (cdr x))  
                    (y nil (push (list head (car x)) y))  ; y = ((1 c) (1 b) (1 a))
                )
                ((endp x) (reverse y))
            )
        )
    )
)

(defun distribute-mapcar (q)
    (cond
        ((not (isLL2a q)) nil)
        (t 
            (defun addhead (x) (list (car q) x))
            (mapcar #'addhead (cadr q))
        )
    )
)



(defun undistribute-recursive (q)
    (cond 
        ((not (isLpairs1st-recursive q)) nil)
        ((null q) nil)
        (t 
            (list 
                (car (car q)) 
                (cons 
                    (cadr (car q)) 
                    (cadr (undistribute-recursive (cdr q)))
                )
            )
        )
    
    )
)

(defun undistribute-iterative (q) 
    (cond
        ((not (isLpairs1st-iterative q)) nil) 
        ((null q) nil)
        (t
            (setq head (car (car q)))  ; head = 1  
            (do 
                (
                    (x q (cdr x))  ; x = ((1 a) (1 b) (1 c)) x = ((1 b) (1 c)) x = ((1 c)) x = () 
                    (y nil (push (cadr (car x)) y))  ; y = (c b a)
                )
                ((endp x) (list head (reverse y)))
            )
        )
    )
)


(defun undistribute-mapcar (q)
    (cond
        ((not (isLpairs1st-every q)) nil)
        ((null q) nil)                  ; checks for empty list
        (t
            (setq head (car (car q)))   ; head = 1
            (list head (mapcar #'cadr q))          ; gives (a b c) and adds head
        )
    )
)





(defun my-every (fun q)
    (if (null q) 
            t 
            (and 
                (funcall fun (car q)) ;funcall whenever a function is the input
                (my-every fun (cdr q))
            )
    )
) 




(defun lenLL (q)
    (cond 
        ((not (listp q)) 0)         ;; each line is a separate condition
        ((null q) 0)
        (t (max (length q) (lenLL (car q)) (lenLL (cdr q))))
    )
)





(defun simplify-not (x)
    (cond 
        ((not (listp x)) x)
        ((null x) x)
        (t
            (defun basecase (q)
                (cond
                    ((equal q '(not t)) nil)
                    ((equal q '(not nil)) t)
                    (t q)
                )
            )
            (basecase (cons (simplify-not (car x)) (simplify-not(cdr x))))
        )
    )

)


(defun simplify (x)
    (cond 
        ((not (listp x)) x)
        ((null x) x)
        (t
            (defun handleand (q)
                (cond
                    ((null q) t)
                    ((member nil q) nil)
                    (t
                        (setq newq (remove t q))
                        (cond 
                            ((null newq) t)
                            ((equal (length newq) 1) (car newq))
                            (t (cons 'and newq))
                        )
                    )
                )
            )
            (defun handleor (q)
                (cond
                    ((null q) nil)
                    ((member t q) t)
                    (t
                        (setq newq (remove nil q))
                        (cond
                            ((null newq) nil)
                            ((equal (length newq) 1) (car newq))
                            (t (cons 'or newq))
                        )
                        
                    )
                )
            )
            (cond 
                ((equal (car x) 'and) (handleand (mapcar #'simplify (cdr x))))          ;; and case
                ((equal (car x) 'or) (handleor (mapcar #'simplify (cdr x))))            ;; or case
                (t 
                    (simplify-not (cons (simplify (car x)) (mapcar #'simplify (cdr x))))    ;; neither and nor or
                )
            )
        )
    )

)



(defun check (x)
    (setq y (simplify x))
    (setq temp1 (eval (list 'let '((a t) (b nil) (c 4) (and 1) (or 2)) x)))
    (setq temp2 (eval (list 'let '((a t) (b nil) (c 4) (and 1) (or 2)) y)))
    
    (if (equal temp1 temp2) 
        (list t temp1 temp2)
        (list nil temp1 temp2)
    )
)

