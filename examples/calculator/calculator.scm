(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix nuklear nk:)
     (rename format (format cl-format)))

(require-library nuklear-glfw-opengl2)
(import (prefix nuklear-glfw-opengl2 backend:))

(define width 1200)
(define height 800)

(glfw:init)
(glfw:make-window width height "Calculator")
(glfw:make-context-current (glfw:window))
(set!-values (width height) (glfw:get-window-size (glfw:window)))

(define context (backend:init! (glfw:window) #t))
(backend:init-font!)

(define op #f)
(define acc 0)
(define term 0)
(define current 0)
(define layout (nk:make-panel))
(define background (nk:rgb->color 28 48 62))

(let loop ()
  (when (and (not (glfw:window-should-close (glfw:window))))
    (glfw:poll-events)
    (backend:new-frame)

    (when (nk:window-begin context layout "Calculator"
                           (nk:make-rect 10 10 180 250)
                           '(border no-scrollbar movable))
      (nk:layout-row-dynamic context 35 1)
      (set! buffer (cl-format #f "~,2f" current))
      (set! buffer (nk:edit-string context 'simple buffer 255 'float))
      (set! current (string->number buffer))

      (nk:layout-row-dynamic context 35 4)
      (for-each
       (lambda (item)
         (if (char-numeric? item)
             (let* ((label (format "~c" item))
                    (number (string->number label)))
               (when (nk:button-label context label)
                 (if (eqv? item #\0)
                     (set! term (* term 10.0))
                     (set! term (+ (* term 10.0) number)))
                 (set! current term)))
             (when (nk:button-label context (format "~c" item))
               (case item
                 ((#\+ #\- #\* #\/ #\=)
                  (if (not op)
                      (set! acc term)
                      (case op
                        ((#\+) (set! acc (+ acc term)))
                        ((#\-) (set! acc (- acc term)))
                        ((#\*) (set! acc (* acc term)))
                        ((#\/) (set! acc (/ acc term)))))
                  (set! term 0)
                  (set! current acc)
                  (if (eqv? item #\=)
                      (set! op #f)
                      (set! op item)))
                 ((#\C)
                  (set! op #f)
                  (set! acc 0)
                  (set! term 0)
                  (set! current 0))))))
       '(#\7 #\8 #\9 #\+
         #\4 #\5 #\6 #\-
         #\1 #\2 #\3 #\*
         #\C #\0 #\= #\/)))
    (nk:window-end context)

    (let-values (((width height) (glfw:get-window-size (glfw:window))))
      (gl:viewport 0 0 width height))
    (gl:clear gl:+color-buffer-bit+)
    (apply gl:clear-color (nk:color->rgba-floats background))
    (backend:render!)
    (glfw:swap-buffers (glfw:window))
    (loop)))

(backend:shutdown!)
(glfw:terminate)
