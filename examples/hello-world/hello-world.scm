(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix nuklear nk:)
     (prefix nuklear-glfw-opengl2 backend:))

(define width 250)
(define height 150)

(glfw:init)
(glfw:make-window width height "Demo")
(glfw:make-context-current (glfw:window))
(set!-values (width height) (glfw:get-window-size (glfw:window)))

(define context (backend:init! (glfw:window) #t))
(backend:init-font!)

(define quit? #f)
(define show-greeting? #f)

(let loop ()
  (when (and (not (glfw:window-should-close (glfw:window))) (not quit?))
    (glfw:poll-events)
    (backend:new-frame)

    (when (nk:window-begin context (nk:make-panel) "Hello World!"
                           (nk:make-rect 10 10 192 100)
                           '(border no-scrollbar movable))
      (nk:layout-row-dynamic context 30 2)
      (when (nk:button-label context "Click Me!")
        (printf "Yay\n")
        (set! show-greeting? #t))
      (when (nk:button-label context "Quit")
        (set! quit? #t))

      (when show-greeting?
        (if (nk:popup-begin context (nk:make-panel) #f "Greeting" '(dynamic)
                            (nk:make-rect 15 50 200 150))
            (begin
              (nk:layout-row-dynamic context 25 1)
              (nk:label context "Hello World!" 'centered)
              (nk:layout-row-dynamic context 25 1)
              (when (nk:button-label context "OK")
                (set! show-greeting? #f)
                (nk:popup-close context))
              (nk:popup-end context))
            (set! show-greeting? #f))))
    (nk:window-end context)

    (let-values (((width height) (glfw:get-window-size (glfw:window))))
      (gl:viewport 0 0 width height))
    (gl:clear gl:+color-buffer-bit+)
    (gl:clear-color (/ 28 255) (/ 48 255) (/ 62 255) 0)
    (backend:render!)
    (glfw:swap-buffers (glfw:window))
    (loop)))

(backend:shutdown!)
(glfw:terminate)
