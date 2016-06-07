(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix nuklear nk:))

(import (prefix nuklear-glfw-opengl2 backend:))

(define width 250)
(define height 150)

(glfw:init)
(glfw:make-window width height "Demo")
(glfw:make-context-current (glfw:window))
(set!-values (width height) (glfw:get-window-size (glfw:window)))

(define ctx (backend:init! (glfw:window) #t))
(backend:init-fonts! (backend:make-font "ProggyClean.ttf" 12))

(define quit? #f)

(let loop ()
  (when (and (not (glfw:window-should-close (glfw:window))) (not quit?))
    (glfw:poll-events)
    (backend:new-frame)

    ;; TODO: nk example code

    ;; TODO: nk bg color set
    (let-values (((width height) (glfw:get-window-size (glfw:window))))
      (gl:viewport 0 0 width height))
    (gl:clear gl:+color-buffer-bit+)
    (gl:clear-color (/ 28 255) (/ 48 255) (/ 62 255) 0)
    (backend:render!)
    (glfw:swap-buffers (glfw:window))
    (loop)))

(backend:shutdown!)
(glfw:terminate)
