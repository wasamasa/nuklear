(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix nuklear nk:)
     extras)

(require-library nuklear-glfw-opengl2)
(import (prefix nuklear-glfw-opengl2 backend:))

(define width 1200)
(define height 800)

(glfw:init)
(glfw:make-window width height "Demo")
(glfw:make-context-current (glfw:window))
(set!-values (width height) (glfw:get-window-size (glfw:window)))

(define context (backend:init! (glfw:window) #t))
(backend:init-font!)
;; (backend:init-font! context "DroidSans.ttf" 15)

(define quit? #f)
(define hard? #f)
(define property 20)
(define background (nk:rgb->color 28 48 62))

(let loop ()
  (when (and (not (glfw:window-should-close (glfw:window))) (not quit?))
    (glfw:poll-events)
    (backend:new-frame)

    (when (nk:window-begin context (nk:make-panel) "Demo"
                           (nk:make-rect 50 50 230 250)
                           '(border movable scalable minimizable title))
      (nk:layout-row-static context 30 80 1)
      (when (nk:button-label context "button")
        (printf "button pressed\n"))

      (nk:layout-row-dynamic context 30 2)
      (when (nk:option-label context "easy" (not hard?))
        (set! hard? #f))
      (when (nk:option-label context "hard" hard?)
        (set! hard? #t))

      (nk:layout-row-dynamic context 25 1)
      (set! property (nk:property-int context "Compression:" 0 property 100 10 1))

      (nk:layout-row-dynamic context 20 1)
      (nk:label context "background:" 'left)

      (nk:layout-row-dynamic context 25 1)
      (when (nk:combo-begin-color context (nk:make-panel) background 400)
        (nk:layout-row-dynamic context 120 1)
        (set! background (nk:color-picker context background))
        (nk:layout-row-dynamic context 25 1)
        (set! (nk:color-r background) (nk:property-int context "#R:" 0 (nk:color-r background) 255 1 1))
        (set! (nk:color-g background) (nk:property-int context "#G:" 0 (nk:color-g background) 255 1 1))
        (set! (nk:color-b background) (nk:property-int context "#B:" 0 (nk:color-b background) 255 1 1))
        (set! (nk:color-a background) (nk:property-int context "#A:" 0 (nk:color-a background) 255 1 1))
        (nk:combo-end context)))
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
