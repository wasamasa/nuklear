(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix nuklear nk:)
     (rename format (format cl-format))
     srfi-1)

(require-library nuklear-glfw-opengl2)
(import (prefix nuklear-glfw-opengl2 backend:))

(define width 1200)
(define height 800)

(glfw:init)
(glfw:make-window width height "Node Editor")
(glfw:make-context-current (glfw:window))
(set!-values (width height) (glfw:get-window-size (glfw:window)))

(define context (backend:init! (glfw:window) #t))
(backend:init-font!)

(define background (nk:rgb->color 28 48 62))
(define dark-gray (nk:rgb->color 50 50 50))
(define light-gray (nk:rgb->color 100 100 100))

(define max-id (make-parameter 0))

(define-record node id name bounds color in-count out-count)
(define-record link in-id in-slot out-id out-slot in out)
(define-record linking active? node in-id in-slot)
(define-record node-editor initialized? nodes links bounds show-grid? scrolling linking)

(define (node-editor-find editor id)
  (let ((nodes (node-editor-nodes editor)))
    (find (lambda (node) (= (node-id node) id)) nodes)))

(define (node-editor-add! editor name bounds color in-count out-count)
  (let ((node (make-node (max-id) name bounds color in-count out-count))
        (nodes (node-editor-nodes editor)))
    (max-id (add1 (max-id)))
    (node-editor-nodes-set! editor (append nodes (list node)))))

(define (node-editor-link! editor in-id in-slot out-id out-slot)
  (let ((link (make-link in-id in-slot out-id out-slot #f #f))
        (links (node-editor-links editor)))
    (node-editor-links-set! editor (cons link links))))

(define (node-editor-reshuffle! editor id)
  (let ((nodes (node-editor-nodes editor)))
    (receive (front back) (break (lambda (node) (= (node-id node) id)) nodes)
     (node-editor-nodes-set! editor (append front (cdr back) (list (car back)))))))

(define (node-editor-init! editor)
  (node-editor-add! editor "Source" (nk:make-rect 40 10 180 220) (nk:rgb->color 255 0 0) 0 1)
  (node-editor-add! editor "Source" (nk:make-rect 40 260 180 220) (nk:rgb->color 0 255 0) 0 1)
  (node-editor-add! editor "Combine" (nk:make-rect 400 100 180 220) (nk:rgb->color 0 0 255) 2 2)
  (node-editor-link! editor 0 0 2 0)
  (node-editor-link! editor 1 0 2 1)
  (node-editor-show-grid?-set! editor #t))

(define editor (make-node-editor #f '() '() #f #f (nk:make-vec2 0 0)
                                 (make-linking #f #f #f #f)))

(define (fpmod x y)
  (fp- x (fp* (fpfloor (fp/ x y)) y)))

(let loop ()
  (when (and (not (glfw:window-should-close (glfw:window))))
    (glfw:poll-events)
    (backend:new-frame)

    (when (not (node-editor-initialized? editor))
      (node-editor-init! editor)
      (node-editor-initialized?-set! editor #t))

    (when (nk:window-begin context (nk:make-panel) "Node Editor"
                           (nk:make-rect 10 10 800 600)
                           '(border no-scrollbar movable closable))
      (let ((input (nk:context-input context))
            (canvas (nk:window-canvas context))
            (total-space (nk:window-content-region context)))
        (nk:layout-space-begin context #t (nk:rect-h total-space) 42)

        (let ((size (nk:layout-space-bounds context))
              (layout (nk:make-panel))
              (updated #f))
          (when (node-editor-show-grid? editor)
            (let ((grid-size 32.0))
              (do ((x (fpmod (- (nk:rect-x size)
                                (nk:vec2-x (node-editor-scrolling editor)))
                             grid-size)
                      (+ x grid-size)))
                  ((> x (nk:rect-w size)))
                (nk:stroke-line
                 canvas (+ x (nk:rect-x size)) (nk:rect-y size)
                 (+ x (nk:rect-x size)) (+ (nk:rect-y size) (nk:rect-h size))
                 1.0 dark-gray))
              (do ((y (fpmod (- (nk:rect-y size)
                                (nk:vec2-y (node-editor-scrolling editor)))
                             grid-size)
                      (+ y grid-size)))
                  ((> y (nk:rect-h size)))
                (nk:stroke-line
                 canvas (nk:rect-x size) (+ y (nk:rect-y size))
                 (+ (nk:rect-x size) (nk:rect-w size))
                 (+ y (nk:rect-y size)) 1.0 dark-gray))))

          (for-each
           (lambda (node)
             (let* ((bounds (node-bounds node))
                    (scroll (node-editor-scrolling editor))
                    (rect (nk:make-rect (- (nk:rect-x bounds) (nk:vec2-x scroll))
                                        (- (nk:rect-y bounds) (nk:vec2-y scroll))
                                        (nk:rect-w bounds)
                                        (nk:rect-h bounds))))
               (nk:layout-space-push context rect))

             (when (nk:group-begin context layout (node-name node)
                                   '(movable no-scrollbar border title))
               (let* ((bounds (node-bounds node))
                      (bounds* (nk:layout-space-rect-to-screen context bounds))
                      (nodes (node-editor-nodes editor)))
                 (when (and (nk:input-mouse-clicked? input 'left bounds)
                            (not (and (not (= (node-id node) (node-id (car nodes))))
                                      (nk:input-mouse-clicked? input 'left bounds*)))
                            (not (= (node-id node) (node-id (last nodes)))))
                   (set! updated node)))
               (nk:layout-row-dynamic context 25 1)
               (let ((color (node-color node)))
                 (nk:button-color context color)
                 (nk:color-r-set! color (nk:property-int context "#R:" 0 (nk:color-r color) 255 1 1))
                 (nk:color-g-set! color (nk:property-int context "#G:" 0 (nk:color-g color) 255 1 1))
                 (nk:color-b-set! color (nk:property-int context "#B:" 0 (nk:color-b color) 255 1 1))
                 (nk:color-a-set! color (nk:property-int context "#A:" 0 (nk:color-a color) 255 1 1)))
               (nk:group-end context))

             (let* ((bounds (nk:layout-space-rect-to-local context (nk:panel-bounds layout)))
                    (scroll (node-editor-scrolling editor))
                    (style (nk:context-style context))
                    (border-width (nk:style-window-border style)))
               (nk:rect-x-set! bounds (+ (- (nk:vec2-x scroll) border-width) (nk:rect-x bounds)))
               (nk:rect-y-set! bounds (+ (- (nk:vec2-y scroll) border-width) (nk:rect-y bounds)))
               (nk:rect-w-set! bounds (+ (* 2 border-width) (nk:rect-w bounds)))
               (nk:rect-h-set! bounds (+ (* 2 border-width) (nk:rect-h bounds)))
               (node-bounds-set! node bounds))

             (let* ((in-count (node-in-count node))
                    (out-count (node-out-count node))
                    (bounds (nk:panel-bounds layout))
                    (height (nk:rect-h bounds))
                    (in-space (/ height (add1 in-count)))
                    (out-space (/ height (add1 out-count))))
               (let loop ((i 0))
                 (when (< i out-count)
                   (let* ((x (- (+ (nk:rect-x bounds) (nk:rect-w bounds)) 4))
                          (y (+ (nk:rect-y bounds) (* out-space (add1 i))))
                          (w 8)
                          (h 8)
                          (circle (nk:make-rect x y w h)))
                     (nk:fill-circle canvas circle light-gray)
                     (when (nk:input-mouse-click-down-in-rect? input 'left circle #t)
                       (let ((linking (node-editor-linking editor)))
                         (linking-active?-set! linking #t)
                         (linking-node-set! linking node)
                         (linking-in-id-set! linking (node-id node))
                         (linking-in-slot-set! linking i))
                       (let* ((l0 (nk:make-vec2 (+ (nk:rect-x circle) 3)
                                                (+ (nk:rect-y circle) 3)))
                              (l0-x (nk:vec2-x l0))
                              (l0-y (nk:vec2-y l0))
                              (l1 (nk:mouse-pos (nk:input-mouse input)))
                              (l1-x (nk:vec2-x l1))
                              (l1-y (nk:vec2-y l1)))
                         (nk:stroke-curve
                          canvas l0-x l0-y (+ l0-x 50) l0-y
                          (- l1-x 50) l1-y l1-x l1-y 1 light-gray)))
                     (loop (add1 i)))))
               (let loop ((i 0))
                 (when (< i in-count)
                   (let* ((x (- (nk:rect-x bounds) 4))
                          (y (+ (nk:rect-y bounds) (* in-space (add1 i))))
                          (w 8)
                          (h 8)
                          (circle (nk:make-rect x y w h)))
                     (nk:fill-circle canvas circle light-gray)
                     (when (and (nk:input-mouse-released? input 'left)
                                (nk:input-mouse-hovering-in-rect? input circle))
                       (let ((linking (node-editor-linking editor)))
                         (when (and (linking-active? linking)
                                    (not (= (node-id (linking-node linking))
                                            (node-id node))))
                           (linking-active?-set! linking #f)
                           (node-editor-link! editor (linking-in-id linking)
                                              (linking-in-slot linking)
                                              (node-id node) i)))))
                   (loop (add1 i))))))
           (node-editor-nodes editor))

          (let ((linking (node-editor-linking editor)))
            (when (and (linking-active? linking)
                       (nk:input-mouse-released? input 'left))
              (linking-active?-set! linking #f)
              (linking-node-set! linking #f)
              (printf "linking failed\n")))

          (for-each
           (lambda (link)
             (let* ((bounds (nk:panel-bounds layout))
                    (height (nk:rect-h bounds))
                    (scroll (node-editor-scrolling editor))
                    (in (node-editor-find editor (link-in-id link)))
                    (out (node-editor-find editor (link-out-id link)))
                    (in-bounds (node-bounds in))
                    (out-bounds (node-bounds out))
                    (in-space (/ height (add1 (node-out-count in))))
                    (out-space (/ height (add1 (node-in-count out))))
                    (l0 (nk:make-vec2 (+ (nk:rect-x in-bounds) (nk:rect-w in-bounds))
                                      (+ (nk:rect-y in-bounds)
                                         (* in-space (add1 (link-in-slot link))) 3)))
                    (l0 (nk:layout-space-to-screen context l0))
                    (l1 (nk:make-vec2 (nk:rect-x out-bounds)
                                      (+ (nk:rect-y out-bounds)
                                         (* out-space (add1 (link-out-slot link))) 3)))
                    (l1 (nk:layout-space-to-screen context l1)))
               (nk:vec2-x-set! l0 (- (nk:vec2-x l0) (nk:vec2-x scroll)))
               (nk:vec2-y-set! l0 (- (nk:vec2-y l0) (nk:vec2-y scroll)))
               (nk:vec2-x-set! l1 (- (nk:vec2-x l1) (nk:vec2-x scroll)))
               (nk:vec2-y-set! l1 (- (nk:vec2-y l1) (nk:vec2-y scroll)))
               (let ((l0-x (nk:vec2-x l0))
                     (l0-y (nk:vec2-y l0))
                     (l1-x (nk:vec2-x l1))
                     (l1-y (nk:vec2-y l1)))
               (nk:stroke-curve canvas l0-x l0-y (+ l0-x 50) l0-y (- l1-x 50)
                                l1-y l1-x l1-y 1.0 light-gray))))
           (node-editor-links editor))

          (when updated
            (node-editor-reshuffle! editor (node-id updated)))

          (when (nk:contextual-begin context (nk:make-panel) '()
                                     (nk:make-vec2 100 220)
                                     (nk:window-bounds context))
            (nk:layout-row-dynamic context 25 1)
            (when (nk:contextual-item-label context "New" 'centered)
              (node-editor-add! editor "New" (nk:make-rect 400 260 180 220)
                                (nk:rgb->color 255 255 255) 1 2))
            (when (nk:contextual-item-label context (if (node-editor-show-grid? editor)
                                                        "Hide Grid"
                                                        "Show Grid")
                                            'centered)
              (node-editor-show-grid?-set! editor (not (node-editor-show-grid? editor))))
            (nk:contextual-end context)))

        (nk:layout-space-end context)
        (when (and (nk:input-mouse-hovering-in-rect? input (nk:window-bounds context))
                   (nk:input-mouse-down? input 'middle))
          (let* ((scroll (node-editor-scrolling editor))
                 (delta (nk:mouse-delta (nk:input-mouse input))))
            (nk:vec2-x-set! scroll (+ (nk:vec2-x scroll) (nk:vec2-x delta)))
            (nk:vec2-y-set! scroll (+ (nk:vec2-y scroll) (nk:vec2-y delta)))))))
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
