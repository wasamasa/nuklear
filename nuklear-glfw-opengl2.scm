(module nuklear-glfw-opengl2
  (anti-alias? max-vertex-buffer max-element-buffer
   init! init-font! new-frame render! shutdown!)

(import chicken scheme foreign)

(use lolevel)

#>
#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT

#define NK_GLFW_GL2_IMPLEMENTATION
#include "nuklear.h"
#include "nuklear_glfw_gl2.h"
<#

;;; parameters

(define anti-alias? (make-parameter #t))
(define max-vertex-buffer (make-parameter (* 512 1024)))
(define max-element-buffer (make-parameter (* 128 1024)))

;;; auxiliary records

(define-record context pointer)
(define-record font filename size)

;;; errors

(define (define-error location message #!rest condition)
  (let ((base (make-property-condition 'exn 'location location 'message message))
        (extra (apply make-property-condition condition)))
    (make-composite-condition base extra)))

(define (nuklear-error message location)
  (define-error location message 'nuklear))

;;; enums

;; enum nk_glfw_init_state
(define NK_GLFW3_DEFAULT (foreign-value "NK_GLFW3_DEFAULT" int))
(define NK_GLFW3_INSTALL_CALLBACKS (foreign-value "NK_GLFW3_INSTALL_CALLBACKS" int))

;; enum nk_anti_aliasing
(define NK_ANTI_ALIASING_OFF (foreign-value "NK_ANTI_ALIASING_OFF" int))
(define NK_ANTI_ALIASING_ON (foreign-value "NK_ANTI_ALIASING_ON" int))

;;; foreign functions

(define nk_glfw3_init (foreign-lambda (nonnull-c-pointer (struct "nk_context")) "nk_glfw3_init" (c-pointer (struct "GLFWwindow")) (enum "nk_glfw_init_state")))
(define nk_glfw3_init_font (foreign-lambda* bool (((c-pointer (struct "nk_context")) ctx) (scheme-object data))
                             "struct nk_font_atlas *atlas;"
                             "struct nk_font *font = 0;"
                             "int is_default = !C_truep(data);"
                             "nk_glfw3_font_stash_begin(&atlas);"
                             "if (!is_default) {"
                             "  C_word pair = (C_word) data;"
                             "  char *font_name = C_c_string(C_u_i_car(pair));"
                             "  int font_size = C_unfix(C_u_i_cdr(pair));"
                             "  font = nk_font_atlas_add_from_file(atlas, font_name, font_size, 0);"
                             "}"
                             "nk_glfw3_font_stash_end();"
                             "if (font)"
                             "  nk_style_set_font(ctx, &font->handle);"
                             "if (is_default || font)"
                             "  C_return(1);"
                             "else"
                             "  C_return(0);"))
(define nk_glfw3_new_frame (foreign-lambda void "nk_glfw3_new_frame"))
(define nk_glfw3_render (foreign-lambda void "nk_glfw3_render" (enum "nk_anti_aliasing") int int))
(define nk_glfw3_shutdown (foreign-lambda void "nk_glfw3_shutdown"))

;;; API

;; TODO: consider swapping meaning of second argument, make it optional
(define (init! window install-callbacks?)
  (when window
    (let ((flag (if install-callbacks?
                    NK_GLFW3_INSTALL_CALLBACKS
                    NK_GLFW3_DEFAULT)))
      (make-context (nk_glfw3_init window flag)))))

(define (init-font! #!optional context path size)
  (let ((ret (if (and context path size)
                 (nk_glfw3_init_font (context-pointer context) (cons path size))
                 (nk_glfw3_init_font #f #f))))
    (when (not ret)
      (signal (nuklear-error "Failed initializing font" 'init-font!)))))

(define new-frame nk_glfw3_new_frame)

(define (render!)
  (let ((flag (if (anti-alias?)
                  NK_ANTI_ALIASING_ON
                  NK_ANTI_ALIASING_OFF)))
    (nk_glfw3_render flag (max-vertex-buffer) (max-element-buffer))))

(define shutdown! nk_glfw3_shutdown)

)
