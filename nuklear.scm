(module nuklear
  (rgb->color color->rgba-floats
   make-panel make-rect make-color
   color-r color-r-set! color-g color-g-set! color-b color-b-set! color-a color-a-set!
   window-begin window-end
   layout-row-dynamic layout-row-static
   label
   button-label
   option-label
   color-picker
   property-int
   popup-begin popup-close popup-end
   combo-begin-color combo-end)

(import chicken scheme foreign)

(use lolevel)

;; TODO: report upstream bug regarding headers (removing not strictly
;; necessary ones leads to compilation errors and warnings)

#>
#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT

#define NK_IMPLEMENTATION
#include "nuklear.h"
<#

;;; enums

;; enum nk_panel_flags
(define NK_WINDOW_BORDER (foreign-value "NK_WINDOW_BORDER" int))
(define NK_WINDOW_BORDER_HEADER (foreign-value "NK_WINDOW_BORDER_HEADER" int))
(define NK_WINDOW_MOVABLE (foreign-value "NK_WINDOW_MOVABLE" int))
(define NK_WINDOW_SCALABLE (foreign-value "NK_WINDOW_SCALABLE" int))
(define NK_WINDOW_CLOSABLE (foreign-value "NK_WINDOW_CLOSABLE" int))
(define NK_WINDOW_MINIMIZABLE (foreign-value "NK_WINDOW_MINIMIZABLE" int))
(define NK_WINDOW_DYNAMIC (foreign-value "NK_WINDOW_DYNAMIC" int))
(define NK_WINDOW_NO_SCROLLBAR (foreign-value "NK_WINDOW_NO_SCROLLBAR" int))
(define NK_WINDOW_TITLE (foreign-value "NK_WINDOW_TITLE" int))

(define (window-flag->int flag)
  (case flag
    ((border) NK_WINDOW_BORDER)
    ((border-header) NK_WINDOW_BORDER_HEADER)
    ((movable) NK_WINDOW_MOVABLE)
    ((scalable) NK_WINDOW_SCALABLE)
    ((closable) NK_WINDOW_CLOSABLE)
    ((minimizable) NK_WINDOW_MINIMIZABLE)
    ((dynamic) NK_WINDOW_DYNAMIC)
    ((no-scrollbar) NK_WINDOW_NO_SCROLLBAR)
    ((title) NK_WINDOW_TITLE)))

(define (window-flags->int flags)
  (apply bitwise-ior (map window-flag->int flags)))

;; enum nk_button_behavior
(define NK_BUTTON_DEFAULT (foreign-value "NK_BUTTON_DEFAULT" int))
(define NK_BUTTON_REPEATER (foreign-value "NK_BUTTON_REPEATER" int))

;; enum nk_color_format
(define NK_RGB (foreign-value "NK_RGB" int))
(define NK_RGBA (foreign-value "NK_RGBA" int))

;; enum nk_popup_type
(define NK_POPUP_STATIC (foreign-value "NK_POPUP_STATIC" int))
(define NK_POPUP_DYNAMIC (foreign-value "NK_POPUP_DYNAMIC" int))

;; enum nk_text_alignment
(define NK_TEXT_LEFT (foreign-value "NK_TEXT_LEFT" int))
(define NK_TEXT_CENTERED (foreign-value "NK_TEXT_CENTERED" int))
(define NK_TEXT_RIGHT (foreign-value "NK_TEXT_RIGHT" int))

(define (label-alignment->int flag)
  (case flag
    ((left) NK_TEXT_LEFT)
    ((centered) NK_TEXT_CENTERED)
    ((right) NK_TEXT_RIGHT)))

;;; typedefs

(define-foreign-type nk_context* (nonnull-c-pointer (struct "nk_context")))
(define-foreign-type nk_panel* (nonnull-c-pointer (struct "nk_panel")))
(define-foreign-type nk_rect* (nonnull-c-pointer (struct "nk_rect")))
(define-foreign-type nk_color* (nonnull-c-pointer (struct "nk_color")))
(define-foreign-type nk_flags unsigned-int32)
(define-foreign-type uint8* (nonnull-c-pointer unsigned-byte))
(define-foreign-type uint8 unsigned-byte)
(define-foreign-type float* (nonnull-c-pointer float))

;;; stack-allocation helpers

;; NOTE: used for retrieving backend's context pointer
(define-record context pointer)

(define-record nk_panel pointer)
(define-record nk_rect pointer)
(define-record nk_color pointer)

(define nk_panel-size (foreign-type-size (struct "nk_panel")))
(define nk_rect-size (foreign-type-size (struct "nk_rect")))
(define nk_color-size(foreign-type-size (struct "nk_color")))

(define (make-panel) (make-nk_panel (make-locative (make-blob nk_panel-size))))
(define (make-rect x y w h)
  (let ((rect* (make-locative (make-blob nk_rect-size))))
    ((foreign-lambda* void ((nk_rect* rect) (float x) (float y) (float w) (float h))
       "struct nk_rect *r = rect; r->x = x, r->y = y, r->w = w, r->h = h;")
     rect* x y w h)
    (make-nk_rect rect*)))
(define (make-color r g b a)
  (let ((color* (make-locative (make-blob nk_color-size))))
    ((foreign-lambda* void ((nk_color* color) (uint8 r) (uint8 g) (uint8 b) (uint8 a))
       "struct nk_color *c = color; c->r = r, c->g = g, c->b = b, c->a = a;")
     color* r g b a)
    (make-nk_color color*)))

(define (color-r color)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* uint8 ((nk_color* c)) "C_return(c->r);") color*)))

(define (color-r-set! color r)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* void ((nk_color* c) (uint8 r)) "c->r = r;") color* r)))

(define color-r (getter-with-setter color-r color-r-set!))

(define (color-g color)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* uint8 ((nk_color* c)) "C_return(c->g);") color*)))

(define (color-g-set! color g)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* void ((nk_color* c) (uint8 g)) "c->g = g;") color* g)))

(define color-g (getter-with-setter color-g color-g-set!))

(define (color-b color)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* uint8 ((nk_color* c)) "C_return(c->b);") color*)))

(define (color-b-set! color b)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* void ((nk_color* c) (uint8 b)) "c->b = b;") color* b)))

(define color-b (getter-with-setter color-b color-b-set!))

(define (color-a color)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* uint8 ((nk_color* c)) "C_return(c->a);") color*)))

(define (color-a-set! color a)
  (let ((color* (nk_color-pointer color)))
    ((foreign-lambda* void ((nk_color* c) (uint8 a)) "c->a = a;") color* a)))

(define color-a (getter-with-setter color-a color-a-set!))

;;; foreign functions

;; utils
(define nk_color_f (foreign-lambda* void ((float* r) (float* g) (float* b) (float* a) (nk_color* c)) "nk_color_f(r, g, b, a, *c);"))

;; window
(define nk_begin (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) (nonnull-c-string title) (nk_rect* bounds) (nk_flags flags)) "C_return(nk_begin(ctx, layout, title, *bounds, flags));"))
(define nk_end (foreign-lambda void "nk_end" nk_context*))

;; layout
(define nk_layout_row_dynamic (foreign-lambda void "nk_layout_row_dynamic" nk_context* float int))
(define nk_layout_row_static (foreign-lambda void "nk_layout_row_static" nk_context* float int int))

;; widget
(define nk_label (foreign-lambda void "nk_label" nk_context* nonnull-c-string nk_flags))

;; button
(define nk_button_label (foreign-lambda bool "nk_button_label" nk_context* nonnull-c-string (enum "nk_button_behavior")))

;; radio
(define nk_option_label (foreign-lambda bool "nk_option_label" nk_context* nonnull-c-string bool))

;; color picker
(define nk_color_picker (foreign-lambda* void ((nk_context* ctx) (nk_color* color) ((enum "nk_color_format") flag) (uint8* r) (uint8* g) (uint8* b) (uint8* a)) "struct nk_color c = nk_color_picker(ctx, *color, flag); *r = c.r, *g = c.g, *b = c.b, *a = c.a;"))

;; property
(define nk_propertyi (foreign-lambda int "nk_propertyi" nk_context* nonnull-c-string int int int int int))

;; popup
(define nk_popup_begin (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) ((enum "nk_popup_type") type) (nonnull-c-string title) (nk_flags flags) (nk_rect* rect)) "C_return(nk_popup_begin(ctx, layout, type, title, flags, *rect));"))
(define nk_popup_close (foreign-lambda void "nk_popup_close" nk_context*))
(define nk_popup_end (foreign-lambda void "nk_popup_end" nk_context*))

;; combo box
(define nk_combo_begin_color (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) (nk_color* c) (int max_height)) "C_return(nk_combo_begin_color(ctx, layout, *c, max_height));"))
(define nk_combo_end (foreign-lambda void "nk_combo_end" nk_context*))

;;; API

(define (rgb->color r g b)
  (make-color r g b 255))

(define (color->rgba-floats color)
  (let ((color* (nk_color-pointer color)))
    (let-location ((r float)
                   (g float)
                   (b float)
                   (a float))
      (nk_color_f (location r) (location g) (location b) (location a) color*)
      (list r g b a))))

(define (window-begin context layout title rect flags)
  (let ((context* (context-pointer context))
        (layout* (nk_panel-pointer layout))
        (rect* (nk_rect-pointer rect))
        (flag (window-flags->int flags)))
    (nk_begin context* layout* title rect* flag)))

(define (window-end context)
  (let ((context* (context-pointer context)))
    (nk_end context*)))

(define (layout-row-dynamic context height columns)
  (let ((context* (context-pointer context)))
    (nk_layout_row_dynamic context* height columns)))

(define (layout-row-static context height item-width columns)
  (let ((context* (context-pointer context)))
    (nk_layout_row_static context* height item-width columns)))

(define (label context text alignment)
  (let ((context* (context-pointer context))
        (flag (label-alignment->int alignment)))
    (nk_label context* text flag)))

(define (button-label context text #!optional repeater?)
  (let ((context* (context-pointer context))
        (flag (if repeater?
                  NK_BUTTON_REPEATER
                  NK_BUTTON_DEFAULT)))
    (nk_button_label context* text flag)))

(define (option-label context text active?)
  (let ((context* (context-pointer context)))
    (nk_option_label context* text active?)))

(define (color-picker context color #!optional rgb?)
  (let ((context* (context-pointer context))
        (color* (nk_color-pointer color))
        (flag (if rgb? NK_RGB NK_RGBA)))
    (let-location ((r uint8)
                   (g uint8)
                   (b uint8)
                   (a uint8))
      (nk_color_picker context* color* flag (location r) (location g) (location b) (location a))
      (make-color r g b a))))

(define (property-int context text min value max step pixel-step)
  (let ((context* (context-pointer context)))
    (nk_propertyi context* text min value max step pixel-step)))

(define (popup-begin context layout dynamic? title flags rect)
  (let ((context* (context-pointer context))
        (layout* (nk_panel-pointer layout))
        (type (if dynamic?
                  NK_POPUP_DYNAMIC
                  NK_POPUP_STATIC))
        (flag (window-flags->int flags))
        (rect* (nk_rect-pointer rect)))
    (nk_popup_begin context* layout* type title flag rect*)))

(define (popup-close context)
  (let ((context* (context-pointer context)))
    (nk_popup_close context*)))

(define (popup-end context)
  (let ((context* (context-pointer context)))
    (nk_popup_end context*)))

(define (combo-begin-color context layout color max-height)
  (let ((context* (context-pointer context))
        (layout* (nk_panel-pointer layout))
        (color* (nk_color-pointer color)))
    (nk_combo_begin_color context* layout* color* max-height)))

(define (combo-end context)
  (let ((context* (context-pointer context)))
    (nk_combo_end context*)))

)
