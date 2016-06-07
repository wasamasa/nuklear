(module nuklear
  (make-panel make-rect
   window-begin window-end
   layout-row-dynamic
   label
   button-label
   popup-begin popup-close popup-end)

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
(define-foreign-type nk_flags unsigned-int32)

;;; stack-allocation helpers

;; NOTE: used for retrieving backend's context pointer
(define-record context pointer)

(define-record nk_panel pointer)
(define-record nk_rect pointer)

(define nk_panel-size (foreign-type-size (struct "nk_panel")))
(define nk_rect-size (foreign-type-size (struct "nk_rect")))

(define (make-panel) (make-nk_panel (make-locative (make-blob nk_panel-size))))
(define (make-rect x y w h)
  (let ((nk_rect* (make-locative (make-blob nk_rect-size))))
    ((foreign-lambda* void ((nk_rect* rect) (float x) (float y) (float w) (float h))
       "struct nk_rect *r = rect; r->x = x, r->y = y, r->w = w, r->h = h;")
     nk_rect* x y w h)
    (make-nk_rect nk_rect*)))

;;; foreign functions

;; window
(define nk_begin (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) (nonnull-c-string title) (nk_rect* bounds) (nk_flags flags)) "C_return(nk_begin(ctx, layout, title, *bounds, flags));"))
(define nk_end (foreign-lambda void "nk_end" nk_context*))

;; layout
(define nk_layout_row_dynamic (foreign-lambda void "nk_layout_row_dynamic" nk_context* float int))

;; widgets
(define nk_label (foreign-lambda void "nk_label" nk_context* nonnull-c-string nk_flags))

;; buttons
(define nk_button_label (foreign-lambda bool "nk_button_label" nk_context* nonnull-c-string (enum "nk_button_behavior")))

;; popups
(define nk_popup_begin (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) ((enum "nk_popup_type") type) (nonnull-c-string title) (nk_flags flags) (nk_rect* rect)) "C_return(nk_popup_begin(ctx, layout, type, title, flags, *rect));"))
(define nk_popup_close (foreign-lambda void "nk_popup_close" nk_context*))
(define nk_popup_end (foreign-lambda void "nk_popup_end" nk_context*))

;;; API

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

)
