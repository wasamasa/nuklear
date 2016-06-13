(module nuklear
  (make-panel make-rect make-color make-vec2
   panel-bounds
   rect-x rect-x-set! rect-y rect-y-set! rect-w rect-w-set! rect-h rect-h-set!
   color-r color-r-set! color-g color-g-set! color-b color-b-set! color-a color-a-set!
   vec2-x vec2-x-set! vec2-y vec2-y-set!

   window-begin window-end
   window-bounds window-content-region
   window-canvas
   window-closed?
   layout-row-dynamic layout-row-static
   layout-space-begin layout-space-end layout-space-push
   layout-space-bounds layout-space-to-screen layout-space-rect-to-screen layout-space-rect-to-local
   group-begin group-end
   label
   button-label button-color
   option-label
   color-picker
   property-int
   edit-string
   popup-begin popup-close popup-end
   combo-begin-color combo-end
   contextual-begin contextual-item-label contextual-end

   rgb->color color->rgba-floats

   stroke-line stroke-curve
   fill-circle

   context-style style-window-border
   context-input input-mouse mouse-delta mouse-pos
   input-mouse-click-down-in-rect? input-mouse-hovering-in-rect? input-mouse-clicked? input-mouse-down? input-mouse-released?)

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

;; enum nk_button_behavior
(define NK_BUTTON_DEFAULT (foreign-value "NK_BUTTON_DEFAULT" int))
(define NK_BUTTON_REPEATER (foreign-value "NK_BUTTON_REPEATER" int))

;; enum nk_color_format
(define NK_RGB (foreign-value "NK_RGB" int))
(define NK_RGBA (foreign-value "NK_RGBA" int))

;; enum nk_popup_type
(define NK_POPUP_STATIC (foreign-value "NK_POPUP_STATIC" int))
(define NK_POPUP_DYNAMIC (foreign-value "NK_POPUP_DYNAMIC" int))

;; enum nk_layout_format
(define NK_DYNAMIC (foreign-value "NK_DYNAMIC" int))
(define NK_STATIC (foreign-value "NK_STATIC" int))

;; enum nk_buttons
(define NK_BUTTON_LEFT (foreign-value "NK_BUTTON_LEFT" int))
(define NK_BUTTON_MIDDLE (foreign-value "NK_BUTTON_MIDDLE" int))
(define NK_BUTTON_RIGHT (foreign-value "NK_BUTTON_RIGHT" int))
;; (define NK_BUTTON_MAX (foreign-value "NK_BUTTON_MAX" int))

(define (button->int flag)
  (case flag
    ((left) NK_BUTTON_LEFT)
    ((middle) NK_BUTTON_MIDDLE)
    ((right) NK_BUTTON_RIGHT)))

;; enum nk_text_alignment
(define NK_TEXT_LEFT (foreign-value "NK_TEXT_LEFT" int))
(define NK_TEXT_CENTERED (foreign-value "NK_TEXT_CENTERED" int))
(define NK_TEXT_RIGHT (foreign-value "NK_TEXT_RIGHT" int))

(define (text-alignment->int flag)
  (case flag
    ((left) NK_TEXT_LEFT)
    ((centered) NK_TEXT_CENTERED)
    ((right) NK_TEXT_RIGHT)))

;; enum nk_edit_types
(define NK_EDIT_SIMPLE (foreign-value "NK_EDIT_SIMPLE" int))
(define NK_EDIT_FIELD (foreign-value "NK_EDIT_FIELD" int))
(define NK_EDIT_BOX (foreign-value "NK_EDIT_BOX" int))
(define NK_EDIT_EDITOR (foreign-value "NK_EDIT_EDITOR" int))

(define (edit-type->int flag)
  (case flag
    ((simple) NK_EDIT_SIMPLE)
    ((field) NK_EDIT_FIELD)
    ((box) NK_EDIT_BOX)
    ((editor) NK_EDIT_EDITOR)))

;; enum nk_edit_flags
(define NK_EDIT_DEFAULT (foreign-value "NK_EDIT_DEFAULT" int))
(define NK_EDIT_READ_ONLY (foreign-value "NK_EDIT_READ_ONLY" int))
(define NK_EDIT_AUTO_SELECT (foreign-value "NK_EDIT_AUTO_SELECT" int))
(define NK_EDIT_SIG_ENTER (foreign-value "NK_EDIT_SIG_ENTER" int))
(define NK_EDIT_ALLOW_TAB (foreign-value "NK_EDIT_ALLOW_TAB" int))
(define NK_EDIT_NO_CURSOR (foreign-value "NK_EDIT_NO_CURSOR" int))
(define NK_EDIT_SELECTABLE (foreign-value "NK_EDIT_SELECTABLE" int))
(define NK_EDIT_CLIPBOARD (foreign-value "NK_EDIT_CLIPBOARD" int))
(define NK_EDIT_CTRL_ENTER_NEWLINE (foreign-value "NK_EDIT_CTRL_ENTER_NEWLINE" int))
(define NK_EDIT_NO_HORIZONTAL_SCROLL (foreign-value "NK_EDIT_NO_HORIZONTAL_SCROLL" int))
(define NK_EDIT_ALWAYS_INSERT_MODE (foreign-value "NK_EDIT_ALWAYS_INSERT_MODE" int))
(define NK_EDIT_MULTILINE (foreign-value "NK_EDIT_MULTILINE" int))

(define (edit-flag->int flag)
  (case flag
    ((default) NK_EDIT_DEFAULT)
    ((read-only) NK_EDIT_READ_ONLY)
    ((auto-select) NK_EDIT_AUTO_SELECT)
    ((sig-enter) NK_EDIT_SIG_ENTER)
    ((allow-tab) NK_EDIT_ALLOW_TAB)
    ((no-cursor) NK_EDIT_NO_CURSOR)
    ((selectable) NK_EDIT_SELECTABLE)
    ((clipboard) NK_EDIT_CLIPBOARD)
    ((ctrl-enter-newline) NK_EDIT_CTRL_ENTER_NEWLINE)
    ((no-horizontal-scroll) NK_EDIT_NO_HORIZONTAL_SCROLL)
    ((always-insert-mode) NK_EDIT_ALWAYS_INSERT_MODE)
    ((multiline) NK_EDIT_MULTILINE)))

(define (edit-flags->int flags)
  (apply bitwise-ior (map edit-flag->int flags)))

;; enum nk_edit_events
(define NK_EDIT_ACTIVE (foreign-value "NK_EDIT_ACTIVE" int))
(define NK_EDIT_INACTIVE (foreign-value "NK_EDIT_INACTIVE" int))
(define NK_EDIT_ACTIVATED (foreign-value "NK_EDIT_ACTIVATED" int))
(define NK_EDIT_DEACTIVATED (foreign-value "NK_EDIT_DEACTIVATED" int))
(define NK_EDIT_COMMITED (foreign-value "NK_EDIT_COMMITED" int))

(define (int->edit-event value)
  (select value
    ((0) #f)
    ((NK_EDIT_ACTIVE) 'active)
    ((NK_EDIT_INACTIVE) 'inactive)
    ((NK_EDIT_ACTIVATED) 'activated)
    ((NK_EDIT_DEACTIVATED) 'deactivated)
    ((NK_EDIT_COMMITED) 'commited)))

(define (int->edit-events value)
  (let loop ((flag 1) (events '()))
    (if (<= flag value)
        (let ((match? (not (zero? (bitwise-and value flag)))))
          (if match?
              (loop (* flag 2) (cons (int->edit-event flag) events))
              (loop (* flag 2) events)))
        events)))

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

;; custom filter enum
#>
enum nk_filter_type {
    NK_FILTER_TYPE_DEFAULT,
    NK_FILTER_TYPE_ASCII,
    NK_FILTER_TYPE_FLOAT,
    NK_FILTER_TYPE_DECIMAL,
    NK_FILTER_TYPE_HEX,
    NK_FILTER_TYPE_OCT,
    NK_FILTER_TYPE_BINARY
};
<#

(define NK_FILTER_TYPE_DEFAULT (foreign-value "NK_FILTER_TYPE_DEFAULT" int))
(define NK_FILTER_TYPE_ASCII (foreign-value "NK_FILTER_TYPE_ASCII" int))
(define NK_FILTER_TYPE_FLOAT (foreign-value "NK_FILTER_TYPE_FLOAT" int))
(define NK_FILTER_TYPE_DECIMAL (foreign-value "NK_FILTER_TYPE_DECIMAL" int))
(define NK_FILTER_TYPE_HEX (foreign-value "NK_FILTER_TYPE_HEX" int))
(define NK_FILTER_TYPE_OCT (foreign-value "NK_FILTER_TYPE_OCT" int))
(define NK_FILTER_TYPE_BINARY (foreign-value "NK_FILTER_TYPE_BINARY" int))

(define (filter-flag->int flag)
  (case flag
    ((default) NK_FILTER_TYPE_DEFAULT)
    ((ascii) NK_FILTER_TYPE_ASCII)
    ((float) NK_FILTER_TYPE_FLOAT)
    ((decimal) NK_FILTER_TYPE_DECIMAL)
    ((hex) NK_FILTER_TYPE_HEX)
    ((oct) NK_FILTER_TYPE_OCT)
    ((binary) NK_FILTER_TYPE_BINARY)))

;;; typedefs

(define-foreign-type nk_context* (nonnull-c-pointer (struct "nk_context")))
(define-foreign-type nk_command_buffer* (nonnull-c-pointer (struct "nk_command_buffer")))
(define-foreign-type nk_input* (nonnull-c-pointer (struct "nk_input")))
(define-foreign-type nk_style* (nonnull-c-pointer (struct "nk_style")))
(define-foreign-type nk_mouse* (nonnull-c-pointer (struct "nk_mouse")))
(define-foreign-type nk_panel* (nonnull-c-pointer (struct "nk_panel")))
(define-foreign-type nk_rect* (nonnull-c-pointer (struct "nk_rect")))
(define-foreign-type nk_color* (nonnull-c-pointer (struct "nk_color")))
(define-foreign-type nk_vec2* (nonnull-c-pointer (struct "nk_vec2")))
(define-foreign-type nk_flags unsigned-int32)
(define-foreign-type nk_flags* (nonnull-c-pointer nk_flags))
(define-foreign-type uint8* (nonnull-c-pointer unsigned-byte))
(define-foreign-type uint8 unsigned-byte)
(define-foreign-type float* (nonnull-c-pointer float))

;;; auxiliary records

;; used for retrieving backend's context pointer
(define-record context pointer)

;; used for retrieving various context-specific things
(define-record command-buffer pointer)
(define-record style pointer)
(define-record input pointer)
(define-record mouse pointer)

(define (context-style context)
  (let* ((context* (context-pointer context))
         (style* ((foreign-lambda* nk_style* ((nk_context* ctx))
                    "C_return(&(ctx->style));")
                  context*)))
    (make-style style*)))

(define (style-window-border style)
  (let ((style* (style-pointer style)))
    ((foreign-lambda* float ((nk_style* style))
       "C_return(style->window.border);")
     style*)))

(define (context-input context)
  (let* ((context* (context-pointer context))
         (input* ((foreign-lambda* nk_input* ((nk_context* ctx))
                    "C_return(&(ctx->input));")
                  context*)))
    (make-input input*)))

(define (input-mouse input)
  (let* ((input* (input-pointer input))
         (mouse* ((foreign-lambda* nk_mouse* ((nk_input* input))
                    "C_return(&(input->mouse));")
                  input*)))
    (make-mouse mouse*)))

(define (mouse-delta mouse)
  (let* ((mouse* (mouse-pointer mouse))
         (delta (make-vec2 0 0))
         (delta* (nk_vec2-pointer delta)))
    ((foreign-lambda* void ((nk_mouse* mouse) (nk_vec2* out))
       "*out = mouse->delta;")
     mouse* delta*)
    delta))

(define (mouse-pos mouse)
  (let* ((mouse* (mouse-pointer mouse))
         (pos (make-vec2 0 0))
         (pos* (nk_vec2-pointer pos)))
    ((foreign-lambda* void ((nk_mouse* mouse) (nk_vec2* out))
       "*out = mouse->pos;")
     mouse* pos*)
    pos))

;;; stack-allocation helpers

(define-record nk_panel pointer)
(define-record nk_rect pointer)
(define-record nk_color pointer)
(define-record nk_vec2 pointer)

(define nk_panel-size (foreign-type-size (struct "nk_panel")))
(define nk_rect-size (foreign-type-size (struct "nk_rect")))
(define nk_color-size (foreign-type-size (struct "nk_color")))
(define nk_vec2-size (foreign-type-size (struct "nk_vec2")))

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

(define (make-vec2 x y)
  (let ((vec2* (make-locative (make-blob nk_vec2-size))))
    ((foreign-lambda* void ((nk_vec2* vec2) (float x) (float y))
       "struct nk_vec2 *v = vec2; v->x = x, v->y = y;")
     vec2* x y)
    (make-nk_vec2 vec2*)))

(define (panel-bounds panel)
  (let* ((panel* (nk_panel-pointer panel))
         (rect (make-rect 0 0 0 0))
         (rect* (nk_rect-pointer rect)))
    ((foreign-lambda* void ((nk_panel* panel) (nk_rect* out))
       "*out = panel->bounds;")
     panel* rect*)
    rect))

(define (rect-x rect)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* float ((nk_rect* r)) "C_return(r->x);") rect*)))

(define (rect-x-set! rect x)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* void ((nk_rect* r) (float x)) "r->x = x;") rect* x)))

(define rect-x (getter-with-setter rect-x rect-x-set!))

(define (rect-y rect)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* float ((nk_rect* r)) "C_return(r->y);") rect*)))

(define (rect-y-set! rect y)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* void ((nk_rect* r) (float y)) "r->y = y;") rect* y)))

(define rect-y (getter-with-setter rect-y rect-y-set!))

(define (rect-w rect)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* float ((nk_rect* r)) "C_return(r->w);") rect*)))

(define (rect-w-set! rect w)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* void ((nk_rect* r) (float w)) "r->w = w;") rect* w)))

(define rect-w (getter-with-setter rect-w rect-w-set!))

(define (rect-h rect)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* float ((nk_rect* r)) "C_return(r->h);") rect*)))

(define (rect-h-set! rect h)
  (let ((rect* (nk_rect-pointer rect)))
    ((foreign-lambda* void ((nk_rect* r) (float h)) "r->h = h;") rect* h)))

(define rect-h (getter-with-setter rect-h rect-h-set!))

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

(define (vec2-x vec2)
  (let ((vec2* (nk_vec2-pointer vec2)))
    ((foreign-lambda* float ((nk_vec2* v)) "C_return(v->x);") vec2*)))

(define (vec2-x-set! vec2 x)
  (let ((vec2* (nk_vec2-pointer vec2)))
    ((foreign-lambda* void ((nk_vec2* v) (float x)) "v->x = x;") vec2* x)))

(define vec2-x (getter-with-setter vec2-x vec2-x-set!))

(define (vec2-y vec2)
  (let ((vec2* (nk_vec2-pointer vec2)))
    ((foreign-lambda* float ((nk_vec2* v)) "C_return(v->y);") vec2*)))

(define (vec2-y-set! vec2 y)
  (let ((vec2* (nk_vec2-pointer vec2)))
    ((foreign-lambda* void ((nk_vec2* v) (float y)) "v->y = y;") vec2* y)))

(define vec2-y (getter-with-setter vec2-y vec2-y-set!))

;;; foreign functions

;; window
(define nk_begin (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) (nonnull-c-string title) (nk_rect* bounds) (nk_flags flags)) "C_return(nk_begin(ctx, layout, title, *bounds, flags));"))
(define nk_end (foreign-lambda void "nk_end" nk_context*))

(define nk_window_get_bounds (foreign-lambda* void ((nk_context* ctx) (nk_rect* data)) "*data = nk_window_get_bounds(ctx);"))
(define nk_window_get_content_region (foreign-lambda* void ((nk_context* ctx) (nk_rect* data)) "*data = nk_window_get_content_region(ctx);"))

(define nk_window_get_canvas (foreign-lambda nk_command_buffer* "nk_window_get_canvas" nk_context*))

(define nk_window_is_closed (foreign-lambda bool "nk_window_is_closed" nk_context* (const nonnull-c-string)))

;; layout
(define nk_layout_row_dynamic (foreign-lambda void "nk_layout_row_dynamic" nk_context* float int))
(define nk_layout_row_static (foreign-lambda void "nk_layout_row_static" nk_context* float int int))

(define nk_layout_space_begin (foreign-lambda void "nk_layout_space_begin" nk_context* (enum "nk_layout_format") float int))
(define nk_layout_space_push (foreign-lambda* void ((nk_context* ctx) (nk_rect* rect)) "nk_layout_space_push(ctx, *rect);"))
(define nk_layout_space_end (foreign-lambda void "nk_layout_space_end" nk_context*))

(define nk_layout_space_bounds (foreign-lambda* void ((nk_context* ctx) (nk_rect* data)) "*data = nk_layout_space_bounds(ctx);"))
(define nk_layout_space_to_screen (foreign-lambda* void ((nk_context* ctx) (nk_vec2* in) (nk_vec2* out)) "*out = nk_layout_space_to_screen(ctx, *in);"))
(define nk_layout_space_rect_to_screen (foreign-lambda* void ((nk_context* ctx) (nk_rect* in) (nk_rect* out)) "*out = nk_layout_space_rect_to_screen(ctx, *in);"))
(define nk_layout_space_rect_to_local (foreign-lambda* void ((nk_context* ctx) (nk_rect* in) (nk_rect* out)) "*out = nk_layout_space_rect_to_local(ctx, *in);"))

;; group
(define nk_group_begin (foreign-lambda bool "nk_group_begin" nk_context* nk_panel* (const nonnull-c-string) nk_flags))
(define nk_group_end (foreign-lambda void "nk_group_end" nk_context*))

;; widget
(define nk_label (foreign-lambda void "nk_label" nk_context* nonnull-c-string nk_flags))

;; button
(define nk_button_label (foreign-lambda bool "nk_button_label" nk_context* nonnull-c-string (enum "nk_button_behavior")))
(define nk_button_color (foreign-lambda* bool ((nk_context* ctx) (nk_color* color) ((enum "nk_button_behavior") flag)) "C_return(nk_button_color(ctx, *color, flag));"))

;; radio
(define nk_option_label (foreign-lambda bool "nk_option_label" nk_context* nonnull-c-string bool))

;; color picker
(define nk_color_picker (foreign-lambda* void ((nk_context* ctx) (nk_color* color) ((enum "nk_color_format") flag) (uint8* r) (uint8* g) (uint8* b) (uint8* a)) "struct nk_color c = nk_color_picker(ctx, *color, flag); *r = c.r, *g = c.g, *b = c.b, *a = c.a;"))

;; property
(define nk_propertyi (foreign-lambda int "nk_propertyi" nk_context* nonnull-c-string int int int int int))

;; textedit
(define nk_edit_string (foreign-lambda* c-string ((nk_context* ctx) (nk_flags edit_flags) (nonnull-c-string text) (int len) (blob buffer) (int max) (nk_flags* edit_value) (int filter_flag))
                         "strncpy(buffer, text, max);"
                         "int (*filter)(const struct nk_text_edit*, nk_rune);"
                         "switch(filter_flag) {"
                         "  case NK_FILTER_TYPE_DEFAULT : filter = nk_filter_default; break;"
                         "  case NK_FILTER_TYPE_ASCII : filter = nk_filter_ascii; break;"
                         "  case NK_FILTER_TYPE_FLOAT : filter = nk_filter_float; break;"
                         "  case NK_FILTER_TYPE_DECIMAL : filter = nk_filter_decimal; break;"
                         "  case NK_FILTER_TYPE_HEX : filter = nk_filter_hex; break;"
                         "  case NK_FILTER_TYPE_OCT : filter = nk_filter_oct; break;"
                         "  case NK_FILTER_TYPE_BINARY : filter = nk_filter_binary; break;"
                         "}"
                         "*edit_value = nk_edit_string(ctx, edit_flags, buffer, &len, max, filter);"
                         "buffer[len] = 0;"
                         "C_return(buffer);"))

;; popup
(define nk_popup_begin (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) ((enum "nk_popup_type") type) (nonnull-c-string title) (nk_flags flags) (nk_rect* rect)) "C_return(nk_popup_begin(ctx, layout, type, title, flags, *rect));"))
(define nk_popup_close (foreign-lambda void "nk_popup_close" nk_context*))
(define nk_popup_end (foreign-lambda void "nk_popup_end" nk_context*))

;; combo box
(define nk_combo_begin_color (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) (nk_color* c) (int max_height)) "C_return(nk_combo_begin_color(ctx, layout, *c, max_height));"))
(define nk_combo_end (foreign-lambda void "nk_combo_end" nk_context*))

;; contextual
(define nk_contextual_begin (foreign-lambda* bool ((nk_context* ctx) (nk_panel* layout) (nk_flags flags) (nk_vec2* vec2) (nk_rect* rect)) "C_return(nk_contextual_begin(ctx, layout, flags, *vec2, *rect));"))
(define nk_contextual_item_label (foreign-lambda bool "nk_contextual_item_label" nk_context* (const nonnull-c-string) nk_flags))
(define nk_contextual_end (foreign-lambda void "nk_contextual_end" nk_context*))

;; utils
(define nk_color_f (foreign-lambda* void ((float* r) (float* g) (float* b) (float* a) (nk_color* c)) "nk_color_f(r, g, b, a, *c);"))

;; drawing
(define nk_stroke_line (foreign-lambda* void ((nk_command_buffer* buffer) (float x0) (float y0) (float x1) (float y1) (float line_thickness) (nk_color* c)) "nk_stroke_line(buffer, x0, y0, x1, y1, line_thickness, *c);"))
(define nk_stroke_curve (foreign-lambda* void ((nk_command_buffer* buffer) (float ax) (float ay) (float ctrl0x) (float ctrl0y) (float ctrl1x) (float ctrl1y) (float bx) (float by) (float line_thickness) (nk_color* c)) "nk_stroke_curve(buffer, ax, ay, ctrl0x, ctrl0y, ctrl1x, ctrl1y, bx, by, line_thickness, *c);"))

(define nk_fill_circle (foreign-lambda* void ((nk_command_buffer* buffer) (nk_rect* r) (nk_color* c)) "nk_fill_circle(buffer, *r, *c);"))

;; input

(define nk_input_has_mouse_click_down_in_rect (foreign-lambda* bool (((const nk_input*) input) ((enum "nk_buttons") button) (nk_rect* rect) (bool down)) "C_return(nk_input_has_mouse_click_down_in_rect(input, button, *rect, down));"))
(define nk_input_is_mouse_hovering_rect (foreign-lambda* bool (((const nk_input*) input) (nk_rect* rect)) "C_return(nk_input_is_mouse_hovering_rect(input, *rect));"))
(define nk_input_mouse_clicked (foreign-lambda* bool (((const nk_input*) input) ((enum "nk_buttons") button) (nk_rect* rect)) "C_return(nk_input_mouse_clicked(input, button, *rect));"))
(define nk_input_is_mouse_down (foreign-lambda bool "nk_input_is_mouse_down" (const nk_input*) (enum "nk_buttons")))
(define nk_input_is_mouse_released (foreign-lambda bool "nk_input_is_mouse_released" (const nk_input*) (enum "nk_buttons")))

;;; API

;; widgets

(define (window-begin context layout title rect flags)
  (let ((context* (context-pointer context))
        (layout* (nk_panel-pointer layout))
        (rect* (nk_rect-pointer rect))
        (flag (window-flags->int flags)))
    (nk_begin context* layout* title rect* flag)))

(define (window-end context)
  (let ((context* (context-pointer context)))
    (nk_end context*)))

(define (window-bounds context)
  (let* ((context* (context-pointer context))
         (rect (make-rect 0 0 0 0))
         (rect* (nk_rect-pointer rect)))
      (nk_window_get_bounds context* rect*)
      rect))

(define (window-content-region context)
  (let* ((context* (context-pointer context))
         (rect (make-rect 0 0 0 0))
         (rect* (nk_rect-pointer rect)))
    (nk_window_get_content_region context* rect*)
    rect))

(define (window-canvas context)
  (let ((context* (context-pointer context)))
    (make-command-buffer (nk_window_get_canvas context*))))

(define (window-closed? context title)
  (let ((context* (context-pointer context)))
    (nk_window_is_closed context* title)))

(define (layout-row-dynamic context height columns)
  (let ((context* (context-pointer context)))
    (nk_layout_row_dynamic context* height columns)))

(define (layout-row-static context height item-width columns)
  (let ((context* (context-pointer context)))
    (nk_layout_row_static context* height item-width columns)))

(define (layout-space-begin context flag height widget-count)
  (let ((context* (context-pointer context))
        (format (if flag NK_STATIC NK_DYNAMIC)))
    (nk_layout_space_begin context* format height widget-count)))

(define (layout-space-end context)
  (let ((context* (context-pointer context)))
    (nk_layout_space_end context*)))

(define (layout-space-push context rect)
  (let ((context* (context-pointer context))
        (rect* (nk_rect-pointer rect)))
    (nk_layout_space_push context* rect*)))

(define (layout-space-bounds context)
  (let* ((context* (context-pointer context))
         (rect (make-rect 0 0 0 0))
         (rect* (nk_rect-pointer rect)))
    (nk_layout_space_bounds context* rect*)
    rect))

(define (layout-space-to-screen context vec2)
  (let* ((context* (context-pointer context))
         (in* (nk_vec2-pointer vec2))
         (out (make-vec2 0 0))
         (out* (nk_vec2-pointer out)))
    (nk_layout_space_to_screen context* in* out*)
    out))

(define (layout-space-rect-to-screen context rect)
  (let* ((context* (context-pointer context))
         (in* (nk_rect-pointer rect))
         (out (make-rect 0 0 0 0))
         (out* (nk_rect-pointer out)))
    (nk_layout_space_rect_to_screen context* in* out*)
    out))

(define (layout-space-rect-to-local context rect)
  (let* ((context* (context-pointer context))
         (in* (nk_rect-pointer rect))
         (out (make-rect 0 0 0 0))
         (out* (nk_rect-pointer out)))
    (nk_layout_space_rect_to_local context* in* out*)
    out))

(define (group-begin context layout title flags)
  (let ((context* (context-pointer context))
        (layout* (nk_panel-pointer layout))
        (flag (window-flags->int flags)))
    (nk_group_begin context* layout* title flag)))

(define (group-end context)
  (let ((context* (context-pointer context)))
    (nk_group_end context*)))

(define (label context text alignment)
  (let ((context* (context-pointer context))
        (flag (text-alignment->int alignment)))
    (nk_label context* text flag)))

(define (button-label context text #!optional repeater?)
  (let ((context* (context-pointer context))
        (flag (if repeater?
                  NK_BUTTON_REPEATER
                  NK_BUTTON_DEFAULT)))
    (nk_button_label context* text flag)))

(define (button-color context color #!optional repeater?)
  (let ((context* (context-pointer context))
        (color* (nk_color-pointer color))
        (flag (if repeater?
                  NK_BUTTON_REPEATER
                  NK_BUTTON_DEFAULT)))
    (nk_button_color context* color* flag)))

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

(define (edit-string context edit-type-or-flags text max #!optional filter-flag)
  (let ((context* (context-pointer context))
        (edit-flags (if (symbol? edit-type-or-flags)
                        (edit-type->int edit-type-or-flags)
                        (edit-flags->int edit-type-or-flags)))
        (len (string-length text))
        (buffer (make-blob max))
        (filter-value (filter-flag->int (or filter-flag 'default))))
    (let-location ((edit-value nk_flags))
      (let ((ret (nk_edit_string context* edit-flags text len buffer max
                                 (location edit-value) filter-value)))
        (values ret (int->edit-events edit-value))))))

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

(define (contextual-begin context layout flags size trigger-bounds)
  (let ((context* (context-pointer context))
        (layout* (nk_panel-pointer layout))
        (flag (window-flags->int flags))
        (size* (nk_vec2-pointer size))
        (trigger-bounds* (nk_rect-pointer trigger-bounds)))
    (nk_contextual_begin context* layout* flag size* trigger-bounds*)))

(define (contextual-item-label context text alignment)
  (let ((context* (context-pointer context))
        (flag (text-alignment->int alignment)))
    (nk_contextual_item_label context* text flag)))

(define (contextual-end context)
  (let ((context* (context-pointer context)))
    (nk_contextual_end context*)))

;; utils

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

;; drawing

(define (stroke-line canvas x0 y0 x1 y1 line-thickness color)
  (let ((canvas* (command-buffer-pointer canvas))
        (color* (nk_color-pointer color)))
    (nk_stroke_line canvas* x0 y0 x1 y1 line-thickness color*)))

(define (stroke-curve canvas ax ay ctrl0x ctrl0y ctrl1x ctrl1y bx by line-thickness color)
  (let ((canvas* (command-buffer-pointer canvas))
        (color* (nk_color-pointer color)))
    (nk_stroke_curve canvas* ax ay ctrl0x ctrl0y ctrl1x ctrl1y bx by line-thickness color*)))

(define (fill-circle canvas rect color)
  (let ((canvas* (command-buffer-pointer canvas))
        (rect* (nk_rect-pointer rect))
        (color* (nk_color-pointer color)))
    (nk_fill_circle canvas* rect* color*)))

;; input

(define (input-mouse-click-down-in-rect? input button rect down?)
  (let ((input* (input-pointer input))
        (flag (button->int button))
        (rect* (nk_rect-pointer rect)))
    (nk_input_has_mouse_click_down_in_rect input* flag rect* down?)))

(define (input-mouse-hovering-in-rect? input rect)
  (let ((input* (input-pointer input))
        (rect* (nk_rect-pointer rect)))
    (nk_input_is_mouse_hovering_rect input* rect*)))

(define (input-mouse-clicked? input button rect)
  (let ((input* (input-pointer input))
        (flag (button->int button))
        (rect* (nk_rect-pointer rect)))
    (nk_input_mouse_clicked input* flag rect*)))

(define (input-mouse-down? input button)
  (let ((input* (input-pointer input))
        (flag (button->int button)))
    (nk_input_is_mouse_down input* flag)))

(define (input-mouse-released? input button)
  (let ((input* (input-pointer input))
        (flag (button->int button)))
    (nk_input_is_mouse_released input* flag)))

)
