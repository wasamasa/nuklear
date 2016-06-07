(module nuklear
  ()

(import chicken scheme foreign)

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

)
