with Interfaces.C.Strings;
with System;
with Generic_Address_To_Access_Conversions;

package Vk is

   type Major_Version_T is range 0 .. 2**9;
   type Minor_Version_T is range 0 .. 2**9;
   type Patch_Version_T is range 0 .. 2**11;

   type Version_T is record
      Major : Major_Version_T;
      Minor : Minor_Version_T;
      Patch : Patch_Version_T;
   end record;
   pragma Pack (Version_T);
   for Version_T'Size use 32;
   for Version_T use record
      Major at 0 range 22 .. 31;
      Minor at 0 range 12 .. 21;
      Patch at 0 range  0 .. 11;
   end record;

   API_Version : constant Version_T := (Major => 1, Minor => 0, Patch => 21);

   subtype Void_Ptr is System.Address;

   type Void_Ptr_Array_T is array (Interfaces.C.size_t range 0 .. 1000) of Void_Ptr;
   pragma Convention (C, Void_Ptr_Array_T);

   package Void_Ptr_Array_Conversions is new Generic_Address_To_Access_Conversions (Void_Ptr_Array_T);

   type Char_Ptr_Array_T is array (Interfaces.C.size_t range 0 .. 1000) of Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, Char_Ptr_Array_T);

   package Char_Ptr_Array_Conversions is new Generic_Address_To_Access_Conversions (Char_Ptr_Array_T);

   MAX_PHYSICAL_DEVICE_NAME_SIZE : constant := 256;
   UUID_SIZE                     : constant := 16;
   MAX_EXTENSION_NAME_SIZE       : constant := 256;
   MAX_DESCRIPTION_SIZE          : constant := 256;
   MAX_MEMORY_TYPES              : constant := 32;
   MAX_MEMORY_HEAPS              : constant := 16;
   VK_TRUE                       : constant := 1;
   VK_FALSE                      : constant := 0;

   type Image_Layout_T is
     (IMAGE_LAYOUT_UNDEFINED, -- Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
      IMAGE_LAYOUT_GENERAL, -- General layout when image can be used for any kind of access
      IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, -- Optimal layout when image is only used for color attachment read/write
      IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, -- Optimal layout when image is only used for depth/stencil attachment read/write
      IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL, -- Optimal layout when image is used for read only depth/stencil attachment and shader access
      IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL, -- Optimal layout when image is used for read only shader access
      IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, -- Optimal layout when image is used only as source of transfer operations
      IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, -- Optimal layout when image is used only as destination of transfer operations
      IMAGE_LAYOUT_PREINITIALIZED -- Initial layout used when the data is populated by the CPU
      );
   for Image_Layout_T use
     (IMAGE_LAYOUT_UNDEFINED                        => 0,
      IMAGE_LAYOUT_GENERAL                          => 1,
      IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL         => 2,
      IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL => 3,
      IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL  => 4,
      IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL         => 5,
      IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL             => 6,
      IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL             => 7,
      IMAGE_LAYOUT_PREINITIALIZED                   => 8);
   for Image_Layout_T'Size use Interfaces.C.int'Size;

   type Attachment_Load_Op_T is (ATTACHMENT_LOAD_OP_LOAD, ATTACHMENT_LOAD_OP_CLEAR, ATTACHMENT_LOAD_OP_DONT_CARE);
   for Attachment_Load_Op_T use
     (ATTACHMENT_LOAD_OP_LOAD      => 0,
      ATTACHMENT_LOAD_OP_CLEAR     => 1,
      ATTACHMENT_LOAD_OP_DONT_CARE => 2);
   for Attachment_Load_Op_T'Size use Interfaces.C.int'Size;

   type Attachment_Store_Op_T is (ATTACHMENT_STORE_OP_STORE, ATTACHMENT_STORE_OP_DONT_CARE);
   for Attachment_Store_Op_T use (ATTACHMENT_STORE_OP_STORE => 0, ATTACHMENT_STORE_OP_DONT_CARE => 1);
   for Attachment_Store_Op_T'Size use Interfaces.C.int'Size;

   type Image_Type_T is (IMAGE_TYPE_1D, IMAGE_TYPE_2D, IMAGE_TYPE_3D);
   for Image_Type_T use (IMAGE_TYPE_1D => 0, IMAGE_TYPE_2D => 1, IMAGE_TYPE_3D => 2);
   for Image_Type_T'Size use Interfaces.C.int'Size;

   type Image_Tiling_T is (IMAGE_TILING_OPTIMAL, IMAGE_TILING_LINEAR);
   for Image_Tiling_T use (IMAGE_TILING_OPTIMAL => 0, IMAGE_TILING_LINEAR => 1);
   for Image_Tiling_T'Size use Interfaces.C.int'Size;

   type Image_View_Type_T is
     (IMAGE_VIEW_TYPE_1D,
      IMAGE_VIEW_TYPE_2D,
      IMAGE_VIEW_TYPE_3D,
      IMAGE_VIEW_TYPE_CUBE,
      IMAGE_VIEW_TYPE_1D_ARRAY,
      IMAGE_VIEW_TYPE_2D_ARRAY,
      IMAGE_VIEW_TYPE_CUBE_ARRAY);
   for Image_View_Type_T use
     (IMAGE_VIEW_TYPE_1D         => 0,
      IMAGE_VIEW_TYPE_2D         => 1,
      IMAGE_VIEW_TYPE_3D         => 2,
      IMAGE_VIEW_TYPE_CUBE       => 3,
      IMAGE_VIEW_TYPE_1D_ARRAY   => 4,
      IMAGE_VIEW_TYPE_2D_ARRAY   => 5,
      IMAGE_VIEW_TYPE_CUBE_ARRAY => 6);
   for Image_View_Type_T'Size use Interfaces.C.int'Size;

   type Command_Buffer_Level_T is (COMMAND_BUFFER_LEVEL_PRIMARY, COMMAND_BUFFER_LEVEL_SECONDARY);
   for Command_Buffer_Level_T use (COMMAND_BUFFER_LEVEL_PRIMARY => 0, COMMAND_BUFFER_LEVEL_SECONDARY => 1);
   for Command_Buffer_Level_T'Size use Interfaces.C.int'Size;

   type Component_Swizzle_T is
     (COMPONENT_SWIZZLE_IDENTITY,
      COMPONENT_SWIZZLE_ZERO,
      COMPONENT_SWIZZLE_ONE,
      COMPONENT_SWIZZLE_R,
      COMPONENT_SWIZZLE_G,
      COMPONENT_SWIZZLE_B,
      COMPONENT_SWIZZLE_A);
   for Component_Swizzle_T use
     (COMPONENT_SWIZZLE_IDENTITY => 0,
      COMPONENT_SWIZZLE_ZERO     => 1,
      COMPONENT_SWIZZLE_ONE      => 2,
      COMPONENT_SWIZZLE_R        => 3,
      COMPONENT_SWIZZLE_G        => 4,
      COMPONENT_SWIZZLE_B        => 5,
      COMPONENT_SWIZZLE_A        => 6);
   for Component_Swizzle_T'Size use Interfaces.C.int'Size;

   type Descriptor_Type_T is
     (DESCRIPTOR_TYPE_SAMPLER,
      DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
      DESCRIPTOR_TYPE_SAMPLED_IMAGE,
      DESCRIPTOR_TYPE_STORAGE_IMAGE,
      DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,
      DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
      DESCRIPTOR_TYPE_UNIFORM_BUFFER,
      DESCRIPTOR_TYPE_STORAGE_BUFFER,
      DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
      DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
      DESCRIPTOR_TYPE_INPUT_ATTACHMENT);
   for Descriptor_Type_T use
     (DESCRIPTOR_TYPE_SAMPLER                => 0,
      DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER => 1,
      DESCRIPTOR_TYPE_SAMPLED_IMAGE          => 2,
      DESCRIPTOR_TYPE_STORAGE_IMAGE          => 3,
      DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER   => 4,
      DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER   => 5,
      DESCRIPTOR_TYPE_UNIFORM_BUFFER         => 6,
      DESCRIPTOR_TYPE_STORAGE_BUFFER         => 7,
      DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC => 8,
      DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC => 9,
      DESCRIPTOR_TYPE_INPUT_ATTACHMENT       => 10);
   for Descriptor_Type_T'Size use Interfaces.C.int'Size;

   type Query_Type_T is (QUERY_TYPE_OCCLUSION, QUERY_TYPE_PIPELINE_STATISTICS, -- Optional
   QUERY_TYPE_TIMESTAMP);
   for Query_Type_T use (QUERY_TYPE_OCCLUSION => 0, QUERY_TYPE_PIPELINE_STATISTICS => 1, QUERY_TYPE_TIMESTAMP => 2);
   for Query_Type_T'Size use Interfaces.C.int'Size;

   type Border_Color_T is
     (BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
      BORDER_COLOR_INT_TRANSPARENT_BLACK,
      BORDER_COLOR_FLOAT_OPAQUE_BLACK,
      BORDER_COLOR_INT_OPAQUE_BLACK,
      BORDER_COLOR_FLOAT_OPAQUE_WHITE,
      BORDER_COLOR_INT_OPAQUE_WHITE);
   for Border_Color_T use
     (BORDER_COLOR_FLOAT_TRANSPARENT_BLACK => 0,
      BORDER_COLOR_INT_TRANSPARENT_BLACK   => 1,
      BORDER_COLOR_FLOAT_OPAQUE_BLACK      => 2,
      BORDER_COLOR_INT_OPAQUE_BLACK        => 3,
      BORDER_COLOR_FLOAT_OPAQUE_WHITE      => 4,
      BORDER_COLOR_INT_OPAQUE_WHITE        => 5);
   for Border_Color_T'Size use Interfaces.C.int'Size;

   type Pipeline_Bind_Point_T is (PIPELINE_BIND_POINT_GRAPHICS, PIPELINE_BIND_POINT_COMPUTE);
   for Pipeline_Bind_Point_T use (PIPELINE_BIND_POINT_GRAPHICS => 0, PIPELINE_BIND_POINT_COMPUTE => 1);
   for Pipeline_Bind_Point_T'Size use Interfaces.C.int'Size;

   type Pipeline_Cache_Header_Version_T is (PIPELINE_CACHE_HEADER_VERSION_ONE);
   for Pipeline_Cache_Header_Version_T use (PIPELINE_CACHE_HEADER_VERSION_ONE => 1);
   for Pipeline_Cache_Header_Version_T'Size use Interfaces.C.int'Size;

   type Primitive_Topology_T is
     (PRIMITIVE_TOPOLOGY_POINT_LIST,
      PRIMITIVE_TOPOLOGY_LINE_LIST,
      PRIMITIVE_TOPOLOGY_LINE_STRIP,
      PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
      PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
      PRIMITIVE_TOPOLOGY_TRIANGLE_FAN,
      PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY,
      PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY,
      PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY,
      PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY,
      PRIMITIVE_TOPOLOGY_PATCH_LIST);
   for Primitive_Topology_T use
     (PRIMITIVE_TOPOLOGY_POINT_LIST                    => 0,
      PRIMITIVE_TOPOLOGY_LINE_LIST                     => 1,
      PRIMITIVE_TOPOLOGY_LINE_STRIP                    => 2,
      PRIMITIVE_TOPOLOGY_TRIANGLE_LIST                 => 3,
      PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP                => 4,
      PRIMITIVE_TOPOLOGY_TRIANGLE_FAN                  => 5,
      PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY      => 6,
      PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY     => 7,
      PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY  => 8,
      PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY => 9,
      PRIMITIVE_TOPOLOGY_PATCH_LIST                    => 10);
   for Primitive_Topology_T'Size use Interfaces.C.int'Size;

   type Sharing_Mode_T is (SHARING_MODE_EXCLUSIVE, SHARING_MODE_CONCURRENT);
   for Sharing_Mode_T use (SHARING_MODE_EXCLUSIVE => 0, SHARING_MODE_CONCURRENT => 1);
   for Sharing_Mode_T'Size use Interfaces.C.int'Size;

   type Index_Type_T is (INDEX_TYPE_UINT16, INDEX_TYPE_UINT32);
   for Index_Type_T use (INDEX_TYPE_UINT16 => 0, INDEX_TYPE_UINT32 => 1);
   for Index_Type_T'Size use Interfaces.C.int'Size;

   type Filter_T is (FILTER_NEAREST, FILTER_LINEAR);
   for Filter_T use (FILTER_NEAREST => 0, FILTER_LINEAR => 1);
   for Filter_T'Size use Interfaces.C.int'Size;

   type Sampler_Mipmap_Mode_T is (SAMPLER_MIPMAP_MODE_NEAREST, -- Choose nearest mip level
   SAMPLER_MIPMAP_MODE_LINEAR -- Linear filter between mip levels
   );
   for Sampler_Mipmap_Mode_T use (SAMPLER_MIPMAP_MODE_NEAREST => 0, SAMPLER_MIPMAP_MODE_LINEAR => 1);
   for Sampler_Mipmap_Mode_T'Size use Interfaces.C.int'Size;

   type Sampler_Address_Mode_T is
     (SAMPLER_ADDRESS_MODE_REPEAT,
      SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT,
      SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
      SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER);
   for Sampler_Address_Mode_T use
     (SAMPLER_ADDRESS_MODE_REPEAT          => 0,
      SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT => 1,
      SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE   => 2,
      SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER => 3);
   for Sampler_Address_Mode_T'Size use Interfaces.C.int'Size;

   type Compare_Op_T is
     (COMPARE_OP_NEVER,
      COMPARE_OP_LESS,
      COMPARE_OP_EQUAL,
      COMPARE_OP_LESS_OR_EQUAL,
      COMPARE_OP_GREATER,
      COMPARE_OP_NOT_EQUAL,
      COMPARE_OP_GREATER_OR_EQUAL,
      COMPARE_OP_ALWAYS);
   for Compare_Op_T use
     (COMPARE_OP_NEVER            => 0,
      COMPARE_OP_LESS             => 1,
      COMPARE_OP_EQUAL            => 2,
      COMPARE_OP_LESS_OR_EQUAL    => 3,
      COMPARE_OP_GREATER          => 4,
      COMPARE_OP_NOT_EQUAL        => 5,
      COMPARE_OP_GREATER_OR_EQUAL => 6,
      COMPARE_OP_ALWAYS           => 7);
   for Compare_Op_T'Size use Interfaces.C.int'Size;

   type Polygon_Mode_T is (POLYGON_MODE_FILL, POLYGON_MODE_LINE, POLYGON_MODE_POINT);
   for Polygon_Mode_T use (POLYGON_MODE_FILL => 0, POLYGON_MODE_LINE => 1, POLYGON_MODE_POINT => 2);
   for Polygon_Mode_T'Size use Interfaces.C.int'Size;

   type Front_Face_T is (FRONT_FACE_COUNTER_CLOCKWISE, FRONT_FACE_CLOCKWISE);
   for Front_Face_T use (FRONT_FACE_COUNTER_CLOCKWISE => 0, FRONT_FACE_CLOCKWISE => 1);
   for Front_Face_T'Size use Interfaces.C.int'Size;

   type Blend_Factor_T is
     (BLEND_FACTOR_ZERO,
      BLEND_FACTOR_ONE,
      BLEND_FACTOR_SRC_COLOR,
      BLEND_FACTOR_ONE_MINUS_SRC_COLOR,
      BLEND_FACTOR_DST_COLOR,
      BLEND_FACTOR_ONE_MINUS_DST_COLOR,
      BLEND_FACTOR_SRC_ALPHA,
      BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
      BLEND_FACTOR_DST_ALPHA,
      BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
      BLEND_FACTOR_CONSTANT_COLOR,
      BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR,
      BLEND_FACTOR_CONSTANT_ALPHA,
      BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA,
      BLEND_FACTOR_SRC_ALPHA_SATURATE,
      BLEND_FACTOR_SRC1_COLOR,
      BLEND_FACTOR_ONE_MINUS_SRC1_COLOR,
      BLEND_FACTOR_SRC1_ALPHA,
      BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA);
   for Blend_Factor_T use
     (BLEND_FACTOR_ZERO                     => 0,
      BLEND_FACTOR_ONE                      => 1,
      BLEND_FACTOR_SRC_COLOR                => 2,
      BLEND_FACTOR_ONE_MINUS_SRC_COLOR      => 3,
      BLEND_FACTOR_DST_COLOR                => 4,
      BLEND_FACTOR_ONE_MINUS_DST_COLOR      => 5,
      BLEND_FACTOR_SRC_ALPHA                => 6,
      BLEND_FACTOR_ONE_MINUS_SRC_ALPHA      => 7,
      BLEND_FACTOR_DST_ALPHA                => 8,
      BLEND_FACTOR_ONE_MINUS_DST_ALPHA      => 9,
      BLEND_FACTOR_CONSTANT_COLOR           => 10,
      BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR => 11,
      BLEND_FACTOR_CONSTANT_ALPHA           => 12,
      BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA => 13,
      BLEND_FACTOR_SRC_ALPHA_SATURATE       => 14,
      BLEND_FACTOR_SRC1_COLOR               => 15,
      BLEND_FACTOR_ONE_MINUS_SRC1_COLOR     => 16,
      BLEND_FACTOR_SRC1_ALPHA               => 17,
      BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA     => 18);
   for Blend_Factor_T'Size use Interfaces.C.int'Size;

   type Blend_Op_T is (BLEND_OP_ADD, BLEND_OP_SUBTRACT, BLEND_OP_REVERSE_SUBTRACT, BLEND_OP_MIN, BLEND_OP_MAX);
   for Blend_Op_T use
     (BLEND_OP_ADD              => 0,
      BLEND_OP_SUBTRACT         => 1,
      BLEND_OP_REVERSE_SUBTRACT => 2,
      BLEND_OP_MIN              => 3,
      BLEND_OP_MAX              => 4);
   for Blend_Op_T'Size use Interfaces.C.int'Size;

   type Stencil_Op_T is
     (STENCIL_OP_KEEP,
      STENCIL_OP_ZERO,
      STENCIL_OP_REPLACE,
      STENCIL_OP_INCREMENT_AND_CLAMP,
      STENCIL_OP_DECREMENT_AND_CLAMP,
      STENCIL_OP_INVERT,
      STENCIL_OP_INCREMENT_AND_WRAP,
      STENCIL_OP_DECREMENT_AND_WRAP);
   for Stencil_Op_T use
     (STENCIL_OP_KEEP                => 0,
      STENCIL_OP_ZERO                => 1,
      STENCIL_OP_REPLACE             => 2,
      STENCIL_OP_INCREMENT_AND_CLAMP => 3,
      STENCIL_OP_DECREMENT_AND_CLAMP => 4,
      STENCIL_OP_INVERT              => 5,
      STENCIL_OP_INCREMENT_AND_WRAP  => 6,
      STENCIL_OP_DECREMENT_AND_WRAP  => 7);
   for Stencil_Op_T'Size use Interfaces.C.int'Size;

   type Logic_Op_T is
     (LOGIC_OP_CLEAR,
      LOGIC_OP_AND,
      LOGIC_OP_AND_REVERSE,
      LOGIC_OP_COPY,
      LOGIC_OP_AND_INVERTED,
      LOGIC_OP_NO_OP,
      LOGIC_OP_XOR,
      LOGIC_OP_OR,
      LOGIC_OP_NOR,
      LOGIC_OP_EQUIVALENT,
      LOGIC_OP_INVERT,
      LOGIC_OP_OR_REVERSE,
      LOGIC_OP_COPY_INVERTED,
      LOGIC_OP_OR_INVERTED,
      LOGIC_OP_NAND,
      LOGIC_OP_SET);
   for Logic_Op_T use
     (LOGIC_OP_CLEAR         => 0,
      LOGIC_OP_AND           => 1,
      LOGIC_OP_AND_REVERSE   => 2,
      LOGIC_OP_COPY          => 3,
      LOGIC_OP_AND_INVERTED  => 4,
      LOGIC_OP_NO_OP         => 5,
      LOGIC_OP_XOR           => 6,
      LOGIC_OP_OR            => 7,
      LOGIC_OP_NOR           => 8,
      LOGIC_OP_EQUIVALENT    => 9,
      LOGIC_OP_INVERT        => 10,
      LOGIC_OP_OR_REVERSE    => 11,
      LOGIC_OP_COPY_INVERTED => 12,
      LOGIC_OP_OR_INVERTED   => 13,
      LOGIC_OP_NAND          => 14,
      LOGIC_OP_SET           => 15);
   for Logic_Op_T'Size use Interfaces.C.int'Size;

   type Internal_Allocation_Type_T is (INTERNAL_ALLOCATION_TYPE_EXECUTABLE);
   for Internal_Allocation_Type_T use (INTERNAL_ALLOCATION_TYPE_EXECUTABLE => 0);
   for Internal_Allocation_Type_T'Size use Interfaces.C.int'Size;

   type System_Allocation_Scope_T is
     (SYSTEM_ALLOCATION_SCOPE_COMMAND,
      SYSTEM_ALLOCATION_SCOPE_OBJECT,
      SYSTEM_ALLOCATION_SCOPE_CACHE,
      SYSTEM_ALLOCATION_SCOPE_DEVICE,
      SYSTEM_ALLOCATION_SCOPE_INSTANCE);
   for System_Allocation_Scope_T use
     (SYSTEM_ALLOCATION_SCOPE_COMMAND  => 0,
      SYSTEM_ALLOCATION_SCOPE_OBJECT   => 1,
      SYSTEM_ALLOCATION_SCOPE_CACHE    => 2,
      SYSTEM_ALLOCATION_SCOPE_DEVICE   => 3,
      SYSTEM_ALLOCATION_SCOPE_INSTANCE => 4);
   for System_Allocation_Scope_T'Size use Interfaces.C.int'Size;

   type Physical_Device_Type_T is
     (PHYSICAL_DEVICE_TYPE_OTHER,
      PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU,
      PHYSICAL_DEVICE_TYPE_DISCRETE_GPU,
      PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU,
      PHYSICAL_DEVICE_TYPE_CPU);
   for Physical_Device_Type_T use
     (PHYSICAL_DEVICE_TYPE_OTHER          => 0,
      PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU => 1,
      PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   => 2,
      PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    => 3,
      PHYSICAL_DEVICE_TYPE_CPU            => 4);
   for Physical_Device_Type_T'Size use Interfaces.C.int'Size;

   type Vertex_Input_Rate_T is (VERTEX_INPUT_RATE_VERTEX, VERTEX_INPUT_RATE_INSTANCE);
   for Vertex_Input_Rate_T use (VERTEX_INPUT_RATE_VERTEX => 0, VERTEX_INPUT_RATE_INSTANCE => 1);
   for Vertex_Input_Rate_T'Size use Interfaces.C.int'Size;

   type Format_T is
     (FORMAT_UNDEFINED,
      FORMAT_R4G4_UNORM_PACK8,
      FORMAT_R4G4B4A4_UNORM_PACK16,
      FORMAT_B4G4R4A4_UNORM_PACK16,
      FORMAT_R5G6B5_UNORM_PACK16,
      FORMAT_B5G6R5_UNORM_PACK16,
      FORMAT_R5G5B5A1_UNORM_PACK16,
      FORMAT_B5G5R5A1_UNORM_PACK16,
      FORMAT_A1R5G5B5_UNORM_PACK16,
      FORMAT_R8_UNORM,
      FORMAT_R8_SNORM,
      FORMAT_R8_USCALED,
      FORMAT_R8_SSCALED,
      FORMAT_R8_UINT,
      FORMAT_R8_SINT,
      FORMAT_R8_SRGB,
      FORMAT_R8G8_UNORM,
      FORMAT_R8G8_SNORM,
      FORMAT_R8G8_USCALED,
      FORMAT_R8G8_SSCALED,
      FORMAT_R8G8_UINT,
      FORMAT_R8G8_SINT,
      FORMAT_R8G8_SRGB,
      FORMAT_R8G8B8_UNORM,
      FORMAT_R8G8B8_SNORM,
      FORMAT_R8G8B8_USCALED,
      FORMAT_R8G8B8_SSCALED,
      FORMAT_R8G8B8_UINT,
      FORMAT_R8G8B8_SINT,
      FORMAT_R8G8B8_SRGB,
      FORMAT_B8G8R8_UNORM,
      FORMAT_B8G8R8_SNORM,
      FORMAT_B8G8R8_USCALED,
      FORMAT_B8G8R8_SSCALED,
      FORMAT_B8G8R8_UINT,
      FORMAT_B8G8R8_SINT,
      FORMAT_B8G8R8_SRGB,
      FORMAT_R8G8B8A8_UNORM,
      FORMAT_R8G8B8A8_SNORM,
      FORMAT_R8G8B8A8_USCALED,
      FORMAT_R8G8B8A8_SSCALED,
      FORMAT_R8G8B8A8_UINT,
      FORMAT_R8G8B8A8_SINT,
      FORMAT_R8G8B8A8_SRGB,
      FORMAT_B8G8R8A8_UNORM,
      FORMAT_B8G8R8A8_SNORM,
      FORMAT_B8G8R8A8_USCALED,
      FORMAT_B8G8R8A8_SSCALED,
      FORMAT_B8G8R8A8_UINT,
      FORMAT_B8G8R8A8_SINT,
      FORMAT_B8G8R8A8_SRGB,
      FORMAT_A8B8G8R8_UNORM_PACK32,
      FORMAT_A8B8G8R8_SNORM_PACK32,
      FORMAT_A8B8G8R8_USCALED_PACK32,
      FORMAT_A8B8G8R8_SSCALED_PACK32,
      FORMAT_A8B8G8R8_UINT_PACK32,
      FORMAT_A8B8G8R8_SINT_PACK32,
      FORMAT_A8B8G8R8_SRGB_PACK32,
      FORMAT_A2R10G10B10_UNORM_PACK32,
      FORMAT_A2R10G10B10_SNORM_PACK32,
      FORMAT_A2R10G10B10_USCALED_PACK32,
      FORMAT_A2R10G10B10_SSCALED_PACK32,
      FORMAT_A2R10G10B10_UINT_PACK32,
      FORMAT_A2R10G10B10_SINT_PACK32,
      FORMAT_A2B10G10R10_UNORM_PACK32,
      FORMAT_A2B10G10R10_SNORM_PACK32,
      FORMAT_A2B10G10R10_USCALED_PACK32,
      FORMAT_A2B10G10R10_SSCALED_PACK32,
      FORMAT_A2B10G10R10_UINT_PACK32,
      FORMAT_A2B10G10R10_SINT_PACK32,
      FORMAT_R16_UNORM,
      FORMAT_R16_SNORM,
      FORMAT_R16_USCALED,
      FORMAT_R16_SSCALED,
      FORMAT_R16_UINT,
      FORMAT_R16_SINT,
      FORMAT_R16_SFLOAT,
      FORMAT_R16G16_UNORM,
      FORMAT_R16G16_SNORM,
      FORMAT_R16G16_USCALED,
      FORMAT_R16G16_SSCALED,
      FORMAT_R16G16_UINT,
      FORMAT_R16G16_SINT,
      FORMAT_R16G16_SFLOAT,
      FORMAT_R16G16B16_UNORM,
      FORMAT_R16G16B16_SNORM,
      FORMAT_R16G16B16_USCALED,
      FORMAT_R16G16B16_SSCALED,
      FORMAT_R16G16B16_UINT,
      FORMAT_R16G16B16_SINT,
      FORMAT_R16G16B16_SFLOAT,
      FORMAT_R16G16B16A16_UNORM,
      FORMAT_R16G16B16A16_SNORM,
      FORMAT_R16G16B16A16_USCALED,
      FORMAT_R16G16B16A16_SSCALED,
      FORMAT_R16G16B16A16_UINT,
      FORMAT_R16G16B16A16_SINT,
      FORMAT_R16G16B16A16_SFLOAT,
      FORMAT_R32_UINT,
      FORMAT_R32_SINT,
      FORMAT_R32_SFLOAT,
      FORMAT_R32G32_UINT,
      FORMAT_R32G32_SINT,
      FORMAT_R32G32_SFLOAT,
      FORMAT_R32G32B32_UINT,
      FORMAT_R32G32B32_SINT,
      FORMAT_R32G32B32_SFLOAT,
      FORMAT_R32G32B32A32_UINT,
      FORMAT_R32G32B32A32_SINT,
      FORMAT_R32G32B32A32_SFLOAT,
      FORMAT_R64_UINT,
      FORMAT_R64_SINT,
      FORMAT_R64_SFLOAT,
      FORMAT_R64G64_UINT,
      FORMAT_R64G64_SINT,
      FORMAT_R64G64_SFLOAT,
      FORMAT_R64G64B64_UINT,
      FORMAT_R64G64B64_SINT,
      FORMAT_R64G64B64_SFLOAT,
      FORMAT_R64G64B64A64_UINT,
      FORMAT_R64G64B64A64_SINT,
      FORMAT_R64G64B64A64_SFLOAT,
      FORMAT_B10G11R11_UFLOAT_PACK32,
      FORMAT_E5B9G9R9_UFLOAT_PACK32,
      FORMAT_D16_UNORM,
      FORMAT_X8_D24_UNORM_PACK32,
      FORMAT_D32_SFLOAT,
      FORMAT_S8_UINT,
      FORMAT_D16_UNORM_S8_UINT,
      FORMAT_D24_UNORM_S8_UINT,
      FORMAT_D32_SFLOAT_S8_UINT,
      FORMAT_BC1_RGB_UNORM_BLOCK,
      FORMAT_BC1_RGB_SRGB_BLOCK,
      FORMAT_BC1_RGBA_UNORM_BLOCK,
      FORMAT_BC1_RGBA_SRGB_BLOCK,
      FORMAT_BC2_UNORM_BLOCK,
      FORMAT_BC2_SRGB_BLOCK,
      FORMAT_BC3_UNORM_BLOCK,
      FORMAT_BC3_SRGB_BLOCK,
      FORMAT_BC4_UNORM_BLOCK,
      FORMAT_BC4_SNORM_BLOCK,
      FORMAT_BC5_UNORM_BLOCK,
      FORMAT_BC5_SNORM_BLOCK,
      FORMAT_BC6H_UFLOAT_BLOCK,
      FORMAT_BC6H_SFLOAT_BLOCK,
      FORMAT_BC7_UNORM_BLOCK,
      FORMAT_BC7_SRGB_BLOCK,
      FORMAT_ETC2_R8G8B8_UNORM_BLOCK,
      FORMAT_ETC2_R8G8B8_SRGB_BLOCK,
      FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK,
      FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK,
      FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK,
      FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK,
      FORMAT_EAC_R11_UNORM_BLOCK,
      FORMAT_EAC_R11_SNORM_BLOCK,
      FORMAT_EAC_R11G11_UNORM_BLOCK,
      FORMAT_EAC_R11G11_SNORM_BLOCK,
      FORMAT_ASTC_4x4_UNORM_BLOCK,
      FORMAT_ASTC_4x4_SRGB_BLOCK,
      FORMAT_ASTC_5x4_UNORM_BLOCK,
      FORMAT_ASTC_5x4_SRGB_BLOCK,
      FORMAT_ASTC_5x5_UNORM_BLOCK,
      FORMAT_ASTC_5x5_SRGB_BLOCK,
      FORMAT_ASTC_6x5_UNORM_BLOCK,
      FORMAT_ASTC_6x5_SRGB_BLOCK,
      FORMAT_ASTC_6x6_UNORM_BLOCK,
      FORMAT_ASTC_6x6_SRGB_BLOCK,
      FORMAT_ASTC_8x5_UNORM_BLOCK,
      FORMAT_ASTC_8x5_SRGB_BLOCK,
      FORMAT_ASTC_8x6_UNORM_BLOCK,
      FORMAT_ASTC_8x6_SRGB_BLOCK,
      FORMAT_ASTC_8x8_UNORM_BLOCK,
      FORMAT_ASTC_8x8_SRGB_BLOCK,
      FORMAT_ASTC_10x5_UNORM_BLOCK,
      FORMAT_ASTC_10x5_SRGB_BLOCK,
      FORMAT_ASTC_10x6_UNORM_BLOCK,
      FORMAT_ASTC_10x6_SRGB_BLOCK,
      FORMAT_ASTC_10x8_UNORM_BLOCK,
      FORMAT_ASTC_10x8_SRGB_BLOCK,
      FORMAT_ASTC_10x10_UNORM_BLOCK,
      FORMAT_ASTC_10x10_SRGB_BLOCK,
      FORMAT_ASTC_12x10_UNORM_BLOCK,
      FORMAT_ASTC_12x10_SRGB_BLOCK,
      FORMAT_ASTC_12x12_UNORM_BLOCK,
      FORMAT_ASTC_12x12_SRGB_BLOCK);
   for Format_T use
     (FORMAT_UNDEFINED                  => 0,
      FORMAT_R4G4_UNORM_PACK8           => 1,
      FORMAT_R4G4B4A4_UNORM_PACK16      => 2,
      FORMAT_B4G4R4A4_UNORM_PACK16      => 3,
      FORMAT_R5G6B5_UNORM_PACK16        => 4,
      FORMAT_B5G6R5_UNORM_PACK16        => 5,
      FORMAT_R5G5B5A1_UNORM_PACK16      => 6,
      FORMAT_B5G5R5A1_UNORM_PACK16      => 7,
      FORMAT_A1R5G5B5_UNORM_PACK16      => 8,
      FORMAT_R8_UNORM                   => 9,
      FORMAT_R8_SNORM                   => 10,
      FORMAT_R8_USCALED                 => 11,
      FORMAT_R8_SSCALED                 => 12,
      FORMAT_R8_UINT                    => 13,
      FORMAT_R8_SINT                    => 14,
      FORMAT_R8_SRGB                    => 15,
      FORMAT_R8G8_UNORM                 => 16,
      FORMAT_R8G8_SNORM                 => 17,
      FORMAT_R8G8_USCALED               => 18,
      FORMAT_R8G8_SSCALED               => 19,
      FORMAT_R8G8_UINT                  => 20,
      FORMAT_R8G8_SINT                  => 21,
      FORMAT_R8G8_SRGB                  => 22,
      FORMAT_R8G8B8_UNORM               => 23,
      FORMAT_R8G8B8_SNORM               => 24,
      FORMAT_R8G8B8_USCALED             => 25,
      FORMAT_R8G8B8_SSCALED             => 26,
      FORMAT_R8G8B8_UINT                => 27,
      FORMAT_R8G8B8_SINT                => 28,
      FORMAT_R8G8B8_SRGB                => 29,
      FORMAT_B8G8R8_UNORM               => 30,
      FORMAT_B8G8R8_SNORM               => 31,
      FORMAT_B8G8R8_USCALED             => 32,
      FORMAT_B8G8R8_SSCALED             => 33,
      FORMAT_B8G8R8_UINT                => 34,
      FORMAT_B8G8R8_SINT                => 35,
      FORMAT_B8G8R8_SRGB                => 36,
      FORMAT_R8G8B8A8_UNORM             => 37,
      FORMAT_R8G8B8A8_SNORM             => 38,
      FORMAT_R8G8B8A8_USCALED           => 39,
      FORMAT_R8G8B8A8_SSCALED           => 40,
      FORMAT_R8G8B8A8_UINT              => 41,
      FORMAT_R8G8B8A8_SINT              => 42,
      FORMAT_R8G8B8A8_SRGB              => 43,
      FORMAT_B8G8R8A8_UNORM             => 44,
      FORMAT_B8G8R8A8_SNORM             => 45,
      FORMAT_B8G8R8A8_USCALED           => 46,
      FORMAT_B8G8R8A8_SSCALED           => 47,
      FORMAT_B8G8R8A8_UINT              => 48,
      FORMAT_B8G8R8A8_SINT              => 49,
      FORMAT_B8G8R8A8_SRGB              => 50,
      FORMAT_A8B8G8R8_UNORM_PACK32      => 51,
      FORMAT_A8B8G8R8_SNORM_PACK32      => 52,
      FORMAT_A8B8G8R8_USCALED_PACK32    => 53,
      FORMAT_A8B8G8R8_SSCALED_PACK32    => 54,
      FORMAT_A8B8G8R8_UINT_PACK32       => 55,
      FORMAT_A8B8G8R8_SINT_PACK32       => 56,
      FORMAT_A8B8G8R8_SRGB_PACK32       => 57,
      FORMAT_A2R10G10B10_UNORM_PACK32   => 58,
      FORMAT_A2R10G10B10_SNORM_PACK32   => 59,
      FORMAT_A2R10G10B10_USCALED_PACK32 => 60,
      FORMAT_A2R10G10B10_SSCALED_PACK32 => 61,
      FORMAT_A2R10G10B10_UINT_PACK32    => 62,
      FORMAT_A2R10G10B10_SINT_PACK32    => 63,
      FORMAT_A2B10G10R10_UNORM_PACK32   => 64,
      FORMAT_A2B10G10R10_SNORM_PACK32   => 65,
      FORMAT_A2B10G10R10_USCALED_PACK32 => 66,
      FORMAT_A2B10G10R10_SSCALED_PACK32 => 67,
      FORMAT_A2B10G10R10_UINT_PACK32    => 68,
      FORMAT_A2B10G10R10_SINT_PACK32    => 69,
      FORMAT_R16_UNORM                  => 70,
      FORMAT_R16_SNORM                  => 71,
      FORMAT_R16_USCALED                => 72,
      FORMAT_R16_SSCALED                => 73,
      FORMAT_R16_UINT                   => 74,
      FORMAT_R16_SINT                   => 75,
      FORMAT_R16_SFLOAT                 => 76,
      FORMAT_R16G16_UNORM               => 77,
      FORMAT_R16G16_SNORM               => 78,
      FORMAT_R16G16_USCALED             => 79,
      FORMAT_R16G16_SSCALED             => 80,
      FORMAT_R16G16_UINT                => 81,
      FORMAT_R16G16_SINT                => 82,
      FORMAT_R16G16_SFLOAT              => 83,
      FORMAT_R16G16B16_UNORM            => 84,
      FORMAT_R16G16B16_SNORM            => 85,
      FORMAT_R16G16B16_USCALED          => 86,
      FORMAT_R16G16B16_SSCALED          => 87,
      FORMAT_R16G16B16_UINT             => 88,
      FORMAT_R16G16B16_SINT             => 89,
      FORMAT_R16G16B16_SFLOAT           => 90,
      FORMAT_R16G16B16A16_UNORM         => 91,
      FORMAT_R16G16B16A16_SNORM         => 92,
      FORMAT_R16G16B16A16_USCALED       => 93,
      FORMAT_R16G16B16A16_SSCALED       => 94,
      FORMAT_R16G16B16A16_UINT          => 95,
      FORMAT_R16G16B16A16_SINT          => 96,
      FORMAT_R16G16B16A16_SFLOAT        => 97,
      FORMAT_R32_UINT                   => 98,
      FORMAT_R32_SINT                   => 99,
      FORMAT_R32_SFLOAT                 => 100,
      FORMAT_R32G32_UINT                => 101,
      FORMAT_R32G32_SINT                => 102,
      FORMAT_R32G32_SFLOAT              => 103,
      FORMAT_R32G32B32_UINT             => 104,
      FORMAT_R32G32B32_SINT             => 105,
      FORMAT_R32G32B32_SFLOAT           => 106,
      FORMAT_R32G32B32A32_UINT          => 107,
      FORMAT_R32G32B32A32_SINT          => 108,
      FORMAT_R32G32B32A32_SFLOAT        => 109,
      FORMAT_R64_UINT                   => 110,
      FORMAT_R64_SINT                   => 111,
      FORMAT_R64_SFLOAT                 => 112,
      FORMAT_R64G64_UINT                => 113,
      FORMAT_R64G64_SINT                => 114,
      FORMAT_R64G64_SFLOAT              => 115,
      FORMAT_R64G64B64_UINT             => 116,
      FORMAT_R64G64B64_SINT             => 117,
      FORMAT_R64G64B64_SFLOAT           => 118,
      FORMAT_R64G64B64A64_UINT          => 119,
      FORMAT_R64G64B64A64_SINT          => 120,
      FORMAT_R64G64B64A64_SFLOAT        => 121,
      FORMAT_B10G11R11_UFLOAT_PACK32    => 122,
      FORMAT_E5B9G9R9_UFLOAT_PACK32     => 123,
      FORMAT_D16_UNORM                  => 124,
      FORMAT_X8_D24_UNORM_PACK32        => 125,
      FORMAT_D32_SFLOAT                 => 126,
      FORMAT_S8_UINT                    => 127,
      FORMAT_D16_UNORM_S8_UINT          => 128,
      FORMAT_D24_UNORM_S8_UINT          => 129,
      FORMAT_D32_SFLOAT_S8_UINT         => 130,
      FORMAT_BC1_RGB_UNORM_BLOCK        => 131,
      FORMAT_BC1_RGB_SRGB_BLOCK         => 132,
      FORMAT_BC1_RGBA_UNORM_BLOCK       => 133,
      FORMAT_BC1_RGBA_SRGB_BLOCK        => 134,
      FORMAT_BC2_UNORM_BLOCK            => 135,
      FORMAT_BC2_SRGB_BLOCK             => 136,
      FORMAT_BC3_UNORM_BLOCK            => 137,
      FORMAT_BC3_SRGB_BLOCK             => 138,
      FORMAT_BC4_UNORM_BLOCK            => 139,
      FORMAT_BC4_SNORM_BLOCK            => 140,
      FORMAT_BC5_UNORM_BLOCK            => 141,
      FORMAT_BC5_SNORM_BLOCK            => 142,
      FORMAT_BC6H_UFLOAT_BLOCK          => 143,
      FORMAT_BC6H_SFLOAT_BLOCK          => 144,
      FORMAT_BC7_UNORM_BLOCK            => 145,
      FORMAT_BC7_SRGB_BLOCK             => 146,
      FORMAT_ETC2_R8G8B8_UNORM_BLOCK    => 147,
      FORMAT_ETC2_R8G8B8_SRGB_BLOCK     => 148,
      FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK  => 149,
      FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK   => 150,
      FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK  => 151,
      FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK   => 152,
      FORMAT_EAC_R11_UNORM_BLOCK        => 153,
      FORMAT_EAC_R11_SNORM_BLOCK        => 154,
      FORMAT_EAC_R11G11_UNORM_BLOCK     => 155,
      FORMAT_EAC_R11G11_SNORM_BLOCK     => 156,
      FORMAT_ASTC_4x4_UNORM_BLOCK       => 157,
      FORMAT_ASTC_4x4_SRGB_BLOCK        => 158,
      FORMAT_ASTC_5x4_UNORM_BLOCK       => 159,
      FORMAT_ASTC_5x4_SRGB_BLOCK        => 160,
      FORMAT_ASTC_5x5_UNORM_BLOCK       => 161,
      FORMAT_ASTC_5x5_SRGB_BLOCK        => 162,
      FORMAT_ASTC_6x5_UNORM_BLOCK       => 163,
      FORMAT_ASTC_6x5_SRGB_BLOCK        => 164,
      FORMAT_ASTC_6x6_UNORM_BLOCK       => 165,
      FORMAT_ASTC_6x6_SRGB_BLOCK        => 166,
      FORMAT_ASTC_8x5_UNORM_BLOCK       => 167,
      FORMAT_ASTC_8x5_SRGB_BLOCK        => 168,
      FORMAT_ASTC_8x6_UNORM_BLOCK       => 169,
      FORMAT_ASTC_8x6_SRGB_BLOCK        => 170,
      FORMAT_ASTC_8x8_UNORM_BLOCK       => 171,
      FORMAT_ASTC_8x8_SRGB_BLOCK        => 172,
      FORMAT_ASTC_10x5_UNORM_BLOCK      => 173,
      FORMAT_ASTC_10x5_SRGB_BLOCK       => 174,
      FORMAT_ASTC_10x6_UNORM_BLOCK      => 175,
      FORMAT_ASTC_10x6_SRGB_BLOCK       => 176,
      FORMAT_ASTC_10x8_UNORM_BLOCK      => 177,
      FORMAT_ASTC_10x8_SRGB_BLOCK       => 178,
      FORMAT_ASTC_10x10_UNORM_BLOCK     => 179,
      FORMAT_ASTC_10x10_SRGB_BLOCK      => 180,
      FORMAT_ASTC_12x10_UNORM_BLOCK     => 181,
      FORMAT_ASTC_12x10_SRGB_BLOCK      => 182,
      FORMAT_ASTC_12x12_UNORM_BLOCK     => 183,
      FORMAT_ASTC_12x12_SRGB_BLOCK      => 184);
   for Format_T'Size use Interfaces.C.int'Size;

   type Structure_Type_T is
     (STRUCTURE_TYPE_APPLICATION_INFO,
      STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
      STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
      STRUCTURE_TYPE_DEVICE_CREATE_INFO,
      STRUCTURE_TYPE_SUBMIT_INFO,
      STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
      STRUCTURE_TYPE_BIND_SPARSE_INFO,
      STRUCTURE_TYPE_FENCE_CREATE_INFO,
      STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
      STRUCTURE_TYPE_EVENT_CREATE_INFO,
      STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
      STRUCTURE_TYPE_BUFFER_CREATE_INFO,
      STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO,
      STRUCTURE_TYPE_IMAGE_CREATE_INFO,
      STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
      STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
      STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
      STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
      STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
      STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
      STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
      STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
      STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
      STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
      STRUCTURE_TYPE_COPY_DESCRIPTOR_SET,
      STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
      STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
      STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
      STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO,
      STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
      STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
      STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
      STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
      STRUCTURE_TYPE_MEMORY_BARRIER,
      STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO,
      STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO);
   for Structure_Type_T use
     (STRUCTURE_TYPE_APPLICATION_INFO                          => 0,
      STRUCTURE_TYPE_INSTANCE_CREATE_INFO                      => 1,
      STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO                  => 2,
      STRUCTURE_TYPE_DEVICE_CREATE_INFO                        => 3,
      STRUCTURE_TYPE_SUBMIT_INFO                               => 4,
      STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO                      => 5,
      STRUCTURE_TYPE_MAPPED_MEMORY_RANGE                       => 6,
      STRUCTURE_TYPE_BIND_SPARSE_INFO                          => 7,
      STRUCTURE_TYPE_FENCE_CREATE_INFO                         => 8,
      STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO                     => 9,
      STRUCTURE_TYPE_EVENT_CREATE_INFO                         => 10,
      STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO                    => 11,
      STRUCTURE_TYPE_BUFFER_CREATE_INFO                        => 12,
      STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO                   => 13,
      STRUCTURE_TYPE_IMAGE_CREATE_INFO                         => 14,
      STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO                    => 15,
      STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO                 => 16,
      STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO                => 17,
      STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO         => 18,
      STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO   => 19,
      STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO => 20,
      STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO   => 21,
      STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO       => 22,
      STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO  => 23,
      STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO    => 24,
      STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO  => 25,
      STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO    => 26,
      STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO        => 27,
      STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO             => 28,
      STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO              => 29,
      STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO               => 30,
      STRUCTURE_TYPE_SAMPLER_CREATE_INFO                       => 31,
      STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO         => 32,
      STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO               => 33,
      STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO              => 34,
      STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET                      => 35,
      STRUCTURE_TYPE_COPY_DESCRIPTOR_SET                       => 36,
      STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO                   => 37,
      STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO                   => 38,
      STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO                  => 39,
      STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO              => 40,
      STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO           => 41,
      STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO                 => 42,
      STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO                    => 43,
      STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER                     => 44,
      STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER                      => 45,
      STRUCTURE_TYPE_MEMORY_BARRIER                            => 46,
      STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO               => 47,
      STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO                 => 48);
   for Structure_Type_T'Size use Interfaces.C.int'Size;

   type Subpass_Contents_T is (SUBPASS_CONTENTS_INLINE, SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS);
   for Subpass_Contents_T use (SUBPASS_CONTENTS_INLINE => 0, SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS => 1);
   for Subpass_Contents_T'Size use Interfaces.C.int'Size;

   type Result_T is
     (ERROR_FORMAT_NOT_SUPPORTED, -- Requested format is not supported on this device
      ERROR_TOO_MANY_OBJECTS, -- Too many objects of the type have already been created
      ERROR_INCOMPATIBLE_DRIVER, -- Unable to find a Vulkan driver
      ERROR_FEATURE_NOT_PRESENT, -- Requested feature is not available on this device
      ERROR_EXTENSION_NOT_PRESENT, -- Extension specified does not exist
      ERROR_LAYER_NOT_PRESENT, -- Layer specified does not exist
      ERROR_MEMORY_MAP_FAILED, -- Mapping of a memory object has failed
      ERROR_DEVICE_LOST, -- The logical device has been lost. See &lt;&lt;devsandqueues-lost-device&gt;&gt;
      ERROR_INITIALIZATION_FAILED, -- Initialization of a object has failed
      ERROR_OUT_OF_DEVICE_MEMORY, -- A device memory allocation has failed
      ERROR_OUT_OF_HOST_MEMORY, -- A host memory allocation has failed
      SUCCESS, -- Command completed successfully
      NOT_READY, -- A fence or query has not yet completed
      TIMEOUT, -- A wait operation has not completed in the specified time
      EVENT_SET, -- An event is signaled
      EVENT_RESET, -- An event is unsignaled
      INCOMPLETE -- A return array was too small for the result
      );
   for Result_T use
     (ERROR_FORMAT_NOT_SUPPORTED  => -11,
      ERROR_TOO_MANY_OBJECTS      => -10,
      ERROR_INCOMPATIBLE_DRIVER   => -9,
      ERROR_FEATURE_NOT_PRESENT   => -8,
      ERROR_EXTENSION_NOT_PRESENT => -7,
      ERROR_LAYER_NOT_PRESENT     => -6,
      ERROR_MEMORY_MAP_FAILED     => -5,
      ERROR_DEVICE_LOST           => -4,
      ERROR_INITIALIZATION_FAILED => -3,
      ERROR_OUT_OF_DEVICE_MEMORY  => -2,
      ERROR_OUT_OF_HOST_MEMORY    => -1,
      SUCCESS                     => 0,
      NOT_READY                   => 1,
      TIMEOUT                     => 2,
      EVENT_SET                   => 3,
      EVENT_RESET                 => 4,
      INCOMPLETE                  => 5);
   for Result_T'Size use Interfaces.C.int'Size;

   type Dynamic_State_T is
     (DYNAMIC_STATE_VIEWPORT,
      DYNAMIC_STATE_SCISSOR,
      DYNAMIC_STATE_LINE_WIDTH,
      DYNAMIC_STATE_DEPTH_BIAS,
      DYNAMIC_STATE_BLEND_CONSTANTS,
      DYNAMIC_STATE_DEPTH_BOUNDS,
      DYNAMIC_STATE_STENCIL_COMPARE_MASK,
      DYNAMIC_STATE_STENCIL_WRITE_MASK,
      DYNAMIC_STATE_STENCIL_REFERENCE);
   for Dynamic_State_T use
     (DYNAMIC_STATE_VIEWPORT             => 0,
      DYNAMIC_STATE_SCISSOR              => 1,
      DYNAMIC_STATE_LINE_WIDTH           => 2,
      DYNAMIC_STATE_DEPTH_BIAS           => 3,
      DYNAMIC_STATE_BLEND_CONSTANTS      => 4,
      DYNAMIC_STATE_DEPTH_BOUNDS         => 5,
      DYNAMIC_STATE_STENCIL_COMPARE_MASK => 6,
      DYNAMIC_STATE_STENCIL_WRITE_MASK   => 7,
      DYNAMIC_STATE_STENCIL_REFERENCE    => 8);
   for Dynamic_State_T'Size use Interfaces.C.int'Size;

   type Present_Mode_Khr_T is
     (PRESENT_MODE_IMMEDIATE_KHR, PRESENT_MODE_MAILBOX_KHR, PRESENT_MODE_FIFO_KHR, PRESENT_MODE_FIFO_RELAXED_KHR);
   for Present_Mode_Khr_T use
     (PRESENT_MODE_IMMEDIATE_KHR    => 0,
      PRESENT_MODE_MAILBOX_KHR      => 1,
      PRESENT_MODE_FIFO_KHR         => 2,
      PRESENT_MODE_FIFO_RELAXED_KHR => 3);
   for Present_Mode_Khr_T'Size use Interfaces.C.int'Size;

   type Color_Space_Khr_T is (COLOR_SPACE_SRGB_NONLINEAR_KHR);
   for Color_Space_Khr_T use (COLOR_SPACE_SRGB_NONLINEAR_KHR => 0);
   for Color_Space_Khr_T'Size use Interfaces.C.int'Size;

   type Debug_Report_Object_Type_Ext_T is
     (DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT,
      DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT,
      DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT,
      DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT,
      DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT,
      DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT,
      DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT,
      DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT,
      DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT,
      DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT,
      DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT,
      DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT,
      DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT,
      DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT,
      DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT,
      DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT,
      DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT,
      DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT,
      DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT,
      DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT);
   for Debug_Report_Object_Type_Ext_T use
     (DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT               => 0,
      DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT              => 1,
      DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT       => 2,
      DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT                => 3,
      DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT                 => 4,
      DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT             => 5,
      DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT        => 6,
      DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT                 => 7,
      DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT         => 8,
      DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT                => 9,
      DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT                 => 10,
      DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT                 => 11,
      DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT            => 12,
      DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT           => 13,
      DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT            => 14,
      DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT         => 15,
      DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT        => 16,
      DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT       => 17,
      DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT           => 18,
      DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT              => 19,
      DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT => 20,
      DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT               => 21,
      DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT       => 22,
      DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT        => 23,
      DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT           => 24,
      DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT          => 25,
      DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT           => 26,
      DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT         => 27,
      DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT          => 28);
   for Debug_Report_Object_Type_Ext_T'Size use Interfaces.C.int'Size;

   type Debug_Report_Error_Ext_T is (DEBUG_REPORT_ERROR_NONE_EXT, DEBUG_REPORT_ERROR_CALLBACK_REF_EXT);
   for Debug_Report_Error_Ext_T use (DEBUG_REPORT_ERROR_NONE_EXT => 0, DEBUG_REPORT_ERROR_CALLBACK_REF_EXT => 1);
   for Debug_Report_Error_Ext_T'Size use Interfaces.C.int'Size;

   type Rasterization_Order_Amd_T is (RASTERIZATION_ORDER_STRICT_AMD, RASTERIZATION_ORDER_RELAXED_AMD);
   for Rasterization_Order_Amd_T use (RASTERIZATION_ORDER_STRICT_AMD => 0, RASTERIZATION_ORDER_RELAXED_AMD => 1);
   for Rasterization_Order_Amd_T'Size use Interfaces.C.int'Size;

   type Sample_Mask_T is new Interfaces.Unsigned_32;

   type Bool_32_T is new Interfaces.Unsigned_32;

   type Flags_T is new Interfaces.Unsigned_32;

   type Device_Size_T is new Interfaces.Unsigned_64;

   type Framebuffer_Create_Flags_T is new Flags_T;

   type Query_Pool_Create_Flags_T is new Flags_T;

   type Render_Pass_Create_Flags_T is new Flags_T;

   type Sampler_Create_Flags_T is new Flags_T;

   type Pipeline_Layout_Create_Flags_T is new Flags_T;

   type Pipeline_Cache_Create_Flags_T is new Flags_T;

   type Pipeline_Depth_Stencil_State_Create_Flags_T is new Flags_T;

   type Pipeline_Dynamic_State_Create_Flags_T is new Flags_T;

   type Pipeline_Color_Blend_State_Create_Flags_T is new Flags_T;

   type Pipeline_Multisample_State_Create_Flags_T is new Flags_T;

   type Pipeline_Rasterization_State_Create_Flags_T is new Flags_T;

   type Pipeline_Viewport_State_Create_Flags_T is new Flags_T;

   type Pipeline_Tessellation_State_Create_Flags_T is new Flags_T;

   type Pipeline_Input_Assembly_State_Create_Flags_T is new Flags_T;

   type Pipeline_Vertex_Input_State_Create_Flags_T is new Flags_T;

   type Pipeline_Shader_Stage_Create_Flags_T is new Flags_T;

   type Descriptor_Set_Layout_Create_Flags_T is new Flags_T;

   type Buffer_View_Create_Flags_T is new Flags_T;

   type Instance_Create_Flags_T is new Flags_T;

   type Device_Create_Flags_T is new Flags_T;

   type Device_Queue_Create_Flags_T is new Flags_T;

   type Queue_Flags_T is new Flags_T;

   type Queue_Flag_Bits_T is new Queue_Flags_T;
   QUEUE_GRAPHICS_BIT       : Queue_Flag_Bits_T := 1; -- Queue supports graphics operations
   QUEUE_COMPUTE_BIT        : Queue_Flag_Bits_T := 2; -- Queue supports compute operations
   QUEUE_TRANSFER_BIT       : Queue_Flag_Bits_T := 4; -- Queue supports transfer operations
   QUEUE_SPARSE_BINDING_BIT : Queue_Flag_Bits_T := 8; -- Queue supports sparse resource memory management operations

   type Memory_Property_Flags_T is new Flags_T;

   type Memory_Property_Flag_Bits_T is new Memory_Property_Flags_T;
   MEMORY_PROPERTY_DEVICE_LOCAL_BIT : Memory_Property_Flag_Bits_T :=
     1; -- If otherwise stated, then allocate memory on device
   MEMORY_PROPERTY_HOST_VISIBLE_BIT  : Memory_Property_Flag_Bits_T := 2; -- Memory is mappable by host
   MEMORY_PROPERTY_HOST_COHERENT_BIT : Memory_Property_Flag_Bits_T :=
     4; -- Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
   MEMORY_PROPERTY_HOST_CACHED_BIT      : Memory_Property_Flag_Bits_T := 8; -- Memory will be cached by the host
   MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT : Memory_Property_Flag_Bits_T :=
     16; -- Memory may be allocated by the driver when it is required

   type Memory_Heap_Flags_T is new Flags_T;

   type Memory_Heap_Flag_Bits_T is new Memory_Heap_Flags_T;
   MEMORY_HEAP_DEVICE_LOCAL_BIT : Memory_Heap_Flag_Bits_T := 1; -- If set, heap represents device memory

   type Access_Flags_T is new Flags_T;

   type Access_Flag_Bits_T is new Access_Flags_T;
   ACCESS_INDIRECT_COMMAND_READ_BIT         : Access_Flag_Bits_T := 1; -- Controls coherency of indirect command reads
   ACCESS_INDEX_READ_BIT                    : Access_Flag_Bits_T := 2; -- Controls coherency of index reads
   ACCESS_VERTEX_ATTRIBUTE_READ_BIT         : Access_Flag_Bits_T := 4; -- Controls coherency of vertex attribute reads
   ACCESS_UNIFORM_READ_BIT                  : Access_Flag_Bits_T := 8; -- Controls coherency of uniform buffer reads
   ACCESS_INPUT_ATTACHMENT_READ_BIT         : Access_Flag_Bits_T := 16; -- Controls coherency of input attachment reads
   ACCESS_SHADER_READ_BIT                   : Access_Flag_Bits_T := 32; -- Controls coherency of shader reads
   ACCESS_SHADER_WRITE_BIT                  : Access_Flag_Bits_T := 64; -- Controls coherency of shader writes
   ACCESS_COLOR_ATTACHMENT_READ_BIT         : Access_Flag_Bits_T := 128; -- Controls coherency of color attachment reads
   ACCESS_COLOR_ATTACHMENT_WRITE_BIT : Access_Flag_Bits_T := 256; -- Controls coherency of color attachment writes
   ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT : Access_Flag_Bits_T :=
     512; -- Controls coherency of depth/stencil attachment reads
   ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT : Access_Flag_Bits_T :=
     1024; -- Controls coherency of depth/stencil attachment writes
   ACCESS_TRANSFER_READ_BIT  : Access_Flag_Bits_T := 2048; -- Controls coherency of transfer reads
   ACCESS_TRANSFER_WRITE_BIT : Access_Flag_Bits_T := 4096; -- Controls coherency of transfer writes
   ACCESS_HOST_READ_BIT      : Access_Flag_Bits_T := 8192; -- Controls coherency of host reads
   ACCESS_HOST_WRITE_BIT     : Access_Flag_Bits_T := 16384; -- Controls coherency of host writes
   ACCESS_MEMORY_READ_BIT    : Access_Flag_Bits_T := 32768; -- Controls coherency of memory reads
   ACCESS_MEMORY_WRITE_BIT   : Access_Flag_Bits_T := 65536; -- Controls coherency of memory writes

   type Buffer_Usage_Flags_T is new Flags_T;

   type Buffer_Usage_Flag_Bits_T is new Buffer_Usage_Flags_T;
   BUFFER_USAGE_TRANSFER_SRC_BIT : Buffer_Usage_Flag_Bits_T := 1; -- Can be used as a source of transfer operations
   BUFFER_USAGE_TRANSFER_DST_BIT : Buffer_Usage_Flag_Bits_T := 2; -- Can be used as a destination of transfer operations
   BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT : Buffer_Usage_Flag_Bits_T := 4; -- Can be used as TBO
   BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT : Buffer_Usage_Flag_Bits_T := 8; -- Can be used as IBO
   BUFFER_USAGE_UNIFORM_BUFFER_BIT       : Buffer_Usage_Flag_Bits_T := 16; -- Can be used as UBO
   BUFFER_USAGE_STORAGE_BUFFER_BIT       : Buffer_Usage_Flag_Bits_T := 32; -- Can be used as SSBO
   BUFFER_USAGE_INDEX_BUFFER_BIT         : Buffer_Usage_Flag_Bits_T :=
     64; -- Can be used as source of fixed-function index fetch (index buffer)
   BUFFER_USAGE_VERTEX_BUFFER_BIT : Buffer_Usage_Flag_Bits_T :=
     128; -- Can be used as source of fixed-function vertex fetch (VBO)
   BUFFER_USAGE_INDIRECT_BUFFER_BIT : Buffer_Usage_Flag_Bits_T :=
     256; -- Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)

   type Buffer_Create_Flags_T is new Flags_T;

   type Buffer_Create_Flag_Bits_T is new Buffer_Create_Flags_T;
   BUFFER_CREATE_SPARSE_BINDING_BIT   : Buffer_Create_Flag_Bits_T := 1; -- Buffer should support sparse backing
   BUFFER_CREATE_SPARSE_RESIDENCY_BIT : Buffer_Create_Flag_Bits_T :=
     2; -- Buffer should support sparse backing with partial residency
   BUFFER_CREATE_SPARSE_ALIASED_BIT : Buffer_Create_Flag_Bits_T :=
     4; -- Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers

   type Shader_Stage_Flags_T is new Flags_T;

   type Shader_Stage_Flag_Bits_T is new Shader_Stage_Flags_T;
   SHADER_STAGE_VERTEX_BIT                  : Shader_Stage_Flag_Bits_T := 1;
   SHADER_STAGE_TESSELLATION_CONTROL_BIT    : Shader_Stage_Flag_Bits_T := 2;
   SHADER_STAGE_TESSELLATION_EVALUATION_BIT : Shader_Stage_Flag_Bits_T := 4;
   SHADER_STAGE_GEOMETRY_BIT                : Shader_Stage_Flag_Bits_T := 8;
   SHADER_STAGE_FRAGMENT_BIT                : Shader_Stage_Flag_Bits_T := 16;
   SHADER_STAGE_COMPUTE_BIT                 : Shader_Stage_Flag_Bits_T := 32;
   SHADER_STAGE_ALL_GRAPHICS                : Shader_Stage_Flag_Bits_T := 16#0000001F#;
   SHADER_STAGE_ALL                         : Shader_Stage_Flag_Bits_T := 16#7FFFFFFF#;

   type Image_Usage_Flags_T is new Flags_T;

   type Image_Usage_Flag_Bits_T is new Image_Usage_Flags_T;
   IMAGE_USAGE_TRANSFER_SRC_BIT : Image_Usage_Flag_Bits_T := 1; -- Can be used as a source of transfer operations
   IMAGE_USAGE_TRANSFER_DST_BIT : Image_Usage_Flag_Bits_T := 2; -- Can be used as a destination of transfer operations
   IMAGE_USAGE_SAMPLED_BIT      : Image_Usage_Flag_Bits_T :=
     4; -- Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
   IMAGE_USAGE_STORAGE_BIT : Image_Usage_Flag_Bits_T :=
     8; -- Can be used as storage image (STORAGE_IMAGE descriptor type)
   IMAGE_USAGE_COLOR_ATTACHMENT_BIT : Image_Usage_Flag_Bits_T := 16; -- Can be used as framebuffer color attachment
   IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT : Image_Usage_Flag_Bits_T :=
     32; -- Can be used as framebuffer depth/stencil attachment
   IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT : Image_Usage_Flag_Bits_T := 64; -- Image data not needed outside of rendering
   IMAGE_USAGE_INPUT_ATTACHMENT_BIT     : Image_Usage_Flag_Bits_T := 128; -- Can be used as framebuffer input attachment

   type Image_Create_Flags_T is new Flags_T;

   type Image_Create_Flag_Bits_T is new Image_Create_Flags_T;
   IMAGE_CREATE_SPARSE_BINDING_BIT   : Image_Create_Flag_Bits_T := 1; -- Image should support sparse backing
   IMAGE_CREATE_SPARSE_RESIDENCY_BIT : Image_Create_Flag_Bits_T :=
     2; -- Image should support sparse backing with partial residency
   IMAGE_CREATE_SPARSE_ALIASED_BIT : Image_Create_Flag_Bits_T :=
     4; -- Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
   IMAGE_CREATE_MUTABLE_FORMAT_BIT : Image_Create_Flag_Bits_T :=
     8; -- Allows image views to have different format than the base image
   IMAGE_CREATE_CUBE_COMPATIBLE_BIT : Image_Create_Flag_Bits_T :=
     16; -- Allows creating image views with cube type from the created image

   type Image_View_Create_Flags_T is new Flags_T;

   type Pipeline_Create_Flags_T is new Flags_T;

   type Pipeline_Create_Flag_Bits_T is new Pipeline_Create_Flags_T;
   PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT : Pipeline_Create_Flag_Bits_T := 1;
   PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT    : Pipeline_Create_Flag_Bits_T := 2;
   PIPELINE_CREATE_DERIVATIVE_BIT           : Pipeline_Create_Flag_Bits_T := 4;

   type Color_Component_Flags_T is new Flags_T;

   type Color_Component_Flag_Bits_T is new Color_Component_Flags_T;
   COLOR_COMPONENT_R_BIT : Color_Component_Flag_Bits_T := 1;
   COLOR_COMPONENT_G_BIT : Color_Component_Flag_Bits_T := 2;
   COLOR_COMPONENT_B_BIT : Color_Component_Flag_Bits_T := 4;
   COLOR_COMPONENT_A_BIT : Color_Component_Flag_Bits_T := 8;

   type Fence_Create_Flags_T is new Flags_T;

   type Fence_Create_Flag_Bits_T is new Fence_Create_Flags_T;
   FENCE_CREATE_SIGNALED_BIT : Fence_Create_Flag_Bits_T := 1;

   type Semaphore_Create_Flags_T is new Flags_T;

   type Format_Feature_Flags_T is new Flags_T;

   type Format_Feature_Flag_Bits_T is new Format_Feature_Flags_T;
   FORMAT_FEATURE_SAMPLED_IMAGE_BIT : Format_Feature_Flag_Bits_T :=
     1; -- Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
   FORMAT_FEATURE_STORAGE_IMAGE_BIT : Format_Feature_Flag_Bits_T :=
     2; -- Format can be used for storage images (STORAGE_IMAGE descriptor type)
   FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT : Format_Feature_Flag_Bits_T :=
     4; -- Format supports atomic operations in case it's used for storage images
   FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT : Format_Feature_Flag_Bits_T :=
     8; -- Format can be used for uniform texel buffers (TBOs)
   FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT : Format_Feature_Flag_Bits_T :=
     16; -- Format can be used for storage texel buffers (IBOs)
   FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT : Format_Feature_Flag_Bits_T :=
     32; -- Format supports atomic operations in case it's used for storage texel buffers
   FORMAT_FEATURE_VERTEX_BUFFER_BIT : Format_Feature_Flag_Bits_T := 64; -- Format can be used for vertex buffers (VBOs)
   FORMAT_FEATURE_COLOR_ATTACHMENT_BIT : Format_Feature_Flag_Bits_T :=
     128; -- Format can be used for color attachment images
   FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT : Format_Feature_Flag_Bits_T :=
     256; -- Format supports blending in case it's used for color attachment images
   FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT : Format_Feature_Flag_Bits_T :=
     512; -- Format can be used for depth/stencil attachment images
   FORMAT_FEATURE_BLIT_SRC_BIT : Format_Feature_Flag_Bits_T :=
     1024; -- Format can be used as the source image of blits with vkCmdBlitImage
   FORMAT_FEATURE_BLIT_DST_BIT : Format_Feature_Flag_Bits_T :=
     2048; -- Format can be used as the destination image of blits with vkCmdBlitImage
   FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT : Format_Feature_Flag_Bits_T :=
     4096; -- Format can be filtered with VK_FILTER_LINEAR when being sampled

   type Query_Control_Flags_T is new Flags_T;

   type Query_Control_Flag_Bits_T is new Query_Control_Flags_T;
   QUERY_CONTROL_PRECISE_BIT : Query_Control_Flag_Bits_T := 1; -- Require precise results to be collected by the query

   type Query_Result_Flags_T is new Flags_T;

   type Query_Result_Flag_Bits_T is new Query_Result_Flags_T;
   QUERY_RESULT_64_BIT : Query_Result_Flag_Bits_T :=
     1; -- Results of the queries are written to the destination buffer as 64-bit values
   QUERY_RESULT_WAIT_BIT : Query_Result_Flag_Bits_T :=
     2; -- Results of the queries are waited on before proceeding with the result copy
   QUERY_RESULT_WITH_AVAILABILITY_BIT : Query_Result_Flag_Bits_T :=
     4; -- Besides the results of the query, the availability of the results is also written
   QUERY_RESULT_PARTIAL_BIT : Query_Result_Flag_Bits_T :=
     8; -- Copy the partial results of the query even if the final results aren't available

   type Shader_Module_Create_Flags_T is new Flags_T;

   type Event_Create_Flags_T is new Flags_T;

   type Command_Pool_Create_Flags_T is new Flags_T;

   type Command_Pool_Create_Flag_Bits_T is new Command_Pool_Create_Flags_T;
   COMMAND_POOL_CREATE_TRANSIENT_BIT : Command_Pool_Create_Flag_Bits_T := 1; -- Command buffers have a short lifetime
   COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT : Command_Pool_Create_Flag_Bits_T :=
     2; -- Command buffers may release their memory individually

   type Command_Pool_Reset_Flags_T is new Flags_T;

   type Command_Pool_Reset_Flag_Bits_T is new Command_Pool_Reset_Flags_T;
   COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT : Command_Pool_Reset_Flag_Bits_T :=
     1; -- Release resources owned by the pool

   type Command_Buffer_Reset_Flags_T is new Flags_T;

   type Command_Buffer_Reset_Flag_Bits_T is new Command_Buffer_Reset_Flags_T;
   COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT : Command_Buffer_Reset_Flag_Bits_T :=
     1; -- Release resources owned by the buffer

   type Command_Buffer_Usage_Flags_T is new Flags_T;

   type Command_Buffer_Usage_Flag_Bits_T is new Command_Buffer_Usage_Flags_T;
   COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT      : Command_Buffer_Usage_Flag_Bits_T := 1;
   COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT : Command_Buffer_Usage_Flag_Bits_T := 2;
   COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT     : Command_Buffer_Usage_Flag_Bits_T :=
     4; -- Command buffer may be submitted/executed more than once simultaneously

   type Query_Pipeline_Statistic_Flags_T is new Flags_T;

   type Query_Pipeline_Statistic_Flag_Bits_T is new Query_Pipeline_Statistic_Flags_T;
   QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 1; -- Optional
   QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 2; -- Optional
   QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 4; -- Optional
   QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 8; -- Optional
   QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 16; -- Optional
   QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 32; -- Optional
   QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 64; -- Optional
   QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 128; -- Optional
   QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT : Query_Pipeline_Statistic_Flag_Bits_T :=
     256; -- Optional
   QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT : Query_Pipeline_Statistic_Flag_Bits_T :=
     512; -- Optional
   QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT : Query_Pipeline_Statistic_Flag_Bits_T := 1024; -- Optional

   type Memory_Map_Flags_T is new Flags_T;

   type Image_Aspect_Flags_T is new Flags_T;

   type Image_Aspect_Flag_Bits_T is new Image_Aspect_Flags_T;
   IMAGE_ASPECT_COLOR_BIT    : Image_Aspect_Flag_Bits_T := 1;
   IMAGE_ASPECT_DEPTH_BIT    : Image_Aspect_Flag_Bits_T := 2;
   IMAGE_ASPECT_STENCIL_BIT  : Image_Aspect_Flag_Bits_T := 4;
   IMAGE_ASPECT_METADATA_BIT : Image_Aspect_Flag_Bits_T := 8;

   type Sparse_Memory_Bind_Flags_T is new Flags_T;

   type Sparse_Memory_Bind_Flag_Bits_T is new Sparse_Memory_Bind_Flags_T;
   SPARSE_MEMORY_BIND_METADATA_BIT : Sparse_Memory_Bind_Flag_Bits_T := 1; -- Operation binds resource metadata to memory

   type Sparse_Image_Format_Flags_T is new Flags_T;

   type Sparse_Image_Format_Flag_Bits_T is new Sparse_Image_Format_Flags_T;
   SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT : Sparse_Image_Format_Flag_Bits_T :=
     1; -- Image uses a single miptail region for all array layers
   SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT : Sparse_Image_Format_Flag_Bits_T :=
     2; -- Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-miptail levels.
   SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT : Sparse_Image_Format_Flag_Bits_T :=
     4; -- Image uses a non-standard sparse image block dimensions

   type Subpass_Description_Flags_T is new Flags_T;

   type Pipeline_Stage_Flags_T is new Flags_T;

   type Pipeline_Stage_Flag_Bits_T is new Pipeline_Stage_Flags_T;
   PIPELINE_STAGE_TOP_OF_PIPE_BIT : Pipeline_Stage_Flag_Bits_T := 1; -- Before subsequent commands are processed
   PIPELINE_STAGE_DRAW_INDIRECT_BIT : Pipeline_Stage_Flag_Bits_T := 2; -- Draw/DispatchIndirect command fetch
   PIPELINE_STAGE_VERTEX_INPUT_BIT                   : Pipeline_Stage_Flag_Bits_T := 4; -- Vertex/index fetch
   PIPELINE_STAGE_VERTEX_SHADER_BIT                  : Pipeline_Stage_Flag_Bits_T := 8; -- Vertex shading
   PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT    : Pipeline_Stage_Flag_Bits_T := 16; -- Tessellation control shading
   PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT : Pipeline_Stage_Flag_Bits_T :=
     32; -- Tessellation evaluation shading
   PIPELINE_STAGE_GEOMETRY_SHADER_BIT      : Pipeline_Stage_Flag_Bits_T := 64; -- Geometry shading
   PIPELINE_STAGE_FRAGMENT_SHADER_BIT      : Pipeline_Stage_Flag_Bits_T := 128; -- Fragment shading
   PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT : Pipeline_Stage_Flag_Bits_T :=
     256; -- Early fragment (depth and stencil) tests
   PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT : Pipeline_Stage_Flag_Bits_T :=
     512; -- Late fragment (depth and stencil) tests
   PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT : Pipeline_Stage_Flag_Bits_T := 1024; -- Color attachment writes
   PIPELINE_STAGE_COMPUTE_SHADER_BIT          : Pipeline_Stage_Flag_Bits_T := 2048; -- Compute shading
   PIPELINE_STAGE_TRANSFER_BIT                : Pipeline_Stage_Flag_Bits_T := 4096; -- Transfer/copy operations
   PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT : Pipeline_Stage_Flag_Bits_T := 8192; -- After previous commands have completed
   PIPELINE_STAGE_HOST_BIT                    : Pipeline_Stage_Flag_Bits_T :=
     16384; -- Indicates host (CPU) is a source/sink of the dependency
   PIPELINE_STAGE_ALL_GRAPHICS_BIT : Pipeline_Stage_Flag_Bits_T := 32768; -- All stages of the graphics pipeline
   PIPELINE_STAGE_ALL_COMMANDS_BIT : Pipeline_Stage_Flag_Bits_T := 65536; -- All stages supported on the queue

   type Sample_Count_Flags_T is new Flags_T;

   type Sample_Count_Flag_Bits_T is new Sample_Count_Flags_T;
   SAMPLE_COUNT_1_BIT  : Sample_Count_Flag_Bits_T := 1; -- Sample count 1 supported
   SAMPLE_COUNT_2_BIT  : Sample_Count_Flag_Bits_T := 2; -- Sample count 2 supported
   SAMPLE_COUNT_4_BIT  : Sample_Count_Flag_Bits_T := 4; -- Sample count 4 supported
   SAMPLE_COUNT_8_BIT  : Sample_Count_Flag_Bits_T := 8; -- Sample count 8 supported
   SAMPLE_COUNT_16_BIT : Sample_Count_Flag_Bits_T := 16; -- Sample count 16 supported
   SAMPLE_COUNT_32_BIT : Sample_Count_Flag_Bits_T := 32; -- Sample count 32 supported
   SAMPLE_COUNT_64_BIT : Sample_Count_Flag_Bits_T := 64; -- Sample count 64 supported

   type Attachment_Description_Flags_T is new Flags_T;

   type Attachment_Description_Flag_Bits_T is new Attachment_Description_Flags_T;
   ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT : Attachment_Description_Flag_Bits_T :=
     1; -- The attachment may alias physical memory of another attachment in the same render pass

   type Stencil_Face_Flags_T is new Flags_T;

   type Stencil_Face_Flag_Bits_T is new Stencil_Face_Flags_T;
   STENCIL_FACE_FRONT_BIT : Stencil_Face_Flag_Bits_T := 1; -- Front face
   STENCIL_FACE_BACK_BIT  : Stencil_Face_Flag_Bits_T := 2; -- Back face
   STENCIL_FRONT_AND_BACK : Stencil_Face_Flag_Bits_T := 16#00000003#; -- Front and back faces

   type Cull_Mode_Flags_T is new Flags_T;

   type Cull_Mode_Flag_Bits_T is new Cull_Mode_Flags_T;
   CULL_MODE_NONE           : Cull_Mode_Flag_Bits_T := 0;
   CULL_MODE_FRONT_BIT      : Cull_Mode_Flag_Bits_T := 1;
   CULL_MODE_BACK_BIT       : Cull_Mode_Flag_Bits_T := 2;
   CULL_MODE_FRONT_AND_BACK : Cull_Mode_Flag_Bits_T := 16#00000003#;

   type Descriptor_Pool_Create_Flags_T is new Flags_T;

   type Descriptor_Pool_Create_Flag_Bits_T is new Descriptor_Pool_Create_Flags_T;
   DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT : Descriptor_Pool_Create_Flag_Bits_T :=
     1; -- Descriptor sets may be freed individually

   type Descriptor_Pool_Reset_Flags_T is new Flags_T;

   type Dependency_Flags_T is new Flags_T;

   type Dependency_Flag_Bits_T is new Dependency_Flags_T;
   DEPENDENCY_BY_REGION_BIT : Dependency_Flag_Bits_T := 1; -- Dependency is per pixel region

   type Composite_Alpha_Flags_Khr_T is new Flags_T;

   type Composite_Alpha_Flag_Bits_Khr_T is new Composite_Alpha_Flags_Khr_T;
   COMPOSITE_ALPHA_OPAQUE_BIT_KHR          : Composite_Alpha_Flag_Bits_Khr_T := 1;
   COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR  : Composite_Alpha_Flag_Bits_Khr_T := 2;
   COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR : Composite_Alpha_Flag_Bits_Khr_T := 4;
   COMPOSITE_ALPHA_INHERIT_BIT_KHR         : Composite_Alpha_Flag_Bits_Khr_T := 8;

   type Display_Plane_Alpha_Flags_Khr_T is new Flags_T;

   type Display_Plane_Alpha_Flag_Bits_Khr_T is new Display_Plane_Alpha_Flags_Khr_T;
   DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR                  : Display_Plane_Alpha_Flag_Bits_Khr_T := 1;
   DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR                  : Display_Plane_Alpha_Flag_Bits_Khr_T := 2;
   DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR               : Display_Plane_Alpha_Flag_Bits_Khr_T := 4;
   DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR : Display_Plane_Alpha_Flag_Bits_Khr_T := 8;

   type Surface_Transform_Flags_Khr_T is new Flags_T;

   type Surface_Transform_Flag_Bits_Khr_T is new Surface_Transform_Flags_Khr_T;
   SURFACE_TRANSFORM_IDENTITY_BIT_KHR                     : Surface_Transform_Flag_Bits_Khr_T := 1;
   SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                    : Surface_Transform_Flag_Bits_Khr_T := 2;
   SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                   : Surface_Transform_Flag_Bits_Khr_T := 4;
   SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                   : Surface_Transform_Flag_Bits_Khr_T := 8;
   SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR            : Surface_Transform_Flag_Bits_Khr_T := 16;
   SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR  : Surface_Transform_Flag_Bits_Khr_T := 32;
   SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR : Surface_Transform_Flag_Bits_Khr_T := 64;
   SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR : Surface_Transform_Flag_Bits_Khr_T := 128;
   SURFACE_TRANSFORM_INHERIT_BIT_KHR                      : Surface_Transform_Flag_Bits_Khr_T := 256;

   type Swapchain_Create_Flags_Khr_T is new Flags_T;

   type Display_Mode_Create_Flags_Khr_T is new Flags_T;

   type Display_Surface_Create_Flags_Khr_T is new Flags_T;

   type Android_Surface_Create_Flags_Khr_T is new Flags_T;

   type Mir_Surface_Create_Flags_Khr_T is new Flags_T;

   type Wayland_Surface_Create_Flags_Khr_T is new Flags_T;

   type Win_32_Surface_Create_Flags_Khr_T is new Flags_T;

   type Xlib_Surface_Create_Flags_Khr_T is new Flags_T;

   type Xcb_Surface_Create_Flags_Khr_T is new Flags_T;

   type Debug_Report_Flags_Ext_T is new Flags_T;

   type Debug_Report_Flag_Bits_Ext_T is new Debug_Report_Flags_Ext_T;
   DEBUG_REPORT_INFORMATION_BIT_EXT         : Debug_Report_Flag_Bits_Ext_T := 1;
   DEBUG_REPORT_WARNING_BIT_EXT             : Debug_Report_Flag_Bits_Ext_T := 2;
   DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT : Debug_Report_Flag_Bits_Ext_T := 4;
   DEBUG_REPORT_ERROR_BIT_EXT               : Debug_Report_Flag_Bits_Ext_T := 8;
   DEBUG_REPORT_DEBUG_BIT_EXT               : Debug_Report_Flag_Bits_Ext_T := 16;

   type Instance_T is private;

   type Physical_Device_T is private;

   type Device_T is private;

   type Queue_T is private;

   type Command_Buffer_T is private;

   type Device_Memory_T is private;

   type Command_Pool_T is private;

   type Buffer_T is private;

   type Buffer_View_T is private;

   type Image_T is private;

   type Image_View_T is private;

   type Shader_Module_T is private;

   type Pipeline_T is private;

   type Pipeline_Layout_T is private;

   type Sampler_T is private;

   type Descriptor_Set_T is private;

   type Descriptor_Set_Layout_T is private;

   type Descriptor_Pool_T is private;

   type Fence_T is private;

   type Semaphore_T is private;

   type Event_T is private;

   type Query_Pool_T is private;

   type Framebuffer_T is private;

   type Render_Pass_T is private;

   type Pipeline_Cache_T is private;

   type Display_Khr_T is private;

   type Display_Mode_Khr_T is private;

   type Surface_Khr_T is private;

   type Swapchain_Khr_T is private;

   type Debug_Report_Callback_Ext_T is private;

   type Pfn_Vk_Internal_Allocation_Notification_T is access procedure
     (Puser_Data       : Void_Ptr;
      Size             : Interfaces.C.size_t;
      Allocation_Type  : Internal_Allocation_Type_T;
      Allocation_Scope : System_Allocation_Scope_T);
   pragma Convention (Stdcall, Pfn_Vk_Internal_Allocation_Notification_T);

   type Pfn_Vk_Internal_Free_Notification_T is access procedure
     (Puser_Data       : Void_Ptr;
      Size             : Interfaces.C.size_t;
      Allocation_Type  : Internal_Allocation_Type_T;
      Allocation_Scope : System_Allocation_Scope_T);
   pragma Convention (Stdcall, Pfn_Vk_Internal_Free_Notification_T);

   type Pfn_Vk_Reallocation_Function_T is access function
     (Puser_Data       : Void_Ptr;
      Poriginal        : Void_Ptr;
      Size             : Interfaces.C.size_t;
      Alignment        : Interfaces.C.size_t;
      Allocation_Scope : System_Allocation_Scope_T) return Void_Ptr;
   pragma Convention (Stdcall, Pfn_Vk_Reallocation_Function_T);

   type Pfn_Vk_Allocation_Function_T is access function
     (Puser_Data       : Void_Ptr;
      Size             : Interfaces.C.size_t;
      Alignment        : Interfaces.C.size_t;
      Allocation_Scope : System_Allocation_Scope_T) return Void_Ptr;
   pragma Convention (Stdcall, Pfn_Vk_Allocation_Function_T);

   type Pfn_Vk_Free_Function_T is access procedure (Puser_Data : Void_Ptr; Pmemory : Void_Ptr);
   pragma Convention (Stdcall, Pfn_Vk_Free_Function_T);

   type Pfn_Vk_Void_Function_T is access procedure;
   pragma Convention (Stdcall, Pfn_Vk_Void_Function_T);

   type Pfn_Vk_Debug_Report_Callback_Ext_T is access function
     (Flags         : Debug_Report_Flags_Ext_T;
      Object_Type   : Debug_Report_Object_Type_Ext_T;
      Object        : Interfaces.Unsigned_64;
      Location      : Interfaces.C.size_t;
      Message_Code  : Interfaces.Integer_32;
      Player_Prefix : Interfaces.C.Strings.chars_ptr;
      Pmessage      : Interfaces.C.Strings.chars_ptr;
      Puser_Data    : Void_Ptr) return Bool_32_T;
   pragma Convention (Stdcall, Pfn_Vk_Debug_Report_Callback_Ext_T);

   type Offset_2D_T is record
      X : Interfaces.Integer_32;
      Y : Interfaces.Integer_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Offset_2D_T);

   type Offset_3D_T is record
      X : Interfaces.Integer_32;
      Y : Interfaces.Integer_32;
      Z : Interfaces.Integer_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Offset_3D_T);

   type Extent_2D_T is record
      Width  : Interfaces.Unsigned_32;
      Height : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Extent_2D_T);

   type Extent_3D_T is record
      Width  : Interfaces.Unsigned_32;
      Height : Interfaces.Unsigned_32;
      Depth  : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Extent_3D_T);

   type Viewport_T is record
      X         : Interfaces.C.C_float;
      Y         : Interfaces.C.C_float;
      Width     : Interfaces.C.C_float;
      Height    : Interfaces.C.C_float;
      Min_Depth : Interfaces.C.C_float;
      Max_Depth : Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, Viewport_T);
   -- pname:width must: be greater than `0.0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxViewportDimensions[0]
   -- pname:height must: be greater than `0.0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxViewportDimensions[1]
   -- pname:x and pname:y must: each be between pname:viewportBoundsRange[0] and pname:viewportBoundsRange[1], inclusive
   -- pname:x + pname:width must: be less than or equal to pname:viewportBoundsRange[1]
   -- pname:y + pname:height must: be less than or equal to pname:viewportBoundsRange[1]
   -- pname:minDepth must: be between `0.0` and `1.0`, inclusive
   -- pname:maxDepth must: be between `0.0` and `1.0`, inclusive

   type Rect_2D_T is record
      Offset : Offset_2D_T;
      Extent : Extent_2D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Rect_2D_T);

   type Rect_3D_T is record
      Offset : Offset_3D_T;
      Extent : Extent_3D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Rect_3D_T);

   type Clear_Rect_T is record
      Rect             : Rect_2D_T;
      Base_Array_Layer : Interfaces.Unsigned_32;
      Layer_Count      : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Clear_Rect_T);

   type Component_Mapping_T is record
      R : Component_Swizzle_T;
      G : Component_Swizzle_T;
      B : Component_Swizzle_T;
      A : Component_Swizzle_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Component_Mapping_T);

   type Extension_Name_Array_Index_T is range 0 .. MAX_EXTENSION_NAME_SIZE;

   type Extension_Name_Array_T is array (Extension_Name_Array_Index_T) of Interfaces.C.char;
   pragma Convention (C, Extension_Name_Array_T);

   type Extension_Properties_T is record
      Extension_Name : Extension_Name_Array_T;
      Spec_Version   : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Extension_Properties_T);

   type Layer_Name_Array_Index_T is range 0 .. MAX_EXTENSION_NAME_SIZE;

   type Layer_Name_Array_T is array (Layer_Name_Array_Index_T) of Interfaces.C.char;
   pragma Convention (C, Layer_Name_Array_T);

   type Description_Array_Index_T is range 0 .. MAX_DESCRIPTION_SIZE;

   type Description_Array_T is array (Description_Array_Index_T) of Interfaces.C.char;
   pragma Convention (C, Description_Array_T);

   type Layer_Properties_T is record
      Layer_Name             : Layer_Name_Array_T;
      Spec_Version           : Interfaces.Unsigned_32;
      Implementation_Version : Interfaces.Unsigned_32;
      Description            : Description_Array_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Layer_Properties_T);

   type Application_Info_T is record
      Stype               : Structure_Type_T;
      Pnext               : Void_Ptr;
      Papplication_Name   : Interfaces.C.Strings.chars_ptr;
      Application_Version : Interfaces.Unsigned_32;
      Pengine_Name        : Interfaces.C.Strings.chars_ptr;
      Engine_Version      : Interfaces.Unsigned_32;
      Api_Version         : Version_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Application_Info_T);
   -- pname:apiVersion must: be zero, or otherwise it must: be a version that the implementation supports, or supports an effective substitute for

   type Allocation_Callbacks_T is record
      Puser_Data              : Void_Ptr;
      Pfn_Allocation          : Pfn_Vk_Allocation_Function_T;
      Pfn_Reallocation        : Pfn_Vk_Reallocation_Function_T;
      Pfn_Free                : Pfn_Vk_Free_Function_T;
      Pfn_Internal_Allocation : Pfn_Vk_Internal_Allocation_Notification_T;
      Pfn_Internal_Free       : Pfn_Vk_Internal_Free_Notification_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Allocation_Callbacks_T);
   -- pname:pfnAllocation must: be a pointer to a valid user-defined PFN_vkAllocationFunction
   -- pname:pfnReallocation must: be a pointer to a valid user-defined PFN_vkReallocationFunction
   -- pname:pfnFree must: be a pointer to a valid user-defined PFN_vkFreeFunction
   -- If either of pname:pfnInternalAllocation or pname:pfnInternalFree is not `NULL`, both must: be valid callbacks

   type Float_Const_Ptr is access constant Interfaces.C.C_float;

   type Device_Queue_Create_Info_T is record
      Stype              : Structure_Type_T;
      Pnext              : Void_Ptr;
      Flags              : Device_Queue_Create_Flags_T;
      Queue_Family_Index : Interfaces.Unsigned_32;
      Queue_Count        : Interfaces.Unsigned_32;
      Pqueue_Priorities  : Float_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Device_Queue_Create_Info_T);
   -- pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties
   -- pname:queueCount must: be less than or equal to the pname:queueCount member of the sname:VkQueueFamilyProperties structure, as returned by fname:vkGetPhysicalDeviceQueueFamilyProperties in the pname:pQueueFamilyProperties[pname:queueFamilyIndex]
   -- Each element of pname:pQueuePriorities must: be between `0.0` and `1.0` inclusive

   type Application_Info_Const_Ptr is access constant Application_Info_T;

   type Instance_Create_Info_T is record
      Stype                      : Structure_Type_T;
      Pnext                      : Void_Ptr;
      Flags                      : Instance_Create_Flags_T;
      Papplication_Info          : Application_Info_Const_Ptr;
      Enabled_Layer_Count        : Interfaces.Unsigned_32;
      Pp_Enabled_Layer_Names     : Char_Ptr_Array_Conversions.Object_Address;
      Enabled_Extension_Count    : Interfaces.Unsigned_32;
      Pp_Enabled_Extension_Names : Char_Ptr_Array_Conversions.Object_Address;
   end record;
   pragma Convention (C_Pass_By_Copy, Instance_Create_Info_T);
   -- Any given element of pname:ppEnabledLayerNames must: be the name of a layer present on the system, exactly matching a string returned in the sname:VkLayerProperties structure by fname:vkEnumerateInstanceLayerProperties
   -- Any given element of pname:ppEnabledExtensionNames must: be the name of an extension present on the system, exactly matching a string returned in the sname:VkExtensionProperties structure by fname:vkEnumerateInstanceExtensionProperties
   -- If an extension listed in pname:ppEnabledExtensionNames is provided as part of a layer, then both the layer and extension must: be enabled to enable that extension

   type Queue_Family_Properties_T is record
      Queue_Flags                    : Queue_Flags_T;
      Queue_Count                    : Interfaces.Unsigned_32;
      Timestamp_Valid_Bits           : Interfaces.Unsigned_32;
      Min_Image_Transfer_Granularity : Extent_3D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Queue_Family_Properties_T);

   type Memory_Allocate_Info_T is record
      Stype             : Structure_Type_T;
      Pnext             : Void_Ptr;
      Allocation_Size   : Device_Size_T;
      Memory_Type_Index : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Memory_Allocate_Info_T);
   -- pname:allocationSize must: be less than or equal to the amount of memory available to the sname:VkMemoryHeap specified by pname:memoryTypeIndex and the calling command's sname:VkDevice
   -- pname:allocationSize must: be greater than `0`

   type Memory_Requirements_T is record
      Size             : Device_Size_T;
      Alignment        : Device_Size_T;
      Memory_Type_Bits : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Memory_Requirements_T);

   type Sparse_Image_Format_Properties_T is record
      Aspect_Mask       : Image_Aspect_Flags_T;
      Image_Granularity : Extent_3D_T;
      Flags             : Sparse_Image_Format_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Sparse_Image_Format_Properties_T);

   type Sparse_Image_Memory_Requirements_T is record
      Format_Properties        : Sparse_Image_Format_Properties_T;
      Image_Mip_Tail_First_Lod : Interfaces.Unsigned_32;
      Image_Mip_Tail_Size      : Device_Size_T;
      Image_Mip_Tail_Offset    : Device_Size_T;
      Image_Mip_Tail_Stride    : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Sparse_Image_Memory_Requirements_T);

   type Memory_Type_T is record
      Property_Flags : Memory_Property_Flags_T;
      Heap_Index     : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Memory_Type_T);

   type Memory_Heap_T is record
      Size  : Device_Size_T;
      Flags : Memory_Heap_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Memory_Heap_T);

   type Mapped_Memory_Range_T is record
      Stype  : Structure_Type_T;
      Pnext  : Void_Ptr;
      Memory : Device_Memory_T;
      Offset : Device_Size_T;
      Size   : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Mapped_Memory_Range_T);
   -- pname:memory must: currently be mapped
   -- If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:offset and pname:size must: specify a range contained within the currently mapped range of pname:memory
   -- If pname:size is equal to ename:VK_WHOLE_SIZE, pname:offset must: be within the currently mapped range of pname:memory
   -- pname:offset must: be a multiple of sname:VkPhysicalDeviceLimits::pname:nonCoherentAtomSize
   -- If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be a multiple of sname:VkPhysicalDeviceLimits::pname:nonCoherentAtomSize

   type Format_Properties_T is record
      Linear_Tiling_Features  : Format_Feature_Flags_T;
      Optimal_Tiling_Features : Format_Feature_Flags_T;
      Buffer_Features         : Format_Feature_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Format_Properties_T);

   type Image_Format_Properties_T is record
      Max_Extent        : Extent_3D_T;
      Max_Mip_Levels    : Interfaces.Unsigned_32;
      Max_Array_Layers  : Interfaces.Unsigned_32;
      Sample_Counts     : Sample_Count_Flags_T;
      Max_Resource_Size : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Format_Properties_T);

   type Descriptor_Buffer_Info_T is record
      Buffer    : Buffer_T;
      Offset    : Device_Size_T;
      The_Range : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Descriptor_Buffer_Info_T);
   -- pname:offset must: be less than the size of pname:buffer
   -- If pname:range is not equal to ename:VK_WHOLE_SIZE, pname:range must: be greater than `0`
   -- If pname:range is not equal to ename:VK_WHOLE_SIZE, pname:range must: be less than or equal to the size of pname:buffer minus pname:offset

   type Descriptor_Image_Info_T is record
      Sampler      : Sampler_T;
      Image_View   : Image_View_T;
      Image_Layout : Image_Layout_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Descriptor_Image_Info_T);

   type Descriptor_Image_Info_Const_Ptr is access constant Descriptor_Image_Info_T;

   type Descriptor_Buffer_Info_Const_Ptr is access constant Descriptor_Buffer_Info_T;

   type Buffer_View_Const_Ptr is access constant Buffer_View_T;

   type Write_Descriptor_Set_T is record
      Stype              : Structure_Type_T;
      Pnext              : Void_Ptr;
      Dst_Set            : Descriptor_Set_T;
      Dst_Binding        : Interfaces.Unsigned_32;
      Dst_Array_Element  : Interfaces.Unsigned_32;
      Descriptor_Count   : Interfaces.Unsigned_32;
      Descriptor_Type    : Descriptor_Type_T;
      Pimage_Info        : Descriptor_Image_Info_Const_Ptr;
      Pbuffer_Info       : Descriptor_Buffer_Info_Const_Ptr;
      Ptexel_Buffer_View : Buffer_View_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Write_Descriptor_Set_T);
   -- pname:dstBinding must: be a valid binding point within pname:dstSet
   -- pname:descriptorType must: match the type of pname:dstBinding within pname:dstSet
   -- The sum of pname:dstArrayElement and pname:descriptorCount must: be less than or equal to the number of array elements in the descriptor set binding specified by pname:dstBinding, and all applicable consecutive bindings, as described by &lt;&lt;descriptorsets-updates-consecutive&gt;&gt;
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE or ename:VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT, pname:pImageInfo must: be a pointer to an array of pname:descriptorCount valid sname:VkDescriptorImageInfo structures
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER, pname:pTexelBufferView must: be a pointer to an array of pname:descriptorCount valid sname:VkBufferView handles
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, pname:pBufferInfo must: be a pointer to an array of pname:descriptorCount valid sname:VkDescriptorBufferInfo structures
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_SAMPLER or ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, and pname:dstSet was not created with a layout that included immutable samplers for pname:dstBinding with pname:descriptorType, the pname:sampler member of any given element of pname:pImageInfo must: be a valid sname:VkSampler object
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE or ename:VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT, the pname:imageView and pname:imageLayout members of any given element of pname:pImageInfo must: be a valid sname:VkImageView and elink:VkImageLayout, respectively
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER or ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, the pname:offset member of any given element of pname:pBufferInfo must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minUniformBufferOffsetAlignment
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, the pname:offset member of any given element of pname:pBufferInfo must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minStorageBufferOffsetAlignment
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER or ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, the pname:buffer member of any given element of pname:pBufferInfo must: have been created with ename:VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT set
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, the pname:buffer member of any given element of pname:pBufferInfo must: have been created with ename:VK_BUFFER_USAGE_STORAGE_BUFFER_BIT set
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER or ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, the pname:range member of any given element of pname:pBufferInfo must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxUniformBufferRange
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, the pname:range member of any given element of pname:pBufferInfo must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxStorageBufferRange
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER, the sname:VkBuffer that any given element of pname:pTexelBufferView was created from must: have been created with ename:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT set
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER, the sname:VkBuffer that any given element of pname:pTexelBufferView was created from must: have been created with ename:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT set
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE or ename:VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT, the pname:imageView member of any given element of pname:pImageInfo must: have been created with the identity swizzle

   type Copy_Descriptor_Set_T is record
      Stype             : Structure_Type_T;
      Pnext             : Void_Ptr;
      Src_Set           : Descriptor_Set_T;
      Src_Binding       : Interfaces.Unsigned_32;
      Src_Array_Element : Interfaces.Unsigned_32;
      Dst_Set           : Descriptor_Set_T;
      Dst_Binding       : Interfaces.Unsigned_32;
      Dst_Array_Element : Interfaces.Unsigned_32;
      Descriptor_Count  : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Copy_Descriptor_Set_T);
   -- pname:srcBinding must: be a valid binding within pname:srcSet
   -- The sum of pname:srcArrayElement and pname:descriptorCount must: be less than or equal to the number of array elements in the descriptor set binding specified by pname:srcBinding, and all applicable consecutive bindings, as described by &lt;&lt;descriptorsets-updates-consecutive&gt;&gt;
   -- pname:dstBinding must: be a valid binding within pname:dstSet
   -- The sum of pname:dstArrayElement and pname:descriptorCount must: be less than or equal to the number of array elements in the descriptor set binding specified by pname:dstBinding, and all applicable consecutive bindings, as described by &lt;&lt;descriptorsets-updates-consecutive&gt;&gt;
   -- If pname:srcSet is equal to pname:dstSet, then the source and destination ranges of descriptors mustnot: overlap, where the ranges may: include array elements from consecutive bindings as described by &lt;&lt;descriptorsets-updates-consecutive&gt;&gt;

   type Uint_32_Const_Ptr is access constant Interfaces.Unsigned_32;

   type Buffer_Create_Info_T is record
      Stype                    : Structure_Type_T;
      Pnext                    : Void_Ptr;
      Flags                    : Buffer_Create_Flags_T;
      Size                     : Device_Size_T;
      Usage                    : Buffer_Usage_Flags_T;
      Sharing_Mode             : Sharing_Mode_T;
      Queue_Family_Index_Count : Interfaces.Unsigned_32;
      Pqueue_Family_Indices    : Uint_32_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Buffer_Create_Info_T);
   -- pname:size must: be greater than `0`
   -- If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:pQueueFamilyIndices must: be a pointer to an array of pname:queueFamilyIndexCount basetype:uint32_t values
   -- If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:queueFamilyIndexCount must: be greater than `1`
   -- If the &lt;&lt;features-features-sparseBinding,sparse bindings&gt;&gt; feature is not enabled, pname:flags mustnot: contain ename:VK_BUFFER_CREATE_SPARSE_BINDING_BIT
   -- If the &lt;&lt;features-features-sparseResidencyBuffer,sparse buffer residency&gt;&gt; feature is not enabled, pname:flags mustnot: contain ename:VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
   -- If the &lt;&lt;features-features-sparseResidencyAliased,sparse aliased residency&gt;&gt; feature is not enabled, pname:flags mustnot: contain ename:VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
   -- If pname:flags contains ename:VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT or ename:VK_BUFFER_CREATE_SPARSE_ALIASED_BIT, it must: also contain ename:VK_BUFFER_CREATE_SPARSE_BINDING_BIT

   type Buffer_View_Create_Info_T is record
      Stype     : Structure_Type_T;
      Pnext     : Void_Ptr;
      Flags     : Buffer_View_Create_Flags_T;
      Buffer    : Buffer_T;
      Format    : Format_T;
      Offset    : Device_Size_T;
      The_Range : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Buffer_View_Create_Info_T);
   -- pname:offset must: be less than the size of pname:buffer
   -- pname:offset must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minTexelBufferOffsetAlignment
   -- If pname:range is not equal to ename:VK_WHOLE_SIZE:
   -- pname:buffer must: have been created with a pname:usage value containing at least one of ename:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT or ename:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
   -- If pname:buffer was created with pname:usage containing ename:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT, pname:format must: be supported for uniform texel buffers, as specified by the ename:VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT flag in sname:VkFormatProperties::pname:bufferFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:buffer was created with pname:usage containing ename:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT, pname:format must: be supported for storage texel buffers, as specified by the ename:VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT flag in sname:VkFormatProperties::pname:bufferFeatures returned by fname:vkGetPhysicalDeviceFormatProperties

   type Image_Subresource_T is record
      Aspect_Mask : Image_Aspect_Flags_T;
      Mip_Level   : Interfaces.Unsigned_32;
      Array_Layer : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Subresource_T);
   -- pname:mipLevel must: be less than the pname:mipLevels specified in slink:VkImageCreateInfo when the image was created
   -- pname:arrayLayer must: be less than the pname:arrayLayers specified in slink:VkImageCreateInfo when the image was created

   type Image_Subresource_Layers_T is record
      Aspect_Mask      : Image_Aspect_Flags_T;
      Mip_Level        : Interfaces.Unsigned_32;
      Base_Array_Layer : Interfaces.Unsigned_32;
      Layer_Count      : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Subresource_Layers_T);
   -- If pname:aspectMask contains ename:VK_IMAGE_ASPECT_COLOR_BIT, it mustnot: contain either of ename:VK_IMAGE_ASPECT_DEPTH_BIT or ename:VK_IMAGE_ASPECT_STENCIL_BIT
   -- pname:aspectMask mustnot: contain ename:VK_IMAGE_ASPECT_METADATA_BIT
   -- pname:mipLevel must: be less than the pname:mipLevels specified in slink:VkImageCreateInfo when the image was created
   -- latexmath:[$(baseArrayLayer + layerCount)$] must: be less than or equal to the pname:arrayLayers specified in slink:VkImageCreateInfo when the image was created

   type Image_Subresource_Range_T is record
      Aspect_Mask      : Image_Aspect_Flags_T;
      Base_Mip_Level   : Interfaces.Unsigned_32;
      Level_Count      : Interfaces.Unsigned_32;
      Base_Array_Layer : Interfaces.Unsigned_32;
      Layer_Count      : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Subresource_Range_T);
   -- If pname:levelCount is not ename:VK_REMAINING_MIP_LEVELS, latexmath:[$(baseMipLevel + levelCount)$] must: be less than or equal to the pname:mipLevels specified in slink:VkImageCreateInfo when the image was created
   -- If pname:layerCount is not ename:VK_REMAINING_ARRAY_LAYERS, latexmath:[$(baseArrayLayer + layerCount)$] must: be less than or equal to the pname:arrayLayers specified in slink:VkImageCreateInfo when the image was created

   type Memory_Barrier_T is record
      Stype           : Structure_Type_T;
      Pnext           : Void_Ptr;
      Src_Access_Mask : Access_Flags_T;
      Dst_Access_Mask : Access_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Memory_Barrier_T);

   type Buffer_Memory_Barrier_T is record
      Stype                  : Structure_Type_T;
      Pnext                  : Void_Ptr;
      Src_Access_Mask        : Access_Flags_T;
      Dst_Access_Mask        : Access_Flags_T;
      Src_Queue_Family_Index : Interfaces.Unsigned_32;
      Dst_Queue_Family_Index : Interfaces.Unsigned_32;
      Buffer                 : Buffer_T;
      Offset                 : Device_Size_T;
      Size                   : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Buffer_Memory_Barrier_T);
   -- pname:offset must: be less than the size of pname:buffer
   -- If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be greater than `0`
   -- If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be less than or equal to than the size of pname:buffer minus pname:offset
   -- If pname:buffer was created with a sharing mode of ename:VK_SHARING_MODE_CONCURRENT, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: both be ename:VK_QUEUE_FAMILY_IGNORED
   -- If pname:buffer was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: either both be ename:VK_QUEUE_FAMILY_IGNORED, or both be a valid queue family (see &lt;&lt;devsandqueues-queueprops&gt;&gt;)
   -- If pname:buffer was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, and pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex are valid queue families, at least one of them must: be the same as the family of the queue that will execute this barrier

   type Image_Memory_Barrier_T is record
      Stype                  : Structure_Type_T;
      Pnext                  : Void_Ptr;
      Src_Access_Mask        : Access_Flags_T;
      Dst_Access_Mask        : Access_Flags_T;
      Old_Layout             : Image_Layout_T;
      New_Layout             : Image_Layout_T;
      Src_Queue_Family_Index : Interfaces.Unsigned_32;
      Dst_Queue_Family_Index : Interfaces.Unsigned_32;
      Image                  : Image_T;
      Subresource_Range      : Image_Subresource_Range_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Memory_Barrier_T);
   -- pname:oldLayout must: be ename:VK_IMAGE_LAYOUT_UNDEFINED, ename:VK_IMAGE_LAYOUT_PREINITIALIZED or the current layout of the image region affected by the barrier
   -- pname:newLayout mustnot: be ename:VK_IMAGE_LAYOUT_UNDEFINED or ename:VK_IMAGE_LAYOUT_PREINITIALIZED
   -- If pname:image was created with a sharing mode of ename:VK_SHARING_MODE_CONCURRENT, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: both be ename:VK_QUEUE_FAMILY_IGNORED
   -- If pname:image was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: either both be ename:VK_QUEUE_FAMILY_IGNORED, or both be a valid queue family (see &lt;&lt;devsandqueues-queueprops&gt;&gt;)
   -- If pname:image was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, and pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex are valid queue families, at least one of them must: be the same as the family of the queue that will execute this barrier
   -- pname:subresourceRange must: be a valid image subresource range for the image (see &lt;&lt;resources-image-views&gt;&gt;)
   -- If pname:image has a depth/stencil format with both depth and stencil components, then pname:aspectMask member of pname:subresourceRange must: include both ename:VK_IMAGE_ASPECT_DEPTH_BIT and ename:VK_IMAGE_ASPECT_STENCIL_BIT
   -- If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT set
   -- If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT set
   -- If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT set
   -- If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_SAMPLED_BIT or ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT set
   -- If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_SRC_BIT set
   -- If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT set

   type Image_Create_Info_T is record
      Stype                    : Structure_Type_T;
      Pnext                    : Void_Ptr;
      Flags                    : Image_Create_Flags_T;
      Image_Type               : Image_Type_T;
      Format                   : Format_T;
      Extent                   : Extent_3D_T;
      Mip_Levels               : Interfaces.Unsigned_32;
      Array_Layers             : Interfaces.Unsigned_32;
      Samples                  : Sample_Count_Flag_Bits_T;
      Tiling                   : Image_Tiling_T;
      Usage                    : Image_Usage_Flags_T;
      Sharing_Mode             : Sharing_Mode_T;
      Queue_Family_Index_Count : Interfaces.Unsigned_32;
      Pqueue_Family_Indices    : Uint_32_Const_Ptr;
      Initial_Layout           : Image_Layout_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Create_Info_T);
   -- If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:pQueueFamilyIndices must: be a pointer to an array of pname:queueFamilyIndexCount basetype:uint32_t values
   -- If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:queueFamilyIndexCount must: be greater than `1`
   -- pname:format mustnot: be ename:VK_FORMAT_UNDEFINED
   -- The pname:width, pname:height, and pname:depth members of pname:extent must: all be greater than `0`
   -- pname:mipLevels must: be greater than `0`
   -- pname:arrayLayers must: be greater than `0`
   -- If pname:imageType is ename:VK_IMAGE_TYPE_1D, pname:extent.width must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimension1D, or sname:VkImageFormatProperties::pname:maxExtent.width (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
   -- If pname:imageType is ename:VK_IMAGE_TYPE_2D and pname:flags does not contain ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT, pname:extent.width and pname:extent.height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimension2D, or sname:VkImageFormatProperties::pname:maxExtent.width/height (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
   -- If pname:imageType is ename:VK_IMAGE_TYPE_2D and pname:flags contains ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT, pname:extent.width and pname:extent.height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimensionCube, or sname:VkImageFormatProperties::pname:maxExtent.width/height (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
   -- If pname:imageType is ename:VK_IMAGE_TYPE_2D and pname:flags contains ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT, pname:extent.width and pname:extent.height must: be equal
   -- If pname:imageType is ename:VK_IMAGE_TYPE_3D, pname:extent.width, pname:extent.height and pname:extent.depth must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimension3D, or sname:VkImageFormatProperties::pname:maxExtent.width/height/depth (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
   -- If pname:imageType is ename:VK_IMAGE_TYPE_1D, both pname:extent.height and pname:extent.depth must: be `1`
   -- If pname:imageType is ename:VK_IMAGE_TYPE_2D, pname:extent.depth must: be `1`
   -- pname:mipLevels must: be less than or equal to latexmath:[$\lfloor\log_2(\max(\mathit{extent.width}, \mathit{extent.height}, \mathit{extent.depth}))\rfloor + 1$]
   -- If any of pname:extent.width, pname:extent.height or pname:extent.depth are greater than the equivalently named members of sname:VkPhysicalDeviceLimits::pname:maxImageDimension3D, pname:mipLevels must: be less than or equal to sname:VkImageFormatProperties::pname:maxMipLevels (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure)
   -- pname:arrayLayers must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageArrayLayers, or sname:VkImageFormatProperties::pname:maxArrayLayers (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
   -- pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampleCounts returned by flink:vkGetPhysicalDeviceProperties, or sname:VkImageFormatProperties::pname:sampleCounts returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure
   -- If pname:usage includes ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT or ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT, pname:extent.width must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferWidth
   -- If pname:usage includes ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT or ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT, pname:extent.height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferHeight
   -- If pname:usage includes ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:framebufferColorSampleCounts
   -- If pname:usage includes ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, and pname:format includes a depth aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:framebufferDepthSampleCounts
   -- If pname:usage includes ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, and pname:format includes a stencil aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:framebufferStencilSampleCounts
   -- If pname:usage includes ename:VK_IMAGE_USAGE_SAMPLED_BIT, and pname:format includes a color aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampledImageColorSampleCounts
   -- If pname:usage includes ename:VK_IMAGE_USAGE_SAMPLED_BIT, and pname:format includes a depth aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampledImageDepthSampleCounts
   -- If pname:usage includes ename:VK_IMAGE_USAGE_SAMPLED_BIT, and pname:format is an integer format, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampledImageIntegerSampleCounts
   -- If pname:usage includes ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:storageImageSampleCounts
   -- If the &lt;&lt;features-features-textureCompressionETC2,ETC2 texture compression&gt;&gt; feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK, ename:VK_FORMAT_EAC_R11_UNORM_BLOCK, ename:VK_FORMAT_EAC_R11_SNORM_BLOCK, ename:VK_FORMAT_EAC_R11G11_UNORM_BLOCK, or ename:VK_FORMAT_EAC_R11G11_SNORM_BLOCK
   -- If the &lt;&lt;features-features-textureCompressionASTC_LDR,ASTC LDR texture compression&gt;&gt; feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ASTC_4x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_4x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_12x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x12_UNORM_BLOCK, or ename:VK_FORMAT_ASTC_12x12_SRGB_BLOCK
   -- If the &lt;&lt;features-features-textureCompressionBC,BC texture compression&gt;&gt; feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_BC1_RGB_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGB_SRGB_BLOCK, ename:VK_FORMAT_BC1_RGBA_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGBA_SRGB_BLOCK, ename:VK_FORMAT_BC2_UNORM_BLOCK, ename:VK_FORMAT_BC2_SRGB_BLOCK, ename:VK_FORMAT_BC3_UNORM_BLOCK, ename:VK_FORMAT_BC3_SRGB_BLOCK, ename:VK_FORMAT_BC4_UNORM_BLOCK, ename:VK_FORMAT_BC4_SNORM_BLOCK, ename:VK_FORMAT_BC5_UNORM_BLOCK, ename:VK_FORMAT_BC5_SNORM_BLOCK, ename:VK_FORMAT_BC6H_UFLOAT_BLOCK, ename:VK_FORMAT_BC6H_SFLOAT_BLOCK, ename:VK_FORMAT_BC7_UNORM_BLOCK, or ename:VK_FORMAT_BC7_SRGB_BLOCK
   -- If the &lt;&lt;features-features-shaderStorageImageMultisample,multisampled storage images&gt;&gt; feature is not enabled, and pname:usage contains ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:samples must: be ename:VK_SAMPLE_COUNT_1_BIT
   -- If the &lt;&lt;features-features-sparseBinding,sparse bindings&gt;&gt; feature is not enabled, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_BINDING_BIT
   -- If the &lt;&lt;features-features-sparseResidencyImage2D,sparse residency for 2D images&gt;&gt; feature is not enabled, and pname:imageType is ename:VK_IMAGE_TYPE_2D, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   -- If the &lt;&lt;features-features-sparseResidencyImage3D,sparse residency for 3D images&gt;&gt; feature is not enabled, and pname:imageType is ename:VK_IMAGE_TYPE_3D, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   -- If the &lt;&lt;features-features-sparseResidency2Samples,sparse residency for images with 2 samples&gt;&gt; feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_2_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   -- If the &lt;&lt;features-features-sparseResidency4Samples,sparse residency for images with 4 samples&gt;&gt; feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_4_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   -- If the &lt;&lt;features-features-sparseResidency8Samples,sparse residency for images with 8 samples&gt;&gt; feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_8_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   -- If the &lt;&lt;features-features-sparseResidency16Samples,sparse residency for images with 16 samples&gt;&gt; feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_16_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_SAMPLED_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_STORAGE_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_SAMPLED_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_STORAGE_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
   -- If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
   -- If pname:flags contains ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT or ename:VK_IMAGE_CREATE_SPARSE_ALIASED_BIT, it must: also contain ename:VK_IMAGE_CREATE_SPARSE_BINDING_BIT

   type Subresource_Layout_T is record
      Offset      : Device_Size_T;
      Size        : Device_Size_T;
      Row_Pitch   : Device_Size_T;
      Array_Pitch : Device_Size_T;
      Depth_Pitch : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Subresource_Layout_T);

   type Image_View_Create_Info_T is record
      Stype             : Structure_Type_T;
      Pnext             : Void_Ptr;
      Flags             : Image_View_Create_Flags_T;
      Image             : Image_T;
      View_Type         : Image_View_Type_T;
      Format            : Format_T;
      Components        : Component_Mapping_T;
      Subresource_Range : Image_Subresource_Range_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_View_Create_Info_T);
   -- If pname:image was not created with ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT then pname:viewType mustnot: be ename:VK_IMAGE_VIEW_TYPE_CUBE or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
   -- If the &lt;&lt;features-features-imageCubeArray,image cubemap arrays&gt;&gt; feature is not enabled, pname:viewType mustnot: be ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
   -- If the &lt;&lt;features-features-textureCompressionETC2,ETC2 texture compression&gt;&gt; feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK, ename:VK_FORMAT_EAC_R11_UNORM_BLOCK, ename:VK_FORMAT_EAC_R11_SNORM_BLOCK, ename:VK_FORMAT_EAC_R11G11_UNORM_BLOCK, or ename:VK_FORMAT_EAC_R11G11_SNORM_BLOCK
   -- If the &lt;&lt;features-features-textureCompressionASTC_LDR,ASTC LDR texture compression&gt;&gt; feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ASTC_4x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_4x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_12x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x12_UNORM_BLOCK, or ename:VK_FORMAT_ASTC_12x12_SRGB_BLOCK
   -- If the &lt;&lt;features-features-textureCompressionBC,BC texture compression&gt;&gt; feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_BC1_RGB_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGB_SRGB_BLOCK, ename:VK_FORMAT_BC1_RGBA_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGBA_SRGB_BLOCK, ename:VK_FORMAT_BC2_UNORM_BLOCK, ename:VK_FORMAT_BC2_SRGB_BLOCK, ename:VK_FORMAT_BC3_UNORM_BLOCK, ename:VK_FORMAT_BC3_SRGB_BLOCK, ename:VK_FORMAT_BC4_UNORM_BLOCK, ename:VK_FORMAT_BC4_SNORM_BLOCK, ename:VK_FORMAT_BC5_UNORM_BLOCK, ename:VK_FORMAT_BC5_SNORM_BLOCK, ename:VK_FORMAT_BC6H_UFLOAT_BLOCK, ename:VK_FORMAT_BC6H_SFLOAT_BLOCK, ename:VK_FORMAT_BC7_UNORM_BLOCK, or ename:VK_FORMAT_BC7_SRGB_BLOCK
   -- If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_SAMPLED_BIT, pname:format must: be supported for sampled images, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:format must: be supported for storage images, as specified by the ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, pname:format must: be supported for color attachments, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:format must: be supported for depth/stencil attachments, as specified by the ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_SAMPLED_BIT, pname:format must: be supported for sampled images, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:format must: be supported for storage images, as specified by the ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, pname:format must: be supported for color attachments, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:format must: be supported for depth/stencil attachments, as specified by the ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- pname:subresourceRange must: be a valid image subresource range for pname:image (see &lt;&lt;resources-image-views&gt;&gt;)
   -- If pname:image was created with the ename:VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT flag, pname:format must: be compatible with the pname:format used to create pname:image, as defined in &lt;&lt;features-formats-compatibility-classes,Format Compatibility Classes&gt;&gt;
   -- If pname:image was not created with the ename:VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT flag, pname:format must: be identical to the pname:format used to create pname:image
   -- pname:subResourceRange and pname:viewType must: be compatible with the image, as described in the &lt;&lt;resources-image-views-compatibility,table below&gt;&gt;

   type Buffer_Copy_T is record
      Src_Offset : Device_Size_T;
      Dst_Offset : Device_Size_T;
      Size       : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Buffer_Copy_T);

   type Sparse_Memory_Bind_T is record
      Resource_Offset : Device_Size_T;
      Size            : Device_Size_T;
      Memory          : Device_Memory_T;
      Memory_Offset   : Device_Size_T;
      Flags           : Sparse_Memory_Bind_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Sparse_Memory_Bind_T);
   -- If pname:memory is not sname:VK_NULL_HANDLE, pname:memory and pname:memoryOffset must: match the memory requirements of the resource, as described in section &lt;&lt;resources-association&gt;&gt;
   -- If pname:memory is not sname:VK_NULL_HANDLE, pname:memory mustnot: have been created with a memory type that reports ename:VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT bit set
   -- pname:size must: be greater than `0`
   -- pname:resourceOffset must: be less than the size of the resource
   -- pname:size must: be less than or equal to the size of the resource minus pname:resourceOffset
   -- pname:memoryOffset must: be less than the size of pname:memory
   -- pname:size must: be less than or equal to the size of pname:memory minus pname:memoryOffset

   type Sparse_Image_Memory_Bind_T is record
      Subresource   : Image_Subresource_T;
      Offset        : Offset_3D_T;
      Extent        : Extent_3D_T;
      Memory        : Device_Memory_T;
      Memory_Offset : Device_Size_T;
      Flags         : Sparse_Memory_Bind_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Sparse_Image_Memory_Bind_T);
   -- If the &lt;&lt;features-features-sparseResidencyAliased,sparse aliased residency&gt;&gt; feature is not enabled, and if any other resources are bound to ranges of pname:memory, the range of pname:memory being bound mustnot: overlap with those bound ranges
   -- pname:memory and pname:memoryOffset must: match the memory requirements of the calling command's pname:image, as described in section &lt;&lt;resources-association&gt;&gt;
   -- pname:subresource must: be a valid image subresource for pname:image (see &lt;&lt;resources-image-views&gt;&gt;)
   -- pname:offset.x must: be a multiple of the sparse image block width (sname:VkSparseImageFormatProperties::pname:imageGranularity.width) of the image
   -- pname:extent.width must: either be a multiple of the sparse image block width of the image, or else pname:extent.width + pname:offset.x must: equal the width of the image subresource
   -- pname:offset.y must: be a multiple of the sparse image block height (sname:VkSparseImageFormatProperties::pname:imageGranularity.height) of the image
   -- pname:extent.height must: either be a multiple of the sparse image block height of the image, or else pname:extent.height + pname:offset.y must: equal the height of the image subresource
   -- pname:offset.z must: be a multiple of the sparse image block depth (sname:VkSparseImageFormatProperties::pname:imageGranularity.depth) of the image
   -- pname:extent.depth must: either be a multiple of the sparse image block depth of the image, or else pname:extent.depth + pname:offset.z must: equal the depth of the image subresource

   type Sparse_Memory_Bind_Const_Ptr is access constant Sparse_Memory_Bind_T;

   type Sparse_Buffer_Memory_Bind_Info_T is record
      Buffer     : Buffer_T;
      Bind_Count : Interfaces.Unsigned_32;
      Pbinds     : Sparse_Memory_Bind_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Sparse_Buffer_Memory_Bind_Info_T);

   type Sparse_Image_Opaque_Memory_Bind_Info_T is record
      Image      : Image_T;
      Bind_Count : Interfaces.Unsigned_32;
      Pbinds     : Sparse_Memory_Bind_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Sparse_Image_Opaque_Memory_Bind_Info_T);
   -- For any given element of pname:pBinds, if the pname:flags member of that element contains ename:VK_SPARSE_MEMORY_BIND_METADATA_BIT, the binding range defined must: be within the mip tail region of the metadata aspect of pname:image

   type Sparse_Image_Memory_Bind_Const_Ptr is access constant Sparse_Image_Memory_Bind_T;

   type Sparse_Image_Memory_Bind_Info_T is record
      Image      : Image_T;
      Bind_Count : Interfaces.Unsigned_32;
      Pbinds     : Sparse_Image_Memory_Bind_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Sparse_Image_Memory_Bind_Info_T);

   type Semaphore_Const_Ptr is access constant Semaphore_T;

   type Sparse_Buffer_Memory_Bind_Info_Const_Ptr is access constant Sparse_Buffer_Memory_Bind_Info_T;

   type Sparse_Image_Opaque_Memory_Bind_Info_Const_Ptr is access constant Sparse_Image_Opaque_Memory_Bind_Info_T;

   type Sparse_Image_Memory_Bind_Info_Const_Ptr is access constant Sparse_Image_Memory_Bind_Info_T;

   type Bind_Sparse_Info_T is record
      Stype                   : Structure_Type_T;
      Pnext                   : Void_Ptr;
      Wait_Semaphore_Count    : Interfaces.Unsigned_32;
      Pwait_Semaphores        : Semaphore_Const_Ptr;
      Buffer_Bind_Count       : Interfaces.Unsigned_32;
      Pbuffer_Binds           : Sparse_Buffer_Memory_Bind_Info_Const_Ptr;
      Image_Opaque_Bind_Count : Interfaces.Unsigned_32;
      Pimage_Opaque_Binds     : Sparse_Image_Opaque_Memory_Bind_Info_Const_Ptr;
      Image_Bind_Count        : Interfaces.Unsigned_32;
      Pimage_Binds            : Sparse_Image_Memory_Bind_Info_Const_Ptr;
      Signal_Semaphore_Count  : Interfaces.Unsigned_32;
      Psignal_Semaphores      : Semaphore_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Bind_Sparse_Info_T);

   type Image_Copy_T is record
      Src_Subresource : Image_Subresource_Layers_T;
      Src_Offset      : Offset_3D_T;
      Dst_Subresource : Image_Subresource_Layers_T;
      Dst_Offset      : Offset_3D_T;
      Extent          : Extent_3D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Copy_T);
   -- The pname:aspectMask member of pname:srcSubresource and pname:dstSubresource must: match
   -- The pname:layerCount member of pname:srcSubresource and pname:dstSubresource must: match
   -- If either of the calling command's pname:srcImage or pname:dstImage parameters are of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of both pname:srcSubresource and pname:dstSubresource must: be `0` and `1`, respectively
   -- The pname:aspectMask member of pname:srcSubresource must: specify aspects present in the calling command's pname:srcImage
   -- The pname:aspectMask member of pname:dstSubresource must: specify aspects present in the calling command's pname:dstImage
   -- pname:srcOffset.x and (pname:extent.width + pname:srcOffset.x) must: both be greater than or equal to `0` and less than or equal to the source image subresource width
   -- pname:srcOffset.y and (pname:extent.height + pname:srcOffset.y) must: both be greater than or equal to `0` and less than or equal to the source image subresource height
   -- pname:srcOffset.z and (pname:extent.depth + pname:srcOffset.z) must: both be greater than or equal to `0` and less than or equal to the source image subresource depth
   -- pname:dstOffset.x and (pname:extent.width + pname:dstOffset.x) must: both be greater than or equal to `0` and less than or equal to the destination image subresource width
   -- pname:dstOffset.y and (pname:extent.height + pname:dstOffset.y) must: both be greater than or equal to `0` and less than or equal to the destination image subresource height
   -- pname:dstOffset.z and (pname:extent.depth + pname:dstOffset.z) must: both be greater than or equal to `0` and less than or equal to the destination image subresource depth
   -- If the calling command's pname:srcImage is a compressed format image:
   -- If the calling command's pname:dstImage is a compressed format image:
   -- pname:srcOffset, pname:dstOffset, and pname:extent must: respect the image transfer granularity requirements of the queue family that it will be submitted against, as described in &lt;&lt;devsandqueues-physical-device-enumeration,Physical Device Enumeration&gt;&gt;

   type Src_Offsets_Array_Index_T is range 0 .. 2;

   type Src_Offsets_Array_T is array (Src_Offsets_Array_Index_T) of Offset_3D_T;
   pragma Convention (C, Src_Offsets_Array_T);

   type Dst_Offsets_Array_Index_T is range 0 .. 2;

   type Dst_Offsets_Array_T is array (Dst_Offsets_Array_Index_T) of Offset_3D_T;
   pragma Convention (C, Dst_Offsets_Array_T);

   type Image_Blit_T is record
      Src_Subresource : Image_Subresource_Layers_T;
      Src_Offsets     : Src_Offsets_Array_T;
      Dst_Subresource : Image_Subresource_Layers_T;
      Dst_Offsets     : Dst_Offsets_Array_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Blit_T);
   -- The pname:aspectMask member of pname:srcSubresource and pname:dstSubresource must: match
   -- The pname:layerCount member of pname:srcSubresource and pname:dstSubresource must: match
   -- If either of the calling command's pname:srcImage or pname:dstImage parameters are of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of both pname:srcSubresource and pname:dstSubresource must: be `0` and `1`, respectively
   -- The pname:aspectMask member of pname:srcSubresource must: specify aspects present in the calling command's pname:srcImage
   -- The pname:aspectMask member of pname:dstSubresource must: specify aspects present in the calling command's pname:dstImage
   -- pname:srcOffset[0].x and pname:srcOffset[1].x must: both be greater than or equal to `0` and less than or equal to the source image subresource width
   -- pname:srcOffset[0].y and pname:srcOffset[1].y must: both be greater than or equal to `0` and less than or equal to the source image subresource height
   -- pname:srcOffset[0].z and pname:srcOffset[1].z must: both be greater than or equal to `0` and less than or equal to the source image subresource depth
   -- pname:dstOffset[0].x and pname:dstOffset[1].x must: both be greater than or equal to `0` and less than or equal to the destination image subresource width
   -- pname:dstOffset[0].y and pname:dstOffset[1].y must: both be greater than or equal to `0` and less than or equal to the destination image subresource height
   -- pname:dstOffset[0].z and pname:dstOffset[1].z must: both be greater than or equal to `0` and less than or equal to the destination image subresource depth

   type Buffer_Image_Copy_T is record
      Buffer_Offset       : Device_Size_T;
      Buffer_Row_Length   : Interfaces.Unsigned_32;
      Buffer_Image_Height : Interfaces.Unsigned_32;
      Image_Subresource   : Image_Subresource_Layers_T;
      Image_Offset        : Offset_3D_T;
      Image_Extent        : Extent_3D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Buffer_Image_Copy_T);
   -- pname:bufferOffset must: be a multiple of the calling command's sname:VkImage parameter's texel size
   -- pname:bufferOffset must: be a multiple of `4`
   -- pname:bufferRowLength must: be `0`, or greater than or equal to the pname:width member of pname:imageExtent
   -- pname:bufferImageHeight must: be `0`, or greater than or equal to the pname:height member of pname:imageExtent
   -- pname:imageOffset.x and (pname:imageExtent.width + pname:imageOffset.x) must: both be greater than or equal to `0` and less than or equal to the image subresource width
   -- pname:imageOffset.y and (imageExtent.height + pname:imageOffset.y) must: both be greater than or equal to `0` and less than or equal to the image subresource height
   -- pname:imageOffset.z and (imageExtent.depth + pname:imageOffset.z) must: both be greater than or equal to `0` and less than or equal to the image subresource depth
   -- If the calling command's sname:VkImage parameter is a compressed format image:
   -- pname:bufferOffset, pname:bufferRowLength, pname:bufferImageHeight and all members of pname:imageOffset and pname:imageExtent must: respect the image transfer granularity requirements of the queue family that it will be submitted against, as described in &lt;&lt;devsandqueues-physical-device-enumeration,Physical Device Enumeration&gt;&gt;
   -- The pname:aspectMask member of pname:imageSubresource must: specify aspects present in the calling command's sname:VkImage parameter
   -- The pname:aspectMask member of pname:imageSubresource must: only have a single bit set
   -- If the calling command's sname:VkImage parameter is of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of pname:imageSubresource must: be `0` and `1`, respectively

   type Image_Resolve_T is record
      Src_Subresource : Image_Subresource_Layers_T;
      Src_Offset      : Offset_3D_T;
      Dst_Subresource : Image_Subresource_Layers_T;
      Dst_Offset      : Offset_3D_T;
      Extent          : Extent_3D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Image_Resolve_T);
   -- The pname:aspectMask member of pname:srcSubresource and pname:dstSubresource must: only contain ename:VK_IMAGE_ASPECT_COLOR_BIT
   -- The pname:layerCount member of pname:srcSubresource and pname:dstSubresource must: match
   -- If either of the calling command's pname:srcImage or pname:dstImage parameters are of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of both pname:srcSubresource and pname:dstSubresource must: be `0` and `1`, respectively

   type Shader_Module_Create_Info_T is record
      Stype     : Structure_Type_T;
      Pnext     : Void_Ptr;
      Flags     : Shader_Module_Create_Flags_T;
      Code_Size : Interfaces.C.size_t;
      Pcode     : Uint_32_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Shader_Module_Create_Info_T);
   -- pname:codeSize must: be greater than 0
   -- pname:codeSize must: be a multiple of 4
   -- pname:pCode must: point to valid SPIR-V code, formatted and packed as described by https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html[the SPIR-V Specification v1.0]
   -- pname:pCode must: adhere to the validation rules described by the &lt;&lt;spirvenv-module-validation, Validation Rules within a Module&gt;&gt; section of the &lt;&lt;spirvenv-capabilities,SPIR-V Environment&gt;&gt; appendix
   -- pname:pCode must: declare the code:Shader capability
   -- pname:pCode mustnot: declare any capability that is not supported by the API, as described by the &lt;&lt;spirvenv-module-validation, Capabilities&gt;&gt; section of the &lt;&lt;spirvenv-capabilities,SPIR-V Environment&gt;&gt; appendix
   -- If pname:pCode declares any of the capabilities that are listed as not required by the implementation, the relevant feature must: be enabled, as listed in the &lt;&lt;spirvenv-capabilities-table,SPIR-V Environment&gt;&gt; appendix

   type Sampler_Const_Ptr is access constant Sampler_T;

   type Descriptor_Set_Layout_Binding_T is record
      Binding             : Interfaces.Unsigned_32;
      Descriptor_Type     : Descriptor_Type_T;
      Descriptor_Count    : Interfaces.Unsigned_32;
      Stage_Flags         : Shader_Stage_Flags_T;
      Pimmutable_Samplers : Sampler_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Descriptor_Set_Layout_Binding_T);
   -- If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_SAMPLER or ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, and pname:descriptorCount is not `0` and pname:pImmutableSamplers is not `NULL`, pname:pImmutableSamplers must: be a pointer to an array of pname:descriptorCount valid sname:VkSampler handles
   -- If pname:descriptorCount is not `0`, pname:stageFlags must: be a valid combination of elink:VkShaderStageFlagBits values

   type Descriptor_Set_Layout_Binding_Const_Ptr is access constant Descriptor_Set_Layout_Binding_T;

   type Descriptor_Set_Layout_Create_Info_T is record
      Stype         : Structure_Type_T;
      Pnext         : Void_Ptr;
      Flags         : Descriptor_Set_Layout_Create_Flags_T;
      Binding_Count : Interfaces.Unsigned_32;
      Pbindings     : Descriptor_Set_Layout_Binding_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Descriptor_Set_Layout_Create_Info_T);

   type Descriptor_Pool_Size_T is record
      The_Type         : Descriptor_Type_T;
      Descriptor_Count : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Descriptor_Pool_Size_T);
   -- pname:descriptorCount must: be greater than `0`

   type Descriptor_Pool_Size_Const_Ptr is access constant Descriptor_Pool_Size_T;

   type Descriptor_Pool_Create_Info_T is record
      Stype           : Structure_Type_T;
      Pnext           : Void_Ptr;
      Flags           : Descriptor_Pool_Create_Flags_T;
      Max_Sets        : Interfaces.Unsigned_32;
      Pool_Size_Count : Interfaces.Unsigned_32;
      Ppool_Sizes     : Descriptor_Pool_Size_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Descriptor_Pool_Create_Info_T);
   -- pname:maxSets must: be greater than `0`

   type Descriptor_Set_Layout_Const_Ptr is access constant Descriptor_Set_Layout_T;

   type Descriptor_Set_Allocate_Info_T is record
      Stype                : Structure_Type_T;
      Pnext                : Void_Ptr;
      Descriptor_Pool      : Descriptor_Pool_T;
      Descriptor_Set_Count : Interfaces.Unsigned_32;
      Pset_Layouts         : Descriptor_Set_Layout_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Descriptor_Set_Allocate_Info_T);
   -- pname:descriptorSetCount mustnot: be greater than the number of sets that are currently available for allocation in pname:descriptorPool
   -- pname:descriptorPool must: have enough free descriptor capacity remaining to allocate the descriptor sets of the specified layouts

   type Specialization_Map_Entry_T is record
      Constant_Id : Interfaces.Unsigned_32;
      Offset      : Interfaces.Unsigned_32;
      Size        : Interfaces.C.size_t;
   end record;
   pragma Convention (C_Pass_By_Copy, Specialization_Map_Entry_T);

   type Specialization_Map_Entry_Const_Ptr is access constant Specialization_Map_Entry_T;

   type Specialization_Info_T is record
      Map_Entry_Count : Interfaces.Unsigned_32;
      Pmap_Entries    : Specialization_Map_Entry_Const_Ptr;
      Data_Size       : Interfaces.C.size_t;
      Pdata           : Void_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Specialization_Info_T);
   -- The pname:offset member of any given element of pname:pMapEntries must: be less than pname:dataSize
   -- For any given element of pname:pMapEntries, pname:size must: be less than or equal to pname:dataSize minus pname:offset

   type Specialization_Info_Const_Ptr is access constant Specialization_Info_T;

   type Pipeline_Shader_Stage_Create_Info_T is record
      Stype                : Structure_Type_T;
      Pnext                : Void_Ptr;
      Flags                : Pipeline_Shader_Stage_Create_Flags_T;
      Stage                : Shader_Stage_Flag_Bits_T;
      Module               : Shader_Module_T;
      Pname                : Interfaces.C.Strings.chars_ptr;
      Pspecialization_Info : Specialization_Info_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Shader_Stage_Create_Info_T);
   -- If the &lt;&lt;features-features-geometryShader,geometry shaders&gt;&gt; feature is not enabled, pname:stage mustnot: be ename:VK_SHADER_STAGE_GEOMETRY_BIT
   -- If the &lt;&lt;features-features-tessellationShader,tessellation shaders&gt;&gt; feature is not enabled, pname:stage mustnot: be ename:VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT or ename:VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
   -- pname:stage mustnot: be ename:VK_SHADER_STAGE_ALL_GRAPHICS, or ename:VK_SHADER_STAGE_ALL
   -- pname:pName must: be the name of an code:OpEntryPoint in pname:module with an execution model that matches pname:stage
   -- If the identified entry point includes any variable in its interface that is declared with the code:ClipDistance code:BuiltIn decoration, that variable mustnot: have an array size greater than sname:VkPhysicalDeviceLimits::pname:maxClipDistances
   -- If the identified entry point includes any variable in its interface that is declared with the code:CullDistance code:BuiltIn decoration, that variable mustnot: have an array size greater than sname:VkPhysicalDeviceLimits::pname:maxCullDistances
   -- If the identified entry point includes any variables in its interface that are declared with the code:ClipDistance or code:CullDistance code:BuiltIn decoration, those variables mustnot: have array sizes which sum to more than sname:VkPhysicalDeviceLimits::pname:maxCombinedClipAndCullDistances
   -- If the identified entry point includes any variable in its interface that is declared with the code:SampleMask code:BuiltIn decoration, that variable mustnot: have an array size greater than sname:VkPhysicalDeviceLimits::pname:maxSampleMaskWords
   -- If pname:stage is ename:VK_SHADER_STAGE_VERTEX_BIT, the identified entry point mustnot: include any input variable in its interface that is decorated with code:CullDistance
   -- If pname:stage is ename:VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT or ename:VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT, and the identified entry point has an code:OpExecutionMode instruction that specifies a patch size with code:OutputVertices, the patch size must: be greater than `0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxTessellationPatchSize
   -- If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, the identified entry point must: have an code:OpExecutionMode instruction that specifies a maximum output vertex count that is greater than `0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxGeometryOutputVertices
   -- If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, the identified entry point must: have an code:OpExecutionMode instruction that specifies an invocation count that is greater than `0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxGeometryShaderInvocations
   -- If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, and the identified entry point writes to code:Layer for any primitive, it must: write the same value to code:Layer for all vertices of a given primitive
   -- If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, and the identified entry point writes to code:ViewportIndex for any primitive, it must: write the same value to code:ViewportIndex for all vertices of a given primitive
   -- If pname:stage is ename:VK_SHADER_STAGE_FRAGMENT_BIT, the identified entry point mustnot: include any output variables in its interface decorated with code:CullDistance
   -- If pname:stage is ename:VK_SHADER_STAGE_FRAGMENT_BIT, and the identified entry point writes to code:FragDepth in any execution path, it must: write to code:FragDepth in all execution paths

   type Compute_Pipeline_Create_Info_T is record
      Stype                : Structure_Type_T;
      Pnext                : Void_Ptr;
      Flags                : Pipeline_Create_Flags_T;
      Stage                : Pipeline_Shader_Stage_Create_Info_T;
      Layout               : Pipeline_Layout_T;
      Base_Pipeline_Handle : Pipeline_T;
      Base_Pipeline_Index  : Interfaces.Integer_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Compute_Pipeline_Create_Info_T);
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, pname:basePipelineHandle must: be sname:VK_NULL_HANDLE
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, it must: be a valid index into the calling command's pname:pCreateInfos parameter
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineIndex must: be `-1`
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineHandle must: be a valid sname:VkPipeline handle
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, it must: be a valid handle to a compute sname:VkPipeline
   -- The pname:stage member of pname:stage must: be ename:VK_SHADER_STAGE_COMPUTE_BIT
   -- The shader code for the entry point identified by pname:stage and the rest of the state identified by this structure must: adhere to the pipeline linking rules described in the &lt;&lt;interfaces,Shader Interfaces&gt;&gt; chapter
   -- pname:layout must: be &lt;&lt;descriptorsets-pipelinelayout-consistency,consistent&gt;&gt; with all shaders specified in pname:pStages

   type Vertex_Input_Binding_Description_T is record
      Binding    : Interfaces.Unsigned_32;
      Stride     : Interfaces.Unsigned_32;
      Input_Rate : Vertex_Input_Rate_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Vertex_Input_Binding_Description_T);
   -- pname:binding must: be less than sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
   -- pname:stride must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindingStride

   type Vertex_Input_Attribute_Description_T is record
      Location : Interfaces.Unsigned_32;
      Binding  : Interfaces.Unsigned_32;
      Format   : Format_T;
      Offset   : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Vertex_Input_Attribute_Description_T);
   -- pname:location must: be less than sname:VkPhysicalDeviceLimits::pname:maxVertexInputAttributes
   -- pname:binding must: be less than sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
   -- pname:offset must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputAttributeOffset
   -- pname:format must: be allowed as a vertex buffer format, as specified by the ename:VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT flag in sname:VkFormatProperties::pname:bufferFeatures returned by fname:vkGetPhysicalDeviceFormatProperties

   type Vertex_Input_Binding_Description_Const_Ptr is access constant Vertex_Input_Binding_Description_T;

   type Vertex_Input_Attribute_Description_Const_Ptr is access constant Vertex_Input_Attribute_Description_T;

   type Pipeline_Vertex_Input_State_Create_Info_T is record
      Stype                              : Structure_Type_T;
      Pnext                              : Void_Ptr;
      Flags                              : Pipeline_Vertex_Input_State_Create_Flags_T;
      Vertex_Binding_Description_Count   : Interfaces.Unsigned_32;
      Pvertex_Binding_Descriptions       : Vertex_Input_Binding_Description_Const_Ptr;
      Vertex_Attribute_Description_Count : Interfaces.Unsigned_32;
      Pvertex_Attribute_Descriptions     : Vertex_Input_Attribute_Description_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Vertex_Input_State_Create_Info_T);
   -- pname:vertexBindingDescriptionCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
   -- pname:vertexAttributeDescriptionCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputAttributes
   -- For every pname:binding specified by any given element of pname:pVertexAttributeDescriptions, a sname:VkVertexInputBindingDescription must: exist in pname:pVertexBindingDescriptions with the same value of pname:binding
   -- All elements of pname:pVertexBindingDescriptions must: describe distinct binding numbers
   -- All elements of pname:pVertexAttributeDescriptions must: describe distinct attribute locations

   type Pipeline_Input_Assembly_State_Create_Info_T is record
      Stype                    : Structure_Type_T;
      Pnext                    : Void_Ptr;
      Flags                    : Pipeline_Input_Assembly_State_Create_Flags_T;
      Topology                 : Primitive_Topology_T;
      Primitive_Restart_Enable : Bool_32_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Input_Assembly_State_Create_Info_T);
   -- If pname:topology is ename:VK_PRIMITIVE_TOPOLOGY_POINT_LIST, ename:VK_PRIMITIVE_TOPOLOGY_LINE_LIST, ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, ename:VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY, ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY or ename:VK_PRIMITIVE_TOPOLOGY_PATCH_LIST, pname:primitiveRestartEnable must: be ename:VK_FALSE
   -- If the &lt;&lt;features-features-geometryShader,geometry shaders&gt;&gt; feature is not enabled, pname:topology mustnot: be any of ename:VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY, ename:VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY, ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY or ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
   -- If the &lt;&lt;features-features-tessellationShader,tessellation shaders&gt;&gt; feature is not enabled, pname:topology mustnot: be ename:VK_PRIMITIVE_TOPOLOGY_PATCH_LIST

   type Pipeline_Tessellation_State_Create_Info_T is record
      Stype                : Structure_Type_T;
      Pnext                : Void_Ptr;
      Flags                : Pipeline_Tessellation_State_Create_Flags_T;
      Patch_Control_Points : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Tessellation_State_Create_Info_T);
   -- pname:patchControlPoints must: be greater than zero and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxTessellationPatchSize

   type Viewport_Const_Ptr is access constant Viewport_T;

   type Rect_2D_Const_Ptr is access constant Rect_2D_T;

   type Pipeline_Viewport_State_Create_Info_T is record
      Stype          : Structure_Type_T;
      Pnext          : Void_Ptr;
      Flags          : Pipeline_Viewport_State_Create_Flags_T;
      Viewport_Count : Interfaces.Unsigned_32;
      Pviewports     : Viewport_Const_Ptr;
      Scissor_Count  : Interfaces.Unsigned_32;
      Pscissors      : Rect_2D_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Viewport_State_Create_Info_T);
   -- If the &lt;&lt;features-features-multiViewport,multiple viewports&gt;&gt; feature is not enabled, pname:viewportCount must: be `1`
   -- If the &lt;&lt;features-features-multiViewport,multiple viewports&gt;&gt; feature is not enabled, pname:scissorCount must: be `1`
   -- pname:viewportCount must: be between `1` and sname:VkPhysicalDeviceLimits::pname:maxViewports, inclusive
   -- pname:scissorCount must: be between `1` and sname:VkPhysicalDeviceLimits::pname:maxViewports, inclusive
   -- pname:scissorCount and pname:viewportCount must: be identical

   type Pipeline_Rasterization_State_Create_Info_T is record
      Stype                      : Structure_Type_T;
      Pnext                      : Void_Ptr;
      Flags                      : Pipeline_Rasterization_State_Create_Flags_T;
      Depth_Clamp_Enable         : Bool_32_T;
      Rasterizer_Discard_Enable  : Bool_32_T;
      Polygon_Mode               : Polygon_Mode_T;
      Cull_Mode                  : Cull_Mode_Flags_T;
      Front_Face                 : Front_Face_T;
      Depth_Bias_Enable          : Bool_32_T;
      Depth_Bias_Constant_Factor : Interfaces.C.C_float;
      Depth_Bias_Clamp           : Interfaces.C.C_float;
      Depth_Bias_Slope_Factor    : Interfaces.C.C_float;
      Line_Width                 : Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Rasterization_State_Create_Info_T);
   -- If the &lt;&lt;features-features-depthClamp,depth clamping&gt;&gt; feature is not enabled, pname:depthClampEnable must: be ename:VK_FALSE
   -- If the &lt;&lt;features-features-fillModeNonSolid,non-solid fill modes&gt;&gt; feature is not enabled, pname:polygonMode must: be ename:VK_POLYGON_MODE_FILL

   type Sample_Mask_Const_Ptr is access constant Sample_Mask_T;

   type Pipeline_Multisample_State_Create_Info_T is record
      Stype                    : Structure_Type_T;
      Pnext                    : Void_Ptr;
      Flags                    : Pipeline_Multisample_State_Create_Flags_T;
      Rasterization_Samples    : Sample_Count_Flag_Bits_T;
      Sample_Shading_Enable    : Bool_32_T;
      Min_Sample_Shading       : Interfaces.C.C_float;
      Psample_Mask             : Sample_Mask_Const_Ptr;
      Alpha_To_Coverage_Enable : Bool_32_T;
      Alpha_To_One_Enable      : Bool_32_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Multisample_State_Create_Info_T);
   -- If the &lt;&lt;features-features-sampleRateShading,sample rate shading&gt;&gt; feature is not enabled, pname:sampleShadingEnable must: be ename:VK_FALSE
   -- If the &lt;&lt;features-features-alphaToOne,alpha to one&gt;&gt; feature is not enabled, pname:alphaToOneEnable must: be ename:VK_FALSE
   -- pname:minSampleShading must: be in the range latexmath:[$[0,1\]$]

   type Pipeline_Color_Blend_Attachment_State_T is record
      Blend_Enable           : Bool_32_T;
      Src_Color_Blend_Factor : Blend_Factor_T;
      Dst_Color_Blend_Factor : Blend_Factor_T;
      Color_Blend_Op         : Blend_Op_T;
      Src_Alpha_Blend_Factor : Blend_Factor_T;
      Dst_Alpha_Blend_Factor : Blend_Factor_T;
      Alpha_Blend_Op         : Blend_Op_T;
      Color_Write_Mask       : Color_Component_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Color_Blend_Attachment_State_T);
   -- If the &lt;&lt;features-features-dualSrcBlend,dual source blending&gt;&gt; feature is not enabled, pname:srcColorBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
   -- If the &lt;&lt;features-features-dualSrcBlend,dual source blending&gt;&gt; feature is not enabled, pname:dstColorBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
   -- If the &lt;&lt;features-features-dualSrcBlend,dual source blending&gt;&gt; feature is not enabled, pname:srcAlphaBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
   -- If the &lt;&lt;features-features-dualSrcBlend,dual source blending&gt;&gt; feature is not enabled, pname:dstAlphaBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA

   type Pipeline_Color_Blend_Attachment_State_Const_Ptr is access constant Pipeline_Color_Blend_Attachment_State_T;

   type Blend_Constants_Array_Index_T is range 0 .. 4;

   type Blend_Constants_Array_T is array (Blend_Constants_Array_Index_T) of Interfaces.C.C_float;
   pragma Convention (C, Blend_Constants_Array_T);

   type Pipeline_Color_Blend_State_Create_Info_T is record
      Stype            : Structure_Type_T;
      Pnext            : Void_Ptr;
      Flags            : Pipeline_Color_Blend_State_Create_Flags_T;
      Logic_Op_Enable  : Bool_32_T;
      Logic_Op         : Logic_Op_T;
      Attachment_Count : Interfaces.Unsigned_32;
      Pattachments     : Pipeline_Color_Blend_Attachment_State_Const_Ptr;
      Blend_Constants  : Blend_Constants_Array_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Color_Blend_State_Create_Info_T);
   -- If the &lt;&lt;features-features-independentBlend,independent blending&gt;&gt; feature is not enabled, all elements of pname:pAttachments must: be identical
   -- If the &lt;&lt;features-features-logicOp,logic operations&gt;&gt; feature is not enabled, pname:logicOpEnable must: be ename:VK_FALSE
   -- If pname:logicOpEnable is ename:VK_TRUE, pname:logicOp must: be a valid elink:VkLogicOp value

   type Dynamic_State_Const_Ptr is access constant Dynamic_State_T;

   type Pipeline_Dynamic_State_Create_Info_T is record
      Stype               : Structure_Type_T;
      Pnext               : Void_Ptr;
      Flags               : Pipeline_Dynamic_State_Create_Flags_T;
      Dynamic_State_Count : Interfaces.Unsigned_32;
      Pdynamic_States     : Dynamic_State_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Dynamic_State_Create_Info_T);

   type Stencil_Op_State_T is record
      Fail_Op       : Stencil_Op_T;
      Pass_Op       : Stencil_Op_T;
      Depth_Fail_Op : Stencil_Op_T;
      Compare_Op    : Compare_Op_T;
      Compare_Mask  : Interfaces.Unsigned_32;
      Write_Mask    : Interfaces.Unsigned_32;
      Reference     : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Stencil_Op_State_T);

   type Pipeline_Depth_Stencil_State_Create_Info_T is record
      Stype                    : Structure_Type_T;
      Pnext                    : Void_Ptr;
      Flags                    : Pipeline_Depth_Stencil_State_Create_Flags_T;
      Depth_Test_Enable        : Bool_32_T;
      Depth_Write_Enable       : Bool_32_T;
      Depth_Compare_Op         : Compare_Op_T;
      Depth_Bounds_Test_Enable : Bool_32_T;
      Stencil_Test_Enable      : Bool_32_T;
      Front                    : Stencil_Op_State_T;
      Back                     : Stencil_Op_State_T;
      Min_Depth_Bounds         : Interfaces.C.C_float;
      Max_Depth_Bounds         : Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Depth_Stencil_State_Create_Info_T);
   -- If the &lt;&lt;features-features-depthBounds,depth bounds testing&gt;&gt; feature is not enabled, pname:depthBoundsTestEnable must: be ename:VK_FALSE

   type Pipeline_Shader_Stage_Create_Info_Const_Ptr is access constant Pipeline_Shader_Stage_Create_Info_T;

   type Pipeline_Vertex_Input_State_Create_Info_Const_Ptr is access constant Pipeline_Vertex_Input_State_Create_Info_T;

   type Pipeline_Input_Assembly_State_Create_Info_Const_Ptr is
     access constant Pipeline_Input_Assembly_State_Create_Info_T;

   type Pipeline_Tessellation_State_Create_Info_Const_Ptr is access constant Pipeline_Tessellation_State_Create_Info_T;

   type Pipeline_Viewport_State_Create_Info_Const_Ptr is access constant Pipeline_Viewport_State_Create_Info_T;

   type Pipeline_Rasterization_State_Create_Info_Const_Ptr is
     access constant Pipeline_Rasterization_State_Create_Info_T;

   type Pipeline_Multisample_State_Create_Info_Const_Ptr is access constant Pipeline_Multisample_State_Create_Info_T;

   type Pipeline_Depth_Stencil_State_Create_Info_Const_Ptr is
     access constant Pipeline_Depth_Stencil_State_Create_Info_T;

   type Pipeline_Color_Blend_State_Create_Info_Const_Ptr is access constant Pipeline_Color_Blend_State_Create_Info_T;

   type Pipeline_Dynamic_State_Create_Info_Const_Ptr is access constant Pipeline_Dynamic_State_Create_Info_T;

   type Graphics_Pipeline_Create_Info_T is record
      Stype                 : Structure_Type_T;
      Pnext                 : Void_Ptr;
      Flags                 : Pipeline_Create_Flags_T;
      Stage_Count           : Interfaces.Unsigned_32;
      Pstages               : Pipeline_Shader_Stage_Create_Info_Const_Ptr;
      Pvertex_Input_State   : Pipeline_Vertex_Input_State_Create_Info_Const_Ptr;
      Pinput_Assembly_State : Pipeline_Input_Assembly_State_Create_Info_Const_Ptr;
      Ptessellation_State   : Pipeline_Tessellation_State_Create_Info_Const_Ptr;
      Pviewport_State       : Pipeline_Viewport_State_Create_Info_Const_Ptr;
      Prasterization_State  : Pipeline_Rasterization_State_Create_Info_Const_Ptr;
      Pmultisample_State    : Pipeline_Multisample_State_Create_Info_Const_Ptr;
      Pdepth_Stencil_State  : Pipeline_Depth_Stencil_State_Create_Info_Const_Ptr;
      Pcolor_Blend_State    : Pipeline_Color_Blend_State_Create_Info_Const_Ptr;
      Pdynamic_State        : Pipeline_Dynamic_State_Create_Info_Const_Ptr;
      Layout                : Pipeline_Layout_T;
      Render_Pass           : Render_Pass_T;
      Subpass               : Interfaces.Unsigned_32;
      Base_Pipeline_Handle  : Pipeline_T;
      Base_Pipeline_Index   : Interfaces.Integer_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Graphics_Pipeline_Create_Info_T);
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, pname:basePipelineHandle must: be sname:VK_NULL_HANDLE
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, it must: be a valid index into the calling command's pname:pCreateInfos parameter
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineIndex must: be `-1`
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineHandle must: be a valid sname:VkPipeline handle
   -- If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, it must: be a valid handle to a graphics sname:VkPipeline
   -- The pname:stage member of each element of pname:pStages must: be unique
   -- The pname:stage member of one element of pname:pStages must: be ename:VK_SHADER_STAGE_VERTEX_BIT
   -- The pname:stage member of any given element of pname:pStages mustnot: be ename:VK_SHADER_STAGE_COMPUTE_BIT
   -- If pname:pStages includes a tessellation control shader stage, it must: include a tessellation evaluation shader stage
   -- If pname:pStages includes a tessellation evaluation shader stage, it must: include a tessellation control shader stage
   -- If pname:pStages includes a tessellation control shader stage and a tessellation evaluation shader stage, pname:pTessellationState mustnot: be `NULL`
   -- If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, the shader code of at least one must: contain an code:OpExecutionMode instruction that specifies the type of subdivision in the pipeline
   -- If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, and the shader code of both contain an code:OpExecutionMode instruction that specifies the type of subdivision in the pipeline, they must: both specify the same subdivision mode
   -- If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, the shader code of at least one must: contain an code:OpExecutionMode instruction that specifies the output patch size in the pipeline
   -- If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, and the shader code of both contain an code:OpExecutionMode instruction that specifies the out patch size in the pipeline, they must: both specify the same patch size
   -- If pname:pStages includes tessellation shader stages, the pname:topology member of pname:pInputAssembly must: be ename:VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
   -- If pname:pStages includes a geometry shader stage, and doesn't include any tessellation shader stages, its shader code must: contain an code:OpExecutionMode instruction that specifies an input primitive type that is &lt;&lt;shaders-geometry-execution, compatible&gt;&gt; with the primitive topology specified in pname:pInputAssembly
   -- If pname:pStages includes a geometry shader stage, and also includes tessellation shader stages, its shader code must: contain an code:OpExecutionMode instruction that specifies an input primitive type that is &lt;&lt;shaders-geometry-execution, compatible&gt;&gt; with the primitive topology that is output by the tessellation stages
   -- If pname:pStages includes a fragment shader stage and a geometry shader stage, and the fragment shader code reads from an input variable that is decorated with code:PrimitiveID, then the geometry shader code must: write to a matching output variable, decorated with code:PrimitiveID, in all execution paths
   -- If pname:pStages includes a fragment shader stage, its shader code mustnot: read from any input attachment that is defined as ename:VK_ATTACHMENT_UNUSED in pname:subpass
   -- The shader code for the entry points identified by pname:pStages, and the rest of the state identified by this structure must: adhere to the pipeline linking rules described in the &lt;&lt;interfaces,Shader Interfaces&gt;&gt; chapter
   -- If pname:subpass uses a depth/stencil attachment in pname:renderpass that has a layout of ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL in the sname:VkAttachmentReference defined by pname:subpass, and pname:pDepthStencilState is not `NULL`, the pname:depthWriteEnable member of pname:pDepthStencilState must: be ename:VK_FALSE
   -- If pname:subpass uses a depth/stencil attachment in pname:renderpass that has a layout of ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL in the sname:VkAttachmentReference defined by pname:subpass, and pname:pDepthStencilState is not `NULL`, the pname:failOp, pname:passOp and pname:depthFailOp members of each of the pname:front and pname:back members of pname:pDepthStencilState must: be ename:VK_STENCIL_OP_KEEP
   -- If pname:pColorBlendState is not `NULL`, the pname:blendEnable member of each element of the pname:pAttachment member of pname:pColorBlendState must: be ename:VK_FALSE if the pname:format of the attachment referred to in pname:subpass of pname:renderPass does not support color blend operations, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures or sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
   -- If pname:pColorBlendState is not `NULL`, The pname:attachmentCount member of pname:pColorBlendState must: be equal to the pname:colorAttachmentCount used to create pname:subpass
   -- If no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_VIEWPORT, the pname:pViewports member of pname:pViewportState must: be a pointer to an array of pname:pViewportState->viewportCount sname:VkViewport structures
   -- If no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_SCISSOR, the pname:pScissors member of pname:pViewportState must: be a pointer to an array of pname:pViewportState->scissorCount sname:VkRect2D structures
   -- If the wide lines feature is not enabled, and no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_LINE_WIDTH, the pname:lineWidth member of pname:pRasterizationState must: be `1.0`
   -- If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, pname:pViewportState must: be a pointer to a valid sname:VkPipelineViewportStateCreateInfo structure
   -- If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, pname:pMultisampleState must: be a pointer to a valid sname:VkPipelineMultisampleStateCreateInfo structure
   -- If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, and pname:subpass uses a depth/stencil attachment, pname:pDepthStencilState must: be a pointer to a valid sname:VkPipelineDepthStencilStateCreateInfo structure
   -- If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, and pname:subpass uses color attachments, pname:pColorBlendState must: be a pointer to a valid sname:VkPipelineColorBlendStateCreateInfo structure
   -- If the depth bias clamping feature is not enabled, no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_DEPTH_BIAS, and the pname:depthBiasEnable member of pname:pDepthStencil is ename:VK_TRUE, the pname:depthBiasClamp member of pname:pDepthStencil must: be `0.0`
   -- If no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_DEPTH_BOUNDS, and the pname:depthBoundsTestEnable member of pname:pDepthStencil is ename:VK_TRUE, the pname:minDepthBounds and pname:maxDepthBounds members of pname:pDepthStencil must: be between `0.0` and `1.0`, inclusive
   -- pname:layout must: be &lt;&lt;descriptorsets-pipelinelayout-consistency,consistent&gt;&gt; with all shaders specified in pname:pStages
   -- If pname:subpass uses color and/or depth/stencil attachments, then the pname:rasterizationSamples member of pname:pMultisampleState must: be the same as the sample count for those subpass attachments
   -- If pname:subpass does not use any color and/or depth/stencil attachments, then the pname:rasterizationSamples member of pname:pMultisampleState must: follow the rules for a &lt;&lt;renderpass-noattachments, zero-attachment subpass&gt;&gt;
   -- pname:subpass must: be a valid subpass within pname:renderpass

   type Pipeline_Cache_Create_Info_T is record
      Stype             : Structure_Type_T;
      Pnext             : Void_Ptr;
      Flags             : Pipeline_Cache_Create_Flags_T;
      Initial_Data_Size : Interfaces.C.size_t;
      Pinitial_Data     : Void_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Cache_Create_Info_T);
   -- If pname:initialDataSize is not `0`, it must: be equal to the size of pname:pInitialData, as returned by fname:vkGetPipelineCacheData when pname:pInitialData was originally retrieved
   -- If pname:initialDataSize is not `0`, pname:pInitialData must: have been retrieved from a previous call to fname:vkGetPipelineCacheData

   type Push_Constant_Range_T is record
      Stage_Flags : Shader_Stage_Flags_T;
      Offset      : Interfaces.Unsigned_32;
      Size        : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Push_Constant_Range_T);
-- pname:offset must: be less than sname:VkPhysicalDeviceLimits::pname:maxPushConstantsSize
-- pname:size must: be greater than `0`
-- pname:size must: be a multiple of `4`
-- pname:size must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPushConstantsSize minus pname:offset

   type Push_Constant_Range_Const_Ptr is access constant Push_Constant_Range_T;

   type Pipeline_Layout_Create_Info_T is record
      Stype                     : Structure_Type_T;
      Pnext                     : Void_Ptr;
      Flags                     : Pipeline_Layout_Create_Flags_T;
      Set_Layout_Count          : Interfaces.Unsigned_32;
      Pset_Layouts              : Descriptor_Set_Layout_Const_Ptr;
      Push_Constant_Range_Count : Interfaces.Unsigned_32;
      Ppush_Constant_Ranges     : Push_Constant_Range_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Layout_Create_Info_T);
   -- pname:setLayoutCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxBoundDescriptorSets
   -- The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_SAMPLER and ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorSamplers
   -- The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER and ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorUniformBuffers
   -- The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER and ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorStorageBuffers
   -- The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, and ename:VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorSampledImages
   -- The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE, and ename:VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorStorageImages

   type Sampler_Create_Info_T is record
      Stype                    : Structure_Type_T;
      Pnext                    : Void_Ptr;
      Flags                    : Sampler_Create_Flags_T;
      Mag_Filter               : Filter_T;
      Min_Filter               : Filter_T;
      Mipmap_Mode              : Sampler_Mipmap_Mode_T;
      Address_Mode_U           : Sampler_Address_Mode_T;
      Address_Mode_V           : Sampler_Address_Mode_T;
      Address_Mode_W           : Sampler_Address_Mode_T;
      Mip_Lod_Bias             : Interfaces.C.C_float;
      Anisotropy_Enable        : Bool_32_T;
      Max_Anisotropy           : Interfaces.C.C_float;
      Compare_Enable           : Bool_32_T;
      Compare_Op               : Compare_Op_T;
      Min_Lod                  : Interfaces.C.C_float;
      Max_Lod                  : Interfaces.C.C_float;
      Border_Color             : Border_Color_T;
      Unnormalized_Coordinates : Bool_32_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Sampler_Create_Info_T);
   -- The absolute value of pname:mipLodBias must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxSamplerLodBias
   -- If the &lt;&lt;features-features-samplerAnisotropy,anisotropic sampling&gt;&gt; feature is not enabled, pname:anisotropyEnable must: be ename:VK_FALSE
   -- If pname:anisotropyEnable is ename:VK_TRUE, pname:maxAnisotropy must: be between `1.0` and sname:VkPhysicalDeviceLimits::pname:maxSamplerAnisotropy, inclusive
   -- If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:minFilter and pname:magFilter must: be equal
   -- If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:mipmapMode must: be ename:VK_SAMPLER_MIPMAP_MODE_NEAREST
   -- If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:minLod and pname:maxLod must: be zero
   -- If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:addressModeU and pname:addressModeV must: each be either ename:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE or ename:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
   -- If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:anisotropyEnable must: be ename:VK_FALSE
   -- If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:compareEnable must: be ename:VK_FALSE
   -- If any of pname:addressModeU, pname:addressModeV or pname:addressModeW are ename:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER, pname:borderColor must: be a valid elink:VkBorderColor value
   -- If the VK_KHR_mirror_clamp_to_edge extension is not enabled, pname:addressModeU, pname:addressModeV and pname:addressModeW mustnot: be ename:VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
   -- If pname:compareEnable is ename:VK_TRUE, pname:compareOp must: be a valid elink:VkCompareOp value

   type Command_Pool_Create_Info_T is record
      Stype              : Structure_Type_T;
      Pnext              : Void_Ptr;
      Flags              : Command_Pool_Create_Flags_T;
      Queue_Family_Index : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Command_Pool_Create_Info_T);
-- pname:queueFamilyIndex must: be the index of a queue family available in the calling command's pname:device parameter

   type Command_Buffer_Allocate_Info_T is record
      Stype                : Structure_Type_T;
      Pnext                : Void_Ptr;
      Command_Pool         : Command_Pool_T;
      Level                : Command_Buffer_Level_T;
      Command_Buffer_Count : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Command_Buffer_Allocate_Info_T);
   -- pname:commandBufferCount must: be greater than `0`

   type Command_Buffer_Inheritance_Info_T is record
      Stype                  : Structure_Type_T;
      Pnext                  : Void_Ptr;
      Render_Pass            : Render_Pass_T;
      Subpass                : Interfaces.Unsigned_32;
      Framebuffer            : Framebuffer_T;
      Occlusion_Query_Enable : Bool_32_T;
      Query_Flags            : Query_Control_Flags_T;
      Pipeline_Statistics    : Query_Pipeline_Statistic_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Command_Buffer_Inheritance_Info_T);
   -- If the &lt;&lt;features-features-inheritedQueries,inherited queries&gt;&gt; feature is not enabled, pname:occlusionQueryEnable must: be ename:VK_FALSE
   -- If the &lt;&lt;features-features-inheritedQueries,inherited queries&gt;&gt; feature is enabled, pname:queryFlags must: be a valid combination of elink:VkQueryControlFlagBits values
   -- If the &lt;&lt;features-features-pipelineStatisticsQuery,pipeline statistics queries&gt;&gt; feature is not enabled, pname:pipelineStatistics must: be code:0

   type Command_Buffer_Inheritance_Info_Const_Ptr is access constant Command_Buffer_Inheritance_Info_T;

   type Command_Buffer_Begin_Info_T is record
      Stype             : Structure_Type_T;
      Pnext             : Void_Ptr;
      Flags             : Command_Buffer_Usage_Flags_T;
      Pinheritance_Info : Command_Buffer_Inheritance_Info_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Command_Buffer_Begin_Info_T);
   -- If pname:flags contains ename:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, the pname:renderPass member of pname:pInheritanceInfo must: be a valid sname:VkRenderPass
   -- If pname:flags contains ename:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, the pname:subpass member of pname:pInheritanceInfo must: be a valid subpass index within the pname:renderPass member of pname:pInheritanceInfo
   -- If pname:flags contains ename:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, the pname:framebuffer member of pname:pInheritanceInfo must: be either sname:VK_NULL_HANDLE, or a valid sname:VkFramebuffer that is compatible with the pname:renderPass member of pname:pInheritanceInfo

   type Float_32_Array_Index_T is range 0 .. 4;

   type Float_32_Array_T is array (Float_32_Array_Index_T) of Interfaces.C.C_float;
   pragma Convention (C, Float_32_Array_T);

   type Int_32_Array_Index_T is range 0 .. 4;

   type Int_32_Array_T is array (Int_32_Array_Index_T) of Interfaces.Integer_32;
   pragma Convention (C, Int_32_Array_T);

   type Uint_32_Array_Index_T is range 0 .. 4;

   type Uint_32_Array_T is array (Uint_32_Array_Index_T) of Interfaces.Unsigned_32;
   pragma Convention (C, Uint_32_Array_T);

   type Clear_Color_Value_Kind_Id_T is
     (Clear_Color_Value_Float_32, Clear_Color_Value_Int_32, Clear_Color_Value_Uint_32);

   type Clear_Color_Value_T (Kind_Id : Clear_Color_Value_Kind_Id_T := Clear_Color_Value_Kind_Id_T'First) is record
      case Kind_Id is
         when Clear_Color_Value_Float_32 =>
            Float_32 : Float_32_Array_T;
         when Clear_Color_Value_Int_32 =>
            Int_32 : Int_32_Array_T;
         when Clear_Color_Value_Uint_32 =>
            Uint_32 : Uint_32_Array_T;
      end case;
   end record;
   pragma Unchecked_Union (Clear_Color_Value_T);
   pragma Convention (C, Clear_Color_Value_T);

   type Clear_Depth_Stencil_Value_T is record
      Depth   : Interfaces.C.C_float;
      Stencil : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Clear_Depth_Stencil_Value_T);

   type Clear_Value_Kind_Id_T is (Clear_Value_Color, Clear_Value_Depth_Stencil);

   type Clear_Value_T (Kind_Id : Clear_Value_Kind_Id_T := Clear_Value_Kind_Id_T'First) is record
      case Kind_Id is
         when Clear_Value_Color =>
            Color : Clear_Color_Value_T;
         when Clear_Value_Depth_Stencil =>
            Depth_Stencil : Clear_Depth_Stencil_Value_T;
      end case;
   end record;
   pragma Unchecked_Union (Clear_Value_T);
   pragma Convention (C, Clear_Value_T);

   type Clear_Attachment_T is record
      Aspect_Mask      : Image_Aspect_Flags_T;
      Color_Attachment : Interfaces.Unsigned_32;
      Clear_Value      : Clear_Value_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Clear_Attachment_T);
   -- If pname:aspectMask includes ename:VK_IMAGE_ASPECT_COLOR_BIT, it mustnot: include ename:VK_IMAGE_ASPECT_DEPTH_BIT or ename:VK_IMAGE_ASPECT_STENCIL_BIT
   -- pname:aspectMask mustnot: include ename:VK_IMAGE_ASPECT_METADATA_BIT

   type Attachment_Description_T is record
      Flags            : Attachment_Description_Flags_T;
      Format           : Format_T;
      Samples          : Sample_Count_Flag_Bits_T;
      Load_Op          : Attachment_Load_Op_T;
      Store_Op         : Attachment_Store_Op_T;
      Stencil_Load_Op  : Attachment_Load_Op_T;
      Stencil_Store_Op : Attachment_Store_Op_T;
      Initial_Layout   : Image_Layout_T;
      Final_Layout     : Image_Layout_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Attachment_Description_T);

   type Attachment_Reference_T is record
      Attachment : Interfaces.Unsigned_32;
      Layout     : Image_Layout_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Attachment_Reference_T);

   type Attachment_Reference_Const_Ptr is access constant Attachment_Reference_T;

   type Subpass_Description_T is record
      Flags                     : Subpass_Description_Flags_T;
      Pipeline_Bind_Point       : Pipeline_Bind_Point_T;
      Input_Attachment_Count    : Interfaces.Unsigned_32;
      Pinput_Attachments        : Attachment_Reference_Const_Ptr;
      Color_Attachment_Count    : Interfaces.Unsigned_32;
      Pcolor_Attachments        : Attachment_Reference_Const_Ptr;
      Presolve_Attachments      : Attachment_Reference_Const_Ptr;
      Pdepth_Stencil_Attachment : Attachment_Reference_Const_Ptr;
      Preserve_Attachment_Count : Interfaces.Unsigned_32;
      Ppreserve_Attachments     : Uint_32_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Subpass_Description_T);
   -- pname:pipelineBindPoint must: be ename:VK_PIPELINE_BIND_POINT_GRAPHICS
   -- pname:colorCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxColorAttachments
   -- If the first use of an attachment in this render pass is as an input attachment, and the attachment is not also used as a color or depth/stencil attachment in the same subpass, then pname:loadOp mustnot: be ename:VK_ATTACHMENT_LOAD_OP_CLEAR
   -- If pname:pResolveAttachments is not `NULL`, for each resolve attachment that does not have the value ename:VK_ATTACHMENT_UNUSED, the corresponding color attachment mustnot: have the value ename:VK_ATTACHMENT_UNUSED
   -- If pname:pResolveAttachments is not `NULL`, the sample count of each element of pname:pColorAttachments must: be anything other than ename:VK_SAMPLE_COUNT_1_BIT
   -- Any given element of pname:pResolveAttachments must: have a sample count of ename:VK_SAMPLE_COUNT_1_BIT
   -- Any given element of pname:pResolveAttachments must: have the same elink:VkFormat as its corresponding color attachment
   -- All attachments in pname:pColorAttachments and pname:pDepthStencilAttachment that are not ename:VK_ATTACHMENT_UNUSED must: have the same sample count
   -- If any input attachments are ename:VK_ATTACHMENT_UNUSED, then any pipelines bound during the subpass mustnot: accesss those input attachments from the fragment shader
   -- The pname:attachment member of any element of pname:pPreserveAttachments mustnot: be ename:VK_ATTACHMENT_UNUSED
   -- Any given element of pname:pPreserveAttachments mustnot: also be an element of any other member of the subpass description
   -- If any attachment is used as both an input attachment and a color or depth/stencil attachment, then each use must: use the same pname:layout

   type Subpass_Dependency_T is record
      Src_Subpass      : Interfaces.Unsigned_32;
      Dst_Subpass      : Interfaces.Unsigned_32;
      Src_Stage_Mask   : Pipeline_Stage_Flags_T;
      Dst_Stage_Mask   : Pipeline_Stage_Flags_T;
      Src_Access_Mask  : Access_Flags_T;
      Dst_Access_Mask  : Access_Flags_T;
      Dependency_Flags : Dependency_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Subpass_Dependency_T);
   -- If the &lt;&lt;features-features-geometryShader,geometry shaders&gt;&gt; feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
   -- If the &lt;&lt;features-features-geometryShader,geometry shaders&gt;&gt; feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
   -- If the &lt;&lt;features-features-tessellationShader,tessellation shaders&gt;&gt; feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
   -- If the &lt;&lt;features-features-tessellationShader,tessellation shaders&gt;&gt; feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
   -- pname:srcSubpass must: be less than or equal to pname:dstSubpass, unless one of them is ename:VK_SUBPASS_EXTERNAL, to avoid cyclic dependencies and ensure a valid execution order
   -- pname:srcSubpass and pname:dstSubpass mustnot: both be equal to ename:VK_SUBPASS_EXTERNAL

   type Attachment_Description_Const_Ptr is access constant Attachment_Description_T;

   type Subpass_Description_Const_Ptr is access constant Subpass_Description_T;

   type Subpass_Dependency_Const_Ptr is access constant Subpass_Dependency_T;

   type Render_Pass_Create_Info_T is record
      Stype            : Structure_Type_T;
      Pnext            : Void_Ptr;
      Flags            : Render_Pass_Create_Flags_T;
      Attachment_Count : Interfaces.Unsigned_32;
      Pattachments     : Attachment_Description_Const_Ptr;
      Subpass_Count    : Interfaces.Unsigned_32;
      Psubpasses       : Subpass_Description_Const_Ptr;
      Dependency_Count : Interfaces.Unsigned_32;
      Pdependencies    : Subpass_Dependency_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Render_Pass_Create_Info_T);
   -- If any two subpasses operate on attachments with overlapping ranges of the same sname:VkDeviceMemory object, and at least one subpass writes to that area of sname:VkDeviceMemory, a subpass dependency must: be included (either directly or via some intermediate subpasses) between them
   -- If the pname:attachment member of any element of pname:pInputAttachments, pname:pColorAttachments, pname:pResolveAttachments or pname:pDepthStencilAttachment, or the attachment indexed by any element of pname:pPreserveAttachments in any given element of pname:pSubpasses is bound to a range of a sname:VkDeviceMemory object that overlaps with any other attachment in any subpass (including the same subpass), the sname:VkAttachmentDescription structures describing them must: include ename:VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT in pname:flags
   -- If the pname:attachment member of any element of pname:pInputAttachments, pname:pColorAttachments, pname:pResolveAttachments or pname:pDepthStencilAttachment, or any element of pname:pPreserveAttachments in any given element of pname:pSubpasses is not ename:VK_ATTACHMENT_UNUSED, it must: be less than pname:attachmentCount
   -- The value of any element of the pname:pPreserveAttachments member in any given element of pname:pSubpasses mustnot: be ename:VK_ATTACHMENT_UNUSED

   type Event_Create_Info_T is record
      Stype : Structure_Type_T;
      Pnext : Void_Ptr;
      Flags : Event_Create_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Event_Create_Info_T);

   type Fence_Create_Info_T is record
      Stype : Structure_Type_T;
      Pnext : Void_Ptr;
      Flags : Fence_Create_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Fence_Create_Info_T);

   type Physical_Device_Features_T is record
      Robust_Buffer_Access                         : Bool_32_T;
      Full_Draw_Index_Uint_32                      : Bool_32_T;
      Image_Cube_Array                             : Bool_32_T;
      Independent_Blend                            : Bool_32_T;
      Geometry_Shader                              : Bool_32_T;
      Tessellation_Shader                          : Bool_32_T;
      Sample_Rate_Shading                          : Bool_32_T;
      Dual_Src_Blend                               : Bool_32_T;
      Logic_Op                                     : Bool_32_T;
      Multi_Draw_Indirect                          : Bool_32_T;
      Draw_Indirect_First_Instance                 : Bool_32_T;
      Depth_Clamp                                  : Bool_32_T;
      Depth_Bias_Clamp                             : Bool_32_T;
      Fill_Mode_Non_Solid                          : Bool_32_T;
      Depth_Bounds                                 : Bool_32_T;
      Wide_Lines                                   : Bool_32_T;
      Large_Points                                 : Bool_32_T;
      Alpha_To_One                                 : Bool_32_T;
      Multi_Viewport                               : Bool_32_T;
      Sampler_Anisotropy                           : Bool_32_T;
      Texture_Compression_Etc_2                    : Bool_32_T;
      Texture_Compression_Astc_ldr                 : Bool_32_T;
      Texture_Compression_Bc                       : Bool_32_T;
      Occlusion_Query_Precise                      : Bool_32_T;
      Pipeline_Statistics_Query                    : Bool_32_T;
      Vertex_Pipeline_Stores_And_Atomics           : Bool_32_T;
      Fragment_Stores_And_Atomics                  : Bool_32_T;
      Shader_Tessellation_And_Geometry_Point_Size  : Bool_32_T;
      Shader_Image_Gather_Extended                 : Bool_32_T;
      Shader_Storage_Image_Extended_Formats        : Bool_32_T;
      Shader_Storage_Image_Multisample             : Bool_32_T;
      Shader_Storage_Image_Read_Without_Format     : Bool_32_T;
      Shader_Storage_Image_Write_Without_Format    : Bool_32_T;
      Shader_Uniform_Buffer_Array_Dynamic_Indexing : Bool_32_T;
      Shader_Sampled_Image_Array_Dynamic_Indexing  : Bool_32_T;
      Shader_Storage_Buffer_Array_Dynamic_Indexing : Bool_32_T;
      Shader_Storage_Image_Array_Dynamic_Indexing  : Bool_32_T;
      Shader_Clip_Distance                         : Bool_32_T;
      Shader_Cull_Distance                         : Bool_32_T;
      Shader_Float_64                              : Bool_32_T;
      Shader_Int_64                                : Bool_32_T;
      Shader_Int_16                                : Bool_32_T;
      Shader_Resource_Residency                    : Bool_32_T;
      Shader_Resource_Min_Lod                      : Bool_32_T;
      Sparse_Binding                               : Bool_32_T;
      Sparse_Residency_Buffer                      : Bool_32_T;
      Sparse_Residency_Image_2_D                   : Bool_32_T;
      Sparse_Residency_Image_3_D                   : Bool_32_T;
      Sparse_Residency_2_Samples                   : Bool_32_T;
      Sparse_Residency_4_Samples                   : Bool_32_T;
      Sparse_Residency_8_Samples                   : Bool_32_T;
      Sparse_Residency_16_Samples                  : Bool_32_T;
      Sparse_Residency_Aliased                     : Bool_32_T;
      Variable_Multisample_Rate                    : Bool_32_T;
      Inherited_Queries                            : Bool_32_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Physical_Device_Features_T);
   -- If any member of this structure is ename:VK_FALSE, as returned by flink:vkGetPhysicalDeviceFeatures, then it must: be ename:VK_FALSE when passed as part of the sname:VkDeviceCreateInfo struct when creating a device

   type Physical_Device_Sparse_Properties_T is record
      Residency_Standard_2_Dblock_Shape             : Bool_32_T;
      Residency_Standard_2_Dmultisample_Block_Shape : Bool_32_T;
      Residency_Standard_3_Dblock_Shape             : Bool_32_T;
      Residency_Aligned_Mip_Size                    : Bool_32_T;
      Residency_Non_Resident_Strict                 : Bool_32_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Physical_Device_Sparse_Properties_T);

   type Max_Compute_Work_Group_Count_Array_Index_T is range 0 .. 3;

   type Max_Compute_Work_Group_Count_Array_T is
     array (Max_Compute_Work_Group_Count_Array_Index_T) of Interfaces.Unsigned_32;
   pragma Convention (C, Max_Compute_Work_Group_Count_Array_T);

   type Max_Compute_Work_Group_Size_Array_Index_T is range 0 .. 3;

   type Max_Compute_Work_Group_Size_Array_T is
     array (Max_Compute_Work_Group_Size_Array_Index_T) of Interfaces.Unsigned_32;
   pragma Convention (C, Max_Compute_Work_Group_Size_Array_T);

   type Max_Viewport_Dimensions_Array_Index_T is range 0 .. 2;

   type Max_Viewport_Dimensions_Array_T is array (Max_Viewport_Dimensions_Array_Index_T) of Interfaces.Unsigned_32;
   pragma Convention (C, Max_Viewport_Dimensions_Array_T);

   type Viewport_Bounds_Range_Array_Index_T is range 0 .. 2;

   type Viewport_Bounds_Range_Array_T is array (Viewport_Bounds_Range_Array_Index_T) of Interfaces.C.C_float;
   pragma Convention (C, Viewport_Bounds_Range_Array_T);

   type Point_Size_Range_Array_Index_T is range 0 .. 2;

   type Point_Size_Range_Array_T is array (Point_Size_Range_Array_Index_T) of Interfaces.C.C_float;
   pragma Convention (C, Point_Size_Range_Array_T);

   type Line_Width_Range_Array_Index_T is range 0 .. 2;

   type Line_Width_Range_Array_T is array (Line_Width_Range_Array_Index_T) of Interfaces.C.C_float;
   pragma Convention (C, Line_Width_Range_Array_T);

   type Physical_Device_Limits_T is record
      Max_Image_Dimension_1_D                               : Interfaces.Unsigned_32;
      Max_Image_Dimension_2_D                               : Interfaces.Unsigned_32;
      Max_Image_Dimension_3_D                               : Interfaces.Unsigned_32;
      Max_Image_Dimension_Cube                              : Interfaces.Unsigned_32;
      Max_Image_Array_Layers                                : Interfaces.Unsigned_32;
      Max_Texel_Buffer_Elements                             : Interfaces.Unsigned_32;
      Max_Uniform_Buffer_Range                              : Interfaces.Unsigned_32;
      Max_Storage_Buffer_Range                              : Interfaces.Unsigned_32;
      Max_Push_Constants_Size                               : Interfaces.Unsigned_32;
      Max_Memory_Allocation_Count                           : Interfaces.Unsigned_32;
      Max_Sampler_Allocation_Count                          : Interfaces.Unsigned_32;
      Buffer_Image_Granularity                              : Device_Size_T;
      Sparse_Address_Space_Size                             : Device_Size_T;
      Max_Bound_Descriptor_Sets                             : Interfaces.Unsigned_32;
      Max_Per_Stage_Descriptor_Samplers                     : Interfaces.Unsigned_32;
      Max_Per_Stage_Descriptor_Uniform_Buffers              : Interfaces.Unsigned_32;
      Max_Per_Stage_Descriptor_Storage_Buffers              : Interfaces.Unsigned_32;
      Max_Per_Stage_Descriptor_Sampled_Images               : Interfaces.Unsigned_32;
      Max_Per_Stage_Descriptor_Storage_Images               : Interfaces.Unsigned_32;
      Max_Per_Stage_Descriptor_Input_Attachments            : Interfaces.Unsigned_32;
      Max_Per_Stage_Resources                               : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Samplers                           : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Uniform_Buffers                    : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Uniform_Buffers_Dynamic            : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Storage_Buffers                    : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Storage_Buffers_Dynamic            : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Sampled_Images                     : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Storage_Images                     : Interfaces.Unsigned_32;
      Max_Descriptor_Set_Input_Attachments                  : Interfaces.Unsigned_32;
      Max_Vertex_Input_Attributes                           : Interfaces.Unsigned_32;
      Max_Vertex_Input_Bindings                             : Interfaces.Unsigned_32;
      Max_Vertex_Input_Attribute_Offset                     : Interfaces.Unsigned_32;
      Max_Vertex_Input_Binding_Stride                       : Interfaces.Unsigned_32;
      Max_Vertex_Output_Components                          : Interfaces.Unsigned_32;
      Max_Tessellation_Generation_Level                     : Interfaces.Unsigned_32;
      Max_Tessellation_Patch_Size                           : Interfaces.Unsigned_32;
      Max_Tessellation_Control_Per_Vertex_Input_Components  : Interfaces.Unsigned_32;
      Max_Tessellation_Control_Per_Vertex_Output_Components : Interfaces.Unsigned_32;
      Max_Tessellation_Control_Per_Patch_Output_Components  : Interfaces.Unsigned_32;
      Max_Tessellation_Control_Total_Output_Components      : Interfaces.Unsigned_32;
      Max_Tessellation_Evaluation_Input_Components          : Interfaces.Unsigned_32;
      Max_Tessellation_Evaluation_Output_Components         : Interfaces.Unsigned_32;
      Max_Geometry_Shader_Invocations                       : Interfaces.Unsigned_32;
      Max_Geometry_Input_Components                         : Interfaces.Unsigned_32;
      Max_Geometry_Output_Components                        : Interfaces.Unsigned_32;
      Max_Geometry_Output_Vertices                          : Interfaces.Unsigned_32;
      Max_Geometry_Total_Output_Components                  : Interfaces.Unsigned_32;
      Max_Fragment_Input_Components                         : Interfaces.Unsigned_32;
      Max_Fragment_Output_Attachments                       : Interfaces.Unsigned_32;
      Max_Fragment_Dual_Src_Attachments                     : Interfaces.Unsigned_32;
      Max_Fragment_Combined_Output_Resources                : Interfaces.Unsigned_32;
      Max_Compute_Shared_Memory_Size                        : Interfaces.Unsigned_32;
      Max_Compute_Work_Group_Count                          : Max_Compute_Work_Group_Count_Array_T;
      Max_Compute_Work_Group_Invocations                    : Interfaces.Unsigned_32;
      Max_Compute_Work_Group_Size                           : Max_Compute_Work_Group_Size_Array_T;
      Sub_Pixel_Precision_Bits                              : Interfaces.Unsigned_32;
      Sub_Texel_Precision_Bits                              : Interfaces.Unsigned_32;
      Mipmap_Precision_Bits                                 : Interfaces.Unsigned_32;
      Max_Draw_Indexed_Index_Value                          : Interfaces.Unsigned_32;
      Max_Draw_Indirect_Count                               : Interfaces.Unsigned_32;
      Max_Sampler_Lod_Bias                                  : Interfaces.C.C_float;
      Max_Sampler_Anisotropy                                : Interfaces.C.C_float;
      Max_Viewports                                         : Interfaces.Unsigned_32;
      Max_Viewport_Dimensions                               : Max_Viewport_Dimensions_Array_T;
      Viewport_Bounds_Range                                 : Viewport_Bounds_Range_Array_T;
      Viewport_Sub_Pixel_Bits                               : Interfaces.Unsigned_32;
      Min_Memory_Map_Alignment                              : Interfaces.C.size_t;
      Min_Texel_Buffer_Offset_Alignment                     : Device_Size_T;
      Min_Uniform_Buffer_Offset_Alignment                   : Device_Size_T;
      Min_Storage_Buffer_Offset_Alignment                   : Device_Size_T;
      Min_Texel_Offset                                      : Interfaces.Integer_32;
      Max_Texel_Offset                                      : Interfaces.Unsigned_32;
      Min_Texel_Gather_Offset                               : Interfaces.Integer_32;
      Max_Texel_Gather_Offset                               : Interfaces.Unsigned_32;
      Min_Interpolation_Offset                              : Interfaces.C.C_float;
      Max_Interpolation_Offset                              : Interfaces.C.C_float;
      Sub_Pixel_Interpolation_Offset_Bits                   : Interfaces.Unsigned_32;
      Max_Framebuffer_Width                                 : Interfaces.Unsigned_32;
      Max_Framebuffer_Height                                : Interfaces.Unsigned_32;
      Max_Framebuffer_Layers                                : Interfaces.Unsigned_32;
      Framebuffer_Color_Sample_Counts                       : Sample_Count_Flags_T;
      Framebuffer_Depth_Sample_Counts                       : Sample_Count_Flags_T;
      Framebuffer_Stencil_Sample_Counts                     : Sample_Count_Flags_T;
      Framebuffer_No_Attachments_Sample_Counts              : Sample_Count_Flags_T;
      Max_Color_Attachments                                 : Interfaces.Unsigned_32;
      Sampled_Image_Color_Sample_Counts                     : Sample_Count_Flags_T;
      Sampled_Image_Integer_Sample_Counts                   : Sample_Count_Flags_T;
      Sampled_Image_Depth_Sample_Counts                     : Sample_Count_Flags_T;
      Sampled_Image_Stencil_Sample_Counts                   : Sample_Count_Flags_T;
      Storage_Image_Sample_Counts                           : Sample_Count_Flags_T;
      Max_Sample_Mask_Words                                 : Interfaces.Unsigned_32;
      Timestamp_Compute_And_Graphics                        : Bool_32_T;
      Timestamp_Period                                      : Interfaces.C.C_float;
      Max_Clip_Distances                                    : Interfaces.Unsigned_32;
      Max_Cull_Distances                                    : Interfaces.Unsigned_32;
      Max_Combined_Clip_And_Cull_Distances                  : Interfaces.Unsigned_32;
      Discrete_Queue_Priorities                             : Interfaces.Unsigned_32;
      Point_Size_Range                                      : Point_Size_Range_Array_T;
      Line_Width_Range                                      : Line_Width_Range_Array_T;
      Point_Size_Granularity                                : Interfaces.C.C_float;
      Line_Width_Granularity                                : Interfaces.C.C_float;
      Strict_Lines                                          : Bool_32_T;
      Standard_Sample_Locations                             : Bool_32_T;
      Optimal_Buffer_Copy_Offset_Alignment                  : Device_Size_T;
      Optimal_Buffer_Copy_Row_Pitch_Alignment               : Device_Size_T;
      Non_Coherent_Atom_Size                                : Device_Size_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Physical_Device_Limits_T);

   type Semaphore_Create_Info_T is record
      Stype : Structure_Type_T;
      Pnext : Void_Ptr;
      Flags : Semaphore_Create_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Semaphore_Create_Info_T);

   type Query_Pool_Create_Info_T is record
      Stype               : Structure_Type_T;
      Pnext               : Void_Ptr;
      Flags               : Query_Pool_Create_Flags_T;
      Query_Type          : Query_Type_T;
      Query_Count         : Interfaces.Unsigned_32;
      Pipeline_Statistics : Query_Pipeline_Statistic_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Pool_Create_Info_T);
   -- If the &lt;&lt;features-features-pipelineStatisticsQuery,pipeline statistics queries&gt;&gt; feature is not enabled, pname:queryType mustnot: be ename:VK_QUERY_TYPE_PIPELINE_STATISTICS
   -- If pname:queryType is ename:VK_QUERY_TYPE_PIPELINE_STATISTICS, pname:pipelineStatistics must: be a valid combination of elink:VkQueryPipelineStatisticFlagBits values

   type Image_View_Const_Ptr is access constant Image_View_T;

   type Framebuffer_Create_Info_T is record
      Stype            : Structure_Type_T;
      Pnext            : Void_Ptr;
      Flags            : Framebuffer_Create_Flags_T;
      Render_Pass      : Render_Pass_T;
      Attachment_Count : Interfaces.Unsigned_32;
      Pattachments     : Image_View_Const_Ptr;
      Width            : Interfaces.Unsigned_32;
      Height           : Interfaces.Unsigned_32;
      Layers           : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Framebuffer_Create_Info_T);
   -- pname:attachmentCount must: be equal to the attachment count specified in pname:renderPass
   -- Any given element of pname:pAttachments that is used as a color attachment or resolve attachment by pname:renderPass must: have been created with a pname:usage value including ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
   -- Any given element of pname:pAttachments that is used as a depth/stencil attachment by pname:renderPass must: have been created with a pname:usage value including ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
   -- Any given element of pname:pAttachments that is used as an input attachment by pname:renderPass must: have been created with a pname:usage value including ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
   -- Any given element of pname:pAttachments must: have been created with an elink:VkFormat value that matches the elink:VkFormat specified by the corresponding sname:VkAttachmentDescription in pname:renderPass
   -- Any given element of pname:pAttachments must: have been created with a pname:samples value that matches the pname:samples value specified by the corresponding sname:VkAttachmentDescription in pname:renderPass
   -- Any given element of pname:pAttachments must: have dimensions at least as large as the corresponding framebuffer dimension
   -- Any given element of pname:pAttachments must: only specify a single mip-level
   -- Any given element of pname:pAttachments must: have been created with the identity swizzle
   -- pname:width must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferWidth
   -- pname:height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferHeight
   -- pname:layers must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferLayers

   type Draw_Indirect_Command_T is record
      Vertex_Count   : Interfaces.Unsigned_32;
      Instance_Count : Interfaces.Unsigned_32;
      First_Vertex   : Interfaces.Unsigned_32;
      First_Instance : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Draw_Indirect_Command_T);
   -- For a given vertex buffer binding, any attribute data fetched must: be entirely contained within the corresponding vertex buffer binding, as described in &lt;&lt;fxvertex-input&gt;&gt;
   -- If the &lt;&lt;features-features-drawIndirectFirstInstance,drawIndirectFirstInstance&gt;&gt; feature is not enabled, pname:firstInstance must: be code:0

   type Draw_Indexed_Indirect_Command_T is record
      Index_Count    : Interfaces.Unsigned_32;
      Instance_Count : Interfaces.Unsigned_32;
      First_Index    : Interfaces.Unsigned_32;
      Vertex_Offset  : Interfaces.Integer_32;
      First_Instance : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Draw_Indexed_Indirect_Command_T);
   -- For a given vertex buffer binding, any attribute data fetched must: be entirely contained within the corresponding vertex buffer binding, as described in &lt;&lt;fxvertex-input&gt;&gt;
   -- (pname:indexSize * (pname:firstIndex + pname:indexCount) + pname:offset) must: be less than or equal to the size of the currently bound index buffer, with pname:indexSize being based on the type specified by pname:indexType, where the index buffer, pname:indexType, and pname:offset are specified via fname:vkCmdBindIndexBuffer
   -- If the &lt;&lt;features-features-drawIndirectFirstInstance,drawIndirectFirstInstance&gt;&gt; feature is not enabled, pname:firstInstance must: be code:0

   type Dispatch_Indirect_Command_T is record
      X : Interfaces.Unsigned_32;
      Y : Interfaces.Unsigned_32;
      Z : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Dispatch_Indirect_Command_T);
   -- pname:x must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[0]
   -- pname:y must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[1]
   -- pname:z must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[2]

   type Pipeline_Stage_Flags_Const_Ptr is access constant Pipeline_Stage_Flags_T;

   type Command_Buffer_Const_Ptr is access constant Command_Buffer_T;

   type Submit_Info_T is record
      Stype                  : Structure_Type_T;
      Pnext                  : Void_Ptr;
      Wait_Semaphore_Count   : Interfaces.Unsigned_32;
      Pwait_Semaphores       : Semaphore_Const_Ptr;
      Pwait_Dst_Stage_Mask   : Pipeline_Stage_Flags_Const_Ptr;
      Command_Buffer_Count   : Interfaces.Unsigned_32;
      Pcommand_Buffers       : Command_Buffer_Const_Ptr;
      Signal_Semaphore_Count : Interfaces.Unsigned_32;
      Psignal_Semaphores     : Semaphore_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Submit_Info_T);
   -- Any given element of pname:pSignalSemaphores must: currently be unsignaled
   -- Any given element of pname:pCommandBuffers must: either have been recorded with the ename:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT, or not currently be executing on the device
   -- Any given element of pname:pCommandBuffers must: be in the executable state
   -- If any given element of pname:pCommandBuffers contains commands that execute secondary command buffers, those secondary command buffers must: have been recorded with the ename:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT, or not currently be executing on the device
   -- If any given element of pname:pCommandBuffers was created with ename:VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, it mustnot: have been previously submitted without re-recording that command buffer
   -- If any given element of pname:pCommandBuffers contains commands that execute secondary command buffers created with ename:VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, each such secondary command buffer mustnot: have been previously submitted without re-recording that command buffer
   -- Any given element of pname:pCommandBuffers mustnot: contain commands that execute a secondary command buffer, if that secondary command buffer has been recorded in another primary command buffer after it was recorded into this sname:VkCommandBuffer
   -- Any given element of pname:pCommandBuffers must: have been created on a sname:VkCommandPool that was created for the same queue family that the calling command's pname:queue belongs to
   -- Any given element of pname:pCommandBuffers mustnot: have been created with ename:VK_COMMAND_BUFFER_LEVEL_SECONDARY
   -- Any given element of sname:VkSemaphore in pname:pWaitSemaphores must: refer to a prior signal of that sname:VkSemaphore that won't be consumed by any other wait on that semaphore
   -- If the &lt;&lt;features-features-geometryShader,geometry shaders&gt;&gt; feature is not enabled, any given element of pname:pWaitDstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
   -- If the &lt;&lt;features-features-tessellationShader,tessellation shaders&gt;&gt; feature is not enabled, any given element of pname:pWaitDstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT

   type Display_Properties_Khr_T is record
      Display                : Display_Khr_T;
      Display_Name           : Interfaces.C.Strings.chars_ptr;
      Physical_Dimensions    : Extent_2D_T;
      Physical_Resolution    : Extent_2D_T;
      Supported_Transforms   : Surface_Transform_Flags_Khr_T;
      Plane_Reorder_Possible : Bool_32_T;
      Persistent_Content     : Bool_32_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Properties_Khr_T);

   type Display_Plane_Properties_Khr_T is record
      Current_Display     : Display_Khr_T;
      Current_Stack_Index : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Plane_Properties_Khr_T);

   type Display_Mode_Parameters_Khr_T is record
      Visible_Region : Extent_2D_T;
      Refresh_Rate   : Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Mode_Parameters_Khr_T);

   type Display_Mode_Properties_Khr_T is record
      Display_Mode : Display_Mode_Khr_T;
      Parameters   : Display_Mode_Parameters_Khr_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Mode_Properties_Khr_T);

   type Display_Mode_Create_Info_Khr_T is record
      Stype      : Structure_Type_T;
      Pnext      : Void_Ptr;
      Flags      : Display_Mode_Create_Flags_Khr_T;
      Parameters : Display_Mode_Parameters_Khr_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Mode_Create_Info_Khr_T);
   -- The pname:width and pname:height members of the pname:visibleRegion member of pname:parameters must: be greater than `0`
   -- The pname:refreshRate member of pname:parameters must: be greater than `0`

   type Display_Plane_Capabilities_Khr_T is record
      Supported_Alpha  : Display_Plane_Alpha_Flags_Khr_T;
      Min_Src_Position : Offset_2D_T;
      Max_Src_Position : Offset_2D_T;
      Min_Src_Extent   : Extent_2D_T;
      Max_Src_Extent   : Extent_2D_T;
      Min_Dst_Position : Offset_2D_T;
      Max_Dst_Position : Offset_2D_T;
      Min_Dst_Extent   : Extent_2D_T;
      Max_Dst_Extent   : Extent_2D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Plane_Capabilities_Khr_T);

   type Display_Surface_Create_Info_Khr_T is record
      Stype             : Structure_Type_T;
      Pnext             : Void_Ptr;
      Flags             : Display_Surface_Create_Flags_Khr_T;
      Display_Mode      : Display_Mode_Khr_T;
      Plane_Index       : Interfaces.Unsigned_32;
      Plane_Stack_Index : Interfaces.Unsigned_32;
      Transform         : Surface_Transform_Flag_Bits_Khr_T;
      Global_Alpha      : Interfaces.C.C_float;
      Alpha_Mode        : Display_Plane_Alpha_Flag_Bits_Khr_T;
      Image_Extent      : Extent_2D_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Surface_Create_Info_Khr_T);
   -- pname:planeIndex must: be less than the number of display planes supported by the device as determined by calling fname:vkGetPhysicalDeviceDisplayPlanePropertiesKHR
   -- If the pname:planeReorderPossible member of the sname:VkDisplayPropertiesKHR structure returned by fname:vkGetPhysicalDeviceDisplayPropertiesKHR for the display corresponding to pname:displayMode is ename:VK_TRUE then pname:planeStackIndex must: be less than the number of display planes supported by the device as determined by calling fname:vkGetPhysicalDeviceDisplayPlanePropertiesKHR; otherwise pname:planeStackIndex must: equal the pname:currentStackIndex member of sname:VkDisplayPlanePropertiesKHR returned by fname:vkGetPhysicalDeviceDisplayPlanePropertiesKHR for the display plane corresponding to pname:displayMode
   -- If pname:alphaMode is ename:VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR then pname:globalAlpha must: be between `0` and `1`, inclusive
   -- pname:alphaMode must: be `0` or one of the bits present in the pname:supportedAlpha member of sname:VkDisplayPlaneCapabilitiesKHR returned by fname:vkGetDisplayPlaneCapabilitiesKHR for the display plane corresponding to pname:displayMode
   -- The pname:width and pname:height members of pname:imageExtent must: be less than the pname:maxImageDimensions2D member of sname:VkPhysicalDeviceLimits

   type Display_Present_Info_Khr_T is record
      Stype      : Structure_Type_T;
      Pnext      : Void_Ptr;
      Src_Rect   : Rect_2D_T;
      Dst_Rect   : Rect_2D_T;
      Persistent : Bool_32_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Display_Present_Info_Khr_T);
   -- pname:srcRect must: specify a rectangular region that is a subset of the image being presented
   -- pname:dstRect must: specify a rectangular region that is a subset of the pname:visibleRegion parameter of the display mode the swapchain being presented uses
   -- If the pname:persistentContent member of the sname:VkDisplayPropertiesKHR structure returned by fname:vkGetPhysicalDeviceDisplayPropertiesKHR for the display the present operation targets then pname:persistent must: be ename:VK_FALSE

   type Surface_Capabilities_Khr_T is record
      Min_Image_Count           : Interfaces.Unsigned_32;
      Max_Image_Count           : Interfaces.Unsigned_32;
      Current_Extent            : Extent_2D_T;
      Min_Image_Extent          : Extent_2D_T;
      Max_Image_Extent          : Extent_2D_T;
      Max_Image_Array_Layers    : Interfaces.Unsigned_32;
      Supported_Transforms      : Surface_Transform_Flags_Khr_T;
      Current_Transform         : Surface_Transform_Flag_Bits_Khr_T;
      Supported_Composite_Alpha : Composite_Alpha_Flags_Khr_T;
      Supported_Usage_Flags     : Image_Usage_Flags_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Surface_Capabilities_Khr_T);

   type Surface_Format_Khr_T is record
      Format      : Format_T;
      Color_Space : Color_Space_Khr_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Surface_Format_Khr_T);

   type Swapchain_Create_Info_Khr_T is record
      Stype                    : Structure_Type_T;
      Pnext                    : Void_Ptr;
      Flags                    : Swapchain_Create_Flags_Khr_T;
      Surface                  : Surface_Khr_T;
      Min_Image_Count          : Interfaces.Unsigned_32;
      Image_Format             : Format_T;
      Image_Color_Space        : Color_Space_Khr_T;
      Image_Extent             : Extent_2D_T;
      Image_Array_Layers       : Interfaces.Unsigned_32;
      Image_Usage              : Image_Usage_Flags_T;
      Image_Sharing_Mode       : Sharing_Mode_T;
      Queue_Family_Index_Count : Interfaces.Unsigned_32;
      Pqueue_Family_Indices    : Uint_32_Const_Ptr;
      Pre_Transform            : Surface_Transform_Flag_Bits_Khr_T;
      Composite_Alpha          : Composite_Alpha_Flag_Bits_Khr_T;
      Present_Mode             : Present_Mode_Khr_T;
      Clipped                  : Bool_32_T;
      Old_Swapchain            : Swapchain_Khr_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Swapchain_Create_Info_Khr_T);
   -- pname:surface must: be a surface that is supported by the device as determined using fname:vkGetPhysicalDeviceSurfaceSupportKHR
   -- The native window referred to by pname:surface mustnot: already be associated with a swapchain other than pname:oldSwapchain, or with a non-Vulkan graphics API surface
   -- pname:minImageCount must: be greater than or equal to the value returned in the pname:minImageCount member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
   -- pname:minImageCount must: be less than or equal to the value returned in the pname:maxImageCount member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface if the returned pname:maxImageCount is not zero
   -- pname:imageFormat and pname:imageColorspace must: match the pname:format and pname:colorSpace members, respectively, of one of the sname:VkSurfaceFormatKHR structures returned by fname:vkGetPhysicalDeviceSurfaceFormatsKHR for the surface
   -- pname:imageExtent must: be between pname:minImageExtent and pname:maxImageExtent, inclusive, where pname:minImageExtent and pname:maxImageExtent are members of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
   -- pname:imageArrayLayers must: be greater than `0` and less than or equal to the pname:maxImageArrayLayers member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
   -- pname:imageUsage must: be a subset of the supported usage flags present in the pname:supportedUsageFlags member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
   -- If pname:imageSharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:pQueueFamilyIndices must: be a pointer to an array of pname:queueFamilyIndexCount basetype:uint32_t values
   -- If pname:imageSharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:queueFamilyIndexCount must: be greater than `1`
   -- pname:preTransform must: be one of the bits present in the pname:supportedTransforms member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
   -- pname:compositeAlpha must: be one of the bits present in the pname:supportedCompositeAlpha member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
   -- pname:presentMode must: be one of the ename:VkPresentModeKHR values returned by fname:vkGetPhysicalDeviceSurfacePresentModesKHR for the surface

   type Swapchain_Khr_Const_Ptr is access constant Swapchain_Khr_T;

   type Result_Ptr is access all Result_T;

   type Present_Info_Khr_T is record
      Stype                : Structure_Type_T;
      Pnext                : Void_Ptr;
      Wait_Semaphore_Count : Interfaces.Unsigned_32;
      Pwait_Semaphores     : Semaphore_Const_Ptr;
      Swapchain_Count      : Interfaces.Unsigned_32;
      Pswapchains          : Swapchain_Khr_Const_Ptr;
      Pimage_Indices       : Uint_32_Const_Ptr;
      Presults             : Result_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Present_Info_Khr_T);
   -- Any given element of pname:pImageIndices must: be the index of a presentable image acquired from the swapchain specified by the corresponding element of the pname:pSwapchains array
   -- Any given element of sname:VkSemaphore in pname:pWaitSemaphores must: refer to a prior signal of that sname:VkSemaphore that won't be consumed by any other wait on that semaphore

   type Debug_Report_Callback_Create_Info_Ext_T is record
      Stype        : Structure_Type_T;
      Pnext        : Void_Ptr;
      Flags        : Debug_Report_Flags_Ext_T;
      Pfn_Callback : Pfn_Vk_Debug_Report_Callback_Ext_T;
      Puser_Data   : Void_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Debug_Report_Callback_Create_Info_Ext_T);

   type Pipeline_Rasterization_State_Rasterization_Order_Amd_T is record
      Stype               : Structure_Type_T;
      Pnext               : Void_Ptr;
      Rasterization_Order : Rasterization_Order_Amd_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Pipeline_Rasterization_State_Rasterization_Order_Amd_T);

   type Debug_Marker_Object_Name_Info_Ext_T is record
      Stype        : Structure_Type_T;
      Pnext        : Void_Ptr;
      Object_Type  : Debug_Report_Object_Type_Ext_T;
      Object       : Interfaces.Unsigned_64;
      Pobject_Name : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Debug_Marker_Object_Name_Info_Ext_T);

   type Debug_Marker_Object_Tag_Info_Ext_T is record
      Stype       : Structure_Type_T;
      Pnext       : Void_Ptr;
      Object_Type : Debug_Report_Object_Type_Ext_T;
      Object      : Interfaces.Unsigned_64;
      Tag_Name    : Interfaces.Unsigned_64;
      Tag_Size    : Interfaces.C.size_t;
      Ptag        : Void_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Debug_Marker_Object_Tag_Info_Ext_T);

   type Color_Array_Index_T is range 0 .. 4;

   type Color_Array_T is array (Color_Array_Index_T) of Interfaces.C.C_float;
   pragma Convention (C, Color_Array_T);

   type Debug_Marker_Marker_Info_Ext_T is record
      Stype        : Structure_Type_T;
      Pnext        : Void_Ptr;
      Pmarker_Name : Interfaces.C.Strings.chars_ptr;
      Color        : Color_Array_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Debug_Marker_Marker_Info_Ext_T);

   type Device_Name_Array_Index_T is range 0 .. MAX_PHYSICAL_DEVICE_NAME_SIZE;

   type Device_Name_Array_T is array (Device_Name_Array_Index_T) of Interfaces.C.char;
   pragma Convention (C, Device_Name_Array_T);

   type Pipeline_Cache_Uuid_Array_Index_T is range 0 .. UUID_SIZE;

   type Pipeline_Cache_Uuid_Array_T is array (Pipeline_Cache_Uuid_Array_Index_T) of Interfaces.Unsigned_8;
   pragma Convention (C, Pipeline_Cache_Uuid_Array_T);

   type Physical_Device_Properties_T is record
      Api_Version         : Version_T;
      Driver_Version      : Interfaces.Unsigned_32;
      Vendor_Id           : Interfaces.Unsigned_32;
      Device_Id           : Interfaces.Unsigned_32;
      Device_Type         : Physical_Device_Type_T;
      Device_Name         : Device_Name_Array_T;
      Pipeline_Cache_Uuid : Pipeline_Cache_Uuid_Array_T;
      Limits              : Physical_Device_Limits_T;
      Sparse_Properties   : Physical_Device_Sparse_Properties_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Physical_Device_Properties_T);

   type Device_Queue_Create_Info_Const_Ptr is access constant Device_Queue_Create_Info_T;

   type Physical_Device_Features_Const_Ptr is access constant Physical_Device_Features_T;

   type Device_Create_Info_T is record
      Stype                      : Structure_Type_T;
      Pnext                      : Void_Ptr;
      Flags                      : Device_Create_Flags_T;
      Queue_Create_Info_Count    : Interfaces.Unsigned_32;
      Pqueue_Create_Infos        : Device_Queue_Create_Info_Const_Ptr;
      Enabled_Layer_Count        : Interfaces.Unsigned_32;
      Pp_Enabled_Layer_Names     : Char_Ptr_Array_Conversions.Object_Address;
      Enabled_Extension_Count    : Interfaces.Unsigned_32;
      Pp_Enabled_Extension_Names : Char_Ptr_Array_Conversions.Object_Address;
      Penabled_Features          : Physical_Device_Features_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Device_Create_Info_T);
   -- pname:ppEnabledLayerNames must: either be sname:NULL or contain the same sequence of layer names that was enabled when creating the parent instance
   -- Any given element of pname:ppEnabledExtensionNames must: be the name of an extension present on the system, exactly matching a string returned in the sname:VkExtensionProperties structure by fname:vkEnumerateDeviceExtensionProperties
   -- If an extension listed in pname:ppEnabledExtensionNames is provided as part of a layer, then both the layer and extension must: be enabled to enable that extension
   -- The pname:queueFamilyIndex member of any given element of pname:pQueueCreateInfos must: be unique within pname:pQueueCreateInfos

   type Memory_Types_Array_Index_T is range 0 .. MAX_MEMORY_TYPES;

   type Memory_Types_Array_T is array (Memory_Types_Array_Index_T) of Memory_Type_T;
   pragma Convention (C, Memory_Types_Array_T);

   type Memory_Heaps_Array_Index_T is range 0 .. MAX_MEMORY_HEAPS;

   type Memory_Heaps_Array_T is array (Memory_Heaps_Array_Index_T) of Memory_Heap_T;
   pragma Convention (C, Memory_Heaps_Array_T);

   type Physical_Device_Memory_Properties_T is record
      Memory_Type_Count : Interfaces.Unsigned_32;
      Memory_Types      : Memory_Types_Array_T;
      Memory_Heap_Count : Interfaces.Unsigned_32;
      Memory_Heaps      : Memory_Heaps_Array_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Physical_Device_Memory_Properties_T);

   type Clear_Value_Const_Ptr is access constant Clear_Value_T;

   type Render_Pass_Begin_Info_T is record
      Stype             : Structure_Type_T;
      Pnext             : Void_Ptr;
      Render_Pass       : Render_Pass_T;
      Framebuffer       : Framebuffer_T;
      Render_Area       : Rect_2D_T;
      Clear_Value_Count : Interfaces.Unsigned_32;
      Pclear_Values     : Clear_Value_Const_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Render_Pass_Begin_Info_T);
   -- pname:clearValueCount must: be greater than or equal to the number of attachments in pname:renderPass that specify a pname:loadOp of ename:VK_ATTACHMENT_LOAD_OP_CLEAR

   type Instance_Create_Info_Const_Ptr is access constant Instance_Create_Info_T;

   type Allocation_Callbacks_Const_Ptr is access constant Allocation_Callbacks_T;

   type Instance_Ptr is access all Instance_T;

   function Create_Instance
     (Pcreate_Info : Instance_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pinstance    : Instance_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Instance, "vkCreateInstance");

   procedure Destroy_Instance (Instance : Instance_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Instance, "vkDestroyInstance");

   type Uint_32_Ptr is access all Interfaces.Unsigned_32;

   type Physical_Device_Ptr is access all Physical_Device_T;

   function Enumerate_Physical_Devices
     (Instance               : Instance_T;
      Pphysical_Device_Count : Uint_32_Ptr;
      Pphysical_Devices      : Physical_Device_Ptr) return Result_T;
   pragma Import (Stdcall, Enumerate_Physical_Devices, "vkEnumeratePhysicalDevices");

   function Get_Device_Proc_Addr
     (Device : Device_T;
      Pname  : Interfaces.C.Strings.chars_ptr) return Pfn_Vk_Void_Function_T;
   pragma Import (Stdcall, Get_Device_Proc_Addr, "vkGetDeviceProcAddr");

   function Get_Instance_Proc_Addr
     (Instance : Instance_T;
      Pname    : Interfaces.C.Strings.chars_ptr) return Pfn_Vk_Void_Function_T;
   pragma Import (Stdcall, Get_Instance_Proc_Addr, "vkGetInstanceProcAddr");

   type Physical_Device_Properties_Ptr is access all Physical_Device_Properties_T;

   procedure Get_Physical_Device_Properties
     (Physical_Device : Physical_Device_T;
      Pproperties     : Physical_Device_Properties_Ptr);
   pragma Import (Stdcall, Get_Physical_Device_Properties, "vkGetPhysicalDeviceProperties");

   type Queue_Family_Properties_Ptr is access all Queue_Family_Properties_T;

   procedure Get_Physical_Device_Queue_Family_Properties
     (Physical_Device              : Physical_Device_T;
      Pqueue_Family_Property_Count : Uint_32_Ptr;
      Pqueue_Family_Properties     : Queue_Family_Properties_Ptr);
   pragma Import (Stdcall, Get_Physical_Device_Queue_Family_Properties, "vkGetPhysicalDeviceQueueFamilyProperties");

   type Physical_Device_Memory_Properties_Ptr is access all Physical_Device_Memory_Properties_T;

   procedure Get_Physical_Device_Memory_Properties
     (Physical_Device    : Physical_Device_T;
      Pmemory_Properties : Physical_Device_Memory_Properties_Ptr);
   pragma Import (Stdcall, Get_Physical_Device_Memory_Properties, "vkGetPhysicalDeviceMemoryProperties");

   type Physical_Device_Features_Ptr is access all Physical_Device_Features_T;

   procedure Get_Physical_Device_Features
     (Physical_Device : Physical_Device_T;
      Pfeatures       : Physical_Device_Features_Ptr);
   pragma Import (Stdcall, Get_Physical_Device_Features, "vkGetPhysicalDeviceFeatures");

   type Format_Properties_Ptr is access all Format_Properties_T;

   procedure Get_Physical_Device_Format_Properties
     (Physical_Device    : Physical_Device_T;
      Format             : Format_T;
      Pformat_Properties : Format_Properties_Ptr);
   pragma Import (Stdcall, Get_Physical_Device_Format_Properties, "vkGetPhysicalDeviceFormatProperties");

   type Image_Format_Properties_Ptr is access all Image_Format_Properties_T;

   function Get_Physical_Device_Image_Format_Properties
     (Physical_Device          : Physical_Device_T;
      Format                   : Format_T;
      The_Type                 : Image_Type_T;
      Tiling                   : Image_Tiling_T;
      Usage                    : Image_Usage_Flags_T;
      Flags                    : Image_Create_Flags_T;
      Pimage_Format_Properties : Image_Format_Properties_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Physical_Device_Image_Format_Properties, "vkGetPhysicalDeviceImageFormatProperties");

   type Device_Create_Info_Const_Ptr is access constant Device_Create_Info_T;

   type Device_Ptr is access all Device_T;

   function Create_Device
     (Physical_Device : Physical_Device_T;
      Pcreate_Info    : Device_Create_Info_Const_Ptr;
      Pallocator      : Allocation_Callbacks_Const_Ptr;
      Pdevice         : Device_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Device, "vkCreateDevice");

   procedure Destroy_Device (Device : Device_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Device, "vkDestroyDevice");

   type Layer_Properties_Ptr is access all Layer_Properties_T;

   function Enumerate_Instance_Layer_Properties
     (Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Layer_Properties_Ptr) return Result_T;
   pragma Import (Stdcall, Enumerate_Instance_Layer_Properties, "vkEnumerateInstanceLayerProperties");

   type Extension_Properties_Ptr is access all Extension_Properties_T;

   function Enumerate_Instance_Extension_Properties
     (Player_Name     : Interfaces.C.Strings.chars_ptr;
      Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Extension_Properties_Ptr) return Result_T;
   pragma Import (Stdcall, Enumerate_Instance_Extension_Properties, "vkEnumerateInstanceExtensionProperties");

   function Enumerate_Device_Layer_Properties
     (Physical_Device : Physical_Device_T;
      Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Layer_Properties_Ptr) return Result_T;
   pragma Import (Stdcall, Enumerate_Device_Layer_Properties, "vkEnumerateDeviceLayerProperties");

   function Enumerate_Device_Extension_Properties
     (Physical_Device : Physical_Device_T;
      Player_Name     : Interfaces.C.Strings.chars_ptr;
      Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Extension_Properties_Ptr) return Result_T;
   pragma Import (Stdcall, Enumerate_Device_Extension_Properties, "vkEnumerateDeviceExtensionProperties");

   type Queue_Ptr is access all Queue_T;

   procedure Get_Device_Queue
     (Device             : Device_T;
      Queue_Family_Index : Interfaces.Unsigned_32;
      Queue_Index        : Interfaces.Unsigned_32;
      Pqueue             : Queue_Ptr);
   pragma Import (Stdcall, Get_Device_Queue, "vkGetDeviceQueue");

   type Submit_Info_Const_Ptr is access constant Submit_Info_T;

   function Queue_Submit
     (Queue        : Queue_T;
      Submit_Count : Interfaces.Unsigned_32;
      Psubmits     : Submit_Info_Const_Ptr;
      Fence        : Fence_T) return Result_T;
   pragma Import (Stdcall, Queue_Submit, "vkQueueSubmit");

   function Queue_Wait_Idle (Queue : Queue_T) return Result_T;
   pragma Import (Stdcall, Queue_Wait_Idle, "vkQueueWaitIdle");

   function Device_Wait_Idle (Device : Device_T) return Result_T;
   pragma Import (Stdcall, Device_Wait_Idle, "vkDeviceWaitIdle");

   type Memory_Allocate_Info_Const_Ptr is access constant Memory_Allocate_Info_T;

   type Device_Memory_Ptr is access all Device_Memory_T;

   function Allocate_Memory
     (Device         : Device_T;
      Pallocate_Info : Memory_Allocate_Info_Const_Ptr;
      Pallocator     : Allocation_Callbacks_Const_Ptr;
      Pmemory        : Device_Memory_Ptr) return Result_T;
   pragma Import (Stdcall, Allocate_Memory, "vkAllocateMemory");

   procedure Free_Memory (Device : Device_T; Memory : Device_Memory_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Free_Memory, "vkFreeMemory");

   function Map_Memory
     (Device  : Device_T;
      Memory  : Device_Memory_T;
      Offset  : Device_Size_T;
      Size    : Device_Size_T;
      Flags   : Memory_Map_Flags_T;
      Pp_Data : Void_Ptr) return Result_T;
   pragma Import (Stdcall, Map_Memory, "vkMapMemory");

   procedure Unmap_Memory (Device : Device_T; Memory : Device_Memory_T);
   pragma Import (Stdcall, Unmap_Memory, "vkUnmapMemory");

   type Mapped_Memory_Range_Const_Ptr is access constant Mapped_Memory_Range_T;

   function Flush_Mapped_Memory_Ranges
     (Device             : Device_T;
      Memory_Range_Count : Interfaces.Unsigned_32;
      Pmemory_Ranges     : Mapped_Memory_Range_Const_Ptr) return Result_T;
   pragma Import (Stdcall, Flush_Mapped_Memory_Ranges, "vkFlushMappedMemoryRanges");

   function Invalidate_Mapped_Memory_Ranges
     (Device             : Device_T;
      Memory_Range_Count : Interfaces.Unsigned_32;
      Pmemory_Ranges     : Mapped_Memory_Range_Const_Ptr) return Result_T;
   pragma Import (Stdcall, Invalidate_Mapped_Memory_Ranges, "vkInvalidateMappedMemoryRanges");

   type Device_Size_Ptr is access all Device_Size_T;

   procedure Get_Device_Memory_Commitment
     (Device                     : Device_T;
      Memory                     : Device_Memory_T;
      Pcommitted_Memory_In_Bytes : Device_Size_Ptr);
   pragma Import (Stdcall, Get_Device_Memory_Commitment, "vkGetDeviceMemoryCommitment");

   type Memory_Requirements_Ptr is access all Memory_Requirements_T;

   procedure Get_Buffer_Memory_Requirements
     (Device               : Device_T;
      Buffer               : Buffer_T;
      Pmemory_Requirements : Memory_Requirements_Ptr);
   pragma Import (Stdcall, Get_Buffer_Memory_Requirements, "vkGetBufferMemoryRequirements");

   function Bind_Buffer_Memory
     (Device        : Device_T;
      Buffer        : Buffer_T;
      Memory        : Device_Memory_T;
      Memory_Offset : Device_Size_T) return Result_T;
   pragma Import (Stdcall, Bind_Buffer_Memory, "vkBindBufferMemory");

   procedure Get_Image_Memory_Requirements
     (Device               : Device_T;
      Image                : Image_T;
      Pmemory_Requirements : Memory_Requirements_Ptr);
   pragma Import (Stdcall, Get_Image_Memory_Requirements, "vkGetImageMemoryRequirements");

   function Bind_Image_Memory
     (Device        : Device_T;
      Image         : Image_T;
      Memory        : Device_Memory_T;
      Memory_Offset : Device_Size_T) return Result_T;
   pragma Import (Stdcall, Bind_Image_Memory, "vkBindImageMemory");

   type Sparse_Image_Memory_Requirements_Ptr is access all Sparse_Image_Memory_Requirements_T;

   procedure Get_Image_Sparse_Memory_Requirements
     (Device                           : Device_T;
      Image                            : Image_T;
      Psparse_Memory_Requirement_Count : Uint_32_Ptr;
      Psparse_Memory_Requirements      : Sparse_Image_Memory_Requirements_Ptr);
   pragma Import (Stdcall, Get_Image_Sparse_Memory_Requirements, "vkGetImageSparseMemoryRequirements");

   type Sparse_Image_Format_Properties_Ptr is access all Sparse_Image_Format_Properties_T;

   procedure Get_Physical_Device_Sparse_Image_Format_Properties
     (Physical_Device : Physical_Device_T;
      Format          : Format_T;
      The_Type        : Image_Type_T;
      Samples         : Sample_Count_Flag_Bits_T;
      Usage           : Image_Usage_Flags_T;
      Tiling          : Image_Tiling_T;
      Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Sparse_Image_Format_Properties_Ptr);
   pragma Import
     (Stdcall,
      Get_Physical_Device_Sparse_Image_Format_Properties,
      "vkGetPhysicalDeviceSparseImageFormatProperties");

   type Bind_Sparse_Info_Const_Ptr is access constant Bind_Sparse_Info_T;

   function Queue_Bind_Sparse
     (Queue           : Queue_T;
      Bind_Info_Count : Interfaces.Unsigned_32;
      Pbind_Info      : Bind_Sparse_Info_Const_Ptr;
      Fence           : Fence_T) return Result_T;
   pragma Import (Stdcall, Queue_Bind_Sparse, "vkQueueBindSparse");

   type Fence_Create_Info_Const_Ptr is access constant Fence_Create_Info_T;

   type Fence_Ptr is access all Fence_T;

   function Create_Fence
     (Device       : Device_T;
      Pcreate_Info : Fence_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pfence       : Fence_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Fence, "vkCreateFence");

   procedure Destroy_Fence (Device : Device_T; Fence : Fence_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Fence, "vkDestroyFence");

   type Fence_Const_Ptr is access constant Fence_T;

   function Reset_Fences
     (Device      : Device_T;
      Fence_Count : Interfaces.Unsigned_32;
      Pfences     : Fence_Const_Ptr) return Result_T;
   pragma Import (Stdcall, Reset_Fences, "vkResetFences");

   function Get_Fence_Status (Device : Device_T; Fence : Fence_T) return Result_T;
   pragma Import (Stdcall, Get_Fence_Status, "vkGetFenceStatus");

   function Wait_For_Fences
     (Device      : Device_T;
      Fence_Count : Interfaces.Unsigned_32;
      Pfences     : Fence_Const_Ptr;
      Wait_All    : Bool_32_T;
      Timeout     : Interfaces.Unsigned_64) return Result_T;
   pragma Import (Stdcall, Wait_For_Fences, "vkWaitForFences");

   type Semaphore_Create_Info_Const_Ptr is access constant Semaphore_Create_Info_T;

   type Semaphore_Ptr is access all Semaphore_T;

   function Create_Semaphore
     (Device       : Device_T;
      Pcreate_Info : Semaphore_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Psemaphore   : Semaphore_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Semaphore, "vkCreateSemaphore");

   procedure Destroy_Semaphore
     (Device     : Device_T;
      Semaphore  : Semaphore_T;
      Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Semaphore, "vkDestroySemaphore");

   type Event_Create_Info_Const_Ptr is access constant Event_Create_Info_T;

   type Event_Ptr is access all Event_T;

   function Create_Event
     (Device       : Device_T;
      Pcreate_Info : Event_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pevent       : Event_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Event, "vkCreateEvent");

   procedure Destroy_Event (Device : Device_T; Event : Event_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Event, "vkDestroyEvent");

   function Get_Event_Status (Device : Device_T; Event : Event_T) return Result_T;
   pragma Import (Stdcall, Get_Event_Status, "vkGetEventStatus");

   function Set_Event (Device : Device_T; Event : Event_T) return Result_T;
   pragma Import (Stdcall, Set_Event, "vkSetEvent");

   function Reset_Event (Device : Device_T; Event : Event_T) return Result_T;
   pragma Import (Stdcall, Reset_Event, "vkResetEvent");

   type Query_Pool_Create_Info_Const_Ptr is access constant Query_Pool_Create_Info_T;

   type Query_Pool_Ptr is access all Query_Pool_T;

   function Create_Query_Pool
     (Device       : Device_T;
      Pcreate_Info : Query_Pool_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pquery_Pool  : Query_Pool_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Query_Pool, "vkCreateQueryPool");

   procedure Destroy_Query_Pool
     (Device     : Device_T;
      Query_Pool : Query_Pool_T;
      Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Query_Pool, "vkDestroyQueryPool");

   function Get_Query_Pool_Results
     (Device      : Device_T;
      Query_Pool  : Query_Pool_T;
      First_Query : Interfaces.Unsigned_32;
      Query_Count : Interfaces.Unsigned_32;
      Data_Size   : Interfaces.C.size_t;
      Pdata       : Void_Ptr;
      Stride      : Device_Size_T;
      Flags       : Query_Result_Flags_T) return Result_T;
   pragma Import (Stdcall, Get_Query_Pool_Results, "vkGetQueryPoolResults");

   type Buffer_Create_Info_Const_Ptr is access constant Buffer_Create_Info_T;

   type Buffer_Ptr is access all Buffer_T;

   function Create_Buffer
     (Device       : Device_T;
      Pcreate_Info : Buffer_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pbuffer      : Buffer_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Buffer, "vkCreateBuffer");

   procedure Destroy_Buffer (Device : Device_T; Buffer : Buffer_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Buffer, "vkDestroyBuffer");

   type Buffer_View_Create_Info_Const_Ptr is access constant Buffer_View_Create_Info_T;

   type Buffer_View_Ptr is access all Buffer_View_T;

   function Create_Buffer_View
     (Device       : Device_T;
      Pcreate_Info : Buffer_View_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pview        : Buffer_View_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Buffer_View, "vkCreateBufferView");

   procedure Destroy_Buffer_View
     (Device      : Device_T;
      Buffer_View : Buffer_View_T;
      Pallocator  : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Buffer_View, "vkDestroyBufferView");

   type Image_Create_Info_Const_Ptr is access constant Image_Create_Info_T;

   type Image_Ptr is access all Image_T;

   function Create_Image
     (Device       : Device_T;
      Pcreate_Info : Image_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pimage       : Image_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Image, "vkCreateImage");

   procedure Destroy_Image (Device : Device_T; Image : Image_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Image, "vkDestroyImage");

   type Image_Subresource_Const_Ptr is access constant Image_Subresource_T;

   type Subresource_Layout_Ptr is access all Subresource_Layout_T;

   procedure Get_Image_Subresource_Layout
     (Device       : Device_T;
      Image        : Image_T;
      Psubresource : Image_Subresource_Const_Ptr;
      Playout      : Subresource_Layout_Ptr);
   pragma Import (Stdcall, Get_Image_Subresource_Layout, "vkGetImageSubresourceLayout");

   type Image_View_Create_Info_Const_Ptr is access constant Image_View_Create_Info_T;

   type Image_View_Ptr is access all Image_View_T;

   function Create_Image_View
     (Device       : Device_T;
      Pcreate_Info : Image_View_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pview        : Image_View_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Image_View, "vkCreateImageView");

   procedure Destroy_Image_View
     (Device     : Device_T;
      Image_View : Image_View_T;
      Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Image_View, "vkDestroyImageView");

   type Shader_Module_Create_Info_Const_Ptr is access constant Shader_Module_Create_Info_T;

   type Shader_Module_Ptr is access all Shader_Module_T;

   function Create_Shader_Module
     (Device         : Device_T;
      Pcreate_Info   : Shader_Module_Create_Info_Const_Ptr;
      Pallocator     : Allocation_Callbacks_Const_Ptr;
      Pshader_Module : Shader_Module_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Shader_Module, "vkCreateShaderModule");

   procedure Destroy_Shader_Module
     (Device        : Device_T;
      Shader_Module : Shader_Module_T;
      Pallocator    : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Shader_Module, "vkDestroyShaderModule");

   type Pipeline_Cache_Create_Info_Const_Ptr is access constant Pipeline_Cache_Create_Info_T;

   type Pipeline_Cache_Ptr is access all Pipeline_Cache_T;

   function Create_Pipeline_Cache
     (Device          : Device_T;
      Pcreate_Info    : Pipeline_Cache_Create_Info_Const_Ptr;
      Pallocator      : Allocation_Callbacks_Const_Ptr;
      Ppipeline_Cache : Pipeline_Cache_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Pipeline_Cache, "vkCreatePipelineCache");

   procedure Destroy_Pipeline_Cache
     (Device         : Device_T;
      Pipeline_Cache : Pipeline_Cache_T;
      Pallocator     : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Pipeline_Cache, "vkDestroyPipelineCache");

   type Size_Ptr is access all Interfaces.C.size_t;

   function Get_Pipeline_Cache_Data
     (Device         : Device_T;
      Pipeline_Cache : Pipeline_Cache_T;
      Pdata_Size     : Size_Ptr;
      Pdata          : Void_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Pipeline_Cache_Data, "vkGetPipelineCacheData");

   type Pipeline_Cache_Const_Ptr is access constant Pipeline_Cache_T;

   function Merge_Pipeline_Caches
     (Device          : Device_T;
      Dst_Cache       : Pipeline_Cache_T;
      Src_Cache_Count : Interfaces.Unsigned_32;
      Psrc_Caches     : Pipeline_Cache_Const_Ptr) return Result_T;
   pragma Import (Stdcall, Merge_Pipeline_Caches, "vkMergePipelineCaches");

   type Graphics_Pipeline_Create_Info_Const_Ptr is access constant Graphics_Pipeline_Create_Info_T;

   type Pipeline_Ptr is access all Pipeline_T;

   function Create_Graphics_Pipelines
     (Device            : Device_T;
      Pipeline_Cache    : Pipeline_Cache_T;
      Create_Info_Count : Interfaces.Unsigned_32;
      Pcreate_Infos     : Graphics_Pipeline_Create_Info_Const_Ptr;
      Pallocator        : Allocation_Callbacks_Const_Ptr;
      Ppipelines        : Pipeline_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Graphics_Pipelines, "vkCreateGraphicsPipelines");

   type Compute_Pipeline_Create_Info_Const_Ptr is access constant Compute_Pipeline_Create_Info_T;

   function Create_Compute_Pipelines
     (Device            : Device_T;
      Pipeline_Cache    : Pipeline_Cache_T;
      Create_Info_Count : Interfaces.Unsigned_32;
      Pcreate_Infos     : Compute_Pipeline_Create_Info_Const_Ptr;
      Pallocator        : Allocation_Callbacks_Const_Ptr;
      Ppipelines        : Pipeline_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Compute_Pipelines, "vkCreateComputePipelines");

   procedure Destroy_Pipeline (Device : Device_T; Pipeline : Pipeline_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Pipeline, "vkDestroyPipeline");

   type Pipeline_Layout_Create_Info_Const_Ptr is access constant Pipeline_Layout_Create_Info_T;

   type Pipeline_Layout_Ptr is access all Pipeline_Layout_T;

   function Create_Pipeline_Layout
     (Device           : Device_T;
      Pcreate_Info     : Pipeline_Layout_Create_Info_Const_Ptr;
      Pallocator       : Allocation_Callbacks_Const_Ptr;
      Ppipeline_Layout : Pipeline_Layout_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Pipeline_Layout, "vkCreatePipelineLayout");

   procedure Destroy_Pipeline_Layout
     (Device          : Device_T;
      Pipeline_Layout : Pipeline_Layout_T;
      Pallocator      : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Pipeline_Layout, "vkDestroyPipelineLayout");

   type Sampler_Create_Info_Const_Ptr is access constant Sampler_Create_Info_T;

   type Sampler_Ptr is access all Sampler_T;

   function Create_Sampler
     (Device       : Device_T;
      Pcreate_Info : Sampler_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Psampler     : Sampler_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Sampler, "vkCreateSampler");

   procedure Destroy_Sampler (Device : Device_T; Sampler : Sampler_T; Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Sampler, "vkDestroySampler");

   type Descriptor_Set_Layout_Create_Info_Const_Ptr is access constant Descriptor_Set_Layout_Create_Info_T;

   type Descriptor_Set_Layout_Ptr is access all Descriptor_Set_Layout_T;

   function Create_Descriptor_Set_Layout
     (Device       : Device_T;
      Pcreate_Info : Descriptor_Set_Layout_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pset_Layout  : Descriptor_Set_Layout_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Descriptor_Set_Layout, "vkCreateDescriptorSetLayout");

   procedure Destroy_Descriptor_Set_Layout
     (Device                : Device_T;
      Descriptor_Set_Layout : Descriptor_Set_Layout_T;
      Pallocator            : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Descriptor_Set_Layout, "vkDestroyDescriptorSetLayout");

   type Descriptor_Pool_Create_Info_Const_Ptr is access constant Descriptor_Pool_Create_Info_T;

   type Descriptor_Pool_Ptr is access all Descriptor_Pool_T;

   function Create_Descriptor_Pool
     (Device           : Device_T;
      Pcreate_Info     : Descriptor_Pool_Create_Info_Const_Ptr;
      Pallocator       : Allocation_Callbacks_Const_Ptr;
      Pdescriptor_Pool : Descriptor_Pool_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Descriptor_Pool, "vkCreateDescriptorPool");

   procedure Destroy_Descriptor_Pool
     (Device          : Device_T;
      Descriptor_Pool : Descriptor_Pool_T;
      Pallocator      : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Descriptor_Pool, "vkDestroyDescriptorPool");

   function Reset_Descriptor_Pool
     (Device          : Device_T;
      Descriptor_Pool : Descriptor_Pool_T;
      Flags           : Descriptor_Pool_Reset_Flags_T) return Result_T;
   pragma Import (Stdcall, Reset_Descriptor_Pool, "vkResetDescriptorPool");

   type Descriptor_Set_Allocate_Info_Const_Ptr is access constant Descriptor_Set_Allocate_Info_T;

   type Descriptor_Set_Ptr is access all Descriptor_Set_T;

   function Allocate_Descriptor_Sets
     (Device           : Device_T;
      Pallocate_Info   : Descriptor_Set_Allocate_Info_Const_Ptr;
      Pdescriptor_Sets : Descriptor_Set_Ptr) return Result_T;
   pragma Import (Stdcall, Allocate_Descriptor_Sets, "vkAllocateDescriptorSets");

   type Descriptor_Set_Const_Ptr is access constant Descriptor_Set_T;

   function Free_Descriptor_Sets
     (Device               : Device_T;
      Descriptor_Pool      : Descriptor_Pool_T;
      Descriptor_Set_Count : Interfaces.Unsigned_32;
      Pdescriptor_Sets     : Descriptor_Set_Const_Ptr) return Result_T;
   pragma Import (Stdcall, Free_Descriptor_Sets, "vkFreeDescriptorSets");

   type Write_Descriptor_Set_Const_Ptr is access constant Write_Descriptor_Set_T;

   type Copy_Descriptor_Set_Const_Ptr is access constant Copy_Descriptor_Set_T;

   procedure Update_Descriptor_Sets
     (Device                 : Device_T;
      Descriptor_Write_Count : Interfaces.Unsigned_32;
      Pdescriptor_Writes     : Write_Descriptor_Set_Const_Ptr;
      Descriptor_Copy_Count  : Interfaces.Unsigned_32;
      Pdescriptor_Copies     : Copy_Descriptor_Set_Const_Ptr);
   pragma Import (Stdcall, Update_Descriptor_Sets, "vkUpdateDescriptorSets");

   type Framebuffer_Create_Info_Const_Ptr is access constant Framebuffer_Create_Info_T;

   type Framebuffer_Ptr is access all Framebuffer_T;

   function Create_Framebuffer
     (Device       : Device_T;
      Pcreate_Info : Framebuffer_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pframebuffer : Framebuffer_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Framebuffer, "vkCreateFramebuffer");

   procedure Destroy_Framebuffer
     (Device      : Device_T;
      Framebuffer : Framebuffer_T;
      Pallocator  : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Framebuffer, "vkDestroyFramebuffer");

   type Render_Pass_Create_Info_Const_Ptr is access constant Render_Pass_Create_Info_T;

   type Render_Pass_Ptr is access all Render_Pass_T;

   function Create_Render_Pass
     (Device       : Device_T;
      Pcreate_Info : Render_Pass_Create_Info_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Prender_Pass : Render_Pass_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Render_Pass, "vkCreateRenderPass");

   procedure Destroy_Render_Pass
     (Device      : Device_T;
      Render_Pass : Render_Pass_T;
      Pallocator  : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Render_Pass, "vkDestroyRenderPass");

   type Extent_2D_Ptr is access all Extent_2D_T;

   procedure Get_Render_Area_Granularity (Device : Device_T; Render_Pass : Render_Pass_T; Pgranularity : Extent_2D_Ptr);
   pragma Import (Stdcall, Get_Render_Area_Granularity, "vkGetRenderAreaGranularity");

   type Command_Pool_Create_Info_Const_Ptr is access constant Command_Pool_Create_Info_T;

   type Command_Pool_Ptr is access all Command_Pool_T;

   function Create_Command_Pool
     (Device        : Device_T;
      Pcreate_Info  : Command_Pool_Create_Info_Const_Ptr;
      Pallocator    : Allocation_Callbacks_Const_Ptr;
      Pcommand_Pool : Command_Pool_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Command_Pool, "vkCreateCommandPool");

   procedure Destroy_Command_Pool
     (Device       : Device_T;
      Command_Pool : Command_Pool_T;
      Pallocator   : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Command_Pool, "vkDestroyCommandPool");

   function Reset_Command_Pool
     (Device       : Device_T;
      Command_Pool : Command_Pool_T;
      Flags        : Command_Pool_Reset_Flags_T) return Result_T;
   pragma Import (Stdcall, Reset_Command_Pool, "vkResetCommandPool");

   type Command_Buffer_Allocate_Info_Const_Ptr is access constant Command_Buffer_Allocate_Info_T;

   type Command_Buffer_Ptr is access all Command_Buffer_T;

   function Allocate_Command_Buffers
     (Device           : Device_T;
      Pallocate_Info   : Command_Buffer_Allocate_Info_Const_Ptr;
      Pcommand_Buffers : Command_Buffer_Ptr) return Result_T;
   pragma Import (Stdcall, Allocate_Command_Buffers, "vkAllocateCommandBuffers");

   procedure Free_Command_Buffers
     (Device               : Device_T;
      Command_Pool         : Command_Pool_T;
      Command_Buffer_Count : Interfaces.Unsigned_32;
      Pcommand_Buffers     : Command_Buffer_Const_Ptr);
   pragma Import (Stdcall, Free_Command_Buffers, "vkFreeCommandBuffers");

   type Command_Buffer_Begin_Info_Const_Ptr is access constant Command_Buffer_Begin_Info_T;

   function Begin_Command_Buffer
     (Command_Buffer : Command_Buffer_T;
      Pbegin_Info    : Command_Buffer_Begin_Info_Const_Ptr) return Result_T;
   pragma Import (Stdcall, Begin_Command_Buffer, "vkBeginCommandBuffer");

   function End_Command_Buffer (Command_Buffer : Command_Buffer_T) return Result_T;
   pragma Import (Stdcall, End_Command_Buffer, "vkEndCommandBuffer");

   function Reset_Command_Buffer
     (Command_Buffer : Command_Buffer_T;
      Flags          : Command_Buffer_Reset_Flags_T) return Result_T;
   pragma Import (Stdcall, Reset_Command_Buffer, "vkResetCommandBuffer");

   procedure Cmd_Bind_Pipeline
     (Command_Buffer      : Command_Buffer_T;
      Pipeline_Bind_Point : Pipeline_Bind_Point_T;
      Pipeline            : Pipeline_T);
   pragma Import (Stdcall, Cmd_Bind_Pipeline, "vkCmdBindPipeline");

   procedure Cmd_Set_Viewport
     (Command_Buffer : Command_Buffer_T;
      First_Viewport : Interfaces.Unsigned_32;
      Viewport_Count : Interfaces.Unsigned_32;
      Pviewports     : Viewport_Const_Ptr);
   pragma Import (Stdcall, Cmd_Set_Viewport, "vkCmdSetViewport");

   procedure Cmd_Set_Scissor
     (Command_Buffer : Command_Buffer_T;
      First_Scissor  : Interfaces.Unsigned_32;
      Scissor_Count  : Interfaces.Unsigned_32;
      Pscissors      : Rect_2D_Const_Ptr);
   pragma Import (Stdcall, Cmd_Set_Scissor, "vkCmdSetScissor");

   procedure Cmd_Set_Line_Width (Command_Buffer : Command_Buffer_T; Line_Width : Interfaces.C.C_float);
   pragma Import (Stdcall, Cmd_Set_Line_Width, "vkCmdSetLineWidth");

   procedure Cmd_Set_Depth_Bias
     (Command_Buffer             : Command_Buffer_T;
      Depth_Bias_Constant_Factor : Interfaces.C.C_float;
      Depth_Bias_Clamp           : Interfaces.C.C_float;
      Depth_Bias_Slope_Factor    : Interfaces.C.C_float);
   pragma Import (Stdcall, Cmd_Set_Depth_Bias, "vkCmdSetDepthBias");

   procedure Cmd_Set_Blend_Constants (Command_Buffer : Command_Buffer_T; Blend_Constants : Blend_Constants_Array_T);
   pragma Import (Stdcall, Cmd_Set_Blend_Constants, "vkCmdSetBlendConstants");

   procedure Cmd_Set_Depth_Bounds
     (Command_Buffer   : Command_Buffer_T;
      Min_Depth_Bounds : Interfaces.C.C_float;
      Max_Depth_Bounds : Interfaces.C.C_float);
   pragma Import (Stdcall, Cmd_Set_Depth_Bounds, "vkCmdSetDepthBounds");

   procedure Cmd_Set_Stencil_Compare_Mask
     (Command_Buffer : Command_Buffer_T;
      Face_Mask      : Stencil_Face_Flags_T;
      Compare_Mask   : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Set_Stencil_Compare_Mask, "vkCmdSetStencilCompareMask");

   procedure Cmd_Set_Stencil_Write_Mask
     (Command_Buffer : Command_Buffer_T;
      Face_Mask      : Stencil_Face_Flags_T;
      Write_Mask     : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Set_Stencil_Write_Mask, "vkCmdSetStencilWriteMask");

   procedure Cmd_Set_Stencil_Reference
     (Command_Buffer : Command_Buffer_T;
      Face_Mask      : Stencil_Face_Flags_T;
      Reference      : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Set_Stencil_Reference, "vkCmdSetStencilReference");

   procedure Cmd_Bind_Descriptor_Sets
     (Command_Buffer       : Command_Buffer_T;
      Pipeline_Bind_Point  : Pipeline_Bind_Point_T;
      Layout               : Pipeline_Layout_T;
      First_Set            : Interfaces.Unsigned_32;
      Descriptor_Set_Count : Interfaces.Unsigned_32;
      Pdescriptor_Sets     : Descriptor_Set_Const_Ptr;
      Dynamic_Offset_Count : Interfaces.Unsigned_32;
      Pdynamic_Offsets     : Uint_32_Const_Ptr);
   pragma Import (Stdcall, Cmd_Bind_Descriptor_Sets, "vkCmdBindDescriptorSets");

   procedure Cmd_Bind_Index_Buffer
     (Command_Buffer : Command_Buffer_T;
      Buffer         : Buffer_T;
      Offset         : Device_Size_T;
      Index_Type     : Index_Type_T);
   pragma Import (Stdcall, Cmd_Bind_Index_Buffer, "vkCmdBindIndexBuffer");

   type Buffer_Const_Ptr is access constant Buffer_T;

   type Device_Size_Const_Ptr is access constant Device_Size_T;

   procedure Cmd_Bind_Vertex_Buffers
     (Command_Buffer : Command_Buffer_T;
      First_Binding  : Interfaces.Unsigned_32;
      Binding_Count  : Interfaces.Unsigned_32;
      Pbuffers       : Buffer_Const_Ptr;
      Poffsets       : Device_Size_Const_Ptr);
   pragma Import (Stdcall, Cmd_Bind_Vertex_Buffers, "vkCmdBindVertexBuffers");

   procedure Cmd_Draw
     (Command_Buffer : Command_Buffer_T;
      Vertex_Count   : Interfaces.Unsigned_32;
      Instance_Count : Interfaces.Unsigned_32;
      First_Vertex   : Interfaces.Unsigned_32;
      First_Instance : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Draw, "vkCmdDraw");

   procedure Cmd_Draw_Indexed
     (Command_Buffer : Command_Buffer_T;
      Index_Count    : Interfaces.Unsigned_32;
      Instance_Count : Interfaces.Unsigned_32;
      First_Index    : Interfaces.Unsigned_32;
      Vertex_Offset  : Interfaces.Integer_32;
      First_Instance : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Draw_Indexed, "vkCmdDrawIndexed");

   procedure Cmd_Draw_Indirect
     (Command_Buffer : Command_Buffer_T;
      Buffer         : Buffer_T;
      Offset         : Device_Size_T;
      Draw_Count     : Interfaces.Unsigned_32;
      Stride         : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Draw_Indirect, "vkCmdDrawIndirect");

   procedure Cmd_Draw_Indexed_Indirect
     (Command_Buffer : Command_Buffer_T;
      Buffer         : Buffer_T;
      Offset         : Device_Size_T;
      Draw_Count     : Interfaces.Unsigned_32;
      Stride         : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Draw_Indexed_Indirect, "vkCmdDrawIndexedIndirect");

   procedure Cmd_Dispatch
     (Command_Buffer : Command_Buffer_T;
      X              : Interfaces.Unsigned_32;
      Y              : Interfaces.Unsigned_32;
      Z              : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Dispatch, "vkCmdDispatch");

   procedure Cmd_Dispatch_Indirect (Command_Buffer : Command_Buffer_T; Buffer : Buffer_T; Offset : Device_Size_T);
   pragma Import (Stdcall, Cmd_Dispatch_Indirect, "vkCmdDispatchIndirect");

   type Buffer_Copy_Const_Ptr is access constant Buffer_Copy_T;

   procedure Cmd_Copy_Buffer
     (Command_Buffer : Command_Buffer_T;
      Src_Buffer     : Buffer_T;
      Dst_Buffer     : Buffer_T;
      Region_Count   : Interfaces.Unsigned_32;
      Pregions       : Buffer_Copy_Const_Ptr);
   pragma Import (Stdcall, Cmd_Copy_Buffer, "vkCmdCopyBuffer");

   type Image_Copy_Const_Ptr is access constant Image_Copy_T;

   procedure Cmd_Copy_Image
     (Command_Buffer   : Command_Buffer_T;
      Src_Image        : Image_T;
      Src_Image_Layout : Image_Layout_T;
      Dst_Image        : Image_T;
      Dst_Image_Layout : Image_Layout_T;
      Region_Count     : Interfaces.Unsigned_32;
      Pregions         : Image_Copy_Const_Ptr);
   pragma Import (Stdcall, Cmd_Copy_Image, "vkCmdCopyImage");

   type Image_Blit_Const_Ptr is access constant Image_Blit_T;

   procedure Cmd_Blit_Image
     (Command_Buffer   : Command_Buffer_T;
      Src_Image        : Image_T;
      Src_Image_Layout : Image_Layout_T;
      Dst_Image        : Image_T;
      Dst_Image_Layout : Image_Layout_T;
      Region_Count     : Interfaces.Unsigned_32;
      Pregions         : Image_Blit_Const_Ptr;
      Filter           : Filter_T);
   pragma Import (Stdcall, Cmd_Blit_Image, "vkCmdBlitImage");

   type Buffer_Image_Copy_Const_Ptr is access constant Buffer_Image_Copy_T;

   procedure Cmd_Copy_Buffer_To_Image
     (Command_Buffer   : Command_Buffer_T;
      Src_Buffer       : Buffer_T;
      Dst_Image        : Image_T;
      Dst_Image_Layout : Image_Layout_T;
      Region_Count     : Interfaces.Unsigned_32;
      Pregions         : Buffer_Image_Copy_Const_Ptr);
   pragma Import (Stdcall, Cmd_Copy_Buffer_To_Image, "vkCmdCopyBufferToImage");

   procedure Cmd_Copy_Image_To_Buffer
     (Command_Buffer   : Command_Buffer_T;
      Src_Image        : Image_T;
      Src_Image_Layout : Image_Layout_T;
      Dst_Buffer       : Buffer_T;
      Region_Count     : Interfaces.Unsigned_32;
      Pregions         : Buffer_Image_Copy_Const_Ptr);
   pragma Import (Stdcall, Cmd_Copy_Image_To_Buffer, "vkCmdCopyImageToBuffer");

   procedure Cmd_Update_Buffer
     (Command_Buffer : Command_Buffer_T;
      Dst_Buffer     : Buffer_T;
      Dst_Offset     : Device_Size_T;
      Data_Size      : Device_Size_T;
      Pdata          : Uint_32_Const_Ptr);
   pragma Import (Stdcall, Cmd_Update_Buffer, "vkCmdUpdateBuffer");

   procedure Cmd_Fill_Buffer
     (Command_Buffer : Command_Buffer_T;
      Dst_Buffer     : Buffer_T;
      Dst_Offset     : Device_Size_T;
      Size           : Device_Size_T;
      Data           : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Fill_Buffer, "vkCmdFillBuffer");

   type Clear_Color_Value_Const_Ptr is access constant Clear_Color_Value_T;

   type Image_Subresource_Range_Const_Ptr is access constant Image_Subresource_Range_T;

   procedure Cmd_Clear_Color_Image
     (Command_Buffer : Command_Buffer_T;
      Image          : Image_T;
      Image_Layout   : Image_Layout_T;
      Pcolor         : Clear_Color_Value_Const_Ptr;
      Range_Count    : Interfaces.Unsigned_32;
      Pranges        : Image_Subresource_Range_Const_Ptr);
   pragma Import (Stdcall, Cmd_Clear_Color_Image, "vkCmdClearColorImage");

   type Clear_Depth_Stencil_Value_Const_Ptr is access constant Clear_Depth_Stencil_Value_T;

   procedure Cmd_Clear_Depth_Stencil_Image
     (Command_Buffer : Command_Buffer_T;
      Image          : Image_T;
      Image_Layout   : Image_Layout_T;
      Pdepth_Stencil : Clear_Depth_Stencil_Value_Const_Ptr;
      Range_Count    : Interfaces.Unsigned_32;
      Pranges        : Image_Subresource_Range_Const_Ptr);
   pragma Import (Stdcall, Cmd_Clear_Depth_Stencil_Image, "vkCmdClearDepthStencilImage");

   type Clear_Attachment_Const_Ptr is access constant Clear_Attachment_T;

   type Clear_Rect_Const_Ptr is access constant Clear_Rect_T;

   procedure Cmd_Clear_Attachments
     (Command_Buffer   : Command_Buffer_T;
      Attachment_Count : Interfaces.Unsigned_32;
      Pattachments     : Clear_Attachment_Const_Ptr;
      Rect_Count       : Interfaces.Unsigned_32;
      Prects           : Clear_Rect_Const_Ptr);
   pragma Import (Stdcall, Cmd_Clear_Attachments, "vkCmdClearAttachments");

   type Image_Resolve_Const_Ptr is access constant Image_Resolve_T;

   procedure Cmd_Resolve_Image
     (Command_Buffer   : Command_Buffer_T;
      Src_Image        : Image_T;
      Src_Image_Layout : Image_Layout_T;
      Dst_Image        : Image_T;
      Dst_Image_Layout : Image_Layout_T;
      Region_Count     : Interfaces.Unsigned_32;
      Pregions         : Image_Resolve_Const_Ptr);
   pragma Import (Stdcall, Cmd_Resolve_Image, "vkCmdResolveImage");

   procedure Cmd_Set_Event (Command_Buffer : Command_Buffer_T; Event : Event_T; Stage_Mask : Pipeline_Stage_Flags_T);
   pragma Import (Stdcall, Cmd_Set_Event, "vkCmdSetEvent");

   procedure Cmd_Reset_Event (Command_Buffer : Command_Buffer_T; Event : Event_T; Stage_Mask : Pipeline_Stage_Flags_T);
   pragma Import (Stdcall, Cmd_Reset_Event, "vkCmdResetEvent");

   type Event_Const_Ptr is access constant Event_T;

   type Memory_Barrier_Const_Ptr is access constant Memory_Barrier_T;

   type Buffer_Memory_Barrier_Const_Ptr is access constant Buffer_Memory_Barrier_T;

   type Image_Memory_Barrier_Const_Ptr is access constant Image_Memory_Barrier_T;

   procedure Cmd_Wait_Events
     (Command_Buffer              : Command_Buffer_T;
      Event_Count                 : Interfaces.Unsigned_32;
      Pevents                     : Event_Const_Ptr;
      Src_Stage_Mask              : Pipeline_Stage_Flags_T;
      Dst_Stage_Mask              : Pipeline_Stage_Flags_T;
      Memory_Barrier_Count        : Interfaces.Unsigned_32;
      Pmemory_Barriers            : Memory_Barrier_Const_Ptr;
      Buffer_Memory_Barrier_Count : Interfaces.Unsigned_32;
      Pbuffer_Memory_Barriers     : Buffer_Memory_Barrier_Const_Ptr;
      Image_Memory_Barrier_Count  : Interfaces.Unsigned_32;
      Pimage_Memory_Barriers      : Image_Memory_Barrier_Const_Ptr);
   pragma Import (Stdcall, Cmd_Wait_Events, "vkCmdWaitEvents");

   procedure Cmd_Pipeline_Barrier
     (Command_Buffer              : Command_Buffer_T;
      Src_Stage_Mask              : Pipeline_Stage_Flags_T;
      Dst_Stage_Mask              : Pipeline_Stage_Flags_T;
      Dependency_Flags            : Dependency_Flags_T;
      Memory_Barrier_Count        : Interfaces.Unsigned_32;
      Pmemory_Barriers            : Memory_Barrier_Const_Ptr;
      Buffer_Memory_Barrier_Count : Interfaces.Unsigned_32;
      Pbuffer_Memory_Barriers     : Buffer_Memory_Barrier_Const_Ptr;
      Image_Memory_Barrier_Count  : Interfaces.Unsigned_32;
      Pimage_Memory_Barriers      : Image_Memory_Barrier_Const_Ptr);
   pragma Import (Stdcall, Cmd_Pipeline_Barrier, "vkCmdPipelineBarrier");

   procedure Cmd_Begin_Query
     (Command_Buffer : Command_Buffer_T;
      Query_Pool     : Query_Pool_T;
      Query          : Interfaces.Unsigned_32;
      Flags          : Query_Control_Flags_T);
   pragma Import (Stdcall, Cmd_Begin_Query, "vkCmdBeginQuery");

   procedure Cmd_End_Query
     (Command_Buffer : Command_Buffer_T;
      Query_Pool     : Query_Pool_T;
      Query          : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_End_Query, "vkCmdEndQuery");

   procedure Cmd_Reset_Query_Pool
     (Command_Buffer : Command_Buffer_T;
      Query_Pool     : Query_Pool_T;
      First_Query    : Interfaces.Unsigned_32;
      Query_Count    : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Reset_Query_Pool, "vkCmdResetQueryPool");

   procedure Cmd_Write_Timestamp
     (Command_Buffer : Command_Buffer_T;
      Pipeline_Stage : Pipeline_Stage_Flag_Bits_T;
      Query_Pool     : Query_Pool_T;
      Query          : Interfaces.Unsigned_32);
   pragma Import (Stdcall, Cmd_Write_Timestamp, "vkCmdWriteTimestamp");

   procedure Cmd_Copy_Query_Pool_Results
     (Command_Buffer : Command_Buffer_T;
      Query_Pool     : Query_Pool_T;
      First_Query    : Interfaces.Unsigned_32;
      Query_Count    : Interfaces.Unsigned_32;
      Dst_Buffer     : Buffer_T;
      Dst_Offset     : Device_Size_T;
      Stride         : Device_Size_T;
      Flags          : Query_Result_Flags_T);
   pragma Import (Stdcall, Cmd_Copy_Query_Pool_Results, "vkCmdCopyQueryPoolResults");

   procedure Cmd_Push_Constants
     (Command_Buffer : Command_Buffer_T;
      Layout         : Pipeline_Layout_T;
      Stage_Flags    : Shader_Stage_Flags_T;
      Offset         : Interfaces.Unsigned_32;
      Size           : Interfaces.Unsigned_32;
      Pvalues        : Void_Ptr);
   pragma Import (Stdcall, Cmd_Push_Constants, "vkCmdPushConstants");

   type Render_Pass_Begin_Info_Const_Ptr is access constant Render_Pass_Begin_Info_T;

   procedure Cmd_Begin_Render_Pass
     (Command_Buffer     : Command_Buffer_T;
      Prender_Pass_Begin : Render_Pass_Begin_Info_Const_Ptr;
      Contents           : Subpass_Contents_T);
   pragma Import (Stdcall, Cmd_Begin_Render_Pass, "vkCmdBeginRenderPass");

   procedure Cmd_Next_Subpass (Command_Buffer : Command_Buffer_T; Contents : Subpass_Contents_T);
   pragma Import (Stdcall, Cmd_Next_Subpass, "vkCmdNextSubpass");

   procedure Cmd_End_Render_Pass (Command_Buffer : Command_Buffer_T);
   pragma Import (Stdcall, Cmd_End_Render_Pass, "vkCmdEndRenderPass");

   procedure Cmd_Execute_Commands
     (Command_Buffer       : Command_Buffer_T;
      Command_Buffer_Count : Interfaces.Unsigned_32;
      Pcommand_Buffers     : Command_Buffer_Const_Ptr);
   pragma Import (Stdcall, Cmd_Execute_Commands, "vkCmdExecuteCommands");

   type Surface_Khr_Ptr is access all Surface_Khr_T;

   function Create_Android_Surface_Khr
     (Instance   : Instance_T;
      Pallocator : Allocation_Callbacks_Const_Ptr;
      Psurface   : Surface_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Android_Surface_Khr, "vkCreateAndroidSurfaceKHR");

   type Display_Properties_Khr_Ptr is access all Display_Properties_Khr_T;

   function Get_Physical_Device_Display_Properties_Khr
     (Physical_Device : Physical_Device_T;
      Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Display_Properties_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Physical_Device_Display_Properties_Khr, "vkGetPhysicalDeviceDisplayPropertiesKHR");

   type Display_Plane_Properties_Khr_Ptr is access all Display_Plane_Properties_Khr_T;

   function Get_Physical_Device_Display_Plane_Properties_Khr
     (Physical_Device : Physical_Device_T;
      Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Display_Plane_Properties_Khr_Ptr) return Result_T;
   pragma Import
     (Stdcall,
      Get_Physical_Device_Display_Plane_Properties_Khr,
      "vkGetPhysicalDeviceDisplayPlanePropertiesKHR");

   type Display_Khr_Ptr is access all Display_Khr_T;

   function Get_Display_Plane_Supported_Displays_Khr
     (Physical_Device : Physical_Device_T;
      Plane_Index     : Interfaces.Unsigned_32;
      Pdisplay_Count  : Uint_32_Ptr;
      Pdisplays       : Display_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Display_Plane_Supported_Displays_Khr, "vkGetDisplayPlaneSupportedDisplaysKHR");

   type Display_Mode_Properties_Khr_Ptr is access all Display_Mode_Properties_Khr_T;

   function Get_Display_Mode_Properties_Khr
     (Physical_Device : Physical_Device_T;
      Display         : Display_Khr_T;
      Pproperty_Count : Uint_32_Ptr;
      Pproperties     : Display_Mode_Properties_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Display_Mode_Properties_Khr, "vkGetDisplayModePropertiesKHR");

   type Display_Mode_Create_Info_Khr_Const_Ptr is access constant Display_Mode_Create_Info_Khr_T;

   type Display_Mode_Khr_Ptr is access all Display_Mode_Khr_T;

   function Create_Display_Mode_Khr
     (Physical_Device : Physical_Device_T;
      Display         : Display_Khr_T;
      Pcreate_Info    : Display_Mode_Create_Info_Khr_Const_Ptr;
      Pallocator      : Allocation_Callbacks_Const_Ptr;
      Pmode           : Display_Mode_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Display_Mode_Khr, "vkCreateDisplayModeKHR");

   type Display_Plane_Capabilities_Khr_Ptr is access all Display_Plane_Capabilities_Khr_T;

   function Get_Display_Plane_Capabilities_Khr
     (Physical_Device : Physical_Device_T;
      Mode            : Display_Mode_Khr_T;
      Plane_Index     : Interfaces.Unsigned_32;
      Pcapabilities   : Display_Plane_Capabilities_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Display_Plane_Capabilities_Khr, "vkGetDisplayPlaneCapabilitiesKHR");

   type Display_Surface_Create_Info_Khr_Const_Ptr is access constant Display_Surface_Create_Info_Khr_T;

   function Create_Display_Plane_Surface_Khr
     (Instance     : Instance_T;
      Pcreate_Info : Display_Surface_Create_Info_Khr_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Psurface     : Surface_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Display_Plane_Surface_Khr, "vkCreateDisplayPlaneSurfaceKHR");

   type Swapchain_Create_Info_Khr_Const_Ptr is access constant Swapchain_Create_Info_Khr_T;

   type Swapchain_Khr_Ptr is access all Swapchain_Khr_T;

   function Create_Shared_Swapchains_Khr
     (Device          : Device_T;
      Swapchain_Count : Interfaces.Unsigned_32;
      Pcreate_Infos   : Swapchain_Create_Info_Khr_Const_Ptr;
      Pallocator      : Allocation_Callbacks_Const_Ptr;
      Pswapchains     : Swapchain_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Shared_Swapchains_Khr, "vkCreateSharedSwapchainsKHR");

   function Create_Mir_Surface_Khr
     (Instance   : Instance_T;
      Pallocator : Allocation_Callbacks_Const_Ptr;
      Psurface   : Surface_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Mir_Surface_Khr, "vkCreateMirSurfaceKHR");

   procedure Destroy_Surface_Khr
     (Instance   : Instance_T;
      Surface    : Surface_Khr_T;
      Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Surface_Khr, "vkDestroySurfaceKHR");

   type Bool_32_Ptr is access all Bool_32_T;

   function Get_Physical_Device_Surface_Support_Khr
     (Physical_Device    : Physical_Device_T;
      Queue_Family_Index : Interfaces.Unsigned_32;
      Surface            : Surface_Khr_T;
      Psupported         : Bool_32_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Physical_Device_Surface_Support_Khr, "vkGetPhysicalDeviceSurfaceSupportKHR");

   type Surface_Capabilities_Khr_Ptr is access all Surface_Capabilities_Khr_T;

   function Get_Physical_Device_Surface_Capabilities_Khr
     (Physical_Device       : Physical_Device_T;
      Surface               : Surface_Khr_T;
      Psurface_Capabilities : Surface_Capabilities_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Physical_Device_Surface_Capabilities_Khr, "vkGetPhysicalDeviceSurfaceCapabilitiesKHR");

   type Surface_Format_Khr_Ptr is access all Surface_Format_Khr_T;

   function Get_Physical_Device_Surface_Formats_Khr
     (Physical_Device       : Physical_Device_T;
      Surface               : Surface_Khr_T;
      Psurface_Format_Count : Uint_32_Ptr;
      Psurface_Formats      : Surface_Format_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Physical_Device_Surface_Formats_Khr, "vkGetPhysicalDeviceSurfaceFormatsKHR");

   type Present_Mode_Khr_Ptr is access all Present_Mode_Khr_T;

   function Get_Physical_Device_Surface_Present_Modes_Khr
     (Physical_Device     : Physical_Device_T;
      Surface             : Surface_Khr_T;
      Ppresent_Mode_Count : Uint_32_Ptr;
      Ppresent_Modes      : Present_Mode_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Physical_Device_Surface_Present_Modes_Khr, "vkGetPhysicalDeviceSurfacePresentModesKHR");

   function Create_Swapchain_Khr
     (Device       : Device_T;
      Pcreate_Info : Swapchain_Create_Info_Khr_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pswapchain   : Swapchain_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Swapchain_Khr, "vkCreateSwapchainKHR");

   procedure Destroy_Swapchain_Khr
     (Device     : Device_T;
      Swapchain  : Swapchain_Khr_T;
      Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Swapchain_Khr, "vkDestroySwapchainKHR");

   function Get_Swapchain_Images_Khr
     (Device                 : Device_T;
      Swapchain              : Swapchain_Khr_T;
      Pswapchain_Image_Count : Uint_32_Ptr;
      Pswapchain_Images      : Image_Ptr) return Result_T;
   pragma Import (Stdcall, Get_Swapchain_Images_Khr, "vkGetSwapchainImagesKHR");

   function Acquire_Next_Image_Khr
     (Device       : Device_T;
      Swapchain    : Swapchain_Khr_T;
      Timeout      : Interfaces.Unsigned_64;
      Semaphore    : Semaphore_T;
      Fence        : Fence_T;
      Pimage_Index : Uint_32_Ptr) return Result_T;
   pragma Import (Stdcall, Acquire_Next_Image_Khr, "vkAcquireNextImageKHR");

   type Present_Info_Khr_Const_Ptr is access constant Present_Info_Khr_T;

   function Queue_Present_Khr (Queue : Queue_T; Ppresent_Info : Present_Info_Khr_Const_Ptr) return Result_T;
   pragma Import (Stdcall, Queue_Present_Khr, "vkQueuePresentKHR");

   function Create_Wayland_Surface_Khr
     (Instance   : Instance_T;
      Pallocator : Allocation_Callbacks_Const_Ptr;
      Psurface   : Surface_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Wayland_Surface_Khr, "vkCreateWaylandSurfaceKHR");

   function Create_Win_32_Surface_Khr
     (Instance   : Instance_T;
      Pallocator : Allocation_Callbacks_Const_Ptr;
      Psurface   : Surface_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Win_32_Surface_Khr, "vkCreateWin32SurfaceKHR");

   function Get_Physical_Device_Win_32_Presentation_Support_Khr
     (Physical_Device    : Physical_Device_T;
      Queue_Family_Index : Interfaces.Unsigned_32) return Bool_32_T;
   pragma Import
     (Stdcall,
      Get_Physical_Device_Win_32_Presentation_Support_Khr,
      "vkGetPhysicalDeviceWin32PresentationSupportKHR");

   function Create_Xlib_Surface_Khr
     (Instance   : Instance_T;
      Pallocator : Allocation_Callbacks_Const_Ptr;
      Psurface   : Surface_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Xlib_Surface_Khr, "vkCreateXlibSurfaceKHR");

   function Create_Xcb_Surface_Khr
     (Instance   : Instance_T;
      Pallocator : Allocation_Callbacks_Const_Ptr;
      Psurface   : Surface_Khr_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Xcb_Surface_Khr, "vkCreateXcbSurfaceKHR");

   type Debug_Report_Callback_Create_Info_Ext_Const_Ptr is access constant Debug_Report_Callback_Create_Info_Ext_T;

   type Debug_Report_Callback_Ext_Ptr is access all Debug_Report_Callback_Ext_T;

   function Create_Debug_Report_Callback_Ext
     (Instance     : Instance_T;
      Pcreate_Info : Debug_Report_Callback_Create_Info_Ext_Const_Ptr;
      Pallocator   : Allocation_Callbacks_Const_Ptr;
      Pcallback    : Debug_Report_Callback_Ext_Ptr) return Result_T;
   pragma Import (Stdcall, Create_Debug_Report_Callback_Ext, "vkCreateDebugReportCallbackEXT");

   procedure Destroy_Debug_Report_Callback_Ext
     (Instance   : Instance_T;
      Callback   : Debug_Report_Callback_Ext_T;
      Pallocator : Allocation_Callbacks_Const_Ptr);
   pragma Import (Stdcall, Destroy_Debug_Report_Callback_Ext, "vkDestroyDebugReportCallbackEXT");

   procedure Debug_Report_Message_Ext
     (Instance      : Instance_T;
      Flags         : Debug_Report_Flags_Ext_T;
      Object_Type   : Debug_Report_Object_Type_Ext_T;
      Object        : Interfaces.Unsigned_64;
      Location      : Interfaces.C.size_t;
      Message_Code  : Interfaces.Integer_32;
      Player_Prefix : Interfaces.C.Strings.chars_ptr;
      Pmessage      : Interfaces.C.Strings.chars_ptr);
   pragma Import (Stdcall, Debug_Report_Message_Ext, "vkDebugReportMessageEXT");

   type Debug_Marker_Object_Name_Info_Ext_Ptr is access all Debug_Marker_Object_Name_Info_Ext_T;

   function Debug_Marker_Set_Object_Name_Ext
     (Device     : Device_T;
      Pname_Info : Debug_Marker_Object_Name_Info_Ext_Ptr) return Result_T;
   pragma Import (Stdcall, Debug_Marker_Set_Object_Name_Ext, "vkDebugMarkerSetObjectNameEXT");

   type Debug_Marker_Object_Tag_Info_Ext_Ptr is access all Debug_Marker_Object_Tag_Info_Ext_T;

   function Debug_Marker_Set_Object_Tag_Ext
     (Device    : Device_T;
      Ptag_Info : Debug_Marker_Object_Tag_Info_Ext_Ptr) return Result_T;
   pragma Import (Stdcall, Debug_Marker_Set_Object_Tag_Ext, "vkDebugMarkerSetObjectTagEXT");

   type Debug_Marker_Marker_Info_Ext_Ptr is access all Debug_Marker_Marker_Info_Ext_T;

   procedure Cmd_Debug_Marker_Begin_Ext
     (Command_Buffer : Command_Buffer_T;
      Pmarker_Info   : Debug_Marker_Marker_Info_Ext_Ptr);
   pragma Import (Stdcall, Cmd_Debug_Marker_Begin_Ext, "vkCmdDebugMarkerBeginEXT");

   procedure Cmd_Debug_Marker_End_Ext (Command_Buffer : Command_Buffer_T);
   pragma Import (Stdcall, Cmd_Debug_Marker_End_Ext, "vkCmdDebugMarkerEndEXT");

   procedure Cmd_Debug_Marker_Insert_Ext
     (Command_Buffer : Command_Buffer_T;
      Pmarker_Info   : Debug_Marker_Marker_Info_Ext_Ptr);
   pragma Import (Stdcall, Cmd_Debug_Marker_Insert_Ext, "vkCmdDebugMarkerInsertEXT");

private

   type Hidden_Instance_T is null record;
   type Instance_T is access Hidden_Instance_T;

   type Hidden_Physical_Device_T is null record;
   type Physical_Device_T is access Hidden_Physical_Device_T;

   type Hidden_Device_T is null record;
   type Device_T is access Hidden_Device_T;

   type Hidden_Queue_T is null record;
   type Queue_T is access Hidden_Queue_T;

   type Hidden_Command_Buffer_T is null record;
   type Command_Buffer_T is access Hidden_Command_Buffer_T;

   type Hidden_Device_Memory_T is null record;
   type Device_Memory_T is access Hidden_Device_Memory_T;

   type Hidden_Command_Pool_T is null record;
   type Command_Pool_T is access Hidden_Command_Pool_T;

   type Hidden_Buffer_T is null record;
   type Buffer_T is access Hidden_Buffer_T;

   type Hidden_Buffer_View_T is null record;
   type Buffer_View_T is access Hidden_Buffer_View_T;

   type Hidden_Image_T is null record;
   type Image_T is access Hidden_Image_T;

   type Hidden_Image_View_T is null record;
   type Image_View_T is access Hidden_Image_View_T;

   type Hidden_Shader_Module_T is null record;
   type Shader_Module_T is access Hidden_Shader_Module_T;

   type Hidden_Pipeline_T is null record;
   type Pipeline_T is access Hidden_Pipeline_T;

   type Hidden_Pipeline_Layout_T is null record;
   type Pipeline_Layout_T is access Hidden_Pipeline_Layout_T;

   type Hidden_Sampler_T is null record;
   type Sampler_T is access Hidden_Sampler_T;

   type Hidden_Descriptor_Set_T is null record;
   type Descriptor_Set_T is access Hidden_Descriptor_Set_T;

   type Hidden_Descriptor_Set_Layout_T is null record;
   type Descriptor_Set_Layout_T is access Hidden_Descriptor_Set_Layout_T;

   type Hidden_Descriptor_Pool_T is null record;
   type Descriptor_Pool_T is access Hidden_Descriptor_Pool_T;

   type Hidden_Fence_T is null record;
   type Fence_T is access Hidden_Fence_T;

   type Hidden_Semaphore_T is null record;
   type Semaphore_T is access Hidden_Semaphore_T;

   type Hidden_Event_T is null record;
   type Event_T is access Hidden_Event_T;

   type Hidden_Query_Pool_T is null record;
   type Query_Pool_T is access Hidden_Query_Pool_T;

   type Hidden_Framebuffer_T is null record;
   type Framebuffer_T is access Hidden_Framebuffer_T;

   type Hidden_Render_Pass_T is null record;
   type Render_Pass_T is access Hidden_Render_Pass_T;

   type Hidden_Pipeline_Cache_T is null record;
   type Pipeline_Cache_T is access Hidden_Pipeline_Cache_T;

   type Hidden_Display_Khr_T is null record;
   type Display_Khr_T is access Hidden_Display_Khr_T;

   type Hidden_Display_Mode_Khr_T is null record;
   type Display_Mode_Khr_T is access Hidden_Display_Mode_Khr_T;

   type Hidden_Surface_Khr_T is null record;
   type Surface_Khr_T is access Hidden_Surface_Khr_T;

   type Hidden_Swapchain_Khr_T is null record;
   type Swapchain_Khr_T is access Hidden_Swapchain_Khr_T;

   type Hidden_Debug_Report_Callback_Ext_T is null record;
   type Debug_Report_Callback_Ext_T is access Hidden_Debug_Report_Callback_Ext_T;

end Vk;
