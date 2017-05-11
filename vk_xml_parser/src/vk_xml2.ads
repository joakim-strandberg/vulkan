with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Vk_XML2 is

   type XML_Text_T is new Ada.Strings.Unbounded.Unbounded_String;

   type XML_Text_Ptr is access all XML_Text_T;

   type XML_Out_Commented_Message_T is new Ada.Strings.Unbounded.Unbounded_String;

   type XML_Out_Commented_Message_Ptr is access all XML_Out_Commented_Message_T;

   package Tag is

      type Name_T is new Ada.Strings.Unbounded.Unbounded_String;

      type Author_T is new Ada.Strings.Unbounded.Unbounded_String;

      type Contact_T is new Ada.Strings.Unbounded.Unbounded_String;

      type T is limited
         record
            Name    : aliased Name_T;
            Author  : aliased Author_T;
            Contact : aliased Contact_T;
         end record;

      type Ptr is access all T;

   end Tag;

   package Tags is

      type Child_Kind_Id_T is (
                               Child_Tag
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Tag) is record
         case Kind_Id is
            when Child_Tag => Tag_V : aliased Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Tags;

   -- Representation of <comment>...<comment>
   package Comment is

      type Value_T is new Ada.Strings.Unbounded.Unbounded_String;

      type T is limited
         record
            Value : Value_T;
         end record;

      type Ptr is access all T;

   end Comment;

   package Name is

      type Value_T is new Ada.Strings.Unbounded.Unbounded_String;

      type T is limited
         record
            Value : Value_T;
         end record;

      type Ptr is access all T;

   end Name;

   package Nested_Type is

      type Nullable_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Value : Nullable_Value_T;
         end record;

      type Ptr is access all T;

   end Nested_Type;

   package Enum is

      type Value_T is new Ada.Strings.Unbounded.Unbounded_String;

      type T is limited
         record
            Value : Value_T;
         end record;

      type Ptr is access all T;

   end Enum;

   -- enum tags that has an <enums>-tag as parent
   package Enums_Enum is

      type Nullable_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      subtype Bit_Position_T is Natural range 0..32;

      type Nullable_Bit_Position_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Bit_Position_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Value        : Nullable_Value_T;
            Name         : Nullable_Name_T;
            Comment      : Nullable_Comment_T;
            Bit_Position : Nullable_Bit_Position_T;
         end record;

      type Ptr is access all T;

   end Enums_Enum;

   package Member is

      type Child_Kind_Id_T is (
                               Child_Name,
                               Child_Nested_Type,
                               Child_Enum,
                               Child_XML_Text
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Name) is record
         case Kind_Id is
            when Child_Name        => Name_V        : aliased Name.Ptr;
            when Child_Nested_Type => Nested_Type_V : aliased Nested_Type.Ptr;
            when Child_Enum        => Enum_V        : aliased Enum.Ptr;
            when Child_XML_Text    => XML_Text_V    : aliased XML_Text_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Optional_T is new Boolean;

      type Nullable_Len_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type No_Auto_Validity_T is new Boolean;

      type Nullable_Valid_Extension_Structs_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Children                : Child_Vectors.Vector;
            Optional                : Optional_T := Optional_T'(False);
            Len                     : Nullable_Len_T;
            No_Auto_Validity        : No_Auto_Validity_T := No_Auto_Validity_T'(False);
            Valid_Extension_Structs : Nullable_Valid_Extension_Structs_T;
         end record;

      type Ptr is access all T;

   end Member;

   package Usage is

      type Child_Kind_Id_T is (
                               Child_XML_Text
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Text) is record
         case Kind_Id is
            when Child_XML_Text => XML_Text_V : aliased XML_Text_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Nullable_Command_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Struct_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Children : Child_Vectors.Vector;
            Command  : Nullable_Command_T;
            Struct   : Nullable_Struct_T;
         end record;

      type Ptr is access all T;

   end Usage;

   package Validity is

      type Child_Kind_Id_T is (
                               Child_Usage
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Usage) is record
         case Kind_Id is
            when Child_Usage => Usage_V : aliased Usage.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Validity;

   -- <type>..</type>
   package Type_T is

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Category_T is new Ada.Strings.Unbounded.Unbounded_String;

      type Nullable_Requires_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Parent_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Returned_Only_T is new Boolean;

      type Child_Kind_Id_T is (
                               Child_XML_Text,
                               Child_Name,
                               Child_Nested_Type,
                               Child_Member,
                               Child_Validity,
                               Child_Out_Commented_Message
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Name) is record
         case Kind_Id is
            when Child_XML_Text    => XML_Text_V    : aliased XML_Text_Ptr;
            when Child_Name        => Name_V        : aliased Name.Ptr;
            when Child_Nested_Type => Nested_Type_V : aliased Nested_Type.Ptr;
            when Child_Member      => Member_V      : aliased Member.Ptr;
            when Child_Validity    => Validity_V    : aliased Validity.Ptr;
            when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Nullable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Name          : Nullable_Name_T;
            Category      : Category_T;
            Requires      : Nullable_Requires_T;
            Parent        : Nullable_Parent_T;
            Returned_Only : Returned_Only_T := Returned_Only_T'(False);
            Comment       : Nullable_Comment_T;
            Children      : Child_Vectors.Vector;
         end record;

      function To_String (This : T) return String;

      type Ptr is access all T;

   end Type_T;

   package Types is

      type Child_Kind_Id_T is (
                               Child_Type,
                               Child_Out_Commented_Message
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Type) is record
         case Kind_Id is
            when Child_Type                  => Type_V                  : aliased Type_T.Ptr;
            when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Types;

   package Vendor_Id is

      type Name_T is new Ada.Strings.Unbounded.Unbounded_String;

      type Id_T is new Ada.Strings.Unbounded.Unbounded_String;

      type Comment_T is new Ada.Strings.Unbounded.Unbounded_String;

      type T is limited
         record
            Name    : Name_T;
            Id      : Id_T;
            Comment : Comment_T;
         end record;

      type Ptr is access all T;

   end Vendor_Id;

   package Vendor_Ids is

      type Child_Kind_Id_T is (
                               Child_Vendor_Id
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Vendor_Id) is record
         case Kind_Id is
            when Child_Vendor_Id => Vendor_Id_V : aliased Vendor_Id.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Vendor_Ids;

   package Unused is

      type Nullable_Start_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Start : Nullable_Start_T;
         end record;

      type Ptr is access all T;

   end Unused;

   package Enums is

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Child_Kind_Id_T is (
                               Child_Enums_Enum,
                               Child_Out_Commented_Message,
                               Child_Unused
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Enums_Enum) is record
         case Kind_Id is
            when Child_Enums_Enum            => Enums_Enum_V            : aliased Enums_Enum.Ptr;
            when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Ptr;
            when Child_Unused                => Unused_V                : aliased Unused.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Type_Attribue_T is (
                               Enum,
                               Bit_Mask
                              );

      type Nullable_Type_Attribue_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Type_Attribue_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Name          : Nullable_Name_T;
            Comment       : Nullable_Comment_T;
            Children      : Child_Vectors.Vector;
            Type_Attribue : Nullable_Type_Attribue_T;
         end record;

      type Ptr is access all T;

   end Enums;

   package Proto is

      type Child_Kind_Id_T is (
                               Child_Nested_Type,
                               Child_Name
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Nested_Type) is record
         case Kind_Id is
            when Child_Nested_Type => Nested_Type_V : aliased Nested_Type.Ptr;
            when Child_Name        => Name_V        : aliased Name.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Proto;

   package Param is

      type Child_Kind_Id_T is (
                               Child_Nested_Type,
                               Child_XML_Text,
                               Child_Name
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Nested_Type) is record
         case Kind_Id is
            when Child_Nested_Type => Nested_Type_V : aliased Nested_Type.Ptr;
            when Child_XML_Text    => XML_Text_V    : aliased XML_Text_Ptr;
            when Child_Name        => Name_V        : aliased Name.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Optional_T is new Boolean;

      type Nullable_External_Sync_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Len_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_No_Auto_Validity_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Boolean;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Children         : Child_Vectors.Vector;
            Optional         : Optional_T;
            External_Sync    : Nullable_External_Sync_T;
            Len              : Nullable_Len_T;
            No_Auto_Validity : Nullable_No_Auto_Validity_T;
         end record;

      type Ptr is access all T;

   end Param;

   package External_Sync_Parameter is

      type Nullable_XML_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            XML_Value : Nullable_XML_Value_T;
         end record;

      type Ptr is access all T;

   end External_Sync_Parameter;

   package Implicit_External_Sync_Parameters is

      type Child_Kind_Id_T is (
                               Child_External_Sync_Parameter
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_External_Sync_Parameter) is record
         case Kind_Id is
            when Child_External_Sync_Parameter => External_Sync_Parameter_V : aliased External_Sync_Parameter.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Implicit_External_Sync_Parameters;

   package Command is

      type Success_Code_T is new Ada.Strings.Unbounded.Unbounded_String;

      package Success_Code_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                  Element_Type => Success_Code_T,
                                                                  "="          => "=");

      type Error_Code_T is new Ada.Strings.Unbounded.Unbounded_String;

      package Error_Code_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                Element_Type => Error_Code_T,
                                                                "="          => "=");

      type Child_Kind_Id_T is (
                               Child_Proto,
                               Child_Param,
                               Child_Validity,
                               Child_Implicit_External_Sync_Parameters
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Proto) is record
         case Kind_Id is
            when Child_Proto                             => Proto_V      : aliased Proto.Ptr;
            when Child_Param                             => Param_V      : aliased Param.Ptr;
            when Child_Validity                          => Validity_V   : aliased Validity.Ptr;
            when Child_Implicit_External_Sync_Parameters => Parameters_V : aliased Implicit_External_Sync_Parameters.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Queue_T is (
                       Sparse_Binding,
                       Graphics,
                       Compute,
                       Transfer
                      );

      package Queue_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Queue_T,
                                                           "="          => "=");

      type Render_Pass_T is (
                             Inside,
                             Outside,
                             Both
                            );

      package Render_Pass_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                 Element_Type => Render_Pass_T,
                                                                 "="          => "=");

      type Command_Buffer_Level_T is (
                                      Primary,
                                      Secondary
                                     );

      package Command_Buffer_Level_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                          Element_Type => Command_Buffer_Level_T,
                                                                          "="          => "=");

      type T is limited
         record
            Success_Codes         : Success_Code_Vectors.Vector;
            Error_Codes           : Error_Code_Vectors.Vector;
            Children              : Child_Vectors.Vector;
            Queues                : Queue_Vectors.Vector;
            Render_Passes         : Render_Pass_Vectors.Vector;
            Command_Buffer_Levels : Command_Buffer_Level_Vectors.Vector;
         end record;

      function To_String (This : T) return String;

      type Ptr is access all T;

   end Command;

   package Commands is

      type Child_Kind_Id_T is (
                               Child_Command
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Command) is record
         case Kind_Id is
            when Child_Command => Command_V : aliased Command.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Commands;

   package Require_Enum is

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Offset_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Natural range 0..10;
               when False => null;
            end case;
         end record;

      type Nullable_Dir_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Extends_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Bit_Position_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Natural range 0..20;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Name         : Nullable_Name_T;
            Value        : Nullable_Value_T;
            Offset       : Nullable_Offset_T;
            Dir          : Nullable_Dir_T;
            Extends      : Nullable_Extends_T;
            Comment      : Nullable_Comment_T;
            Bit_Position : Nullable_Bit_Position_T;
         end record;

      type Ptr is access all T;

   end Require_Enum;

   package Require_Command is

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Name : Nullable_Name_T;
         end record;

      type Ptr is access all T;

   end Require_Command;

   package Require is

      type Nullable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Child_Kind_Id_T is (
                               Child_Type,
                               Child_Enum,
                               Child_Command,
                               Child_Out_Commented_Message,
                               Child_Usage
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Type) is record
         case Kind_Id is
            when Child_Type                  => Type_V                  : aliased Type_T.Ptr;
            when Child_Enum                  => Enum_V                  : aliased Require_Enum.Ptr;
            when Child_Command               => Command_V               : aliased Require_Command.Ptr;
            when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Ptr;
            when Child_Usage                 => Usage_V                 : aliased Usage.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Comment  : Nullable_Comment_T;
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Require;

   package Feature is

      type Nullable_API_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Number_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Child_Kind_Id_T is (
                               Child_Require
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Require) is record
         case Kind_Id is
            when Child_Require   => Require_V   : aliased Require.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            API      : Nullable_API_T;
            Name     : Nullable_Name_T;
            Number   : Nullable_Number_T;
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Feature;

   package Extension is

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Number_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Positive range 1..30;
               when False => null;
            end case;
         end record;

      type Supported_T is (
                           Vulkan,
                           Disabled
                          );

      type Nullable_Supported_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Supported_T;
               when False => null;
            end case;
         end record;

      type Child_Kind_Id_T is (
                               Child_Require
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Require) is record
         case Kind_Id is
            when Child_Require => Require_V : aliased Require.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Nullable_Protect_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Author_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type Nullable_Contact_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Ada.Strings.Unbounded.Unbounded_String;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            Name      : Nullable_Name_T;
            Number    : Nullable_Number_T;
            Supported : Nullable_Supported_T;
            Children  : Child_Vectors.Vector;
            Protect   : Nullable_Protect_T;
            Author    : Nullable_Author_T;
            Contact   : Nullable_Contact_T;
         end record;

      type Ptr is access all T;

   end Extension;

   package Extensions is

      type Child_Kind_Id_T is (
                               Child_Extension,
                               Child_Out_Commented_Message
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Extension) is record
         case Kind_Id is
            when Child_Extension             => Extension_V             : aliased Extension.Ptr;
            when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Extensions;

   package Registry is

      type Child_Kind_Id_T is (
                               Child_Comment,
                               Child_XML_Text,
                               Child_Out_Commented_Message,
                               Child_Vendor_Ids,
                               Child_Tags,
                               Child_Types,
                               Child_Enums,
                               Child_Commands,
                               Child_Feature,
                               Child_Extensions
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Comment) is record
         case Kind_Id is
            when Child_XML_Text              => XML_Text_V              : aliased XML_Text_Ptr;
            when Child_Comment               => C                       : aliased Comment.Ptr;
            when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Ptr;
            when Child_Vendor_Ids            => Vendor_Ids_V            : aliased Vendor_Ids.Ptr;
            when Child_Tags                  => Tags_V                  : aliased Tags.Ptr;
            when Child_Types                 => Types_V                 : aliased Types.Ptr;
            when Child_Enums                 => Enums_V                 : aliased Enums.Ptr;
            when Child_Commands              => Commands_V              : aliased Commands.Ptr;
            when Child_Feature               => Feature_V               : aliased Feature.Ptr;
            when Child_Extensions            => Extensions_V            : aliased Extensions.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type T is limited
         record
            Children : Child_Vectors.Vector;
         end record;

      type Ptr is access all T;

   end Registry;

end Vk_XML2;
