with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Basic_Dynamic_Pools;
with Aida;

generic
package Generic_Vk_XML is

   Main_Pool : Basic_Dynamic_Pools.Basic_Dynamic_Pool (2_000_000);

   use all type Aida.String_T; -- Is used in child packages

   type String_Ptr is access all Aida.String_T with Storage_Pool => Main_Pool;

   Empty_String : aliased Aida.String_T := "";

   type Nullable_String_Ptr (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : not null String_Ptr := Empty_String'Access;
         when False => null;
      end case;
   end record;

   type Nullable_Boolean_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   package Tag_Tag is

      type T is tagged limited private with
        Default_Initial_Condition => not T.Exists_Name and not T.Exists_Author and not T.Exists_Contact;

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Author (This  : in out T;
                            Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Author,
        Post   => This.Exists_Author and This.Author = Value;

      function Author (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Author;

      function Exists_Author (This : T) return Boolean with
        Global => null;

      procedure Set_Contact (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Contact,
        Post   => This.Exists_Contact and This.Contact = Value;

      function Contact (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Author;

      function Exists_Contact (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Name    : Nullable_String_Ptr;
            My_Author  : Nullable_String_Ptr;
            My_Contact : Nullable_String_Ptr;
         end record;

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Author (This : T) return Aida.String_T is (This.My_Author.Value.all);

      function Exists_Author (This : T) return Boolean is (This.My_Author.Exists);

      function Contact (This : T) return Aida.String_T is (This.My_Contact.Value.all);

      function Exists_Contact (This : T) return Boolean is (This.My_Contact.Exists);

   end Tag_Tag;

   package Tags_Tag is

      type Child_Kind_Id_T is (
                               Child_Tag
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Tag) is record
         case Kind_Id is
         when Child_Tag => Tag : Tag_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Tags_Tag;

   -- Representation of <comment>...<comment>
   package Comment_Tag is

      type T is tagged limited private;

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Value,
        Post   => This.Exists_Value and This.Value = Value;

      function Value (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Value;

      function Exists_Value (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Value : Nullable_String_Ptr;
         end record;

      function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

      function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

   end Comment_Tag;

   package Name_Tag is

      type T is tagged limited private;

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Value,
        Post   => This.Exists_Value and This.Value = Value;

      function Value (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Value;

      function Exists_Value (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Value : Nullable_String_Ptr;
         end record;

      function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

      function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

   end Name_Tag;

   package Nested_Type_Tag is

      type T is tagged limited private;

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Value,
        Post   => This.Exists_Value and This.Value = Value;

      function Value (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Value;

      function Exists_Value (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited record
         My_Value : Nullable_String_Ptr;
      end record;

      function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

      function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

   end Nested_Type_Tag;

   package Enum_Tag is

      type T is tagged limited private;

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Value,
        Post   => This.Exists_Value and This.Value = Value;

      function Value (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Value;

      function Exists_Value (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited record
         My_Value : Nullable_String_Ptr;
      end record;

      function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

      function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

   end Enum_Tag;

   -- enum tags that has an <enums>-tag as parent
   package Enums_Enum_Tag is

      use all type Aida.Int32_T;

      subtype Bit_Position_T is Aida.Int32_T range 0..32;

      type T is tagged limited private;

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Value,
        Post   => This.Exists_Value and This.Value = Value;

      function Value (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Value;

      function Exists_Value (This : T) return Boolean with
        Global => null;

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Comment (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Comment,
        Post   => This.Exists_Comment and This.Comment = Value;

      function Comment (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Comment;

      function Exists_Comment (This : T) return Boolean with
        Global => null;

      procedure Set_Bit_Position (This  : in out T;
                                  Value : Bit_Position_T) with
        Global => null,
        Pre    => not This.Exists_Bit_Position,
        Post   => This.Exists_Bit_Position and This.Bit_Position = Value;

      function Bit_Position (This : T) return Bit_Position_T with
        Global => null,
        Pre    => This.Exists_Bit_Position;

      function Exists_Bit_Position (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type Nullable_Bit_Position_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Bit_Position_T;
         when False => null;
         end case;
      end record;

      type T is tagged limited record
         My_Value        : Nullable_String_Ptr;
         My_Name         : Nullable_String_Ptr;
         My_Comment      : Nullable_String_Ptr;
         My_Bit_Position : Nullable_Bit_Position_T;
      end record;

      function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

      function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

      function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

      function Bit_Position (This : T) return Bit_Position_T is (This.My_Bit_Position.Value);

      function Exists_Bit_Position (This : T) return Boolean is (This.My_Bit_Position.Exists);

   end Enums_Enum_Tag;

   package Member_Tag is

      type Child_Kind_Id_T is (
                               Child_Name,
                               Child_Nested_Type,
                               Child_Enum,
                               Child_XML_Text
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Name) is record
         case Kind_Id is
         when Child_Name        => Name        : Name_Tag.Ptr;
         when Child_Nested_Type => Nested_Type : Nested_Type_Tag.Ptr;
         when Child_Enum        => Enum        : Enum_Tag.Ptr;
         when Child_XML_Text    => XML_Text    : String_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref_T (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref_T;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) with
        Global => null,
        Pre    => not This.Exists_Optional,
        Post   => This.Exists_Optional and This.Optional = Value;

      function Optional (This : T) return Boolean with
        Global => null,
        Pre    => This.Exists_Optional;

      function Exists_Optional (This : T) return Boolean with
        Global => null;

      procedure Set_Len (This  : in out T;
                         Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Len,
        Post   => This.Exists_Len and This.Len = Value;

      function Len (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Len;

      function Exists_Len (This : T) return Boolean with
        Global => null;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) with
        Global => null,
        Pre    => not This.Exists_No_Auto_Validity,
        Post   => This.Exists_No_Auto_Validity and This.No_Auto_Validity = Value;

      function No_Auto_Validity (This : T) return Boolean with
        Global => null,
        Pre    => This.Exists_No_Auto_Validity;

      function Exists_No_Auto_Validity (This : T) return Boolean with
        Global => null;

      procedure Set_Valid_Extension_Structs (This  : in out T;
                                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Valid_Extension_Structs,
        Post   => This.Exists_Valid_Extension_Structs and This.Valid_Extension_Structs = Value;

      function Valid_Extension_Structs (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Valid_Extension_Structs;

      function Exists_Valid_Extension_Structs (This : T) return Boolean with
        Global => null;

      function To_String (This : T) return Aida.String_T;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type Nullable_Optional_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Boolean;
         when False => null;
         end case;
      end record;

      type Nullable_No_Auto_Validity_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Boolean;
         when False => null;
         end case;
      end record;

      type T is tagged limited record
         My_Children                : aliased Child_Vectors.Vector;
         My_Optional                : Nullable_Optional_T;
         My_Len                     : Nullable_String_Ptr;
         My_No_Auto_Validity        : Nullable_No_Auto_Validity_T;
         My_Valid_Extension_Structs : Nullable_String_Ptr;
      end record;

      function Children (This : aliased T) return Children_Ref_T is ((E => This.My_Children'Access));

      function Optional (This : T) return Boolean is (This.My_Optional.Value);

      function Exists_Optional (This : T) return Boolean is (This.My_Optional.Exists);

      function Len (This : T) return Aida.String_T is (This.My_Len.Value.all);

      function Exists_Len (This : T) return Boolean is (This.My_Len.Exists);

      function No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Value);

      function Exists_No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Exists);

      function Valid_Extension_Structs (This : T) return Aida.String_T is (This.My_Valid_Extension_Structs.Value.all);

      function Exists_Valid_Extension_Structs (This : T) return Boolean is (This.My_Valid_Extension_Structs.Exists);

   end Member_Tag;

   package Usage_Tag is

      type Child_Kind_Id_T is (
                               Child_XML_Text
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Text) is record
         case Kind_Id is
         when Child_XML_Text => XML_Text : String_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_Command (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Command,
        Post   => This.Exists_Command and This.Command = Value;

      function Command (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Command;

      function Exists_Command (This : T) return Boolean with
        Global => null;

      procedure Set_Struct (This  : in out T;
                            Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Struct,
        Post   => This.Exists_Struct and This.Struct = Value;

      function Struct (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Struct;

      function Exists_Struct (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
            My_Command  : Nullable_String_Ptr;
            My_Struct   : Nullable_String_Ptr;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

      function Command (This : T) return Aida.String_T is (This.My_Command.Value.all);

      function Exists_Command (This : T) return Boolean is (This.My_Command.Exists);

      function Struct (This : T) return Aida.String_T is (This.My_Struct.Value.all);

      function Exists_Struct (This : T) return Boolean is (This.My_Struct.Exists);

   end Usage_Tag;

   package Validity_Tag is

      type Child_Kind_Id_T is (
                               Child_Usage
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Usage) is record
         case Kind_Id is
         when Child_Usage => Usage : Usage_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Validity_Tag;

   -- <type>..</type>
   package Type_Tag is

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
         when Child_XML_Text              => XML_Text              : String_Ptr;
         when Child_Name                  => Name                  : Name_Tag.Ptr;
         when Child_Nested_Type           => Nested_Type           : Nested_Type_Tag.Ptr;
         when Child_Member                => Member                : Member_Tag.Ptr;
         when Child_Validity              => Validity              : Validity_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : String_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Category (This  : in out T;
                              Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Category,
        Post   => This.Exists_Category and This.Category = Value;

      function Category (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Category;

      function Exists_Category (This : T) return Boolean with
        Global => null;

      procedure Set_Requires (This  : in out T;
                              Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Requires,
        Post   => This.Exists_Requires and This.Requires = Value;

      function Requires (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Requires;

      function Exists_Requires (This : T) return Boolean with
        Global => null;

      procedure Set_Parent (This  : in out T;
                            Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Parent,
        Post   => This.Exists_Parent and This.Parent = Value;

      function Parent (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Parent;

      function Exists_Parent (This : T) return Boolean with
        Global => null;

      procedure Set_Returned_Only (This  : in out T;
                                   Value : Boolean) with
        Global => null,
        Pre    => not This.Exists_Returned_Only,
        Post   => This.Exists_Returned_Only and This.Returned_Only = Value;

      function Returned_Only (This : T) return Boolean with
        Global => null,
        Pre    => This.Exists_Returned_Only;

      function Exists_Returned_Only (This : T) return Boolean with
        Global => null;

      procedure Set_Comment (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Comment,
        Post   => This.Exists_Comment and This.Comment = Value;

      function Comment (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Comment;

      function Exists_Comment (This : T) return Boolean with
        Global => null;

      function To_String (This : T) return String;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited record
         My_Name          : Nullable_String_Ptr;
         My_Category      : Nullable_String_Ptr;
         My_Requires      : Nullable_String_Ptr;
         My_Parent        : Nullable_String_Ptr;
         My_Returned_Only : Nullable_Boolean_T;
         My_Comment       : Nullable_String_Ptr;
         My_Children      : aliased Child_Vectors.Vector;
      end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Category (This : T) return Aida.String_T is (This.My_Category.Value.all);

      function Exists_Category (This : T) return Boolean is (This.My_Category.Exists);

      function Requires (This : T) return Aida.String_T is (This.My_Requires.Value.all);

      function Exists_Requires (This : T) return Boolean is (This.My_Requires.Exists);

      function Parent (This : T) return Aida.String_T is (This.My_Parent.Value.all);

      function Exists_Parent (This : T) return Boolean is (This.My_Parent.Exists);

      function Returned_Only (This : T) return Boolean is (This.My_Returned_Only.Value);

      function Exists_Returned_Only (This : T) return Boolean is (This.My_Returned_Only.Exists);

      function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

      function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

   end Type_Tag;

   package Types_Tag is

      type Child_Kind_Id_T is (
                               Child_Type,
                               Child_Out_Commented_Message
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Type) is record
         case Kind_Id is
         when Child_Type                  => Type_V                : Type_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : String_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Types_Tag;

   package Vendor_Id_Tag is

      type T is tagged limited private;

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Id (This  : in out T;
                        Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Id,
        Post   => This.Exists_Id and This.Id = Value;

      function Id (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Id;

      function Exists_Id (This : T) return Boolean with
        Global => null;

      procedure Set_Comment (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Comment,
        Post   => This.Exists_Comment and This.Comment = Value;

      function Comment (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Comment;

      function Exists_Comment (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Name    : Nullable_String_Ptr;
            My_Id      : Nullable_String_Ptr;
            My_Comment : Nullable_String_Ptr;
         end record;

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Id (This : T) return Aida.String_T is (This.My_Id.Value.all);

      function Exists_Id (This : T) return Boolean is (This.My_Id.Exists);

      function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

      function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

   end Vendor_Id_Tag;

   package Vendor_Ids_Tag is

      type Child_Kind_Id_T is (
                               Child_Vendor_Id
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Vendor_Id) is record
         case Kind_Id is
         when Child_Vendor_Id => Vendor_Id : Vendor_Id_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Vendor_Ids_Tag;

   package Unused_Tag is

      type T is tagged limited private;

      procedure Set_Start (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Start,
        Post   => This.Exists_Start and This.Start = Value;

      function Start (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Start;

      function Exists_Start (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Start : Nullable_String_Ptr;
         end record;

      function Start (This : T) return Aida.String_T is (This.My_Start.Value.all);

      function Exists_Start (This : T) return Boolean is (This.My_Start.Exists);

   end Unused_Tag;

   package Enums_Tag is

      type Child_Kind_Id_T is (
                               Child_Enums_Enum,
                            Child_Out_Commented_Message,
                            Child_Unused
                           );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Enums_Enum) is record
         case Kind_Id is
         when Child_Enums_Enum            => Enums_Enum            : Enums_Enum_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : String_Ptr;
         when Child_Unused                => Unused                : Unused_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type Type_Attribue_T is (
                               Enum,
                               Bit_Mask
                              );

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Comment (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Comment,
        Post   => This.Exists_Comment and This.Comment = Value;

      function Comment (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Comment;

      function Exists_Comment (This : T) return Boolean with
        Global => null;

      procedure Set_Type_Attribute (This  : in out T;
                                    Value : Type_Attribue_T) with
        Global => null,
        Pre    => not This.Exists_Type_Attribute,
        Post   => This.Exists_Type_Attribute and This.Type_Attribute = Value;

      function Type_Attribute (This : T) return Type_Attribue_T with
        Global => null,
        Pre    => This.Exists_Type_Attribute;

      function Exists_Type_Attribute (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type Nullable_Type_Attribute_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Type_Attribue_T;
         when False => null;
         end case;
      end record;

      type T is tagged limited
         record
            My_Name           : Nullable_String_Ptr;
            My_Comment        : Nullable_String_Ptr;
            My_Children       : aliased Child_Vectors.Vector;
            My_Type_Attribute : Nullable_Type_Attribute_T;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

      function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

      function Type_Attribute (This : T) return Type_Attribue_T is (This.My_Type_Attribute.Value);

      function Exists_Type_Attribute (This : T) return Boolean is (This.My_Type_Attribute.Exists);

   end Enums_Tag;

   package Proto_Tag is

      type Child_Kind_Id_T is (
                               Child_Nested_Type,
                               Child_Name
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Nested_Type) is record
         case Kind_Id is
         when Child_Nested_Type => Nested_Type : Nested_Type_Tag.Ptr;
         when Child_Name        => Name        : Name_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Proto_Tag;

   package Param_Tag is

      type Child_Kind_Id_T is (
                               Child_Nested_Type,
                               Child_XML_Text,
                               Child_Name
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Nested_Type) is record
         case Kind_Id is
         when Child_Nested_Type => Nested_Type : Nested_Type_Tag.Ptr;
         when Child_XML_Text    => XML_Text    : String_Ptr;
         when Child_Name        => Name        : Name_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) with
        Global => null,
        Pre    => not This.Exists_Optional,
        Post   => This.Exists_Optional and This.Optional = Value;

      function Optional (This : T) return Boolean with
        Global => null,
        Pre    => This.Exists_Optional;

      function Exists_Optional (This : T) return Boolean with
        Global => null;

      procedure Set_External_Sync (This  : in out T;
                                   Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_External_Sync,
        Post   => This.Exists_External_Sync and This.External_Sync = Value;

      function External_Sync (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_External_Sync;

      function Exists_External_Sync (This : T) return Boolean with
        Global => null;

      procedure Set_Len (This  : in out T;
                         Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Len,
        Post   => This.Exists_Len and This.Len = Value;

      function Len (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Len;

      function Exists_Len (This : T) return Boolean with
        Global => null;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) with
        Global => null,
        Pre    => not This.Exists_No_Auto_Validity,
        Post   => This.Exists_No_Auto_Validity and This.No_Auto_Validity = Value;

      function No_Auto_Validity (This : T) return Boolean with
        Global => null,
        Pre    => This.Exists_No_Auto_Validity;

      function Exists_No_Auto_Validity (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type Nullable_Optional_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Boolean;
         when False => null;
         end case;
      end record;

      type Nullable_No_Auto_Validity_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Boolean;
         when False => null;
         end case;
      end record;

      type T is tagged limited
         record
            My_Children         : aliased Child_Vectors.Vector;
            My_Optional         : Nullable_Optional_T;
            My_External_Sync    : Nullable_String_Ptr;
            My_Len              : Nullable_String_Ptr;
            My_No_Auto_Validity : Nullable_No_Auto_Validity_T;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

      function Optional (This : T) return Boolean is (This.My_Optional.Value);

      function Exists_Optional (This : T) return Boolean is (This.My_Optional.Exists);

      function External_Sync (This : T) return Aida.String_T is (This.My_External_Sync.Value.all);

      function Exists_External_Sync (This : T) return Boolean is (This.My_External_Sync.Exists);

      function Len (This : T) return Aida.String_T is (This.My_Len.Value.all);

      function Exists_Len (This : T) return Boolean is (This.My_Len.Exists);

      function No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Value);

      function Exists_No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Exists);

   end Param_Tag;

   package External_Sync_Parameter_Tag is

      type T is tagged limited private;

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Value,
        Post   => This.Exists_Value and This.Value = Value;

      function Value (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Value;

      function Exists_Value (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Value : Nullable_String_Ptr;
         end record;

      function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

      function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

   end External_Sync_Parameter_Tag;

   package Implicit_External_Sync_Parameters_Tag is

      type Child_Kind_Id_T is (
                               Child_External_Sync_Parameter
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_External_Sync_Parameter) is record
         case Kind_Id is
         when Child_External_Sync_Parameter => External_Sync_Parameter : External_Sync_Parameter_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Implicit_External_Sync_Parameters_Tag;

   package Command_Tag is

      use all type Ada.Containers.Count_Type;

      package Success_Code_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                  Element_Type => String_Ptr,
                                                                  "="          => "=");

      type Success_Codes_Ref (E : not null access constant Success_Code_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      package Error_Code_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                Element_Type => String_Ptr,
                                                                "="          => "=");

      type Error_Codes_Ref (E : not null access constant Error_Code_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type Child_Kind_Id_T is (
                               Child_Proto,
                               Child_Param,
                               Child_Validity,
                               Child_Implicit_External_Sync_Parameters
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Proto) is record
         case Kind_Id is
         when Child_Proto                             => Proto      : Proto_Tag.Ptr;
         when Child_Param                             => Param      : Param_Tag.Ptr;
         when Child_Validity                          => Validity   : Validity_Tag.Ptr;
         when Child_Implicit_External_Sync_Parameters => Parameters : Implicit_External_Sync_Parameters_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type Queue_T is (
                       Sparse_Binding,
                       Graphics,
                       Compute,
                       Transfer
                      );

      package Queue_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Queue_T,
                                                           "="          => "=");

      type Queues_Ref (E : not null access constant Queue_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type Render_Pass_T is (
                             Inside,
                             Outside,
                             Both
                            );

      package Render_Pass_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                 Element_Type => Render_Pass_T,
                                                                 "="          => "=");

      type Render_Passes_Ref (E : not null access constant Render_Pass_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type Command_Buffer_Level_T is (
                                      Primary,
                                      Secondary
                                     );

      package Command_Buffer_Level_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                          Element_Type => Command_Buffer_Level_T,
                                                                          "="          => "=");

      type Command_Buffer_Levels_Ref (E : not null access constant Command_Buffer_Level_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Success_Codes (This : aliased T) return Success_Codes_Ref;

      procedure Append_Success_Code (This : in out T;
                                     Item : Aida.String_T) with
        Global => null;--,
      --     Post   => This.Success_Codes'Result.Length = This.Success_Codes.Length + 1;

      function Error_Codes (This : aliased T) return Error_Codes_Ref;

      procedure Append_Error_Code (This : in out T;
                                   Item : Aida.String_T);

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Item : Child_T);

      function Queues (This : aliased T) return Queues_Ref;

      procedure Append_Queue (This : in out T;
                              Item : Queue_T);

      function Render_Passes (This : aliased T) return Render_Passes_Ref;

      procedure Append_Render_Pass (This : in out T;
                                    Item : Render_Pass_T);

      function Command_Buffer_Levels (This : aliased T) return Command_Buffer_Levels_Ref;

      procedure Append_Command_Buffer_Level (This : in out T;
                                             Item : Command_Buffer_Level_T);

      function To_String (This : T) return String;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Success_Codes         : aliased Success_Code_Vectors.Vector;
            My_Error_Codes           : aliased Error_Code_Vectors.Vector;
            My_Children              : aliased Child_Vectors.Vector;
            My_Queues                : aliased Queue_Vectors.Vector;
            My_Render_Passes         : aliased Render_Pass_Vectors.Vector;
            My_Command_Buffer_Levels : aliased Command_Buffer_Level_Vectors.Vector;
         end record;

      function Success_Codes (This : aliased T) return Success_Codes_Ref is ((E => This.My_Success_Codes'Access));

      function Error_Codes (This : aliased T) return Error_Codes_Ref is ((E => This.My_Error_Codes'Access));

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

      function Queues (This : aliased T) return Queues_Ref is ((E => This.My_Queues'Access));

      function Render_Passes (This : aliased T) return Render_Passes_Ref is ((E => This.My_Render_Passes'Access));

      function Command_Buffer_Levels (This : aliased T) return Command_Buffer_Levels_Ref is ((E => This.My_Command_Buffer_Levels'Access));

   end Command_Tag;

   package Commands_Tag is

      type Child_Kind_Id_T is (
                               Child_Command
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Command) is record
         case Kind_Id is
         when Child_Command => Command : Command_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Commands_Tag;

   package Require_Enum_Tag is

      type Offset_T is new Aida.Int32_T range 0..10;

      type Bit_Position_T is new Aida.Int32_T range 0..20;

      type T is tagged limited private;

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Value,
        Post   => This.Exists_Value and This.Value = Value;

      function Value (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Value;

      function Exists_Value (This : T) return Boolean with
        Global => null;

      procedure Set_Offset (This  : in out T;
                            Value : Offset_T) with
        Global => null,
        Pre    => not This.Exists_Offset,
        Post   => This.Exists_Offset and This.Offset = Value;

      function Offset (This : T) return Offset_T with
        Global => null,
        Pre    => This.Exists_Offset;

      function Exists_Offset (This : T) return Boolean with
        Global => null;

      procedure Set_Dir (This  : in out T;
                         Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Dir,
        Post   => This.Exists_Dir and This.Dir = Value;

      function Dir (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Dir;

      function Exists_Dir (This : T) return Boolean with
        Global => null;

      procedure Set_Comment (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Comment,
        Post   => This.Exists_Comment and This.Comment = Value;

      function Comment (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Comment;

      function Exists_Comment (This : T) return Boolean with
        Global => null;

      procedure Set_Extends (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Extends,
        Post   => This.Exists_Extends and This.Extends = Value;

      function Extends (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Extends;

      function Exists_Extends (This : T) return Boolean with
        Global => null;

      procedure Set_Bit_Position (This  : in out T;
                                  Value : Bit_Position_T) with
        Global => null,
        Pre    => not This.Exists_Bit_Position,
        Post   => This.Exists_Bit_Position and This.Bit_Position = Value;

      function Bit_Position (This : T) return Bit_Position_T with
        Global => null,
        Pre    => This.Exists_Bit_Position;

      function Exists_Bit_Position (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type Nullable_Offset_T (Exists : Boolean := False) is record
         case Exists is
         when True  => Value : Offset_T;
         when False => null;
         end case;
      end record;

      type Nullable_Bit_Position_T (Exists : Boolean := False) is record
         case Exists is
         when True  => Value : Bit_Position_T;
         when False => null;
         end case;
      end record;

      type T is tagged limited
         record
            My_Name         : Nullable_String_Ptr;
            My_Value        : Nullable_String_Ptr;
            My_Offset       : Nullable_Offset_T;
            My_Dir          : Nullable_String_Ptr;
            My_Extends      : Nullable_String_Ptr;
            My_Comment      : Nullable_String_Ptr;
            My_Bit_Position : Nullable_Bit_Position_T;
         end record;

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

      function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

      function Offset (This : T) return Offset_T is (This.My_Offset.Value);

      function Exists_Offset (This : T) return Boolean is (This.My_Offset.Exists);

      function Dir (This : T) return Aida.String_T is (This.My_Dir.Value.all);

      function Exists_Dir (This : T) return Boolean is (This.My_Dir.Exists);

      function Extends (This : T) return Aida.String_T is (This.My_Extends.Value.all);

      function Exists_Extends (This : T) return Boolean is (This.My_Extends.Exists);

      function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

      function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

      function Bit_Position (This : T) return Bit_Position_T is (This.My_Bit_Position.Value);

      function Exists_Bit_Position (This : T) return Boolean is (This.My_Bit_Position.Exists);

   end Require_Enum_Tag;

   package Require_Command_Tag is

      type T is tagged limited private;

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited record
         My_Name : Nullable_String_Ptr;
      end record;

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

   end Require_Command_Tag;

   package Require_Tag is

      type Child_Kind_Id_T is (
                               Child_Type,
                               Child_Enum,
                               Child_Command,
                               Child_Out_Commented_Message,
                               Child_Usage
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Type) is record
         case Kind_Id is
         when Child_Type                  => Type_V                : Type_Tag.Ptr;
         when Child_Enum                  => Enum                  : Require_Enum_Tag.Ptr;
         when Child_Command               => Command               : Require_Command_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : String_Ptr;
         when Child_Usage                 => Usage                 : Usage_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_Comment (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Comment,
        Post   => This.Exists_Comment and This.Comment = Value;

      function Comment (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Comment;

      function Exists_Comment (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Comment  : Nullable_String_Ptr;
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

      function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

      function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

   end Require_Tag;

   package Feature_Tag is

      type Child_Kind_Id_T is (
                               Child_Require
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Require) is record
         case Kind_Id is
         when Child_Require   => Require : Require_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_API (This  : in out T;
                         Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_API,
        Post   => This.Exists_API and This.API = Value;

      function API (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_API;

      function Exists_API (This : T) return Boolean with
        Global => null;

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Number (This  : in out T;
                            Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Number,
        Post   => This.Exists_Number and This.Number = Value;

      function Number (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Number;

      function Exists_Number (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_API      : Nullable_String_Ptr;
            My_Name     : Nullable_String_Ptr;
            My_Number   : Nullable_String_Ptr;
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function API (This : T) return Aida.String_T is (This.My_API.Value.all);

      function Exists_API (This : T) return Boolean is (This.My_API.Exists);

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Number (This : T) return Aida.String_T is (This.My_Number.Value.all);

      function Exists_Number (This : T) return Boolean is (This.My_Number.Exists);

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Feature_Tag;

   package Extension_Tag is

      type Number_T is new Positive range 1..30;

      type Supported_T is (
                           Vulkan,
                           Disabled
                          );

      type Child_Kind_Id_T is (
                               Child_Require
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Require) is record
         case Kind_Id is
         when Child_Require => Require : Require_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Name,
        Post   => This.Exists_Name and This.Name = Value;

      function Name (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Name;

      function Exists_Name (This : T) return Boolean with
        Global => null;

      procedure Set_Number (This  : in out T;
                            Value : Number_T) with
        Global => null,
        Pre    => not This.Exists_Number,
        Post   => This.Exists_Number and This.Number = Value;

      function Number (This : T) return Number_T with
        Global => null,
        Pre    => This.Exists_Number;

      function Exists_Number (This : T) return Boolean with
        Global => null;

      procedure Set_Supported (This  : in out T;
                               Value : Supported_T) with
        Global => null,
        Pre    => not This.Exists_Supported,
        Post   => This.Exists_Supported and This.Supported = Value;

      function Supported (This : T) return Supported_T with
        Global => null,
        Pre    => This.Exists_Supported;

      function Exists_Supported (This : T) return Boolean with
        Global => null;

      procedure Set_Protect (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Protect,
        Post   => This.Exists_Protect and This.Protect = Value;

      function Protect (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Protect;

      function Exists_Protect (This : T) return Boolean with
        Global => null;

      procedure Set_Author (This  : in out T;
                            Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Author,
        Post   => This.Exists_Author and This.Author = Value;

      function Author (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Author;

      function Exists_Author (This : T) return Boolean with
        Global => null;

      procedure Set_Contact (This  : in out T;
                             Value : Aida.String_T) with
        Global => null,
        Pre    => not This.Exists_Contact,
        Post   => This.Exists_Contact and This.Contact = Value;

      function Contact (This : T) return Aida.String_T with
        Global => null,
        Pre    => This.Exists_Contact;

      function Exists_Contact (This : T) return Boolean with
        Global => null;

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type Nullable_Number_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Number_T;
         when False => null;
         end case;
      end record;

      type Nullable_Supported_T (Exists : Boolean := False) is record
         case Exists is
         when True => Value : Supported_T;
         when False => null;
         end case;
      end record;

      type T is tagged limited
         record
            My_Name      : Nullable_String_Ptr;
            My_Number    : Nullable_Number_T;
            My_Supported : Nullable_Supported_T;
            My_Children  : aliased Child_Vectors.Vector;
            My_Protect   : Nullable_String_Ptr;
            My_Author    : Nullable_String_Ptr;
            My_Contact   : Nullable_String_Ptr;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

      function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

      function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

      function Number (This : T) return Number_T is (This.My_Number.Value);

      function Exists_Number (This : T) return Boolean is (This.My_Number.Exists);

      function Supported (This : T) return Supported_T is (This.My_Supported.Value);

      function Exists_Supported (This : T) return Boolean is (This.My_Supported.Exists);

      function Protect (This : T) return Aida.String_T is (This.My_Protect.Value.all);

      function Exists_Protect (This : T) return Boolean is (This.My_Protect.Exists);

      function Author (This : T) return Aida.String_T is (This.My_Author.Value.all);

      function Exists_Author (This : T) return Boolean is (This.My_Author.Exists);

      function Contact (This : T) return Aida.String_T is (This.My_Contact.Value.all);

      function Exists_Contact (This : T) return Boolean is (This.My_Contact.Exists);

   end Extension_Tag;

   package Extensions_Tag is

      type Child_Kind_Id_T is (
                               Child_Extension,
                               Child_Out_Commented_Message
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T := Child_Extension) is record
         case Kind_Id is
         when Child_Extension             => Extension             : Extension_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : String_Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited
         record
            My_Children : aliased Child_Vectors.Vector;
         end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Extensions_Tag;

   package Registry_Tag is

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
            when Child_XML_Text              => XML_Text              : String_Ptr;
            when Child_Comment               => Comment               : Comment_Tag.Ptr;
            when Child_Out_Commented_Message => Out_Commented_Message : String_Ptr;
            when Child_Vendor_Ids            => Vendor_Ids            : Vendor_Ids_Tag.Ptr;
            when Child_Tags                  => Tags                  : Tags_Tag.Ptr;
            when Child_Types                 => Types                 : Types_Tag.Ptr;
            when Child_Enums                 => Enums                 : Enums_Tag.Ptr;
            when Child_Commands              => Commands              : Commands_Tag.Ptr;
            when Child_Feature               => Feature               : Feature_Tag.Ptr;
            when Child_Extensions            => Extensions            : Extensions_Tag.Ptr;
         end case;
      end record;

      package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Child_T,
                                                           "="          => "=");

      type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
        Implicit_Dereference => E;

      type T is tagged limited private;

      function Children (This : aliased T) return Children_Ref;

      procedure Append_Child (This    : in out T;
                              Comment : not null Comment_Tag.Ptr);

      procedure Append_Child (This  : in out T;
                              Child : Child_T);

      type Ptr is access all T with Storage_Pool => Main_Pool;

   private

      type T is tagged limited record
         My_Children : aliased Child_Vectors.Vector;
      end record;

      function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   end Registry_Tag;

end Generic_Vk_XML;
