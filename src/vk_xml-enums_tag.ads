with Vk_XML.Enums_Enum_Tag;
with Vk_XML.Unused_Tag;
with Vk_XML.Comment_Tag;

package Vk_XML.Enums_Tag is

   type Child_Kind_Id_T is (
                            Child_Enums_Enum,
                            Child_Out_Commented_Message,
                            Child_Unused,
                            Child_Comment
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Out_Commented_Message) is record
      case Kind_Id is
         when Child_Enums_Enum            => Enums_Enum            : not null Enums_Enum_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : not null String_Ptr := Empty_String'Access;
         when Child_Unused                => Unused                : not null Unused_Tag.Ptr;
         when Child_Comment               => Comment               : not null Comment_Tag.Ptr;
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
                       Value : Aida.String_T;
                       SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : T) return Boolean with
     Global => null;

   procedure Set_Comment (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle) with
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

end Vk_XML.Enums_Tag;
