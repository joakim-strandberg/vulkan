with Vk_XML.Name_Tag;
with Vk_XML.Member_Tag;
with Vk_XML.Validity_Tag;
with Vk_XML.Nested_Type_Tag;

-- <type category="struct" name="VkPhysicalDeviceProperties" returnedonly="true">
package Vk_XML.Type_Tag is

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

   procedure Set_Category (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Category,
     Post   => This.Exists_Category and This.Category = Value;

   function Category (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Category;

   function Exists_Category (This : T) return Boolean with
     Global => null;

   procedure Set_Requires (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Requires,
     Post   => This.Exists_Requires and This.Requires = Value;

   function Requires (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Requires;

   function Exists_Requires (This : T) return Boolean with
     Global => null;

   procedure Set_Parent (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
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

end Vk_XML.Type_Tag;
