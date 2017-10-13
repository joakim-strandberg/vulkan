with Vk_XML.Type_Tag;
with Vk_XML.Require_Enum_Tag;
with Vk_XML.Require_Command_Tag;
with Vk_XML.Usage_Tag;

package Vk_XML.Require_Tag is

   type Child_Kind_Id_T is (
                            Child_Type,
                            Child_Enum,
                            Child_Command,
                            Child_Out_Commented_Message,
                            Child_Usage
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Out_Commented_Message) is record
      case Kind_Id is
         when Child_Type                  => Type_V                : not null Type_Tag.Ptr;
         when Child_Enum                  => Enum                  : not null Require_Enum_Tag.Ptr;
         when Child_Command               => Command               : not null Require_Command_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : not null String_Ptr := Empty_String'Access;
         when Child_Usage                 => Usage                 : not null Usage_Tag.Ptr;
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

end Vk_XML.Require_Tag;
