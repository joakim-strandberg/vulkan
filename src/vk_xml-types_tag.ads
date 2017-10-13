with Vk_XML.Type_Tag;

package Vk_XML.Types_Tag is

   type Child_Kind_Id_T is (
                            Child_Type,
                            Child_Out_Commented_Message
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Out_Commented_Message) is record
      case Kind_Id is
         when Child_Type                  => Type_V                : not null Type_Tag.Ptr;
         when Child_Out_Commented_Message => Out_Commented_Message : not null String_Ptr := Empty_String'Access;
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

end Vk_XML.Types_Tag;
