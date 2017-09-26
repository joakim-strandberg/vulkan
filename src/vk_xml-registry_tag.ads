with Vk_XML.Comment_Tag;
with Vk_XML.Vendor_Ids_Tag;
with Vk_XML.Tags_Tag;
with Vk_XML.Types_Tag;
with Vk_XML.Enums_Tag;
with Vk_XML.Commands_Tag;
with Vk_XML.Feature_Tag;
with Vk_XML.Extensions_Tag;

package Vk_XML.Registry_Tag is

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

end Vk_XML.Registry_Tag;
