with Ada.Strings.Unbounded;

package body Vk_XML.Member_Tag is

   use all type Ada.Strings.Unbounded.Unbounded_String;

   procedure Set_Valid_Extension_Structs (This : in out T;
                                          Value : Aida.String_T;
                                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Valid_Extension_Structs := (Exists => True,
                                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Valid_Extension_Structs;

   procedure Set_No_Auto_Validity (This : in out T;
                                   Value : Boolean)
   is
   begin
      This.My_No_Auto_Validity := (Exists => True,
                                   Value  => Value);
   end Set_No_Auto_Validity;

   procedure Set_Len (This : in out T;
                      Value : Aida.String_T;
                      SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Len := (Exists => True,
                      Value  => new (SP) Aida.String_T'(Value));
   end Set_Len;

   procedure Set_Optional (This : in out T;
                           Value : Boolean)
   is
   begin
      This.My_Optional := (Exists => True,
                           Value  => Value);
   end Set_Optional;

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   function To_String (This : T) return Aida.String_T is
      S : Ada.Strings.Unbounded.Unbounded_String;

   begin
      if This.My_Children.Is_Empty then
         Append (S, "<member ");
         Append (S, "/>");
      else
         Append (S, "<member>");

         for Type_V of This.My_Children loop
            case Type_V.Kind_Id is
               when Child_Name =>
                  Append (S, "<name/>");
               when Child_Nested_Type =>
                  Append (S, "<type/>");
               when Child_Enum =>
                  Append (S, "<enum/>");
               when Child_XML_Text =>
                  Append (S, String (Type_V.XML_Text.all));
            end case;
         end loop;

         Append (S, "</member>");
      end if;

      return Aida.String_T (To_String (S));
   end To_String;

end Vk_XML.Member_Tag;
