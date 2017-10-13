with Ada.Strings.Unbounded;
with Vk_XML.Nested_Type_Tag;
with Vk_XML.Name_Tag;

package body Vk_XML.Command_Tag is

   use all type Ada.Strings.Unbounded.Unbounded_String;

   use all type Vk_XML.Proto_Tag.Child_Kind_Id_T;

   procedure Append_Success_Code (This : in out T;
                                  Item : Aida.String_T;
                                  SP    : Dynamic_Pools.Subpool_Handle)
   is
      R : String_Ptr := new (SP) Aida.String_T'(Item);
   begin
      This.My_Success_Codes.Append (R);
   end Append_Success_Code;

   procedure Append_Error_Code (This : in out T;
                                Item : Aida.String_T;
                                SP    : Dynamic_Pools.Subpool_Handle)
   is
      R : String_Ptr := new (SP) Aida.String_T'(Item);
   begin
      This.My_Error_Codes.Append (R);
   end Append_Error_Code;

   procedure Append_Child (This  : in out T;
                           Item  : Child_T)
   is
   begin
      This.My_Children.Append (Item);
   end Append_Child;

   procedure Append_Queue (This : in out T;
                           Item : Queue_T)
   is
   begin
      This.My_Queues.Append (Item);
   end Append_Queue;

   procedure Append_Render_Pass (This : in out T;
                                 Item : Render_Pass_T)
   is
   begin
      This.My_Render_Passes.Append (Item);
   end Append_Render_Pass;

   procedure Append_Command_Buffer_Level (This : in out T;
                                          Item : Command_Buffer_Level_T)
   is
   begin
      This.My_Command_Buffer_Levels.Append (Item);
   end Append_Command_Buffer_Level;

   function To_String (This : T) return String is
      R : Ada.Strings.Unbounded.Unbounded_String;

      procedure Handle_Proto (Proto_V : Proto_Tag.Ptr) is

         procedure Handle_Nested_Type (Nested_Type_V : Nested_Type_Tag.Ptr) is
         begin
            Append (R, "<type>");

            if Nested_Type_V.Exists_Value then
               Append (R, String (Nested_Type_V.Value));
            end if;

            Append (R, "</type>");
         end Handle_Nested_Type;

         procedure Handle_Name (Name_V : Name_Tag.Ptr) is
         begin
            Append (R, "<name>");
            Append (R, String (Name_V.Value));
            Append (R, "</name>");
         end Handle_Name;

      begin
         for Child of Proto_V.Children loop
            case Child.Kind_Id is
               when Child_Nested_Type => Handle_Nested_Type (Child.Nested_Type);
               when Child_Name        => Handle_Name (Child.Name);
               when Child_Dummy       => null;
            end case;
         end loop;
      end Handle_Proto;

   begin
      Append (R, "<command >");
      for Child of This.Children loop
         case Child.Kind_Id is
            when Child_Dummy => null;
            when Child_Proto => Handle_Proto (Child.Proto);
            when Child_Param => null;
            when Child_Validity => null;
            when Child_Implicit_External_Sync_Parameters => null;
         end case;
      end loop;
      Append (R, "</command>");

      return To_String (R);
   end To_String;

end Vk_XML.Command_Tag;
