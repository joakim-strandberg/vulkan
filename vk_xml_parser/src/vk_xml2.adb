package body Vk_XML2 is

   use all type Name.Value_T;
   use all type Proto.Child_Kind_Id_T;

   use all type Ada.Strings.Unbounded.Unbounded_String;

   use type Ada.Containers.Count_Type;

   package body Type_T is

      function To_String (This : T) return String is
         S : Ada.Strings.Unbounded.Unbounded_String;

         procedure Append_Name_To_String is
         begin
            if This.Name.Exists then
               Append (S, "name='");
               Append (S, To_String (This.Name.Value));
               Append (S, "' ");
            else
               null;
            end if;
         end Append_Name_To_String;

         procedure Append_Category_To_String is
         begin
            if Length (This.Category) = 0 then
               null;
            else
               Append (S, "category='");
               Append (S, To_String (This.Category));
               Append (S, "' ");
            end if;
         end Append_Category_To_String;

         procedure Append_Requires_To_String is
         begin
            if This.Requires.Exists then
               Append (S, "requires='");
               Append (S, To_String (This.Requires.Value));
               Append (S, "' ");
            else
               null;
            end if;
         end Append_Requires_To_String;

      begin
         if This.Children.Length = 0 then
            Append (S, "<type ");
            Append_Name_To_String;
            Append_Category_To_String;
            Append_Requires_To_String;
            Append (S, "/>");
         else
            Append (S, "<type ");
            Append_Name_To_String;
            Append_Category_To_String;
            Append_Requires_To_String;
            Append (S, ">");
            Append (S, "</type>");
         end if;

         return To_String (S);
      end To_String;

   end Type_T;

   package body Command is

      function To_String (This : T) return String is
         R : Ada.Strings.Unbounded.Unbounded_String;

         procedure Handle_Proto (Proto_V : Proto.Ptr) is

            procedure Handle_Nested_Type (Nested_Type_V : Nested_Type.Ptr) is
            begin
               Append (R, "<type>");

               if Nested_Type_V.Value.Exists then
                  Append (R, To_String (Nested_Type_V.Value.Value));
               end if;

               Append (R, "</type>");
            end Handle_Nested_Type;

            procedure Handle_Name (Name_V : Name.Ptr) is
            begin
               Append (R, "<name>");
               Append (R, To_String (Name_V.Value));
               Append (R, "</name>");
            end Handle_Name;

         begin
            for Child of Proto_V.Children loop
               case Child.Kind_Id is
                  when Child_Nested_Type => Handle_Nested_Type (Child.Nested_Type_V);
                  when Child_Name        => Handle_Name (Child.Name_V);
               end case;
            end loop;
         end Handle_Proto;

      begin
         Append (R, "<command >");
         for Child of This.Children loop
            case Child.Kind_Id is
               when Child_Proto => Handle_Proto (Child.Proto_V);
               when Child_Param => null;
               when Child_Validity => null;
               when Child_Implicit_External_Sync_Parameters => null;
            end case;
         end loop;
         Append (R, "</command>");

         return To_String (R);
      end To_String;
   end Command;

end Vk_XML2;
