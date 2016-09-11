with Vk_XML;
with Aida.Text_IO;
with Aida.UTF8;
with Ada.Text_IO;
with Std_String;
with Aida.Strings;

package body Vk_Package_Creator with SPARK_Mode is

   use all type Vk_XML.Registry_Shared_Ptr.T;
   use all type Vk_XML.Registry.Fs.Child_Vectors.Immutable_T;
   use all type Vk_XML.Registry.Fs.Child_Kind_Id_T;
   use all type Vk_XML.XML_Out_Commented_Message_Shared_Ptr.T;
   use all type Vk_XML.Types_Shared_Ptr.T;
   use all type Vk_XML.Types.Fs.Child_Vectors.Immutable_T;
   use all type Vk_XML.Types.Fs.Child_Kind_Id_T;
   use all type Vk_XML.Type_Shared_Ptr.T;
   use all type Vk_XML.Enums.Fs.Child_Vectors.Immutable_T;
   use all type Vk_XML.Enums.Fs.Name.T;
   use all type Vk_XML.Enums.Fs.Child_Kind_Id_T;
   use all type Vk_XML.Enums_Shared_Ptr.T;
   use all type Vk_XML.Enums_Enum_Shared_Ptr.T;
   use all type Vk_XML.Enums_Enum.Fs.Value.T;
   use all type Vk_XML.Enums_Enum.Fs.Name.T;

   File : Ada.Text_IO.File_Type;

   procedure Put_Tabs (N : Natural) is
   begin
      for I in Natural range 1..N loop
         Ada.Text_IO.Put (File => File,
                          Item => "   ");
      end loop;
   end Put_Tabs;

   procedure Put_Line (Text : String) is
   begin
      Ada.Text_IO.Put_Line (File => File,
                            Item => Text);
   end Put_Line;

   procedure Put (Text : String) is
   begin
      Ada.Text_IO.Put (File => File,
                       Item => Text);
   end Put;

   -- Remove VK_ if the identifier begins with that
   -- Will not remove VK_ if it would result in a reserved word in Ada
   function Adaify_Constant_Name (N : String) return String is
   begin
      if Std_String.Starts_With (This         => N,
                                 Searched_For => "VK_")
      then
         if N'Length > 3 then
            declare
               Short_N : String := N (N'First + 3..N'Last);
            begin
               if Short_N = "TRUE" or Short_N = "FALSE" then
                  return N;
               else
                  return N (N'First + 3..N'Last);
               end if;
            end;
         else
            return N;
         end if;
      else
         return N;
      end if;
   end Adaify_Constant_Name;

--     procedure Generate_Struct_Name (Old_Name : String;
--                                     New_Name : in out Aida.Strings.Unbounded_String_Type)
--     is
--        P : Integer := Old_Name'First;
--
--        CP : Aida.UTF8.Code_Point := 0;
--
--        Is_Previous_Lowercase : Boolean := False;
--        Is_Previous_A_Number  : Boolean := False;
--        Is_Previous_An_Undercase  : Boolean := False;
--     begin
--        New_Name.Initialize ("");
--        Aida.UTF8.Get (Source  => Old_Name,
--                       Pointer => P,
--                       Value   => CP);
--
--        if Strings_Edit.UTF8.Mapping.Is_Uppercase (CP) then
--           New_Name.Append (Aida.UTF8.Image (CP));
--        else
--           New_Name.Append (Aida.UTF8.Image (Strings_Edit.UTF8.Mapping.To_Uppercase (CP)));
--        end if;
--
--        while P <= Old_Name'Last loop
--           Strings_Edit.UTF8.Get (Source  => Old_Name,
--                                  Pointer => P,
--                                  Value   => CP);
--
--           if Strings_Edit.UTF8.Image (CP) = "_" then
--              New_Name.Append ("_");
--              Is_Previous_An_Undercase := True;
--           else
--              if Strings_Edit.UTF8.Categorization.Is_Digit (CP) then
--                 if Is_Previous_A_Number then
--                    New_Name.Append (Aida.UTF8.Image (CP));
--                 else
--                    New_Name.Append ("_" & Strings_Edit.UTF8.Image (CP));
--                 end if;
--
--                 Is_Previous_A_Number := True;
--              else
--                 if Strings_Edit.UTF8.Mapping.Is_Uppercase (CP) then
--                    if Is_Previous_Lowercase then
--                       New_Name.Append ("_" & Strings_Edit.UTF8.Image (CP));
--                       Is_Previous_Lowercase := False;
--                    else
--                       New_Name.Append (Aida.UTF8.Image (Strings_Edit.UTF8.Mapping.To_Lowercase (CP)));
--                    end if;
--                 else
--                    if Is_Previous_An_Undercase then
--                       New_Name.Append (Aida.UTF8.Image (Strings_Edit.UTF8.Mapping.To_Uppercase (CP)));
--                    else
--                       New_Name.Append (Aida.UTF8.Image (CP));
--                    end if;
--                    Is_Previous_Lowercase := True;
--                 end if;
--
--                 Is_Previous_A_Number := False;
--              end if;
--
--              Is_Previous_An_Undercase := False;
--           end if;
--
--        end loop;
--     end Generate_Struct_Name;


   procedure Handle_Child_Type (Type_V : Vk_XML.Type_Shared_Ptr.T) is
   begin
      null;
   end Handle_Child_Type;

   procedure Handle_Out_Commented_Message (Out_Commented_Message_V : Vk_XML.XML_Out_Commented_Message_Shared_Ptr.T) is
   begin
      null;
      --        Aida.Text_IO.Put ("Out commented message:");
--        Aida.Text_IO.Put_Line (Vk_XML.XML_Out_Commented_Message_Shared_Ptr.To_String (Out_Commented_Message_V));
   end Handle_Out_Commented_Message;

   procedure Handle_Child_Types (Types_V : Vk_XML.Types_Shared_Ptr.T) is
   begin
      for I in Positive range First_Index (Children (Types_V))..Last_Index (Children (Types_V)) loop
         case Element (Children (Types_V), I).Kind_Id is
            when Child_XML_Dummy             => null;
            when Child_Type                  => Handle_Child_Type (Element (Children (Types_V), I).Type_V);
            when Child_Out_Commented_Message => Handle_Out_Commented_Message(Element (Children (Types_V), I).Out_Commented_Message_V);
         end case;
      end loop;
   end Handle_Child_Types;

   procedure Handle_API_Constants_Enum (Enum_V : Vk_XML.Enums_Enum_Shared_Ptr.T) is
   begin
      if Name (Enum_V).Exists then
         if Value (Enum_V).Exists then
            declare
               Has_Failed : Boolean;
               I : Integer;
               V : String := To_String (Value (Enum_V).Value_V);
               N : String := To_String (Name (Enum_V).Value);
            begin
               Std_String.To_Integer (Source => V,
                                      Target => I,
                                      Has_Failed => Has_Failed);

               if Has_Failed then
                  Aida.Text_IO.Put ("Could not convert '");
                  Aida.Text_IO.Put (V);
                  Aida.Text_IO.Put ("' to integer for ");
                  Aida.Text_IO.Put_Line (N);
               else
                  Put_Tabs (1);
                  Put (Adaify_Constant_Name (N));
                  Put (" : constant := ");
                  Put (V);
                  Put_Line (";");
               end if;
            end;
         else
            Aida.Text_IO.Put_Line ("A <enum> tag exists without Value attribute!?");
         end if;
      else
         Aida.Text_IO.Put_Line ("A <enum> tag exists without Name attribute!?");
      end if;
   end Handle_API_Constants_Enum;

   procedure Handle_Child_Enums_Enum (Enum_V : Vk_XML.Enums_Enum_Shared_Ptr.T) is
   begin
      null;
   end Handle_Child_Enums_Enum;

   procedure Handle_Registry_Child_Enums (Enums_V : Vk_XML.Enums_Shared_Ptr.T) is
   begin
      if Name (Enums_V).Exists then
         if To_String (Name (Enums_V).Value) = "API Constants" then
            for I in Positive range First_Index (Children (Enums_V))..Last_Index (Children (Enums_V)) loop
               case Element (Children (Enums_V), I).Kind_Id is
                  when Child_XML_Dummy             => null;
                  when Child_Enums_Enum            => Handle_API_Constants_Enum (Element (Children (Enums_V), I).Enums_Enum_V);
                  when Child_Out_Commented_Message => null;--Handle_Out_Commented_Message(Element (Children (Types_V), I).Out_Commented_Message_V);
                  when Child_Unused                => null;
               end case;
            end loop;
         else
            null;
         end if;
      else
         Aida.Text_IO.Put_Line ("A <enums> tag exists without Name attribute!?");
      end if;
   end Handle_Registry_Child_Enums;

   procedure Create_Vk_Package (R : Vk_XML.Registry_Shared_Ptr.T) is
   begin
      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => "vk.ads");
      Put_Line ("");
      Put_Line ("package Vk is");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("pragma Linker_Options (""-lvulkan-1"");");
      Put_Line ("");

      for I in Positive range First_Index (Children (R))..Last_Index (Children (R)) loop
         case Element (Children (R), I).Kind_Id is
            when Child_Comment =>
--                 Aida.Text_IO.Put ("Registry child comment with value:");
--                 Aida.Text_IO.Put_Line (Vk_XML.Comment.Fs.Value.To_String (Vk_XML.Comment_Shared_Ptr.Value (Vk_XML.Registry.Fs.Child_Vectors.Element (Children (R), I).C)));
               null;
            when Child_Out_Commented_Message =>
--                 Aida.Text_IO.Put ("Registry child out commented message:");
--                 Aida.Text_IO.Put_Line (Vk_XML.XML_Out_Commented_Message_Shared_Ptr.To_String (Vk_XML.Registry.Fs.Child_Vectors.Element (Children (R), I).Out_Commented_Message_V));
               null;
            when Child_Vendor_Ids =>
               null;
            when Child_Tags =>
               null;
            when Child_Types =>
               Handle_Child_Types (Element (Children (R), I).Types_V);
            when Child_Enums =>
               Handle_Registry_Child_Enums (Element (Children (R), I).Enums_V);
            when Child_Commands =>
               null;
            when Child_Feature =>
               null;
            when Child_Extensions =>
               null;
            when Child_XML_Dummy =>
               null;
            when Child_XML_Text =>
               null;
         end case;
      end loop;

      Put_Line ("");
      Put_Line ("end Vk;");

      Ada.Text_IO.Close (File);
   end Create_Vk_Package;

end Vk_Package_Creator;
