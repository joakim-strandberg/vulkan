with Aida.Text_IO;
with Aida.UTF8;
with Ada.Text_IO;
with Aida.UTF8_Code_Point;
with GNAT.Source_Info;
with Ada.Strings.Fixed.Hash;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with Vk_XML.Command_Tag;
with Vk_XML.Commands_Tag;
with Vk_XML.Comment_Tag;
with Vk_XML.Enum_Tag;
with Vk_XML.Enums_Enum_Tag;
with Vk_XML.Enums_Tag;
with Vk_XML.Extension_Tag;
with Vk_XML.Extensions_Tag;
with Vk_XML.External_Sync_Parameter_Tag;
with Vk_XML.Feature_Tag;
with Vk_XML.Implicit_External_Sync_Parameters_Tag;
with Vk_XML.Member_Tag;
with Vk_XML.Name_Tag;
with Vk_XML.Nested_Type_Tag;
with Vk_XML.Param_Tag;
with Vk_XML.Proto_Tag;
with Vk_XML.Registry_Tag;
with Vk_XML.Require_Command_Tag;
with Vk_XML.Require_Enum_Tag;
with Vk_XML.Require_Tag;
with Vk_XML.Tag_Tag;
with Vk_XML.Tags_Tag;
with Vk_XML.Type_Tag;
with Vk_XML.Types_Tag;
with Vk_XML.Unused_Tag;
with Vk_XML.Usage_Tag;
with Vk_XML.Validity_Tag;
with Vk_XML.Vendor_Id_Tag;
with Vk_XML.Vendor_Ids_Tag;

package body Vk_Package_Creator is

   use type Ada.Containers.Count_Type;

   use all type Aida.String_T;
   use all type Aida.Int32_T;

   use all type Aida.UTF8_Code_Point.T;
   use all type Ada.Strings.Unbounded.Unbounded_String;

   use all type Vk_XML.String_Ptr;
   use all type Vk_XML.Registry_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Vendor_Ids_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Tags_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Types_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Type_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Member_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Validity_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Usage_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Enums_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Enums_Tag.Type_Attribue_T;
   use all type Vk_XML.Commands_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Command_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Command_Tag.Queue_T;
   use all type Vk_XML.Command_Tag.Render_Pass_T;
   use all type Vk_XML.Command_Tag.Command_Buffer_Level_T;
   use all type Vk_XML.Proto_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Param_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Implicit_External_Sync_Parameters_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Feature_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Require_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Extension_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Extension_Tag.Supported_T;
   use all type Vk_XML.Extensions_Tag.Child_Kind_Id_T;
--     use all type Vk_XML.Vendor_Id_Tag.Name_T;
--     use all type Vk_XML.Vendor_Id_Tag.Id_T;
--     use all type Vk_XML.Vendor_Id_Tag.Comment_T;
--     use all type Vk_XML.Tag_Tag.Name_T;
--     use all type Vk_XML.Tag_Tag.Author_T;
--     use all type Vk_XML.Tag_Tag.Contact_T;
--     use all type Vk_XML.Type_Tag.Category_T;
--     use all type Vk_XML.Type_Tag.Returned_Only_T;
--     use all type Vk_XML.Member_Tag.No_Auto_Validity_T;
--     use all type Vk_XML.Member_Tag.Optional_T;
--     use all type Vk_XML.Command_Tag.Success_Code_T;
--     use all type Vk_XML.Command_Tag.Error_Code_T;
--     use all type Vk_XML.Param_Tag.Optional_T;
--     use all type Vk_XML.Comment.Value_T;
--     use all type Vk_XML.Name_T;
--     use all type Vk_XML.Nested_Type.Nullable_Value_T;
--     use all type Vk_XML.Enum.Value_T;

   use all type Vk_XML.Type_Tag.T;
   use all type Vk_XML.Command_Tag.T;

   use type Vk_XML.Param_Tag.Ptr;

--     use all type Member_Vectors.Vector;
--     use all type Struct_Type_Vectors.Vector;
--     use all type Param_Vectors.Vector;
--     use all type C_Type_Name_To_Ada_Name_Map_Owner.Map;

   type Generating_Code_For_OS_T is (
                                     Windows
                                    );

   Generating_Code_For_OS : Generating_Code_For_OS_T := Windows;

   T_End  : constant String := "_T";
   AT_End : constant String := "_Ptr";
   CAT_End : constant String := "_Const_Ptr";

   File : Ada.Text_IO.File_Type;

   function Hash_Of_Unbounded_String (Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Fixed.Hash (To_String (Key));
   end Hash_Of_Unbounded_String;

   use type C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

   C_Type_Name_To_Ada_Name_Map : C_Type_Name_To_Ada_Name_Map_Owner.Map;

   procedure Put_Tabs (N : Natural) is
   begin
      for I in Natural range 1..N loop
         Ada.Text_IO.Put (File => File,
                          Item => "   ");
      end loop;
   end Put_Tabs;

   procedure Put_Line (Text : Aida.String_T) is
   begin
      Ada.Text_IO.Put_Line (File => File,
                            Item => String (Text));
   end Put_Line;

   -- To be used when printing to file using unbounded string
   procedure Puts_Line (Text : String) is
   begin
      Ada.Text_IO.Put_Line (File => File,
                            Item => Text);
   end Puts_Line;

   procedure Put (Text : Aida.String_T) is
   begin
      Ada.Text_IO.Put (File => File,
                       Item => String (Text));
   end Put;

   -- To be used when printing to file using unbounded string
   procedure Puts (Text : String) is
   begin
      Ada.Text_IO.Put (File => File,
                       Item => Text);
   end Puts;

   -- Remove VK_ if the identifier begins with that
   -- Will not remove VK_ if it would result in a reserved word in Ada
   function Adaify_Constant_Name (N : Aida.String_T) return Aida.String_T is
   begin
      if Starts_With (This         => N,
                      Searched_For => "VK_")
      then
         if N'Length > 3 then
            declare
               Short_N : Aida.String_T := N (N'First + 3..N'Last);
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

   procedure Remove_Initial_Vk (New_Name : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if
        Length (New_Name) = 3 and then
        New_Name = "Vk_"
      then
         Set_Unbounded_String (New_Name, "");
         --        else
         --           Aida.Text_IO.Put_Line (New_Name.To_String & " != Vk_");
      end if;
   end Remove_Initial_Vk;

   procedure Make_Upper_Case (Source : Aida.String_T;
                              Target : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      P : Integer := Source'First;

      CP : Aida.UTF8_Code_Point.T := 0;
   begin
      while P <= Source'Last loop
         Aida.UTF8.Get (Source  => Source,
                        Pointer => P,
                        Value   => CP);

         if Image (CP) = "_" then
            Append (Target, "_");
         elsif Is_Digit (CP) then
            Append (Target, String (Image (CP)));
         elsif Is_Lowercase (CP) then
            Append (Target, String (Image (To_Uppercase (CP))));
         else
            Append (Target, String (Image (CP)));
         end if;

      end loop;
   end Make_Upper_Case;

   procedure Hidden_Adaify_Name (Old_Name : Aida.String_T;
                                 New_Name : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      P : Integer := Old_Name'First;

      CP : Aida.UTF8_Code_Point.T := 0;

      Is_Previous_Lowercase : Boolean := False;
      Is_Previous_A_Number  : Boolean := False;
      Is_Previous_An_Undercase  : Boolean := False;
   begin
      Set_Unbounded_String (New_Name, "");
      Aida.UTF8.Get (Source  => Old_Name,
                     Pointer => P,
                     Value   => CP);

      if Is_Uppercase (CP) then
         Append (New_Name, String (Image (CP)));
      else
         Append (New_Name, String (Image (To_Uppercase (CP))));
      end if;

      while P <= Old_Name'Last loop
         Aida.UTF8.Get (Source  => Old_Name,
                        Pointer => P,
                        Value   => CP);

         if Image (CP) = "_" then
            Append (New_Name, "_");
            Remove_Initial_Vk (New_Name);
            Is_Previous_An_Undercase := True;
         else
            if Is_Digit (CP) then
               if Is_Previous_A_Number then
                  Append (New_Name, String (Image (CP)));
               else
                  Append (New_Name, "_");
                  Remove_Initial_Vk (New_Name);
                  Append (New_Name, String (Image (CP)));
               end if;

               Is_Previous_A_Number := True;
            else
               if Is_Uppercase (CP) then
                  if Is_Previous_Lowercase then
                     Append (New_Name, "_");
                     Remove_Initial_Vk (New_Name);
                     Append (New_Name, String (Image (CP)));
                     Is_Previous_Lowercase := False;
                  else
                     Append (New_Name, String (Image (To_Lowercase (CP))));
                  end if;
               else
                  if Is_Previous_An_Undercase then
                     Append (New_Name, String (Image (To_Uppercase (CP))));
                  else
                     Append (New_Name, String (Image (CP)));
                  end if;
                  Is_Previous_Lowercase := True;
               end if;

               Is_Previous_A_Number := False;
            end if;

            Is_Previous_An_Undercase := False;
         end if;

      end loop;

      if New_Name = "Range" then
         Set_Unbounded_String (New_Name, "The_Range");
      end if;

      if New_Name = "Type" then
         Set_Unbounded_String (New_Name, "The_Type");
      end if;
   end Hidden_Adaify_Name;

   procedure Adaify_Name (Old_Name : Aida.String_T;
                          New_Name : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      P : Integer := Old_Name'First;

      CP : Aida.UTF8_Code_Point.T := 0;
   begin
      -- Some variables begin with a miniscule p followed by a capital letter to indicate a pointer.
      -- Remove the p-prefix.
      if
        Old_Name'Length > 4 and then
        Old_Name (Old_Name'First) = 'p'
      then
         if Old_Name (Old_Name'First + 1) = 'p' then
            P := Old_Name'First + 2;
            Aida.UTF8.Get (Source  => Old_Name,
                           Pointer => P,
                           Value   => CP);
            if Is_Uppercase (CP) then
               Hidden_Adaify_Name (Old_Name (Old_Name'First + 2..Old_Name'Last), New_Name);
            else
               Hidden_Adaify_Name (Old_Name, New_Name);
            end if;
         else
            P := Old_Name'First + 1;
            Aida.UTF8.Get (Source  => Old_Name,
                           Pointer => P,
                           Value   => CP);
            if Is_Uppercase (CP) then
               Hidden_Adaify_Name (Old_Name (Old_Name'First + 1..Old_Name'Last), New_Name);
            else
               Hidden_Adaify_Name (Old_Name, New_Name);
            end if;
         end if;

      else
         Hidden_Adaify_Name (Old_Name, New_Name);
      end if;
   end Adaify_Name;

   procedure Adaify_Array_Index_Type_Name (Old_Name : Aida.String_T;
                                           New_Name : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      Extension : String := "_Array_Index" & T_End;
   begin
      Adaify_Name (Old_Name => Old_Name,
                   New_Name => New_Name);

      Append (New_Name, Extension);
   end Adaify_Array_Index_Type_Name;

   procedure Adaify_Array_Type_Name (Old_Name : Aida.String_T;
                                     New_Name : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      Extension : String := "_Array" & T_End;
   begin
      Adaify_Name (Old_Name => Old_Name,
                   New_Name => New_Name);

      Append (New_Name, Extension);
   end Adaify_Array_Type_Name;

   procedure Adaify_Array_Conversions_Package (Source : Aida.String_T;
                                               Target : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      Extension : String := "_Array_Conversions";
   begin
      Adaify_Name (Old_Name => Source,
                   New_Name => Target);

      Append (Target, Extension);
   end Adaify_Array_Conversions_Package;

   procedure Adaify_Type_Name (Old_Name : Aida.String_T;
                               New_Name : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Adaify_Name (Old_Name => Old_Name,
                   New_Name => New_Name);
      -- if statement to handle the cases where Old_Name is Uint_32_T and Size_T.
--        if Old_Name (Old_Name'Last-1..Old_Name'Last) /= "_t" then
--           Adaify_Name (Old_Name => Old_Name,
--                        New_Name => New_Name);
--        else
--           Adaify_Name (Old_Name => Old_Name (Old_Name'First..Old_Name'Last-2),
--                        New_Name => New_Name);
--        end if;

      declare
         R : String := To_String (New_Name);
      begin
         if
           R'Length > 2 and then
           (R (R'Last-2..R'Last) = "2_D" or
                R (R'Last-2..R'Last) = "3_D")
         then
            Set_Unbounded_String (New_Name, R (R'First..R'Last-2) & "D");
         end if;
      end;

      Append (New_Name, T_End);
   end Adaify_Type_Name;

   procedure Adaify_Access_Type_Name (Old_Name : Aida.String_T;
                                      New_Name : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Old_Name (Old_Name'Last-1..Old_Name'Last) /= "_t" then
         Adaify_Name (Old_Name => Old_Name,
                      New_Name => New_Name);
      else
         Adaify_Name (Old_Name => Old_Name (Old_Name'First..Old_Name'Last-2),
                      New_Name => New_Name);
      end if;

      declare
         R : String := To_String (New_Name);
      begin
         if
           R'Length > 2 and then
           (R (R'Last-2..R'Last) = "2_D" or
                R (R'Last-2..R'Last) = "3_D")
         then
            Set_Unbounded_String (New_Name, R (R'First..R'Last-2) & "D");
         end if;
      end;

      Append (New_Name, AT_End);
   end Adaify_Access_Type_Name;

   procedure Adaify_Constant_Access_Type_Name (Old_Name : Aida.String_T;
                                               New_Name : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Old_Name (Old_Name'Last-1..Old_Name'Last) /= "_t" then
         Adaify_Name (Old_Name => Old_Name,
                      New_Name => New_Name);
      else
         Adaify_Name (Old_Name => Old_Name (Old_Name'First..Old_Name'Last-2),
                      New_Name => New_Name);
      end if;

      declare
         R : String := To_String (New_Name);
      begin
         if
           R'Length > 2 and then
           (R (R'Last-2..R'Last) = "2_D" or
                R (R'Last-2..R'Last) = "3_D")
         then
            Set_Unbounded_String (New_Name, R (R'First..R'Last-2) & "D");
         end if;
      end;

      Append (New_Name, CAT_End);
   end Adaify_Constant_Access_Type_Name;

   function Value_Of_Bit (B : Vk_XML.Enums_Enum_Tag.Bit_Position_T) return Long_Integer is
   begin
      return 2 ** Integer (B);
   end Value_Of_Bit;

   VOID_C_TYPE : constant String := "void";

   procedure Handle_Child_Type (Type_V : Vk_XML.Type_Tag.T;
                                R      : Vk_XML.Registry_Tag.T)
   is

      procedure Generate_Code_For_Enum_Bitmask_If_Found (Searched_For     : Aida.String_T;
                                                         Parent_Type_Name : Ada.Strings.Unbounded.Unbounded_String) is
         Shall_Continue_Search : Boolean := True;

         procedure Search_Enum_Tags_And_Generate_Code_If_Found (Enums_V : Vk_XML.Enums_Tag.Ptr) is

            procedure Auto_Generate_Code_For_Found_Enum is

               Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;

               procedure Handle_Enum_Bitmask (Enum_V : Vk_XML.Enums_Enum_Tag.Ptr) is
               begin
                  if Enum_V.Exists_Name then
                     if Enum_V.Exists_Bit_Position then
                        declare
                           V : Vk_XML.Enums_Enum_Tag.Bit_Position_T := Enum_V.Bit_Position;
                           N : Aida.String_T := Enum_V.Name;
                        begin
                           Put_Tabs (1);
                           Put (Adaify_Constant_Name (N));
                           Put (" : ");
                           Put (Aida.String_T (To_String (Adafied_Name)));
                           Put (" := ");
                           Put (Aida.String_T (Value_Of_Bit (V)'Img));
                           Put (";");

                           if Enum_V.Exists_Comment then
                              Put (" -- ");
                              Put (Enum_V.Comment);
                           end if;
                           Put_Line ("");
                        end;
                     elsif Enum_V.Exists_Value then
                        declare
                           V : Ada.Strings.Unbounded.Unbounded_String;
                           Extracted_Text : Ada.Strings.Unbounded.Unbounded_String;
                        begin
                           Set_Unbounded_String (V, String (Enum_V.Value));
                           if
                             Index (V, "0x") = 1
                           then
                              Extracted_Text := Unbounded_Slice (Source => V,
                                                                 Low    => 3,
                                                                 High   => Length (V));
                              Put_Tabs (1);
                              Put (Adaify_Constant_Name (Enum_V.Name));
                              Put (" : ");
                              Put (Aida.String_T (To_String (Adafied_Name)));
                              Put (" := 16#");
                              Put (Aida.String_T (To_String (Extracted_Text)));
                              Put ("#;");

                              if Enum_V.Exists_Comment then
                                 Put (" -- ");
                                 Put (Enum_V.Comment);
                              end if;
                              Put_Line ("");
                           elsif V = "0" then
                              Put_Tabs (1);
                              Put (Adaify_Constant_Name (Enum_V.Name));
                              Put (" : ");
                              Put (Aida.String_T (To_String (Adafied_Name)));
                              Put (" := ");
                              Puts (To_String (V));
                              Put (";");

                              if Enum_V.Exists_Comment then
                                 Put (" -- ");
                                 Put (Enum_V.Comment);
                              end if;
                              Put_Line ("");
                           else
                              Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Cannot interpret value of  <enum> tag!? ");
--                                To_Standard_Out (Enum_V);
                           end if;
                        end;
                     else
                        Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", A <enum> tag exists without Bit position attribute!? ");
--                          To_Standard_Out (Enum_V);
                     end if;
                  else
                     Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", A <enum> tag exists without Name attribute!?");
                  end if;
               end Handle_Enum_Bitmask;

               Is_First_Enum : Boolean := True;

               Name_To_Adafy : Aida.String_T := Enums_V.Name;
            begin
               Adaify_Type_Name (Old_Name => Name_To_Adafy,
                                 New_Name => Adafied_Name);

               Put_Tabs (1);
               Put ("type ");
               Puts (To_String (Adafied_Name));
               Put (" is new ");
               Puts (To_String (Parent_Type_Name));
               Put_Line (";");

               declare
                  Old_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  Set_Unbounded_String (Old_Type_Name, String (Name_To_Adafy));
                  C_Type_Name_To_Ada_Name_Map.Insert (Key       => Old_Type_Name,
                                                      New_Item  => Adafied_Name);
               end;

               for Enum_V of Enums_V.Children loop
                  case Enum_V.Kind_Id is
                     when Child_Enums_Enum => Handle_Enum_Bitmask (Enum_V.Enums_Enum);
                     when others           => null;
                  end case;
               end loop;

               Put_Line ("");
            end Auto_Generate_Code_For_Found_Enum;

         begin
            if Enums_V.Exists_Name then
               if Enums_V.Name = Searched_For then
                  if Enums_V.Exists_Type_Attribute then
                     case Enums_V.Type_Attribute is
                        when Enum     => null;
                        when Bit_Mask =>
                           Auto_Generate_Code_For_Found_Enum;
                           Shall_Continue_Search := False;
                     end case;
                  end if;
               end if;
            end if;
         end Search_Enum_Tags_And_Generate_Code_If_Found;

      begin
         for Registry_Child of R.Children loop
            case Registry_Child.Kind_Id is
               when Child_Enums =>
                  Search_Enum_Tags_And_Generate_Code_If_Found (Registry_Child.Enums);
               when others =>
                  null;
            end case;

            if not Shall_Continue_Search then
               exit;
            end if;
         end loop;

         if Shall_Continue_Search then
            Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", could not find enum bitmask with name ");
            Aida.Text_IO.Put_Line (To_String (Type_V));
         end if;
      end Generate_Code_For_Enum_Bitmask_If_Found;

   begin
      if
        (Type_V.Exists_Category and then Type_V.Category = "include") and not Type_V.Exists_Requires
      then
         null; -- ignore for example <type category="include">#include "<name>vulkan.h</name>"</type>
      elsif
        Type_V.Exists_Requires and then
        Type_V.Exists_Name and then (
                                       Type_V.Requires = "X11/Xlib.h" or
                                         Type_V.Requires = "android/native_window.h" or
                                           Type_V.Requires = "mir_toolkit/client_types.h" or
                                         Type_V.Requires = "wayland-client.h" or
                                           Type_V.Requires = "windows.h" or
                                         Type_V.Requires = "xcb/xcb.h"
                                      )
      then
         null; -- ignore for example <type requires="android/native_window.h" name="ANativeWindow"/>
      elsif Type_V.Exists_Category and then Type_V.Category = "define" then
         null; -- ignore for example <type category="define">#define <name>VK_VERSION_MAJOR</name>(version) ((uint32_t)(version) &gt;&gt; 22)</type>
      elsif
        Type_V.Exists_Category and then Type_V.Category = "basetype" and then
        Type_V.Children.Length = 4
      then
         declare
            Typedef_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index);
            Type_Element    : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 1);
            Name_Element    : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 2);
         begin
            if
              Typedef_Element.Kind_Id = Child_XML_Text and then
              Typedef_Element.XML_Text.all = "typedef "
            then
               if
                 Name_Element.Kind_Id = Child_Name
               then
                  if
                    Type_Element.Kind_Id = Child_Nested_Type and then
                    Type_Element.Nested_Type.Exists_Value
                  then
                     declare
                        New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                        Parent_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                     begin
                        if Type_Element.Nested_Type.Value = "uint32_t" then
                           Append (Parent_Type_Name, "Interfaces.Unsigned_32");
                        elsif Type_Element.Nested_Type.Value = "uint64_t" then
                           Append (Parent_Type_Name, "Interfaces.Unsigned_64");
                        end if;

                        if Length (Parent_Type_Name) > 0 then
                           Adaify_Type_Name (Old_Name => Name_Element.Name.Value,
                                             New_Name => New_Type_Name);
                           Put_Tabs (1);
                           Put ("type ");
                           Puts (To_String (New_Type_Name));
                           Put (" is new ");
                           Puts (To_String (Parent_Type_Name));
                           Put_Line (";");
                           Put_Line ("");

                           declare
                              C_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                           begin
                              Set_Unbounded_String (C_Type_Name, String (Name_Element.Name.Value));
                              C_Type_Name_To_Ada_Name_Map_Owner.Insert (Container => C_Type_Name_To_Ada_Name_Map,
                                                                        Key       => C_Type_Name,
                                                                        New_Item  => New_Type_Name);
                           end;
                        else
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Erroneous conversion of ");
                           Aida.Text_IO.Put_Line (To_String (Type_V));
                        end if;
                     end;
                  else
                     Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                     Aida.Text_IO.Put_Line (To_String (Type_V));
                  end if;
               else
                  Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                  Aida.Text_IO.Put_Line (To_String (Type_V));
               end if;
            else
               Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
               Aida.Text_IO.Put_Line (To_String (Type_V));
            end if;
         end;
      elsif
        Type_V.Exists_Requires and then
        Type_V.Requires = "vk_platform" and then
        Type_V.Exists_Name and then (
                                       Type_V.Name = "void" or
                                         Type_V.Name = "char" or
                                           Type_V.Name = "float" or
                                         Type_V.Name = "uint8_t" or
                                           Type_V.Name = "uint32_t" or
                                         Type_V.Name = "uint64_t" or
                                           Type_V.Name = "int32_t" or
                                         Type_V.Name = "size_t"
                                      )
      then
         null;
      elsif
        Type_V.Exists_Category and then Type_V.Category = "bitmask" and then
        Type_V.Children.Length = 4
      then
         declare
            Typedef_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 0);
            Type_Element    : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 1);
            Name_Element    : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 2);
         begin
            if
              Typedef_Element.Kind_Id = Child_XML_Text and then
              Typedef_Element.XML_Text.all = "typedef "
            then
               if
                 Name_Element.Kind_Id = Child_Name
               then
                  if
                    Type_Element.Kind_Id = Child_Nested_Type and then
                    Type_Element.Nested_Type.Exists_Value
                  then
                     declare
                        New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                        Parent_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                        Searched_For : Ada.Strings.Unbounded.Unbounded_String;

                        Cursor_V : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;
                     begin
                        Set_Unbounded_String (Searched_For, String (Type_Element.Nested_Type.Value));

                        Cursor_V := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                            Key       => Searched_For);

                        if Cursor_V /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                           Adaify_Type_Name (Old_Name => Type_Element.Nested_Type.Value,
                                             New_Name => Parent_Type_Name);
                        end if;

                        if Length (Parent_Type_Name) > 0 then
                           Adaify_Type_Name (Old_Name => Name_Element.Name.Value,
                                             New_Name => New_Type_Name);
                           Put_Tabs (1);
                           Put ("type ");
                           Puts (To_String (New_Type_Name));
                           Put (" is new ");
                           Puts (To_String (Parent_Type_Name));
                           Put_Line (";");
                           Put_Line ("");

                           declare
                              C_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                           begin
                              Set_Unbounded_String (C_Type_Name, String (Name_Element.Name.Value));
                              C_Type_Name_To_Ada_Name_Map_Owner.Insert (Container => C_Type_Name_To_Ada_Name_Map,
                                                                        Key       => C_Type_Name,
                                                                        New_Item  => New_Type_Name);
                           end;

                           if Type_V.Exists_Requires then
                              Generate_Code_For_Enum_Bitmask_If_Found (Type_V.Requires,
                                                                       New_Type_Name);
                           end if;
                        else
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Erroneous conversion of ");
                           Aida.Text_IO.Put_Line (To_String (Type_V));
                        end if;
                     end;
                  else
                     Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                     Aida.Text_IO.Put_Line (To_String (Type_V));
                  end if;
               else
                  Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                  Aida.Text_IO.Put_Line (To_String (Type_V));
               end if;
            else
               Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
               Aida.Text_IO.Put_Line (To_String (Type_V));
            end if;
         end;
      elsif
        Type_V.Exists_Category and then Type_V.Category = "handle" and then
        Type_V.Children.Length = 4
      then
         declare
            Nested_Type_Element   : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index);
            Left_Bracket_Element  : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 1);
            Name_Element          : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 2);
            Right_Bracket_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 3);
         begin
            if
              (Left_Bracket_Element.Kind_Id = Child_XML_Text and then
               Left_Bracket_Element.XML_Text.all = "(") and
              (Right_Bracket_Element.Kind_Id = Child_XML_Text and then
               Right_Bracket_Element.XML_Text.all = ")") and
              (Nested_Type_Element.Kind_Id = Child_Nested_Type and then
               Nested_Type_Element.Nested_Type.Exists_Value and then
                   (Nested_Type_Element.Nested_Type.Value = "VK_DEFINE_HANDLE" or Nested_Type_Element.Nested_Type.Value = "VK_DEFINE_NON_DISPATCHABLE_HANDLE")) and
              Name_Element.Kind_Id = Child_Name
            then
               declare
                  Old_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                  New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  Adaify_Type_Name (Old_Name => Name_Element.Name.Value,
                                    New_Name => New_Type_Name);

                  Put_Tabs (1);
                  Put ("type ");
                  Puts (To_String (New_Type_Name));
                  Put_Line (" is new System.Address;");
                  Put_Line ("");

                  Set_Unbounded_String (Old_Type_Name, String (Name_Element.Name.Value));

                  C_Type_Name_To_Ada_Name_Map.Insert (Key      => Old_Type_Name,
                                                      New_Item => New_Type_Name);
               end;
            end if;
         end;
      elsif
        Type_V.Exists_Category and then Type_V.Category = "enum" and then
        Type_V.Exists_Name
      then
         null; -- Skip these since they are generated from the enum type definitions
         -- It should be checked that all expected enum type definitions has been generated in this step!
         -- TODO: Add this extra nice feature!
      elsif
        Type_V.Exists_Category and then Type_V.Category = "funcpointer" and then
        not Type_V.Exists_Name
      then
         declare
            Typedef_Void_VKAPI_Ptr_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index);
            Procedure_Name_Element         : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 1);
         begin
            if (Typedef_Void_VKAPI_Ptr_Element.Kind_Id = Child_XML_Text and then
                Typedef_Void_VKAPI_Ptr_Element.XML_Text.all = "typedef void (VKAPI_PTR *") and
              Procedure_Name_Element.Kind_Id = Child_Name
            then
               declare
                  Old_Name     : Ada.Strings.Unbounded.Unbounded_String;
                  New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  Set_Unbounded_String (Old_Name, String (Procedure_Name_Element.Name.Value));
                  Adaify_Type_Name (Old_Name => Procedure_Name_Element.Name.Value,
                                    New_Name => New_Type_Name);

                  if
                    Type_V.Children.Length = 3 and then
                    Type_V.Children.Element (Type_V.Children.First_Index + 2).Kind_Id = Child_XML_Text and then
                    Type_V.Children.Element (Type_V.Children.First_Index + 2).XML_Text.all = ")(void);"
                  then
                     Put_Tabs (1);
                     Put ("type ");
                     Puts (To_String (New_Type_Name));
                     Put_Line (" is access procedure;");
                     if Generating_Code_For_OS = Windows then
                        Put_Tabs (1); Puts_Line ("pragma Convention (Stdcall, " & To_String (New_Type_Name) & ");");
                     else
                        Put_Tabs (1); Puts_Line ("pragma Convention (C, " & To_String (New_Type_Name) & ");");
                     end if;
                     Put_Line ("");

                     C_Type_Name_To_Ada_Name_Map_Owner.Insert (Container => C_Type_Name_To_Ada_Name_Map,
                                                               Key       => Old_Name,
                                                               New_Item  => New_Type_Name);
                  else
                     Put_Tabs (1);
                     Put ("type ");
                     Puts (To_String (New_Type_Name));
                     Put_Line (" is access procedure (");

                     C_Type_Name_To_Ada_Name_Map_Owner.Insert (Container => C_Type_Name_To_Ada_Name_Map,
                                                               Key       => Old_Name,
                                                               New_Item  => New_Type_Name);

                     for I in Positive range Type_V.Children.First_Index + 3..(Type_V.Children.Last_Index - 1) loop
                        declare
                           Nested_Type_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (I);
                           Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                           C_Var_Name_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (I + 1);
                           Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;
                        begin
                           if
                             Nested_Type_Element.Kind_Id = Child_Nested_Type and then
                             Nested_Type_Element.Nested_Type.Exists_Value and then
                             C_Var_Name_Element.Kind_Id = Child_XML_Text
                           then
                              declare
                                 C_Var_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 Comma_Index : Natural;
                              begin
                                 Set_Unbounded_String (C_Var_Name, String (C_Var_Name_Element.XML_Text.all));
                                 Comma_Index := Ada.Strings.Unbounded.Index (C_Var_Name, ",");

                                 if Comma_Index > 0 then
                                    declare
                                       Total         : String := To_String (C_Var_Name);
                                       N_With_Spaces : String := Total (Total'First+1..Comma_Index - 1);
                                       N             : Aida.String_T := Aida.String_T (Ada.Strings.Fixed.Trim (Source => N_With_Spaces,
                                                                                                               Side   => Ada.Strings.Both));
                                       Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                                    begin
                                       Adaify_Name (Old_Name => N,
                                                    New_Name => Adafied_Name);
                                       if Total (Total'First) = '*' then
                                          Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value) & "*");

                                          Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                         Key       => Nested_Type_Name);

                                          if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                             Put_Tabs (2);
                                             Puts (To_String (Adafied_Name));
                                             Put (" : ");
                                             Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                             Put_Line (";");
                                          else
                                             Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                                          end if;
                                       else
                                          Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value));

                                          Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                         Key       => Nested_Type_Name);

                                          if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                             Put_Tabs (2);
                                             Puts (To_String (Adafied_Name));
                                             Put (" : ");
                                             Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                             Put_Line (";");
                                          else
                                             Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                                             Aida.Text_IO.Put_Line (To_String (Type_V));
                                          end if;
                                       end if;
                                    end;
                                 else
                                    Comma_Index := Index (C_Var_Name, ")");

                                    if Comma_Index > 0 then
                                       declare
                                          Total         : String := To_String (C_Var_Name);
                                          N_With_Spaces : String := Total (Total'First+1..Comma_Index - 1);
                                          N             : Aida.String_T := Aida.String_T (Ada.Strings.Fixed.Trim (Source => N_With_Spaces,
                                                                                                                  Side   => Ada.Strings.Both));

                                          Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                                       begin
                                          Set_Unbounded_String (Old_Name, String (N));
                                          Adaify_Name (Old_Name => N,
                                                       New_Name => Adafied_Name);
                                          if Total (Total'First) = '*' then
                                             Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value) & "*");

                                             Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                            Key       => Nested_Type_Name);

                                             if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                                Put_Tabs (2);
                                                Puts (To_String (Adafied_Name));
                                                Put (" : ");
                                                Puts_Line (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                             else
                                                Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                                             end if;
                                          else
                                             Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value));

                                             Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                            Key       => Nested_Type_Name);

                                             if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                                Put_Tabs (2);
                                                Puts (To_String (Adafied_Name));
                                                Put (" : ");
                                                Puts_Line (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                             else
                                                Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                                                Aida.Text_IO.Put_Line (To_String (Type_V));
                                             end if;
                                          end if;
                                       end;
                                    else
                                       Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", has not found ) for");
                                       Aida.Text_IO.Put_Line (To_String (Type_V));
                                    end if;
                                 end if;
                              end;
                           end if;
                        end;
                     end loop;

                     Put_Tabs (2);
                     Put_Line (");");
                     if Generating_Code_For_OS = Windows then
                        Put_Tabs (1); Puts_Line ("pragma Convention (Stdcall, " & To_String (New_Type_Name) & ");");
                     else
                        Put_Tabs (1); Puts_Line ("pragma Convention (C, " & To_String (New_Type_Name) & ");");
                     end if;
                     Put_Line ("");
                  end if;
               end;
            elsif
              ((Typedef_Void_VKAPI_Ptr_Element.Kind_Id = Child_XML_Text and then
                Typedef_Void_VKAPI_Ptr_Element.XML_Text.all = "typedef void* (VKAPI_PTR *") or
                 (Typedef_Void_VKAPI_Ptr_Element.Kind_Id = Child_XML_Text and then
                  Typedef_Void_VKAPI_Ptr_Element.XML_Text.all = "typedef VkBool32 (VKAPI_PTR *")) and
              Procedure_Name_Element.Kind_Id = Child_Name
            then
               declare
                  Return_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                  Old_Name     : Ada.Strings.Unbounded.Unbounded_String;
                  New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  if Typedef_Void_VKAPI_Ptr_Element.XML_Text.all = "typedef void* (VKAPI_PTR *" then
                     Set_Unbounded_String (Return_Type_Name, "void*");
                  else
                     Set_Unbounded_String (Return_Type_Name, "VkBool32");
                  end if;

                  Set_Unbounded_String (Old_Name, String (Procedure_Name_Element.Name.Value));
                  Adaify_Type_Name (Old_Name => Procedure_Name_Element.Name.Value,
                                    New_Name => New_Type_Name);

                  Put_Tabs (1);
                  Put ("type ");
                  Puts (To_String (New_Type_Name));
                  Put_Line (" is access function (");

                  C_Type_Name_To_Ada_Name_Map_Owner.Insert (Container => C_Type_Name_To_Ada_Name_Map,
                                                            Key       => Old_Name,
                                                            New_Item  => New_Type_Name);

                  for I in Positive range Type_V.Children.First_Index + 3..(Type_V.Children.Last_Index - 1) loop
                     declare
                        Nested_Type_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (I);
                        Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                        C_Var_Name_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (I + 1);
                        Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;
                     begin
                        if
                          Nested_Type_Element.Kind_Id = Child_Nested_Type and then
                          Nested_Type_Element.Nested_Type.Exists_Value and then
                          C_Var_Name_Element.Kind_Id = Child_XML_Text
                        then
                           declare
                              C_Var_Name : Ada.Strings.Unbounded.Unbounded_String;
                              Comma_Index : Natural;
                           begin
                              Set_Unbounded_String (C_Var_Name, String (C_Var_Name_Element.XML_Text.all));
                              Comma_Index := Index (C_Var_Name, ",");

                              if Comma_Index > 0 then
                                 declare
                                    Total         : String := To_String (C_Var_Name);
                                    N_With_Spaces : String := Total (Total'First+1..Comma_Index - 1);
                                    N             : Aida.String_T := Aida.String_T (Ada.Strings.Fixed.Trim (Source => N_With_Spaces,
                                                                                                            Side   => Ada.Strings.Both));
                                    Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Adaify_Name (Old_Name => N,
                                                 New_Name => Adafied_Name);
                                    if Total (Total'First) = '*' then
                                       declare
                                          Prev_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (I - 1);
                                       begin
                                          if
                                            Prev_Element.Kind_Id = Child_XML_Text
                                          then
                                             declare
                                                P : Aida.String_T renames Prev_Element.XML_Text.all;
                                             begin
                                                if P (P'Last-5..P'Last) = "const " then
                                                   Set_Unbounded_String (Nested_Type_Name, "const " & String (Nested_Type_Element.Nested_Type.Value) & "*");
                                                else
                                                   Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value) & "*");
                                                end if;
                                             end;
                                          else
                                             Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value) & "*");
                                          end if;

                                          Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                         Key       => Nested_Type_Name);

                                          if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                             Put_Tabs (2);
                                             Puts (To_String (Adafied_Name));
                                             Put (" : ");
                                             Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                             Put_Line (";");
                                          else
                                             Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", could not find type " & To_String (Nested_Type_Name) & ", skipping conversion of ");
                                          end if;
                                       end;
                                    else
                                       Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value));

                                       Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                      Key       => Nested_Type_Name);

                                       if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                          Put_Tabs (2);
                                          Puts (To_String (Adafied_Name));
                                          Put (" : ");
                                          Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                          Put_Line (";");
                                       else
                                          Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", could not find type " & To_String (Nested_Type_Name) & ", skipping conversion of ");
                                          Aida.Text_IO.Put_Line (To_String (Type_V));
                                       end if;
                                    end if;
                                 end;
                              else
                                 Comma_Index := Index (C_Var_Name, ")");

                                 if Comma_Index > 0 then
                                    declare
                                       Total         : String := To_String (C_Var_Name);
                                       N_With_Spaces : String := Total (Total'First+1..Comma_Index - 1);
                                       N             : Aida.String_T := Aida.String_T (Ada.Strings.Fixed.Trim (Source => N_With_Spaces,
                                                                                                               Side   => Ada.Strings.Both));

                                       Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                                    begin
                                       Set_Unbounded_String (Old_Name, String (N));
                                       Adaify_Name (Old_Name => N,
                                                    New_Name => Adafied_Name);
                                       if Total (Total'First) = '*' then
                                          Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value) & "*");

                                          Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                         Key       => Nested_Type_Name);

                                          if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                             Put_Tabs (2);
                                             Puts (To_String (Adafied_Name));
                                             Put (" : ");
                                             Puts_Line (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                          else
                                             Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                                          end if;
                                       else
                                          Set_Unbounded_String (Nested_Type_Name, String (Nested_Type_Element.Nested_Type.Value));

                                          Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                         Key       => Nested_Type_Name);

                                          if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                             Put_Tabs (2);
                                             Puts (To_String (Adafied_Name));
                                             Put (" : ");
                                             Puts_Line (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                          else
                                             Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                                             Aida.Text_IO.Put_Line (To_String (Type_V));
                                          end if;
                                       end if;
                                    end;
                                 else
                                    Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", has not found ) for");
                                    Aida.Text_IO.Put_Line (To_String (Type_V));
                                 end if;
                              end if;
                           end;
                        end if;
                     end;
                  end loop;

                  Put_Tabs (2);
                  Put (") return ");

                  declare
                     Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;
                  begin
                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                    Key       => Return_Type_Name);

                     if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                        Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                     else
                        Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", could not find type " & To_String (Return_Type_Name));
                        Aida.Text_IO.Put_Line (To_String (Type_V));
                     end if;
                  end;

                  Put_Line (";");
                  if Generating_Code_For_OS = Windows then
                     Put_Tabs (1); Puts_Line ("pragma Convention (Stdcall, " & To_String (New_Type_Name) & ");");
                  else
                     Put_Tabs (1); Puts_Line ("pragma Convention (C, " & To_String (New_Type_Name) & ");");
                  end if;
                  Put_Line ("");
               end;
            else
               Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
               Aida.Text_IO.Put_Line (To_String (Type_V));
            end if;
         end;
      elsif
        Type_V.Exists_Category and then (Type_V.Category = "struct" or Type_V.Category = "union") and then
        Type_V.Exists_Name
      then
         null; -- Ignore for now. Will be generated later because the struct types need to be sorted first.
      else
         Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
         Aida.Text_IO.Put_Line (To_String (Type_V));
      end if;
   end Handle_Child_Type;

   procedure Handle_Out_Commented_Message (Out_Commented_Message_V : Vk_XML.String_Ptr) is
   begin
      null;
      --        Aida.Text_IO.Put ("Out commented message:");
      --        Aida.Text_IO.Put_Line (Vk_XML.XML_Out_Commented_Message.Ptro_String (Out_Commented_Message_V));
   end Handle_Out_Commented_Message;

   procedure Handle_API_Constants_Enum (Enum_V : Vk_XML.Enums_Enum_Tag.Ptr) is
   begin
      if Enum_V.Exists_Name then
         if Enum_V.Exists_Value then
            declare
               Has_Failed : Boolean;
               I : Aida.Int32_T;
               N : String := String (Enum_V.Name);
            begin
               To_Int32 (Source => Enum_V.Value,
                         Target => I,
                         Has_Failed => Has_Failed);

               if Has_Failed then
                  Aida.Text_IO.Put ("Could not convert '");
                  Aida.Text_IO.Put (String (Enum_V.Value));
                  Aida.Text_IO.Put ("' to integer for ");
                  Aida.Text_IO.Put_Line (N);
               else
                  Put_Tabs (1);
                  Put (Adaify_Constant_Name (Enum_V.Name));
                  Put (" : constant := ");
                  Put (Enum_V.Value);
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

   procedure Handle_Child_Enums_Enum (Enum_V       : Vk_XML.Enums_Enum_Tag.Ptr;
                                      Is_Last_Enum : in Boolean) is
   begin
      if Enum_V.Exists_Name then
         if Enum_V.Exists_Value then
            declare
               Has_Failed : Boolean;
               I : Aida.Int32_T;
               V : String := String (Enum_V.Value);
               N : String := String (Enum_V.Name);
            begin
               To_Int32 (Source => Enum_V.Value,
                         Target => I,
                         Has_Failed => Has_Failed);

               if Has_Failed then
                  Aida.Text_IO.Put ("Could not convert '");
                  Aida.Text_IO.Put (V);
                  Aida.Text_IO.Put ("' to integer for ");
                  Aida.Text_IO.Put_Line (N);
               else
                  Put_Tabs (2);
                  Put (Adaify_Constant_Name (Enum_V.Name));
                  if not Is_Last_Enum then
                     Put (",");
                  end if;

                  if Enum_V.Exists_Comment then
                     Put (" -- ");
                     Put (Enum_V.Comment);
                  end if;
                  Put_Line ("");
               end if;
            end;
         else
            Aida.Text_IO.Put_Line ("A <enum> tag exists without Value attribute!?");
         end if;
      else
         Aida.Text_IO.Put_Line ("A <enum> tag exists without Name attribute!?");
      end if;
   end Handle_Child_Enums_Enum;

   procedure Handle_Child_Enums_Enum_Representation_Clause (Enum_V        : Vk_XML.Enums_Enum_Tag.Ptr;
                                                            Is_First_Enum : in out Boolean) is
   begin
      if Enum_V.Exists_Name then
         if Enum_V.Exists_Value then
            declare
               Has_Failed : Boolean;
               I : Aida.Int32_T;
               V : String := String (Enum_V.Value);
               N : String := String (Enum_V.Name);
            begin
               To_Int32 (Source => Enum_V.Value,
                         Target => I,
                         Has_Failed => Has_Failed);

               if Has_Failed then
                  Aida.Text_IO.Put ("Could not convert '");
                  Aida.Text_IO.Put (V);
                  Aida.Text_IO.Put ("' to integer for ");
                  Aida.Text_IO.Put_Line (N);
               else
                  if Is_First_Enum then
                     Is_First_Enum := False;
                  else
                     Put_Line (",");
                  end if;
                  Put_Tabs (2);
                  Put (Adaify_Constant_Name (Enum_V.Name));
                  Put (" => ");
                  Put (Enum_V.Value);
               end if;
            end;
         else
            Aida.Text_IO.Put_Line ("A <enum> tag exists without Value attribute!?");
         end if;
      else
         Aida.Text_IO.Put_Line ("A <enum> tag exists without Name attribute!?");
      end if;
   end Handle_Child_Enums_Enum_Representation_Clause;

   package Enum_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                       Element_Type => Vk_XML.Enums_Enum_Tag.Ptr,
                                                       "="          => Vk_XML.Enums_Enum_Tag."=");

   procedure Handle_Registry_Child_Enums (Enums_V : Vk_XML.Enums_Tag.Ptr) is

      procedure Handle_Type_Attribute_Exists is
         Name_To_Adafy : Aida.String_T := Enums_V.Name;
         Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;

         procedure Auto_Generate_Code_For_Enum is
            Is_First_Enum : Boolean := True;
            Is_Last_Enum : Boolean;

            Enum_Vector : Enum_Vectors.Vector;

            procedure Populate_Enum_Vector is
            begin
               for Enums_Child of Enums_V.Children loop
                  case Enums_Child.Kind_Id is
                     when Child_Enums_Enum            =>
                        declare
                           Test : Vk_XML.Enums_Enum_Tag.Ptr := Enums_Child.Enums_Enum;
                        begin
                           Enum_Vector.Append (Test);
                        end;
                     when Child_Out_Commented_Message => null;
                     when Child_Unused                => null;
                  end case;
               end loop;
            end Populate_Enum_Vector;

            procedure Populate_Permutation_Array_And_Then_Generate_Ada_Code is

               type Array_Index_T is new Integer range Enum_Vectors.First_Index (Enum_Vector)..Enum_Vectors.Last_Index (Enum_Vector);

               type Permutation_Array_T is array (Array_Index_T) of Vk_XML.Enums_Enum_Tag.Ptr;

               Permutation_Array : Permutation_Array_T;

               function "<" (L, R : Vk_XML.Enums_Enum_Tag.Ptr) return Boolean is
                  Has_Failed : Boolean;
                  LI : Aida.Int32_T;
                  LV : Aida.String_T := L.Value;

                  RI : Aida.Int32_T;
                  RV : Aida.String_T := R.Value;
               begin
                  To_Int32 (Source => LV,
                            Target => LI,
                            Has_Failed => Has_Failed);

                  if Has_Failed then
                     raise Constraint_Error with "Could not convert '" & String (LV) & "' to integer";
                  else
                     To_Int32 (Source => RV,
                               Target => RI,
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        raise Constraint_Error with "Could not convert '" & String (RV) & "' to integer";
                     else
                        return LI < RI;
                     end if;
                  end if;
               end "<";

               procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort (Index_Type   => Array_Index_T,
                                                                                    Element_Type => Vk_XML.Enums_Enum_Tag.Ptr,
                                                                                    Array_Type   => Permutation_Array_T,
                                                                                    "<"          => "<");

               procedure Populate_Permutation_Array is
               begin
                  for I in Positive range Enum_Vectors.First_Index (Enum_Vector)..Enum_Vectors.Last_Index (Enum_Vector) loop
                     Permutation_Array (Array_Index_T (I)) := Enum_Vectors.Element (Container => Enum_Vector,
                                                                                    Index     => I);
                  end loop;

                  Sort (Permutation_Array);
               end Populate_Permutation_Array;

            begin
               Populate_Permutation_Array;

               Put_Tabs (1);
               Put ("type ");
               Puts (To_String (Adafied_Name));
               Put_Line (" is (");

               for I in Permutation_Array'Range loop
                  Is_Last_Enum := (I = Permutation_Array'Last);
                  Handle_Child_Enums_Enum (Permutation_Array (I), Is_Last_Enum);
               end loop;

               Put_Tabs (1);
               Put_Line (");");
               Put_Tabs (1);
               Put ("for ");
               Puts (To_String (Adafied_Name));
               Put_Line (" use (");

               Is_First_Enum := True;
               for I in Permutation_Array'Range loop
                  Handle_Child_Enums_Enum_Representation_Clause (Permutation_Array (I), Is_First_Enum);
               end loop;
               Put_Line ("");
               Put_Tabs (1);
               Put_Line (");");
               Put_Tabs (1);
               Puts_Line ("for " & To_String (Adafied_Name) & "'Size use Interfaces.C.int'Size;");
               Put_Line ("");

               declare
                  C_Type_Name : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String (String (Name_To_Adafy));
               begin
                  C_Type_Name_To_Ada_Name_Map.Insert (Key      => C_Type_Name,
                                                      New_Item => Adafied_Name);
               end;
            end Populate_Permutation_Array_And_Then_Generate_Ada_Code;

         begin
            Populate_Enum_Vector;
            Populate_Permutation_Array_And_Then_Generate_Ada_Code;
         end Auto_Generate_Code_For_Enum;

      begin
         Adaify_Type_Name (Old_Name => Name_To_Adafy,
                           New_Name => Adafied_Name);

         case Enums_V.Type_Attribute is
            when Enum     => Auto_Generate_Code_For_Enum;
            when Bit_Mask => null; -- The bit mask information is/will be used when generating code for <type>-tags.
         end case;
      end Handle_Type_Attribute_Exists;

   begin
      if Enums_V.Exists_Name then
         if Enums_V.Name = "API Constants" then
            for Enums_Child of Enums_V.Children loop
               case Enums_Child.Kind_Id is
                  when Child_Enums_Enum            => Handle_API_Constants_Enum (Enums_Child.Enums_Enum);
                  when Child_Out_Commented_Message => null;--Handle_Out_Commented_Message(Element (Children (Types_V), I).Out_Commented_Message);
                  when Child_Unused                => null;
               end case;
            end loop;
            Put_Line ("");
         else
            if Enums_V.Exists_Type_Attribute then
               Handle_Type_Attribute_Exists;
            else
               Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", A <enums> tag exists without Type attribute!?");
            end if;
         end if;
      else
         Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", A <enums> tag exists without Name attribute!?");
      end if;
   end Handle_Registry_Child_Enums;

   procedure Create_Vk_Package (R : Vk_XML.Registry_Tag.Ptr) is

      procedure Generate_Code_For_The_Public_Part is

         procedure Handle_Child_Types (Types_V : Vk_XML.Types_Tag.Ptr;
                                       R       : Vk_XML.Registry_Tag.T)
         is
            procedure Generate_Code_For_The_Non_Struct_Types is
            begin
               for Types_Child of Types_V.Children loop
                  case Types_Child.Kind_Id is
                     when Child_Type                  => Handle_Child_Type (Types_Child.Type_V.all, R);
                     when Child_Out_Commented_Message => Handle_Out_Commented_Message(Types_Child.Out_Commented_Message);
                  end case;
               end loop;
            end Generate_Code_For_The_Non_Struct_Types;

            Unsorted_Structs  : Struct_Type_Vectors.Vector;
            Sorted_Structs    : Struct_Type_Vectors.Vector;
            Left_Over_Structs : Struct_Type_Vectors.Vector; -- The unsorted structs minus the sorted ones

            procedure Sort_The_Struct_Types_With_Respect_To_Dependencies is

               procedure Populate_The_Unsorted_Structs_Vector is

                  procedure If_Struct_Add_To_Vector (Type_V : Vk_XML.Type_Tag.Ptr) is
                  begin
                     if Type_V.Exists_Category and then (Type_V.Category = "struct" or Type_V.Category = "union" ) and then
                       Type_V.Exists_Name
                     then
                        Unsorted_Structs.Append (Type_V);
                     end if;
                  end If_Struct_Add_To_Vector;

               begin
                  for Types_Child of Types_V.Children loop
                     case Types_Child.Kind_Id is
                        when Child_Type => If_Struct_Add_To_Vector (Types_Child.Type_V);
                        when others => null;
                     end case;
                  end loop;
               end Populate_The_Unsorted_Structs_Vector;

               procedure Populate_The_Sorted_Vector is
                  Shall_Continue : Boolean := True;
                  Number_Of_Structs_Before_Sorting_Session : Ada.Containers.Count_Type;

                  procedure Add_Struct_If_All_Member_Types_Are_Known (Type_V : not null access Vk_XML.Type_Tag.T) is
                     Members : Member_Vectors.Vector;

                     procedure Populate_The_Members_Vector is
                     begin
                        for Type_Child of Type_V.Children loop
                           if Type_Child.Kind_Id = Child_Member then
                              Members.Append (Type_Child.Member);
                           end if;
                        end loop;
                     end Populate_The_Members_Vector;

                     Are_All_Member_Types_Known : Boolean := True;

                     procedure Analyze_Member (Member_Children : Vk_XML.Member_Tag.Child_Vectors.Vector) is
                     begin
                        if Member_Children.Length = 2 then
                           declare
                              First : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index);
                              Second : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 1);
                           begin
                              if
                                First.Kind_Id = Child_Nested_Type and then
                                First.Nested_Type.Exists_Value and then
                                Second.Kind_Id = Child_Name and then
                                Second.Name.Value'Length > 0
                              then
                                 declare
                                    Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                    Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                    Adafied_Name     : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Adaify_Name (Old_Name => Second.Name.Value,
                                                 New_Name => Adafied_Name);

                                    Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value));

                                    Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                   Key       => Nested_Type_Name);

                                    if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                       Are_All_Member_Types_Known := False;
                                    end if;
                                 end;
                              else
                                 Are_All_Member_Types_Known := False;
                              end if;
                           end;
                        elsif Member_Children.Length = 3 then
                           declare
                              First  : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index);
                              Second : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 1);
                              Third  : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 2);
                           begin
                              if
                                First.Kind_Id = Child_Nested_Type and then
                                First.Nested_Type.Exists_Value and then
                                Second.Kind_Id = Child_XML_Text and then
                                Third.Kind_Id = Child_Name and then
                                Third.Name.Value'Length > 0
                              then
                                 declare
                                    Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                    Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value));

                                    Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                   Key       => Nested_Type_Name);

                                    if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                       if
                                         Ada.Strings.Fixed.Trim (Source => String (Second.XML_Text.all),
                                                                 Side   => Ada.Strings.Both) = "*"
                                       then
                                          Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value) & "*");

                                          Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                         Key       => Nested_Type_Name);

                                          if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                             Are_All_Member_Types_Known := False;
                                          end if;
                                       else
                                          Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                          Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                       end if;
                                    end if;
                                 end;
                              elsif
                                First.Kind_Id = Child_Nested_Type and then
                                First.Nested_Type.Exists_Value and then
                                Second.Kind_Id = Child_Name and then
                                Second.Name.Value'Length > 0 and then
                                Third.Kind_Id = Child_XML_Text
                              then
                                 declare
                                    Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                    Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value));

                                    Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                   Key       => Nested_Type_Name);

                                    if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                       Are_All_Member_Types_Known := False;
                                    end if;
                                 end;
                              else
                                 Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Type_V.all));
                              end if;
                           end;
                        elsif Member_Children.Length = 4 then
                           declare
                              First  : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index);
                              Second : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 1);
                              Third  : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 2);
                              Fourth : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 3);
                           begin
                              if
                                First.Kind_Id = Child_XML_Text and then
                                Second.Kind_Id = Child_Nested_Type and then
                                Second.Nested_Type.Exists_Value and then
                                Third.Kind_Id = Child_XML_Text and then
                                Fourth.Kind_Id = Child_Name and then
                                Fourth.Name.Value'Length > 0
                              then
                                 declare
                                    Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                    Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Set_Unbounded_String (Nested_Type_Name, String (Second.Nested_Type.Value));

                                    Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                   Key       => Nested_Type_Name);

                                    if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                       if
                                         Ada.Strings.Fixed.Trim (Source => String (Third.XML_Text.all),
                                                                 Side   => Ada.Strings.Both) = "*"
                                       then
                                          Set_Unbounded_String (Nested_Type_Name, String (Second.Nested_Type.Value) & "*");

                                          Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                         Key       => Nested_Type_Name);

                                          if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                             Are_All_Member_Types_Known := False;
                                          end if;
                                       else
                                          Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                          Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                       end if;
                                    end if;
                                 end;
                              else
                                 Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Type_V.all));
                              end if;
                           end;
                        elsif Member_Children.Length = 5 then
                           declare
                              First         : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index);
                              Second        : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 1);
                              Left_Bracket  : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 2);
                              Enum_Element  : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 3);
                              Right_Bracket : Vk_XML.Member_Tag.Child_T renames Member_Children.Element (Member_Children.First_Index + 4);
                           begin
                              if
                                First.Kind_Id = Child_Nested_Type and then
                                First.Nested_Type.Exists_Value and then
                                Left_Bracket.Kind_Id = Child_XML_Text and then
                                Right_Bracket.Kind_Id = Child_XML_Text and then
                                Left_Bracket.XML_Text.all = "[" and then
                                Right_Bracket.XML_Text.all = "]" and then
                                Enum_Element.Kind_Id = Child_Enum and then
                                Second.Kind_Id = Child_Name and then
                                Second.Name.Value'Length > 0
                              then
                                 declare
                                    Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                    Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value));

                                    Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                   Key       => Nested_Type_Name);

                                    if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                       Are_All_Member_Types_Known := False;
                                    end if;
                                 end;
                              else
                                 Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Type_V.all));
                              end if;
                           end;
                        else
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                           Aida.Text_IO.Put_Line (To_String (Type_V.all));
                        end if;
                     end Analyze_Member;

                  begin
                     Populate_The_Members_Vector;

                     for Member of Members loop
                        Analyze_Member (Member.Children);

                        if Type_V.Name = "VkAndroidSurfaceCreateInfoKHR" then
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", all members are not known or can't handle " & Are_All_Member_Types_Known'Img);
                           Aida.Text_IO.Put_Line (To_String (Type_V.all));
                        end if;
                     end loop;

                     if Are_All_Member_Types_Known then
                        Sorted_Structs.Append (Type_V);

                        declare
                           Old_Name      : Ada.Strings.Unbounded.Unbounded_String;
                           New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                        begin
                           Set_Unbounded_String (Old_Name, String (Type_V.Name));
                           Adaify_Type_Name (Old_Name => Type_V.Name,
                                             New_Name => New_Type_Name);
                           C_Type_Name_To_Ada_Name_Map_Owner.Insert (Container => C_Type_Name_To_Ada_Name_Map,
                                                                     Key       => Old_Name,
                                                                     New_Item  => New_Type_Name);
                        end;
                     else
                        Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", all members are not known or can't handle ");
                        Aida.Text_IO.Put_Line (To_String (Type_V.all));
                     end if;
                  end Add_Struct_If_All_Member_Types_Are_Known;

                  procedure Populate_The_Left_Over_Structs is
                  begin
                     Left_Over_Structs.Clear;

                     for Unsorted_Struct of Unsorted_Structs loop
                        if not Sorted_Structs.Contains (Unsorted_Struct) then
                           Left_Over_Structs.Append (Unsorted_Struct);
                        end if;
                     end loop;
                  end Populate_The_Left_Over_Structs;

               begin
                  while Shall_Continue loop
                     Populate_The_Left_Over_Structs;

                     Number_Of_Structs_Before_Sorting_Session := Sorted_Structs.Length;

                     for Struct of Left_Over_Structs loop
                        Add_Struct_If_All_Member_Types_Are_Known (Struct);
                     end loop;

                     Aida.Text_IO.Put_Line ("loop " & Number_Of_Structs_Before_Sorting_Session'Img);
                     Aida.Text_IO.Put_Line ("total " & Left_Over_Structs.Length'Img);

                     if Number_Of_Structs_Before_Sorting_Session = Sorted_Structs.Length then
                        Shall_Continue := False;
                        if Sorted_Structs.Length = Unsorted_Structs.Length then
                           Aida.Text_IO.Put_Line ("Good news! No circular dependencies detected between the struct types (including the union types)!");
                        else
                           Aida.Text_IO.Put_Line ("Bad news! Not all circular dependencies could be resolved! This needs investigation.");
                        end if;
                     else
                        Shall_Continue := Unsorted_Structs.Length /= Sorted_Structs.Length;
                     end if;
                  end loop;
               end Populate_The_Sorted_Vector;

            begin
               Populate_The_Unsorted_Structs_Vector;
               Populate_The_Sorted_Vector;
            end Sort_The_Struct_Types_With_Respect_To_Dependencies;

            procedure Generate_Code_For_The_Sorted_Structs is

               procedure Generate_The_Code (Variable_Name        : Aida.String_T;
                                            The_Nested_Type_Name : Aida.String_T;
                                            Last_Range_Index     : Aida.String_T;
                                            Type_V               : Vk_XML.Type_Tag.Ptr)
               is
                  Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                  Nested_Type_Name              : Ada.Strings.Unbounded.Unbounded_String;
                  Adafied_Array_Type_Name       : Ada.Strings.Unbounded.Unbounded_String;
                  Adafied_Array_Index_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  Adaify_Array_Type_Name (Old_Name => Variable_Name,
                                          New_Name => Adafied_Array_Type_Name);

                  Adaify_Array_Index_Type_Name (Old_Name => Variable_Name,
                                                New_Name => Adafied_Array_Index_Type_Name);

                  Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name));

                  Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                 Key       => Nested_Type_Name);


                  if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                     Put_Tabs (1);
                     Put ("type ");
                     Puts (To_String (Adafied_Array_Index_Type_Name));
                     Put (" is range 0..");
                     Put (Last_Range_Index);
                     Put_Line (";");
                     Put_Line ("");

                     Put_Tabs (1);
                     Put ("type ");
                     Puts (To_String (Adafied_Array_Type_Name));
                     Put (" is array (");
                     Puts (To_String (Adafied_Array_Index_Type_Name));
                     Put (") of ");
                     Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                     Put_Line (";");
                     Put_Tabs (1);
                     Puts_Line ("pragma Convention (C, " & To_String (Adafied_Array_Type_Name) & ");");
                     Put_Line ("");
                  else
                     Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                     Aida.Text_IO.Put_Line (To_String (Type_V.all));
                  end if;
               end Generate_The_Code;

               procedure Generate_Potential_Access_Type (The_Nested_Type_Name : Aida.String_T;
                                                         Second               : Aida.String_T;
                                                         Type_V               : Vk_XML.Type_Tag.Ptr)
               is
                  Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                  Nested_Type_Name         : Ada.Strings.Unbounded.Unbounded_String;
                  Adafied_Access_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  if
                    Ada.Strings.Fixed.Trim (Source => String (Second),
                                            Side   => Ada.Strings.Both) = "*"
                  then
                     Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name) & "*");

                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                     if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                        Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name));

                        Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                        if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                           Adaify_Access_Type_Name (Old_Name => The_Nested_Type_Name,
                                                    New_Name => Adafied_Access_Type_Name);

                           Put_Tabs (1);
                           Put ("type ");
                           Puts (To_String (Adafied_Access_Type_Name));
                           Put (" is access all ");
                           Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                           Put_Line (";");
                           Put_Line ("");

                           Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name) & "*");
                           C_Type_Name_To_Ada_Name_Map.Insert (Nested_Type_Name, Adafied_Access_Type_Name);
                        else
                           Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                           Aida.Text_IO.Put_Line (To_String (Type_V.all));
                        end if;
                     end if;
                  else
                     Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                     Aida.Text_IO.Put_Line (To_String (Type_V.all));
                  end if;
               end Generate_Potential_Access_Type;

               procedure Generate_Potential_Constant_Access_Type (The_Nested_Type_Name : Aida.String_T;
                                                                  First                : Aida.String_T;
                                                                  Third                : Aida.String_T;
                                                                  Type_V               : Vk_XML.Type_Tag.Ptr)
               is
                  Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                  Nested_Type_Name                  : Ada.Strings.Unbounded.Unbounded_String;
                  Adafied_Constant_Access_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  if
                    Ada.Strings.Fixed.Trim (Source => String (First),
                                            Side   => Ada.Strings.Both) = "const" and then
                    Ada.Strings.Fixed.Trim (Source => String (Third),
                                            Side   => Ada.Strings.Both) = "*"
                  then
                     Set_Unbounded_String (Nested_Type_Name, "const " & String (The_Nested_Type_Name) & "*");

                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                     if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                        Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name));

                        Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                        if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                           Adaify_Constant_Access_Type_Name (Old_Name => The_Nested_Type_Name,
                                                             New_Name => Adafied_Constant_Access_Type_Name);

                           Put_Tabs (1);
                           Put ("type ");
                           Puts (To_String (Adafied_Constant_Access_Type_Name));
                           Put (" is access constant ");
                           Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                           Put_Line (";");
                           Put_Line ("");

                           Set_Unbounded_String (Nested_Type_Name, "const " & String (The_Nested_Type_Name) & "*");
                           C_Type_Name_To_Ada_Name_Map.Insert (Nested_Type_Name, Adafied_Constant_Access_Type_Name);
                        else
                           Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                           Aida.Text_IO.Put_Line (To_String (Type_V.all));
                        end if;
                     end if;
                  elsif
                    Ada.Strings.Fixed.Trim (Source => String (First),
                                            Side   => Ada.Strings.Both) = "const" and then
                    Ada.Strings.Fixed.Trim (Source => String (Third),
                                            Side   => Ada.Strings.Both) = "* const*" and then
                    The_Nested_Type_Name = "char"
                  then
                     null;
                  else
                     Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                     Aida.Text_IO.Put_Line (To_String (Type_V.all));
                  end if;
               end Generate_Potential_Constant_Access_Type;

               Members : Member_Vectors.Vector;

               -- This subprogram also generates code for constant access types if they are not already defined for a member.
               procedure Generate_Code_For_The_Array_Declarations_If_Any (Type_V : Vk_XML.Type_Tag.Ptr) is
               begin
                  for Member of Members loop
                     if Member.Children.Length = 3 then
                        declare
                           First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           Third  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0 and then
                             Third.Kind_Id = Child_XML_Text
                           then
                              declare
                                 V : Aida.String_T renames Third.XML_Text.all;
                              begin
                                 Generate_The_Code (Second.Name.Value,
                                                    First.Nested_Type.Value,
                                                    V (V'First + 1 .. V'Last - 1),
                                                    Type_V);
                              end;
                           elsif
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_XML_Text and then
                             Third.Kind_Id = Child_Name and then
                             Third.Name.Value'Length > 0
                           then
                              Generate_Potential_Access_Type (The_Nested_Type_Name => First.Nested_Type.Value,
                                                              Second               => Second.XML_Text.all,
                                                              Type_V               => Type_V);
                           end if;
                        end;
                     elsif Member.Children.Length = 4 then
                        declare
                           First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           Third  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                           Fourth : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 3);
                        begin
                           if
                             First.Kind_Id = Child_XML_Text and then
                             Second.Kind_Id = Child_Nested_Type and then
                             Second.Nested_Type.Exists_Value and then
                             Third.Kind_Id = Child_XML_Text and then
                             Fourth.Kind_Id = Child_Name and then
                             Fourth.Name.Value'Length > 0
                           then
                              Generate_Potential_Constant_Access_Type (Second.Nested_Type.Value,
                                                                       First.XML_Text.all,
                                                                       Third.XML_Text.all,
                                                                       Type_V);
                           end if;
                        end;
                     elsif Member.Children.Length = 5 then
                        declare
                           First         : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second        : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           Left_Bracket  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                           Enum_Element  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 3);
                           Right_Bracket : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 4);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Left_Bracket.Kind_Id = Child_XML_Text and then
                             Right_Bracket.Kind_Id = Child_XML_Text and then
                             Left_Bracket.XML_Text.all = "[" and then
                             Right_Bracket.XML_Text.all = "]" and then
                             Enum_Element.Kind_Id = Child_Enum and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0
                           then
                              Generate_The_Code (Second.Name.Value,
                                                 First.Nested_Type.Value,
                                                 Adaify_Constant_Name (Enum_Element.Enum.Value),
                                                 Type_V);
                           end if;
                        end;
                     end if;
                  end loop;
               end Generate_Code_For_The_Array_Declarations_If_Any;

               procedure Generate_Usage_Comments_After_Record_Definition_If_Any (Type_V : Vk_XML.Type_Tag.Ptr) is
               begin
--                    for Type_Child of Type_V.Children loop
--                       if Type_Child.Kind_Id = Child_Validity then
--                          declare
--                             Validity_V : Vk_XML.Validity_Tag.Ptr := Type_Child.Validity;
--                          begin
--                             for Validity_Child of Validity_V.Children loop
--                                if Validity_Child.Kind_Id = Child_Usage then
--                                   declare
--                                      Usage_V : Vk_XML.Usage_Tag.Ptr := Validity_Child.Usage;
--                                   begin
--                                      for Usage_Child of Usage_V.Children loop
--                                         if Usage_Child.Kind_Id = Child_XML_Text then
--                                            declare
--                                               Text_V : Aida.String_T := Usage_Child.XML_Text.all;
--                                               N : Natural := Text_V'Last;
--                                            begin
--                                               for J in Positive range Text_V'First..Text_V'Last loop
--                                                  if Text_V (J) = Ada.Characters.Latin_1.LF or Text_V (J) = Ada.Characters.Latin_1.CR then
--                                                     N := J - 1;
--                                                     exit;
--                                                  end if;
--                                               end loop;
--
--                                               -- TODO: Handle text containing new-lines better!
--                                               Put_Tabs (1); Put ("-- ");
--                                               Put_Line (Text_V (Text_V'First..N));
--                                            end;
--                                         end if;
--                                      end loop;
--                                   end;
--                                end if;
--                             end loop;
--                          end;
--                       end if;
--                    end loop;
                  null;
               end Generate_Usage_Comments_After_Record_Definition_If_Any;

               procedure Populate_Members_Vector (Type_V : Vk_XML.Type_Tag.Ptr) is
               begin
                  Members.Clear;

                  for Type_Child of Type_V.Children loop
                     if Type_Child.Kind_Id = Child_Member then
                        Members.Append (Type_Child.Member);
                     end if;
                  end loop;
               end Populate_Members_Vector;

               procedure Generate_Code_For_Struct (Type_V : Vk_XML.Type_Tag.Ptr) is

                  New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

               begin
                  Adaify_Type_Name (Old_Name => Type_V.Name,
                                    New_Name => New_Type_Name);

                  Populate_Members_Vector (Type_V);

                  Generate_Code_For_The_Array_Declarations_If_Any (Type_V);

                  Put_Tabs (1);
                  Put ("type ");
                  Puts (To_String (New_Type_Name));
                  Put_Line (" is");
                  Put_Tabs (2); Put_Line ("record");

                  for Member of Members loop
                     if Member.Children.Length = 2 then
                        declare
                           First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0
                           then
                              declare
                                 Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                 Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 Adafied_Name     : Ada.Strings.Unbounded.Unbounded_String;
                              begin
                                 Adaify_Name (Old_Name => Second.Name.Value,
                                              New_Name => Adafied_Name);

                                 Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value));

                                 Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                Key       => Nested_Type_Name);

                                 if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                    Put_Tabs (3);
                                    Puts (To_String (Adafied_Name));
                                    Put (" : ");
                                    if
                                      Second.Name.Value = "apiVersion" and then
                                      Nested_Type_Name = "uint32_t"
                                    then
                                       Put ("Version_T");
                                    else
                                       Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                    end if;
                                    Put_Line (";");
                                 else
                                    Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                    Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                 end if;
                              end;
                           else
                              Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                              Aida.Text_IO.Put_Line (To_String (Type_V.all));
                           end if;
                        end;
                     elsif Member.Children.Length = 3 then
                        declare
                           First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           Third  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0 and then
                             Third.Kind_Id = Child_XML_Text
                           then
                              declare
                                 Adafied_Array_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                                 Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                              begin
                                 Adaify_Name (Old_Name => Second.Name.Value,
                                              New_Name => Adafied_Name);
                                 Adaify_Array_Type_Name (Old_Name => Second.Name.Value,
                                                         New_Name => Adafied_Array_Type_Name);

                                 Put_Tabs (3);
                                 Puts (To_String (Adafied_Name));
                                 Put (" : ");
                                 Puts (To_String (Adafied_Array_Type_Name));
                                 Put_Line (";");
                              end;
                           elsif
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_XML_Text and then
                             Third.Kind_Id = Child_Name and then
                             Third.Name.Value'Length > 0
                           then
                              if
                                Ada.Strings.Fixed.Trim (Source => String (Second.XML_Text.all), Side => Ada.Strings.Both) = "*"
                              then
                                 declare
                                    Searched_For : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                    Old_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                    Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Adaify_Name (Old_Name => Third.Name.Value,
                                                 New_Name => Adafied_Name);

                                    Set_Unbounded_String (Old_Type_Name, String (First.Nested_Type.Value) & "*");

                                    Searched_For := C_Type_Name_To_Ada_Name_Map.Find (Old_Type_Name);

                                    if Searched_For /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                       Put_Tabs (3);
                                       Puts (To_String (Adafied_Name));
                                       Put (" : ");
                                       Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For)));
                                       Put_Line (";");
                                    else
                                       Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't find " & To_String (Old_Type_Name) & ", can't handle ");
                                       Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                    end if;
                                 end;
                              else
                                 Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Type_V.all));
                              end if;
                           else
                              Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                              Aida.Text_IO.Put_Line (To_String (Type_V.all));
                           end if;
                        end;
                     elsif Member.Children.Length = 4 then
                        declare
                           First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           Third  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                           Fourth : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 3);
                        begin
                           if
                             First.Kind_Id = Child_XML_Text and then
                             Second.Kind_Id = Child_Nested_Type and then
                             Second.Nested_Type.Exists_Value and then
                             Third.Kind_Id = Child_XML_Text and then
                             Fourth.Kind_Id = Child_Name and then
                             Fourth.Name.Value'Length > 0
                           then
                              declare
                                 Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;

                                 Third_Element : Aida.String_T := Aida.String_T (Ada.Strings.Fixed.Trim (Source => String (Third.XML_Text.all),
                                                                                                         Side   => Ada.Strings.Both));
                              begin
                                 if
                                   First.XML_Text.all = "const " and then
                                   Third_Element = "*"
                                 then
                                    Adaify_Name (Old_Name => Fourth.Name.Value,
                                                 New_Name => Adafied_Name);

                                    declare
                                       Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                       Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                    begin
                                       Set_Unbounded_String (Nested_Type_Name, "const " & String (Second.Nested_Type.Value) & "*");

                                       Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                      Key       => Nested_Type_Name);

                                       if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                          Put_Tabs (3);
                                          Puts (To_String (Adafied_Name));
                                          Put (" : ");
                                          Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                          Put_Line (";");
                                       else
                                          Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                          Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                       end if;
                                    end;
                                 elsif
                                   First.XML_Text.all = "const " and then
                                   Third_Element = "* const*"
                                 then
                                    Adaify_Name (Old_Name => Fourth.Name.Value,
                                                 New_Name => Adafied_Name);

                                    declare
                                       Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                       Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                    begin
                                       Set_Unbounded_String (Nested_Type_Name, "const " & String (Second.Nested_Type.Value) & "* const*");

                                       Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                      Key       => Nested_Type_Name);

                                       if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                          Put_Tabs (3);
                                          Puts (To_String (Adafied_Name));
                                          Put (" : ");
                                          Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                          Put_Line (";");
                                       else
                                          Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                          Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                       end if;
                                    end;
                                 else
                                    Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                    Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                 end if;
                              end;
                           end if;
                        end;
                     elsif Member.Children.Length = 5 then
                        declare
                           First         : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second        : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           Left_Bracket  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                           Enum_Element  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 3);
                           Right_Bracket : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 4);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Left_Bracket.Kind_Id = Child_XML_Text and then
                             Right_Bracket.Kind_Id = Child_XML_Text and then
                             Left_Bracket.XML_Text.all = "[" and then
                             Right_Bracket.XML_Text.all = "]" and then
                             Enum_Element.Kind_Id = Child_Enum and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0
                           then
                              declare
                                 Adafied_Array_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                                 Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                              begin
                                 Adaify_Name (Old_Name => Second.Name.Value,
                                              New_Name => Adafied_Name);
                                 Adaify_Array_Type_Name (Old_Name => Second.Name.Value,
                                                         New_Name => Adafied_Array_Type_Name);

                                 Put_Tabs (3);
                                 Puts (To_String (Adafied_Name));
                                 Put (" : ");
                                 Puts (To_String (Adafied_Array_Type_Name));
                                 Put_Line (";");
                              end;
                           else
                              Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                              Aida.Text_IO.Put_Line (To_String (Type_V.all));
                           end if;
                        end;
                     else
                        Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                        Aida.Text_IO.Put_Line (To_String (Type_V.all));
                     end if;
                  end loop;

                  Put_Tabs (2); Put_Line ("end record;");
                  Put_Tabs (1); Puts_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Type_Name) & ");");

                  Generate_Usage_Comments_After_Record_Definition_If_Any (Type_V);

                  Put_Line ("");
               end Generate_Code_For_Struct;

               procedure Generate_Code_For_Union (Type_V : Vk_XML.Type_Tag.Ptr) is

                  New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                  procedure Generate_Code_For_Discriminant is
                     Prefix : Ada.Strings.Unbounded.Unbounded_String;

                     use type Vk_XML.Member_Tag.Ptr;
                  begin
                     Adaify_Name (Old_Name => Type_V.Name,
                                  New_Name => Prefix);

                     Put_Tabs (1);
                     Put ("type ");
                     Puts (To_String (Prefix) & "_Kind_Id_T");
                     Put_Line (" is (");

                     for Member of Members loop
                        if Member.Children.Length = 2 then
                           declare
                              First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                              Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           begin
                              if
                                First.Kind_Id = Child_Nested_Type and then
                                First.Nested_Type.Exists_Value and then
                                Second.Kind_Id = Child_Name and then
                                Second.Name.Value'Length > 0
                              then
                                 declare
                                    Adafied_Discriminant_Value : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Adaify_Name (Old_Name => Second.Name.Value,
                                                 New_Name => Adafied_Discriminant_Value);

                                    Put_Tabs (2);
                                    Puts (To_String (Prefix) & "_" & To_String (Adafied_Discriminant_Value));
                                    if Member = Members.Last_Element then
                                       Put_Line ("");
                                    else
                                       Put_Line (",");
                                    end if;
                                 end;
                              else
                                 Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Type_V.all));
                              end if;
                           end;
                        elsif Member.Children.Length = 3 then
                           declare
                              First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                              Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                              Third  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                           begin
                              if
                                First.Kind_Id = Child_Nested_Type and then
                                First.Nested_Type.Exists_Value and then
                                Second.Kind_Id = Child_Name and then
                                Second.Name.Value'Length > 0 and then
                                Third.Kind_Id = Child_XML_Text
                              then
                                 declare
                                    Adafied_Discriminant_Value : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Adaify_Name (Old_Name => Second.Name.Value,
                                                 New_Name => Adafied_Discriminant_Value);

                                    Put_Tabs (2);
                                    Puts (To_String (Prefix) & "_" & To_String (Adafied_Discriminant_Value));
                                    if Member = Members.Last_Element then
                                       Put_Line ("");
                                    else
                                       Put_Line (",");
                                    end if;
                                 end;
                              end if;
                           end;
                        end if;
                     end loop;
                     Put_Tabs (2); Put_Line (");");
                     Put_Line ("");
                  end Generate_Code_For_Discriminant;

                  Prefix : Ada.Strings.Unbounded.Unbounded_String;
               begin
                  Adaify_Name (Old_Name => Type_V.Name,
                               New_Name => Prefix);

                  Adaify_Type_Name (Old_Name => Type_V.Name,
                                    New_Name => New_Type_Name);

                  Populate_Members_Vector (Type_V);

                  Generate_Code_For_The_Array_Declarations_If_Any (Type_V);

                  Generate_Code_For_Discriminant;

                  Put_Tabs (1);
                  Put ("type ");
                  Puts (To_String (New_Type_Name));
                  Puts_Line (" (Kind_Id : " & To_String (Prefix) & "_Kind_Id_T := " & To_String (Prefix) & "_Kind_Id_T'First) is");
                  Put_Tabs (2); Put_Line ("record");
                  Put_Tabs (3); Put_Line ("case Kind_Id is");

                  for Member of Members loop
                     if Member.Children.Length = 2 then
                        declare
                           First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0
                           then
                              declare
                                 Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                 Nested_Type_Name           : Ada.Strings.Unbounded.Unbounded_String;
                                 Adafied_Name               : Ada.Strings.Unbounded.Unbounded_String;
                                 Adafied_Discriminant_Value : Ada.Strings.Unbounded.Unbounded_String;
                              begin
                                 Adaify_Name (Old_Name => Second.Name.Value,
                                              New_Name => Adafied_Discriminant_Value);

                                 Adaify_Name (Old_Name => Second.Name.Value,
                                              New_Name => Adafied_Name);

                                 Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value));

                                 Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                Key       => Nested_Type_Name);

                                 if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                    Put_Tabs (4); Puts_Line ("when " & To_String (Prefix) & "_" & To_String (Adafied_Discriminant_Value) & " =>");
                                    Put_Tabs (5);
                                    Puts (To_String (Adafied_Name));
                                    Put (" : ");
                                    Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                    Put_Line (";");
                                 else
                                    Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                    Aida.Text_IO.Put_Line (To_String (Type_V.all));
                                 end if;
                              end;
                           else
                              Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                              Aida.Text_IO.Put_Line (To_String (Type_V.all));
                           end if;
                        end;

                     elsif Member.Children.Length = 3 then
                        declare
                           First  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index);
                           Second : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 1);
                           Third  : Vk_XML.Member_Tag.Child_T renames Member.Children.Element (Member.Children.First_Index + 2);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0 and then
                             Third.Kind_Id = Child_XML_Text
                           then
                              declare
                                 Adafied_Array_Type_Name       : Ada.Strings.Unbounded.Unbounded_String;

                                 Adafied_Name      : Ada.Strings.Unbounded.Unbounded_String;

                                 Adafied_Discriminant_Value : Ada.Strings.Unbounded.Unbounded_String;
                              begin
                                 Adaify_Name (Old_Name => Second.Name.Value,
                                              New_Name => Adafied_Discriminant_Value);

                                 Adaify_Name (Old_Name => Second.Name.Value,
                                              New_Name => Adafied_Name);

                                 Adaify_Array_Type_Name (Old_Name => Second.Name.Value,
                                                         New_Name => Adafied_Array_Type_Name);

                                 Put_Tabs (4); Puts_Line ("when " & To_String (Prefix) & "_" & To_String (Adafied_Discriminant_Value) & " =>");
                                 Put_Tabs (5);
                                 Puts (To_String (Adafied_Name));
                                 Put (" : ");
                                 Puts (To_String (Adafied_Array_Type_Name));
                                 Put_Line (";");
                              end;
                           else
                              Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                              Aida.Text_IO.Put_Line (To_String (Type_V.all));
                           end if;
                        end;
                     else
                        Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", Skipping conversion of ");
                        Aida.Text_IO.Put_Line (To_String (Type_V.all));
                     end if;
                  end loop;

                  Put_Tabs (3); Put_Line ("end case;");
                  Put_Tabs (2); Put_Line ("end record;");
                  Put_Tabs (1); Put ("pragma Unchecked_Union (");
                  Puts (To_String (New_Type_Name));
                  Put_Line (");");
                  Put_Tabs (1); Put ("pragma Convention (C, ");
                  Puts (To_String (New_Type_Name));
                  Put_Line (");");

                  Generate_Usage_Comments_After_Record_Definition_If_Any (Type_V);

                  Put_Line ("");
               end Generate_Code_For_Union;

            begin
               for Struct of Sorted_Structs loop
                  if Struct.Category = "struct" then
                     Generate_Code_For_Struct (Struct);
                  else
                     Generate_Code_For_Union (Struct);
                  end if;
               end loop;
            end Generate_Code_For_The_Sorted_Structs;

         begin
            Generate_Code_For_The_Non_Struct_Types;
            Sort_The_Struct_Types_With_Respect_To_Dependencies;
            Generate_Code_For_The_Sorted_Structs;
         end Handle_Child_Types;

         procedure Generate_Code_For_The_Enum_Types is
         begin
            for Request_Child of R.Children loop
               case Request_Child.Kind_Id is
                  when Child_Enums =>
                     Handle_Registry_Child_Enums (Request_Child.Enums);
                  when others =>
                     null;
               end case;
            end loop;
         end Generate_Code_For_The_Enum_Types;

         procedure Generate_Code_For_Special_Types is
         begin
            Put_Tabs (1); Put_Line ("subtype Void_Ptr is System.Address;");
            Put_Line ("");
            Put_Tabs (1); Put_Line ("type Void_Ptr_Array_T is array (Interfaces.C.size_t range 0..1000) of Void_Ptr;");
            Put_Line ("pragma Convention (C, Void_Ptr_Array_T);");
            Put_Line ("");
            Put_Tabs (1); Put_Line ("package Void_Ptr_Array_Conversions is new Generic_Address_To_Access_Conversions (Void_Ptr_Array_T);");
            Put_Line ("");
            Put_Tabs (1); Put_Line ("type Char_Ptr_Array_T is array (Interfaces.C.size_t range 0..1000) of Interfaces.C.Strings.chars_ptr;");
            Put_Line ("pragma Convention (C, Char_Ptr_Array_T);");
            Put_Line ("");
            Put_Tabs (1); Put_Line ("package Char_Ptr_Array_Conversions is new Generic_Address_To_Access_Conversions (Char_Ptr_Array_T);");
            Put_Line ("");
         end Generate_Code_For_Special_Types;

         procedure Handle_Command (Command_V : Vk_XML.Command_Tag.T) is

            Is_Function : Boolean := False;
            Return_Type : Ada.Strings.Unbounded.Unbounded_String;

            Params : Param_Vectors.Vector;

            C_Subprogram_Name : Ada.Strings.Unbounded.Unbounded_String;
            Subprogram_Name   : Ada.Strings.Unbounded.Unbounded_String;

            procedure Handle_Proto (Proto_V : Vk_XML.Proto_Tag.Ptr) is

               procedure Handle_Proto_Children is
                  First  : Vk_XML.Proto_Tag.Child_T renames Proto_V.Children.Element (Proto_V.Children.First_Index);
                  Second : Vk_XML.Proto_Tag.Child_T renames Proto_V.Children.Element (Proto_V.Children.First_Index + 1);

                  procedure Generate_Code_For_Subprogram is
                  begin
                     if First.Nested_Type.Value = "void" then
                        Is_Function := False;
                        Put_Tabs (1); Put ("procedure ");
                     else
                        Is_Function := True;
                        Adaify_Type_Name (Old_Name => First.Nested_Type.Value,
                                          New_Name => Return_Type);
                        Put_Tabs (1); Put ("function ");
                     end if;

                     Set_Unbounded_String (C_Subprogram_Name, String (Second.Name.Value));

                     Adaify_Name (Old_Name => Second.Name.Value,
                                  New_Name => Subprogram_Name);

                     Puts (To_String (Subprogram_Name));

                     if Params.Length > 0 then
                        Put_Line (" (");
                     else
                        Put_Line (";");
                     end if;
                  end Generate_Code_For_Subprogram;

               begin
                  if
                    First.Kind_Id = Child_Nested_Type and then
                    First.Nested_Type.Exists_Value and then
                    Second.Kind_Id = Child_Name
                  then
                     Generate_Code_For_Subprogram;
                  else
                     Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                     Aida.Text_IO.Put_Line (To_String (Command_V));
                  end if;
               end Handle_Proto_Children;

            begin
               if Proto_V.Children.Length = 2 then
                  Handle_Proto_Children;
               else
                  Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                  Aida.Text_IO.Put_Line (To_String (Command_V));
               end if;
            end Handle_Proto;

            procedure Populate_Params_Vector is
            begin
               for Command_Child of Command_V.Children loop
                  case Command_Child.Kind_Id is
                     when Child_Param => Param_Vectors.Append (Container => Params,
                                                               New_Item  => Command_Child.Param);
                     when others => null;
                  end case;
               end loop;
            end Populate_Params_Vector;

            procedure Generate_Code_For_The_Subprogram_Name is
            begin
               for Command_Child of Command_V.Children loop
                  case Command_Child.Kind_Id is
                     when Child_Proto =>
                        Handle_Proto (Command_Child.Proto);
                     when others      => null;
                  end case;
               end loop;
            end Generate_Code_For_The_Subprogram_Name;

            procedure Generate_Potential_Constant_Access_Type (Variable_Name        : Aida.String_T;
                                                               The_Nested_Type_Name : Aida.String_T;
                                                               First                : Aida.String_T;
                                                               Third                : Aida.String_T;
                                                               Command_V            : Vk_XML.Command_Tag.T)
            is
               Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

               Nested_Type_Name                  : Ada.Strings.Unbounded.Unbounded_String;
               Adafied_Constant_Access_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
            begin
               if
                 First = "const " and then
                 Ada.Strings.Fixed.Trim (Source => String (Third),
                                         Side   => Ada.Strings.Both) = "*"
               then
                  Set_Unbounded_String (Nested_Type_Name, "const " & String (The_Nested_Type_Name) & "*");

                  Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                  if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                     Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name));

                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                     if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                        Adaify_Constant_Access_Type_Name (Old_Name => The_Nested_Type_Name,
                                                          New_Name => Adafied_Constant_Access_Type_Name);

                        Put_Tabs (1);
                        Put ("type ");
                        Puts (To_String (Adafied_Constant_Access_Type_Name));
                        Put (" is access constant ");
                        Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                        Put_Line (";");
                        Put_Line ("");

                        Set_Unbounded_String (Nested_Type_Name, "const " & String (The_Nested_Type_Name) & "*");
                        C_Type_Name_To_Ada_Name_Map.Insert (Nested_Type_Name, Adafied_Constant_Access_Type_Name);
                     else
                        Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                        Aida.Text_IO.Put_Line (To_String (Command_V));
                     end if;
                  end if;
               else
                  Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                  Aida.Text_IO.Put_Line (To_String (Command_V));
               end if;
            end Generate_Potential_Constant_Access_Type;

            procedure Generate_Potential_Access_Type (The_Nested_Type_Name : Aida.String_T;
                                                      Second               : Aida.String_T;
                                                      Command_V            : Vk_XML.Command_Tag.T)
            is
               Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

               Nested_Type_Name         : Ada.Strings.Unbounded.Unbounded_String;
               Adafied_Access_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
            begin
               if
                 Ada.Strings.Fixed.Trim (Source => String (Second),
                                         Side   => Ada.Strings.Both) = "*"
               then
                  Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name) & "*");

                  Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                  if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                     Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name));

                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                     if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                        Adaify_Access_Type_Name (Old_Name => The_Nested_Type_Name,
                                                 New_Name => Adafied_Access_Type_Name);

                        Set_Unbounded_String (Adafied_Access_Type_Name, "access " & To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));


--                          Put_Tabs (1);
--                          Put ("type ");
--                          Puts (To_String (Adafied_Access_Type_Name));
--                          Put (" is access all ");
--                          Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
--                          Put_Line (";");
--                          Put_Line ("");

                        Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name) & "*");
                        C_Type_Name_To_Ada_Name_Map.Insert (Nested_Type_Name, Adafied_Access_Type_Name);
                     else
                        Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                        Aida.Text_IO.Put_Line (To_String (Command_V));
                     end if;
                  end if;
               elsif
                 Ada.Strings.Fixed.Trim (Source => String (Second),
                                         Side   => Ada.Strings.Both) = "**"
               then
                  Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name) & "**");

                  Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                  if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                     Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name));

                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                     if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                        Adaify_Constant_Access_Type_Name (Old_Name => The_Nested_Type_Name,
                                                          New_Name => Adafied_Access_Type_Name);

                        Put_Tabs (1);
                        Put ("type ");
                        Puts (To_String (Adafied_Access_Type_Name));
                        Put (" is access constant ");
                        Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                        Put_Line (";");
                        Put_Line ("");

                        Set_Unbounded_String (Nested_Type_Name, String (The_Nested_Type_Name) & "*");
                        C_Type_Name_To_Ada_Name_Map.Insert (Nested_Type_Name, Adafied_Access_Type_Name);
                     else
                        Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                        Aida.Text_IO.Put_Line (To_String (Command_V));
                     end if;
                  end if;
               else
                  Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                  Aida.Text_IO.Put_Line (To_String (Command_V));
               end if;
            end Generate_Potential_Access_Type;

            function Is_Pointer_Actually_An_Array (Command_V : Vk_XML.Command_Tag.T;
                                                   Param_V   : Vk_XML.Param_Tag.Ptr) return Boolean
            is
               Is_Found : Boolean := False;
            begin
               if Param_V.Exists_Len then
                  for Param of Params loop
                     if Param.Children.Length = 2 then
                        declare
                           First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                           Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_Name and then
                             Second.Name.Value'Length > 0
                           then
                              if Param_V.Len = Second.Name.Value then
                                 Is_Found := True;
                                 exit;
                              end if;
                           end if;
                        end;
                     elsif Param.Children.Length = 3 then
                        declare
                           First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                           Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                           Third  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 2);
                        begin
                           if
                             First.Kind_Id = Child_Nested_Type and then
                             First.Nested_Type.Exists_Value and then
                             Second.Kind_Id = Child_XML_Text and then
                             Third.Kind_Id = Child_Name and then
                             Third.Name.Value'Length > 0
                           then
                              if Param_V.Len = Third.Name.Value then
                                 Is_Found := True;
                                 exit;
                              end if;
                           else
                              Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                              Aida.Text_IO.Put_Line (To_String (Command_V));
                           end if;
                        end;
                     elsif Param.Children.Length = 4 then
                        declare
                           First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                           Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                           Third  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 2);
                           Fourth : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 3);
                        begin
                           if
                             First.Kind_Id = Child_XML_Text and then
                             Second.Kind_Id = Child_Nested_Type and then
                             Second.Nested_Type.Exists_Value and then
                             Third.Kind_Id = Child_XML_Text and then
                             Fourth.Kind_Id = Child_Name and then
                             Fourth.Name.Value'Length > 0
                           then
                              if Param_V.Len = Fourth.Name.Value then
                                 Is_Found := True;
                                 exit;
                              end if;
                           elsif
                             First.Kind_Id = Child_XML_Text and then
                             Second.Kind_Id = Child_Nested_Type and then
                             Second.Nested_Type.Exists_Value and then
                             Third.Kind_Id = Child_Name and then
                             Third.Name.Value'Length > 0 and then
                             Fourth.Kind_Id = Child_XML_Text
                           then
                              if Param_V.Len = Third.Name.Value then
                                 Is_Found := True;
                                 exit;
                              end if;
                           else
                              Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                              Aida.Text_IO.Put_Line (To_String (Command_V));
                           end if;
                        end;
                     else
                        Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                        Aida.Text_IO.Put_Line (To_String (Command_V));
                     end if;
                  end loop;

                  return Is_Found;
               else
                  return False;
               end if;
            end Is_Pointer_Actually_An_Array;

            procedure Generate_Potential_Array_Declaration (The_Nested_Type_Name : String;
                                                            Second               : String;
                                                            C_Variable_Name      : String;
                                                            Command_V            : Vk_XML.Command_Tag.T)
            is
               Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

               Nested_Type_Name         : Ada.Strings.Unbounded.Unbounded_String;
               Adafied_Access_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

               Adafied_Variable_Name    : Ada.Strings.Unbounded.Unbounded_String;
               Upper_Case_Variable_Name : Ada.Strings.Unbounded.Unbounded_String;

               Adafied_Array_Type_Name  : Ada.Strings.Unbounded.Unbounded_String;
               Array_Conversions_Package_Name : Ada.Strings.Unbounded.Unbounded_String;
            begin
               if
                 Ada.Strings.Fixed.Trim (Source => Second,
                                         Side   => Ada.Strings.Both) = "*"
               then
                  Set_Unbounded_String (Nested_Type_Name, The_Nested_Type_Name & "*");

                  Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                  if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                     Set_Unbounded_String (Nested_Type_Name, The_Nested_Type_Name);

                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                     if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                        if
                          The_Nested_Type_Name = "VkExtensionProperties" and then
                          C_Variable_Name = "pProperties"
                        then
                           Set_Unbounded_String (Upper_Case_Variable_Name, "EXTENSION_PROPERTIES");
                           Set_Unbounded_String (Adafied_Array_Type_Name, "Extension_Properties_Array_T");
                           Set_Unbounded_String (Array_Conversions_Package_Name, "Extension_Properties_Array_Conversions");
                        elsif
                          The_Nested_Type_Name = "VkLayerProperties" and then
                          C_Variable_Name = "pProperties"
                        then
                           Set_Unbounded_String (Upper_Case_Variable_Name, "LAYER_PROPERTIES");
                           Set_Unbounded_String (Adafied_Array_Type_Name, "Layer_Properties_Array_T");
                           Set_Unbounded_String (Array_Conversions_Package_Name, "Layer_Properties_Array_Conversions");
                        elsif
                          The_Nested_Type_Name = "VkSparseImageFormatProperties" and then
                          C_Variable_Name = "pProperties"
                        then
                           Set_Unbounded_String (Upper_Case_Variable_Name, "SPARSE_IMAGE_FORMAT_PROPERTIES");
                           Set_Unbounded_String (Adafied_Array_Type_Name, "Sparse_Image_Format_Properties_Array_T");
                           Set_Unbounded_String (Array_Conversions_Package_Name, "Sparse_Image_Format_Properties_Array_Conversions");
                        elsif
                          The_Nested_Type_Name = "VkDisplayPropertiesKHR" and then
                          C_Variable_Name = "pProperties"
                        then
                           Set_Unbounded_String (Upper_Case_Variable_Name, "DISPLAY_PROPERTIES");
                           Set_Unbounded_String (Adafied_Array_Type_Name, "Display_Properties_Array_T");
                           Set_Unbounded_String (Array_Conversions_Package_Name, "Display_Properties_Array_Conversions");
                        elsif
                          The_Nested_Type_Name = "VkDisplayPlanePropertiesKHR" and then
                          C_Variable_Name = "pProperties"
                        then
                           Set_Unbounded_String (Upper_Case_Variable_Name, "DISPLAY_PLANE_PROPERTIES");
                           Set_Unbounded_String (Adafied_Array_Type_Name, "Display_Plane_Properties_Array_T");
                           Set_Unbounded_String (Array_Conversions_Package_Name, "Display_Plane_Properties_Array_Conversions");
                        elsif
                          The_Nested_Type_Name = "VkDisplayModePropertiesKHR" and then
                          C_Variable_Name = "pProperties"
                        then
                           Set_Unbounded_String (Upper_Case_Variable_Name, "DISPLAY_MODE_PROPERTIES");
                           Set_Unbounded_String (Adafied_Array_Type_Name, "Display_Mode_Properties_Array_T");
                           Set_Unbounded_String (Array_Conversions_Package_Name, "Display_Mode_Properties_Array_Conversions");
                        else
                           Adaify_Name (Old_Name => Aida.String_T (C_Variable_Name),
                                        New_Name => Adafied_Variable_Name);

                           Make_Upper_Case (Source => Aida.String_T (To_String (Adafied_Variable_Name)),
                                            Target => Upper_Case_Variable_Name);

                           Adaify_Array_Type_Name (Old_Name => Aida.String_T (C_Variable_Name),
                                                   New_Name => Adafied_Array_Type_Name);

                           Adaify_Array_Conversions_Package (Source => Aida.String_T (C_Variable_Name),
                                                             Target => Array_Conversions_Package_Name);
                        end if;

                        Put_Tabs (1);
                        Puts_Line ("MAX_" & To_String (Upper_Case_Variable_Name) & "_INDEX : constant := 100;");
                        Put_Line ("");

                        Put_Tabs (1);
                        Put ("type ");
                        Puts (To_String (Adafied_Array_Type_Name));
                        Puts (" is array (Interfaces.C.size_t range 0 .. MAX_" & To_String (Upper_Case_Variable_Name) & "_INDEX");
                        Put (") of ");
                        Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                        Put_Line (";");
                        Put_Tabs (1);
                        Puts_Line ("pragma Convention (C, " & To_String (Adafied_Array_Type_Name) & ");");
                        Put_Line ("");

                        Put_Tabs (1);
                        Puts_Line ("package " & To_String (Array_Conversions_Package_Name) & " is new Generic_Address_To_Access_Conversions (" & To_String (Adafied_Array_Type_Name) & ");");
                        Put_Line ("");

                        Set_Unbounded_String (Adafied_Access_Type_Name, To_String (Array_Conversions_Package_Name) & ".Object_Address");

                        Set_Unbounded_String (Nested_Type_Name, The_Nested_Type_Name & "*");
                        C_Type_Name_To_Ada_Name_Map.Insert (Nested_Type_Name, Adafied_Access_Type_Name);
                     else
                        Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                        Aida.Text_IO.Put_Line (To_String (Command_V));
                     end if;
                  end if;
               elsif
                 Ada.Strings.Fixed.Trim (Source => Second,
                                         Side   => Ada.Strings.Both) = "**"
               then
                  Set_Unbounded_String (Nested_Type_Name, The_Nested_Type_Name & "**");

                  Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                  if Searched_For_Cursor = C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                     Set_Unbounded_String (Nested_Type_Name, The_Nested_Type_Name);

                     Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                     if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then

                        Adaify_Constant_Access_Type_Name (Old_Name => Aida.String_T (The_Nested_Type_Name),
                                                          New_Name => Adafied_Access_Type_Name);

                        Put_Tabs (1);
                        Put ("type ");
                        Puts (To_String (Adafied_Access_Type_Name));
                        Put (" is access constant ");
                        Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                        Put_Line (";");
                        Put_Line ("");

                        Set_Unbounded_String (Nested_Type_Name, The_Nested_Type_Name & "*");
                        C_Type_Name_To_Ada_Name_Map.Insert (Nested_Type_Name, Adafied_Access_Type_Name);
                     else
                        Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                        Aida.Text_IO.Put_Line (To_String (Command_V));
                     end if;
                  end if;
               else
                  Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                  Aida.Text_IO.Put_Line (To_String (Command_V));
               end if;
            end Generate_Potential_Array_Declaration;

            procedure Generate_Code_For_The_Constant_Access_Types_If_Any (Command_V : Vk_XML.Command_Tag.T) is
            begin
               for Param of Params loop
                  if Param.Children.Length = 3 then
                     declare
                        First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                        Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                        Third  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 2);
                     begin
                        if
                          First.Kind_Id = Child_Nested_Type and then
                          First.Nested_Type.Exists_Value and then
                          Second.Kind_Id = Child_XML_Text and then
                          Third.Kind_Id = Child_Name and then
                          Third.Name.Value'Length > 0
                        then
                           if Is_Pointer_Actually_An_Array (Command_V, Param) then
                              Generate_Potential_Array_Declaration (String (First.Nested_Type.Value),
                                                                    String (Second.XML_Text.all),
                                                                    String (Third.Name.Value),
                                                                    Command_V);
                           else
                              Generate_Potential_Access_Type (First.Nested_Type.Value,
                                                              Second.XML_Text.all,
                                                              Command_V);
                           end if;
                        end if;
                     end;
                  elsif Param.Children.Length = 4 then
                     declare
                        First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                        Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                        Third  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 2);
                        Fourth : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 3);
                     begin
                        if
                          First.Kind_Id = Child_XML_Text and then
                          Second.Kind_Id = Child_Nested_Type and then
                          Second.Nested_Type.Exists_Value and then
                          Third.Kind_Id = Child_XML_Text and then
                          Fourth.Kind_Id = Child_Name and then
                          Fourth.Name.Value'Length > 0
                        then
                           Generate_Potential_Constant_Access_Type (Fourth.Name.Value,
                                                                    Second.Nested_Type.Value,
                                                                    First.XML_Text.all,
                                                                    Third.XML_Text.all,
                                                                    Command_V);
                        end if;
                     end;
                  end if;
               end loop;
            end Generate_Code_For_The_Constant_Access_Types_If_Any;

            procedure Generate_Code_For_The_Subprogram_Parameters_If_Any is

               procedure Generate_Code_For_Parameter (Param   : Vk_XML.Param_Tag.Ptr;
                                                      Is_Last : Boolean) is
               begin
                  if Param.Children.Length = 2 then
                     declare
                        First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                        Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                     begin
                        if
                          First.Kind_Id = Child_Nested_Type and then
                          First.Nested_Type.Exists_Value and then
                          Second.Kind_Id = Child_Name and then
                          Second.Name.Value'Length > 0
                        then
                           declare
                              Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                              Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                              Adafied_Name     : Ada.Strings.Unbounded.Unbounded_String;
                           begin
                              Adaify_Name (Old_Name => Second.Name.Value,
                                           New_Name => Adafied_Name);

                              Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value));

                              Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                             Key       => Nested_Type_Name);

                              if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                 Put_Tabs (2);
                                 Puts (To_String (Adafied_Name));
                                 Put (" : ");
                                 Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                 if Is_Last then
                                    Put_Line ("");
                                 else
                                    Put_Line (";");
                                 end if;
                              else
                                 Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Command_V));
                              end if;
                           end;
                        else
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                           Aida.Text_IO.Put_Line (To_String (Command_V));
                        end if;
                     end;
                  elsif Param.Children.Length = 3 then
                     declare
                        First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                        Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                        Third  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 2);
                     begin
                        if
                          First.Kind_Id = Child_Nested_Type and then
                          First.Nested_Type.Exists_Value and then
                          Second.Kind_Id = Child_XML_Text and then
                          Third.Kind_Id = Child_Name and then
                          Third.Name.Value'Length > 0
                        then
                           declare
                              Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                              Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                              Adafied_Name     : Ada.Strings.Unbounded.Unbounded_String;
                           begin
                              if
                                Ada.Strings.Fixed.Trim (Source => String (Second.XML_Text.all),
                                                        Side   => Ada.Strings.Both) = "*"
                              then
                                 Adaify_Name (Old_Name => Third.Name.Value,
                                              New_Name => Adafied_Name);

                                 Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value) & "*");

                                 Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map.Find (Nested_Type_Name);

                                 if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                    Put_Tabs (2);
                                    Puts (To_String (Adafied_Name));
                                    Put (" : ");
                                    Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                    if Is_Last then
                                       Put_Line ("");
                                    else
                                       Put_Line (";");
                                    end if;
                                 else
                                    Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                    Aida.Text_IO.Put_Line (To_String (Command_V));
                                 end if;
                              elsif
                                Ada.Strings.Fixed.Trim (Source => String (Second.XML_Text.all),
                                                        Side   => Ada.Strings.Both) = "**"
                              then
                                 Adaify_Name (Old_Name => Third.Name.Value,
                                              New_Name => Adafied_Name);

                                 Set_Unbounded_String (Nested_Type_Name, String (First.Nested_Type.Value) & "*");

                                 Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                Key       => Nested_Type_Name);

                                 if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                    Put_Tabs (2);
                                    Puts (To_String (Adafied_Name));
                                    Put (" : ");
                                    Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                    if Is_Last then
                                       Put_Line ("");
                                    else
                                       Put_Line (";");
                                    end if;
                                 else
                                    Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                    Aida.Text_IO.Put_Line (To_String (Command_V));
                                 end if;
                              else
                                 Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Command_V));
                              end if;
                           end;
                        else
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                           Aida.Text_IO.Put_Line (To_String (Command_V));
                        end if;
                     end;
                  elsif Param.Children.Length = 4 then
                     declare
                        First  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index);
                        Second : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 1);
                        Third  : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 2);
                        Fourth : Vk_XML.Param_Tag.Child_T renames Param.Children.Element (Param.Children.First_Index + 3);
                     begin
                        if
                          First.Kind_Id = Child_XML_Text and then
                          Second.Kind_Id = Child_Nested_Type and then
                          Second.Nested_Type.Exists_Value and then
                          Third.Kind_Id = Child_XML_Text and then
                          Fourth.Kind_Id = Child_Name and then
                          Fourth.Name.Value'Length > 0
                        then
                           declare
                              Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;

                              Star : String := String (Third.XML_Text.all);
                           begin
                              if
                                First.XML_Text.all = "const " and then
                                Star (Star'First) = '*'
                              then
                                 Adaify_Name (Old_Name => Fourth.Name.Value,
                                              New_Name => Adafied_Name);

                                 declare
                                    Searched_For_Cursor : C_Type_Name_To_Ada_Name_Map_Owner.Cursor;

                                    Nested_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
                                 begin
                                    Set_Unbounded_String (Nested_Type_Name, "const " & String (Second.Nested_Type.Value) & "*");

                                    Searched_For_Cursor := C_Type_Name_To_Ada_Name_Map_Owner.Find (Container => C_Type_Name_To_Ada_Name_Map,
                                                                                                   Key       => Nested_Type_Name);

                                    if Searched_For_Cursor /= C_Type_Name_To_Ada_Name_Map_Owner.No_Element then
                                       Put_Tabs (3);
                                       Puts (To_String (Adafied_Name));
                                       Put (" : ");
                                       Puts (To_String (C_Type_Name_To_Ada_Name_Map.Constant_Reference (Searched_For_Cursor)));
                                       if Is_Last then
                                          Put_Line ("");
                                       else
                                          Put_Line (";");
                                       end if;
                                    else
                                       Aida.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", can't handle ");
                                       Aida.Text_IO.Put_Line (To_String (Command_V));
                                    end if;
                                 end;
                              else
                                 Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Command_V));
                              end if;
                           end;
                        elsif
                          First.Kind_Id = Child_XML_Text and then
                          Second.Kind_Id = Child_Nested_Type and then
                          Second.Nested_Type.Exists_Value and then
                          Third.Kind_Id = Child_Name and then
                          Third.Name.Value'Length > 0 and then
                          Fourth.Kind_Id = Child_XML_Text
                        then
                           declare
                              Adafied_Array_Type_Name : Ada.Strings.Unbounded.Unbounded_String;

                              Adafied_Name : Ada.Strings.Unbounded.Unbounded_String;
                           begin
                              Adaify_Name (Old_Name => Third.Name.Value,
                                           New_Name => Adafied_Name);
                              Adaify_Array_Type_Name (Old_Name => Third.Name.Value,
                                                      New_Name => Adafied_Array_Type_Name);

                              Put_Tabs (3);
                              Puts (To_String (Adafied_Name));
                              Put (" : ");
                              Puts (To_String (Adafied_Array_Type_Name));
                              if Is_Last then
                                 Put_Line ("");
                              else
                                 Put_Line (";");
                              end if;
                           end;
                        else
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                           Aida.Text_IO.Put_Line (To_String (Command_V));
                        end if;
                     end;
                  else
                     Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                     Aida.Text_IO.Put_Line (To_String (Command_V));
                  end if;
               end Generate_Code_For_Parameter;

            begin
               for Param of Params loop
                  Generate_Code_For_Parameter (Param, Param = Params.Last_Element);
               end loop;
            end Generate_Code_For_The_Subprogram_Parameters_If_Any;

            procedure Generate_Code_For_The_Subprogram_Ending is
            begin
               if Params.Length > 0 then
                  if Is_Function then
                     Puts_Line (") return " & To_String (Return_Type) & " with");
                  else
                     Put_Line (") with");
                  end if;
               else
                  Put_Line (" with");
               end if;

               Put_Line ("Import        => True,");

               if Generating_Code_For_OS = Windows then
                  Put_Line ("Convention    => Stdcall,");
               else
                  Put_Line ("Convention    => C,");
               end if;

               Puts_Line ("External_Name => """ & To_String (C_Subprogram_Name) & """;");

               Put_Line ("");
            end Generate_Code_For_The_Subprogram_Ending;

            function Shall_Generate_Code_For_Subprogram return Boolean is
            begin
               for Command_Child of Command_V.Children loop
                  case Command_Child.Kind_Id is
                     when Child_Proto =>
                        if Command_Child.Proto.Children.Length = 2 then
                           declare
                              First  : Vk_XML.Proto_Tag.Child_T renames Command_Child.Proto.Children.Element (Command_Child.Proto.Children.First_Index);
                              Second : Vk_XML.Proto_Tag.Child_T renames Command_Child.Proto.Children.Element (Command_Child.Proto.Children.First_Index + 1);
                           begin
                              if
                                First.Kind_Id = Child_Nested_Type and then
                                First.Nested_Type.Exists_Value and then
                                Second.Kind_Id = Child_Name
                              then
                                 case Generating_Code_For_OS is
                                    when Windows =>
                                       if
                                         Second.Name.Value = "vkCreateAndroidSurfaceKHR" or
                                         Second.Name.Value = "vkGetPhysicalDeviceMirPresentationSupportKHR" or
                                         Second.Name.Value = "vkCreateMirSurfaceKHR" or
                                         Second.Name.Value = "vkCreateWaylandSurfaceKHR" or
                                         Second.Name.Value = "vkGetPhysicalDeviceWaylandPresentationSupportKHR" or
                                         Second.Name.Value = "vkGetPhysicalDeviceXlibPresentationSupportKHR" or
                                         Second.Name.Value = "vkCreateXlibSurfaceKHR" or
                                         Second.Name.Value = "vkGetPhysicalDeviceXcbPresentationSupportKHR" or
                                         Second.Name.Value = "vkCreateXcbSurfaceKHR"
                                       then
                                          return False;
                                       end if;
                                 end case;
                              else
                                 Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                                 Aida.Text_IO.Put_Line (To_String (Command_V));
                              end if;
                           end;
                        else
                           Aida.Text_IO.Put (GNAT.Source_Info.Source_Location & ", can't handle ");
                           Aida.Text_IO.Put_Line (To_String (Command_V));
                        end if;
                     when others      => null;
                  end case;
               end loop;

               return True;
            end Shall_Generate_Code_For_Subprogram;

         begin
            Populate_Params_Vector;

            if Shall_Generate_Code_For_Subprogram then
               Generate_Code_For_The_Constant_Access_Types_If_Any (Command_V);
               Generate_Code_For_The_Subprogram_Name;
               Generate_Code_For_The_Subprogram_Parameters_If_Any;
               Generate_Code_For_The_Subprogram_Ending;
            end if;
         end Handle_Command;

         procedure Handle_Commands (Commands_V : Vk_XML.Commands_Tag.Ptr)
         is
         begin
            for Command of Commands_V.Children loop
               case Command.Kind_Id is
                  when Child_Command =>
                     Handle_Command (Command.Command.all);
                  when others =>
                     null;
               end case;
            end loop;
         end Handle_Commands;

      begin
         Put_Line ("");
         Put_Tabs (1);Put_Line ("type Major_Version_T is range 0..2**9;");
         Put_Tabs (1);Put_Line ("type Minor_Version_T is range 0..2**9;");
         Put_Tabs (1);Put_Line ("type Patch_Version_T is range 0..2**11;");
         Put_Line ("");
         Put_Tabs (1);Put_Line ("type Version_T is");
         Put_Tabs (1);Put_Line ("   record");
         Put_Tabs (1);Put_Line ("      Major : Major_Version_T;");
         Put_Tabs (1);Put_Line ("      Minor : Minor_Version_T;");
         Put_Tabs (1);Put_Line ("      Patch : Patch_Version_T;");
         Put_Tabs (1);Put_Line ("   end record;");
         Put_Tabs (1);Put_Line ("pragma Pack (Version_T);");
         Put_Tabs (1);Put_Line ("for Version_T'Size use 32;");
         Put_Tabs (1);Put_Line ("for Version_T use");
         Put_Tabs (1);Put_Line ("   record");
         Put_Tabs (1);Put_Line ("      Major at 0 range 22 .. 31;");
         Put_Tabs (1);Put_Line ("      Minor at 0 range 12 .. 21;");
         Put_Tabs (1);Put_Line ("      Patch at 0 range 0  .. 11;");
         Put_Tabs (1);Put_Line ("   end record;");
         Put_Line ("");
         Put_Tabs (1);Put_Line ("API_Version : constant Version_T := (Major => 1, Minor => 0, Patch => 21);");
         Put_Line ("");

         Generate_Code_For_Special_Types;

         Generate_Code_For_The_Enum_Types;

         for Request_Child of R.Children loop
            case Request_Child.Kind_Id is
               when Child_Comment =>
                  null;
               when Child_Out_Commented_Message =>
                  null;
               when Child_Vendor_Ids =>
                  null;
               when Child_Tags =>
                  null;
               when Child_Types =>
                  Handle_Child_Types (Request_Child.Types, R.all);
               when Child_Enums =>
                  null;
               when Child_Commands =>
                  Handle_Commands (Request_Child.Commands);
               when Child_Feature =>
                  null;
               when Child_Extensions =>
                  null;
               when Child_XML_Text =>
                  null;
            end case;
         end loop;
      end Generate_Code_For_The_Public_Part;

      procedure Generate_Code_For_The_Private_Part is

         procedure Handle_Child_Types (Types_V : Vk_XML.Types_Tag.Ptr;
                                       R       : Vk_XML.Registry_Tag.Ptr)
         is

            procedure Handle_Child_Type_In_The_Private_Part (Type_V : Vk_XML.Type_Tag.Ptr;
                                                             R      : Vk_XML.Registry_Tag.Ptr)
            is
            begin
               if
                 Type_V.Exists_Category and then Type_V.Category = "handle" and then
                 Type_V.Children.Length = 4
               then
                  declare
                     Nested_Type_Element   : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index);
                     Left_Bracket_Element  : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 1);
                     Name_Element          : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 2);
                     Right_Bracket_Element : Vk_XML.Type_Tag.Child_T renames Type_V.Children.Element (Type_V.Children.First_Index + 3);
                  begin
                     if
                       (Left_Bracket_Element.Kind_Id = Child_XML_Text and then
                        Left_Bracket_Element.XML_Text.all = "(") and
                       (Right_Bracket_Element.Kind_Id = Child_XML_Text and then
                        Right_Bracket_Element.XML_Text.all = ")") and
                       (Nested_Type_Element.Kind_Id = Child_Nested_Type and then
                        Nested_Type_Element.Nested_Type.Exists_Value and then
                            (String (Nested_Type_Element.Nested_Type.Value) = "VK_DEFINE_HANDLE" or String (Nested_Type_Element.Nested_Type.Value) = "VK_DEFINE_NON_DISPATCHABLE_HANDLE")) and
                       Name_Element.Kind_Id = Child_Name
                     then
--                          declare
--                             New_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
--                          begin
--                             Adaify_Type_Name (Old_Name => To_String (Value (Name_Element.Name)),
--                                               New_Name => New_Type_Name);
--
--                             declare
--                                Hidden_Type_Name : String := "Hidden_" & To_String (New_Type_Name);
--                             begin
--                                Put_Tabs (1);
--                                Put ("type ");
--                                Put (Hidden_Type_Name);
--                                Put_Line (" is null record;");
--
--                                Put_Tabs (1);
--                                Put ("type ");
--                                Put (To_String (New_Type_Name));
--                                Put (" is access ");
--                                Put (Hidden_Type_Name);
--                                Put_Line (";");
--                                Put_Line ("");
--                             end;
--                          end;
                        null;
                     end if;
                  end;
               end if;
            end Handle_Child_Type_In_The_Private_Part;

         begin
            for Types_Child of Types_V.Children loop
               case Types_Child.Kind_Id is
                  when Child_Type => Handle_Child_Type_In_The_Private_Part (Types_Child.Type_V, R);
                  when others     => null;
               end case;
            end loop;
         end Handle_Child_Types;

         procedure Generate_Code_For_Private_Part_Of_Special_Types is
         begin
--              Put_Tabs (1); Put_Line ("type Void_Record_T is null record;");
--              Put_Tabs (1); Put_Line ("type Void_Ptr is access all Void_Record_T;");
--              Put_Line ("");
            null;
         end Generate_Code_For_Private_Part_Of_Special_Types;

      begin
         Generate_Code_For_Private_Part_Of_Special_Types;

         for Request_Child of R.Children loop
            case Request_Child.Kind_Id is
               when Child_Types =>
                  Handle_Child_Types (Request_Child.Types, R);
               when others =>
                  null;
            end case;
         end loop;
      end Generate_Code_For_The_Private_Part;

      procedure Initialize_Global_Variables is

         procedure Add (C_Type_Name : String;
                        Ada_Type_Name : String)
         is
            CN : Ada.Strings.Unbounded.Unbounded_String;
            AN : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Set_Unbounded_String (CN, C_Type_Name);
            Set_Unbounded_String (AN, Ada_Type_Name);
            C_Type_Name_To_Ada_Name_Map_Owner.Insert (Container => C_Type_Name_To_Ada_Name_Map,
                                                      Key       => CN,
                                                      New_Item  => AN);
         end Add;

      begin
         C_Type_Name_To_Ada_Name_Map_Owner.Clear (C_Type_Name_To_Ada_Name_Map);
         Add ("size_t", "Interfaces.C.size_t");
         Add ("int32_t", "Interfaces.Integer_32");
         Add ("uint8_t", "Interfaces.Unsigned_8");
         Add ("uint32_t", "Interfaces.Unsigned_32");
         Add ("uint64_t", "Interfaces.Unsigned_64");
         Add ("float", "Interfaces.C.C_float");
         Add ("char", "Interfaces.C.char");
         Add ("void*", "Void_Ptr");
         Add ("const void*", "Void_Ptr");
         Add ("void**", "Void_Ptr_Array_Conversions.Object_Address");
         Add ("char*", "Interfaces.C.Strings.chars_ptr");
         Add ("const char*", "Interfaces.C.Strings.chars_ptr");
         Add ("const char* const*", "Char_Ptr_Array_Conversions.Object_Address");
      end Initialize_Global_Variables;

   begin
      Initialize_Global_Variables;

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => "vk.ads");
      Put_Line ("with Interfaces.C.Strings;");
      Put_Line ("with Generic_Address_To_Access_Conversions;");
      Put_Line ("with System;");
      Put_Line ("");
      Put_Line ("package Vk is");

      Generate_Code_For_The_Public_Part;

      Put_Line ("");
      Put_Line ("private");
      Put_Line ("");

      Generate_Code_For_The_Private_Part;

      Put_Line ("");
      Put_Line ("end Vk;");

      Ada.Text_IO.Close (File);
   end Create_Vk_Package;

end Vk_Package_Creator;
