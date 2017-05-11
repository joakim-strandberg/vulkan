with Ada.Text_IO;
with GNAT.Source_Info;
with Std_String;
with Ada.Strings.Hash;
with Ada.Exceptions;
with Ada.Containers.Hashed_Maps;
with Aida.Strings;
with Current_Tag;
with Current_Tag_Fs;
with Aida.XML;
with Aida.Generic_Subprogram_Call_Result;
with Aida.Containers;
with Aida.XML.Generic_Parse_XML_File;
with Ada.Characters.Latin_1;

with Aida.Text_IO;
with Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable;

pragma Elaborate_All (Aida.Generic_Subprogram_Call_Result);
pragma Elaborate_All (Aida.XML.Generic_Parse_XML_File);
pragma Elaborate_All (Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable);
with Ada.Strings.Unbounded;

package body Vk_XML_Reader is

   XML_Tag_Registry                                 : constant String := "registry";
   XML_Tag_Comment                                  : constant String := "comment";
   XML_Tag_Vendor_Ids                               : constant String := "vendorids";
   XML_Tag_Vendor_Id                                : constant String := "vendorid";
   XML_Tag_Vendor_Id_Attribute_Name                 : constant String := "name";
   XML_Tag_Vendor_Id_Attribute_Id                   : constant String := "id";
   XML_Tag_Vendor_Id_Attribute_Comment              : constant String := "comment";
   XML_Tag_Tags                                     : constant String := "tags";
   XML_Tag_Tag                                      : constant String := "tag";
   XML_Tag_Tag_Attribute_Name                       : constant String := "name";
   XML_Tag_Tag_Attribute_Author                     : constant String := "author";
   XML_Tag_Tag_Attribute_Contact                    : constant String := "contact";
   XML_Tag_Types                                    : constant String := "types";
   XML_Tag_Type                                     : constant String := "type";
   XML_Tag_Type_Attribute_Name                      : constant String := "name";
   XML_Tag_Type_Attribute_Category                  : constant String := "category";
   XML_Tag_Type_Attribute_Requires                  : constant String := "requires";
   XML_Tag_Type_Attribute_Parent                    : constant String := "parent";
   XML_Tag_Type_Attribute_Returned_Only             : constant String := "returnedonly";
   XML_Tag_Type_Attribute_Comment                   : constant String := "comment";
   XML_Tag_Name                                     : constant String := "name";
   XML_Tag_Member                                   : constant String := "member";
   XML_Tag_Member_Attribute_Optional                : constant String := "optional";
   XML_Tag_Member_Attribute_Len                     : constant String := "len";
   XML_Tag_Member_Attribute_No_Auto_Validity        : constant String := "noautovalidity";
   XML_Tag_Member_Attribute_Valid_Extension_Structs : constant String := "validextensionstructs";
   XML_Tag_Validity                                 : constant String := "validity";
   XML_Tag_Usage                                    : constant String := "usage";
   XML_Tag_Usage_Attribute_Command                  : constant String := "command";
   XML_Tag_Usage_Attribute_Struct                   : constant String := "struct";
   XML_Tag_Enum                                     : constant String := "enum";
   XML_Tag_Enums                                    : constant String := "enums";
   XML_Tag_Enums_Attribute_Name                     : constant String := "name";
   XML_Tag_Enums_Attribute_Comment                  : constant String := "comment";
   XML_Tag_Enums_Attribute_Type                     : constant String := "type";
   XML_Tag_Enums_Enum_Attribute_Value               : constant String := "value";
   XML_Tag_Enums_Enum_Attribute_Name                : constant String := "name";
   XML_Tag_Enums_Enum_Attribute_Comment             : constant String := "comment";
   XML_Tag_Enums_Enum_Attribute_Bit_Position        : constant String := "bitpos";
   XML_Tag_Unused                                   : constant String := "unused";
   XML_Tag_Unused_Attribute_Start                   : constant String := "start";
   XML_Tag_Commands                                 : constant String := "commands";
   XML_Tag_Command                                  : constant String := "command";
   XML_Tag_Command_Attribute_Success_Codes          : constant String := "successcodes";
   XML_Tag_Command_Attribute_Error_Codes            : constant String := "errorcodes";
   XML_Tag_Command_Attribute_Queues                 : constant String := "queues";
   XML_Tag_Command_Attribute_Render_Pass            : constant String := "renderpass";
   XML_Tag_Command_Attribute_Cmd_Buffer_Level       : constant String := "cmdbufferlevel";
   XML_Tag_Proto                                    : constant String := "proto";
   XML_Tag_Param                                    : constant String := "param";
   XML_Tag_Param_Attribute_Optional                 : constant String := "optional";
   XML_Tag_Param_Attribute_External_Sync            : constant String := "externsync";
   XML_Tag_Param_Attribute_Len                      : constant String := "len";
   XML_Tag_Param_Attribute_No_Auto_Validity         : constant String := "noautovalidity";
   XML_Tag_Implicit_External_Syns_Params            : constant String := "implicitexternsyncparams";
   XML_Tag_External_Sync_Parameter                  : constant String := "param";
   XML_Tag_Feature                                  : constant String := "feature";
   XML_Tag_Feature_Attribute_API                    : constant String := "api";
   XML_Tag_Feature_Attribute_Name                   : constant String := "name";
   XML_Tag_Feature_Attribute_Number                 : constant String := "number";
   XML_Tag_Require                                  : constant String := "require";
   XML_Tag_Require_Attribute_Comment                : constant String := "comment";
   XML_Tag_Require_Enum_Attribute_Name              : constant String := "name";
   XML_Tag_Require_Enum_Attribute_Value             : constant String := "value";
   XML_Tag_Require_Enum_Attribute_Offset            : constant String := "offset";
   XML_Tag_Require_Enum_Attribute_Dir               : constant String := "dir";
   XML_Tag_Require_Enum_Attribute_Extends           : constant String := "extends";
   XML_Tag_Require_Enum_Attribute_Comment           : constant String := "comment";
   XML_Tag_Require_Enum_Attribute_Bit_Position      : constant String := "bitpos";
   XML_Tag_Require_Command_Attribute_Name           : constant String := "name";
   XML_Tag_Extensions                               : constant String := "extensions";
   XML_Tag_Extension                                : constant String := "extension";
   XML_Tag_Extension_Attribute_Name                 : constant String := "name";
   XML_Tag_Extension_Attribute_Number               : constant String := "number";
   XML_Tag_Extension_Attribute_Supported            : constant String := "supported";
   XML_Tag_Extension_Attribute_Protect              : constant String := "protect";
   XML_Tag_Extension_Attribute_Author               : constant String := "author";
   XML_Tag_Extension_Attribute_Contact              : constant String := "contact";

   use all type Ada.Strings.Unbounded.Unbounded_String;
   use all type Aida.XML.Tag_Name.T;
   use all type Aida.XML.Tag_Name_Vectors.Vector;
   use all type Aida.XML.Subprogram_Call_Result.T;
   use all type Aida.Containers.Count_Type;
   use all type Current_Tag.T;
   use all type Vk_XML2.XML_Text_T;
   use all type Vk_XML2.XML_Out_Commented_Message_T;
   use all type Vk_XML2.Registry.Child_Kind_Id_T;
   use all type Vk_XML2.Vendor_Ids.Child_Kind_Id_T;
   use all type Vk_XML2.Tags.Child_Kind_Id_T;
   use all type Vk_XML2.Types.Child_Kind_Id_T;
   use all type Vk_XML2.Type_T.Child_Kind_Id_T;
   use all type Vk_XML2.Member.Child_Kind_Id_T;
   use all type Vk_XML2.Validity.Child_Kind_Id_T;
   use all type Vk_XML2.Usage.Child_Kind_Id_T;
   use all type Vk_XML2.Enums.Child_Kind_Id_T;
   use all type Vk_XML2.Enums.Type_Attribue_T;
   use all type Vk_XML2.Commands.Child_Kind_Id_T;
   use all type Vk_XML2.Command.Child_Kind_Id_T;
   use all type Vk_XML2.Command.Queue_T;
   use all type Vk_XML2.Command.Render_Pass_T;
   use all type Vk_XML2.Command.Command_Buffer_Level_T;
   use all type Vk_XML2.Proto.Child_Kind_Id_T;
   use all type Vk_XML2.Param.Child_Kind_Id_T;
   use all type Vk_XML2.Implicit_External_Sync_Parameters.Child_Kind_Id_T;
   use all type Vk_XML2.Feature.Child_Kind_Id_T;
   use all type Vk_XML2.Require.Child_Kind_Id_T;
   use all type Vk_XML2.Extension.Child_Kind_Id_T;
   use all type Vk_XML2.Extension.Supported_T;
   use all type Vk_XML2.Extensions.Child_Kind_Id_T;
   use all type Vk_XML2.Vendor_Id.Name_T;
   use all type Vk_XML2.Vendor_Id.Id_T;
   use all type Vk_XML2.Vendor_Id.Comment_T;
   use all type Vk_XML2.Tag.Name_T;
   use all type Vk_XML2.Tag.Author_T;
   use all type Vk_XML2.Tag.Contact_T;
   use all type Vk_XML2.Type_T.Category_T;
   use all type Vk_XML2.Type_T.Returned_Only_T;
   use all type Vk_XML2.Member.No_Auto_Validity_T;
   use all type Vk_XML2.Member.Optional_T;
   use all type Vk_XML2.Command.Success_Code_T;
   use all type Vk_XML2.Command.Error_Code_T;
   use all type Vk_XML2.Param.Optional_T;
   use all type Vk_XML2.Comment.Value_T;
   use all type Vk_XML2.Name.Value_T;
   use all type Vk_XML2.Nested_Type.Nullable_Value_T;
   use all type Vk_XML2.Enum.Value_T;

   use Current_Tag_Fs.Tag_Id;

   --     package Mutable_XML_Out_Commented_Message_Shared_Ptr is new Vk_XML2.XML_Out_Commented_Message_Shared_Ptr.Mutable;
   --
   --     use all type Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;

   --     package Mutable_XML_Text_Shared_Ptr is new Vk_XML2.XML_Text.Mutable;
   --
   --     use all type Mutable_XML_Text_Shared_Ptr.Mutable_T;

   function Make_Current_Tag (Kind_Id    : Current_Tag_Fs.Tag_Id.Enumeration_T;
                              Parent_Tag_Id : Current_Tag_Fs.Id_T) return Current_Tag.T;

   function Make_Current_Tag (Kind_Id       : Current_Tag_Fs.Tag_Id.Enumeration_T;
                              Parent_Tag_Id : Current_Tag_Fs.Id_T) return Current_Tag.T
   is
      R : Current_Tag.T (Kind_Id);
   begin
      R.Parent_Tag := Parent_Tag_Id;
      return R;
   end Make_Current_Tag;

   function Hash (Parent_And_Self_Tags : Aida.XML.Tag_Name_Vectors.Vector) return Ada.Containers.Hash_Type is
      R : Ada.Containers.Hash_Type := 0;

      use type Ada.Containers.Hash_Type;
   begin
      if Is_Empty (Parent_And_Self_Tags) then
         return 0;
      end if;

      for I in Integer range First_Index (Parent_And_Self_Tags)..Last_Index (Parent_And_Self_Tags) loop
         R := R + Ada.Strings.Hash (To_String (Element (Parent_And_Self_Tags, I)));
      end loop;
      R := R mod Ada.Containers.Hash_Type (Natural'Last);
      return R;
   end Hash;

   package Current_Tag_To_Tags_Map_Type_Owner is new Ada.Containers.Hashed_Maps (Key_Type        => Aida.XML.Tag_Name_Vector_T,
                                                                                 Element_Type    => Current_Tag.T,
                                                                                 Hash            => Hash,
                                                                                 Equivalent_Keys => Aida.XML.Tag_Name_Vectors."=",
                                                                                 "="             => Current_Tag."=");

   use type Current_Tag_To_Tags_Map_Type_Owner.Cursor;

   Parents_Including_Self_To_Current_Tag_Map : Current_Tag_To_Tags_Map_Type_Owner.Map;

   package Mutable_Tag_Name is new Aida.XML.Tag_Name.Mutable;

   use all type Mutable_Tag_Name.Mutable_T;

   procedure Populate_Parents_Including_Self (Parents_Including_Self : in out Aida.XML.Tag_Name_Vectors.Vector;
                                              Parents                : Aida.XML.Tag_Name_Vectors.Vector;
                                              Tag_Name               : String)
   is
   begin
      for I in Integer range First_Index (Parents)..Last_Index (Parents) loop
         declare
            N : Mutable_Tag_Name.Mutable_T;
         begin
            Mutable_Tag_Name.Initialize (This => N,
                                         Text => Aida.XML.Tag_Name.To_String (Element (Parents,  I)));
            Append (Container => Parents_Including_Self,
                    New_Item  => Aida.XML.Tag_Name.T (N));
         end;
      end loop;

      declare
         N : Mutable_Tag_Name.Mutable_T;
      begin
         Mutable_Tag_Name.Initialize (N, Tag_Name);
         Append (Container => Parents_Including_Self,
                 New_Item  => Aida.XML.Tag_Name.T (N));
      end;
   end Populate_Parents_Including_Self;

   type Find_Tag_Call_Result_T (Exists : Boolean := False) is
      record
         case Exists is
            when True  => Current_Tag_V : Current_Tag.T;
            when False => null;
         end case;
      end record;

   function To_String (Tags : Aida.XML.Tag_Name_Vector_T) return String is
      R : Aida.Strings.Unbounded_String_Type;
   begin
      for I in Integer range First_Index (Tags)..Last_Index (Tags) loop
         R.Append (Aida.XML.Tag_Name.To_String (Element (Tags, I)) & ", ");
      end loop;

      return R.To_String;
   end To_String;

   function Find_Tag (Key : Aida.XML.Tag_Name_Vector_T) return Find_Tag_Call_Result_T
   is
   begin
      declare
         Searched_For : Current_Tag_To_Tags_Map_Type_Owner.Cursor :=
           Current_Tag_To_Tags_Map_Type_Owner.Find (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                    Key       => Key);
      begin
         if Searched_For /= Current_Tag_To_Tags_Map_Type_Owner.No_Element then
            return (Exists => True,
                    Current_Tag_V => Current_Tag_To_Tags_Map_Type_Owner.Element (Parents_Including_Self_To_Current_Tag_Map, Key));
         else
            return (Exists => False);
         end if;
      end;
   end Find_Tag;

   procedure Parse (Contents    : String;
                    Registry    : not null access Vk_XML2.Registry.T;
                    Subpool     : Dynamic_Pools.Subpool_Handle;
                    Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
   is
      procedure Start_Tag (Tag_Name    : String;
                           Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                           Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure Start_Tag (Tag_Name    : String;
                           Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                           Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Parents_Including_Self : Aida.XML.Tag_Name_Vector_T;

         CR : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         if not CR.Exists then
            if Length (Parent_Tags) > 0 then
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Could not find parent of " & To_String (Parent_Tags) & ", " & Tag_Name);
            else
               if Tag_Name = XML_Tag_Registry then
                  declare
                     Current_Tag_V : Current_Tag.T (Current_Tag_Fs.Tag_Id.Registry);
                  begin
                     Current_Tag_V.Registry := Registry;

                     Current_Tag.Initialize (This => Current_Tag_V);

                     Parents_Including_Self_To_Current_Tag_Map.Insert (Parents_Including_Self, Current_Tag_V);
                  end;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Expected " & XML_Tag_Registry & ", but found " & Tag_Name);
               end if;
            end if;
         else
            declare
               Prev_Tag : Current_Tag.T := CR.Current_Tag_V;
            begin
               case Prev_Tag.Kind_Id is
               when Current_Tag_Fs.Tag_Id.Registry =>
                  if Tag_Name = XML_Tag_Comment then
                     declare
                        Comment : not null Vk_XML2.Comment.Ptr := new Vk_XML2.Comment.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id => Child_Comment,
                                                             C       => Comment);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Comment);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Comment := Comment;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Vendor_Ids then
                     declare
                        Vendor_Ids_V : not null Vk_XML2.Vendor_Ids.Ptr := new Vk_XML2.Vendor_Ids.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id      => Child_Vendor_Ids,
                                                             Vendor_Ids_V => Vendor_Ids_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Vendor_Ids);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Vendor_Ids_V := Vendor_Ids_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Tags then
                     declare
                        Tags_V : not null Vk_XML2.Tags.Ptr := new Vk_XML2.Tags.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id => Child_Tags,
                                                             Tags_V  => Tags_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Tags);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Tags_V := Tags_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Types then
                     declare
                        Types_V : not null Vk_XML2.Types.Ptr := new Vk_XML2.Types.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id => Child_Types,
                                                             Types_V  => Types_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Types);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Types_V := Types_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enums then
                     declare
                        Enums_V : not null Vk_XML2.Enums.Ptr := new Vk_XML2.Enums.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id => Child_Enums,
                                                             Enums_V  => Enums_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Enums);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enums_V := Enums_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Commands then
                     declare
                        Commands_V : not null Vk_XML2.Commands.Ptr := new Vk_XML2.Commands.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id    => Child_Commands,
                                                             Commands_V => Commands_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Commands);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Commands_V := Commands_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Feature then
                     declare
                        Feature_V : not null Vk_XML2.Feature.Ptr := new Vk_XML2.Feature.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id   => Child_Feature,
                                                             Feature_V => Feature_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Feature);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Feature_V := Feature_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Extensions then
                     declare
                        Extensions_V : not null Vk_XML2.Extensions.Ptr := new Vk_XML2.Extensions.T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id   => Child_Extensions,
                                                             Extensions_V => Extensions_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Extensions);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Extensions_V := Extensions_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Registry.Children.Append (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Vendor_Ids =>
                  if Tag_Name = XML_Tag_Vendor_Id then
                     declare
                        Vendor_Id_V : not null Vk_XML2.Vendor_Id.Ptr := new Vk_XML2.Vendor_Id.T;
                        Child : Vk_XML2.Vendor_Ids.Child_T := (Kind_Id     => Child_Vendor_Id,
                                                               Vendor_Id_V => Vendor_Id_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Vendor_Id);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Vendor_Id_V := Vendor_Id_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Vendor_Ids_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Tags =>
                  if Tag_Name = XML_Tag_Tag then
                     declare
                        Tag_V : not null Vk_XML2.Tag.Ptr := new Vk_XML2.Tag.T;
                        Child : Vk_XML2.Tags.Child_T := (Kind_Id => Child_Tag,
                                                         Tag_V   => Tag_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Tag);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Tag_V := Tag_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Tags_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Types =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Type_V : not null Vk_XML2.Type_T.Ptr := new Vk_XML2.Type_T.T;
                        Child : Vk_XML2.Types.Child_T := (Kind_Id => Child_Type,
                                                          Type_V  => Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Type_T);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Type_V := Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Types_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Type_T =>
                  if Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML2.Name.Ptr := new Vk_XML2.Name.T;
                        Child : Vk_XML2.Type_T.Child_T := (Kind_Id => Child_Name,
                                                           Name_V  => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML2.Nested_Type.Ptr := new Vk_XML2.Nested_Type.T;
                        Child : Vk_XML2.Type_T.Child_T := (Kind_Id       => Child_Nested_Type,
                                                           Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Member then
                     declare
                        Member_V : not null Vk_XML2.Member.Ptr := new Vk_XML2.Member.T;
                        Child : Vk_XML2.Type_T.Child_T := (Kind_Id  => Child_Member,
                                                           Member_V => Member_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Member);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Member_V := Member_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Validity then
                     declare
                        Validity_V : not null Vk_XML2.Validity.Ptr := new Vk_XML2.Validity.T;
                        Child : Vk_XML2.Type_T.Child_T := (Kind_Id    => Child_Validity,
                                                           Validity_V => Validity_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Validity);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Validity_V := Validity_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Member =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML2.Nested_Type.Ptr := new Vk_XML2.Nested_Type.T;
                        Child : Vk_XML2.Member.Child_T := (Kind_Id       => Child_Nested_Type,
                                                           Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Member_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML2.Name.Ptr := new Vk_XML2.Name.T;
                        Child : Vk_XML2.Member.Child_T := (Kind_Id => Child_Name,
                                                           Name_V  => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Member_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enum then
                     declare
                        Enum_V : not null Vk_XML2.Enum.Ptr := new Vk_XML2.Enum.T;
                        Child : Vk_XML2.Member.Child_T := (Kind_Id => Child_Enum,
                                                           Enum_V  => Enum_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enum_V := Enum_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Member_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Validity =>
                  if Tag_Name = XML_Tag_Usage then
                     declare
                        Usage_V : not null Vk_XML2.Usage.Ptr := new Vk_XML2.Usage.T;
                        Child : Vk_XML2.Validity.Child_T := (Kind_Id => Child_Usage,
                                                             Usage_V => Usage_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Usage);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Usage_V := Usage_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Validity_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Enums =>
                  if Tag_Name = XML_Tag_Enum then
                     declare
                        Enums_Enum_V : not null Vk_XML2.Enums_Enum.Ptr := new Vk_XML2.Enums_Enum.T;
                        Child : Vk_XML2.Enums.Child_T := (Kind_Id      => Child_Enums_Enum,
                                                          Enums_Enum_V => Enums_Enum_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Enums_Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enums_Enum_V := Enums_Enum_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Enums_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Unused then
                     declare
                        Unused_V : not null Vk_XML2.Unused.Ptr := new Vk_XML2.Unused.T;
                        Child : Vk_XML2.Enums.Child_T := (Kind_Id      => Child_Unused,
                                                          Unused_V => Unused_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Unused);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Unused_V := Unused_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Enums_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Commands =>
                  if Tag_Name = XML_Tag_Command then
                     declare
                        Command_V : not null Vk_XML2.Command.Ptr := new Vk_XML2.Command.T;
                        Child : Vk_XML2.Commands.Child_T := (Kind_Id   => Child_Command,
                                                             Command_V => Command_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Command);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Command_V := Command_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Commands_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Command =>
                  if Tag_Name = XML_Tag_Proto then
                     declare
                        Proto_V : not null Vk_XML2.Proto.Ptr := new Vk_XML2.Proto.T;
                        Child : Vk_XML2.Command.Child_T := (Kind_Id => Child_Proto,
                                                            Proto_V => Proto_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Proto);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Proto_V := Proto_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Param then
                     declare
                        Param_V : not null Vk_XML2.Param.Ptr := new Vk_XML2.Param.T;
                        Child : Vk_XML2.Command.Child_T := (Kind_Id => Child_Param,
                                                            Param_V => Param_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Param);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Param_V := Param_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Validity then
                     declare
                        Validity_V : not null Vk_XML2.Validity.Ptr := new Vk_XML2.Validity.T;
                        Child : Vk_XML2.Command.Child_T := (Kind_Id => Child_Validity,
                                                            Validity_V => Validity_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Validity);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Validity_V := Validity_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Implicit_External_Syns_Params then
                     declare
                        Parameters_V : not null Vk_XML2.Implicit_External_Sync_Parameters.Ptr := new Vk_XML2.Implicit_External_Sync_Parameters.T;
                        Child : Vk_XML2.Command.Child_T := (Kind_Id      => Child_Implicit_External_Sync_Parameters,
                                                            Parameters_V => Parameters_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Implicit_External_Sync_Parameters);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Parameters_V := Parameters_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Proto =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML2.Nested_Type.Ptr := new Vk_XML2.Nested_Type.T;
                        Child : Vk_XML2.Proto.Child_T := (Kind_Id       => Child_Nested_Type,
                                                          Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Proto_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML2.Name.Ptr := new Vk_XML2.Name.T;
                        Child : Vk_XML2.Proto.Child_T := (Kind_Id       => Child_Name,
                                                          Name_V => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Proto_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Param =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML2.Nested_Type.Ptr := new Vk_XML2.Nested_Type.T;
                        Child : Vk_XML2.Param.Child_T := (Kind_Id       => Child_Nested_Type,
                                                          Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Param_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML2.Name.Ptr := new Vk_XML2.Name.T;
                        Child : Vk_XML2.Param.Child_T := (Kind_Id       => Child_Name,
                                                          Name_V => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Param_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Implicit_External_Sync_Parameters =>
                  if Tag_Name = XML_Tag_External_Sync_Parameter then
                     declare
                        External_Sync_Parameter_V : not null Vk_XML2.External_Sync_Parameter.Ptr := new Vk_XML2.External_Sync_Parameter.T;
                        Child : Vk_XML2.Implicit_External_Sync_Parameters.Child_T := (Kind_Id                   => Child_External_Sync_Parameter,
                                                                                      External_Sync_Parameter_V => External_Sync_Parameter_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.External_Sync_Parameter);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.External_Sync_Parameter_V := External_Sync_Parameter_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Parameters_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Feature =>
                  if Tag_Name = XML_Tag_Require then
                     declare
                        Require_V : not null Vk_XML2.Require.Ptr := new Vk_XML2.Require.T;
                        Child : Vk_XML2.Feature.Child_T := (Kind_Id   => Child_Require,
                                                            Require_V => Require_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_V := Require_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Feature_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Require =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Type_V : not null Vk_XML2.Type_T.Ptr := new Vk_XML2.Type_T.T;
                        Child : Vk_XML2.Require.Child_T := (Kind_Id => Child_Type,
                                                            Type_V  => Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Type_T);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Type_V := Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enum then
                     declare
                        Enum_V : not null Vk_XML2.Require_Enum.Ptr := new Vk_XML2.Require_Enum.T;
                        Child : Vk_XML2.Require.Child_T := (Kind_Id => Child_Enum,
                                                            Enum_V  => Enum_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require_Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_Enum_V := Enum_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Command then
                     declare
                        Command_V : not null Vk_XML2.Require_Command.Ptr := new Vk_XML2.Require_Command.T;
                        Child : Vk_XML2.Require.Child_T := (Kind_Id => Child_Command,
                                                            Command_V  => Command_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require_Command);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_Command_V := Command_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Usage then
                     declare
                        Usage_V : not null Vk_XML2.Usage.Ptr := new Vk_XML2.Usage.T;
                        Child : Vk_XML2.Require.Child_T := (Kind_Id => Child_Usage,
                                                            Usage_V => Usage_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Usage);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Usage_V := Usage_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Extensions =>
                  if Tag_Name = XML_Tag_Extension then
                     declare
                        Extension_V : not null Vk_XML2.Extension.Ptr := new Vk_XML2.Extension.T;
                        Child : Vk_XML2.Extensions.Child_T := (Kind_Id     => Child_Extension,
                                                               Extension_V => Extension_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Extension);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Extension_V := Extension_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Extensions_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Extension =>
                  if Tag_Name = XML_Tag_Require then
                     declare
                        Require_V : not null Vk_XML2.Require.Ptr := new Vk_XML2.Require.T;
                        Child : Vk_XML2.Extension.Child_T := (Kind_Id   => Child_Require,
                                                              Require_V => Require_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_V := Require_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Prev_Tag.Extension_V.Children.Append (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
                  end if;
               when Current_Tag_Fs.Tag_Id.Comment |
                    Current_Tag_Fs.Tag_Id.Vendor_Id |
                    Current_Tag_Fs.Tag_Id.Tag |
                    Current_Tag_Fs.Tag_Id.Name |
                    Current_Tag_Fs.Tag_Id.Nested_Type |
                    Current_Tag_Fs.Tag_Id.Usage |
                    Current_Tag_Fs.Tag_Id.Enum |
                    Current_Tag_Fs.Tag_Id.Enums_Enum |
                    Current_Tag_Fs.Tag_Id.Unused |
                    Current_Tag_Fs.Tag_Id.External_Sync_Parameter |
                    Current_Tag_Fs.Tag_Id.Require_Enum |
                    Current_Tag_Fs.Tag_Id.Require_Command =>
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end case;
            end;
         end if;
      end Start_Tag;

      procedure Attribute (Attribute_Name              : String;
                           Attribute_Value             : String;
                           Parent_Tags_And_Current_Tag : Aida.XML.Tag_Name_Vector_T;
                           Call_Result                 : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure Attribute (Attribute_Name              : String;
                           Attribute_Value             : String;
                           Parent_Tags_And_Current_Tag : Aida.XML.Tag_Name_Vector_T;
                           Call_Result                 : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Searched_For : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags_And_Current_Tag);
      begin
         if not Searched_For.Exists then
            Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", attribute name " & Attribute_Name & " and value " & Attribute_Value & ", parents: " & To_String (Parent_Tags_And_Current_Tag));
            return;
         end if;

         declare
            Current_Tag_V : Current_Tag.T := Searched_For.Current_Tag_V;
         begin
            case Current_Tag_V.Kind_Id is
            when Current_Tag_Fs.Tag_Id.Vendor_Id =>
               if Attribute_Name = XML_Tag_Vendor_Id_Attribute_Name then
                  Current_Tag_V.Vendor_Id_V.Name := To_Unbounded_String (Attribute_Value);
               elsif Attribute_Name = XML_Tag_Vendor_Id_Attribute_Id then
                  Current_Tag_V.Vendor_Id_V.Id := To_Unbounded_String (Attribute_Value);
               elsif Attribute_Name = XML_Tag_Vendor_Id_Attribute_Comment then
                  Current_Tag_V.Vendor_Id_V.Comment := To_Unbounded_String (Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Tag =>
               if Attribute_Name = XML_Tag_Tag_Attribute_Name then
                  Current_Tag_V.Tag_V.Name := To_Unbounded_String (Attribute_Value);
               elsif Attribute_Name = XML_Tag_Tag_Attribute_Author then
                  Current_Tag_V.Tag_V.Author := To_Unbounded_String (Attribute_Value);
               elsif Attribute_Name = XML_Tag_Tag_Attribute_Contact then
                  Current_Tag_V.Tag_V.Contact := To_Unbounded_String (Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Type_T =>
               if Attribute_Name = XML_Tag_Type_Attribute_Name then
                  Current_Tag_V.Type_V.Name := (Exists => True,
                                                Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Type_Attribute_Category then
                  Current_Tag_V.Type_V.Category := To_Unbounded_String (Attribute_Value);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Requires then
                  Current_Tag_V.Type_V.Requires := (Exists => True,
                                                    Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Type_Attribute_Parent then
                  Current_Tag_V.Type_V.Parent := (Exists => True,
                                                  Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Type_Attribute_Returned_Only then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Type_V.Returned_Only := True;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Type_Attribute_Comment then
                  Current_Tag_V.Type_V.Comment := (Exists => True,
                                                   Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Member =>
               if Attribute_Name = XML_Tag_Member_Attribute_Optional then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Member_V.Optional := True;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Member_Attribute_Len then
                  Current_Tag_V.Member_V.Len := (Exists => True,
                                                 Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Member_Attribute_No_Auto_Validity then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Member_V.No_Auto_Validity := True;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Member_Attribute_Valid_Extension_Structs then
                  Current_Tag_V.Member_V.Valid_Extension_Structs := (Exists => True,
                                                                     Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Enums =>
               if Attribute_Name = XML_Tag_Enums_Attribute_Name then
                  Current_Tag_V.Enums_V.Name := (Exists => True,
                                                 Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Enums_Attribute_Comment then
                  Current_Tag_V.Enums_V.Comment := (Exists => True,
                                                    Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Enums_Attribute_Type then
                  if Attribute_Value = "enum" then
                     Current_Tag_V.Enums_V.Type_Attribue := (Exists => True,
                                                             Value  => Enum);
                  elsif Attribute_Value = "bitmask" then
                     Current_Tag_V.Enums_V.Type_Attribue := (Exists => True,
                                                             Value  => Bit_Mask);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Enums_Enum =>
               if Attribute_Name = XML_Tag_Enums_Enum_Attribute_Value then
                  Current_Tag_V.Enums_Enum_V.Value := (Exists => True,
                                                       Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Name then
                  Current_Tag_V.Enums_Enum_V.Name := (Exists => True,
                                                      Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Comment then
                  Current_Tag_V.Enums_Enum_V.Comment := (Exists => True,
                                                         Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Bit_Position then
                  declare
                     V : Integer;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => V,
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag_V.Enums_Enum_V.Bit_Position := (Exists => True,
                                                                    Value  => V);
                     end if;
                  end;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Unused =>
               if Attribute_Name = XML_Tag_Unused_Attribute_Start then
                  Current_Tag_V.Unused_V.Start := (Exists => True,
                                                   Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Command =>
               if Attribute_Name = XML_Tag_Command_Attribute_Success_Codes then
                  Current_Tag_V.Command_V.Success_Codes.Append (To_Unbounded_String (Attribute_Value));
                  -- TODO: Split the String into several success codes later
               elsif Attribute_Name = XML_Tag_Command_Attribute_Error_Codes then
                  Current_Tag_V.Command_V.Error_Codes.Append (To_Unbounded_String (Attribute_Value));
                  -- TODO: Split the String into several error codes later
               elsif Attribute_Name = XML_Tag_Command_Attribute_Queues then
                  if Attribute_Value = "sparse_binding" then
                     Current_Tag_V.Command_V.Queues.Append (Sparse_Binding);
                  elsif Attribute_Value = "graphics,compute" then
                     Current_Tag_V.Command_V.Queues.Append (Graphics);
                     Current_Tag_V.Command_V.Queues.Append (Compute);
                  elsif Attribute_Value = "graphics" then
                     Current_Tag_V.Command_V.Queues.Append (Graphics);
                  elsif Attribute_Value = "compute" then
                     Current_Tag_V.Command_V.Queues.Append (Compute);
                  elsif Attribute_Value = "transfer,graphics,compute" then
                     Current_Tag_V.Command_V.Queues.Append (Graphics);
                     Current_Tag_V.Command_V.Queues.Append (Compute);
                     Current_Tag_V.Command_V.Queues.Append (Transfer);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Command_Attribute_Render_Pass then
                  if Attribute_Value = "inside" then
                     Current_Tag_V.Command_V.Render_Passes.Append (Inside);
                  elsif Attribute_Value = "outside" then
                     Current_Tag_V.Command_V.Render_Passes.Append (Outside);
                  elsif Attribute_Value = "both" then
                     Current_Tag_V.Command_V.Render_Passes.Append (Both);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Command_Attribute_Cmd_Buffer_Level then
                  if Attribute_Value = "primary,secondary" then
                     Current_Tag_V.Command_V.Command_Buffer_Levels.Append (Primary);
                     Current_Tag_V.Command_V.Command_Buffer_Levels.Append (Secondary);
                  elsif Attribute_Value = "primary" then
                     Current_Tag_V.Command_V.Command_Buffer_Levels.Append (Primary);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Param =>
               if Attribute_Name = XML_Tag_Param_Attribute_Optional then
                  if Attribute_Value = "true" or Attribute_Value = "false,true" then
                     Current_Tag_V.Param_V.Optional := True;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Param_Attribute_External_Sync then
                  Current_Tag_V.Param_V.External_Sync := (Exists => True,
                                                          Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Param_Attribute_Len then
                  Current_Tag_V.Param_V.Len := (Exists => True,
                                                Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Param_Attribute_No_Auto_Validity then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Param_V.No_Auto_Validity := (Exists => True,
                                                                Value  => True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Feature =>
               if Attribute_Name = XML_Tag_Feature_Attribute_API then
                  Current_Tag_V.Feature_V.API := (Exists => True,
                                                  Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Feature_Attribute_Name then
                  Current_Tag_V.Feature_V.Name := (Exists => True,
                                                   Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Feature_Attribute_Number then
                  Current_Tag_V.Feature_V.Number := (Exists => True,
                                                     Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Require =>
               if Attribute_Name = XML_Tag_Require_Attribute_Comment then
                  Current_Tag_V.Require_V.Comment := (Exists => True,
                                                      Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Require_Enum =>
               if Attribute_Name = XML_Tag_Require_Enum_Attribute_Name then
                  Current_Tag_V.Require_Enum_V.Name := (Exists => True,
                                                        Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Value then
                  Current_Tag_V.Require_Enum_V.Value := (Exists => True,
                                                         Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Offset then
                  declare
                     V : Integer;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => V,
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag_V.Require_Enum_V.Offset := (Exists => True,
                                                                Value  => V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Dir then
                  Current_Tag_V.Require_Enum_V.Dir := (Exists => True,
                                                       Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Extends then
                  Current_Tag_V.Require_Enum_V.Extends := (Exists => True,
                                                           Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Comment then
                  Current_Tag_V.Require_Enum_V.Comment := (Exists => True,
                                                           Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Bit_Position then
                  declare
                     V : Integer;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => V,
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag_V.Require_Enum_V.Bit_Position := (Exists => True,
                                                                      Value  => V);
                     end if;
                  end;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Require_Command =>
               if Attribute_Name = XML_Tag_Require_Command_Attribute_Name then
                  Current_Tag_V.Require_Command_V.Name := (Exists => True,
                                                           Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Extension =>
               if Attribute_Name = XML_Tag_Extension_Attribute_Name then
                  Current_Tag_V.Extension_V.Name := (Exists => True,
                                                     Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Number then
                  declare
                     V : Integer;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => V,
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag_V.Extension_V.Number := (Exists => True,
                                                             Value  => V);

                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Supported then
                  if Attribute_Value = "vulkan" then
                     Current_Tag_V.Extension_V.Supported := (Exists => True,
                                                             Value  => Vulkan);
                  elsif Attribute_Value = "disabled" then
                     Current_Tag_V.Extension_V.Supported := (Exists => True,
                                                             Value  => Disabled);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Protect then
                  Current_Tag_V.Extension_V.Protect := (Exists => True,
                                                        Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Author then
                  Current_Tag_V.Extension_V.Author := (Exists => True,
                                                       Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Contact then
                  Current_Tag_V.Extension_V.Contact := (Exists => True,
                                                        Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Usage =>
               if Attribute_Name = XML_Tag_Usage_Attribute_Command then
                  Current_Tag_V.Usage_V.Command := (Exists => True,
                                                    Value  => To_Unbounded_String (Attribute_Value));
               elsif Attribute_Name = XML_Tag_Usage_Attribute_Struct then
                  Current_Tag_V.Usage_V.Struct := (Exists => True,
                                                   Value  => To_Unbounded_String (Attribute_Value));
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Registry |
                 Current_Tag_Fs.Tag_Id.Comment |
                 Current_Tag_Fs.Tag_Id.Vendor_Ids |
                 Current_Tag_Fs.Tag_Id.Tags |
                 Current_Tag_Fs.Tag_Id.Types |
                 Current_Tag_Fs.Tag_Id.Name |
                 Current_Tag_Fs.Tag_Id.Nested_Type |
                 Current_Tag_Fs.Tag_Id.Validity |
                 Current_Tag_Fs.Tag_Id.Enum |
                 Current_Tag_Fs.Tag_Id.Commands |
                 Current_Tag_Fs.Tag_Id.Proto |
                 Current_Tag_Fs.Tag_Id.Implicit_External_Sync_Parameters |
                 Current_Tag_Fs.Tag_Id.External_Sync_Parameter |
                 Current_Tag_Fs.Tag_Id.Extensions =>
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
            end case;
         end;
      end Attribute;

      procedure End_Tag (Tag_Name    : String;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure End_Tag (Tag_Name    : String;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Parents_Including_Self : Aida.XML.Tag_Name_Vector_T;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self => Parents_Including_Self,
                                          Parents                => Parent_Tags,
                                          Tag_Name               => Tag_Name);

         if
           Current_Tag_To_Tags_Map_Type_Owner.Contains (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                        Key       => Parents_Including_Self)
         then
            Current_Tag_To_Tags_Map_Type_Owner.Delete (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                       Key       => Parents_Including_Self);
         else
            Initialize (Call_Result, "Parents_Including_Self_To_Current_Tag_Map did not contain expected key for end tag " & Tag_Name);
         end if;
      end End_Tag;

      procedure End_Tag (Tag_Name    : String;
                         Tag_Value   : String;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure End_Tag (Tag_Name    : String;
                         Tag_Value   : String;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Parents_Including_Self : Aida.XML.Tag_Name_Vector_T;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         declare
            Find_Tag_Call_Result : Find_Tag_Call_Result_T := Find_Tag (Parents_Including_Self);
         begin
            if Find_Tag_Call_Result.Exists then
               declare
                  Current_Tag_V : Current_Tag.T := Find_Tag_Call_Result.Current_Tag_V;
               begin

                  if
                    Current_Tag_To_Tags_Map_Type_Owner.Contains (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                 Key       => Parents_Including_Self)
                  then
                     Current_Tag_To_Tags_Map_Type_Owner.Delete (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                Key       => Parents_Including_Self);
                  else
                     Initialize (Call_Result, "Parents_Including_Self_To_Current_Tag_Map did not contain expected key for end tag " & Tag_Name);
                  end if;

                  case Current_Tag_V.Kind_Id is
                  when Current_Tag_Fs.Tag_Id.Comment =>
                     Current_Tag_V.Comment.Value := To_Unbounded_String (Tag_Value);
                  when Current_Tag_Fs.Tag_Id.Type_T =>
                     declare
                        XML_Text_V : not null Vk_XML2.XML_Text_Ptr := new Vk_XML2.XML_Text_T;
                        Child : Vk_XML2.Type_T.Child_T := (Kind_Id    => Child_XML_Text,
                                                           XML_Text_V => XML_Text_V);
                     begin
                        Set_Unbounded_String (XML_Text_V.all, Tag_Value);
                        Current_Tag_V.Type_V.Children.Append (Child);
                     end;
                  when Current_Tag_Fs.Tag_Id.Name =>
                     Current_Tag_V.Name_V.Value := To_Unbounded_String (Tag_Value);
                  when Current_Tag_Fs.Tag_Id.Nested_Type =>
                     Current_Tag_V.Nested_Type_V.Value := (Exists => True,
                                                           Value  => To_Unbounded_String (Tag_Value));
                  when Current_Tag_Fs.Tag_Id.Usage =>
                     declare
                        XML_Text_V : not null Vk_XML2.XML_Text_Ptr := new Vk_XML2.XML_Text_T;
                        Child : Vk_XML2.Usage.Child_T := (Kind_Id    => Child_XML_Text,
                                                          XML_Text_V => XML_Text_V);
                     begin
                        Set_Unbounded_String (XML_Text_V.all, Tag_Value);
                        Current_Tag_V.Usage_V.Children.Append (Child);
                     end;
                  when Current_Tag_Fs.Tag_Id.Enum =>
                     Current_Tag_V.Enum_V.Value := To_Unbounded_String (Tag_Value);
                  when Current_Tag_Fs.Tag_Id.External_Sync_Parameter =>
                     Current_Tag_V.External_Sync_Parameter_V.XML_Value := (Exists => True,
                                                                           Value  => To_Unbounded_String (Tag_Value));
                  when Current_Tag_Fs.Tag_Id.Registry |
                       Current_Tag_Fs.Tag_Id.Vendor_Ids |
                       Current_Tag_Fs.Tag_Id.Vendor_Id |
                       Current_Tag_Fs.Tag_Id.Tags |
                       Current_Tag_Fs.Tag_Id.Tag |
                       Current_Tag_Fs.Tag_Id.Types |
                       Current_Tag_Fs.Tag_Id.Member |
                       Current_Tag_Fs.Tag_Id.Validity |
                       Current_Tag_Fs.Tag_Id.Enums |
                       Current_Tag_Fs.Tag_Id.Enums_Enum |
                       Current_Tag_Fs.Tag_Id.Unused |
                       Current_Tag_Fs.Tag_Id.Commands |
                       Current_Tag_Fs.Tag_Id.Command |
                       Current_Tag_Fs.Tag_Id.Proto |
                       Current_Tag_Fs.Tag_Id.Param |
                       Current_Tag_Fs.Tag_Id.Implicit_External_Sync_Parameters |
                       Current_Tag_Fs.Tag_Id.Feature |
                       Current_Tag_Fs.Tag_Id.Require |
                       Current_Tag_Fs.Tag_Id.Require_Enum |
                       Current_Tag_Fs.Tag_Id.Require_Command |
                       Current_Tag_Fs.Tag_Id.Extensions |
                       Current_Tag_Fs.Tag_Id.Extension =>
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Current_Tag_V.Kind_Id'Img);
                  end case;
               end;
            else
               declare
                  Prev_Tag : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
               begin
                  if not Prev_Tag.Exists then
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Both Current_Tag and Prev_Tag was null for end tag '" & Tag_Name & "' and value '" & Tag_Value & "'");
                     return;
                  end if;

                  case Prev_Tag.Current_Tag_V.Kind_Id is
                  when others =>
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "'");
                  end case;
               end;
            end if;
         end;
      end End_Tag;

      procedure Text (Value       : String;
                      Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                      Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Searched_For : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
      begin
         if Length (Parent_Tags) > 0 then
            if not Searched_For.Exists then
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", " & To_String (Parent_Tags));
               return;
            end if;

            if (for all I in Value'Range => Value (I) = ' ' or Value (I) = Ada.Characters.Latin_1.CR or Value (I) = Ada.Characters.Latin_1.LF) then
               null;
            else
               declare
                  Current_Tag_V : Current_Tag.T := Searched_For.Current_Tag_V;
               begin
                  case Current_Tag_V.Kind_Id is
                  when Current_Tag_Fs.Tag_Id.Registry =>
                     declare
                        XML_Text_V : not null Vk_XML2.XML_Text_Ptr := new Vk_XML2.XML_Text_T;
                        Child : Vk_XML2.Registry.Child_T := (Kind_Id    => Child_XML_Text,
                                                             XML_Text_V => XML_Text_V);
                     begin
                        Set_Unbounded_String (XML_Text_V.all, Value);
                        Current_Tag_V.Registry.Children.Append (Child);
                     end;
                  when Current_Tag_Fs.Tag_Id.Type_T =>
                     declare
                        XML_Text_V : not null Vk_XML2.XML_Text_Ptr := new Vk_XML2.XML_Text_T;
                        Child : Vk_XML2.Type_T.Child_T := (Kind_Id    => Child_XML_Text,
                                                           XML_Text_V => XML_Text_V);
                     begin
                        Set_Unbounded_String (XML_Text_V.all, Value);
                        Current_Tag_V.Type_V.Children.Append (Child);
                     end;
                  when Current_Tag_Fs.Tag_Id.Member =>
                     declare
                        XML_Text_V : not null Vk_XML2.XML_Text_Ptr := new Vk_XML2.XML_Text_T;
                        Child : Vk_XML2.Member.Child_T := (Kind_Id    => Child_XML_Text,
                                                           XML_Text_V => XML_Text_V);
                     begin
                        Set_Unbounded_String (XML_Text_V.all, Value);
                        Current_Tag_V.Member_V.Children.Append (Child);
                     end;
                  when Current_Tag_Fs.Tag_Id.Param =>
                     declare
                        XML_Text_V : not null Vk_XML2.XML_Text_Ptr := new Vk_XML2.XML_Text_T;
                        Child : Vk_XML2.Param.Child_T := (Kind_Id    => Child_XML_Text,
                                                          XML_Text_V => XML_Text_V);
                     begin
                        Set_Unbounded_String (XML_Text_V.all, Value);
                        Current_Tag_V.Param_V.Children.Append (Child);
                     end;
                  when others =>
                     Aida.Text_IO.Put ("Text:");
                     Aida.Text_IO.Put_Line (Value);
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", does not have text, " & To_String (Parent_Tags));
                  end case;
               end;
            end if;
         end if;
      end Text;

      procedure Comment (Value       : String;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Searched_For : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
      begin
         if not Searched_For.Exists then
            Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", " & To_String (Parent_Tags));
            return;
         end if;

         declare
            Current_Tag_V : Current_Tag.T := Searched_For.Current_Tag_V;
         begin
            case Current_Tag_V.Kind_Id is
            when Current_Tag_Fs.Tag_Id.Registry =>
               declare
                  Comment : not null Vk_XML2.XML_Out_Commented_Message_Ptr := new Vk_XML2.XML_Out_Commented_Message_T;
                  Child : Vk_XML2.Registry.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                       Out_Commented_Message_V => Comment);
               begin
                  Set_Unbounded_String (Comment.all, Value);
                  Current_Tag_V.Registry.Children.Append (Child);
               end;
            when Current_Tag_Fs.Tag_Id.Types =>
               declare
                  Comment : not null Vk_XML2.XML_Out_Commented_Message_Ptr := new Vk_XML2.XML_Out_Commented_Message_T;
                  Child : Vk_XML2.Types.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                    Out_Commented_Message_V => Comment);
               begin
                  Set_Unbounded_String (Comment.all, Value);
                  Current_Tag_V.Types_V.Children.Append (Child);
               end;
            when Current_Tag_Fs.Tag_Id.Type_T =>
               declare
                  Comment : not null Vk_XML2.XML_Out_Commented_Message_Ptr := new Vk_XML2.XML_Out_Commented_Message_T;
                  Child : Vk_XML2.Type_T.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                     Out_Commented_Message_V => Comment);
               begin
                  Set_Unbounded_String (Comment.all, Value);
                  Current_Tag_V.Type_V.Children.Append (Child);
               end;
            when Current_Tag_Fs.Tag_Id.Enums =>
               declare
                  Comment : not null Vk_XML2.XML_Out_Commented_Message_Ptr := new Vk_XML2.XML_Out_Commented_Message_T;
                  Child : Vk_XML2.Enums.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                    Out_Commented_Message_V => Comment);
               begin
                  Set_Unbounded_String (Comment.all, Value);
                  Current_Tag_V.Enums_V.Children.Append (Child);
               end;
            when Current_Tag_Fs.Tag_Id.Require =>
               declare
                  Comment : not null Vk_XML2.XML_Out_Commented_Message_Ptr := new Vk_XML2.XML_Out_Commented_Message_T;
                  Child : Vk_XML2.Require.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                      Out_Commented_Message_V => Comment);
               begin
                  Set_Unbounded_String (Comment.all, Value);
                  Current_Tag_V.Require_V.Children.Append (Child);
               end;
            when Current_Tag_Fs.Tag_Id.Extensions =>
               declare
                  Comment : not null Vk_XML2.XML_Out_Commented_Message_Ptr := new Vk_XML2.XML_Out_Commented_Message_T;
                  Child : Vk_XML2.Extensions.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                         Out_Commented_Message_V => Comment);
               begin
                  Set_Unbounded_String (Comment.all, Value);
                  Current_Tag_V.Extensions_V.Children.Append (Child);
               end;
            when Current_Tag_Fs.Tag_Id.Comment |
                 Current_Tag_Fs.Tag_Id.Vendor_Ids |
                 Current_Tag_Fs.Tag_Id.Vendor_Id |
                 Current_Tag_Fs.Tag_Id.Tags |
                 Current_Tag_Fs.Tag_Id.Tag |
                 Current_Tag_Fs.Tag_Id.Name |
                 Current_Tag_Fs.Tag_Id.Nested_Type |
                 Current_Tag_Fs.Tag_Id.Member |
                 Current_Tag_Fs.Tag_Id.Validity |
                 Current_Tag_Fs.Tag_Id.Usage |
                 Current_Tag_Fs.Tag_Id.Enum |
                 Current_Tag_Fs.Tag_Id.Enums_Enum |
                 Current_Tag_Fs.Tag_Id.Unused |
                 Current_Tag_Fs.Tag_Id.Commands |
                 Current_Tag_Fs.Tag_Id.Command |
                 Current_Tag_Fs.Tag_Id.Proto |
                 Current_Tag_Fs.Tag_Id.Param |
                 Current_Tag_Fs.Tag_Id.Implicit_External_Sync_Parameters |
                 Current_Tag_Fs.Tag_Id.External_Sync_Parameter |
                 Current_Tag_Fs.Tag_Id.Feature |
                 Current_Tag_Fs.Tag_Id.Require_Enum |
                 Current_Tag_Fs.Tag_Id.Require_Command |
                 Current_Tag_Fs.Tag_Id.Extension =>
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", does not have out commented comments, " & To_String (Parent_Tags));
            end case;
         end;
         --        Aida.Text_IO.Put ("Comment:");
         --        Aida.Text_IO.Put_Line (Value);
      end Comment;

      procedure Parse_XML_File is new Aida.XML.Generic_Parse_XML_File (Start_Tag,
                                                                       Attribute,
                                                                       Text,
                                                                       Comment,
                                                                       End_Tag,
                                                                       End_Tag);
   begin
      --        My_Registry := Registry;
      Parse_XML_File (Contents,
                      Call_Result);
   end Parse;

end Vk_XML_Reader;
