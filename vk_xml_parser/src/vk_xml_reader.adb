with Ada.Text_IO;
with GNAT.Source_Info;
with Std_String;
with Ada.Strings.Hash;
with Ada.Exceptions;
with Ada.Containers.Formal_Hashed_Maps;
with Aida.Strings;
with Current_Tag;
with Current_Tag_Fs;
with Aida.XML;
with Aida.Generic_Subprogram_Call_Result;
with Aida.Containers;
with Aida.XML.Generic_Parse_XML_File;
with Vk;
with Aida.Text_IO;

with Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable;

pragma Elaborate_All (Aida.Generic_Subprogram_Call_Result);
pragma Elaborate_All (Aida.XML.Generic_Parse_XML_File);
pragma Elaborate_All (Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable);

package body Vk_XML_Reader with SPARK_Mode is

   My_Registry : Vk.Registry_Shared_Ptr.T;

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
   XML_Tag_Require_Command_Attribute_Name           : constant String := "name";
   XML_Tag_Extensions                               : constant String := "extensions";
   XML_Tag_Extension                                : constant String := "extension";
   XML_Tag_Extension_Attribute_Name                 : constant String := "name";
   XML_Tag_Extension_Attribute_Number               : constant String := "number";
   XML_Tag_Extension_Attribute_Supported            : constant String := "supported";
   XML_Tag_Extension_Attribute_Protect              : constant String := "protect";
   XML_Tag_Extension_Attribute_Author               : constant String := "author";
   XML_Tag_Extension_Attribute_Contact              : constant String := "contact";

   use all type Aida.XML.Tag_Name.T;
   use all type Aida.XML.Tag_Name_Vectors.Vector;
   use all type Aida.XML.Subprogram_Call_Result.T;
   use all type Aida.Containers.Count_Type;
   use all type Current_Tag.T;
   use all type Vk.Registry.Fs.Child_Kind_Id_T;
   use all type Vk.Registry_Shared_Ptr.T;
   use all type Vk.Vendor_Ids.Fs.Child_Kind_Id_T;
   use all type Vk.Vendor_Ids_Shared_Ptr.T;
   use all type Vk.Tags.Fs.Child_Kind_Id_T;
   use all type Vk.Tags_Shared_Ptr.T;
   use all type Vk.Types.Fs.Child_Kind_Id_T;
   use all type Vk.Types_Shared_Ptr.T;
   use all type Vk.Type_T.Fs.Child_Kind_Id_T;
   use all type Vk.Type_Shared_Ptr.T;
   use all type Vk.Member.Fs.Child_Kind_Id_T;
   use all type Vk.Member_Shared_Ptr.T;
   use all type Vk.Validity.Fs.Child_Kind_Id_T;
   use all type Vk.Validity_Shared_Ptr.T;
   use all type Vk.Usage.Fs.Child_Kind_Id_T;
   use all type Vk.Usage_Shared_Ptr.T;
   use all type Vk.Enums.Fs.Child_Kind_Id_T;
   use all type Vk.Enums.Fs.Type_Attribue_T;
   use all type Vk.Enums_Shared_Ptr.T;
   use all type Vk.Enums_Enum_Shared_Ptr.T;
   use all type Vk.Unused_Shared_Ptr.T;
   use all type Vk.Commands.Fs.Child_Kind_Id_T;
   use all type Vk.Commands_Shared_Ptr.T;
   use all type Vk.Command_Shared_Ptr.T;
   use all type Vk.Command.Fs.Child_Kind_Id_T;
   use all type Vk.Command.Fs.Queue_T;
   use all type Vk.Command.Fs.Render_Pass_T;
   use all type Vk.Command.Fs.Command_Buffer_Level_T;
   use all type Vk.Proto.Fs.Child_Kind_Id_T;
   use all type Vk.Proto_Shared_Ptr.T;
   use all type Vk.Param.Fs.Child_Kind_Id_T;
   use all type Vk.Param_Shared_Ptr.T;
   use all type Vk.Implicit_External_Sync_Parameters.Fs.Child_Kind_Id_T;
   use all type Vk.Implicit_External_Sync_Parameters_Shared_Ptr.T;
   use all type Vk.Feature.Fs.Child_Kind_Id_T;
   use all type Vk.Feature_Shared_Ptr.T;
   use all type Vk.Require_Shared_Ptr.T;
   use all type Vk.Require.Fs.Child_Kind_Id_T;
   use all type Vk.Require_Enum_Shared_Ptr.T;
   use all type Vk.Require_Command_Shared_Ptr.T;
   use all type Vk.Extension.Fs.Child_Kind_Id_T;
   use all type Vk.Extension.Fs.Supported_T;
   use all type Vk.Extension_Shared_Ptr.T;
   use all type Vk.Extensions.Fs.Child_Kind_Id_T;
   use all type Vk.Extensions_Shared_Ptr.T;

   use Current_Tag_Fs.Tag_Id;

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

   package Current_Tag_To_Tags_Map_Type_Owner is new Ada.Containers.Formal_Hashed_Maps (Key_Type        => Aida.XML.Tag_Name_Vector_T,
                                                                                        Element_Type    => Current_Tag.T,
                                                                                        Hash            => Hash,
                                                                                        Equivalent_Keys => Aida.XML.Tag_Name_Vectors."=",
                                                                                        "="             => Current_Tag."=");

   use type Current_Tag_To_Tags_Map_Type_Owner.Cursor;

   Parents_Including_Self_To_Current_Tag_Map : Current_Tag_To_Tags_Map_Type_Owner.Map (100, 12);

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
                  Current_Tag_V.Registry := My_Registry;

                  Current_Tag.Initialize (This => Current_Tag_V);

                  Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                             Key       => Parents_Including_Self,
                                                             New_Item  => Current_Tag_V);
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
                        Comment : Vk.Comment_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id => Child_Comment,
                                                           C       => Comment);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Comment);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Comment := Comment;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Vendor_Ids then
                     declare
                        Vendor_Ids_V : Vk.Vendor_Ids_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id      => Child_Vendor_Ids,
                                                           Vendor_Ids_V => Vendor_Ids_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Vendor_Ids);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Vendor_Ids_V := Vendor_Ids_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Tags then
                     declare
                        Tags_V : Vk.Tags_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id => Child_Tags,
                                                           Tags_V  => Tags_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Tags);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Tags_V := Tags_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Types then
                     declare
                        Types_V : Vk.Types_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id => Child_Types,
                                                           Types_V  => Types_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Types);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Types_V := Types_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enums then
                     declare
                        Enums_V : Vk.Enums_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id => Child_Enums,
                                                           Enums_V  => Enums_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Enums);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enums_V := Enums_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Commands then
                     declare
                        Commands_V : Vk.Commands_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id    => Child_Commands,
                                                           Commands_V => Commands_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Commands);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Commands_V := Commands_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Feature then
                     declare
                        Feature_V : Vk.Feature_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id   => Child_Feature,
                                                           Feature_V => Feature_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Feature);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Feature_V := Feature_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Extensions then
                     declare
                        Extensions_V : Vk.Extensions_Shared_Ptr.T;
                        Child : Vk.Registry.Fs.Child_T := (Kind_Id   => Child_Extensions,
                                                           Extensions_V => Extensions_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Extensions);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Extensions_V := Extensions_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Vk.Registry_Shared_Ptr.Append_Child (This  => Prev_Tag.Registry,
                                                             Child => Child);
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
                        Vendor_Id_V : Vk.Vendor_Id_Shared_Ptr.T;
                        Child : Vk.Vendor_Ids.Fs.Child_T := (Kind_Id     => Child_Vendor_Id,
                                                             Vendor_Id_V => Vendor_Id_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Vendor_Id);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Vendor_Id_V := Vendor_Id_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Vendor_Ids_V,
                                      Child => Child);

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
                        Tag_V : Vk.Tag_Shared_Ptr.T;
                        Child : Vk.Tags.Fs.Child_T := (Kind_Id => Child_Tag,
                                                       Tag_V   => Tag_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Tag);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Tag_V := Tag_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Tags_V,
                                      Child => Child);

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
                        Type_V : Vk.Type_Shared_Ptr.T;
                        Child : Vk.Types.Fs.Child_T := (Kind_Id => Child_Type,
                                                        Type_V  => Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Type_T);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Type_V := Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Types_V,
                                      Child => Child);

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
                        Name_V : Vk.Name_Shared_Ptr.T;
                        Child : Vk.Type_T.Fs.Child_T := (Kind_Id => Child_Name,
                                                         Name_V  => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Type_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : Vk.Nested_Type_Shared_Ptr.T;
                        Child : Vk.Type_T.Fs.Child_T := (Kind_Id       => Child_Nested_Type,
                                                         Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Type_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Member then
                     declare
                        Member_V : Vk.Member_Shared_Ptr.T;
                        Child : Vk.Type_T.Fs.Child_T := (Kind_Id  => Child_Member,
                                                         Member_V => Member_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Member);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Member_V := Member_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Type_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Validity then
                     declare
                        Validity_V : Vk.Validity_Shared_Ptr.T;
                        Child : Vk.Type_T.Fs.Child_T := (Kind_Id    => Child_Validity,
                                                         Validity_V => Validity_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Validity);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Validity_V := Validity_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Type_V,
                                      Child => Child);

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
                        Nested_Type_V : Vk.Nested_Type_Shared_Ptr.T;
                        Child : Vk.Member.Fs.Child_T := (Kind_Id       => Child_Nested_Type,
                                                         Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Member_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : Vk.Name_Shared_Ptr.T;
                        Child : Vk.Member.Fs.Child_T := (Kind_Id => Child_Name,
                                                         Name_V  => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Member_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enum then
                     declare
                        Enum_V : Vk.Enum_Shared_Ptr.T;
                        Child : Vk.Member.Fs.Child_T := (Kind_Id => Child_Enum,
                                                         Enum_V  => Enum_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enum_V := Enum_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Member_V,
                                      Child => Child);

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
                        Usage_V : Vk.Usage_Shared_Ptr.T;
                        Child : Vk.Validity.Fs.Child_T := (Kind_Id => Child_Usage,
                                                           Usage_V => Usage_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Usage);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Usage_V := Usage_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Validity_V,
                                      Child => Child);

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
                        Enums_Enum_V : Vk.Enums_Enum_Shared_Ptr.T;
                        Child : Vk.Enums.Fs.Child_T := (Kind_Id      => Child_Enums_Enum,
                                                        Enums_Enum_V => Enums_Enum_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Enums_Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enums_Enum_V := Enums_Enum_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Enums_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Unused then
                     declare
                        Unused_V : Vk.Unused_Shared_Ptr.T;
                        Child : Vk.Enums.Fs.Child_T := (Kind_Id      => Child_Unused,
                                                        Unused_V => Unused_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Unused);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Unused_V := Unused_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Enums_V,
                                      Child => Child);

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
                        Command_V : Vk.Command_Shared_Ptr.T;
                        Child : Vk.Commands.Fs.Child_T := (Kind_Id   => Child_Command,
                                                           Command_V => Command_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Command);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Command_V := Command_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Commands_V,
                                      Child => Child);

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
                        Proto_V : Vk.Proto_Shared_Ptr.T;
                        Child : Vk.Command.Fs.Child_T := (Kind_Id => Child_Proto,
                                                          Proto_V => Proto_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Proto);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Proto_V := Proto_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Command_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Param then
                     declare
                        Param_V : Vk.Param_Shared_Ptr.T;
                        Child : Vk.Command.Fs.Child_T := (Kind_Id => Child_Param,
                                                          Param_V => Param_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Param);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Param_V := Param_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Command_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Validity then
                     declare
                        Validity_V : Vk.Validity_Shared_Ptr.T;
                        Child : Vk.Command.Fs.Child_T := (Kind_Id => Child_Validity,
                                                          Validity_V => Validity_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Validity);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Validity_V := Validity_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Command_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Implicit_External_Syns_Params then
                     declare
                        Parameters_V : Vk.Implicit_External_Sync_Parameters_Shared_Ptr.T;
                        Child : Vk.Command.Fs.Child_T := (Kind_Id      => Child_Implicit_External_Sync_Parameters,
                                                          Parameters_V => Parameters_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Implicit_External_Sync_Parameters);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Parameters_V := Parameters_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Command_V,
                                      Child => Child);

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
                        Nested_Type_V : Vk.Nested_Type_Shared_Ptr.T;
                        Child : Vk.Proto.Fs.Child_T := (Kind_Id       => Child_Nested_Type,
                                                        Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Proto_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : Vk.Name_Shared_Ptr.T;
                        Child : Vk.Proto.Fs.Child_T := (Kind_Id       => Child_Name,
                                                        Name_V => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Proto_V,
                                      Child => Child);

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
                        Nested_Type_V : Vk.Nested_Type_Shared_Ptr.T;
                        Child : Vk.Param.Fs.Child_T := (Kind_Id       => Child_Nested_Type,
                                                        Nested_Type_V => Nested_Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Param_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : Vk.Name_Shared_Ptr.T;
                        Child : Vk.Param.Fs.Child_T := (Kind_Id       => Child_Name,
                                                        Name_V => Name_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Param_V,
                                      Child => Child);

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
                        External_Sync_Parameter_V : Vk.External_Sync_Parameter_Shared_Ptr.T;
                        Child : Vk.Implicit_External_Sync_Parameters.Fs.Child_T := (Kind_Id                   => Child_External_Sync_Parameter,
                                                                                    External_Sync_Parameter_V => External_Sync_Parameter_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.External_Sync_Parameter);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.External_Sync_Parameter_V := External_Sync_Parameter_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Parameters_V,
                                      Child => Child);

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
                        Require_V : Vk.Require_Shared_Ptr.T;
                        Child : Vk.Feature.Fs.Child_T := (Kind_Id   => Child_Require,
                                                          Require_V => Require_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_V := Require_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Feature_V,
                                      Child => Child);

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
                        Type_V : Vk.Type_Shared_Ptr.T;
                        Child : Vk.Require.Fs.Child_T := (Kind_Id => Child_Type,
                                                          Type_V  => Type_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Type_T);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Type_V := Type_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Require_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enum then
                     declare
                        Enum_V : Vk.Require_Enum_Shared_Ptr.T;
                        Child : Vk.Require.Fs.Child_T := (Kind_Id => Child_Enum,
                                                          Enum_V  => Enum_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require_Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_Enum_V := Enum_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Require_V,
                                      Child => Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Command then
                     declare
                        Command_V : Vk.Require_Command_Shared_Ptr.T;
                        Child : Vk.Require.Fs.Child_T := (Kind_Id => Child_Command,
                                                          Command_V  => Command_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require_Command);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_Command_V := Command_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Require_V,
                                      Child => Child);

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
                        Extension_V : Vk.Extension_Shared_Ptr.T;
                        Child : Vk.Extensions.Fs.Child_T := (Kind_Id     => Child_Extension,
                                                             Extension_V => Extension_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Extension);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Extension_V := Extension_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Extensions_V,
                                      Child => Child);

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
                        Require_V : Vk.Require_Shared_Ptr.T;
                        Child : Vk.Extension.Fs.Child_T := (Kind_Id   => Child_Require,
                                                            Require_V => Require_V);

                        Temp_Tag : Current_Tag.T (Current_Tag_Fs.Tag_Id.Require);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_V := Require_V;

                        Current_Tag.Initialize (Temp_Tag);

                        Append_Child (This  => Prev_Tag.Extension_V,
                                      Child => Child);

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
                  Vk.Vendor_Id_Shared_Ptr.Set_Name (This => Current_Tag_V.Vendor_Id_V,
                                                    Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Vendor_Id_Attribute_Id then
                  Vk.Vendor_Id_Shared_Ptr.Set_Id (This => Current_Tag_V.Vendor_Id_V,
                                                  Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Vendor_Id_Attribute_Comment then
                  Vk.Vendor_Id_Shared_Ptr.Set_Comment (This => Current_Tag_V.Vendor_Id_V,
                                                       Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Tag =>
               if Attribute_Name = XML_Tag_Tag_Attribute_Name then
                  Vk.Tag_Shared_Ptr.Set_Name (This => Current_Tag_V.Tag_V,
                                              Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Tag_Attribute_Author then
                  Vk.Tag_Shared_Ptr.Set_Author (This => Current_Tag_V.Tag_V,
                                                Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Tag_Attribute_Contact then
                  Vk.Tag_Shared_Ptr.Set_Contact (This => Current_Tag_V.Tag_V,
                                                 Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Type_T =>
               if Attribute_Name = XML_Tag_Type_Attribute_Name then
                  Set_Name (This => Current_Tag_V.Type_V,
                            Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Category then
                  Set_Category (This => Current_Tag_V.Type_V,
                                Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Requires then
                  Set_Requires (This => Current_Tag_V.Type_V,
                                Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Parent then
                  Set_Parent (This => Current_Tag_V.Type_V,
                              Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Returned_Only then
                  if Attribute_Value = "true" then
                     Set_Returned_Only (This => Current_Tag_V.Type_V,
                                        Value => True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Type_Attribute_Comment then
                  Set_Comment (This => Current_Tag_V.Type_V,
                               Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Member =>
               if Attribute_Name = XML_Tag_Member_Attribute_Optional then
                  if Attribute_Value = "true" then
                     Vk.Member_Shared_Ptr.Set_Optional (This  => Current_Tag_V.Member_V,
                                                        Value => True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Member_Attribute_Len then
                  Set_Len (This => Current_Tag_V.Member_V,
                           Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Member_Attribute_No_Auto_Validity then
                  if Attribute_Value = "true" then
                     Set_No_Auto_Validity (This  => Current_Tag_V.Member_V,
                                           Value => True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Member_Attribute_Valid_Extension_Structs then
                  Set_Valid_Extension_Structs (This => Current_Tag_V.Member_V,
                                               Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Enums =>
               if Attribute_Name = XML_Tag_Enums_Attribute_Name then
                  Set_Name (This => Current_Tag_V.Enums_V,
                            Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Enums_Attribute_Comment then
                  Set_Comment (This => Current_Tag_V.Enums_V,
                               Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Enums_Attribute_Type then
                  if Attribute_Value = "enum" then
                     Set_Type_Attribue (This  => Current_Tag_V.Enums_V,
                                        Value => Enum);
                  elsif Attribute_Value = "bitmask" then
                     Set_Type_Attribue (This  => Current_Tag_V.Enums_V,
                                        Value => Bit_Mask);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Enums_Enum =>
               if Attribute_Name = XML_Tag_Enums_Enum_Attribute_Value then
                  Set_Value (This => Current_Tag_V.Enums_Enum_V,
                             Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Name then
                  Set_Name (This => Current_Tag_V.Enums_Enum_V,
                            Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Comment then
                  Set_Comment (This => Current_Tag_V.Enums_Enum_V,
                               Text => Attribute_Value);
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
                        Set_Bit_Position (This  => Current_Tag_V.Enums_Enum_V,
                                          Value => Vk.Enums_Enum.Fs.Bit_Position_T (V));
                     end if;
                  end;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Unused =>
               if Attribute_Name = XML_Tag_Unused_Attribute_Start then
                  Set_Start (This => Current_Tag_V.Unused_V,
                             Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Command =>
               if Attribute_Name = XML_Tag_Command_Attribute_Success_Codes then
                  Append_Success_Code (This => Current_Tag_V.Command_V, -- TODO: Split the String into several success codes later
                                       Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Command_Attribute_Error_Codes then
                  Append_Error_Code (This => Current_Tag_V.Command_V, -- TODO: Split the String into several error codes later
                                     Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Command_Attribute_Queues then
                  if Attribute_Value = "sparse_binding" then
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Sparse_Binding);
                  elsif Attribute_Value = "graphics,compute" then
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Graphics);
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Compute);
                  elsif Attribute_Value = "graphics" then
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Graphics);
                  elsif Attribute_Value = "compute" then
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Compute);
                  elsif Attribute_Value = "transfer,graphics,compute" then
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Graphics);
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Compute);
                     Append_Queue (This  => Current_Tag_V.Command_V,
                                   Queue => Transfer);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Command_Attribute_Render_Pass then
                  if Attribute_Value = "inside" then
                     Append_Render_Pass (This  => Current_Tag_V.Command_V,
                                         Value => Inside);
                  elsif Attribute_Value = "outside" then
                     Append_Render_Pass (This  => Current_Tag_V.Command_V,
                                         Value => Outside);
                  elsif Attribute_Value = "both" then
                     Append_Render_Pass (This  => Current_Tag_V.Command_V,
                                         Value => Both);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Command_Attribute_Cmd_Buffer_Level then
                  if Attribute_Value = "primary,secondary" then
                     Append_Command_Buffer_Level (This  => Current_Tag_V.Command_V,
                                                  Value => Primary);
                     Append_Command_Buffer_Level (This  => Current_Tag_V.Command_V,
                                                  Value => Secondary);
                  elsif Attribute_Value = "primary" then
                     Append_Command_Buffer_Level (This  => Current_Tag_V.Command_V,
                                                  Value => Primary);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Param =>
               if Attribute_Name = XML_Tag_Param_Attribute_Optional then
                  if Attribute_Value = "true" or Attribute_Value = "false,true" then
                     Set_Optional (This  => Current_Tag_V.Param_V,
                                   Value => True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Param_Attribute_External_Sync then
                  Set_External_Sync (This => Current_Tag_V.Param_V,
                                     Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Param_Attribute_Len then
                  Set_Len (This => Current_Tag_V.Param_V,
                           Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Param_Attribute_No_Auto_Validity then
                  if Attribute_Value = "true" then
                     Set_No_Auto_Validity (This  => Current_Tag_V.Param_V,
                                           Value => True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Feature =>
               if Attribute_Name = XML_Tag_Feature_Attribute_API then
                  Set_API (This => Current_Tag_V.Feature_V,
                           Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Feature_Attribute_Name then
                  Set_Name (This => Current_Tag_V.Feature_V,
                            Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Feature_Attribute_Number then
                  Set_Number (This => Current_Tag_V.Feature_V,
                              Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Require =>
               if Attribute_Name = XML_Tag_Require_Attribute_Comment then
                  Set_Comment (This => Current_Tag_V.Require_V,
                               Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Require_Enum =>
               if Attribute_Name = XML_Tag_Require_Enum_Attribute_Name then
                  Set_Name (This => Current_Tag_V.Require_Enum_V,
                            Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Value then
                  Set_Value (This => Current_Tag_V.Require_Enum_V,
                             Text => Attribute_Value);
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
                        Set_Offset (This => Current_Tag_V.Require_Enum_V,
                                    Value => Vk.Require_Enum.Fs.Offset_T (V));
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Dir then
                  Set_Dir (This => Current_Tag_V.Require_Enum_V,
                           Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Extends then
                  Set_Extends (This => Current_Tag_V.Require_Enum_V,
                               Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Comment then
                  Set_Comment (This => Current_Tag_V.Require_Enum_V,
                               Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Require_Command =>
               if Attribute_Name = XML_Tag_Require_Command_Attribute_Name then
                  Set_Name (This => Current_Tag_V.Require_Command_V,
                            Text => Attribute_Value);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Current_Tag_Fs.Tag_Id.Extension =>
               if Attribute_Name = XML_Tag_Extension_Attribute_Name then
                  Set_Name (This => Current_Tag_V.Extension_V,
                            Text => Attribute_Value);
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
                        Set_Number (This  => Current_Tag_V.Extension_V,
                                    Value => Vk.Extension.Fs.Number_T (V));
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Supported then
                  if Attribute_Value = "vulkan" then
                     Set_Supported (This  => Current_Tag_V.Extension_V,
                                    Value => Vulkan);
                  elsif Attribute_Value = "disabled" then
                     Set_Supported (This  => Current_Tag_V.Extension_V,
                                    Value => Disabled);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Protect then
                  Set_Protect (This => Current_Tag_V.Extension_V,
                               Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Author then
                  Set_Author (This => Current_Tag_V.Extension_V,
                              Text => Attribute_Value);
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Contact then
                  Set_Contact (This => Current_Tag_V.Extension_V,
                               Text => Attribute_Value);
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
                 Current_Tag_Fs.Tag_Id.Usage |
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
                     Vk.Comment_Shared_Ptr.Set_Value (This => Current_Tag_V.Comment,
                                                      Text => Tag_Value);
                  when Current_Tag_Fs.Tag_Id.Type_T =>
                     declare
                        XML_Text_V : Vk.XML_Text.T;
                        Child : Vk.Type_T.Fs.Child_T := (Kind_Id    => Child_XML_Text,
                                                        XML_Text_V => XML_Text_V);
                     begin
                        Append_Child (This  => Current_Tag_V.Type_V,
                                      Child => Child);
                     end;
                  when Current_Tag_Fs.Tag_Id.Name =>
                     Vk.Name_Shared_Ptr.Set_Value (This => Current_Tag_V.Name_V,
                                                   Text => Tag_Value);
                  when Current_Tag_Fs.Tag_Id.Nested_Type =>
                     Vk.Nested_Type_Shared_Ptr.Set_Value (This => Current_Tag_V.Nested_Type_V,
                                                          Text => Tag_Value);
                  when Current_Tag_Fs.Tag_Id.Usage =>
                     declare
                        XML_Text_V : Vk.XML_Text.T;
                        Child : Vk.Usage.Fs.Child_T := (Kind_Id    => Child_XML_Text,
                                                        XML_Text_V => XML_Text_V);
                     begin
                        Append_Child (This  => Current_Tag_V.Usage_V,
                                      Child => Child);
                     end;
                  when Current_Tag_Fs.Tag_Id.Enum =>
                     Vk.Enum_Shared_Ptr.Set_Value (This => Current_Tag_V.Enum_V,
                                                   Text => Tag_Value);
                  when Current_Tag_Fs.Tag_Id.External_Sync_Parameter =>
                     Vk.External_Sync_Parameter_Shared_Ptr.Set_XML_Value (This => Current_Tag_V.External_Sync_Parameter_V,
                                                                          Text => Tag_Value);
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
                   Call_Result : in out Aida.XML.Subprogram_Call_Result.T) is
   begin
        Aida.Text_IO.Put ("Text:");
        Aida.Text_IO.Put_Line (Value);
      null;
   end Text;

   package Mutable_XML_Out_Commented_Message_Shared_Ptr is new Vk.XML_Out_Commented_Message_Shared_Ptr.Mutable;

   use all type Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;

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
                  Comment : Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;
                  Child : Vk.Registry.Fs.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                     Out_Commented_Message_V => Vk.XML_Out_Commented_Message_Shared_Ptr.T (Comment));
               begin
                  Mutable_XML_Out_Commented_Message_Shared_Ptr.Initialize (This => Comment,
                                                                         Text => Value);
                  Append_Child (This  => Current_Tag_V.Registry,
                                Child => Child);
               end;
            when Current_Tag_Fs.Tag_Id.Types =>
               declare
                  Comment : Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;
                  Child : Vk.Types.Fs.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                  Out_Commented_Message_V => Vk.XML_Out_Commented_Message_Shared_Ptr.T (Comment));
               begin
                  Mutable_XML_Out_Commented_Message_Shared_Ptr.Initialize (This => Comment,
                                                                           Text => Value);
                  Append_Child (This  => Current_Tag_V.Types_V,
                                Child => Child);
               end;
            when Current_Tag_Fs.Tag_Id.Type_T =>
               declare
                  Comment : Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;
                  Child : Vk.Type_T.Fs.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                   Out_Commented_Message_V => Vk.XML_Out_Commented_Message_Shared_Ptr.T (Comment));
               begin
                  Mutable_XML_Out_Commented_Message_Shared_Ptr.Initialize (This => Comment,
                                                                           Text => Value);
                  Append_Child (This  => Current_Tag_V.Type_V,
                                Child => Child);
               end;
            when Current_Tag_Fs.Tag_Id.Enums =>
               declare
                  Comment : Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;
                  Child : Vk.Enums.Fs.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                  Out_Commented_Message_V => Vk.XML_Out_Commented_Message_Shared_Ptr.T (Comment));
               begin
                  Mutable_XML_Out_Commented_Message_Shared_Ptr.Initialize (This => Comment,
                                                                           Text => Value);
                  Append_Child (This  => Current_Tag_V.Enums_V,
                                Child => Child);
               end;
            when Current_Tag_Fs.Tag_Id.Require =>
               declare
                  Comment : Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;
                  Child : Vk.Require.Fs.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                    Out_Commented_Message_V => Vk.XML_Out_Commented_Message_Shared_Ptr.T (Comment));
               begin
                  Mutable_XML_Out_Commented_Message_Shared_Ptr.Initialize (This => Comment,
                                                                           Text => Value);
                  Append_Child (This  => Current_Tag_V.Require_V,
                                Child => Child);
               end;
            when Current_Tag_Fs.Tag_Id.Extensions =>
               declare
                  Comment : Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;
                  Child : Vk.Extensions.Fs.Child_T := (Kind_Id                 => Child_Out_Commented_Message,
                                                       Out_Commented_Message_V => Vk.XML_Out_Commented_Message_Shared_Ptr.T (Comment));
               begin
                  Mutable_XML_Out_Commented_Message_Shared_Ptr.Initialize (This => Comment,
                                                                           Text => Value);
                  Append_Child (This  => Current_Tag_V.Extensions_V,
                                Child => Child);
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

   procedure Parse (Contents      : String;
                    Registry      : in out Vk.Registry_Shared_Ptr.T;
                    Call_Result   : in out Aida.XML.Subprogram_Call_Result.T) is
   begin
      My_Registry := Registry;
      Parse_XML_File (Contents,
                      Call_Result);
   end Parse;

end Vk_XML_Reader;
