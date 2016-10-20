with Ada.Text_IO;
with Interfaces.C.Strings;
with Vk;
with System;

procedure Main is
   Instance : aliased Vk.Instance_T;

   Result : Vk.Result_T;

   Application_Info : aliased Vk.Application_Info_T;

   Application_Name : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("Query Vulkan Application");
   Engine_Name      : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("Put engine name here");

   Info : aliased Vk.Instance_Create_Info_T;
begin
   Application_Info.Stype               := Vk.STRUCTURE_TYPE_APPLICATION_INFO;
   Application_Info.Pnext               := System.Null_Address;
   Application_Info.Papplication_Name   := Application_Name;
   Application_Info.Application_Version := 16#010000#;
   Application_Info.Pengine_Name        := Engine_Name;
   Application_Info.Engine_Version      := 16#010000#;
   Application_Info.Api_Version         := Vk.API_Version;

   Info.Stype                      := Vk.STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
   Info.Pnext                      := System.Null_Address;
   Info.Flags                      := 0;
   Info.Papplication_Info          := Application_Info'Unchecked_Access;
   Info.Enabled_Layer_Count        := 0;
   Info.Pp_Enabled_Layer_Names     := Vk.Char_Ptr_Array_Conversions.Null_Address;
   Info.Enabled_Extension_Count    := 0;
   Info.Pp_Enabled_Extension_Names := Vk.Char_Ptr_Array_Conversions.Null_Address;

   Result := Vk.Create_Instance (Pcreate_Info => Info'Unchecked_Access,
                                 Pallocator   => null,
                                 Pinstance    => Instance'Unchecked_Access);

   Ada.Text_IO.Put_Line (Result'Img);
end Main;
