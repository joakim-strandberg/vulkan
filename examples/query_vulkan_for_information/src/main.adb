with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces.C.Strings;
with Ada.Characters.Latin_1;
with Vk;
with System;

procedure Main is
   use type Vk.Result_T;
   use type Vk.Queue_Flag_Bits_T;
   use type Interfaces.Unsigned_32;

   Instance : aliased Vk.Instance_T;

   Result : Vk.Result_T;

   procedure Print_Queue_Family_Properties_To_Standard_Out (Queue_Family_Properties : Vk.Queue_Family_Properties_T) is

      function Is_Supported (Searched_For : Vk.Queue_Flag_Bits_T) return Boolean is
      begin
         return (Vk.Queue_Flag_Bits_T (Queue_Family_Properties.Queue_Flags) and Searched_For) = Searched_For;
      end Is_Supported;

   begin
      Ada.Text_IO.Put_Line ("Supported kinds of computations: ");
      if Is_Supported (Vk.QUEUE_GRAPHICS_BIT) then
         Ada.Text_IO.Put_Line ("Graphics");
      end if;

      if Is_Supported (Vk.QUEUE_COMPUTE_BIT) then
         Ada.Text_IO.Put_Line ("Compute");
      end if;

      if Is_Supported (Vk.QUEUE_TRANSFER_BIT) then
         Ada.Text_IO.Put_Line ("Transfer");
      end if;

      if Is_Supported (Vk.QUEUE_SPARSE_BINDING_BIT) then
         Ada.Text_IO.Put_Line ("Sparse binding");
      end if;

      Ada.Text_IO.Put_Line ("");
   end Print_Queue_Family_Properties_To_Standard_Out;

   procedure Query_Device_Queues (Physical_Device : Vk.Physical_Device_T) is
      Queue_Count : aliased Interfaces.Unsigned_32;
      Queue_Family_Properties : aliased Vk.Queue_Family_Properties_Array_T;
   begin
      Vk.Get_Physical_Device_Queue_Family_Properties (Physical_Device             => Physical_Device,
                                                      Queue_Family_Property_Count => Queue_Count'Access,
                                                      Queue_Family_Properties     => Vk.Queue_Family_Properties_Array_Conversions.Null_Address);
      if Queue_Count > 0 then
         Ada.Text_IO.Put_Line ("Number of family queues:" & Queue_Count'Image);
         Vk.Get_Physical_Device_Queue_Family_Properties (Physical_Device             => Physical_Device,
                                                         Queue_Family_Property_Count => Queue_Count'Access,
                                                         Queue_Family_Properties     => Vk.Queue_Family_Properties_Array_Conversions.To_Address (Queue_Family_Properties'Unchecked_Access));

         for I in Interfaces.C.size_t range Queue_Family_Properties'First .. Interfaces.C.size_t (Queue_Count - 1) loop
            Print_Queue_Family_Properties_To_Standard_Out (Queue_Family_Properties (I));
         end loop;
      else
         Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", no queue families!!");
      end if;
   end Query_Device_Queues;

   procedure Print_Physical_Device_Properties_To_Standard_Out (Physical_Device : Vk.Physical_Device_T) is
      Physical_Device_Properties : aliased Vk.Physical_Device_Properties_T;
   begin
      Vk.Get_Physical_Device_Properties (Physical_Device => Physical_Device,
                                         Properties      => Physical_Device_Properties'Access);

      Ada.Text_IO.Put_Line ("API Major version: " & Physical_Device_Properties.Api_Version.Major'Img);
      Ada.Text_IO.Put_Line ("API Minor version: " & Physical_Device_Properties.Api_Version.Minor'Img);
      Ada.Text_IO.Put_Line ("API Patch version: " & Physical_Device_Properties.Api_Version.Patch'Img);
      Ada.Text_IO.Put_Line ("Driver version: " & Physical_Device_Properties.Driver_Version'Img);
      Ada.Text_IO.Put_Line ("Vendor id: " & Physical_Device_Properties.Vendor_Id'Img);
      Ada.Text_IO.Put_Line ("Device id: " & Physical_Device_Properties.Device_Id'Img);

      for J in Vk.Device_Name_Array_Index_T range Physical_Device_Properties.Device_Name'First..Physical_Device_Properties.Device_Name'Last loop
         if Character (Physical_Device_Properties.Device_Name (J)) /= Ada.Characters.Latin_1.NUL then
            Ada.Text_IO.Put (Character (Physical_Device_Properties.Device_Name (J)));
         else
            exit;
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("");
   end Print_Physical_Device_Properties_To_Standard_Out;

   procedure Query_Physical_Devices_Properties (Number_Of_Devices : aliased in out Interfaces.Unsigned_32) is
      Physical_Devices : aliased Vk.Physical_Devices_Array_T;
   begin
      Result := Vk.Enumerate_Physical_Devices (Instance              => Instance,
                                               Physical_Device_Count => Number_Of_Devices'Access,
                                               Physical_Devices      => Vk.Physical_Devices_Array_Conversions.To_Address (Physical_Devices'Unchecked_Access));

      if Result = Vk.SUCCESS then
         for I in Interfaces.C.size_t range Physical_Devices'First..Interfaces.C.size_t(Number_Of_Devices-1) loop
            Print_Physical_Device_Properties_To_Standard_Out (Physical_Devices (I));
         end loop;

         if Number_Of_Devices = 1 then
            Query_Device_Queues (Physical_Devices (Physical_Devices'First));
         end if;
      else
         Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Call to Vk.Enumerate_Physical_Devices returned " & Result'Img);
      end if;
   end Query_Physical_Devices_Properties;

   procedure Query_Number_Of_Physical_Devices is
      Number_Of_Devices : aliased Interfaces.Unsigned_32;
   begin
      Result := Vk.Enumerate_Physical_Devices (Instance              => Instance,
                                               Physical_Device_Count => Number_Of_Devices'Access,
                                               Physical_Devices      => Vk.Physical_Devices_Array_Conversions.Null_Address);

      if Result = Vk.SUCCESS then
         Ada.Text_IO.Put_Line ("Number of physical devices:" & Number_Of_Devices'Img);
         Query_Physical_Devices_Properties (Number_Of_Devices);
      else
         Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Call to Vk.Enumerate_Physical_Devices returned " & Result'Img);
      end if;
   end Query_Number_Of_Physical_Devices;

   Info : aliased Vk.Instance_Create_Info_T;

   Application_Info : aliased Vk.Application_Info_T;

   Application_Name : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("Query Vulkan Application");
   Engine_Name      : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("Put engine name here");

begin
   Application_Info.Stype               := Vk.STRUCTURE_TYPE_APPLICATION_INFO;
   Application_Info.Next                := System.Null_Address;
   Application_Info.Application_Name    := Application_Name;
   Application_Info.Application_Version := 16#010000#;
   Application_Info.Engine_Name         := Engine_Name;
   Application_Info.Engine_Version      := 16#010000#;
   Application_Info.Api_Version         := Vk.API_Version;

   Info.Stype                   := Vk.STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
   Info.Next                    := System.Null_Address;
   Info.Flags                   := 0;
   Info.Application_Info        := Application_Info'Unchecked_Access;
   Info.Enabled_Layer_Count     := 0;
   Info.Enabled_Layer_Names     := Vk.Char_Ptr_Array_Conversions.Null_Address;
   Info.Enabled_Extension_Count := 0;
   Info.Enabled_Extension_Names := Vk.Char_Ptr_Array_Conversions.Null_Address;

   Result := Vk.Create_Instance (Create_Info => Info'Access,
                                 Allocator   => null,
                                 Instance    => Instance'Access);

   if Result = Vk.SUCCESS then
      Query_Number_Of_Physical_Devices;
   else
      Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Call to Vk.Create_Instance returned " & Result'Img);
   end if;
end Main;
