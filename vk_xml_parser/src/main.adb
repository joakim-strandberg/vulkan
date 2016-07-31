with Ada.Directories;
with Ada.Direct_IO;
with Aida.Text_IO;
with Aida.XML;
with GNAT.IO_Aux;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Vk_Package_Creator;
with Vk_XML_Reader;
with Vk;

use all type Aida.XML.Subprogram_Call_Result.T;

procedure Main is
     File_Name : String  := "vk.xml";

   procedure Main_Internal is
      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;

      Registry : Vk.Registry_Shared_Ptr.T;

      Call_Result : Aida.XML.Subprogram_Call_Result.T;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => File_Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      Vk_XML_Reader.Parse (Contents    => Contents,
                           Registry    => Registry,
                           Call_Result => Call_Result);

      Vk_Package_Creator.Create_Vk_Package (Registry);

      if not Has_Failed (Call_Result) then
         Aida.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create vk.ads");
         --Vk_Package_Creator.Create_Vk_Package (Registry);
      else
         Aida.Text_IO.Put_Line (Message (Call_Result));
      end if;
   end Main_Internal;

begin
   if not GNAT.IO_Aux.File_Exists (File_Name) then
      Aida.Text_IO.Put_Line ("Could not find file!");
      return;
   end if;

   Main_Internal;
end Main;
