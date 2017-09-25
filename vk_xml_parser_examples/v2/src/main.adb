with Ada.Directories;
with Ada.Direct_IO;
with Aida.Text_IO;
with Aida.XML;
with GNAT.IO_Aux;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Generic_Vk_Package_Creator;
with Generic_Vk_XML_Reader;
with Generic_Vk_XML;

with Basic_Dynamic_Pools;

use all type Aida.XML.Subprogram_Call_Result.T;
with Ada.Text_IO;

procedure Main is
   File_Name : String  := "vk.xml";

   package Vk_XML is new Generic_Vk_XML;

   package Vk_XML_Reader is new Generic_Vk_XML_Reader (Vk_XML);

   package Vk_Package_Creator is new Generic_Vk_Package_Creator (Vk_XML);

   procedure Main_Internal is
      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;

      Registry : Vk_XML.Registry.Ptr := new Vk_XML.Registry.T;

      Call_Result : Aida.XML.Subprogram_Call_Result.T;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => File_Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      Vk_XML_Reader.Parse (Contents    => Contents,
                           Registry    => Registry,
                           Call_Result => Call_Result);

      if not Has_Failed (Call_Result) then
         Aida.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create vk.ads");
         Vk_Package_Creator.Create_Vk_Package (Registry);
      else
         Aida.Text_IO.Put_Line (Message (Call_Result));
      end if;

      Ada.Text_IO.Put ("Allocated memory in default subpool plus subpool:");
      Ada.Text_IO.Put_Line (Basic_Dynamic_Pools.Storage_Size (Vk_XML.Main_Pool)'Img);
      Ada.Text_IO.Put ("Used memory in default subpool plus subpool:     ");
      Ada.Text_IO.Put_Line (Basic_Dynamic_Pools.Storage_Used (Vk_XML.Main_Pool)'Img);
   end Main_Internal;

begin
   if not GNAT.IO_Aux.File_Exists (File_Name) then
      Aida.Text_IO.Put_Line ("Could not find file!");
      return;
   end if;

   Main_Internal;
end Main;
