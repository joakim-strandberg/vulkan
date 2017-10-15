with Ada.Directories;
with Ada.Direct_IO;
with Aida.Text_IO;
with Aida.XML;
with GNAT.IO_Aux;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with Vk_XML_Reader;
with Vk_XML.Registry_Tag;
with Vk_Package_Creator;

with Ada.Text_IO;
with Dynamic_Pools;

procedure Main is

   use all type Aida.XML.Subprogram_Call_Result.T;

   File_Name : String  := "vk.xml";

   procedure Main_Internal is

      Subpool : Dynamic_Pools.Scoped_Subpool := Dynamic_Pools.Create_Subpool (Vk_XML.Main_Pool, 1_000_000);

      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;

      Registry : not null Vk_XML.Registry_Tag.Ptr := new (Subpool.Handle) Vk_XML.Registry_Tag.T;

      Call_Result : Aida.XML.Subprogram_Call_Result.T;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => File_Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      Vk_XML_Reader.Parse (Contents    => Aida.String_T (Contents),
                           Registry    => Registry,
                           SH          => Subpool.Handle,
                           Call_Result => Call_Result);

      if not Has_Failed (Call_Result) then
         Aida.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create vk.ads");
         Vk_Package_Creator.Create_Vk_Package (Registry.all);
      else
         Aida.Text_IO.Put_Line (Message (Call_Result));
      end if;

      Ada.Text_IO.Put ("Allocated memory in default subpool plus subpool:");
      Ada.Text_IO.Put_Line (Dynamic_Pools.Storage_Size (Vk_XML.Main_Pool)'Image);
      Ada.Text_IO.Put ("Used memory in default subpool plus subpool:");
      Ada.Text_IO.Put_Line (Dynamic_Pools.Storage_Used (Vk_XML.Main_Pool)'Image);

      Ada.Text_IO.Put ("Allocated memory in subpool:");
      Ada.Text_IO.Put_Line (Dynamic_Pools.Storage_Size (Subpool.Handle)'Image);
      Ada.Text_IO.Put ("Used memory in subpool:");
      Ada.Text_IO.Put_Line (Dynamic_Pools.Storage_Used (Subpool.Handle)'Image);
   end Main_Internal;

begin
   if not GNAT.IO_Aux.File_Exists (File_Name) then
      Aida.Text_IO.Put_Line ("Could not find file!");
      return;
   end if;

   Main_Internal;
end Main;
