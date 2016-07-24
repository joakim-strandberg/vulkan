with Aida.XML.Generic_Parse_XML_File;

pragma Elaborate_All (Aida.XML.Generic_Parse_XML_File);

with Aida.Text_IO;

package body Ex_Reader with SPARK_Mode is

   procedure Start_Tag (Tag_Name    : String;
                        Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                        Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
     Global => null;

   procedure Start_Tag (Tag_Name    : String;
                        Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                        Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Call_Result);
      pragma Unreferenced (Parent_Tags);
   begin
      Aida.Text_IO.Put ("Start tag: ");
      Aida.Text_IO.Put_Line (Tag_Name);
   end Start_Tag;

   procedure Attribute (Attribute_Name              : String;
                        Attribute_Value             : String;
                        Parent_Tags_And_Current_Tag : Aida.XML.Tag_Name_Vectors.Vector;
                        Call_Result                 : in out Aida.XML.Subprogram_Call_Result.T) with
     Global => null;

   procedure Attribute (Attribute_Name              : String;
                        Attribute_Value             : String;
                        Parent_Tags_And_Current_Tag : Aida.XML.Tag_Name_Vectors.Vector;
                        Call_Result                 : in out Aida.XML.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Parent_Tags_And_Current_Tag);
   begin
      Aida.Text_IO.Put ("Attribute Name: ");
      Aida.Text_IO.Put_Line (Attribute_Name);
      Aida.Text_IO.Put ("Attribute Value: ");
      Aida.Text_IO.Put_Line (Attribute_Value);
   end Attribute;

   procedure End_Tag (Tag_Name    : String;
                      Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                      Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
     Global => null;

   procedure End_Tag (Tag_Name    : String;
                      Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                      Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Tag_Name);
      pragma Unreferenced (Parent_Tags);
   begin
      Aida.Text_IO.Put_Line ("End Tag");
   end End_Tag;

   procedure End_Tag (Tag_Name    : String;
                      Tag_Value   : String;
                      Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                      Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
     Global => null;

   procedure End_Tag (Tag_Name    : String;
                      Tag_Value   : String;
                      Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                      Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
   is
      pragma Unreferenced (Tag_Name);
      pragma Unreferenced (Parent_Tags);
   begin
      Aida.Text_IO.Put ("End Tag: ");
      Aida.Text_IO.Put_Line (Tag_Value);
   end End_Tag;

   procedure Text (Value       : String;
                   Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                   Call_Result : in out Aida.XML.Subprogram_Call_Result.T) is
   begin
      Aida.Text_IO.Put ("Text:");
      Aida.Text_IO.Put_Line (Value);
   end Text;

   procedure Comment (Value       : String;
                      Parent_Tags : Aida.XML.Tag_Name_Vectors.Vector;
                      Call_Result : in out Aida.XML.Subprogram_Call_Result.T) is
   begin
      Aida.Text_IO.Put ("Comment:");
      Aida.Text_IO.Put_Line (Value);
   end Comment;

   procedure Parse_XML_File is new Aida.XML.Generic_Parse_XML_File (Start_Tag,
                                                                    Attribute,
                                                                    Text,
                                                                    Comment,
                                                                    End_Tag,
                                                                    End_Tag);

   use Aida.XML.Subprogram_Call_Result;

   procedure Parse (Contents : String) is
      Call_Result : Aida.XML.Subprogram_Call_Result.T;
   begin
      Parse_XML_File (Contents, Call_Result);
      if Has_Failed (Call_Result) then
         Aida.Text_IO.Put_Line (Message (Call_Result));
      else
         Aida.Text_IO.Put_Line ("Success!");
      end if;
   end Parse;

end Ex_Reader;
