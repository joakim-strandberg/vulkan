with Vk;
with Aida.Text_IO;

package body Vk_Package_Creator with SPARK_Mode is

   use all type Vk.Registry_Shared_Ptr.T;
   use all type Vk.Registry.Fs.Child_Vectors.Immutable_T;
   use all type Vk.Registry.Fs.Child_Kind_Id_T;
   use all type Vk.XML_Out_Commented_Message_Shared_Ptr.T;
   use all type Vk.Types_Shared_Ptr.T;
   use all type Vk.Types.Fs.Child_Vectors.Immutable_T;
   use all type Vk.Types.Fs.Child_Kind_Id_T;
   use all type Vk.Type_Shared_Ptr.T;

   procedure Handle_Child_Type (Type_V : Vk.Type_Shared_Ptr.T) is
   begin
      null;
   end Handle_Child_Type;

   procedure Handle_Out_Commented_Message (Out_Commented_Message_V : Vk.XML_Out_Commented_Message_Shared_Ptr.T) is
   begin
      Aida.Text_IO.Put ("Out commented message:");
      Aida.Text_IO.Put_Line (Vk.XML_Out_Commented_Message_Shared_Ptr.To_String (Out_Commented_Message_V));
   end Handle_Out_Commented_Message;

   procedure Handle_Child_Types (Types_V : Vk.Types_Shared_Ptr.T) is
   begin
      for I in Positive range First_Index (Children (Types_V))..Last_Index (Children (Types_V)) loop
         case Element (Children (Types_V), I).Kind_Id is
            when Child_XML_Dummy             => null;
            when Child_Type                  => Handle_Child_Type (Element (Children (Types_V), I).Type_V);
            when Child_Out_Commented_Message => Handle_Out_Commented_Message(Element (Children (Types_V), I).Out_Commented_Message_V);
         end case;
      end loop;
   end Handle_Child_Types;

   procedure Create_Vk_Package (R : Vk.Registry_Shared_Ptr.T) is
   begin
      for I in Positive range First_Index (Children (R))..Last_Index (Children (R)) loop
         case Element (Children (R), I).Kind_Id is
            when Child_Comment =>
               Aida.Text_IO.Put ("Registry child comment with value:");
               Aida.Text_IO.Put_Line (Vk.Comment.Fs.Value.To_String (Vk.Comment_Shared_Ptr.Value (Vk.Registry.Fs.Child_Vectors.Element (Children (R), I).C)));
            when Child_Out_Commented_Message =>
               Aida.Text_IO.Put ("Registry child out commented message:");
               Aida.Text_IO.Put_Line (Vk.XML_Out_Commented_Message_Shared_Ptr.To_String (Vk.Registry.Fs.Child_Vectors.Element (Children (R), I).Out_Commented_Message_V));
            when Child_Vendor_Ids =>
               null;
            when Child_Tags =>
               null;
            when Child_Types =>
               Handle_Child_Types (Element (Children (R), I).Types_V);
            when Child_Enums =>
               null;
            when Child_Commands =>
               null;
            when Child_Feature =>
               null;
            when Child_Extensions =>
               null;
            when Child_XML_Dummy =>
               null;
         end case;
      end loop;
   end Create_Vk_Package;

end Vk_Package_Creator;
