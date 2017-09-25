package body Vk_XML.Implicit_External_Sync_Parameters_Tag is

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Vk_XML.Implicit_External_Sync_Parameters_Tag;
