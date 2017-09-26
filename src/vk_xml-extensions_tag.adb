package body Vk_XML.Extensions_Tag is

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Vk_XML.Extensions_Tag;
