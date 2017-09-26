package body Vk_XML.Registry_Tag is

   procedure Append_Child (This    : in out T;
                           Comment : not null Comment_Tag.Ptr)
   is
      Child : Child_T := (Kind_Id => Child_Comment,
                          Comment => Comment);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Vk_XML.Registry_Tag;
