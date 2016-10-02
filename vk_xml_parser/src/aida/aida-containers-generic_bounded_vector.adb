package body Aida.Containers.Generic_Bounded_Vector is

   function First_Index (This : T) return Index_T is
      pragma Unreferenced (This);
   begin
      return Index_T'First;
   end First_Index;

   function Element (This  : T;
                     Index : Index_T)
                     return Element_T is
   begin
      return This.My_Elements (Index);
   end Element;

   procedure Clear (This : in out T) is
   begin
      This.My_Number_Of_Elements := 0;
   end Clear;

   procedure Append (This     : in out T;
                     New_Item : Element_T) is
   begin
      if This.My_Number_Of_Elements = 0 then
--           This.My_Last_Index  := Index_T'First;
         This.My_Elements (Index_T'First) := New_Item;
         This.My_Number_Of_Elements := This.My_Number_Of_Elements + 1;
      else
--           This.My_Last_Index  := This.My_Last_Index + 1;
         This.My_Elements (Index_T'First + Index_T'Base (This.My_Number_Of_Elements)) := New_Item;
         This.My_Number_Of_Elements := This.My_Number_Of_Elements + 1;
      end if;
   end Append;

end Aida.Containers.Generic_Bounded_Vector;
