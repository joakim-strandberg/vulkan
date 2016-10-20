package body Generic_Address_To_Access_Conversions is

   function To_Pointer (Value : Object_Address) return Object_Pointer is
   begin
      return Object_Pointer (Private_Conversions.To_Pointer (System.Address (Value)));
   end To_Pointer;

   function To_Address (Value : Object_Pointer) return Object_Address is
   begin
      return Object_Address (Private_Conversions.To_Address (Private_Conversions.Object_Pointer (Value)));
   end To_Address;

end Generic_Address_To_Access_Conversions;
