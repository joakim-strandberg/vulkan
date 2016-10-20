with System.Address_To_Access_Conversions;

generic
   type Object (<>) is limited private;
package Generic_Address_To_Access_Conversions is

   pragma Compile_Time_Warning
     (Object'Unconstrained_Array,
      "Object is unconstrained array type" & ASCII.LF &
        "To_Pointer results may not have bounds");

   type Object_Pointer is access all Object;
   for Object_Pointer'Size use Standard'Address_Size;

   pragma No_Strict_Aliasing (Object_Pointer);
   --  Strictly speaking, this routine should not be used to generate pointers
   --  to other than proper values of the proper type, but in practice, this
   --  is done all the time. This pragma stops the compiler from doing some
   --  optimizations that may cause unexpected results based on the assumption
   --  of no strict aliasing.

   type Object_Address is new System.Address;

   Null_Address : constant Object_Address := Object_Address (System.Null_Address);

   function To_Pointer (Value : Object_Address) return Object_Pointer;
   pragma Inline (To_Pointer);

   function To_Address (Value : Object_Pointer) return Object_Address;
   pragma Inline (To_Address);

private

   package Private_Conversions is new System.Address_To_Access_Conversions (Object);

end Generic_Address_To_Access_Conversions;
