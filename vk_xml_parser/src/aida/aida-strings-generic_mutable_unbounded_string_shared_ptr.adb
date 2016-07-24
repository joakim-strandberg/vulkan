package body Aida.Strings.Generic_Mutable_Unbounded_String_Shared_Ptr with SPARK_Mode => Off is

   use all type US.T;

   procedure Initialize (This : out T;
                         Text : String)
   is
      V : constant US_Ptr := new US.T;
   begin
      This.SP := Smart_Pointers.Create (V);
      US.Initialize (V.all, Text);
   end Initialize;

   procedure Append (This : in out T;
                     Text : String) is
   begin
      US.Append (Smart_Pointers.Value (This.SP).all, Text);
   end Append;

   function "=" (L, R : T) return Boolean is
   begin
      return (Smart_Pointers.Value (L.SP).all = Smart_Pointers.Value (R.SP).all);
   end "=";

end Aida.Strings.Generic_Mutable_Unbounded_String_Shared_Ptr;
