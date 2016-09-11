with Aida.UTF8;

package body Aida.UTF8_Code_Point with SPARK_Mode is

   function Image (Value : T) return String is
      Result  : String (1..4) := (others => ' ');
      Pointer : Integer := Result'First;
   begin
      Aida.UTF8.Put (Result, Pointer, Value);
      return Result (1..Pointer - 1);
   end Image;

end Aida.UTF8_Code_Point;
