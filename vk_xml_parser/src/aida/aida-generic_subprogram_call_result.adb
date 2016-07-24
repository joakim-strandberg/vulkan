package body Aida.Generic_Subprogram_Call_Result with SPARK_Mode is

   use Unbounded_String;

   procedure Initialize (This    : in out T;
                         Message : String) is
   begin
      Initialize (This => This.My_Message,
                  Text => Message);
      This.My_Has_Failed := True;
   end Initialize;

   function Has_Failed (This : T) return Boolean with
     Refined_Post => Has_Failed'Result = This.My_Has_Failed is
   begin
      return This.My_Has_Failed;
   end Has_Failed;

   function Message (This : T) return String is
   begin
      return To_String (This.My_Message);
   end Message;

end Aida.Generic_Subprogram_Call_Result;
