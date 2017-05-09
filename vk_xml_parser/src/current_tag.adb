package body Current_Tag is

   use type Current_Tag_Fs.Id_T;

   Id_Counter : Current_Tag_Fs.Id_T;

   procedure Initialize (This : in out T) is
   begin
      This.Id := Id_Counter;
      Id_Counter := Id_Counter + 1;
   end Initialize;

end Current_Tag;
