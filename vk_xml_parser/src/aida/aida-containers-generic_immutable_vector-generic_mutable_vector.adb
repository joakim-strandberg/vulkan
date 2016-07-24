package body Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector is

   procedure Clear (Container : in out T) is
   begin
      Vectors.Clear (Smart_Pointers.Value (Container.SP).all);
   end Clear;

   procedure Append (Container : in out T;
                     New_Item  : T) is
   begin
      Vectors.Append (Smart_Pointers.Value (Container.SP).all,
                      Smart_Pointers.Value (New_Item.SP).all);
   end Append;

   procedure Append (Container : in out T;
                     New_Item  : Element_Type) is
   begin
      Vectors.Append (Smart_Pointers.Value (Container.SP).all,
                      New_Item);
   end Append;

   procedure Delete_Last (Container : in out T) is
   begin
      Vectors.Delete_Last (Smart_Pointers.Value (Container.SP).all);
   end Delete_Last;

end Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector;
