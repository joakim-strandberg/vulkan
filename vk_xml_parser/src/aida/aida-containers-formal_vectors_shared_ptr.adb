package body Aida.Containers.Formal_Vectors_Shared_Ptr with SPARK_Mode => Off is

   function Length (Container : Vector) return Capacity_Range is
   begin
      return Vectors.Length (Smart_Pointers.Value (Container.SP).all);
   end Length;

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Vectors.Is_Empty (Smart_Pointers.Value (Container.SP).all);
   end Is_Empty;

   procedure Clear (Container : in out Vector) is
   begin
      Vectors.Clear (Smart_Pointers.Value (Container.SP).all);
   end Clear;

   function Element (Container : Vector;
                     Index     : Index_Type) return Element_Type is
   begin
      return Vectors.Element (Smart_Pointers.Value (Container.SP).all,
                              Index);
   end Element;

   procedure Append (Container : in out Vector;
                     New_Item  : Vector) is
   begin
      Vectors.Append (Smart_Pointers.Value (Container.SP).all,
                      Smart_Pointers.Value (New_Item.SP).all);
   end Append;

   procedure Append (Container : in out Vector;
                     New_Item  : Element_Type) is
   begin
      Vectors.Append (Smart_Pointers.Value (Container.SP).all,
                      New_Item);
   end Append;

   procedure Delete_Last (Container : in out Vector) is
   begin
      Vectors.Delete_Last (Smart_Pointers.Value (Container.SP).all);
   end Delete_Last;

   function First_Index (Container : Vector) return Index_Type is
   begin
      return Vectors.First_Index (Smart_Pointers.Value (Container.SP).all);
   end First_Index;

   function First_Element (Container : Vector) return Element_Type is
   begin
      return Vectors.First_Element (Smart_Pointers.Value (Container.SP).all);
   end First_Element;

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Vectors.Last_Index (Smart_Pointers.Value (Container.SP).all);
   end Last_Index;

   function Last_Element (Container : Vector) return Element_Type is
   begin
      return Vectors.Last_Element (Smart_Pointers.Value (Container.SP).all);
   end Last_Element;

   function Find_Index (Container : Vector;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First) return Extended_Index is
   begin
      return Vectors.Find_Index (Smart_Pointers.Value (Container.SP).all,
                                 Item,
                                 Index);
   end Find_Index;

   function Reverse_Find_Index (Container : Vector;
                                Item      : Element_Type;
                                Index     : Index_Type := Index_Type'Last) return Extended_Index is
   begin
      return Vectors.Reverse_Find_Index (Smart_Pointers.Value (Container.SP).all,
                                         Item,
                                         Index);
   end Reverse_Find_Index;

   function Contains (Container : Vector;
                      Item      : Element_Type) return Boolean is
   begin
      return Vectors.Contains (Smart_Pointers.Value (Container.SP).all,
                               Item);
   end Contains;

   function Has_Element (Container : Vector;
                         Position  : Extended_Index) return Boolean is
   begin
      return Vectors.Has_Element (Smart_Pointers.Value (Container.SP).all,
                                  Position);
   end Has_Element;

   function "=" (L, R : Vector) return Boolean is
      LHS : Vectors.Vector renames Smart_Pointers.Value (L.SP).all;
      RHS : Vectors.Vector renames Smart_Pointers.Value (R.SP).all;
   begin
      return (if Length (LHS) = Length (RHS) then
             (for all I in First_Index (LHS)..Last_Index (LHS) => Vectors.Element (LHS, I) = Vectors.Element (RHS, First_Index (RHS) + I - First_Index (LHS)))
                 else
             False);
   end "=";

end Aida.Containers.Formal_Vectors_Shared_Ptr;
