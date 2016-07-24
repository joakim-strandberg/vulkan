package body Aida.Containers.Generic_Immutable_Vector with SPARK_Mode => Off is

   function Length (Container : Immutable_T) return Capacity_Range is
   begin
      return Vectors.Length (Smart_Pointers.Value (Container.SP).all);
   end Length;

   function Is_Empty (Container : Immutable_T) return Boolean is
   begin
      return Vectors.Is_Empty (Smart_Pointers.Value (Container.SP).all);
   end Is_Empty;

   function Element (Container : Immutable_T;
                     Index     : Index_Type) return Element_Type is
   begin
      return Vectors.Element (Smart_Pointers.Value (Container.SP).all,
                              Index);
   end Element;

   function First_Index (Container : Immutable_T) return Index_Type is
   begin
      return Vectors.First_Index (Smart_Pointers.Value (Container.SP).all);
   end First_Index;

   function First_Element (Container : Immutable_T) return Element_Type is
   begin
      return Vectors.First_Element (Smart_Pointers.Value (Container.SP).all);
   end First_Element;

   function Last_Index (Container : Immutable_T) return Extended_Index is
   begin
      return Vectors.Last_Index (Smart_Pointers.Value (Container.SP).all);
   end Last_Index;

   function Last_Element (Container : Immutable_T) return Element_Type is
   begin
      return Vectors.Last_Element (Smart_Pointers.Value (Container.SP).all);
   end Last_Element;

   function Find_Index (Container : Immutable_T;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First) return Extended_Index is
   begin
      return Vectors.Find_Index (Smart_Pointers.Value (Container.SP).all,
                                 Item,
                                 Index);
   end Find_Index;

   function Reverse_Find_Index (Container : Immutable_T;
                                Item      : Element_Type;
                                Index     : Index_Type := Index_Type'Last) return Extended_Index is
   begin
      return Vectors.Reverse_Find_Index (Smart_Pointers.Value (Container.SP).all,
                                         Item,
                                         Index);
   end Reverse_Find_Index;

   function Contains (Container : Immutable_T;
                      Item      : Element_Type) return Boolean is
   begin
      return Vectors.Contains (Smart_Pointers.Value (Container.SP).all,
                               Item);
   end Contains;

   function Has_Element (Container : Immutable_T;
                         Position  : Extended_Index) return Boolean is
   begin
      return Vectors.Has_Element (Smart_Pointers.Value (Container.SP).all,
                                  Position);
   end Has_Element;

end Aida.Containers.Generic_Immutable_Vector;
