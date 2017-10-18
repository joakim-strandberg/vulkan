with Ada.Strings.Unbounded;

package body Vk_XML.Type_Tag is

   use all type Ada.Strings.Unbounded.Unbounded_String;

   procedure Set_Name (This : in out T;
                       Value : Aida.String_T;
                       SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Category (This : in out T;
                           Value : Aida.String_T;
                           SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Category := (Exists => True,
                           Value  => new (SP) Aida.String_T'(Value));
   end Set_Category;

   procedure Set_Requires (This : in out T;
                           Value : Aida.String_T;
                           SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Requires := (Exists => True,
                           Value  => new (SP) Aida.String_T'(Value));
   end Set_Requires;

   procedure Set_Parent (This : in out T;
                         Value : Aida.String_T;
                         SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Parent := (Exists => True,
                         Value  => new (SP) Aida.String_T'(Value));
   end Set_Parent;

   procedure Set_Returned_Only (This : in out T;
                                Value : Boolean)
   is
   begin
      This.My_Returned_Only := (Exists => True,
                                Value  => Value);
   end Set_Returned_Only;

   procedure Set_Comment (This : in out T;
                          Value : Aida.String_T;
                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Comment := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Comment;

   function To_String (This : T) return String is
      S : Ada.Strings.Unbounded.Unbounded_String;

      procedure Append_Name_To_String is
      begin
         if This.Exists_Name then
            Append (S, "name='");
            Append (S, String (This.Name));
            Append (S, "' ");
         else
            null;
         end if;
      end Append_Name_To_String;

      procedure Append_Category_To_String is
      begin
         if This.Exists_Category then
            Append (S, "category='");
            Append (S, String (This.Category));
            Append (S, "' ");
         else
            null;
         end if;
      end Append_Category_To_String;

      procedure Append_Requires_To_String is
      begin
         if This.Exists_Requires then
            Append (S, "requires='");
            Append (S, String (This.Requires));
            Append (S, "' ");
         else
            null;
         end if;
      end Append_Requires_To_String;

   begin
      if This.My_Children.Is_Empty then
         Append (S, "<type ");
         Append_Name_To_String;
         Append_Category_To_String;
         Append_Requires_To_String;
         Append (S, "/>");
      else
         Append (S, "<type ");
         Append_Name_To_String;
         Append_Category_To_String;
         Append_Requires_To_String;
         Append (S, ">");
         Append (S, "</type>");
      end if;

      return To_String (S);
   end To_String;

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Set_Struct_Extends (This  : in out T;
                                 Value : Aida.String_T;
                                 SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Struct_Extends := (Exists => True,
                                 Value  => new (SP) Aida.String_T'(Value));
   end Set_Struct_Extends;

end Vk_XML.Type_Tag;
