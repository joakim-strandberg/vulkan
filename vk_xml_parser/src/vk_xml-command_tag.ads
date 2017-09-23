with Vk_XML.Proto_Tag;
with Vk_XML.Param_Tag;
with Vk_XML.Validity_Tag;
with Vk_XML.Implicit_External_Sync_Parameters_Tag;

package Vk_XML.Command_Tag is

   use all type Ada.Containers.Count_Type;

   package Success_Code_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                               Element_Type => String_Ptr,
                                                               "="          => "=");

   type Success_Codes_Ref (E : not null access constant Success_Code_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   package Error_Code_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => String_Ptr,
                                                             "="          => "=");

   type Error_Codes_Ref (E : not null access constant Error_Code_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Child_Kind_Id_T is (
                            Child_Proto,
                            Child_Param,
                            Child_Validity,
                            Child_Implicit_External_Sync_Parameters
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Proto) is record
      case Kind_Id is
         when Child_Proto                             => Proto      : Proto_Tag.Ptr;
         when Child_Param                             => Param      : Param_Tag.Ptr;
         when Child_Validity                          => Validity   : Validity_Tag.Ptr;
         when Child_Implicit_External_Sync_Parameters => Parameters : Implicit_External_Sync_Parameters_Tag.Ptr;
      end case;
   end record;

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Queue_T is (
                    Sparse_Binding,
                    Graphics,
                    Compute,
                    Transfer
                   );

   package Queue_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Queue_T,
                                                        "="          => "=");

   type Queues_Ref (E : not null access constant Queue_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Render_Pass_T is (
                          Inside,
                          Outside,
                          Both
                         );

   package Render_Pass_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Render_Pass_T,
                                                              "="          => "=");

   type Render_Passes_Ref (E : not null access constant Render_Pass_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Command_Buffer_Level_T is (
                                   Primary,
                                   Secondary
                                  );

   package Command_Buffer_Level_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                       Element_Type => Command_Buffer_Level_T,
                                                                       "="          => "=");

   type Command_Buffer_Levels_Ref (E : not null access constant Command_Buffer_Level_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type T is tagged limited private;

   function Success_Codes (This : aliased T) return Success_Codes_Ref;

   procedure Append_Success_Code (This : in out T;
                                  Item : Aida.String_T) with
     Global => null;--,
--     Post   => This.Success_Codes'Result.Length = This.Success_Codes.Length + 1;

   function Error_Codes (This : aliased T) return Error_Codes_Ref;

   procedure Append_Error_Code (This : in out T;
                                Item : Aida.String_T);

   function Children (This : aliased T) return Children_Ref;

   procedure Append_Child (This  : in out T;
                           Item : Child_T);

   function Queues (This : aliased T) return Queues_Ref;

   procedure Append_Queue (This : in out T;
                           Item : Queue_T);

   function Render_Passes (This : aliased T) return Render_Passes_Ref;

   procedure Append_Render_Pass (This : in out T;
                                 Item : Render_Pass_T);

   function Command_Buffer_Levels (This : aliased T) return Command_Buffer_Levels_Ref;

   procedure Append_Command_Buffer_Level (This : in out T;
                                          Item : Command_Buffer_Level_T);

   function To_String (This : T) return String;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type T is tagged limited
      record
         My_Success_Codes         : aliased Success_Code_Vectors.Vector;
         My_Error_Codes           : aliased Error_Code_Vectors.Vector;
         My_Children              : aliased Child_Vectors.Vector;
         My_Queues                : aliased Queue_Vectors.Vector;
         My_Render_Passes         : aliased Render_Pass_Vectors.Vector;
         My_Command_Buffer_Levels : aliased Command_Buffer_Level_Vectors.Vector;
      end record;

   function Success_Codes (This : aliased T) return Success_Codes_Ref is ((E => This.My_Success_Codes'Access));

   function Error_Codes (This : aliased T) return Error_Codes_Ref is ((E => This.My_Error_Codes'Access));

   function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   function Queues (This : aliased T) return Queues_Ref is ((E => This.My_Queues'Access));

   function Render_Passes (This : aliased T) return Render_Passes_Ref is ((E => This.My_Render_Passes'Access));

   function Command_Buffer_Levels (This : aliased T) return Command_Buffer_Levels_Ref is ((E => This.My_Command_Buffer_Levels'Access));

end Vk_XML.Command_Tag;
