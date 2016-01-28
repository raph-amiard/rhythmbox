with Command; use Command;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils;
with Ada_NanoVG;  use Ada_NanoVG;
with Main_Support; use Main_Support;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Array_Utils;

package Widgets is

   No_Color : constant NVG_Color := RGBA (0, 0, 0, 0);
   GUI_Sample_Nb : Utils.Sample_Period := 0;

   type Base_Widget_Type is abstract new Mouse_Listener_Interface with record
      X, Y, Width, Height : Float;
   end record;

   -----------------
   -- Base_Widget --
   -----------------

   type Base_Widget is access all Base_Widget_Type'Class;

   overriding procedure Mouse_Event
     (Self : in out Base_Widget_Type;
      Kind : Mouse_Event_Kind;
      X, Y : Float) is null;

   procedure Draw
     (Self : access Base_Widget_Type; Ctx : access NVG_Context) is abstract;

   package Widget_Vectors is new Ada.Containers.Vectors (Natural, Base_Widget);

   ----------------------
   -- Container_Widget --
   ----------------------

   type Container_Widget is new Base_Widget_Type with record
      Widgets  : access Widget_Vectors.Vector := new Widget_Vectors.Vector;
      BG_Color : NVG_Color;
   end record;
   type Container is access all Container_Widget;

   function New_Container
     (X, Y, W, H : Float) return Container
   is
     (new Container_Widget'(X        => X,
                            Y        => Y,
                            Width    => W,
                            Height   => H,
                            Widgets  => <>,
                            BG_Color => No_Color));

   overriding procedure Draw
     (Self : access Container_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Container_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   --------------------------------
   -- Vertical_Stacked_Container --
   --------------------------------

   type Vertical_Stacked_Container is new Container_Widget with record
      Current_Y : Float;
      Margin    : Float;
   end record;

   procedure Add_Widget
     (Self : access Vertical_Stacked_Container; W : Base_Widget);

   function Create
     (X, Y, Width, Margin : Float;
      BG_Color            : NVG_Color := No_Color)
      return access Vertical_Stacked_Container'Class
   is
     (new Vertical_Stacked_Container'
        (X         => X,
         Y         => Y,
         Width     => Width,
         Height    => Margin,
         Current_Y => Y,
         Margin    => Margin,
         Widgets   => <>,
         BG_Color  => BG_Color));

   ---------------------
   -- RB_Track_Widget --
   ---------------------

   type RB_Track_Widget is new Base_Widget_Type with record
      Name      : Unbounded_String;
      Sequencer : access Simple_Sequencer;
   end record;

   overriding procedure Draw
     (Self : access RB_Track_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out RB_Track_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   function Create
     (S : access Simple_Sequencer;
      X, Y, Width, Height : Float) return access RB_Track_Widget;

   -------------------------
   -- RB_Play_Stop_Widget --
   -------------------------

   type Play_Stop_State is (Play, Stop);

   type RB_Play_Stop_Widget is new Base_Widget_Type with record
      Margin : Float;
      Button_W, Button_H, SB_X, PB_X, B_Y : Float;
      State : Play_Stop_State := Stop;
   end record;

   function Create_Play_Stop (X, Y : Float) return access RB_Play_Stop_Widget;

   overriding procedure Draw
     (Self : access RB_Play_Stop_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out RB_Play_Stop_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   -------------------------
   -- Vertical_Pot_Widget --
   -------------------------

   type Vertical_Pot_Widget is new Base_Widget_Type with record
      Min_Value, Max_Value, Current_Value : Float;
      Margin                              : Float := 5.0;
      Has_Mouse                           : Boolean := False;
      G                                   : Generator_Access := null;
      Val_Index                           : Natural;
   end record;

   function Vertical_Pot
     (X, Y              : Float;
      G                 : Generator_Access;
      Val_Index         : Natural) return access Vertical_Pot_Widget
   is
     (new Vertical_Pot_Widget'(X             => X,
                               Y             => Y,
                               Width         => 50.0,
                               Height        => 100.0,
                               Min_Value     => G.Get_Min_Value (Val_Index),
                               Max_Value     => G.Get_Max_Value (Val_Index),
                               Current_Value => G.Get_Value (Val_Index),
                               G             => G,
                               Val_Index     => Val_Index,
                               others        => <>));

   overriding procedure Draw
     (Self : access Vertical_Pot_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Vertical_Pot_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   type Horizontal_Stacked_Container is new Container_Widget with record
      Current_X : Float;
      Margin    : Float;
   end record;

   procedure Add_Widget
     (Self : access Horizontal_Stacked_Container; W : Base_Widget);

   -----------------------
   -- Pots_Group_Widget --
   -----------------------

   type Pots_Group_Widget is new Horizontal_Stacked_Container with record
      null;
   end record;

   function Pots_Group
     (G : Generator_Access; X, Y : Float) return access Pots_Group_Widget;

   ----------------------
   -- Seq_Group_Widget --
   ----------------------

   type Seq_Instrument_Tuple is record
      Sequencer  : access Simple_Sequencer;
      Instrument : access Generator'Class;
   end record;

   package Sequencer_Arrays is new Array_Utils (Seq_Instrument_Tuple);
   subtype Sequencer_Array is Sequencer_Arrays.Array_Type;
   subtype Sequencer_Vector is Sequencer_Arrays.Vectors.Vector;

   type Seq_Group_Widget is new Container_Widget with record
      Sequencers_Stack : access Vertical_Stacked_Container;
      Sequencers       : Sequencer_Vector;
      Saved_Widgets    : access Widget_Vectors.Vector := null;
   end record;

   function Seq_Group
     (X, Y, Width, Margin : Float;
      Seq                 : Sequencer_Array)
      return access Seq_Group_Widget'Class;

   procedure Add_Seq (Self : access Seq_Group_Widget;
                      S    : Seq_Instrument_Tuple);

   -------------------
   -- Simple_Button --
   -------------------

   type Simple_Button is new Base_Widget_Type with record
      Active_Color   : NVG_Color;
      Inactive_Color : NVG_Color;
      Is_Active      : Boolean;
      Has_Mouse      : Boolean := False;
   end record;

   procedure Init_Button
     (Button                       : access Simple_Button'Class;
      X, Y, Width, Height          : Float;
      Active_Color, Inactive_Color : NVG_Color := No_Color);

   overriding procedure Draw
     (Self : access Simple_Button; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Simple_Button;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   procedure Do_Action (Self : in out Simple_Button) is null;

   type Instrument_Button is new Simple_Button with record
      Seq_Group : access Seq_Group_Widget'Class;
      Index     : Natural;
   end record;

   overriding procedure Do_Action (Self : in out Instrument_Button);

   type Back_To_Seq_Button is new Simple_Button with record
      Seq_Group : access Seq_Group_Widget'Class;
   end record;

   overriding procedure Do_Action (Self : in out Back_To_Seq_Button);

end Widgets;
