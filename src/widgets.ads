with Command; use Command;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils;
with Ada_NanoVG;  use Ada_NanoVG;
with Main_Support; use Main_Support;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Array_Utils;
with Sequencer;

package Widgets is

   GUI_Sample_Nb : Utils.Sample_Period := 0;

   Color_Note_Inactive       : constant NVG_Color := RGBA (128, 130, 134, 255);
   Color_Note_Active         : constant NVG_Color := RGBA (255, 130, 134, 255);
   Color_Note_Current        : constant NVG_Color := RGBA (10, 255, 100, 255);
   Color_PS_Buttons_BG       : constant NVG_Color := RGBA (50, 50, 50, 255);
   Color_PS_Buttons_Inactive : constant NVG_Color := RGBA (90, 90, 100, 255);
   Color_PS_Buttons_Active   : constant NVG_Color := RGBA (150, 150, 160, 255);

   -----------------
   -- Base_Widget --
   -----------------

   type Base_Widget_Type is abstract new Mouse_Listener_Interface with record
      X, Y, Width, Height : Float;
   end record;

   type Base_Widget is access all Base_Widget_Type'Class;

   overriding procedure Mouse_Event
     (Self : in out Base_Widget_Type;
      Kind : Mouse_Event_Kind;
      X, Y : Float) is null;

   procedure Draw
     (Self : access Base_Widget_Type; Ctx : access NVG_Context) is abstract;

   procedure Layout (Self : in out Base_Widget_Type) is null;

   procedure Set_Width  (Self : in out Base_Widget_Type'Class; Val : Float);
   procedure Set_Height (Self : in out Base_Widget_Type'Class; Val : Float);
   procedure Set_X      (Self : in out Base_Widget_Type'Class; Val : Float);
   procedure Set_Y      (Self : in out Base_Widget_Type'Class; Val : Float);

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
     (X, Y, W, H : Float; BG_Color : NVG_Color := No_Color) return Container
   is
     (new Container_Widget'(X        => X,
                            Y        => Y,
                            Width    => W,
                            Height   => H,
                            Widgets  => <>,
                            BG_Color => BG_Color));

   overriding procedure Draw
     (Self : access Container_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Container_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   overriding procedure Layout (Self : in out Container_Widget);

   --------------------------------
   -- Vertical_Stacked_Container --
   --------------------------------

   type Vertical_Stacked_Container is new Container_Widget with record
      Current_Y : Float;
      Margin    : Float;
      Inter_W   : Float;
   end record;

   procedure Add_Widget
     (Self : access Vertical_Stacked_Container;
      W    : access Base_Widget_Type'Class);

   overriding procedure Layout (Self : in out Vertical_Stacked_Container);

   function Create
     (X, Y, Width, Margin : Float;
      Inter_W             : Float;
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
         Inter_W   => Inter_W,
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

   function RB_Track
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

   function Create_Play_Stop
     (Width, Height : Float) return access RB_Play_Stop_Widget;

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
      Current_Value                       : Float;
      Margin                              : Float := 5.0;
      Has_Mouse                           : Boolean := False;
      G                                   : Generator_Access := null;
      Val_Index                           : Natural;
   end record;

   function Vertical_Pot
     (X, Y              : Float;
      G                 : Generator_Access;
      Val_Index         : Natural) return access Vertical_Pot_Widget'Class
   is
     (new Vertical_Pot_Widget'
        (X             => X,
         Y             => Y,
         Width         => 50.0,
         Height        => 100.0,
         Current_Value => (G.Get_Value (Val_Index)
                           / (G.Get_Max_Value (Val_Index)
                              - G.Get_Min_Value (Val_Index))),
         G             => G,
         Val_Index     => Val_Index,
         others        => <>));

   overriding procedure Draw
     (Self : access Vertical_Pot_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Vertical_Pot_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   -------------------------
   -- Circular_Pot_Widget --
   -------------------------

   CP_Min : constant := 0.7 * Pi;
   CP_Max : constant := 2.3 * Pi;

   type Circular_Pot_Widget is new Vertical_Pot_Widget with record
      Last_X, Last_Y, X_Delta, Y_Delta : Float;
   end record;

   function Circular_Pot
     (X, Y              : Float;
      G                 : Generator_Access;
      Val_Index         : Natural) return access Circular_Pot_Widget'Class
   is
     (new Circular_Pot_Widget'
        (X             => X,
         Y             => Y,
         Width         => 50.0,
         Height        => 50.0,
         Current_Value => (G.Get_Value (Val_Index)
                           / (G.Get_Max_Value (Val_Index)
                              - G.Get_Min_Value (Val_Index))),
         G             => G,
         Val_Index     => Val_Index,
         others        => <>));

   overriding procedure Draw
     (Self : access Circular_Pot_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Circular_Pot_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   ----------------------------------
   -- Horizontal_Stacked_Container --
   ----------------------------------

   type Horizontal_Stacked_Container_Mode is (Take_Height, Set_Height);

   type Horizontal_Stacked_Container is new Container_Widget with record
      Current_X : Float;
      Margin    : Float;
      Inter_W   : Float;
      Mode      : Horizontal_Stacked_Container_Mode := Set_Height;
   end record;

   procedure Add_Widget
     (Self : access Horizontal_Stacked_Container;
      W    : access Base_Widget_Type'Class);

   overriding procedure Layout (Self : in out Horizontal_Stacked_Container);

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
      Sequencer  : access I_Simulation_Listener'Class;
      Instrument : access Generator'Class;
   end record;

   package Sequencer_Arrays is new Array_Utils (Seq_Instrument_Tuple);
   subtype Sequencer_Array is Sequencer_Arrays.Array_Type;
   subtype Sequencer_Vector is Sequencer_Arrays.Vectors.Vector;

   type Seq_Group_Widget is new Container_Widget with record
      Sequencers_Stack : access Vertical_Stacked_Container;
      Buttons_Stack    : access Vertical_Stacked_Container;
      Sequencers       : Sequencer_Vector;
      Saved_Widgets    : access Widget_Vectors.Vector := null;
      Main_Container   : Container;
   end record;

   function Seq_Group
     (X, Y, Width, Margin : Float;
      Seq                 : Sequencer_Array;
      Main_Container      : Container)
      return access Seq_Group_Widget'Class;

   procedure Add_Seq (Self : access Seq_Group_Widget;
                      S    : Seq_Instrument_Tuple);

   overriding procedure Layout (Self : in out Seq_Group_Widget);

   -------------------
   -- Simple_Button --
   -------------------

   type Simple_Button is new Base_Widget_Type with record
      Active_Color   : NVG_Color;
      Inactive_Color : NVG_Color;
      Is_Active      : Boolean;
      Has_Mouse      : Boolean := False;
      Text           : Unbounded_String := Null_Unbounded_String;
   end record;

   procedure Init_Button
     (Button                       : access Simple_Button'Class;
      X, Y, Width, Height          : Float;
      Text                         : Unbounded_String := Null_Unbounded_String;
      Active_Color, Inactive_Color : NVG_Color := No_Color);

   overriding procedure Draw
     (Self : access Simple_Button; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Simple_Button;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   procedure Do_Action (Self : in out Simple_Button) is null;

   -----------------------
   -- Instrument_Button --
   -----------------------

   type Instrument_Button is new Simple_Button with record
      Seq_Group : access Seq_Group_Widget'Class;
      Index     : Natural;
   end record;
   overriding procedure Do_Action (Self : in out Instrument_Button);

   ------------------------
   -- Back_To_Seq_Button --
   ------------------------

   type Back_To_Seq_Button is new Simple_Button with record
      Seq_Group : access Seq_Group_Widget'Class;
   end record;
   overriding procedure Do_Action (Self : in out Back_To_Seq_Button);

   -------------------------
   -- Scrolling_Container --
   -------------------------
   type Scrolling_Button;
   type Scrolling_Button_Access is access all Scrolling_Button'Class;

   type Scrolling_Container is new Container_Widget with record
      Y_Offset            : Float := 0.0;
      Margin              : Float := 5.0;
      Has_Mouse           : Boolean := False;
      Up_B, Down_B        : Scrolling_Button_Access;
   end record;

   function Scrolling (X, Y, Width, Height : Float;
                       Internal_Widget     : access Base_Widget_Type'Class;
                       Margin              : Float := 0.0)
                       return not null access Scrolling_Container'Class;

   function Widget
     (Self : Scrolling_Container) return access Base_Widget_Type'Class
   is
     (Self.Widgets.First_Element);

   overriding procedure Layout (Self : in out Scrolling_Container);

   overriding procedure Draw
     (Self : access Scrolling_Container; Ctx : access NVG_Context);

   type Direction_T is (Up, Down);
   type Scrolling_Button is new Simple_Button with record
      Direction : Direction_T;
      Window    : access Scrolling_Container'Class;
   end record;
   overriding procedure Do_Action (Self : in out Scrolling_Button);

   ----------------
   -- Piano Roll --
   ----------------

   type Piano_Roll_Notes is new Base_Widget_Type with record
      Page_Height : Float;
   end record;

   overriding procedure Draw
     (Self : access Piano_Roll_Notes; Ctx : access NVG_Context);

   type Piano_Roll_Grid is new Base_Widget_Type with record
      Page_Height : Float;
      Bars        : Natural;
      Sub_Bars    : Natural;
      Seq         : access Sequencer.Sequencer;
   end record;

   overriding procedure Draw
     (Self : access Piano_Roll_Grid; Ctx : access NVG_Context);

   overriding procedure Mouse_Event
     (Self : in out Piano_Roll_Grid;
      Kind : Mouse_Event_Kind;
      X, Y : Float);

   function Piano_Roll
     (X, Y, Width, Height : Float;
      Seq                 : access Sequencer.Sequencer) return Base_Widget;
   -----------------------
   -- Piano_Roll_Button --
   -----------------------

   type Piano_Roll_Button is new Simple_Button with record
      Seq_Group : access Seq_Group_Widget'Class;
      Index     : Natural;
   end record;
   overriding procedure Do_Action (Self : in out Piano_Roll_Button);

end Widgets;
