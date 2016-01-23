with Command; use Command;
with Ada_NanoVG; use Ada_NanoVG;
with Ada.Containers.Vectors;
with Main_Support; use Main_Support;
with Glfw.Input;
with Glfw.Input.Mouse;
with Glfw.Input.Keys;
use Glfw;
with Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Widgets is

   subtype Coord is Interfaces.C.double;

   type Base_Widget_Type is abstract tagged record
      X, Y, Width, Height : Coord;
   end record;

   -----------------
   -- Base_Widget --
   -----------------

   type Base_Widget is access all Base_Widget_Type'Class;

   procedure Mouse_Click (Self : access Base_Widget; X, Y : Natural) is null;

   procedure Mouse_Button_Changed
     (Self    : not null access Base_Widget_Type;
      X, Y    : Input.Mouse.Coordinate;
      Button  : Input.Mouse.Button;
      State   : Input.Button_State;
      Mods    : Input.Keys.Modifiers) is null;

   procedure Draw
     (Self : access Base_Widget_Type; Ctx : access NVG_Context) is abstract;

   -------------------
   -- Widget_Window --
   -------------------

   package Widget_Vectors is new Ada.Containers.Vectors (Natural, Base_Widget);

   type Widget_Window is new Simple_Window with record
      Widgets : Widget_Vectors.Vector;
   end record;

   overriding procedure Mouse_Button_Changed
     (Self   : not null access Widget_Window;
      Button : Input.Mouse.Button;
      State  : Input.Button_State;
      Mods   : Input.Keys.Modifiers);

   procedure Draw
     (Self : access Widget_Window; Ctx : access NVG_Context);

   ----------------------
   -- Container_Widget --
   ----------------------

   type Container_Widget is new Base_Widget_Type with record
      Widgets : Widget_Vectors.Vector;
      BG_Color  : NVG_Color;
   end record;

   overriding procedure Draw
     (Self : access Container_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Button_Changed
     (Self    : not null access Container_Widget;
      X, Y    : Input.Mouse.Coordinate;
      Button  : Input.Mouse.Button;
      State   : Input.Button_State;
      Mods    : Input.Keys.Modifiers);

   --------------------------------
   -- Vertical_Stacked_Container --
   --------------------------------

   type Vertical_Stacked_Container is new Container_Widget with record
      Current_Y : Coord;
      Margin    : Coord;
   end record;

   procedure Add_Widget
     (Self : access Vertical_Stacked_Container; W : Base_Widget);

   function Create
     (X, Y, Width, Margin : Coord;
      BG_Color            : NVG_Color := No_Color)
      return access Vertical_Stacked_Container
   is
     (new Vertical_Stacked_Container'
        (X         => X,
         Y         => Y,
         Width     => Width,
         Height    => Margin,
         Current_Y => Y,
         Margin    => Margin,
         Widgets   => Widget_Vectors.Empty_Vector,
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

   overriding procedure Mouse_Button_Changed
     (Self    : not null access RB_Track_Widget;
      X, Y    : Input.Mouse.Coordinate;
      Button  : Input.Mouse.Button;
      State   : Input.Button_State;
      Mods    : Input.Keys.Modifiers);

   function Create
     (S : access Simple_Sequencer;
      Name : String;
      X, Y, Width, Height : Interfaces.C.double) return access RB_Track_Widget;

   -------------------------
   -- RB_Play_Stop_Widget --
   -------------------------

   type Play_Stop_State is (Play, Stop);

   type RB_Play_Stop_Widget is new Base_Widget_Type with record
      Margin : Coord;
      Button_W, Button_H, SB_X, PB_X, B_Y : Float;
      State : Play_Stop_State := Stop;
   end record;

   function Create_Play_Stop (X, Y : Coord) return access RB_Play_Stop_Widget;

   overriding procedure Draw
     (Self : access RB_Play_Stop_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Button_Changed
     (Self    : not null access RB_Play_Stop_Widget;
      X, Y    : Input.Mouse.Coordinate;
      Button  : Input.Mouse.Button;
      State   : Input.Button_State;
      Mods    : Input.Keys.Modifiers);

end Widgets;
