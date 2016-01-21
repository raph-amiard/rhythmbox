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

   type Base_Widget_Type is abstract tagged record
      X, Y, Width, Height : Interfaces.C.double;
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

end Widgets;
