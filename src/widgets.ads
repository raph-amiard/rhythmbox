with Command; use Command;
with Ada.Containers.Vectors;
with Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils;
with Ada_NanoVG; use Ada_NanoVG;

package Widgets is

   No_Color : constant NVG_Color := RGBA (0, 0, 0, 0);
   GUI_Sample_Nb : Utils.Sample_Period := 0;

   type Base_Widget_Type is abstract tagged record
      X, Y, Width, Height : Float;
   end record;

   -----------------
   -- Base_Widget --
   -----------------

   type Base_Widget is access all Base_Widget_Type'Class;

   procedure Mouse_Clicked
     (Self    : not null access Base_Widget_Type;
      X, Y    : Float) is null;

   procedure Draw
     (Self : access Base_Widget_Type; Ctx : access NVG_Context) is abstract;

   -------------------
   -- Widget_Window --
   -------------------

   package Widget_Vectors is new Ada.Containers.Vectors (Natural, Base_Widget);

   type Widget_Window is record
      Widgets : Widget_Vectors.Vector;
   end record;

   procedure Mouse_Clicked
     (Self : not null access Widget_Window; X, Y : Float);

   procedure Draw
     (Self : in out Widget_Window; Ctx : access NVG_Context);

   ----------------------
   -- Container_Widget --
   ----------------------

   type Container_Widget is new Base_Widget_Type with record
      Widgets : Widget_Vectors.Vector;
      BG_Color  : NVG_Color;
   end record;

   overriding procedure Draw
     (Self : access Container_Widget; Ctx : access NVG_Context);

   overriding procedure Mouse_Clicked
     (Self    : not null access Container_Widget;
      X, Y    : Float);

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

   overriding procedure Mouse_Clicked
     (Self : not null access RB_Track_Widget;
      X, Y : Float);

   function Create
     (S : access Simple_Sequencer;
      Name : String;
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

   overriding procedure Mouse_Clicked
     (Self    : not null access RB_Play_Stop_Widget;
      X, Y    : Float);

end Widgets;
