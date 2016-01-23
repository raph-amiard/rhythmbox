with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with Glfw.Input; use Glfw.Input;

package body Widgets is

   use Interfaces.C;

   Color_Note_Inactive       : constant NVG_Color := RGBA (128, 130, 134, 200);
   Color_Note_Active         : constant NVG_Color := RGBA (255, 130, 134, 200);
   Color_Note_Current        : constant NVG_Color := RGBA (10, 255, 100, 200);
   Color_PS_Buttons_BG       : constant NVG_Color := RGBA (50, 50, 50, 255);
   Color_PS_Buttons_Inactive : constant NVG_Color := RGBA (90, 90, 90, 255);
   Color_PS_Buttons_Active : constant NVG_Color := RGBA (150, 150, 150, 255);

   K : constant Sequencer_Note := (Note => (G, 3), Duration => 3000);

   procedure Gr_Rect (Ctx : access NVG_Context; X, Y, W, H, R : Float;
                      Color : NVG_Color);

   -------------
   -- Gr_Rect --
   -------------

   procedure Gr_Rect (Ctx : access NVG_Context; X, Y, W, H, R : Float;
                      Color : NVG_Color)
   is
   begin
      Begin_Path (Ctx);
      Rounded_Rect (Ctx, X, Y, W, H, R);
      Fill_Color (Ctx, Color);
      Fill (Ctx);

      Fill_Paint (Ctx,
                  Linear_Gradient
                    (Ctx, X, Y, X, Y + 15.0,
                     RGBA (255, 255, 255, 8), RGBA (0, 0, 0, 16)));
      Fill (Ctx);
   end Gr_Rect;

   -------------
   -- In_Zone --
   -------------

   function In_Zone (MX, MY, X, Y, W, H : Coord) return Boolean is
     (MX >= X and then MY >= Y
      and then MX <= X + W and then MY <= Y + H);

   ---------------
   -- In_Widget --
   ---------------

   function In_Widget (X, Y : Coord; W : Base_Widget) return Boolean is
      (In_Zone (X, Y, W.X, W.Y, W.Width, W.Height));

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access RB_Track_Widget; Ctx : access NVG_Context)
   is
      Step : constant double := Self.Width / double (Self.Sequencer.Nb_Steps);
      Gr   : NVG_Paint;
      X, Y, W, H : Float;
   begin
      for I in 1 .. Self.Sequencer.Nb_Steps loop
         X := Float (Self.X + (Step * double (I - 1)) + 2.0);
         Y := Float (Self.Y + 2.0);
         W := Float (Step - 2.0);
         H := Float (Self.Height - 2.0);

         Begin_Path (Ctx);
         Rounded_Rect (Ctx, X, Y, W, H, 5.0);

         if Self.Sequencer.Current_Note = I then
            Fill_Color (Ctx, Color_Note_Current);
         elsif Self.Sequencer.Notes (I) = No_Seq_Note then
            Fill_Color (Ctx, Color_Note_Inactive);
         else
            Fill_Color (Ctx, Color_Note_Active);
         end if;

         Fill (Ctx);

         Gr := Linear_Gradient
           (Ctx, X, Y, X, Y + 15.0,
            RGBA (255, 255, 255, 8), RGBA (0, 0, 0, 16));
         Fill_Paint (Ctx, Gr);
         Fill (Ctx);
      end loop;
   end Draw;

   --------------------------
   -- Mouse_Button_Changed --
   --------------------------

   overriding procedure Mouse_Button_Changed
     (Self   : not null access Widget_Window;
      Button : Input.Mouse.Button;
      State  : Input.Button_State;
      Mods   : Input.Keys.Modifiers)
   is
      MX, MY : Glfw.Input.Mouse.Coordinate;
   begin
      Self.Get_Cursor_Pos (MX, MY);

      for W of Self.Widgets loop
         if In_Widget (MX, MY, W) then
            W.Mouse_Button_Changed (MX, MY, Button, State, Mods);
         end if;
      end loop;
   end Mouse_Button_Changed;

   --------------------------
   -- Mouse_Button_Changed --
   --------------------------

   overriding procedure Mouse_Button_Changed
     (Self    : not null access RB_Track_Widget;
      X, Y    : Input.Mouse.Coordinate;
      Button  : Input.Mouse.Button;
      State   : Input.Button_State;
      Mods    : Input.Keys.Modifiers)
   is
      pragma Unreferenced (Y, Button, Mods);
      Step : constant double := Self.Width / double (Self.Sequencer.Nb_Steps);
      Note : constant Natural :=
        Natural (double'Floor ((X - Self.X) / Step)) + 1;
   begin
      Put_Line ("Step : " & Step'Img);
      if State = Pressed then
         Put_Line ("Clicked on sequencer "
                   & To_String (Self.Name) & " Note " & Note'Img);
         if Note in 1 .. Self.Sequencer.Nb_Steps then
            declare
               Seq_Note : Sequencer_Note renames
                 Self.Sequencer.Notes (Note);
            begin
               if Seq_Note = No_Seq_Note then
                  Self.Sequencer.Notes (Note) := K;
               else
                  Self.Sequencer.Notes (Note) := No_Seq_Note;
               end if;
            end;
         end if;
      end if;
   end Mouse_Button_Changed;

   ------------
   -- Create --
   ------------

   function Create
     (S                   : access Simple_Sequencer;
      Name                : String;
      X, Y, Width, Height : Interfaces.C.double) return access RB_Track_Widget
   is
   begin
      Put_Line ("Width : " & Width'Img);
      return new RB_Track_Widget'
        (Sequencer => S, X => X, Y => Y, Width => Width, Height => Height,
         Name      => To_Unbounded_String (Name));
   end Create;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Self : access Widget_Window; Ctx : access NVG_Context)
   is
   begin
      for W of Self.Widgets loop
         W.Draw (Ctx);
      end loop;
   end Draw;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Container_Widget; Ctx : access NVG_Context)
   is
   begin
      if Self.BG_Color /= No_Color then
         Begin_Path (Ctx);
         Rounded_Rect
           (Ctx, Float (Self.X), Float (Self.Y),
            Float (Self.Width), Float (Self.Height), 5.0);
         Fill_Color (Ctx, Self.BG_Color);
         Fill (Ctx);
      end if;

      for W of Self.Widgets loop
         W.Draw (Ctx);
      end loop;
   end Draw;

   ----------------
   -- Add_Widget --
   ----------------

   procedure Add_Widget
     (Self : access Vertical_Stacked_Container; W : Base_Widget) is
   begin
      W.X := Self.X + Self.Margin;
      W.Y := Self.Current_Y + Self.Margin;
      W.Width := Self.Width - (Self.Margin * 2.0);
      Self.Widgets.Append (W);
      Self.Current_Y := Self.Current_Y + W.Height + Self.Margin;
      Self.Height := Self.Height + W.Height + Self.Margin;
   end Add_Widget;

   --------------------------
   -- Mouse_Button_Changed --
   --------------------------

   overriding procedure Mouse_Button_Changed
     (Self    : not null access Container_Widget;
      X, Y    : Input.Mouse.Coordinate;
      Button  : Input.Mouse.Button;
      State   : Input.Button_State;
      Mods    : Input.Keys.Modifiers) is
   begin
      for W of Self.Widgets loop
         if In_Widget (X, Y, W) then
            W.Mouse_Button_Changed (X, Y, Button, State, Mods);
         end if;
      end loop;
   end Mouse_Button_Changed;

   ------------
   -- Create --
   ------------

   function Create_Play_Stop (X, Y : Coord) return access RB_Play_Stop_Widget
   is
      W  : constant Coord := 160.0;
      H  : constant Coord := 80.0;
      M  : constant Float := 5.0;
      HM : constant Float := M / 2.0;
      RX : constant Coord := (X - (W / 2.0));
   begin
      return new RB_Play_Stop_Widget'
        (X      => RX,
         Y      => Y,
         Width  => W,
         Height => H,
         Margin => 5.0,
         Button_W => Float (W / 2.0) - (M + HM),
         Button_H => Float (H) - (2.0 * M),
         PB_X     => Float (RX) + M,
         SB_X     => Float (RX) + Float (W / 2.0) + HM,
         B_Y      => Float (Y) + M,
         State    => Stop);

   end Create_Play_Stop;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access RB_Play_Stop_Widget; Ctx : access NVG_Context)
   is
      M         : constant Float := Float (Self.Margin);
      HM        : constant Float := M / 2.0;
      Half_W    : constant Float := Self.Button_W / 2.0;
      Quarter_W : constant Float := Self.Button_W / 4.0;

   begin
      --  Background
      Begin_Path (Ctx);
      Rounded_Rect (Ctx, Float (Self.X), Float (Self.Y),
                    Float (Self.Width), Float (Self.Height), HM);
      Fill_Color (Ctx, Color_PS_Buttons_BG);
      Fill (Ctx);

      --  Play button
      Gr_Rect
        (Ctx, Self.PB_X, Self.B_Y, Self.Button_W, Self.Button_H, M,
         (if Self.State = Play
          then Color_PS_Buttons_Active else Color_PS_Buttons_Inactive));
      Begin_Path (Ctx);
      Move_To (Ctx, Self.PB_X + Quarter_W, Self.B_Y + Quarter_W);
      Line_To (Ctx, Self.PB_X + Half_W + Quarter_W, Self.B_Y + Half_W);
      Line_To (Ctx, Self.PB_X + Quarter_W, Self.B_Y + Half_W + Quarter_W);
      Line_To (Ctx, Self.PB_X + Quarter_W, Self.B_Y + Quarter_W);
      Close_Path (Ctx);
      Fill_Color (Ctx, RGBA (100, 255, 100, 255));
      Fill (Ctx);

      --  Stop button
      Gr_Rect (Ctx, Self.SB_X, Self.B_Y, Self.Button_W, Self.Button_H, M,
               (if Self.State = Stop
                then Color_PS_Buttons_Active else Color_PS_Buttons_Inactive));

      Begin_Path (Ctx);
      Move_To (Ctx, Self.SB_X + Quarter_W,          Self.B_Y + Quarter_W);
      Line_To (Ctx, Self.SB_X + Half_W + Quarter_W, Self.B_Y + Quarter_W);
      Line_To (Ctx, Self.SB_X + Half_W + Quarter_W,
               Self.B_Y + Half_W + Quarter_W);
      Line_To (Ctx, Self.SB_X + Quarter_W, Self.B_Y + Half_W + Quarter_W);
      Line_To (Ctx, Self.SB_X + Quarter_W, Self.B_Y + Quarter_W);
      Close_Path (Ctx);
      Fill_Color (Ctx, RGBA (255, 100, 100, 255));
      Fill (Ctx);
   end Draw;

   --------------------------
   -- Mouse_Button_Changed --
   --------------------------

   overriding procedure Mouse_Button_Changed
     (Self    : not null access RB_Play_Stop_Widget;
      X, Y    : Input.Mouse.Coordinate;
      Button  : Input.Mouse.Button;
      State   : Input.Button_State;
      Mods    : Input.Keys.Modifiers)
   is
      pragma Unreferenced (Button, Mods);
   begin
      if State = Pressed then
         if In_Zone (X, Y, Coord (Self.PB_X), Coord (Self.B_Y),
                     Coord (Self.Button_W), Coord (Self.Button_H))
         then
            Self.State := Play;
         elsif
           In_Zone (X, Y, Coord (Self.SB_X), Coord (Self.B_Y),
                    Coord (Self.Button_W), Coord (Self.Button_H))
         then
            Self.State := Stop;
         end if;
      end if;
   end Mouse_Button_Changed;

end Widgets;
