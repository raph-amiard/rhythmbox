with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with Glfw.Input; use Glfw.Input;

package body Widgets is

   use Interfaces.C;

   Color_Note_Inactive : constant NVG_Color := RGBA (128, 130, 134, 200);
   Color_Note_Active : constant NVG_Color := RGBA (255, 130, 134, 200);
   Color_Note_Current : constant NVG_Color := RGBA (10, 255, 100, 200);

   K : constant Sequencer_Note := (Note => (G, 3), Duration => 3000);

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
         if MX >= W.X and then MY >= W.Y
           and then MX <= W.X + W.Width and then MY <= W.Y + W.Height
         then
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
      Note : constant Natural := Natural (double'Floor (X / Step)) + 1;
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

end Widgets;
