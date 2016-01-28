with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with Interfaces.C;

package body Widgets is

   procedure Set_Value
     (Self : in out Vertical_Pot_Widget; Y : Float);

   Color_Note_Inactive       : constant NVG_Color := RGBA (128, 130, 134, 255);
   Color_Note_Active         : constant NVG_Color := RGBA (255, 130, 134, 255);
   Color_Note_Current        : constant NVG_Color := RGBA (10, 255, 100, 255);
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

      --  Fill_Paint (Ctx,
      --              Linear_Gradient
      --                (Ctx, X, Y, X, Y + 15.0,
      --                 RGBA (255, 255, 255, 8), RGBA (0, 0, 0, 16)));
      --  Fill (Ctx);
   end Gr_Rect;

   -------------
   -- In_Zone --
   -------------

   function In_Zone (MX, MY, X, Y, W, H : Float) return Boolean is
     (MX >= X and then MY >= Y
      and then MX <= X + W and then MY <= Y + H);

   ---------------
   -- In_Widget --
   ---------------

   function In_Widget (X, Y : Float; W : Base_Widget) return Boolean is
      (In_Zone (X, Y, W.X, W.Y, W.Width, W.Height));

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access RB_Track_Widget; Ctx : access NVG_Context)
   is
      Step         : constant Float :=
        Self.Width / Float (Self.Sequencer.Nb_Steps);
      X, Y, W, H   : Float;
      Current_Note : constant Natural :=
        Note_For_Sample (Self.Sequencer.all, GUI_Sample_Nb);
   begin
      for I in 1 .. Self.Sequencer.Nb_Steps loop
         X := Self.X + (Step * Float (I - 1)) + 2.0;
         Y := Self.Y + 2.0;
         W := Step - 2.0;
         H := Self.Height - 2.0;

         Begin_Path (Ctx);
         Rect (Ctx, X, Y, W, H);

         if Current_Note = I then
            Fill_Color (Ctx, Color_Note_Current);
         elsif Self.Sequencer.Notes (I) = No_Seq_Note then
            Fill_Color (Ctx, Color_Note_Inactive);
         else
            Fill_Color (Ctx, Color_Note_Active);
         end if;

         Fill (Ctx);

         --  Gr := Linear_Gradient
         --    (Ctx, X, Y, X, Y + 15.0,
         --     RGBA (255, 255, 255, 8), RGBA (0, 0, 0, 16));
         --  Fill_Paint (Ctx, Gr);
         --  Fill (Ctx);
      end loop;
   end Draw;

   -------------------
   -- Mouse_Clicked --
   -------------------

   overriding procedure Mouse_Event
     (Self : in out RB_Track_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float)
   is
      pragma Unreferenced (Y);
      Step : constant Float := Self.Width / Float (Self.Sequencer.Nb_Steps);
      Note : constant Natural :=
        Natural (Float'Floor ((X - Self.X) / Step)) + 1;
   begin
      if Kind = Pressed then
         Put_Line ("Step : " & Step'Img);
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
   end Mouse_Event;

   ------------
   -- Create --
   ------------

   function Create
     (S                   : access Simple_Sequencer;
      X, Y, Width, Height : Float) return access RB_Track_Widget
   is
   begin
      Put_Line ("Width : " & Width'Img);
      return new RB_Track_Widget'
        (Sequencer => S, X => X, Y => Y, Width => Width, Height => Height,
         Name      => S.Track_Name);
   end Create;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Container_Widget; Ctx : access NVG_Context)
   is
   begin
      if Self.BG_Color /= No_Color then
         Begin_Path (Ctx);
         Rounded_Rect (Ctx, Self.X, Self.Y, Self.Width, Self.Height, 5.0);
         Fill_Color (Ctx, Self.BG_Color);
         Fill (Ctx);
      end if;

      for W of Self.Widgets.all loop
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

   ----------------
   -- Add_Widget --
   ----------------

   procedure Add_Widget
     (Self : access Horizontal_Stacked_Container; W : Base_Widget) is
   begin
      Self.Widgets.Append (W);
      W.X            := Self.Current_X + Self.Margin;
      Put_Line ("ADDING WIDGET, X = " & W.X'Img);
      W.Y            := Self.Y + Self.Margin;
      W.Height       := Self.Height - (Self.Margin * 2.0);
      Self.Current_X := Self.Current_X + W.Width + Self.Margin;
      Self.Width     := Self.Width + W.Width + Self.Margin;
   end Add_Widget;

   -------------------
   -- Mouse_Clicked --
   -------------------

   overriding procedure Mouse_Event
     (Self : in out Container_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float)
   is
   begin
      for W of Self.Widgets.all loop
         if In_Widget (X, Y, W) then
            W.Mouse_Event (Kind, X, Y);
         end if;
      end loop;
   end Mouse_Event;

   ------------
   -- Create --
   ------------

   function Create_Play_Stop (X, Y : Float) return access RB_Play_Stop_Widget
   is
      W  : constant Float := 100.0;
      H  : constant Float := 50.0;
      M  : constant Float := 5.0;
      HM : constant Float := M / 2.0;
      RX : constant Float := (X - (W / 2.0));
   begin
      return new RB_Play_Stop_Widget'
        (X      => RX,
         Y      => Y,
         Width  => W,
         Height => H,
         Margin => 5.0,
         Button_W => W / 2.0 - (M + HM),
         Button_H => H - (2.0 * M),
         PB_X     => RX + M,
         SB_X     => RX + W / 2.0 + HM,
         B_Y      => Y + M,
         State    => Stop);

   end Create_Play_Stop;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access RB_Play_Stop_Widget; Ctx : access NVG_Context)
   is
      M         : constant Float := Self.Margin;
      HM        : constant Float := M / 2.0;
      Half_W    : constant Float := Self.Button_W / 2.0;
      Quarter_W : constant Float := Self.Button_W / 4.0;

   begin
      --  Background
      Begin_Path (Ctx);
      Rounded_Rect (Ctx, Self.X, Self.Y, Self.Width, Self.Height, HM);
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

   overriding procedure Mouse_Event
     (Self : in out RB_Play_Stop_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float)
   is
   begin
      if Kind = Pressed then
         if In_Zone (X, Y, Self.PB_X, Self.B_Y,
                     Self.Button_W, Self.Button_H)
         then
            Self.State := Play;
         elsif
           In_Zone (X, Y, Self.SB_X, Self.B_Y, Self.Button_W, Self.Button_H)
         then
            Self.State := Stop;
         end if;
      end if;
   end Mouse_Event;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Vertical_Pot_Widget; Ctx : access NVG_Context)
   is
      use Interfaces.C;
      Real_Height : constant Float := Self.Height - Self.Margin * 2.0;
      Level       : constant Float :=
        (Real_Height * Float'Min (Self.Current_Value / Self.Max_Value, 1.0));
   begin
      --  Background
      Begin_Path (Ctx);
      Rounded_Rect (Ctx, Self.X, Self.Y, Self.Width, Self.Height, 5.0);
      Fill_Color (Ctx, Color_PS_Buttons_BG);
      Fill (Ctx);

      declare
         IX       : constant Float := Self.X + Self.Margin;
         IY       : constant Float := Self.Y + Self.Margin;
         IY_Level : constant Float := IY + Real_Height - Level;
         Width    : constant Float := Self.Width - Self.Margin * 2.0;
         Height   : constant Float := Self.Height - Self.Margin * 2.0;
      begin
         --  Container
         Begin_Path (Ctx);
         Rect (Ctx, IX, IY, Width, Height);
         Fill_Color (Ctx, Color_PS_Buttons_Active);
         Fill (Ctx);

         --  Level
         Begin_Path (Ctx);
         Rect (Ctx, IX, IY_Level, Width, Level);
         Fill_Color (Ctx, Color_Note_Active);
         Fill (Ctx);
         Fill (Ctx);
      end;

      --  Text
      Font_Size (Ctx, 18.0);
      Font_Face (Ctx, "sans");
      Text_Align (Ctx, NVG_ALIGN_BOTTOM or NVG_ALIGN_CENTER);
      Text_Letter_Spacing (Ctx, -2.5);
      Fill_Color (Ctx, RGBA (255, 255, 255, 255));
      Text (Ctx, Self.X + Self.Width / 2.0,
            Self.Y + Self.Height + 20.0, Self.G.Get_Name (Self.Val_Index));

   end Draw;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self : in out Vertical_Pot_Widget; Y : Float)
   is
      Real_Height : constant Float := Self.Height - Self.Margin * 2.0;
      Start       : constant Float := Self.Y + Real_Height + Self.Margin;
      Diff        : constant Float := Start - Y;
      Value       : constant Float := (Diff / Real_Height) * Self.Max_Value;
   begin
      if Diff >= 0.0 then
         Self.Current_Value := Float'Min (Value, Self.Max_Value);
         if Self.G /= null then
            Self.G.Set_Value (Self.Val_Index, Self.Current_Value);
         end if;
      end if;
   end Set_Value;

   -----------------
   -- Mouse_Event --
   -----------------

   overriding procedure Mouse_Event
     (Self : in out Vertical_Pot_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float)
   is
      pragma Unreferenced (X);
   begin
      if Kind = Pressed then
         Take_Mouse (Mouse_Listener'(Self'Unrestricted_Access));
         Self.Has_Mouse := True;
         Set_Value (Self, Y);
      elsif Kind = Moved and then Self.Has_Mouse then
         Set_Value (Self, Y);
      elsif Kind = Released and then Self.Has_Mouse then
         Self.Has_Mouse := False;
         Return_Mouse (Mouse_Listener'(Self'Unrestricted_Access));
      end if;
   end Mouse_Event;

   ----------------
   -- Pots_Group --
   ----------------

   function Pots_Group
     (G : Generator_Access; X, Y : Float) return access Pots_Group_Widget
   is
      Obj : constant access Pots_Group_Widget := new Pots_Group_Widget'
        (X          => X,
         Margin     => 5.0,
         BG_Color   => RGBA (100, 100, 100, 255),
         Current_X  => X,
         Y          => Y,
         Width      => 5.0,
         Height     => 220.0,
         others => <>);
      Params : constant Generator_Array := G.Get_Params;
   begin

      for Param of Params loop
         for I in 0 .. Param.Nb_Values - 1 loop
            Obj.Add_Widget (Vertical_Pot (0.0, Y + 5.0, Param, I));
         end loop;
      end loop;

      return Obj;
   end Pots_Group;

   ---------------
   -- Seq_Group --
   ---------------

   function Seq_Group
     (X, Y, Width, Margin : Float;
      Seq                 : Sequencer_Array)
      return access Seq_Group_Widget'Class
   is
      Seqs_Stack : constant access Vertical_Stacked_Container :=
        Create (X      => X + 50.0,
                Y      => Y,
                Width  => Width - 50.0,
                Margin => Margin);

   begin
      return S : access Seq_Group_Widget'Class do
         S := new Seq_Group_Widget'
           (X         => X,
            Y         => Y,
            Width     => Width,
            Height    => Margin,
            Widgets   => <>,
            BG_Color  => RGBA (51, 51, 51, 255),
            Sequencers_Stack => Seqs_Stack,
            Sequencers       => <>,
            Saved_Widgets    => <>);
         S.Widgets.Append (Seqs_Stack);

         for Ss of Seq loop
            S.Add_Seq (Ss);
         end loop;
      end return;

   end Seq_Group;

   -------------
   -- Add_Seq --
   -------------

   procedure Add_Seq (Self : access Seq_Group_Widget;
                      S    : Seq_Instrument_Tuple)
   is
   begin
      Self.Sequencers_Stack.Add_Widget
        (Create (S.Sequencer, 0.0, 0.0, 0.0, 50.0));
      Self.Sequencers.Append (S);
      Self.Height := Self.Sequencers_Stack.Height;
      declare
         S : constant Base_Widget :=
           Self.Sequencers_Stack.Widgets.Last_Element;
         B : constant access Instrument_Button'Class := new Instrument_Button'
           (Seq_Group => Self,
            Index     => Self.Sequencers_Stack.Widgets.Last_Index,
            others    => <>);
      begin
         Init_Button (B, Self.X + 5.0, S.Y, 50.0, S.Height);
         Self.Widgets.Append (B);
      end;
   end Add_Seq;

   procedure Init_Button
     (Button                       : access Simple_Button'Class;
      X, Y, Width, Height          : Float;
      Active_Color, Inactive_Color : NVG_Color := No_Color)
   is
   begin
      Button.X              := X;
      Button.Y              := Y;
      Button.Width          := Width;
      Button.Height         := Height;
      Button.Active_Color   :=
        (if Active_Color = No_Color then Color_PS_Buttons_Active
            else Active_Color);
      Button.Inactive_Color :=
        (if Inactive_Color = No_Color then Color_PS_Buttons_Inactive
            else Inactive_Color);
      Button.Is_Active      := False;
      Button.Has_Mouse      := False;
   end Init_Button;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Simple_Button; Ctx : access NVG_Context)
   is
      M : constant Float := 5.0;
   begin
      Begin_Path (Ctx);
      Rounded_Rect (Ctx, Self.X, Self.Y, Self.Width, Self.Height, 5.0);
      Fill_Color (Ctx, Color_PS_Buttons_BG);
      Fill (Ctx);

      Begin_Path (Ctx);
      Rounded_Rect
        (Ctx, Self.X + M, Self.Y + M,
         Self.Width - M * 2.0, Self.Height - M * 2.0, 5.0);
      Fill_Color
        (Ctx, (if Self.Is_Active
         then Self.Active_Color else Self.Inactive_Color));
      Fill (Ctx);
   end Draw;

   -----------------
   -- Mouse_Event --
   -----------------

   overriding procedure Mouse_Event
     (Self : in out Simple_Button;
      Kind : Mouse_Event_Kind;
      X, Y : Float)
   is
      S : Simple_Button'Class := Self;
   begin
      if Kind = Pressed then
         Take_Mouse (Mouse_Listener'(Self'Unrestricted_Access));
         Self.Has_Mouse := True;
         Self.Is_Active := True;
      elsif Kind = Released and then Self.Has_Mouse then
         Self.Has_Mouse := False;
         Return_Mouse (Mouse_Listener'(Self'Unrestricted_Access));
         Self.Is_Active := False;

         if In_Widget (X, Y, Self'Unrestricted_Access) then
            S.Do_Action;
         end if;
      end if;

   end Mouse_Event;

   ---------------
   -- Do_Action --
   ---------------

   overriding procedure Do_Action (Self : in out Instrument_Button)
   is
      P : constant access Pots_Group_Widget'Class :=
        Pots_Group (Self.Seq_Group.Sequencers (Self.Index).Instrument,
                    Self.Seq_Group.X + 40.0,
                    Self.Seq_Group.Y + 10.0);
      B : constant access Back_To_Seq_Button'Class :=
        new Back_To_Seq_Button'(Seq_Group => Self.Seq_Group, others => <>);
   begin
      Init_Button
        (B, Self.Seq_Group.X + 5.0, Self.Seq_Group.Y + 5.0, 30.0, 30.0);

      Self.Seq_Group.Saved_Widgets := Self.Seq_Group.Widgets;
      Self.Seq_Group.Widgets := new Widget_Vectors.Vector;
      Self.Seq_Group.Widgets.Append (P);
      Self.Seq_Group.Widgets.Append (B);
   end Do_Action;

   ---------------
   -- Do_Action --
   ---------------

   overriding procedure Do_Action (Self : in out Back_To_Seq_Button) is
   begin
      Self.Seq_Group.Widgets := Self.Seq_Group.Saved_Widgets;
   end Do_Action;

end Widgets;
