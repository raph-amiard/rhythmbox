with Interfaces.C;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Widgets is

   procedure Set_Value
     (Self : in out Vertical_Pot_Widget; Y : Float);

   K : constant Sequencer_Note := (Note => (G, 3), Duration => 1000);

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width  (Self : in out Base_Widget_Type'Class; Val : Float) is
   begin
      Self.Width := Val;
      Self.Layout;
   end Set_Width;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height (Self : in out Base_Widget_Type'Class; Val : Float) is
   begin
      Self.Height := Val;
      Self.Layout;
   end Set_Height;

   -----------
   -- Set_X --
   -----------

   procedure Set_X      (Self : in out Base_Widget_Type'Class; Val : Float) is
   begin
      Self.X := Val;
      Self.Layout;
   end Set_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y      (Self : in out Base_Widget_Type'Class; Val : Float) is
   begin
      Self.Y := Val;
      Self.Layout;
   end Set_Y;

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

      Fill_Paint (Ctx, Linear_Gradient
                  (Ctx, X, Y, X, Y + 15.0,
                     RGBA (255, 255, 255, 8), RGBA (0, 0, 0, 16)));
      Fill (Ctx);
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

   function RB_Track
     (S                   : access Simple_Sequencer;
      X, Y, Width, Height : Float) return access RB_Track_Widget
   is
   begin
      return new RB_Track_Widget'
        (Sequencer => S, X => X, Y => Y, Width => Width, Height => Height,
         Name      => S.Track_Name);
   end RB_Track;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Container_Widget; Ctx : access NVG_Context)
   is
   begin
      if Self.BG_Color /= No_Color then
         Begin_Path (Ctx);
         Rect (Ctx, Self.X, Self.Y, Self.Width, Self.Height);
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
     (Self : access Vertical_Stacked_Container;
      W    : access Base_Widget_Type'Class) is
   begin
      Self.Widgets.Append (W);
      Self.Layout;
      W.Layout;
   end Add_Widget;

   ------------
   -- Layout --
   ------------

   overriding procedure Layout (Self : in out Vertical_Stacked_Container) is
      Cur_Y : Float := Self.Y + Self.Margin;

      procedure Layout_Widget
        (W : access Base_Widget_Type'Class; Y : Float);
      procedure Layout_Widget
        (W : access Base_Widget_Type'Class; Y : Float) is
      begin
         W.X := Self.X + Self.Margin;
         W.Y := Y;
         W.Width := Self.Width - (Self.Margin * 2.0);
      end Layout_Widget;
   begin
      if Self.Widgets.Is_Empty then
         return;
      end if;

      Layout_Widget (Self.Widgets.First_Element, Cur_Y);
      Cur_Y := Cur_Y + Self.Widgets.First_Element.Height + Self.Inter_W;

      for I in Self.Widgets.First_Index + 1 .. Self.Widgets.Last_Index loop
         Layout_Widget (Self.Widgets.Element (I), Cur_Y);
         Cur_Y := Cur_Y + Self.Widgets.Element (I).Height + Self.Inter_W;
      end loop;
      Self.Height := Cur_Y + Self.Margin;
   end Layout;

   ----------------
   -- Add_Widget --
   ----------------

   procedure Add_Widget
     (Self : access Horizontal_Stacked_Container;
      W    : access Base_Widget_Type'Class) is
   begin
      Self.Widgets.Append (W);
      Self.Layout;
      W.Layout;
   end Add_Widget;

   ------------
   -- Layout --
   ------------

   overriding procedure Layout (Self : in out Horizontal_Stacked_Container) is
      Cur_X : Float := Self.X + Self.Margin;

      procedure Layout_Widget
        (W : access Base_Widget_Type'Class; X : Float);
      procedure Layout_Widget
        (W : access Base_Widget_Type'Class; X : Float) is
      begin
         W.Y := Self.Y + Self.Margin;
         W.X := X;
         if Self.Mode = Take_Height then
            W.Height := Self.Height - (Self.Margin * 2.0);
         else
            Self.Height := W.Height + (Self.Margin * 2.0);
         end if;
      end Layout_Widget;
   begin
      if Self.Widgets.Is_Empty then
         return;
      end if;

      Layout_Widget (Self.Widgets.First_Element, Cur_X);
      Cur_X := Cur_X + Self.Widgets.First_Element.Width + Self.Inter_W;

      for I in Self.Widgets.First_Index + 1 .. Self.Widgets.Last_Index loop
         Layout_Widget (Self.Widgets.Element (I), Cur_X);
         Cur_X := Cur_X + Self.Widgets.Element (I).Width + Self.Inter_W;
      end loop;
      Self.Width := Cur_X + Self.Margin - Self.X;
   end Layout;

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

   function Create_Play_Stop
     (Width, Height : Float) return access RB_Play_Stop_Widget
   is
      X  : constant Float := Width / 2.0;
      W  : constant Float := 100.0;
      H  : constant Float := 50.0;
      M  : constant Float := 5.0;
      HM : constant Float := M / 2.0;
      RX : constant Float := (X - (W / 2.0));
   begin
      return new RB_Play_Stop_Widget'
        (X      => 0.0,
         Y      => 0.0,
         Width  => Width,
         Height => Height,
         Margin => 5.0,
         Button_W => W / 2.0 - (M + HM),
         Button_H => H - (2.0 * M),
         PB_X     => RX + M,
         SB_X     => RX + W / 2.0 + HM,
         B_Y      => M,
         State    => Stop);
   end Create_Play_Stop;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access RB_Play_Stop_Widget; Ctx : access NVG_Context)
   is
      M         : constant Float := Self.Margin;
      Half_W    : constant Float := Self.Button_W / 2.0;
      Quarter_W : constant Float := Self.Button_W / 4.0;

   begin
      --  Background
      Begin_Path (Ctx);
      Rect (Ctx, Self.X, Self.Y, Self.Width, Self.Height);
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
        (Real_Height * Float'Min (Self.Current_Value, 1.0));
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
      Value       : constant Float := (Diff / Real_Height);
   begin
      if Diff >= 0.0 then
         Self.Current_Value := Float'Min (Value, 1.0);
         if Self.G /= null then
            Self.G.Set_Scaled_Value
              (Self.Val_Index, Scaled_Value_T (Self.Current_Value));
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
         Inter_W    => 10.0,
         BG_Color   => No_Color,
         Current_X  => X,
         Y          => Y,
         Width      => 5.0,
         Height     => 80.0,
         others => <>);
      Params : constant Generator_Array := G.Get_Params;
   begin

      for Param of Params loop
         for I in 0 .. Param.Nb_Values - 1 loop
            Obj.Add_Widget (Circular_Pot (0.0, Y + 5.0, Param, I));
         end loop;
      end loop;

      return Obj;
   end Pots_Group;

   ---------------
   -- Seq_Group --
   ---------------

   function Seq_Group
     (X, Y, Width, Margin : Float;
      Seq                 : Sequencer_Array;
      Main_Container      : Container)
      return access Seq_Group_Widget'Class
   is
      Seqs_Stack : constant access Vertical_Stacked_Container :=
        Create (X       => 0.0,
                Y       => 0.0,
                Width   => 0.0,
                Margin  => 0.0,
                Inter_W => 2.0);

      Buttons_Stack : constant access Vertical_Stacked_Container :=
        Create (X       => 0.0,
                Y       => 0.0,
                Width   => 0.0,
                Margin  => 0.0,
                Inter_W => 2.0);
   begin
      return S : access Seq_Group_Widget'Class do
         S := new Seq_Group_Widget'
           (X                => X,
            Y                => Y,
            Width            => Width,
            Height           => Margin,
            Widgets          => <>,
            BG_Color         => No_Color,
            Sequencers_Stack => Seqs_Stack,
            Buttons_Stack    => Buttons_Stack,
            Main_Container   => Main_Container,
            Sequencers       => <>,
            Saved_Widgets    => <>);

         S.Widgets.Append (Seqs_Stack);
         S.Widgets.Append (Buttons_Stack);

         for Ss of Seq loop
            S.Add_Seq (Ss);
         end loop;
      end return;
   end Seq_Group;

   ------------
   -- Layout --
   ------------

   overriding procedure Layout (Self : in out Seq_Group_Widget) is
   begin
      Self.Sequencers_Stack.Width := Self.Width - 75.0;
      Self.Sequencers_Stack.X := Self.X + 75.0;
      Self.Sequencers_Stack.Y := Self.Y;
      Self.Sequencers_Stack.Layout;

      Self.Buttons_Stack.Width := 70.0;
      Self.Buttons_Stack.X := Self.X;
      Self.Buttons_Stack.Y := Self.Y;
      Self.Buttons_Stack.Layout;

      Self.Height := Self.Buttons_Stack.Height;
   end Layout;

   -------------
   -- Add_Seq --
   -------------

   procedure Add_Seq (Self : access Seq_Group_Widget;
                      S    : Seq_Instrument_Tuple)
   is
   begin
      if S.Sequencer.all in Simple_Sequencer'Class then
         Self.Sequencers_Stack.Add_Widget
           (RB_Track (Simple_Sequencer (S.Sequencer.all)'Access, 0.0, 0.0, 0.0, 40.0));
      else
         declare
            B : constant access Piano_Roll_Button'Class :=
              new Piano_Roll_Button'
              (Seq_Group => Self,
               Index     => Self.Sequencers_Stack.Widgets.Last_Index + 1,
               others    => <>);         begin
            Init_Button (B, 0.0, 0.0, 0.0, 40.0,
                         To_Unbounded_String ("Edit track"));
            Self.Sequencers_Stack.Add_Widget (B);
         end;
      end if;

      Self.Sequencers.Append (S);
      Self.Height := Self.Sequencers_Stack.Height;
      declare
         Seq_Widget : constant Base_Widget :=
           Self.Sequencers_Stack.Widgets.Last_Element;
         B : constant access Instrument_Button'Class := new Instrument_Button'
           (Seq_Group => Self,
            Index     => Self.Sequencers_Stack.Widgets.Last_Index,
            others    => <>);
      begin
         Init_Button (B, 0.0, 0.0, 0.0, Seq_Widget.Height);
         B.Text := To_Unbounded_String (S.Sequencer.Name);
         Self.Buttons_Stack.Add_Widget (B);
      end;
   end Add_Seq;

   -----------------
   -- Init_Button --
   -----------------

   procedure Init_Button
     (Button                       : access Simple_Button'Class;
      X, Y, Width, Height          : Float;
      Text                         : Unbounded_String := Null_Unbounded_String;
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
      Button.Text           := Text;
   end Init_Button;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Simple_Button; Ctx : access NVG_Context)
   is
      M : constant Float := 1.0;
      use Interfaces.C;
   begin
      Begin_Path (Ctx);
      Rounded_Rect
        (Ctx, Self.X + M, Self.Y + M,
         Self.Width - M * 2.0, Self.Height - M * 2.0, 5.0);
      Fill_Color
        (Ctx, (if Self.Is_Active
         then Self.Active_Color else Self.Inactive_Color));
      Fill (Ctx);

      Fill_Paint
        (Ctx, Linear_Gradient
           (Ctx, Self.X, Self.Y, Self.X, Self.Y + Self.Height,
            RGBA (255, 255, 255, 16),
            RGBA (0, 0, 0, 16)));
      Fill (Ctx);

      Begin_Path (Ctx);
      Rounded_Rect (Ctx, Self.X, Self.Y,
                    Self.Width - 1.0, Self.Height - 1.0, 5.0);
      Stroke_Color (Ctx, RGBA (0, 0, 0, 55));
      Stroke (Ctx);

      --  Text
      Font_Size (Ctx, 18.0);
      Font_Face (Ctx, "sans");
      Text_Align (Ctx, NVG_ALIGN_MIDDLE or NVG_ALIGN_CENTER);
      Text_Letter_Spacing (Ctx, -2.0);
      Fill_Color (Ctx, RGBA (200, 200, 200, 200));
      Text (Ctx, Self.X + Self.Width / 2.0,
            Self.Y + Self.Height / 2.0, To_String (Self.Text));
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

      Self.Seq_Group.Saved_Widgets := Self.Seq_Group.Main_Container.Widgets;
      Self.Seq_Group.Main_Container.Widgets := new Widget_Vectors.Vector;
      Self.Seq_Group.Main_Container.Widgets.Append (P);
      Self.Seq_Group.Main_Container.Widgets.Append (B);
   end Do_Action;

   ---------------
   -- Do_Action --
   ---------------

   overriding procedure Do_Action (Self : in out Piano_Roll_Button)
   is
      I : constant access I_Simulation_Listener
        := Self.Seq_Group.Sequencers (Self.Index).Sequencer;
      pragma Unreferenced (I);
      S : constant access Sequencer.Sequencer
        := Sequencer.Sequencer
          (Self.Seq_Group.Sequencers (Self.Index).Sequencer.all)'Access;

      B : constant access Back_To_Seq_Button'Class :=
        new Back_To_Seq_Button'(Seq_Group => Self.Seq_Group, others => <>);
   begin
      Init_Button
        (B, Self.Seq_Group.X + 5.0, Self.Seq_Group.Y + 5.0, 30.0, 30.0);

      Self.Seq_Group.Saved_Widgets := Self.Seq_Group.Main_Container.Widgets;
      Self.Seq_Group.Main_Container.Widgets := new Widget_Vectors.Vector;
      Self.Seq_Group.Main_Container.Widgets.Append
        (Piano_Roll
         (0.0, 50.0, 800.0, 430.0, S));
      Self.Seq_Group.Main_Container.Widgets.Append (B);
   end Do_Action;

   ---------------
   -- Do_Action --
   ---------------

   overriding procedure Do_Action (Self : in out Back_To_Seq_Button) is
   begin
      Self.Seq_Group.Main_Container.Widgets := Self.Seq_Group.Saved_Widgets;
   end Do_Action;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Scrolling_Container; Ctx : access NVG_Context)
   is
      M : constant Float := Self.Margin;
   begin
      if Self.BG_Color /= No_Color then
         Begin_Path (Ctx);
         Rect (Ctx, Self.X, Self.Y, Self.Width, Self.Height);
         Fill_Color (Ctx, Self.BG_Color);
         Fill (Ctx);
      end if;

      Scissor (Ctx, Self.X + M, Self.Y + M,
               Self.Width - M * 2.0, Self.Height - M * 2.0);
      for W of Self.Widgets.all loop
         W.Draw (Ctx);
      end loop;

      --  Draw up/down buttons
      Reset_Scissor (Ctx);
   end Draw;

   ---------------
   -- Scrolling --
   ---------------

   function Scrolling (X, Y, Width, Height : Float;
                       Internal_Widget     : access Base_Widget_Type'Class;
                       Margin              : Float := 0.0)
                       return not null access Scrolling_Container'Class
   is
   begin
      return Ret : constant not null access Scrolling_Container'Class
        := new Scrolling_Container'
          (X => X, Y => Y, Width => Width, Height => Height, Margin => Margin,
           BG_Color => No_Color, others => <>)
      do
         Ret.Up_B := new Scrolling_Button;
         Ret.Down_B := new Scrolling_Button;
         Init_Button (Ret.Up_B, 0.0, 0.0, 25.0, 60.0,
                      To_Unbounded_String ("^"));
         Init_Button (Ret.Down_B, 0.0, 0.0, 25.0, 60.0,
                      To_Unbounded_String ("v"));
         Ret.Up_B.Window := Ret;
         Ret.Down_B.Window := Ret;
         Ret.Up_B.Direction := Up;
         Ret.Down_B.Direction := Down;
         Ret.Widgets.Append (Internal_Widget);
         Ret.Widgets.Append (Base_Widget (Ret.Up_B));
         Ret.Widgets.Append (Base_Widget (Ret.Down_B));
         Ret.Layout;
      end return;
   end Scrolling;

   ------------
   -- Layout --
   ------------

   overriding procedure Layout (Self : in out Scrolling_Container) is
   begin
      --  Set attributes about the contained widget
      Self.Widgets.First_Element.X := Self.X + Self.Margin;
      Self.Widgets.First_Element.Y := Self.Y + Self.Margin - Self.Y_Offset;
      Self.Widgets.First_Element.Width :=
        Self.Width - Self.Margin * 3.0 - Self.Up_B.Width;
      Self.Widgets.First_Element.Layout;

      --  Set attributes about buttons
      Self.Up_B.X :=
        Self.X + (Self.Margin * 2.0) + Self.Widgets.First_Element.Width;
      Self.Down_B.X :=
        Self.X + (Self.Margin * 2.0) + Self.Widgets.First_Element.Width;
      Self.Up_B.Y := Self.Y + Self.Margin;
      Self.Down_B.Y := Self.Y + Self.Height - Self.Down_B.Height - Self.Margin;
   end Layout;

   ------------
   -- Layout --
   ------------

   overriding procedure Layout (Self : in out Container_Widget) is
   begin
      for W of Self.Widgets.all loop
         W.Layout;
      end loop;
   end Layout;

   ---------------
   -- Do_Action --
   ---------------

   overriding procedure Do_Action (Self : in out Scrolling_Button)
   is
      Off : constant Float := (Self.Window.Height - Self.Window.Margin * 2.0);
   begin
      Self.Window.Y_Offset :=
        Self.Window.Y_Offset +
          (if Self.Direction = Down then Off else -Off);
      Self.Window.Layout;
   end Do_Action;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Circular_Pot_Widget; Ctx : access NVG_Context)
   is
      HW : constant Float := Self.Width / 2.0;
      use Interfaces.C;
      Val_Str : String (1 .. 12);
   begin
      Save (Ctx);

      --  Background circle
      Begin_Path (Ctx);
      Circle (Ctx, Self.X + HW, Self.Y + HW, HW - (HW / 10.0));
      Stroke_Color (Ctx, RGBA (50, 51, 55, 255));
      Stroke_Width (Ctx, 7.0);
      Stroke (Ctx);

      --  Background arc
      Begin_Path (Ctx);
      Arc
        (Ctx, Self.X + HW, Self.Y + HW, HW - (HW / 10.0),
         CP_Min, CP_Max, CW);
      Stroke_Color (Ctx, RGBA (70, 71, 75, 255));
      Stroke_Width (Ctx, 7.0);
      Stroke (Ctx);

      --  Value arc
      Begin_Path (Ctx);
      Arc
        (Ctx, Self.X + HW, Self.Y + HW, HW - (HW / 10.0),
         CP_Min, Self.Current_Value
         * (CP_Max - CP_Min) + CP_Min, CW);

      Stroke_Color (Ctx, RGBA (191, 17, 107, 255));
      Stroke_Width (Ctx, 8.0);
      Stroke (Ctx);

      Stroke_Paint
        (Ctx, Linear_Gradient
           (Ctx, Self.X, Self.Y, Self.X + Self.Width, Self.Y + Self.Height,
            RGBA (0, 0, 0, 40), RGBA (255, 255, 255, 40)));
      Stroke_Width (Ctx, 8.0);
      Stroke (Ctx);

      --  Text
      Font_Size (Ctx, 18.0);
      Font_Face (Ctx, "sans");
      Text_Align (Ctx, NVG_ALIGN_BOTTOM or NVG_ALIGN_CENTER);
      Text_Letter_Spacing (Ctx, -2.5);
      Fill_Color (Ctx, RGBA (255, 255, 255, 255));

      if Self.Has_Mouse then
         --  When the pot has mouse, draw the text corresponding to the current
         --  value.
         Put (Val_Str, Self.G.Get_Value (Self.Val_Index), 2, 0);
         Text (Ctx, Self.X + Self.Width / 2.0,
               Self.Y + Self.Height, Val_Str);
      else
         --  Else, draw the name of the parameter Self corresponds to.
         Text (Ctx, Self.X + Self.Width / 2.0,
               Self.Y + Self.Height, Self.G.Get_Name (Self.Val_Index));
      end if;

      Restore (Ctx);
   end Draw;

   -----------------
   -- Mouse_Event --
   -----------------

   overriding procedure Mouse_Event
     (Self : in out Circular_Pot_Widget;
      Kind : Mouse_Event_Kind;
      X, Y : Float)
   is
      Total_Range : constant := 200.0;
   begin
      if Kind = Pressed then
         Take_Mouse (Mouse_Listener'(Self'Unrestricted_Access));
         Self.Has_Mouse := True;
         Self.Last_X := X;
         Self.X_Delta := 0.0;

         Self.Last_Y := Y;
         Self.Y_Delta := 0.0;
      elsif Kind = Moved and then Self.Has_Mouse then
         Self.X_Delta := X - Self.Last_X;
         Self.Last_X := X;

         Self.Y_Delta := Self.Last_Y - Y;
         Self.Last_Y := Y;

         Self.Current_Value :=
           Float'Max
             (Float'Min
                (Self.Current_Value +
                   ((Self.X_Delta + Self.Y_Delta)
                    / Total_Range), 1.0),
              0.0);

         Put_Line
           ("CUR VAL " & Self.Current_Value'Img);

         if Self.G /= null then
            Self.G.Set_Scaled_Value
              (Self.Val_Index, Scaled_Value_T (Self.Current_Value));
         end if;

      elsif Kind = Released and then Self.Has_Mouse then
         Self.Has_Mouse := False;
         Return_Mouse (Mouse_Listener'(Self'Unrestricted_Access));
      end if;
   end Mouse_Event;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Piano_Roll_Notes; Ctx : access NVG_Context)
   is
      Current_Y   : Float := Self.Y;
      Note_Height : constant Float := Self.Page_Height / 16.0;
      use Interfaces.C;
   begin
      Save (Ctx);
      for Octave in reverse 0 .. 8 loop
         for Scale in reverse Scale_Degree_T'Range loop
            Begin_Path (Ctx);
            Rect (Ctx, Self.X, Current_Y, Self.Width, Note_Height);

            if Degrees_Colors (Scale) = White then
               Fill_Color (Ctx, RGBA (255, 255, 255, 240));
            else
               Fill_Color (Ctx, RGBA (0, 0, 0, 240));
            end if;

            Fill (Ctx);

            if Scale = C then
               Font_Size (Ctx, 18.0);
               Font_Face (Ctx, "sans");
               Text_Align (Ctx, NVG_ALIGN_TOP or NVG_ALIGN_CENTER);
               Fill_Color (Ctx, RGBA (0, 0, 0, 255));
               Text (Ctx, Self.X + 30.0, Current_Y,
                     Note_Img (Note_T'(Scale, Octave_T (Octave))));
            end if;

            Current_Y := Current_Y + Note_Height;
         end loop;
      end loop;
      Restore (Ctx);
   end Draw;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Piano_Roll_Grid; Ctx : access NVG_Context)
   is
      Current_Y     : Float := Self.Y;
      Note_Height   : constant Float := Self.Page_Height / 16.0;
      Measure_Width : constant Float := Self.Width / Float (Self.Bars);
      Step_Width    : constant Float :=
        Measure_Width / Float (Self.Sub_Bars);
      Measure_X     : Float;

      function Get_Note_Y (N : Sequencer.Seq_Note) return Float is
        ((Float (8 - N.Note.Octave) * 12.0 +
            Float (Scale_Degree_T'Pos (Scale_Degree_T'Last) -
                       Scale_Degree_T'Pos (N.Note.Scale_Degree)))
         * Note_Height);
   begin
      Save (Ctx);
      for Octave in reverse 0 .. 8 loop
         for Scale in reverse Scale_Degree_T'Range loop
            Begin_Path (Ctx);
            Rect (Ctx, Self.X, Current_Y, Self.Width, Note_Height);

            if Degrees_Colors (Scale) = White then
               Fill_Color (Ctx, RGBA (48, 62, 66, 255));
            else
               Fill_Color (Ctx, RGBA (62, 81, 87, 255));
            end if;

            Fill (Ctx);

            Begin_Path (Ctx);
            Move_To (Ctx, Self.X, Current_Y);
            Line_To (Ctx, Self.X + Self.Width, Current_Y);
            Stroke_Color (Ctx, RGBA (0, 0, 0, 200));
            Stroke_Width (Ctx, 0.5);
            Stroke (Ctx);

            Current_Y := Current_Y + Note_Height;
         end loop;
      end loop;

      --  Draw grid

      for M in 0 .. Self.Bars loop
         Measure_X := Self.X + Measure_Width * Float (M);
         for N in 1 .. Self.Sub_Bars loop
            Begin_Path (Ctx);
            Move_To (Ctx, Measure_X + (Step_Width * Float (N)), Self.Y);
            Line_To (Ctx, Measure_X + (Step_Width * Float (N)), Current_Y);
            Stroke_Color (Ctx, RGBA (0, 0, 0, 100));
            Stroke_Width (Ctx, 0.5);
            Stroke (Ctx);
         end loop;

         Begin_Path (Ctx);
         Move_To (Ctx, Measure_X, Self.Y);
         Line_To (Ctx, Measure_X, Current_Y);
         Stroke_Color (Ctx, RGBA (0, 0, 0, 200));
         Stroke_Width (Ctx, 0.5);
         Stroke (Ctx);

         declare
            Playing_Bar_X : constant Float :=
              Float (Sample_Nb mod Self.Seq.Interval)
              / Float (Self.Seq.Interval)
              * Self.Width;
         begin
            Begin_Path (Ctx);
            Move_To (Ctx, Self.X + Playing_Bar_X, Self.Y);
            Line_To (Ctx, Self.X + Playing_Bar_X, Current_Y);
            Stroke_Color (Ctx, RGBA (255, 255, 255, 100));
            Stroke_Width (Ctx, 1.0);
            Stroke (Ctx);
         end;
      end loop;

      --  Draw notes

      for Note of Self.Seq.Notes loop
         declare
            Note_X     : constant Float :=
              Self.X +
                (Float (Note.Start) / Float (Self.Seq.Interval) * Self.Width);
            Note_Y     : constant Float := Self.Y + Get_Note_Y (Note);
            Note_Width : constant Float :=
              Float (Note.Duration) / Float (Self.Seq.Interval) * Self.Width;
         begin
            Begin_Path (Ctx);
            Rect (Ctx, Note_X, Note_Y, Note_Width, Note_Height);
            Fill_Color (Ctx, RGBA (171, 214, 129, 220));
            Fill (Ctx);

            Stroke_Color (Ctx, RGBA (0, 0, 0, 200));
            Stroke_Width (Ctx, 1.0);
            Stroke (Ctx);
         end;
      end loop;

      Restore (Ctx);
   end Draw;

   ------------
   -- Create --
   ------------

   function Create
     (Width       : Float;
      Page_Height : Float;
      Bars        : Natural;
      Sub_Bars    : Natural;
      Seq         : access Sequencer.Sequencer)
      return access Piano_Roll_Grid'Class
   is
      Note_Height   : constant Float := Page_Height / 16.0;
      R : constant access Piano_Roll_Grid'Class := new Piano_Roll_Grid'
        (X           => 0.0,
         Y           => 0.0,
         Width       => Width,
         Height      => 8.0 * 12.0 * Note_Height,
         Page_Height => Page_Height,
         Bars        => Bars,
         Sub_Bars    => Sub_Bars,
         Seq         => Seq);
   begin
      return R;
   end Create;

   ----------------
   -- Piano_Roll --
   ----------------

   function Piano_Roll
     (X, Y, Width, Height : Float;
      Seq                 : access Sequencer.Sequencer) return Base_Widget
   is
      T : constant access Horizontal_Stacked_Container :=
        new Horizontal_Stacked_Container'(X         => X,
                                          Y         => Y,
                                          Width     => Width,
                                          Height    => Height,
                                          Margin    => 0.0,
                                          Mode      => Take_Height,
                                          others    => <>);

      Controls : constant access Vertical_Stacked_Container'Class :=
        Create (0.0, 0.0, 50.0, 0.0, 5.0);

      Piano_Roll : constant access Horizontal_Stacked_Container :=
        new Horizontal_Stacked_Container'(X         => 0.0,
                                          Y         => 0.0,
                                          Width     => Width - 50.0,
                                          Height    => 0.0,
                                          Margin    => 0.0,
                                          others    => <>);

      PRN : constant access Piano_Roll_Notes :=
        new Piano_Roll_Notes'(0.0, 0.0, 50.0, 0.0, Height);

      PRG        : constant access Piano_Roll_Grid'Class :=
        Create (Width - 125.0, Height,
                Bars => 4, Sub_Bars => 4, Seq => Seq);

      Piano_Roll_Container : constant access Scrolling_Container :=
        Scrolling (0.0, 0.0, Width - 50.0, 0.0, Piano_Roll);
   begin
      T.Add_Widget (Controls);
      T.Add_Widget (Piano_Roll_Container);
      Piano_Roll.Add_Widget (PRN);
      Piano_Roll.Add_Widget (PRG);

      return Base_Widget (T);
   end Piano_Roll;

   -----------------
   -- Mouse_Event --
   -----------------

   pragma Warnings (Off);
   overriding procedure Mouse_Event
     (Self : in out Piano_Roll_Grid;
      Kind : Mouse_Event_Kind;
      X, Y : Float)
   is
   begin
      if Kind = Pressed then
         declare

            Note_Height   : constant Float := Self.Page_Height / 16.0;

            Rel_Y         : Float := Y - Self.Y;
            Note          : Natural :=
              (9 * 12) - Natural (Float'Ceiling (Rel_Y / Note_Height));
            Octave        : Octave_T := Octave_T (Note / 12);
            Degree        : Scale_Degree_T := Scale_Degree_T'Val (Note mod 12);

            Rel_X         : Float := X - Self.X;

            Note_Length   : Sample_Period :=
              Sample_Period (Self.Width / Float (Self.Bars)
                             / Float (Self.Sub_Bars) / Self.Width * Float (Self.Seq.Interval));

            Note_Start    : Sample_Period :=
              Sample_Period ((Rel_X / Self.Width)
                             * Float (Self.Seq.Interval));

         begin
            declare
               N : Sequencer.Seq_Note :=
                 Sequencer.Seq_Note'
                   (Note     => Note_T'(Degree, Octave),
                    Duration => Note_Length,
                    Start    => Note_Start / Note_Length * Note_Length);
               C : Sequencer.Notes_Sets.Cursor
                 := Self.Seq.Notes.Find (N);
            begin
               if Sequencer.Notes_Sets.Has_Element (C) then
                  Self.Seq.Remove_Note (N);
               else
                  Self.Seq.Add_Note (N);
               end if;
            end;

            Put_Line ("IN PIANO ROLL GRID MOUSE EVENT" & Img (X) & Img (Y));
            Put_Line ("REL Y " & Img (Rel_Y) & " NOTE " & Note'Img);
            Put_Line ("NOTE : " & Note_Img (Note_T'(Degree, Octave)));
         end;
      end if;
   end Mouse_Event;

end Widgets;
