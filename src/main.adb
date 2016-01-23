with Command; use Command;
with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Effects; use Effects;
with Waves; use Waves;
with BLIT; use BLIT;
with Soundio; use Soundio;
with Interfaces.C; use Interfaces.C;
with Soundio_Output; use Soundio_Output;

with Glfw.Windows;         use Glfw.Windows;
with Glfw.Windows.Context; use Glfw.Windows.Context;
with Ada_NanoVG;           use Ada_NanoVG;
with Glfw.Input;
with GL.Buffers;           use GL.Buffers;
with Glfw.Input.Mouse;
with GL.Toggles;           use GL.Toggles;
with GL.Blending;          use GL.Blending;
with GL.Window;
with GL.Types;             use GL.Types;
with Glfw.Windows.Hints;
with Widgets; use Widgets;
with Ada.Real_Time;
with Config;
with Glfw.Monitors; use Glfw.Monitors;
with Glfw.Errors;
with Main_Support; use Main_Support;

procedure Main is

   ------------------------
   -- RYTHMBOX VARIABLES --
   ------------------------

   BPM : constant := 120;

   o : constant Sequencer_Note := No_Seq_Note;
   K : constant Sequencer_Note := (Note => (G, 3), Duration => 3000);
   Z : constant Sequencer_Note := (Note => (G, 3), Duration => 5000);
   B : constant Sequencer_Note := (Note => (G, 3), Duration => 8000);

   Kick_Seq : constant access Simple_Sequencer :=
     Create_Sequencer
       (16, BPM, 2,
        (K, o, o, K, o, o, K, o, o, o, B, o, o, o, o, o,
         K, o, o, K, o, o, K, o, o, o, B, o, o, o, o, o));

   Kick_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Kick_Seq);

   Kick : constant access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Sine
              (Create_Pitch_Gen
                 (0, Kick_Source, Proc => LFO (6.0, 200.0))),
              0.1),

        2 => (Create_Sine (Create_Pitch_Gen
              (-24, Kick_Source,
                 Proc => new Attenuator'
                   (Level  => 300.0,
                    Source => Create_ADSR (0, 50, 10000, 0.1, Kick_Source),
                    others => <>))),

              0.7)
       ), Env => Create_ADSR (10, 1000, 200, 0.2, Kick_Source));

   Snare_Seq : constant access Simple_Sequencer :=
     Create_Sequencer
       (Nb_Steps => 16, BPM => BPM, Measures => 2,
        Notes    =>
          (o, o, o, o, Z, o, o, o, o, o, o, o, K, o, o, o,
           o, o, o, o, K, o, o, o, o, o, o, o, B, o, K, K));

   Snare_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Snare_Seq);
   Snare        : constant access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Noise, 0.5),
        2 => (Create_Sine
              (Create_Pitch_Gen
                 (5, Snare_Source,
                    Proc =>
                       new Attenuator'
                      (Level  => 300.0,
                       Source =>
                         Create_ADSR (0, 200, 10000, 0.5, Snare_Source),
                       others => <>))),
              0.1)
       ), Env => Create_ADSR (0, 100, 100, 0.2, Snare_Source));

   Hat_Seq    : constant access Simple_Sequencer :=
     Create_Sequencer
       (16, BPM, 2,
        Notes => (K, o, K, K, K, o, K, K, K, o, K, K, K, o, K, K,
                  K, o, K, K, K, o, K, K, K, o, K, K, K, o, K, K));

   Hat_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Hat_Seq);
   Hat        : constant access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Noise, 0.5)
       ), Env => Create_ADSR (0, 20, 0, 0.0, Hat_Source));

   pragma Warnings (Off, "referenced");
   SNL : constant Sample_Period := 4000;
   S1  : constant Sequencer_Note := ((C, 4), SNL);
   S2  : constant Sequencer_Note := ((F, 4), SNL);
   S3  : constant Sequencer_Note := ((D_Sh, 4), SNL);
   S4  : constant Sequencer_Note := ((A_Sh, 4), SNL);
   S5  : constant Sequencer_Note := ((G, 4), SNL);
   S6  : constant Sequencer_Note := ((D_Sh, 4), SNL);

   Synth_Seq : constant access Simple_Sequencer :=
     Create_Sequencer
       (8, BPM, 4,
        (S1, S1, S1, S1, S1, S2, S2, S2,
         S3, S3, S3, S3, S3, S4, S4, S4,
         S1, S1, S1, S1, S1, S2, S2, S2,
         S5, S5, S5, S5, S5, S6, S6, S6));

   Synth_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Synth_Seq);

   Synth : constant access Disto :=
     Create_Dist
       (Create_LP
          (Create_Mixer
             ((
              4 => (Create_Sine
                    (Create_Pitch_Gen
                         (-30, Synth_Source)), 0.6),
              3 => (BLIT.Create_Saw
                    (Create_Pitch_Gen
                         (-24, Synth_Source)), 0.3),
              2 => (BLIT.Create_Saw
                    (Create_Pitch_Gen
                         (-12, Synth_Source)), 0.3),
              1 => (BLIT.Create_Saw
                    (Create_Pitch_Gen
                         (-17, Synth_Source)), 0.5)
             )),
           Fixed (200.0,
             Modulator => new Attenuator'
               (Level  => 500.0,
                Source => Create_ADSR (10, 150, 200, 0.005, Synth_Source),
                others => <>)),
           0.2), 1.00001, 1.5);

   Main_Mixer : constant access Mixer :=
     Create_Mixer ((
                   (Kick, 0.5),
                   (Snare, 0.7),
                   (Hat, 0.6),
                   (Synth, 0.45)
                  ));

   -----------------------
   -- SOUNDIO VARIABLES --
   -----------------------

   IO                   : constant access Soundio.SoundIo := Create;
   Default_Device_Index : Interfaces.C.int;
   Device               : access SoundIo_Device;
   Out_Stream           : access SoundIo_Out_Stream;
   Dummy_Err            : SoundIo_Error;

   ----------------------
   -- NANOVG VARIABLES --
   ----------------------

   W                   : aliased Widgets.Widget_Window;
   Ctx                 : access NVG_Context;
   Width, Height       : Glfw.Size;
   FB_Width, FB_Height : Glfw.Size;
   MX, MY              : Glfw.Input.Mouse.Coordinate;

   Ring_Buf            : constant FRB.Ring_Buffer := FRB.Create (2 ** 14);

   use Ada.Real_Time;

   Current_Time        : Time := Clock;
   New_Time            : Time;
   pragma Unreferenced (New_Time);
   Elapsed_Time        : Time_Span;
   pragma Warnings (Off);

   Play_Stop           : access RB_Play_Stop_Widget;

   Monitor_Index : Positive := 1;
   Mon_W, Mon_H  : Interfaces.C.int;
begin

   -----------------------
   -- SOUNDIO INIT PART --
   -----------------------

   Dummy_Err := Connect (IO);
   Flush_Events (IO);
   Default_Device_Index := Default_Output_Device_Index (IO);
   Device := Get_Output_Device (IO, Default_Device_Index);
   Out_Stream := Outstream_Create (Device);
   Set_Ring_Buffer (Out_Stream, Ring_Buf, Main_Mixer);

   Out_Stream.Format := Format_Float32NE;
   Out_Stream.Write_Callback := Soundio_Output.Write_Callback'Access;

   Glfw.Init;

   declare
      VM : Video_Mode_List := Monitors (Monitor_Index).Video_Modes;
   begin
      Mon_W := VM (VM'Last).Width;
      Mon_H := VM (VM'Last).Height;
   end;

   -----------------
   -- TRACKS INIT --
   -----------------

   declare
      VSC : access Vertical_Stacked_Container :=
        Create (10.0, 100.0, Coord (Mon_W) - 20.0, 10.0,
                BG_Color => RGBA (0.2, 0.2, 0.2, 1.0));
   begin
      Play_Stop := Create_Play_Stop (Coord (Mon_W) / 2.0, 0.0);

      W.Widgets.Append (Play_Stop);

      W.Widgets.Append (VSC);
      VSC.Add_Widget
        (Widgets.Create
           (Snare_Seq, "Snare", 0.0, 0.0, 0.0, 50.0));

      VSC.Add_Widget
        (Widgets.Create
           (Hat_Seq, "Hat", 0.0, 0.0, 0.0, 50.0));

      VSC.Add_Widget
        (Widgets.Create
           (Kick_Seq, "Kick", 0.0, 0.0, 0.0, 50.0));

      VSC.Add_Widget
        (Widgets.Create
           (Synth_Seq, "Synth", 0.0, 0.0, 0.0, 50.0));
   end;

   Hints.Set_Client_API (OpenGL_ES);
   Hints.Set_Minimum_OpenGL_Version (2, 0);

   Glfw.Errors.Set_Callback (Main_Support.Error_Callback'Access);

   Init (W'Access,
         Glfw.Size (Mon_W), Glfw.Size (Mon_H),
         "NanoVG_Test", Monitors (Monitor_Index));
   Make_Current (W'Access);

   Ctx := Create_GLES2_Context
     ((Antialias => True, Stencil_Strokes => True, Debug => True));

   Set_Swap_Interval (0);
   Glfw.Set_Time (0.0);

   W.Enable_Callback (Callbacks.Key);
   W.Enable_Callback (Callbacks.Mouse_Button);

   --  Start sound output

   for Dummy in 0 .. 10 loop
      Write_Samples (Out_Stream);
   end loop;
   Current_Time := Clock;

   Dummy_Err := Outstream_Open (Out_Stream);
   Dummy_Err := Outstream_Start (Out_Stream);

   while not W.Should_Close loop
      W.Get_Size (Width, Height);
      W.Get_Framebuffer_Size (FB_Width, FB_Height);

      W.Get_Cursor_Pos (MX, MY);

      GL.Window.Set_Viewport (0, 0, Size (FB_Width), Size (FB_Height));
      Set_Color_Clear_Value ((0.13, 0.13, 0.13, 1.0));
      Clear ((Stencil => True, Color => True, Depth => True, others => False));

      Enable (GL.Toggles.Blend);
      Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);
      Enable (GL.Toggles.Cull_Face);
      Disable (GL.Toggles.Depth_Test);

      Begin_Frame (Ctx, Width, Height, 1.0);
      W.Draw (Ctx);

      Restore (Ctx);
      End_Frame (Ctx);

      Enable (GL.Toggles.Depth_Test);
      Swap_Buffers (W'Access);
      Glfw.Input.Poll_Events;

      if Play_Stop.State = Play then
         Play (Out_Stream);
      else
         Stop (Out_Stream);
      end if;

      Elapsed_Time := Clock - Current_Time;
      Current_Time := Clock;

      declare
         Compensate_Samples : Natural :=
           (if Drift_Level > 0 then 100 else 0);
      begin
         Write_Samples
           (Out_Stream,
            Natural (Duration (Config.SAMPLE_RATE)
              * To_Duration (Elapsed_Time))
              + Compensate_Samples);
      end;

      delay 0.01;
   end loop;

   pragma Warnings (Off, "Unreachable");
   Outstream_Destroy (Out_Stream);
   Device_Unref (Device);
   Destroy (IO);

end Main;
