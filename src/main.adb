with Command;              use Command;
with Utils;                use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Effects;              use Effects;
with Waves;                use Waves;
with BLIT;                 use BLIT;
with Soundio;              use Soundio;
with Interfaces.C;         use Interfaces.C;
with Soundio_Output;       use Soundio_Output;
with Ada.Text_IO;          use Ada.Text_IO;

with Widgets;              use Widgets;

--  with Timing;               use Timing;
with Main_Support;         use Main_Support;
with Ada_NanoVG;           use Ada_NanoVG;
with Config;
with Ada.Real_Time;        use Ada.Real_Time;
with Rythmbox_Config;      use Rythmbox_Config;
with Rythmbox_Config_Support;      use Rythmbox_Config_Support;

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
       (16, BPM, 1,
        (K, o, o, K, o, o, K, o, o, o, B, o, o, o, o, o));
         --  K, o, o, K, o, o, K, o, o, o, B, o, o, o, o, o));

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
       (Nb_Steps => 16, BPM => BPM, Measures => 1,
        Notes    =>
          (o, o, o, o, Z, o, o, o, o, o, o, o, K, o, o, o));

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
                         Create_ADSR (0, 100, 10000, 0.1, Snare_Source),
                       others => <>))),
              0.1)
       ), Env => Create_ADSR (0, 100, 100, 0.2, Snare_Source));

   Hat_Seq    : constant access Simple_Sequencer :=
     Create_Sequencer
       (16, BPM, 1,
        Notes => (K, o, K, K, K, o, K, K, K, o, K, K, K, o, K, K));

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
       (16, BPM, 1,
        (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o));

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
             ), Env => Create_ADSR (0, 100, 1000, 0.3, Synth_Source)),
           Fixed (200.0,
             Modulator => new Attenuator'
               (Level  => 4500.0,
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
   Elapsed_Time         : Time_Span;
   Current_Time         : Time;

   ------------------------
   -- GRAPHICS VARIABLES --
   ------------------------

   W                   : aliased Widgets.Widget_Window;
   Ring_Buf            : constant FRB.Ring_Buffer := FRB.Create (2 ** 15);

   Play_Stop           : access RB_Play_Stop_Widget;
   Mon_W, Mon_H        : Natural;
   Ctx                 : access NVG_Context;

   procedure Mouse_Clicked_Callback (X, Y : Integer);
   procedure Mouse_Clicked_Callback (X, Y : Integer) is
   begin
      Mouse_Clicked (W'Access, Float (X), Float (Y));
   end Mouse_Clicked_Callback;

begin

   -----------------------
   -- SOUNDIO INIT PART --
   -----------------------

   Check_Error (Connect (IO));
   Flush_Events (IO);
   Default_Device_Index := Default_Output_Device_Index (IO);
   Device := Get_Output_Device (IO, Default_Device_Index);
   Out_Stream := Outstream_Create (Device);

   if Audio_Mode = Mode_Ring_Buf then
      Set_Ring_Buffer (Out_Stream, Ring_Buf, Main_Mixer);
   else
      Set_Generator (Out_Stream, Main_Mixer);
   end if;

   Out_Stream.Format := Format_Float32NE;
   Out_Stream.Write_Callback := Soundio_Output.Write_Callback'Access;

   Put_Line (IO.current_backend'Img);

   Ctx := Init (Mon_W, Mon_H, 800, 480);
   Set_Mouse_Clicked_Callback (Mouse_Clicked_Callback'Unrestricted_Access);

   -----------------
   -- TRACKS INIT --
   -----------------

   declare
      VSC : constant access Vertical_Stacked_Container :=
        Create (10.0, 100.0, Float (Mon_W) - 20.0, 10.0,
                BG_Color => RGBA (51, 51, 51, 255));
   begin
      Play_Stop := Create_Play_Stop (Float (Mon_W) / 2.0, 0.0);

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

   --  Start sound output

   if Audio_Mode = Mode_Ring_Buf then
      for Dummy in 0 .. 10 loop
         Write_Samples (Out_Stream);
      end loop;
   end if;

   Check_Error (Outstream_Open (Out_Stream));
   Check_Error (Outstream_Start (Out_Stream));

   Play_Stop.State := Stop;
   while not Should_Exit loop
      if Play_Stop.State = Play then
         Play (Out_Stream);
      else
         Stop (Out_Stream);
      end if;

      Poll_Events;
      Widgets.GUI_Sample_Nb := Sample_Nb;
--        Time_Start;
      Start_Frame;

      Set_Background_Color (RGBA (50, 50, 50, 255));
      Widgets.Draw (W, Ctx);
      End_Frame;
--        Time_End ("Drawing time : ");

      if Audio_Mode = Mode_Ring_Buf then
         Elapsed_Time := Clock - Current_Time;
         Current_Time := Clock;
         declare
            Compensate_Samples : constant Natural :=
              (if Drift_Level > 0 then 100 else 0);
         begin
            Write_Samples
              (Out_Stream,
               Natural (Duration (Config.SAMPLE_RATE)
                 * To_Duration (Elapsed_Time))
               + Compensate_Samples);
         end;
      end if;

   end loop;

   pragma Warnings (Off, "Unreachable");
   Outstream_Destroy (Out_Stream);
   Device_Unref (Device);
   Destroy (IO);

end Main;
