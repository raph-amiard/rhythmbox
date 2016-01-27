with Ada.Float_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Timing is
   procedure Time_Start is
   begin
      Current_Time := Clock;
   end Time_Start;

   procedure Time_End (Label : String) is
   begin
      Elapsed_Time := Clock - Current_Time;
      Put (Label & " - ");
      Ada.Float_Text_IO.Put (Float (To_Duration (Elapsed_Time)), 2, 2, 0);
      Put_Line ("");
   end Time_End;

end Timing;
