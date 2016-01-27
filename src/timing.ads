with Ada.Real_Time; use Ada.Real_Time;

package Timing is
   Current_Time        : Time := Clock;
   Elapsed_Time        : Time_Span;

   procedure Time_Start;
   procedure Time_End (Label : String);
end Timing;
