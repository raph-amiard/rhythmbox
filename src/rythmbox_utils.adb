with Ada.Text_IO; use Ada.Text_IO;
with Rythmbox_Config; use Rythmbox_Config;

package body Rythmbox_Utils is

   procedure Debug (Str : String; Cat : Debug_Category := General) is
   begin
      if Enabled_Debug_Cats (Cat) then
         Put_Line (Str);
      end if;
   end Debug;
end Rythmbox_Utils;
