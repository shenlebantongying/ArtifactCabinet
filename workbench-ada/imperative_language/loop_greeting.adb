with Ada.Text_IO; use Ada.Text_IO;

procedure loop_greeting is
begin
    for I in reverse 1 .. 10 loop
        Put_Line ("Hello, " & Integer'Image (I));
        -- & -> concate strings
        -- Integer'Image -> convert int to str
        -- TODO: WHAT THE FUCK IS INTEGER'IMAGE???
    end loop;
end loop_greeting;