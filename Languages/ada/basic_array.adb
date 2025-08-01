with Ada.Text_IO;

procedure basis_array is
    type My_Int is range 0 .. 100;
    type My_Index is range 3 .. 5;
    --                     ^ lower bound can be greater than 3 ?
    type My_Array is array (My_Index) of My_Int;
    SLB_Array : My_Array := (1,2,3);
begin
    for I in My_Index loop
        Ada.Text_IO.Put (My_Int'Image (SLB_Array(I)));
    end loop;
    Ada.Text_IO.New_Line;

    -- The above one is bad, because you have to specify the Index by hand

    for I in SLB_Array'First .. SLB_Array'Last-1 loop
               Ada.Text_IO.Put (My_Int'Image (SLB_Array(I)));
               -- => 1 2, the last-1 means the last but one.
    end loop;
    Ada.Text_IO.New_Line;
end basis_array;
