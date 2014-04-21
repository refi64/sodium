var
	x: integer;
begin
	x := 0;
	begin
		x := x + 1;
		begin
			x := x + 2;
			begin
				x := x + 3;
			end;
			x := x + 4;
		end;
		x := x + 5;
	end;
	WriteLn(x);
end.
