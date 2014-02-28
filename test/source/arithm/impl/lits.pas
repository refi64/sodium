var
	a: Integer;
	b: Real;
begin
	a := 30 + -5 + +7;
	WriteLn(a);
	b := 30 + -5 - +7;
	b := b - (30.3 - -5.1 + +7.8);
	b := (-b) * 30.3e6 * -9.8e-3;
	WriteLn(b);
end.
