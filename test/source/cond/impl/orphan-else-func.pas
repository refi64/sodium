function g(a: Integer): String;
begin
	g := '';
	if a > 0 then
		g := 'Positive!'
	else if a < 0 then
		g := 'You went negative';
end;
var
	a: Integer;
	s: String;
begin
	ReadLn(a);
	s := g(a);
	WriteLn(s);
end.
