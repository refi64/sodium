function g(a: Integer): String;
begin
	if (a = 0) or (a = 1) then
		g := 'Nice choice!'
	else if a < 0 then
		g := 'You went negative'
	else
		g := 'WOW SUCH BIG NUMBER';
end;
var
	a: Integer;
	s: String;
begin
	ReadLn(a);
	s := g(a);
	WriteLn(s);
end.
