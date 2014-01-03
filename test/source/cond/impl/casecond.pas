function g(x: Integer): String;
begin
	case x of
		0, 1:
			g := 'Nice choice!';
		0-100..0-1:
			g := 'You went negative';
		2..200:
			g := 'WOW SUCH BIG NUMBER';
		else
			g := 'Please, stay in [-100..200] range';
	end;
end;

var
	a: Integer;
	s: String;
begin
	ReadLn(a);
	s := g(a);
	WriteLn(s);
end.
