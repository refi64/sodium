function g(x: Integer): String;
begin
	case x + 1 of
		0, 1:
			g := 'Nice choice!';
		-100..-1:
			case x - 1 of
				0: g := 'Impossible!';
				else  g := 'You went negative';
			end;
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
