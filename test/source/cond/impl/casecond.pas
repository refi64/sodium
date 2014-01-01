var
	a: Integer;
begin
	ReadLn(a);
	case a of
		0, 1:
			WriteLn('Nice choice!');
		-1:
			WriteLn('You went negative');
		2:
			WriteLn('WOW SUCH BIG NUMBER');
		else
			WriteLn('Please, stay in [-1..2] range');
	end;
end.
