var
	a: Integer;
begin
	ReadLn(a);
	case a of
		0, 1:
			WriteLn('Nice choice!');
		0-100..0-1:
			WriteLn('You went negative');
		2..200:
			WriteLn('WOW SUCH BIG NUMBER');
		else
			WriteLn('Please, stay in [-100..200] range');
	end;
end.
