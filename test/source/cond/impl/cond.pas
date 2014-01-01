var
	a: Integer;
begin
	ReadLn(a);
	if (a = 0) or (a = 1) then
		WriteLn('Nice choice!')
	else if a < 0 then
		WriteLn('You went negative')
	else
		WriteLn('WOW SUCH BIG NUMBER');
end.
