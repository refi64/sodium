var
	a: Integer;
begin
	ReadLn(a);
	if a > 0 then
		WriteLn('Positive!')
	else if a < 0 then
		WriteLn('You went negative')
	else if a = 0 then
		WriteLn('Neutral');
end.
