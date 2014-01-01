function factorial(n: Integer): Integer;
var
	i: Integer;
begin
	factorial := 1;
	for i := 1 to n do
		factorial := factorial * i;
end;

var
	n: Integer;
begin
	ReadLn(n);
	n := factorial(n);
	WriteLn(n);
end.
