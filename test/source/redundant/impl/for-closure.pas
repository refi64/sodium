function f(n: LongInt): LongInt;
var
	i, b: LongInt;
begin
	f := 1;
	b := 3;
	for i := 1 to n do begin
		begin
			f := f * i + b;
		end;
	end;
end;

var
	n: LongInt;
begin
	n := 7;
	n := f(n);
	WriteLn(n);
end.
