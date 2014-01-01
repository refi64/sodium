function message: String;
begin
	message := 'Hello, pure function!';
end;

var
	s: String;
begin
	s := message();
	WriteLn(s);
end.
