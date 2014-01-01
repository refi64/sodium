function message: String;
begin
	message := 'Hello, world!';
end;

var
	s: String;
begin
	s := message();
	WriteLn(s);
end.
