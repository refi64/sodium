var
	name: String;
	age: Integer;
begin
	WriteLn('What is your ''name?''');
	ReadLn(name);
	WriteLn('How old are you?');
	ReadLn(age);
	WriteLn('Pleased to meet you, ', name
	       , ' of age ', age);
end.
