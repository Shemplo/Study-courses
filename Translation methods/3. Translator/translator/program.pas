program test;

var
	a, b, c : integer;
	
var s: string;
	
begin
	s := "Hello_world";
	write (s);
	
	s := "Enter_two_numbers:";
	write (s);
	
	read (a, b);
	
	a := a * 2;
	b := (a * 2) * (a + 3);
	
	if (a > b) then
	Begin
		if (b < 10) then
			a := b;
		else 
		if (b > 10) then
		begin
			a := b / 2;
		end;
		else
			a := 0;
	end;
	
	for c := (b + 1) * 2 downto 0 do
	begin
		a := a + c;
		while (a > 2) do
			a := a - 1;
	end;
	
	s := "Result:";
	write (s, a, b, c);
	
	a := 10; b := 3;
	s := "div_and_mod_test";
	write (s, a, b);
	
	c := div (a, b);;
	s := "div_a";
	write (s, c);
	
	c := mod (a, b);;
	s := "mod_a";
	write (s, c);
	
	a := 10;
	repeat
		write (a);
		a := a - 1;
	until a >= 0;

end.