// My own search, written 131210 while replacing TXN
// Small parts remain from SearchBuf
{@mode delphi}

unit BetterFindUnit;
interface

function MyFind(buf: AnsiString; searchString: AnsiString; aStart,endchar: Longint;
	findBackwards, ignoreCase, wholeWords: Boolean) : Longint;

implementation

// Stolen from strutils.pp
const
  { Default word delimiters are any character except the core alphanumerics. }
  WordDelimiters: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];
  
type
  TEqualFunction = function (const a,b : char) : boolean;

function EqualWithCase (const a,b : char) : boolean;
begin
  EqualWithCase := (a = b);
end;

function EqualWithoutCase (const a,b : char) : boolean;
begin
  EqualWithoutCase := (LowerCase(a) = LowerCase(b));
end;

function MyFind(buf: AnsiString; searchString: AnsiString; aStart,endchar: Longint;
	findBackwards, ignoreCase, wholeWords: Boolean) : Longint;
var
	found: Boolean;
	s, c: Longint;
	equal: TEqualFunction;

	function IsWholeWord (wordstart, wordend: Longint): Boolean;
	begin
		// Check start
		IsWholeWord := ((wordstart = 1) or (buf[wordstart-1] in worddelimiters)) and
		// Check end
					((wordend = Length(buf)) or (buf[wordend+1] in worddelimiters));
	end;

begin
	if not ignoreCase then
		equal := EqualWithCase
	else
		equal := EqualWithoutCase;

	if not findBackwards then
	begin
		if endchar = 0 then
			endchar := Length(buf);
		for c := aStart to endchar-Length(searchString)+1 do
		begin
			found := true;
			for s := 1 to Length(searchString) do
			begin
				if not equal(buf[c + s - 1], searchString[s]) then
				begin
					found := false;
					Break;
				end;
			end;
			if found = true then
			begin
				if not wholeWords then
				begin
					MyFind := c;
//					WriteLn('Found "', Copy(buf, c, Length(searchString)), '" at ', c);
					Exit(MyFind);
				end
				else
				if IsWholeWord(c, c+Length(searchString)-1) then
				begin
					MyFind := c;
//					WriteLn('Found whole word "', Copy(buf, c, Length(searchString)), '" at ', c);
					Exit(MyFind);
				end;
			end;
		end;
	end
	else
	begin // bw
		if endchar = 0 then
			endchar := 1;
		for c := aStart-Length(searchString) downto endchar do
		begin
			found := true;
			for s := 1 to Length(searchString) do
			begin
				if not equal(buf[c + s - 1], searchString[s]) then
				begin
					found := false;
					Break;
				end;
			end;
			if found = true then
			begin
				if not wholeWords then
				begin
					MyFind := c;
//					WriteLn('Found "', Copy(buf, c, Length(searchString)), '" at ', c);
					Exit(MyFind);
				end
				else
				if IsWholeWord(c, c+Length(searchString)-1) then
				begin
					MyFind := c;
//					WriteLn('Found whole word "', Copy(buf, c, Length(searchString)), '" at ', c);
					Exit(MyFind);
				end;
			end;
		end;
	end;
	MyFind := -1;
	WriteLn('Did not find "', searchString, '"');
end;

end.
