unit SimpleParser;
{$mode macpas}

// Simple parser for general purpose parsing.
// Can only handle a few basic types.
// Can, unlike the color coder, find numerics.
// Scientific syntax (like 15E-2) not supported!
// Also supports Pascal-style hex code ($) and C-style (0x)
// By Ingemar 160107
// 160111: Added string support.
// 16040?: Minus can now be part of an alphanumeric string. (Bad for math expressions, good for file names.)

interface

const
	kNoToken = 0; // Nothing found!
	kOtherToken = 9; // Same number as the ColorCoding unit
	kAlphaNumericToken = 101; // Outside the current range for the ColorCoding unit
	kNumericToken = 102; // Outside the current range for the ColorCoding unit
	kSingleCharToken = 8; // Same number as the ColorCoding unit
	kHexToken = 103; // Outside the current range for the ColorCoding unit
	kStringToken = 6;

procedure SimpleParserGetToken(data: AnsiString; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString); overload;
procedure SimpleParserGetToken(data: AnsiString; var pos, tokenType: Longint; var tokenValue: AnsiString); overload;

implementation

// Variant that ignores the tokenStart and tokenEnd.
procedure SimpleParserGetToken(data: AnsiString; var pos, tokenType: Longint; var tokenValue: AnsiString); overload;
var
	tokenStart, tokenEnd: Longint;
begin
	SimpleParserGetToken(data, pos, tokenStart, tokenEnd, tokenType, tokenValue);
end;

procedure SimpleParserGetToken(data: AnsiString; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString); overload;
var
	s: AnsiString;
	bufferLength: Longint;
	hexFlag: Boolean;
const
	CR = Char(13);
	LF = Char(10);
	TAB = Char(9);
begin
	// Set defaults (150111)
	tokenValue := '';
	tokenType := kNoToken;
	tokenStart := pos;
	tokenEnd := pos;
	hexFlag := false;

	// Guard against bad input
	if Length(data) = 0 then
		Exit;
	if pos > Length(data) then
		Exit;
	bufferLength := Length(data);

	s := '';
	while (data[pos] in [CR, LF, TAB, ' ']) and (pos <= bufferLength) do pos := pos + 1;
	if pos > Length(data) then
		Exit;
	tokenStart := pos;
	// Check for leading minus! Or something else leading!
	if data[pos] = '-' then
	begin
		if pos+1 <= bufferLength then
			if data[pos+1] in ['0'..'9'] then
			begin
				pos := pos + 1;
			end;
	end
	else
	if data[pos] = '0' then // Can it be a 0x?
	begin
		if pos+1 <= bufferLength then
			if data[pos+1] = 'x' then
			begin
				pos := pos + 2;
				hexFlag := true;
			end;
	end
	else
	if data[pos] = '$' then // Can it be a 0x?
	begin
		pos := pos + 1;
		hexFlag := true;
	end
	else
	if data[pos] = '"' then
	begin
		// Scan until end ", skip \"
			pos := pos + 1;
			while not (data[pos] in ['"', CR, LF]) and (pos < bufferLength) do
			begin
				if data[pos] = '\' then
					pos := pos + 1;
				pos := pos + 1;
			end;
			tokenEnd := pos;
			pos := pos + 1; // Skip "
			tokenType := kStringToken;

			tokenStart := tokenStart; // + 1;
			tokenValue := Copy(data, tokenStart+1, tokenEnd - tokenStart-1);
			Exit(SimpleParserGetToken);
	end;

	// Parse the rest!
	// Check first character for numeric, alphanumeric or single char special
	if hexFlag then
	begin
		while (data[pos] in ['0'..'9', 'a'..'f', 'A'..'F']) and (pos <= bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		s := s + Copy(data, tokenStart, pos - tokenStart);
		
		tokenType := kHexToken;
		tokenValue := s;
	end
	else
	if data[pos] in ['0'..'9'] then
	begin
		while (data[pos] in ['0'..'9', '.']) and (pos <= bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		s := s + Copy(data, tokenStart, pos - tokenStart);
		
		tokenType := kNumericToken;
		tokenValue := s;
	end
	else
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
	// Change 160403: "-" now allowed as part of a token! Valid for file names. But this is clearly NOT useful for reading math expressions!
	// Should I make this optional?
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.', '-']) and (pos <= bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		s := Copy(data, tokenStart, pos - tokenStart);
		
		tokenType := kAlphaNumericToken;
		tokenValue := s;
	end
	else
	begin
		// Otherwise skip the symbol
		tokenEnd := tokenStart;
		tokenType := kSingleCharToken;
		tokenValue := data[pos];
		pos := pos + 1;
	end;
end;
end.
