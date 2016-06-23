// Parser for "backtrace full" in GDB
// ParseBacktrace parses the "backtrace full" command for the main variables lists
// ParsePrint parses the reply of "print" for a variable, used for the single variable view (view 2) in LightBugs
// Some cleanup needed
// Important improvement 150727: ParsePrint improved to display class members properly.

{$mode fpc}

unit Backtrace;
interface

uses
	LWPGlobals, UtilsTypes, // for StringArr
	BetterFindUnit, SimpleParser; // for matching case

type
	VarInfoRec = record
		name, displayname: AnsiString; // "name" is what LLDB wants and "displayname" is what the user wants to see
		value: AnsiString;
		varType: AnsiString;
		typeId: Longint; // array/pekare/record borde noteras
		frameNumber: Longint;
	end;
	OutArrType = array of VarInfoRec;
//	StringArr = array of String;

// Type numbers
const
	kScalarOrUnknown = 0;
	kArray = 1;
	kPointer = 2;
	kRecord = 3;
	kObject = 4;
	kStringPointer = 5;

procedure ParseBacktrace(backTrace: AnsiString; var outArr: OutArrType; var frameNames: StringArr);
function ParseVariable(s: AnsiString): AnsiString;
function ParseExpandVariable(s: AnsiString): AnsiString;
function GetAHexValue(s: AnsiString): AnsiString;
procedure ParsePrint(backTrace: AnsiString;
									var outArr: OutArrType;
									var scalarValue: AnsiString);
procedure ParsePrintLLDB(backTrace: AnsiString;
									var outArr: OutArrType;
									var scalarValue: AnsiString);
procedure RecordFingerPosition(editIndex, lineNumber: Longint);

// Needed by ParseVariablesLLDB!
procedure MatchCase(var theString: AnsiString);

implementation
	
const
	kUnknownToken = 0;
	kFrameNumberToken = 1;
	kStartBracketToken = 2;
	kEndBracketToken = 3;
	kHexToken = 4;
	kNumToken = 5;
	kStringToken = 6;
	kEqualToken = 7;
	kPointerToken = 8;
	kStartParenToken = 9;
	kEndParenToken = 10;
	kCommaToken = 11;
	kCRLFToken = 12;
	kInheritedToken = 13;
	
	CR = Char(13);
	LF = Char(10);
	TAB = Char(9);
var
	tokenName: array[0..13] of String = ('U',
						'Frame number',
						'Start bracket',
						'End bracket',
						'Hex',
						'Number',
						'String',
						'Equal',
						'Pointer',
						'Start paren',
						'End paren',
						'Comma',
						'Line break',
						'Inherited');

var
	bracketLevel, inheritedLevel: Longint;
	gCurrentEditIndex: Longint;
	gCurrentLineNumber: Longint;
	isInherited: array[0..100] of Boolean;

procedure RecordFingerPosition(editIndex, lineNumber: Longint);
begin
	gCurrentEditIndex := editIndex;
	gCurrentLineNumber := lineNumber;
end;

// Search the file case insensitive for a match and copy the found data.
procedure MatchCase(var theString: AnsiString);
var
	found: Longint;
begin
// Search for theString in the text of teEdit[gCurrentEditIndex] within the current function.
// Or current file just as a first test.
	if gCurrentEditIndex < 0 then
		Exit;
	if teEdit[gCurrentEditIndex] = nil then Exit;
	
	found := MyFind(teEdit[gCurrentEditIndex]^.text, theString,
		1, Length(teEdit[gCurrentEditIndex]^.text), // Range - should be limited
		false, true, true);
	if found > -1 then
		theString := Copy(teEdit[gCurrentEditIndex]^.text, found, Length(theString));
end;

procedure GetToken(s: AnsiString; var pos: Longint; var tokenType: Longint; var tokenValue: AnsiString);
var
	tokenStart: Longint;
	tempPos: Longint;
	tempTokenType: Longint;
	tempTokenValue: AnsiString;
begin
	tokenValue := '***INVALID***';
	if pos > Length(s) then
	begin
		tokenType := kUnknownToken;
		tokenValue := '';
		Exit;
	end;
	// if new row? bracketLevel = 0?
		while pos <= Length(s) do
			if s[pos] in [TAB, ' '] then
				pos += 1
			else
				break;
	if s[pos] in [CR, LF] then
	begin
		tokenType := kCRLFToken;
		tokenValue := s[pos];
		pos += 1;
		Exit;
	end;
	if s[pos] = '#' then // Egentligen bara efter ny rad!
	begin // frame number token
		// Get numeric value
		pos += 1;
		tokenStart := pos;
		tokenType := kFrameNumberToken;
		while pos <= Length(s) do
			if s[pos] in ['0'..'9'] then
				pos+=1
			else
				break;
		tokenValue := Copy(s, tokenStart, pos-tokenStart);
		Exit;		
	end;
	if s[pos] = '''' then
	begin
		pos += 1;
		tokenStart := pos;
		tokenType := kStringToken;
		while pos <= Length(s) do
			if s[pos] = '''' then
			begin
			// '' not supported yet
				tokenValue := Copy(s, tokenStart, pos-tokenStart);
				pos += 1;
				Exit;
			end
			else
				pos += 1;
		// End of data - error
		tokenValue := Copy(s, tokenStart, pos-tokenStart);
		tokenType := kUnknownToken;
		Exit;		
	end;
	
	if s[pos] = '<' then
		if pos < Length(s) then
			if s[pos+1] = '>' then
			begin
				pos += 2;
				tokenValue := '<>';
				tokenType := kInheritedToken;
				inheritedLevel += 1; // Levels to ignore
				isInherited[bracketLevel+1] := true; // Next level is inherited data!
				Exit;
			end;
	
	if s[pos] = '{' then
	begin
		bracketLevel += 1;
		tokenType := kStartBracketToken;
		tokenValue := '{';
		pos += 1;
		Exit;
	end;
	if s[pos] = '}' then
	begin
		if isInherited[bracketLevel] then inheritedLevel -= 1;
		isInherited[bracketLevel] := false;
		bracketLevel -= 1;
		tokenType := kEndBracketToken;
		tokenValue := '}';
		pos += 1;
		Exit;
	end;
	if s[pos] = '(' then
	begin
		bracketLevel += 1;
		tokenType := kStartParenToken; // kStartParenToken?
		tokenValue := '(';
		pos += 1;
		Exit;
	end;
	if s[pos] = ')' then
	begin
		bracketLevel -= 1;
		tokenType := kEndParenToken; // kEndParenToken?
		tokenValue := ')';
		pos += 1;
		Exit;
	end;
	if s[pos] = '[' then
	begin
		bracketLevel += 1;
		tokenType := kStartBracketToken; // kStartParenToken?
		tokenValue := '[';
		pos += 1;
		Exit;
	end;
	if s[pos] = ']' then
	begin
		bracketLevel -= 1;
		tokenType := kEndBracketToken; // kEndParenToken?
		tokenValue := ']';
		pos += 1;
		Exit;
	end;
	if s[pos] = '=' then
	begin
		tokenType := kEqualToken;
		tokenValue := '=';
		pos += 1;
		Exit;
	end;
	if s[pos] = '^' then
	begin
		tokenType := kPointerToken;
		tokenValue := '^';
		pos += 1;
		Exit;
	end;
	if s[pos] = ',' then
	begin
		tokenType := kCommaToken;
		tokenValue := ',';
		pos += 1;
		Exit;
	end;
	if s[pos] in ['0'..'9', '-', '.'] then
	begin
		tokenStart := pos;
		while pos <= Length(s) do
			if s[pos] in ['0'..'9', 'e', 'E', 'x', 'a', 'b', 'c', 'd', 'f', '-', '.'] then
				pos+=1
			else
				break;
		tokenValue := Copy(s, tokenStart, pos-tokenStart);
		tokenType := kNumToken;
		if Length(tokenValue) > 1 then
			if (tokenValue[1] = '0') and (tokenValue[2] = 'x') then // 0x = C-style hex
			begin
				tokenType := kHexToken;
				// Kolla direkt om det är en AnsiString/strängpekare?
				// Smartare - förenklar på flera ställen!
				tempPos := pos;
				GetToken(s, tempPos, tempTokenType, tempTokenValue);
				if tempTokenType = kStringToken then
				begin
					tokenType := kStringToken;
					pos := tempPos;
					tokenValue := tempTokenValue;
				end;
			end
			else
				tokenType := kNumToken;
		Exit;
	end;
	// Unknown
	tokenStart := pos;
	while pos <= Length(s) do
		if not (s[pos] in [' ', TAB, CR, LF, '}', ')', ']', '=']) then
			pos+=1
		else
			break;
	tokenValue := Copy(s, tokenStart, pos-tokenStart);
	tokenType := kUnknownToken;

end;

procedure PreviewToken(backTrace: AnsiString; pos: Longint; var tokenType: Longint; var tokenValue: AnsiString);
var
	oldBracketLevel: Longint;
begin
	oldBracketLevel := bracketLevel;
	GetToken(backTrace, pos, tokenType, tokenValue);
	bracketLevel := oldBracketLevel;
end;

procedure GoToEndOfLine(var pos: Longint; backTrace: AnsiString);
var
	inString: Boolean;
	stringChar: Char;
begin
	inString := false;
	while pos <= Length(backTrace) do
		if backTrace[pos] in [CR, LF] then
			Exit
		else
		begin
			if backTrace[pos] in ['''', '"'] then
			begin
				if not inString then
				begin
					inString := not inString;
					stringChar := backTrace[pos];
				end
				else
					if backTrace[pos] = stringChar then
						inString := not inString;
			end;
			if not inString then
			begin
				if backTrace[pos] in ['(', '{', '['] then
					bracketLevel += 1;
				if backTrace[pos] in [')', '}', ']'] then
					bracketLevel -= 1;
			end;
			pos += 1;
		end;
end;

procedure ParseBacktrace(backTrace: AnsiString;
									var outArr: OutArrType;
									var frameNames: StringArr);
var
	pos, tokenType, i: Longint;
	tokenValue, tokenValue2, variableName: AnsiString;
	currentFrame: Longint;
	oldpos, oldbracketlevel: Longint;

	procedure OutputVariable(variableName, tokenValue: AnsiString; typeId: Longint);
	begin
		SetLength(outArr, Length(outArr)+1);

		outArr[High(outArr)].name := variableName;
		MatchCase(variableName); // TEST!
		outArr[High(outArr)].displayname := variableName;

		outArr[High(outArr)].value := tokenValue;
		outArr[High(outArr)].typeId := typeId;
		outArr[High(outArr)].frameNumber := currentFrame;
		WriteLn('Outouts ', variableName);
	end;

	procedure ParseArguments(var pos: Longint; backTrace: AnsiString);
	var
		tokenType: Longint;
		tokenValue, variableName: AnsiString;
	begin
		WriteLn('*Entering ParseArguments*');
		WriteLn('ParseArguments got: ', backTrace);
		GetToken(backTrace, pos, tokenType, tokenValue);
		if tokenType <> kStartParenToken then
			WriteLn('Error, missing "("?');
		// Get ( token
		while pos <= Length(backTrace) do
		begin
			GetToken(backTrace, pos, tokenType, tokenValue);
			if tokenType = kEndParenToken then // End of arguments
			begin
				GoToEndOfLine(pos, backTrace); // Rest of line tells file and line number
				WriteLn('*End of ParseArguments from kEndParenToken*');
				Exit;
			end;
			if tokenType = kUnknownToken then // variable name
			begin
				variableName := tokenValue;
				GetToken(backTrace, pos, tokenType, tokenValue); // Should be =
				GetToken(backTrace, pos, tokenType, tokenValue); // Value/address
				if tokenType = kNumToken then // Save as value
				begin
					// Scalar variable!
					WriteLn('Found scalar ', variableName, ' with value ', tokenValue);
					OutputVariable(variableName, tokenValue, kScalarOrUnknown);
				end
				else
				if tokenType = kHexToken then // addess - might be followed by string!
				begin
						WriteLn('Found pointer ', variableName, ' followed by ', tokenValue);
						OutputVariable(variableName, '(pointer)', kPointer);
				end
				else
				if tokenType = kStartBracketToken then // array or record
				begin // Next is either value or unknown
					// Save as array or record
					WriteLn('Found arr/rec ', variableName);
					OutputVariable(variableName, '(array or record)', kArray); // or kRecord
					while bracketLevel > 1 do
					GetToken(backTrace, pos, tokenType, tokenValue); // Skip to end of array/record
					// Decide array or record here!
				end
			end
			else
			if tokenType = kCommaToken then
				WriteLn('Comma found')
			else
				WriteLn('Unexpected ', tokenName[tokenType]);
		end;
		// Get variables until ) is found
		// separated by comma
		WriteLn('End of ParseArguments from end*');
	end;

begin
// #0 = stack frame, ev med argument
// No locals. = ignoreras
// Annat = lokala variabler
// NAMN = 
//				{ = array eller record, följ till }
//				0x00000 'zzzzz' = sträng
//				000000 = numerisk skalär
//				(^TYP) 0x0000 = pekare - kan vara komplex med fler parenteser

	currentFrame := 0;
	bracketLevel := 0;
	inheritedLevel := 0;
	pos := 1;
	SetLength(outArr, 0);
	SetLength(frameNames, 0);
	while pos <= Length(backTrace) do
	begin
		GetToken(backTrace, pos, tokenType, tokenValue);
		if bracketLevel = 0 then
		begin
			case tokenType of
				kFrameNumberToken:
				begin
					GetToken(backTrace, pos, tokenType, tokenValue2);
					if tokenType = kHexToken then
					begin
						GetToken(backTrace, pos, tokenType, tokenValue2); // in
						GetToken(backTrace, pos, tokenType, tokenValue2); // name
					end;
					WriteLn('Frame ', tokenValue, ' with name ', tokenValue2);
					
					// Output frame
					SetLength(frameNames, Length(frameNames)+1);
					frameNames[High(frameNames)] := tokenValue2;
					Val(tokenValue2, currentFrame);
					// Parsa argument här
					ParseArguments(pos, backTrace);
					bracketLevel := 0; // BUG!!! Should be fixed better.
					GoToEndOfLine(pos, backTrace);
				end;
				kUnknownToken:
				begin
					// Keep variable name
					variableName := tokenValue;
					// Get =
					GetToken(backTrace, pos, tokenType, tokenValue);
					if tokenType <> kEqualToken then
					begin
						if (tokenValue = 'locals.') or (tokenValue = 'symbol') then
							WriteLn('No-line')
						else
							WriteLn('Error: Unknown format (', tokenValue, ' for ', variableName,')');
						GoToEndOfLine(pos, backTrace);
					end
					else
					begin
						// Get number/"(", "{"
						GetToken(backTrace, pos, tokenType, tokenValue);
						case tokenType of
							kStringToken:
							begin
								WriteLn(variableName, ' is a string with value ', tokenValue);
								GoToEndOfLine(pos, backTrace);
								OutputVariable(variableName, tokenValue, kStringPointer); // ej pointer, plain string?
							end;
							kNumToken: // Plain number
							begin
								WriteLn(variableName, ' is a number with value ', tokenValue);
								OutputVariable(variableName, tokenValue, kScalarOrUnknown);
							end;
							kHexToken:
							begin
									WriteLn(variableName, ' is a pointer ', tokenValue);
									GoToEndOfLine(pos, backTrace);
									OutputVariable(variableName, '(pointer)', kPointer);
							end;
							kStartParenToken: // Pointer
							begin
								WriteLn(variableName, ' is a pointer');
								GoToEndOfLine(pos, backTrace);
								OutputVariable(variableName, '(pointer)', kPointer);
							end;
							kStartBracketToken: // array/record
							begin
//								Write(variableName, ' is an array or record. ');
								
								// Peek on next token
								PreviewToken(backTrace, pos, tokenType, tokenValue);
								if (tokenType = kStartBracketToken) or (tokenType = kNumToken) then
								begin
									WriteLn(variableName, ' is an array');
									OutputVariable(variableName, '(array)', kArray); // or kRecord
								end
								else
								begin
									WriteLn(variableName, ' is a record');
									OutputVariable(variableName, '(record)', kRecord); // or kRecord
								end;
								
								GoToEndOfLine(pos, backTrace);
//								OutputVariable(variableName, '(array or record)', kArray); // or kRecord
								// Check next to decide!
							end;
							kUnknownToken:
							begin
								if (variableName = 'locals.') or (variableName = 'symbol') then
									WriteLn('No-line') // Should never happen
								else
									WriteLn('Unknown format');							
							end;
						end; // inner case
					end;
//					kStartParenToken:
//					begin
//						GetToken(backTrace, pos, tokenType, tokenValue2);
//						if tokenType = kPointerToken then
//							WriteLn(variableName, ' is a pointer');
//					end;
				end; // kUnknownToken
			end;
			// Flera fall:
			// Frame nr + function name+ args (#1 FUNCTION(A=0))
			// Variable name + value (A = 0)
			// Variable name + (^ : pointer
			// Variable name + { : array or record
			// No : "No locals" eller "No symbol table info"
		end;

//		while (pos <= Length(backTrace)) and (bracketLevel > 0) do
//			GetToken(backTrace, pos, tokenType, tokenValue)
	
// Output
// array of array of somedatarecord
	end;
end; // ParseBacktrace

// Parse a single variable, global/print style. Can be several lines!
// Output to match variables list - brief!
function ParseVariable(s: AnsiString): AnsiString;
var
	pos, tokenType, tokenType2, i: Longint;
	tokenValue, tokenValue2, tokenValue3, variableName: AnsiString;
//	info: VarInfoRec;
	infovalue: AnsiString;
begin
	pos := 1;
//	info.name := '';
	infovalue := '';
//	info.typeId := -1;
	GetToken(s, pos, tokenType, tokenValue); // Should be $number
//	GetToken(s, pos, tokenType, tokenValue2); // Should be number
//	GetToken(s, pos, tokenType2, tokenValue3); // Should be =

//	if Copy(tokenValue, 1, 1) <> '$' then // $
	if Length(tokenValue) > 0 then
	if tokenValue[1] <> '$' then // $
	begin
//		info.name := '(none)';
		infovalue := '(parse error)' + tokenValue;
//		info.typeId := -1;
		Exit(infovalue);
	end;
//	info.name := variableName;
	GetToken(s, pos, tokenType, tokenValue); // Should be =
	GetToken(s, pos, tokenType, tokenValue); // Number, hex, { - CAN ALSO BE ' FOR A STRING!
//	WriteLn('tokenValue = ', tokenvalue);
	if tokenType = kNumToken then
	begin
		infovalue := tokenValue;
//		info.typeId := 0; // Number
		Exit(infovalue);
	end;
	if tokenType = kHexToken then // Pointer
	begin
		tokenValue := '(pointer)';
		infovalue := tokenValue;
//		info.typeId := 0; // pointer
		Exit(infovalue);
	end;
	if tokenType = kStartBracketToken then // Pointer or string
	begin
		GetToken(s, pos, tokenType, tokenValue); // number or {: array. unknown: record
		if tokenType = kUnknownToken then
			infovalue := '(record)'
		else
			infovalue := '(array)';
//		info.typeId := 0; // String
		Exit(infovalue);
	end;
	Exit(infovalue);
end;

// Parse a single variable, global/print style. Can be several lines!
// Output for expanded view, may also be several lines.
// Pump straight through with some filtering!
// {} -> ()
// Hex numbers: Removed
// = removed if nothing useful follows
function ParseExpandVariable(s: AnsiString): AnsiString;
var
	pos, tokenType, i, prevTokenType: Longint;
	tokenValue, tokenValue2, variableName, resultString: AnsiString;
begin
	tokenType := -1; // Illegal, nonexistent
	resultString := '';
	pos := 1;
	
	// Skip start
	GetToken(s, pos, tokenType, tokenValue); // Should be $number
	GetToken(s, pos, tokenType, tokenValue2); // Should be =

				GetToken(s, pos, tokenType, tokenValue);
				if tokenType = kHexToken then
				begin
						tokenValue := '$' + Copy(tokenValue, 3, Length(tokenValue)-2) + tokenValue2;
				end
				else
				begin
					if tokenValue = '{' then
						tokenValue := '(';
				end;
				resultString := tokenValue;

	while pos <= Length(s) do
	begin
		prevTokenType := tokenType;
		GetToken(s, pos, tokenType, tokenValue);
		if tokenType = kHexToken then
		begin
			// Ignore
		end
		else
		begin
			if tokenValue = '{' then
				tokenValue := '(';
			if tokenValue = '}' then
				tokenValue := ')';
(*			if tokenValue = '=' then
			begin
				GetToken(s, pos, tokenType, tokenValue);
				if tokenType = kHexToken then // Next is : or string!
				begin
					GetToken(s, pos, tokenType, tokenValue);
					if tokenType = kStringToken then
						tokenValue := ' = ''' + tokenValue + ''''
					else
						tokenValue := ':';
				end
				else
				begin
					if tokenValue = '{' then
						tokenValue := '(';
					tokenValue := ' = ' + tokenValue;
				end;
			end;*)
			if tokenValue <> ':' then
			if tokenType = kUnknownToken then
				if prevTokenType = kUnknownToken then
					tokenValue := ' ' + tokenValue;
			resultString += tokenValue;
		end;
	end;
	Exit(resultString);
end;

function GetAHexValue(s: AnsiString): AnsiString;
var
	pos, tokenType: Longint;
	tokenValue: AnsiString;
begin
	pos := 1;
	while pos <= Length(s) do
	begin
		GetToken(s, pos, tokenType, tokenValue);
		if tokenType = kHexToken then
			Exit(tokenValue);
	end;
	Exit('');
end;


// Parses the reply from "print", called when a known variable should be
// displayed in field 2 (zoomed view).
procedure ParsePrint(backTrace: AnsiString;
									var outArr: OutArrType;
									var scalarValue: AnsiString);
var
	pos, tokenType, i: Longint;
	tokenValue, tokenValue2, variableName: AnsiString;
	currentFrame: Longint;
	oldpos, oldbracketlevel: Longint;

	procedure OutputVariable(variableName, tokenValue: AnsiString; typeId: Longint);
	begin
		SetLength(outArr, Length(outArr)+1);
		outArr[High(outArr)].name := variableName;
		outArr[High(outArr)].value := tokenValue;
		outArr[High(outArr)].typeId := typeId;
		outArr[High(outArr)].frameNumber := currentFrame; // Irrelevant
		WriteLn('Outputs "', variableName, '"');
	end;

// $xx = ...
// Skalär: Värde
// Ansisträng: hex + värde
// Array eller record: {}, parsa innehåll till lista
// <>: Data under arv!

// Array och record i LLDB:
// (SMALLINT [6]) A = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
// (<anonymous struct>) R = (X = 1, Y = 2, Z = 3)
// (<anonymous struct>) RR = {
//  AA = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//  BB = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//}
// (Är dessa från backtrace? Hur blir de med print?)
// Pekare: (RECTYPEPTR) $134 = 0x00000000
// Skalär: (SMALLINT) $135 = 0
//         (REAL) $136 = 0

// medan under GDB, med print:
// (gdb) print r
// $1 = {
//   X = 1, 
//   Y = 2, 
//   Z = 3
// }
// (gdb) print rr
// $2 = {
//   AA = {0, 0, 0, 0, 0, 0}, 
//   BB = {0, 0, 0, 0, 0, 0}
// }
// (gdb) print a
// $3 = {0, 0, 0, 0, 0, 0}
// Skalär:
// $1 = 0
// Pekare: 
// $2 = 0x0


begin
	currentFrame := 0;
	bracketLevel := 0;
	for i := 0 to 100 do
		isInherited[i] := false;
//	inheritedLevel := 0;
	pos := 1;
	SetLength(outArr, 0);
	scalarValue := '';

	GetToken(backTrace, pos, tokenType, tokenValue); // $nr
	GetToken(backTrace, pos, tokenType, tokenValue); // =
	GetToken(backTrace, pos, tokenType, tokenValue); // { eller första token i skalär
//	GetToken(backTrace, pos, tokenType, tokenValue); // CR

WriteLn('Efter start: ', bracketLevel, ',', inheritedLevel);

// tokenType can be:
// kNumToken: Numerical scalar
// kStringToken: String
// kHexToken: hex, pointer. Hex followed by string is AnsiString, but that is found by GetToken!
// kStartBracketToken ({): 
	
	// This seems not to be needed
	if tokenType = kCRLFToken then
		GetToken(backTrace, pos, tokenType, tokenValue);

	case tokenType of // CASE på start!
		kHexToken: // Pointer
		begin
			scalarValue := tokenValue;
		end;
		kNumToken: // Scalar number
		begin
			scalarValue := tokenValue;
		end;
		kStringToken: // String (not ansistring)
		begin
			scalarValue := tokenValue;
		end;
		kInheritedToken: // <> = { } - CAN NOT BE HERE!
		begin
			WriteLn('<> IMPOSSIBLE inheritance! ', inheritedLevel);
			GetToken(backTrace, pos, tokenType, tokenValue); // Skip the = 
		end;
		kStartBracketToken:
		begin
		// Status: records och array av scalar går fint, arrayer av arrayer sämre.
			while pos <= Length(backTrace) do // CENTRAL LOOP!
			begin
				GetToken(backTrace, pos, tokenType, tokenValue);
				WriteLn(tokenName[tokenType], ', "', tokenValue, '"');
				
				if tokenType = kInheritedToken then // Skip = {
				begin
					GoToEndOfLine(pos, backTrace);
//					GetToken(backTrace, pos, tokenType, tokenValue); // =
//					GetToken(backTrace, pos, tokenType, tokenValue); // {
					WriteLn('<> inheritance! ', inheritedLevel);
				end
				else
				if (tokenType = kUnknownToken) and (tokenValue = 'members') then // Skip "members of"
				begin
					GoToEndOfLine(pos, backTrace);
				end
				else
				if (bracketLevel - inheritedLevel = 2) and (tokenType = kStartBracketToken) then
				begin // Kan ge problem med records? Record i array?
					PreviewToken(backTrace, pos, tokenType, tokenValue);
					if tokenType = kCRLFToken then
					begin
						GetToken(backTrace, pos, tokenType, tokenValue);
						PreviewToken(backTrace, pos, tokenType, tokenValue);
					end;
					WriteLn('Start of array at level 1 found? Next = ', tokenValue);
					if tokenType = kUnknownToken then
						OutputVariable('', '(record)', kRecord)
					else
//						OutputVariable('(array in array)', '(array)', kArray);
						OutputVariable('', '(array)', kArray);
				end
				else
				if bracketLevel - inheritedLevel = 1 then // Or check isInherited?
				begin
					//WriteLn('bracketLevel - inheritedLevel = 1');
					case tokenType of
						kNumToken, kStringToken:
						begin
							// Scalar in array/record?
							WriteLn(tokenValue, ' is a string/scalar with value ', tokenValue);
//							OutputVariable('(none, value = ' + tokenValue + ')', tokenValue, kScalarOrUnknown); // ej pointer, plain string?
							OutputVariable('', tokenValue, kScalarOrUnknown); // ej pointer, plain string?
						end;
						kHexToken: // Pointer in array
						begin
							OutputVariable('', tokenValue, kPointer); // pointer
						end;
						kStartBracketToken:
						begin
							// Start of array. Collect values?
							WriteLn('Start of array at level 1 found?');
							OutputVariable('', '(array)', kArray);
						end;
						kUnknownToken:
						begin
							// Keep variable name
							variableName := tokenValue;
							// Get =
							GetToken(backTrace, pos, tokenType, tokenValue);
							if tokenType <> kEqualToken then
							begin
								if (tokenValue = 'locals.') or (tokenValue = 'symbol') then
									WriteLn('No-line')
								else
									WriteLn('Error: Unknown format (', tokenValue, ' for ', variableName,')');
								GoToEndOfLine(pos, backTrace);
							end
							else
							begin
								// Get number/"(", "{"
								GetToken(backTrace, pos, tokenType, tokenValue);
								case tokenType of
									kStringToken:
									begin
										WriteLn(variableName, ' is a string with value ', tokenValue);
										GoToEndOfLine(pos, backTrace);
										OutputVariable(variableName, tokenValue, kStringPointer); // ej pointer, plain string?
									end;
									kNumToken: // Plain number
									begin
										WriteLn(variableName, ' is a number with value ', tokenValue);
										OutputVariable(variableName, tokenValue, kScalarOrUnknown);
									end;
									kHexToken:
									begin
											GoToEndOfLine(pos, backTrace);
											OutputVariable(variableName, '(pointer)', kPointer);
									end;
									kStartParenToken: // Pointer
									begin
										WriteLn(variableName, ' is a pointer');
										GoToEndOfLine(pos, backTrace);
										OutputVariable(variableName, '(pointer)', kPointer);
									end;
									kStartBracketToken: // array/record
									begin
		//								Write(variableName, ' is an array or record. ');
										
										// Peek on next token
										PreviewToken(backTrace, pos, tokenType, tokenValue);
										if (tokenType = kStartBracketToken) or (tokenType = kNumToken) then
										begin
											WriteLn(variableName, ' is an array');
											OutputVariable(variableName, '(array)', kArray); // or kRecord
										end
										else
										begin
											WriteLn(variableName, ' is a record');
											OutputVariable(variableName, '(record)', kRecord); // or kRecord
										end;
										
										GoToEndOfLine(pos, backTrace);
		//								OutputVariable(variableName, '(array or record)', kArray); // or kRecord
										// Check next to decide!
									end;
									kUnknownToken:
									begin
										if (variableName = 'locals.') or (variableName = 'symbol') then
											WriteLn('No-line') // Should never happen
										else
											WriteLn('Unknown format');							
									end;
								end; // inner case
							end;
						end; // kUnknownToken
					end;
				end;
			end; // while
		end; // kStartBracketToken
	otherwise
		WriteLn('ERROR, can not parse variable');
	end; // case
	
// Output
// array of array of somedatarecord
end; // ParsePrint


procedure RemoveCR(var s: AnsiString);
var
	i: Longint;
begin
	for i := Length(s) downto 1 do
	begin
		if s[i] in [TAB, CR, LF] then
			s := Copy(s, 1, i-1) + Copy(s, i+1, Length(s));
	end;
end;

// Parses the reply from "print", called when a known variable should be
// displayed in field 2 (zoomed view).
procedure ParsePrintLLDB(backTrace: AnsiString;
									var outArr: OutArrType;
									var scalarValue: AnsiString);
var
	pos, tokenType, i: Longint;
	tokenValue, tokenValue2, variableName, indexString: AnsiString;
	currentFrame: Longint;
	oldpos, oldbracketlevel: Longint;

//	procedure CleanLLDBPrint(var s: AnsiString); // Obsolete
//	var
//		pos, tokenStart, tokenEnd, tokenType, start: Longint;
//		tokenValue, typeString: AnsiString;
//	const
//		CR = Char(13);
//		LF = Char(10);
//		TAB = Char(9);
//	begin
//	// Remove (TYPE)
//	// Remove $nn = 
//		pos := 1;
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue); // (
////		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		if tokenValue <> '(' then exit;
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, typeString); // type
////		WriteLn('CleanLLDBPrint found "', typeString, '"');
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue); // )
////		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		if tokenValue <> ')' then exit;

//	// $nr = should NOT be removed... ParsePrint expects it!
//	(*
//	SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue); // $nr
//		// Or two? One for $ and one for numbers?
//		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue);
//		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		if tokenValue <> '=' then exit(CleanLLDBPrint);
//	*)
//		start := pos+1;
//	// Find CR
//		while (pos < Length(s)) and not (s[pos] in [CR, LF]) do pos += 1;
//		s := Copy(s, start, pos-start);
//		// Then beautify numbers if needed. Too early?
//	//	BeautifyValue(s);
//	end;

	procedure OutputVariable(variableName, tokenValue: AnsiString; typeId: Longint);
	begin
		SetLength(outArr, Length(outArr)+1);
		outArr[High(outArr)].name := variableName;
		outArr[High(outArr)].value := tokenValue;
		outArr[High(outArr)].typeId := typeId;
		outArr[High(outArr)].frameNumber := currentFrame; // Irrelevant
		WriteLn('Outputs "', variableName, '"');
	end;

// $xx = ...
// Skalär: Värde
// Ansisträng: hex + värde
// Array eller record: {}, parsa innehåll till lista
// <>: Data under arv!

// Array och record i LLDB:
// (SMALLINT [6]) A = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
// (<anonymous struct>) R = (X = 1, Y = 2, Z = 3)
// (<anonymous struct>) RR = {
//  AA = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//  BB = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//}
// (Är dessa från backtrace? Hur blir de med print?)
// Pekare: (RECTYPEPTR) $134 = 0x00000000
// Skalär: (SMALLINT) $135 = 0
//         (REAL) $136 = 0

// medan under GDB, med print:
// (gdb) print r
// $1 = {
//   X = 1, 
//   Y = 2, 
//   Z = 3
// }
// (gdb) print rr
// $2 = {
//   AA = {0, 0, 0, 0, 0, 0}, 
//   BB = {0, 0, 0, 0, 0, 0}
// }
// (gdb) print a
// $3 = {0, 0, 0, 0, 0, 0}
// Skalär:
// $1 = 0
// Pekare: 
// $2 = 0x0
// Record:
// (gdb) print r
// $1 = {
//   X = 1, 
//   Y = 2, 
//   Z = 3
// }

var
	typeString: AnsiString;
	pos1: Longint;
begin
	currentFrame := 0;
	bracketLevel := 0;
	for i := 0 to 100 do
		isInherited[i] := false;
//	inheritedLevel := 0;
	pos := 1;
	SetLength(outArr, 0);
	scalarValue := '';

// Get past (TYPE). Can be several word in ()!
	SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // (
//	WriteLn('CleanLLDBPrint found "', tokenValue, '"');
	if tokenValue <> '(' then exit;
	SimpleParserGetToken(backTrace, pos, tokenType, typeString); // type
//	WriteLn('CleanLLDBPrint found "', typeString, '"');
	while (tokenValue <> ')') and (pos < Length(backTrace)) do
	begin
		SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // )
//		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
	end;
	if tokenValue <> ')' then exit;

// Use SimpleParser for this?
	GetToken(backTrace, pos, tokenType, tokenValue); // $nr
	GetToken(backTrace, pos, tokenType, tokenValue); // =
	GetToken(backTrace, pos, tokenType, tokenValue); // { eller första token i skalär
//	GetToken(backTrace, pos, tokenType, tokenValue); // CR

// WriteLn('Efter start: ', bracketLevel, ',', inheritedLevel);

// tokenType can be:
// kNumToken: Numerical scalar
// kStringToken: String
// kHexToken: hex, pointer. Hex followed by string is AnsiString, but that is found by GetToken!
// kStartBracketToken ({): 
	
	// This seems not to be needed
	if tokenType = kCRLFToken then
		GetToken(backTrace, pos, tokenType, tokenValue);

	case tokenType of // CASE på start!
		kHexToken: // Pointer
		begin
			scalarValue := tokenValue; // Beautify!
		end;
		kNumToken: // Scalar number
		begin
			scalarValue := tokenValue;
		end;
		kStringToken: // String (not ansistring)
		begin
			scalarValue := tokenValue;
		end;
		kInheritedToken: // <> = { } - CAN NOT BE HERE!
		begin
			WriteLn('<> IMPOSSIBLE inheritance! ', inheritedLevel);
			GetToken(backTrace, pos, tokenType, tokenValue); // Skip the = 
		end;
		kStartParenToken: // (  - array! Is this ONLY array of scalars?
		begin
// Array of scalars or record of scalars:
// (SMALLINT [6]) A = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
// Nej det är ta v. print är:
//(lldb) print A
//(SMALLINT [6]) $8 = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//(lldb) print R
//(<anonymous struct>) $7 = (X = 0, Y = 0, Z = 0)
// Parse [nr]
// Parse =
// Parse up to ,
// until a ) is found or end of string.
			while (pos < Length(backTrace)) do
			begin
				pos1 := pos;
				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier
				pos := pos1;
				if tokenValue = '[' then
				begin // array
					while (tokenValue <> ')') and (pos < Length(backTrace)) do
					begin
						SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // [
						SimpleParserGetToken(backTrace, pos, tokenType, indexString); // nr
						SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // ]
						SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // =
						// Pick up info about what it is here?
						// Read past entire value part. Save?
						pos1 := pos;
						while (tokenValue <> ',') and (tokenValue <> ')') and (pos < Length(backTrace)) do
						begin
							SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // value of scalar item or , or )
						end;
						tokenValue := Copy(backTrace, pos1, pos - pos1);
						if Length(tokenValue) > 0 then
							if tokenValue[Length(tokenValue)] in [')', ','] then
								tokenValue := Copy(tokenValue, 1, Length(tokenValue)-1);
						RemoveCR(tokenValue);
						// Ej om lldb!
						if indexString <> 'lldb' then
							OutputVariable('', tokenValue, kUnknownToken);
//							OutputVariable('['+indexString+']', tokenValue, kUnknownToken);
					end;
				end
				else
				begin // record
					while (tokenValue <> ')') and (pos < Length(backTrace)) do
					begin
						SimpleParserGetToken(backTrace, pos, tokenType, variableName); // identifier
						SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // =
						// Pick up info about what it is here?
						// Read past entire value part. Save?
						pos1 := pos;
						while (tokenValue <> ',') and (tokenValue <> ')') and (pos < Length(backTrace)) do
						begin
							SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // value of scalar item or , or )
						end;
						tokenValue := Copy(backTrace, pos1, pos - pos1);
						if Length(tokenValue) > 0 then
							if tokenValue[Length(tokenValue)] in [')', ','] then
								tokenValue := Copy(tokenValue, 1, Length(tokenValue)-1);
						RemoveCR(tokenValue);
						if variableName <> '(' then
							OutputVariable(variableName, tokenValue, kUnknownToken);
					end;
				end;
			end;


		
		end;
		kStartBracketToken: // { array of arrays, array of records, records...
		begin
		// Possible cases:
		// record of something.
		// What about objects?

// Det är skillnad på print och... är det andra när man begär variabellistor?
// Som ta v. Jajamen! Men nästan samma! Borde kunna använda samma parser!
//(lldb) print AA
//(RECTYPE [4]) $6 = {
//  [0] = (X = 0, Y = 0, Z = 0)
//  [1] = (X = 0, Y = 0, Z = 0)
//  [2] = (X = 0, Y = 0, Z = 0)
//  [3] = (X = 0, Y = 0, Z = 0)
//}
//(lldb) print BB
//(SMALLINT [4][4]) $1 = {
//  [0] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [1] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [2] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [3] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//}
//(lldb) print AA
//(RECTYPE [4]) $2 = {
//  [0] = (X = 0, Y = 0, Z = 0)
//  [1] = (X = 0, Y = 0, Z = 0)
//  [2] = (X = 0, Y = 0, Z = 0)
//  [3] = (X = 0, Y = 0, Z = 0)
//}
// ta v etc ser ut så här:
// Array of arrays:
//(SMALLINT [4][4]) BB = {
//  [0] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [1] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [2] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [3] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//}
// Array of records:
// (RECTYPE [4]) AA = {
//  [0] = (X = 0, Y = 0, Z = 0)
//  [1] = (X = 0, Y = 0, Z = 0)
//  [2] = (X = 0, Y = 0, Z = 0)
//  [3] = (X = 0, Y = 0, Z = 0)
//}



// Parse [ or identifier
// If identifier
//  repeat
//   Parse =
//   Parse value (can be arrays, records... watch out for sub-parenthesis)
//   Parse , or }
//  until }
//  
// else
// If [
//  Parse nr]
//  Parse =
//  Parse (
//  Parse next item for [ or name to tell between array or record
//  Parse up to )
//  until a } is found or end of string.

			pos1 := pos;
			SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier
			pos := pos1;
			if tokenValue = '[' then // array
			begin
				while pos < Length(backTrace) do
				begin
				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // [
				SimpleParserGetToken(backTrace, pos, tokenType, indexString); // index
				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // ]
				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // =
				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // (
//				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier! - decides type
					pos1 := pos;
					while (tokenValue <> ')') and (pos < Length(backTrace)) do
						SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // parse until ,
					tokenValue := Copy(backTrace, pos1, pos - pos1);
					if Length(tokenValue) > 0 then
						if tokenValue[Length(tokenValue)] = ')' then
							tokenValue := Copy(tokenValue, 1, Length(tokenValue)-1);
					RemoveCR(tokenValue);
					if (indexString <> '') and (indexString <> '}') and (indexString <> '(') then
						OutputVariable('['+indexString+']', tokenValue, kUnknownToken);
				end;
			end
			else // record
			begin
				while pos < Length(backTrace) do
				begin
				SimpleParserGetToken(backTrace, pos, tokenType, variableName); // name
				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // =
				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // (
//				SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier! - decides type
					pos1 := pos;
					while (tokenValue <> ')') and (pos < Length(backTrace)) do
						SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // parse until ,
					tokenValue := Copy(backTrace, pos1, pos - pos1);
					if Length(tokenValue) > 0 then
						if tokenValue[Length(tokenValue)] = ')' then
							tokenValue := Copy(tokenValue, 1, Length(tokenValue)-1);
					RemoveCR(tokenValue);
					if (variableName <> '') and (variableName <> '}') then
						OutputVariable(variableName, tokenValue, kUnknownToken);
				end;
			end;
		end;

		
		otherwise
		WriteLn('ERROR, can not parse variable');
	end; // case
	
// Output
// array of array of somedatarecord
end; // ParsePrintLLDB



procedure ParsePrintLLDBOLD(backTrace: AnsiString;
									var outArr: OutArrType;
									var scalarValue: AnsiString);
var
	pos, tokenType, i: Longint;
	tokenValue, tokenValue2, variableName, indexString: AnsiString;
	currentFrame: Longint;
	oldpos, oldbracketlevel: Longint;

//	procedure CleanLLDBPrint(var s: AnsiString); // Obsolete
//	var
//		pos, tokenStart, tokenEnd, tokenType, start: Longint;
//		tokenValue, typeString: AnsiString;
//	const
//		CR = Char(13);
//		LF = Char(10);
//		TAB = Char(9);
//	begin
//	// Remove (TYPE)
//	// Remove $nn = 
//		pos := 1;
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue); // (
////		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		if tokenValue <> '(' then exit;
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, typeString); // type
////		WriteLn('CleanLLDBPrint found "', typeString, '"');
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue); // )
////		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		if tokenValue <> ')' then exit;

//	// $nr = should NOT be removed... ParsePrint expects it!
//	(*
//	SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue); // $nr
//		// Or two? One for $ and one for numbers?
//		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		SimpleParserGetToken(s, pos, tokenStart, tokenEnd, tokenType, tokenValue);
//		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
//		if tokenValue <> '=' then exit(CleanLLDBPrint);
//	*)
//		start := pos+1;
//	// Find CR
//		while (pos < Length(s)) and not (s[pos] in [CR, LF]) do pos += 1;
//		s := Copy(s, start, pos-start);
//		// Then beautify numbers if needed. Too early?
//	//	BeautifyValue(s);
//	end;

	procedure OutputVariable(variableName, tokenValue: AnsiString; typeId: Longint);
	begin
		SetLength(outArr, Length(outArr)+1);
		outArr[High(outArr)].name := variableName;
		outArr[High(outArr)].value := tokenValue;
		outArr[High(outArr)].typeId := typeId;
		outArr[High(outArr)].frameNumber := currentFrame; // Irrelevant
		WriteLn('Outputs "', variableName, '"');
	end;

// $xx = ...
// Skalär: Värde
// Ansisträng: hex + värde
// Array eller record: {}, parsa innehåll till lista
// <>: Data under arv!

// Array och record i LLDB:
// (SMALLINT [6]) A = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
// (<anonymous struct>) R = (X = 1, Y = 2, Z = 3)
// (<anonymous struct>) RR = {
//  AA = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//  BB = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//}
// (Är dessa från backtrace? Hur blir de med print?)
// Pekare: (RECTYPEPTR) $134 = 0x00000000
// Skalär: (SMALLINT) $135 = 0
//         (REAL) $136 = 0

// medan under GDB, med print:
// (gdb) print r
// $1 = {
//   X = 1, 
//   Y = 2, 
//   Z = 3
// }
// (gdb) print rr
// $2 = {
//   AA = {0, 0, 0, 0, 0, 0}, 
//   BB = {0, 0, 0, 0, 0, 0}
// }
// (gdb) print a
// $3 = {0, 0, 0, 0, 0, 0}
// Skalär:
// $1 = 0
// Pekare: 
// $2 = 0x0
// Record:
// (gdb) print r
// $1 = {
//   X = 1, 
//   Y = 2, 
//   Z = 3
// }

var
	typeString: AnsiString;
	pos1: Longint;
begin
	currentFrame := 0;
	bracketLevel := 0;
	for i := 0 to 100 do
		isInherited[i] := false;
//	inheritedLevel := 0;
	pos := 1;
	SetLength(outArr, 0);
	scalarValue := '';

// Get past (TYPE). Can be several word in ()!
	SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // (
//	WriteLn('CleanLLDBPrint found "', tokenValue, '"');
	if tokenValue <> '(' then exit;
	SimpleParserGetToken(backTrace, pos, tokenType, typeString); // type
//	WriteLn('CleanLLDBPrint found "', typeString, '"');
	while (tokenValue <> ')') and (pos < Length(backTrace)) do
	begin
		SimpleParserGetToken(backTrace, pos, tokenType, tokenValue); // )
//		WriteLn('CleanLLDBPrint found "', tokenValue, '"');
	end;
	if tokenValue <> ')' then exit;

// Use SimpleParser for this?
	GetToken(backTrace, pos, tokenType, tokenValue); // $nr
	GetToken(backTrace, pos, tokenType, tokenValue); // =
	GetToken(backTrace, pos, tokenType, tokenValue); // { eller första token i skalär
//	GetToken(backTrace, pos, tokenType, tokenValue); // CR

WriteLn('Efter start: ', bracketLevel, ',', inheritedLevel);

// tokenType can be:
// kNumToken: Numerical scalar
// kStringToken: String
// kHexToken: hex, pointer. Hex followed by string is AnsiString, but that is found by GetToken!
// kStartBracketToken ({): 
	
	// This seems not to be needed
	if tokenType = kCRLFToken then
		GetToken(backTrace, pos, tokenType, tokenValue);

	case tokenType of // CASE på start!
		kHexToken: // Pointer
		begin
			scalarValue := tokenValue; // Beautify!
		end;
		kNumToken: // Scalar number
		begin
			scalarValue := tokenValue;
		end;
		kStringToken: // String (not ansistring)
		begin
			scalarValue := tokenValue;
		end;
		kInheritedToken: // <> = { } - CAN NOT BE HERE!
		begin
			WriteLn('<> IMPOSSIBLE inheritance! ', inheritedLevel);
			GetToken(backTrace, pos, tokenType, tokenValue); // Skip the = 
		end;
		kStartParenToken: // (  - array! Is this ONLY array of scalars?
		begin
// Array of scalars or record of scalars:
// (SMALLINT [6]) A = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
// Nej det är ta v. print är:
//(lldb) print A
//(SMALLINT [6]) $8 = ([0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0)
//(lldb) print R
//(<anonymous struct>) $7 = (X = 0, Y = 0, Z = 0)
// Parse [nr]
// Parse =
// Parse up to ,
// until a ) is found or end of string.
			while (pos < Length(backTrace)) do
			begin
				pos1 := pos;
				GetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier
				pos := pos1;
				if tokenValue = '[' then
				begin // array
					while (tokenValue <> ')') and (pos < Length(backTrace)) do
					begin
						GetToken(backTrace, pos, tokenType, tokenValue); // [
						GetToken(backTrace, pos, tokenType, indexString); // nr
						GetToken(backTrace, pos, tokenType, tokenValue); // ]
						GetToken(backTrace, pos, tokenType, tokenValue); // =
						// Pick up info about what it is here?
						// Read past entire value part. Save?
						pos1 := pos;
						while (tokenValue <> ',') and (tokenValue <> ')') and (pos < Length(backTrace)) do
						begin
							GetToken(backTrace, pos, tokenType, tokenValue); // value of scalar item or , or )
						end;
						tokenValue := Copy(backTrace, pos1, pos - pos1);
//						OutputVariable('['+indexString+']', tokenValue, kUnknownToken);
						OutputVariable('', tokenValue, kUnknownToken);
					end;
				end
				else
				begin // record
					while (tokenValue <> ')') and (pos < Length(backTrace)) do
					begin
						GetToken(backTrace, pos, tokenType, variableName); // identifier
						GetToken(backTrace, pos, tokenType, tokenValue); // =
						// Pick up info about what it is here?
						// Read past entire value part. Save?
						pos1 := pos;
						while (tokenValue <> ',') and (tokenValue <> ')') and (pos < Length(backTrace)) do
						begin
							GetToken(backTrace, pos, tokenType, tokenValue); // value of scalar item or , or )
						end;
						tokenValue := Copy(backTrace, pos1, pos - pos1);
						OutputVariable(variableName, tokenValue, kUnknownToken);
					end;
				end;
			end;


		
		end;
		kStartBracketToken: // { array of arrays, array of records, records...
		begin
		// Possible cases:
		// record of something.
		// What about objects?

// Det är skillnad på print och... är det andra när man begär variabellistor?
// Som ta v. Jajamen! Men nästan samma! Borde kunna använda samma parser!
//(lldb) print AA
//(RECTYPE [4]) $6 = {
//  [0] = (X = 0, Y = 0, Z = 0)
//  [1] = (X = 0, Y = 0, Z = 0)
//  [2] = (X = 0, Y = 0, Z = 0)
//  [3] = (X = 0, Y = 0, Z = 0)
//}
//(lldb) print BB
//(SMALLINT [4][4]) $1 = {
//  [0] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [1] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [2] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [3] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//}
//(lldb) print AA
//(RECTYPE [4]) $2 = {
//  [0] = (X = 0, Y = 0, Z = 0)
//  [1] = (X = 0, Y = 0, Z = 0)
//  [2] = (X = 0, Y = 0, Z = 0)
//  [3] = (X = 0, Y = 0, Z = 0)
//}
// ta v etc ser ut så här:
// Array of arrays:
//(SMALLINT [4][4]) BB = {
//  [0] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [1] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [2] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//  [3] = ([0] = 0, [1] = 0, [2] = 0, [3] = 0)
//}
// Array of records:
// (RECTYPE [4]) AA = {
//  [0] = (X = 0, Y = 0, Z = 0)
//  [1] = (X = 0, Y = 0, Z = 0)
//  [2] = (X = 0, Y = 0, Z = 0)
//  [3] = (X = 0, Y = 0, Z = 0)
//}



// Parse [ or identifier
// If identifier
//  repeat
//   Parse =
//   Parse value (can be arrays, records... watch out for sub-parenthesis)
//   Parse , or }
//  until }
//  
// else
// If [
//  Parse nr]
//  Parse =
//  Parse (
//  Parse next item for [ or name to tell between array or record
//  Parse up to )
//  until a } is found or end of string.

			pos1 := pos;
			GetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier
			pos := pos1;
			if tokenValue = '[' then // array
			begin
				GetToken(backTrace, pos, tokenType, tokenValue); // [
				GetToken(backTrace, pos, tokenType, indexString); // index
				GetToken(backTrace, pos, tokenType, tokenValue); // ]
				GetToken(backTrace, pos, tokenType, tokenValue); // =
				GetToken(backTrace, pos, tokenType, tokenValue); // (
				GetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier! - decides type
				pos1 := pos;
				while (tokenValue <> ')') and (pos < Length(backTrace)) do
					GetToken(backTrace, pos, tokenType, tokenValue); // parse until ,
				tokenValue := Copy(backTrace, pos1, pos - pos1);
//				OutputVariable('['+indexString+']', tokenValue, kUnknownToken);
				OutputVariable('', tokenValue, kUnknownToken);
			end
			else // record
			begin
				GetToken(backTrace, pos, tokenType, variableName); // name
				GetToken(backTrace, pos, tokenType, tokenValue); // =
				GetToken(backTrace, pos, tokenType, tokenValue); // (
				GetToken(backTrace, pos, tokenType, tokenValue); // [ or identifier! - decides type
				pos1 := pos;
				while (tokenValue <> ')') and (pos < Length(backTrace)) do
					GetToken(backTrace, pos, tokenType, tokenValue); // parse until ,
				tokenValue := Copy(backTrace, pos1, pos - pos1);
				OutputVariable(variableName, tokenValue, kUnknownToken);
			end;
		end;

		
		otherwise
		WriteLn('ERROR, can not parse variable');
	end; // case
	
// Output
// array of array of somedatarecord
end; // ParsePrintLLDB

end.
