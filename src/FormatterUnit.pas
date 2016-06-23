unit FormatterUnit;
interface
uses
	MacOSAll, HashTableUnit;


function PascalFormatter(teRec: AnsiString): AnsiString;
function PascalCRFormatter(teRec: AnsiString):AnsiString;

implementation

// Hash tables for color coding
var
	pascalHashTable: HashRec;
const
	kFunctionToken = 1;		// function, procedure
	kBeginEndToken = 2;		// begin/end/if/else/case/of - anything that affects structure)
	kReservedToken = 3;		// Reserved words that do NOT affect structure analysis
	kKnownTypeToken = 4;	// Integer, Longint, Real...
	kCommentToken = 5;		// {} (* *) // Can span several rows!
	kStringToken = 6;		// 'string' "c"
	kSpecialToken = 7;		// ( ) * / + - etc
	kSingleCharToken = 8;	// Any unknown token (identifiers etc)
	kOtherToken = 9;				// Any unknown token (identifiers etc)
	kPreprocessorToken = 10;// For the C parser
	kStartParenToken = 11;	// For the C parser
	kEndParenToken = 12;		// For the Java parser
	kClassToken = 13;		// For the Java parser
	kLibraryToken = 14;		// For Library functions
	kCarbonToken = 15;		// For Apple Carbon functions
	kCocoaToken = 16;		// For Apple Cocoa functions
	kCompilerDirectiveToken = 17;	// For compiler directives
	kCRLFToken = 18;			// At this time only used in code formatter
	//For PascalCRFormatter
	kBeginToken =19; // begin, else, var, type, const
	kFunctionModifier = 20; // override, overload, cdecl, register, message
	kCRafterTV =21; // then, try, repeat, except, finally, do, of, record, uses
	
	//For PascalFormatter
	kRecordToken = 22; // record, class, begin
	kUntilToken = 23; //end, end., until
	kCaseToken = 24; // case, try, repeat
	kForToken = 25; //for, with, while
	kIfElseToken = 26; //if, else
	kVarToken = 27; // var, type, const
	kImplementationToken =28; //Implementation
	
	
	const
	CR = Char(13);
	LF = Char(10);
	TAB = Char(9);


{------------------------ PASCAL COLOR CODER ----------------------------}

procedure InitColorCodingTable;
begin
	
	HashAddEntry(pascalHashTable, 'begin', kBeginToken);
	HashAddEntry(pascalHashTable, 'else', kBeginToken);
	HashAddEntry(pascalHashTable, 'var', kBeginToken);
	HashAddEntry(pascalHashTable, 'type', kBeginToken);
	HashAddEntry(pascalHashTable, 'const', kBeginToken);
	
	HashAddEntry(pascalHashTable, 'override', kFunctionModifier);
	HashAddEntry(pascalHashTable, 'overload', kFunctionModifier);
	HashAddEntry(pascalHashTable, 'cdecl', kFunctionModifier);
	HashAddEntry(pascalHashTable, 'register', kFunctionModifier);
	HashAddEntry(pascalHashTable, 'message', kFunctionModifier);
	
	HashAddEntry(pascalHashTable, 'then', kCRafterTV);
	HashAddEntry(pascalHashTable, 'try', kCRafterTV);
	HashAddEntry(pascalHashTable, 'repeat', kCRafterTV);
	HashAddEntry(pascalHashTable, 'except', kCRafterTV);
	HashAddEntry(pascalHashTable, 'finally', kCRafterTV);
	HashAddEntry(pascalHashTable, 'do', kCRafterTV);
	HashAddEntry(pascalHashTable, 'of', kCRafterTV);
	HashAddEntry(pascalHashTable, 'record', kCRafterTV);
	HashAddEntry(pascalHashTable, 'uses', kCRafterTV);
	
	HashAddEntry(pascalHashTable, 'program', kReservedToken);
	HashAddEntry(pascalHashTable, 'library', kReservedToken); // 110311
	HashAddEntry(pascalHashTable, 'exports', kReservedToken); // 110311
	HashAddEntry(pascalHashTable, 'array', kReservedToken);
	HashAddEntry(pascalHashTable, 'unit', kReservedToken);
	HashAddEntry(pascalHashTable, 'interface', kReservedToken);
	HashAddEntry(pascalHashTable, 'implementation', kReservedToken);
	HashAddEntry(pascalHashTable, 'initialization', kReservedToken); // Ingemar 090629
	HashAddEntry(pascalHashTable, 'finalization', kReservedToken); // Ingemar 110424
	HashAddEntry(pascalHashTable, 'const', kReservedToken);
	HashAddEntry(pascalHashTable, 'type', kReservedToken);
	HashAddEntry(pascalHashTable, 'var', kReservedToken);
	HashAddEntry(pascalHashTable, 'record', kReservedToken);
	HashAddEntry(pascalHashTable, 'object', kReservedToken);
	HashAddEntry(pascalHashTable, 'uses', kReservedToken);

	HashAddEntry(pascalHashTable, 'of', kReservedToken);
	HashAddEntry(pascalHashTable, 'set', kReservedToken);
	HashAddEntry(pascalHashTable, 'variant', kReservedToken);
	HashAddEntry(pascalHashTable, 'packed', kReservedToken);
	HashAddEntry(pascalHashTable, 'overload', kReservedToken);
	
	HashAddEntry(pascalHashTable, 'class', kClassToken);
	HashAddEntry(pascalHashTable, 'objcclass', kClassToken);
	
	


	HashAddEntry(pascalHashTable, 'procedure', kFunctionToken);
	HashAddEntry(pascalHashTable, 'function', kFunctionToken);
	HashAddEntry(pascalHashTable, 'operator', kFunctionToken); // 110314
	HashAddEntry(pascalHashTable, 'constructor', kFunctionToken);
	HashAddEntry(pascalHashTable, 'destructor', kFunctionToken);

	HashAddEntry(pascalHashTable, 'begin', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'end', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'end.', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'if', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'then', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'else', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'case', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'otherwise', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'with', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'downto', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'for', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'to', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'do', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'while', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'repeat', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'until', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'goto', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'try', kBeginEndToken); // Ingemar 090629
	HashAddEntry(pascalHashTable, 'except', kBeginEndToken); // Ingemar 090629


end;
var
previousTokenType: Longint; {Ett steg tillbaka}
previousTokenValue: AnsiString;
lastpreviousTokenValue:AnsiString;
changePreviousTokenValue: Boolean;
type formatterSnapshot = record
	pos: LongInt;
	end;

procedure GetFormatToken(data: AnsiString; {bufferLength: Longint;} var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);
var
	s: ansistring;
	bufferLength: Longint;
begin
// MÅSTE GÖRAS OM!
// Reagera på CRLF som egen token.
// Egen hash med formatspecifika saker; begin end var const type record...
// Kommentarparsern måste vara kvar! Även strängparsern. Därmed nästan samma!
	bufferLength:= Length(data);
	if changePreviousTokenValue then
	begin
		previousTokenValue:= lastPreviousTokenValue;;
		changePreviousTokenValue:=false;
	end
	else
		previousTokenValue:= tokenValue;
		
	previousTokenType:= tokenType;
	while (data[pos] in [TAB, ' ']) and (pos < bufferLength) do pos := pos + 1;
	tokenStart := pos;
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
{Change 070326: Added '.' so Object Pascal code will get methods nicely listed in the function menu}
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.']) and (pos < bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		{Find out what alphanumerical symbol it is by looking it up in the hash table!}
		//SetLength(s, pos - tokenStart);
		//BlockMove(@data[tokenStart], @s[1], pos - tokenStart); // MoveBytes - should be a Copy
		//s:= Copy(data, 1, tokenStart-1);
		s := Copy(data, tokenStart, pos - tokenStart);
		s := LowerCase(s);
		tokenType := HashLookUpInteger(pascalHashTable, s);
		tokenValue := s;
		
		//Writeln('s: ', s);
	end
	else
	begin {strings, comments, compiler directives, math operators, brackets, & other singletons }
	
		if data[pos] in [CR, LF] then
		begin
//			WriteLn('CRLF token');
			// Jag vill bara ha den SISTA - flera i rad är ointressanta
			while (data[pos] in [CR, LF]) and (pos < bufferLength) do begin {Writeln('pos: ', pos, 'CR or LF: ', data[pos]);} pos := pos + 1; end;
			tokenEnd := pos-1;
			tokenType := kCRLFToken;
			tokenValue := data[pos-1];
//			pos := pos + 1;
		end
		else
		if (data[pos] = '{') and (data[pos+1] = '$') then // compiler directive
		begin
			pos := pos + 2;
			while (data[pos] <> '}') and (pos < bufferLength) do pos := pos + 1; //why is it here?
			tokenEnd := pos;
			pos := pos+1;
			tokenType := kCompilerDirectiveToken;
			//tokenValue:= '{$'
		end
		else
		if data[pos] = '{' then // Bracket-comment
		begin
			while (data[pos] <> '}') and (pos < bufferLength) do pos := pos + 1;
			pos := pos+1;
			tokenEnd := pos-1;
			tokenType := kCommentToken;
			tokenValue:= 'Comment';
//			pos := pos + 1; // Test, try to avoid the last character to be caught by someone else
			// NOTE! This is most likely needed for other cases!
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '/') then // Line-comment
		begin
			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			//pos:=pos+1;
			tokenEnd := pos - 1;
			tokenType := kCommentToken;
			tokenValue:= 'Comment';
		end
		else
		if (data[pos] = '(') and (data[pos+1] = '*') then // Block-comment
		begin
			pos := pos + 2;
			while ((data[pos-1] <> '*') or (data[pos] <> ')')) and (pos < bufferLength) do pos := pos + 1;
			pos := pos+1;
			tokenEnd := pos-1;
			tokenType := kCommentToken;
			tokenValue:= 'Comment';
//			pos := pos + 1; // Test, try to avoid the last character to be caught by someone else
			// NOTE! This is most likely needed for other cases!
		end
		else
		if (data[pos] = '''') then // String
		begin
			pos := pos + 1;
			while not (data[pos] in ['''', CR, LF]) and (pos < bufferLength) do
				pos := pos + 1;
			tokenEnd := pos;
			pos := pos + 1; // Skip '
			tokenType := kStringToken;
			// Should we set tokenValue here?
		end
		else
		begin
		// Otherwise skip the symbol
				tokenEnd := tokenStart;
				tokenType := kSingleCharToken;
				tokenValue := data[pos]; // Ord(data[pos]);
				pos := pos + 1;
			//while not (dataPtr in ['a'..'z', 'A'..'Z', '0'..'9', '_', CR, LF, ' ']) do pos := pos + 1;
		end;
	end;
end;

var
	waitingForSemicolon: array of Boolean;
	waitingForCR: array of Boolean;

	//elseFlag: Boolean;
	ifWOBegin: array of Boolean;
	elseWOBegin: array of Boolean;
	elseWithBegin: array of Boolean;
	loopWOBegin: array of Boolean;
function FormatIFElse(level : Longint):Boolean;
begin
	FormatIFElse:= false;
	if (ifWOBegin[level] xor (elseWOBegin[level] or elseWithBegin[level])) then
		if not (elseWithBegin[level]) then
		begin
		 	FormatIFElse:= true;
		end;
		 
	if loopWOBegin[level] then
		FormatIFElse:= true;

end;

const
	kDefaultFlagArraySize = 30;
	kLevelMargin = 10;


// Make sure that level doesn't go out of bounds! Expand arrays if it goes up too far.
procedure CheckLevel(var level: Longint);
begin
	if level < 0 then
		level := 0
	else
		if level > High(waitingForSemicolon) - kLevelMargin then
		begin
			SetLength(waitingForSemicolon, Length(waitingForSemicolon)*2);
			SetLength(waitingForCR, Length(waitingForCR)*2);
			SetLength(ifWOBegin, Length(ifWOBegin)*2);
			SetLength(elseWOBegin, Length(elseWOBegin)*2);
			SetLength(elseWithBegin, Length(elseWithBegin)*2);
			SetLength(loopWOBegin, Length(loopWOBegin)*2);
		end;
end;

procedure IndentOnCRLF(var chars, caseChars: AnsiString;  var bufferLength, pos: Longint; level: Longint); 
//Replace bufferlength with length(chars)
var
	pos1, diff: Longint;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;

begin
	//WriteLn('Indent ', pos, ' to level ', level);
	
	// Vad är nästa token?
	pos1 := pos;
	//diff:=0
	GetFormatToken(chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
	
	//If tokenValue='end' then tokenType:= 2;

	if tokenType = kBeginEndToken then
	begin
		if (tokenValue = 'end') or (tokenValue = 'end.') or (tokenValue = 'until') or (tokenValue = 'except') or (tokenValue = 'finally')
		{or (tokenValue = 'private') or (tokenValue = 'public')} then
			begin
				//writeln('Level down', level, ' in Indent due to ', tokenValue);				
				level-=1;		
			end;
			
		if (tokenValue = 'else') then
		begin
			if elseWithBegin[level+1] then
		 	begin
		 		level-=1;
		 	 	//writeln('IFELSE level down in IndentCRLF becoz of elseWithBegin and level ',level);
			 	
		 	end;
		end;

//			level -= 1; // Local modification only - only if within one-line if!
			// "except" och "finally" borde ha samma? else?
	end;
	//Writeln('LEVEL: ', level);
	{for i:=0 to level do
	begin
		writeln('ifWOBegin ', i, ' ', ifWOBegin[i]);
		writeln('elseWOBegin ', i, ' ', elseWOBegin[i]);
		writeln('elseWithBegin ', i, ' ', elseWithBegin[i]);
		writeln('loopWOBegin ', i, ' ', loopWOBegin[i]);

	end;}
	
	if level < 0 then
		level := 0;
		
	CheckLevel(level);
	
	// Vad är nästa? Redan kollat, detta är CRFL som EJ följs av CRLF
	pos1 := pos;
	//Writeln('tokenValue in IndentOnCRLF: ', tokenValue);
	// Hitta slut på indentering
	while (chars[pos1] in [' ', TAB]) and (pos1 < bufferLength) do pos1 := pos1 + 1;

	diff := level - (pos1-pos);
	//WriteLn('Diff at CRLF: ', diff, ' at level ', level);
	if diff <= 0 then // mindre data - kopiera först, korta sedan    - take away uneccessary space/tab
	begin
		chars:= Copy(chars, 1, pos - 1)+ Copy(#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9, 1, level) + Copy(chars, pos1, bufferLength-pos1+1);
		bufferLength += diff;
	end;
	if diff>0 then // mer data - läng först, kopiera sedan     - add more spaces/tabs
	begin
		chars:= Copy(chars, 1, pos-1) + Copy(#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9, 1, level) + Copy(chars, pos1, bufferLength-pos1+1);
		bufferLength += (diff-1);
	end;

	bufferLength := Length(chars);
	
end;
procedure ResetFlags(var level:LongInt);
begin
	ifWOBegin[level]:=false;
	elseWOBegin[level]:=false;
	loopWOBegin[level]:=false;
	elseWithBegin[level+1]:=false;
end;

function PascalFormatter(teRec: AnsiString{HaldaPtr; editIndex: Integer}):AnsiString;
var
	bufferLength: Longint;
	//err: OSErr;
	chars, caseChars: AnsiString;
	pos, tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
//	hasSeenProgram, hasSeenUnit, hasSeenInterface, hasSeenImplementation: Boolean;
	
//	secondPreviousTokenType: Longint; {Två steg tillbaka}
//	secondPreviousTokenValue: AnsiString;

	level, pos1: Longint;
	i: Longint;
//	waitingForSemicolon: array of Boolean;
	secondWaitForCR, pauseSecondWaitForCR: Boolean;
	//predictElse: Boolean;

begin
//	WriteLn('ENTER FORMATTER');
	SetLength(waitingForSemicolon, kDefaultFlagArraySize);
	SetLength(waitingForCR, kDefaultFlagArraySize);
	SetLength(ifWOBegin, kDefaultFlagArraySize);
	SetLength(elseWOBegin, kDefaultFlagArraySize);
	SetLength(elseWithBegin, kDefaultFlagArraySize);
	SetLength(loopWOBegin, kDefaultFlagArraySize);
	//SetLength(offset, 20);
//	SetLength(elseFlag, 20);

	for i := 0 to High(waitingForSemicolon) do
	begin
		waitingForSemicolon[i] := false;
		waitingForCR[i] := false;
		ifWOBegin[i]:= false;
		elseWOBegin[i]:= false;
		elseWithBegin[i]:= false;
		loopWOBegin[i]:= false;
		//offset[i] := 0;
	end;
	secondWaitForCR:= false;
	pauseSecondWaitForCR:=false;
	//elseFlag := false;
	// Get the text from the text field
	chars := teRec;//^.text;
	caseChars := chars;
	
	// Change chars to lower case
	LowerCase(chars);
	
	bufferLength := Length(chars);
	pos := 0;
	level := 0;
	

	repeat
		GetFormatToken(chars,  pos, tokenStart, tokenEnd, tokenType, tokenValue);
		//Writeln('tV: ',tokenValue, ' tT: ', tokenType, ' pos: ', pos);
		
		case tokenType of
		
			kBeginEndToken:  //kBeginEndToken=2
			begin
				if {(tokenType=kRecordToken )} (tokenValue = 'begin') or (tokenValue = 'record') or (tokenValue = 'class') {or (tokenValue = 'case')}  then
				begin
					level += 1;
					ResetFlags(level);
					//writeln('Level Up: ', level,  ' in kBiginToken due to: ', tokenValue);
				end;
				if {(tokenType = kUntilToken)} (tokenValue = 'end') or (tokenValue = 'end.') or (tokenValue = 'until') then
				begin
						level -= 1;
						//writeln('Level down: ', level,  ' in kBiginToken due to: ', tokenValue);
			
				end;
				if {(tokenType = kCaseToken)} (tokenValue = 'case') or (tokenValue = 'try') or (tokenValue = 'repeat') then // Anything that will lead to "end"
				begin
					level += 1;
					ResetFlags(level);
				end;
				
				if {( (tokenType = kIfElseToken) or (tokenType = kForToken) )} (tokenValue = 'if') or (tokenValue = 'for') or (tokenValue = 'else') or (tokenValue = 'with') or (tokenValue = 'while') then // + "on"
					// Dessa borde INTE indentera om det följs av begin... men det låter jobbigt att parsa efter
				begin
					// För if, sök "then"
					pos1 := pos;
					if tokenValue = 'if' then
					begin
						//predictElse:=false;
						//Parse until CR followed by 'then', Even user missed CR after 'then', CRFormatter adds CR after 'then'.
						repeat 
							GetFormatToken({dataPtr} chars,  pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							//writeln('IN IF LOOP: ', 'tokenValue: ', tokenValue, ' pos1: ', pos1);
					
						until (tokenType = kCRLFToken) or (pos1 >= bufferLength);
						pos:=pos1-1;
						
						// Sök första efter (efter "then" för "if", 'for', 'while', 'with'), är det "begin"?
						repeat 
							GetFormatToken({dataPtr} chars,  pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							//writeln('IN IF LOOP1: ', 'tokenValue: ', tokenValue, 'pos1: ', pos1);
						until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength);  
						//writeln('**TV**', tokenValue);
						//If 'if', 'for', 'while', 'with' followed by 'if..then' then do not set waitForCR, which make level down. Use pauseSecondWaitForCR for postpone that.
						if ( (tokenValue = 'if') or (tokenValue = 'for') or (tokenValue = 'while') or (tokenValue = 'with') or (tokenValue = 'case') ) then
						begin
							pauseSecondWaitForCR:= true;
							//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
						end
						else
						begin
							pauseSecondWaitForCR:= false; 
							//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
						end;
								
						// Om inte, intendera enbart nästa sats
						if tokenValue <> 'begin' then
						begin	
							level += 1;
							ResetFlags(level);
							ifWOBegin[level]:= true;
							
							//writeln('IN IF LOOP, LEVEL UP: ', level, ' DUE TO ', tokenValue);
							//IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
							if not (pauseSecondWaitForCR) then
							begin
								CheckLevel(level);
								waitingForCR[level] := true;
							end;
								
								{if level >= 0 then
								begin
									if level > High(waitingForCR) then
										SetLength(waitingForCR, level*2 + 10);
									waitingForCR[level] := true;
									writeln('***level***', level );
								end;}
				
						end;
					
						
						//If elseFlag is true, and begin then no need to level down because the case is 'else followed by if begin' 
						{if ((tokenValue='begin') and elseFlag) then 
						begin
							elseFlag := False;
							predictElse:=true;
							Writeln('elseFlag: ', elseFlag, ' due to if begin');
						end;}
						
					end
					
					else if {(tokenType=kForToken)} (tokenValue = 'for') or (tokenValue='while') or (tokenValue = 'with') then
					begin
						pos1 := pos;
							repeat GetFormatToken({dataPtr} chars,  pos1, tokenStart, tokenEnd, tokenType, tokenValue)
							until (tokenValue = 'do') or (pos1 >= bufferLength);
						// Sök första efter (efter "then" för "if"), är det "begin"?
						repeat GetFormatToken({dataPtr} chars,  pos1, tokenStart, tokenEnd, tokenType, tokenValue)
						until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength);  
						//writeln('TOKEN VALUE: ', tokenValue);
						//If 'if', 'for', 'while', 'with' followed by 'for, or while, or with ...do ' then do not set waitingForSemicolon. Use pauseSecondWaitForCR for postpone that.
						if ( (tokenValue = 'if') or (tokenValue = 'for') or (tokenValue = 'while') or (tokenValue = 'with') or (tokenValue = 'case') ) then
						begin
							pauseSecondWaitForCR:= true;
							//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
						end
						else
						begin
							pauseSecondWaitForCR:= false; 
							//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
						end;
						
						// Om inte, intendera enbart nästa sats
						if tokenValue <> 'begin' then
						begin
							level+=1;
							ResetFlags(level);
							loopWOBegin[level]:=true;
							{if level>0 then
							begin
								offset[level]:= offset[level-1]+1;
								//offset[level]+=1;
								offset[level-1]:=0;
							end;}
							if not (pauseSecondWaitForCR) then
							begin
								CheckLevel(level);
								waitingForSemicolon[level] := true;
							end;
								{if level >= 0 then
								begin
									if level > High(waitingForSemicolon) then
										SetLength(waitingForSemicolon, level*2 + 10);
									waitingForSemicolon[level] := true;
									writeln('wfs in kBeginToken for loop: ', waitingForSemicolon[level]);
								end;}
						end;
						{if (tokenValue = 'if') then
						begin
							elseFlag:=true; 
							Writeln('elseFlag: ', elseFlag, ' due to after for/while/with if');
						end;}
					end
					else if (tokenValue = 'else') then
					begin
						//if (previousTokenValue='end') then
						//begin
							if elseWithBegin[level+1] then
						 	begin
						 		level-=1;
						 	 	//writeln('IFELSE level down in kBeginEndToken becoz of elseWithBegin and level ',level);
						 	 	//pos1:= pos- length(tokenValue);
							 	IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
						 	end;
						//end;
						pos1:=pos;
						repeat GetFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
						until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength); 
						//Writeln('tokenValue after else: ', tokenValue);
						//If 'if', 'for', 'while', 'with' followed by 'for, or while, or with ...do ' then do not set waitingForSemicolon. Use pauseSecondWaitForCR for postpone that.

						if ( (tokenValue = 'if') or (tokenValue = 'for') or (tokenValue = 'while') or (tokenValue = 'with') or (tokenValue = 'case') ) then
						begin
							pauseSecondWaitForCR:= true;
							//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
						end
						else
						begin
							pauseSecondWaitForCR:= false; 
							//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
						end;
						// Om inte, intendera enbart nästa sats
						if tokenValue <> 'begin' then
						begin
							level+=1;
							ResetFlags(level);
							elseWOBegin[level]:= true;
							if not (pauseSecondWaitForCR) then
							begin
								CheckLevel(level);
								//Set both waitingForSemicolon and waitingForCR. One of them will be used and the other will be reset. 
								waitingForSemicolon[level] := true;
								waitingForCR[level] := true;
							end;
							
							{if level >= 0 then
							begin
								if level > High(waitingForSemicolon) then
									SetLength(waitingForSemicolon, level*2 + 10);
								waitingForSemicolon[level] := true;
							end;}
						end
						else 
						begin
							if (tokenValue = 'begin') then
								elseWithBegin[level+1]:= true;
						end;
						//Else flag was used while 'if' followed by 'else, for, while, with'. 	
						{if (tokenValue = 'if') then
						begin
							elseFlag:=true; 
							Writeln('elseFlag: ', elseFlag, ' due to after else if');
						end
						else 		//No 'if' after 'else'
							if ( (predictElse) and (tokenValue <> 'begin') ) then // we will think about this later, It may be skipped.
							begin
								elseFlag:= true;
								predictElse:=false;
							end;}
								
					end;
					
				end;
	
			end;
			
			kReservedToken: //kReservedToken=3
			// var type const record unit...
			begin		
//				if (tokenValue = 'var') or (tokenValue = 'type') or (tokenValue = 'const') or (tokenValue = 'uses') then
				if tokenValue = 'uses' then
				begin
//				Duger inte för var/const/type som kan vara flera i rad
						level += 1;
						IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
						CheckLevel(level);
						waitingForSemicolon[level] := true;
						{if level >= 0 then
						begin
							if level > High(waitingForSemicolon) then
								SetLength(waitingForSemicolon, level*2 + 10);
							waitingForSemicolon[level] := true;
						end;}
				end;

				if {(tokenType=kVarToken)}(tokenValue = 'var') or (tokenValue = 'type') or (tokenValue = 'const') then
				begin
					level+=1;
					//WriteLn('block starter temporary level ', level);
					// Parse to ";", process CR or LF followed by anything else
					// or record - parsed as part of the block
					// Then test the next token to see if there is more.
					
					// Men if, for, while... och repeat...until måste ju också fungera! case!
					// VARJE rad skall ha +1 tills semikolon kommer!
					
					// Const och var kommer att få problem med records, som har semikolon. Processa parenteser?
					
					// type kan innehålla class! Kräver waitingForSemicolon?
					// och type kan innehålla record!
					// Borde inte alla dessa lösas med waitingForSemiColon?
					
					repeat
						repeat
							GetFormatToken(chars, pos, tokenStart, tokenEnd, tokenType, tokenValue);
							//Writeln('In Type Loop: ', tokenValue, ' pos: ', pos);
							if tokenType = kCRLFToken then
								IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
							if ( (tokenType = kReservedToken) or (tokenType = kClassToken) ) then
							begin
								//Writeln('*******Entered kReservedToken******');
								// record statement - parse until end
								if (tokenValue = 'record') or (tokenValue = 'object') then // inside "var" or "type"
								begin
									level+=1;
									//WriteLn('record start');
									repeat
										GetFormatToken({dataPtr} chars,  pos, tokenStart, tokenEnd, tokenType, tokenValue);
										if tokenType = kCRLFToken then
											IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
									until (tokenType = kBeginEndToken) or (pos >= bufferLength);
									level-= 1;
									//WriteLn('record end at ', tokenValue);
								end
								else
								// class statement - parse until end
								if ( {tokenType = kClassToken} (tokenValue = 'class') or (tokenValue = 'objcclass') )  then // inside "var" or "type"
								begin
									level += 1;
									//WriteLn('class starts');
									pos1:=pos;
									repeat
										GetFormatToken(chars,  pos1, tokenStart, tokenEnd, tokenType, tokenValue);
										//writeln('IN CLASS LOOP: ', 'TOKENVALUE: ', tokenValue, ' POS1: ', pos1);
										if tokenType = kCRLFToken then
											IndentOnCRLF(chars, caseChars, bufferLength, pos1, level);
									until (tokenValue = 'end') or (pos1 >= bufferLength);
									level-= 1;
									pos:=pos1;
								end;
							end;
						until (tokenValue = ';') or (pos >= bufferLength);//second until
						pos1 := pos;
							repeat
								GetFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);						
							until (not (tokenType in [kCommentToken, kCompilerDirectiveToken, kCRLFToken])) or (pos1 >= bufferLength);
					until ( {tokenType in [kVarToken, kImplementationToken, kFunctionToken]}(tokenvalue= 'type') or (tokenvalue= 'var') or (tokenvalue= 'begin') or (tokenvalue= 'const') or (tokenvalue= 'procedure') or
					(tokenvalue= 'function') or (tokenvalue= 'implementation') or (tokenValue = 'constructor') )or (pos >= bufferLength);//first until
					level-= 1;
				end;
			end;
			
			kSingleCharToken: //kSingleCharToken=8
			begin
				if tokenValue = ';' then
					if level <= High(waitingForSemicolon) then
					begin
						if waitingForSemicolon[level] then
						begin
							//If waitingForCR[level] is true then no need to level at wiatingforCR.
							if waitingForCR[level] then
								waitingForCR[level]:=false;
							waitingForSemicolon[level] := false;
							ResetFlags(level);
							{ifWOBegin[level]:=false;
 							elseWOBegin[level]:=false;
 							loopWOBegin[level]:=false;
 							elseWithBegin[level+1]:=false;}
							level -= 1;
							//Writeln('Level down due to wFsemicolon: ', level);
							{if elseFlag then
							begin
								waitingForSemicolon[level] := false;
								ifWOBegin[level]:=false;
		 						elseWOBegin[level]:=false;
		 						loopWOBegin[level]:=false;
								level-=1;
								Writeln('Level down: ', level, 'due to elseFlag ', elseFlag );
								elseFlag:= False;
							end;}
						
							
						end;
						//If 'secondWaitForCR' is true, make it false, bcoz FormatIFELSE will level down bcoz of ';' found. 'secondWaitForCR' should only applicable while no ';' after if..then (if..then statement else ...)
						if secondWaitForCR then
							secondWaitForCR:=false;
						//Reset elseWithBegin[level+1] when end followed by ';'. Its no more usefull
						if previousTokenValue='end' then
							if elseWithBegin[level+1] then
								elseWithBegin[level+1]:=false;
								
						if FormatIFElse(level) then
						begin
							repeat 
								if FormatIFElse(level) then
								begin
									ifWOBegin[level]:=false;
		 							elseWOBegin[level]:=false;
		 							loopWOBegin[level]:=false;
		 							elseWithBegin[level+1]:=false;
									level-=1;
									//writeln('IFELSE level down in kSingleCharToken and level ',level);
								end;
							until not FormatIFElse(level);
						end;
					end;
			end;
			
			kCRLFToken:// kCRLFToken=18 , calls for every line
			begin
				//writeln('**Level:**', level, ' waitForCR[level]: ', waitingForCR[level], 'pauseSecondWaitForCR: ', pauseSecondWaitForCR);
				if secondWaitForCR then
				begin
					pos1 := pos;
					repeat
						GetFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);						
					until (not (tokenType in [kCommentToken,  kCRLFToken])) or (pos1 >= bufferLength);
					if tokenValue='else' then
						if elseWOBegin[level] then
						begin
							waitingForSemicolon[level]:=false; // Reset waitForSemicolon as it is level down here.
							level-=1;
						end;
						
					level-=1;
					//Writeln('Level down due to secondWaitForCR: ', level);
					secondWaitForCR:=false;
					IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
				end;
		
				if level <= High(waitingForCR) then
					begin
						if waitingForCR[level] then
						begin
							waitingForCR[level] := false;
							secondWaitForCR:= true;
						
							//level -= 1;
						end;
						IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
					end;
				
					
				
			end;
				// IndentOnCRLF(CharsHandle(chars), dataPtr, bufferLength, pos, level);
			kCommentToken:
				IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
			
		end; // case	
		
	until pos >= bufferLength;
	PascalFormatter:= chars;


// Sätt tillbaka markering
// Markeringen bör justeras av ändringar!

//	WriteLn('EXIT FORMATTER');
end;

function AddCR(chars: AnsiString; pos: LongInt; tokenValue: AnsiString):AnsiString;
var
bufferLength: LongInt;
begin
	bufferLength:= Length(chars);
	AddCR := Copy(chars, 1, pos-length(tokenValue)-1)+ #13 + Copy(chars, pos-length(tokenValue), bufferLength+1);

end;

function PascalCRFormatter(teRec: AnsiString):AnsiString;
var
	bufferLength: Longint;
	//err: OSErr;
	chars: AnsiString;
	pos, tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
//	hasSeenProgram, hasSeenUnit, hasSeenInterface, hasSeenImplementation: Boolean;

//	secondPreviousTokenType: Longint; {Två steg tillbaka}
//	secondPreviousTokenValue: AnsiString;

	pos1: Longint;
//	elseFlag: Boolean=false;
	bracketFlag: Boolean =true;
	quitProcedure: boolean=false;
	bracketFound: Boolean;
	typeParsing: Boolean = false;
//	procedureFlag:Boolean;
	
	lastSemicolon:Longint;
	
begin
//	WriteLn('ENTER CR FORMATTER');

	// Get the text from the text field
	chars := teRec;//^.text;
//	caseChars := chars;
	
	// Change chars to lower case
	LowerCase(chars);
	
	bufferLength := Length(chars);
	pos := 0;
//	level := 0;

	repeat
		
		//Writeln('previousTokenValue: ',previousTokenValue);
		GetFormatToken(chars, pos, tokenStart, tokenEnd, tokenType, tokenValue);
		//Writeln('tV: ',tokenValue, ' tT: ', tokenType, ' pos: ', pos);
		
		//Add CR after tokenValue
		if ( ((previousTokenValue=';') or {(previousTokenType= kBeginToken)} (previousTokenValue='begin') or (previousTokenValue='else') or (previousTokenValue='var') or (previousTokenValue='type') or (previousTokenValue='const') or
		{Only after} {(previousTokenType= kCRafterTV)} (previousTokenValue='then') or (previousTokenValue='try') or (previousTokenValue='repeat') or (previousTokenValue='except') or 
		(previousTokenValue='finally') or (previousTokenValue='do') or (previousTokenValue='of') or (previousTokenValue='record') or (previousTokenValue='uses') ) 
		and (not (tokenValue= #13)) ) then
		begin
			//Writeln('previousTokenValue: ',previousTokenValue, ' tokenValue: ', tokenValue, 'tokenType: ', tokenType,' pos: ', pos);
			//Writeln('Add CR Here due to, ', previousTokenValue);
			if not (tokenType=kCommentToken) then
			begin
				chars := AddCR(chars, pos, tokenValue);
				pos:=pos-length(TokenValue);
				bufferLength+=1;
				//Writeln('new pos: ', pos);
				//Writeln('Add CR after TV due to pTV, ', previousTokenValue );
			end;
		end		
		//Add CR before tokenValue
		else
		if ((not (previousTokenValue=#13)) and ( {(tokenType= kBeginToken)} (tokenValue='begin')  or (tokenValue='else') or (tokenValue='var') or (tokenValue='type') or (tokenValue='const') or 
		{only before} {(tokenType= kFunctionToken)} (tokenValue='procedure') or (tokenValue='function') or (tokenValue = 'constructor') )) then
		begin
			//Writeln('previousTokenValue: ',previousTokenValue, ' tokenValue: ', tokenValue, ' pos: ', pos);
			
			//While type parsing,  no need insert CR before token Value (certainlybefore procedure in type). It only needs to add CR after ';' 
				if not typeParsing then		
				begin	
					chars := AddCR(chars, pos, tokenValue);
					pos:=pos-length(TokenValue);
					//Writeln('Add CR before TV due to tV, ', tokenValue );
					//Writeln('new pos: ', pos);
					bufferLength+=1;
				end;
		
//			Writeln('new pos: ', pos);
		end;
	
		//Special Cases: procedure, function, array, type, ;, 	
		if ( tokenType= kFunctionToken {(tokenValue= 'procedure') or (tokenValue= 'function')} ) then
		begin
		pos1:=pos;
		bracketFound:= False;
		quitProcedure:= False;
		bracketFlag:= true;
		//writeln('Entered procedure or function loop: tokenValue: ', tokenValue);
			//Parse function with / without parameters
			repeat GetFormatToken(chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
			 //writeln('In procedure loop: tokenValue: ', tokenValue, ' pos1: ', pos1);
			 	//If user used '(', and missed to use ')', then the formatter keeps parsing to look for ')', perhaps it make IDE wheel rolling forever. 
			 	//ToDO: It can released when formatter finds 'begin' as most likely 'begin' or 'var' is expected after procedure or function.
				if tokenValue='(' then
				begin
					bracketFound:= true;
					//bracketFlag:= true;
				//writeln('bracketFlag: ', bracketFlag);
				end;
			
				if tokenValue = ';' then // this is the case, when ';' . It's either in brackets of procedure or function or end of function without brackets
					if not bracketFound then // To make sure no brackets found so that quit loop as its end.	
					begin				
						quitProcedure:= true; 
						//pos1-=1;
					end;	
				if tokenValue=')' then
					bracketFlag:= false;
				
			until (quitProcedure) or not (bracketFlag) or (pos1 >= bufferLength); 
			
			//Continue to parse function after brackets
			if not quitProcedure then // If not quitProcedure means It has brackets or parameters in other words
			begin
				repeat GetFormatToken(chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
				   //	writeln('TOKENVALUE IN PROCEDURE OR FUNCTION LOOP2: ', tokenValue, 'pos1: ', pos1);
				   	//procedureFlag:= true;
				  	//writeln('procedureFlag: ', procedureFlag);
				  	if tokenValue=';' then
					begin
						//Set procedureFlag and keep parsing until last ';' in function
				  		//Save position of ';' and It's previousTokenValue in order to parse from ';', If it is the latest one.
				  		//TODO: Perhaps should save previousTokenType too
						lastSemicolon:=pos1; 
						lastPreviousTokenValue:= previousTokenValue;
					end;
				(*	else 
						if tokenValue=':' then
						begin
							GetFormatToken(chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue); // To get the data type of function, eg; function test():LongInt; Here 'LongInt'
							//writeln('TOKENVALUE IN PROCEDURE OR FUNCTION LOOP2: ', tokenValue, 'pos1: ', pos1);
						end
					else // Get out from the loop, If the token value is not one of the following function modifier
						if ( {(tokenType= kFunctionModifier) or} not ( (tokenValue = 'override') or (tokenValue = 'overload') or (tokenValue = 'cdecl') or (tokenValue = 'register') or (tokenValue = 'message') ) ) then
						begin
							procedureFlag:=false;
							//writeln('procedureFlag: ', procedureFlag);	
						end;*)


				until ( {(procedureFlag)} ( (tokenValue = 'var') or(tokenValue = 'type') or(tokenValue = 'const') or(tokenValue = 'begin') or(tokenValue = 'procedure') or
				(tokenValue = 'function') or(tokenValue = 'implementation') or (tokenValue = 'constructor') or (tokenValue = 'end') ) or (pos1 >= bufferLength) ); 
				//GetFormatToken(chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
				pos1:= lastSemicolon-1;
				changePreviousTokenValue:=true;

				//previousTokenValue:= lastpreviousTokenValue;
			end;
			
			pos:=pos1;
		end;
		
		if (tokenValue= 'array')  then
		begin
		pos1:=pos;
		//writeln('Entered array loop: tokenValue: ', tokenValue);
			repeat GetFormatToken(chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
			 
			until (tokenValue = ';') or (pos1 >= bufferLength);
			pos:=pos1;
		end;
		
		if ( (typeParsing) and (tokenValue = 'end') ) then
			typeParsing:= false;
			
		if (tokenValue= 'type')  then
			typeParsing:= true;

	
	until pos >= bufferLength;
	PascalCRFormatter:= chars;
end;


procedure HashInit;
begin
// Must be done SOMEWHERE
	HashInitTable(pascalHashTable, kOtherToken, 'FAIL');
	InitColorCodingTable;
end;

begin
	HashInit;
end.