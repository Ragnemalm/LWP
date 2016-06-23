{$mode macpas}
unit CFormatterUnit;
interface
uses
	MacOSAll, HashTableUnit;

function CFormatter(teRec: AnsiString ):AnsiString;
function CCRFormatter(teRec: AnsiString):AnsiString;

implementation

const
	CR = Char(13);
	LF = Char(10);
	TAB = Char(9);

var
	previousTokenType: Longint; {Ett steg tillbaka}
	previousTokenValue: AnsiString;
	cHashTable: HashRec;

// Token types
const
	kFunctionToken = 1;		// function, procedure
	kBeginEndToken = 2;		// begin/end/if/else/case/of - anything that affects structure)
	kReservedToken = 3;		// Reserved words that do NOT affect structure analysis
	kKnownTypeToken = 4;	// Integer, Longint, Real...
	kCommentToken = 5;		// {} (* *) // Can span several rows!
	kStringToken = 6;		// 'string' "c"
	kExpressionToken = 7;		// +,-,/,*,=,?
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
	//kExpressionToken =19;


procedure InitCColorCodingTable;
begin
	HashAddEntry(cHashTable, 'if', kBeginEndToken);
	HashAddEntry(cHashTable, 'else', kBeginEndToken);
	HashAddEntry(cHashTable, 'case', kBeginEndToken);
	HashAddEntry(cHashTable, 'for', kBeginEndToken); 
	HashAddEntry(cHashTable, 'while', kBeginEndToken); 
	HashAddEntry(cHashTable, 'repeat', kBeginEndToken); 
	HashAddEntry(cHashTable, 'until', kBeginEndToken);
	HashAddEntry(cHashTable, 'do', kBeginEndToken);
	HashAddEntry(cHashTable, 'switch', kBeginEndToken);
	
	HashAddEntry(cHashTable, '+', kExpressionToken);
	HashAddEntry(cHashTable, '-', kExpressionToken);
	HashAddEntry(cHashTable, '/', kExpressionToken);
	HashAddEntry(cHashTable, '*', kExpressionToken);
	HashAddEntry(cHashTable, '?', kExpressionToken);
	HashAddEntry(cHashTable, ':', kExpressionToken);
	HashAddEntry(cHashTable, '=', kExpressionToken);
	
	
	HashAddEntry(cHashTable, 'enum', kKnownTypeToken); 
	HashAddEntry(cHashTable, 'long', kKnownTypeToken);
	HashAddEntry(cHashTable, 'int', kKnownTypeToken);
	HashAddEntry(cHashTable, 'float', kKnownTypeToken);
	HashAddEntry(cHashTable, 'double', kKnownTypeToken);
	HashAddEntry(cHashTable, 'void', kKnownTypeToken);
	HashAddEntry(cHashTable, 'char', kKnownTypeToken);
	HashAddEntry(cHashTable, 'signed', kKnownTypeToken); 
	HashAddEntry(cHashTable, 'unsigned', kKnownTypeToken);
	HashAddEntry(cHashTable, 'register', kKnownTypeToken);
	
	
end;
var
pos, tokenStart, tokenEnd, tokenType: Longint;
tokenValue: AnsiString;
secondPreviousTokenType: LongInt;
secondPreviousTokenValue: AnsiString;

procedure GetCFormatToken(data: AnsiString; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);
var
	s: AnsiString;
	bufferLength:LongInt;
begin
	if Length(data) = 0 then Exit(GetCFormatToken);
	
	bufferLength:= Length(data);
	secondPreviousTokenValue:= previousTokenValue;
	secondPreviousTokenType:= previousTokenType;
	
	previousTokenValue:= tokenValue;
	previousTokenType:= tokenType;
	
	while (data[pos] in [TAB, ' ']) and (pos <= bufferLength) do pos := pos + 1;
	//if (pos >= bufferLength) then
	//	Exit(GetCFormatToken);
	tokenStart := pos;
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
		// 101006: Allows : for better C++ navigation.
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) and (pos < bufferLength) do pos := pos + 1;
// Maybe I should do the following too, to avoid inclusion of ":" in other cases? It will at least catch most irrelevant cases.
// But should NOT affect ObjC methods!
//		if data[pos-1] = ':' then pos := pos - 1; // ":" last is skipped
		tokenEnd := pos - 1;
		{Look up alphanumerical symbol}
		
		s := Copy(data, tokenStart, pos - tokenStart);
		s := LowerCase(s);
		tokenType := HashLookUpInteger(cHashTable, s);
		tokenValue := s;
		
	end
	else
	begin 
		//if (pos < bufferLength) then
		if data[pos] in [CR, LF] then
		begin
			//WriteLn('CRLF token');
			// Jag vill bara ha den SISTA - flera i rad är ointressanta
			while (data[pos] in [CR, LF]) and (pos < bufferLength) do
			begin
				//Writeln('pos: ', pos, ' bufferLength: ', bufferLength, ' CR or LF: ', data[pos]);
				pos := pos + 1;
			end;
			tokenEnd := pos-1;
			tokenType := kCRLFToken;
			tokenValue := data[pos-1];
//			pos := pos + 1;
		end
		else

		if data[pos] in ['{', '}'] then // Begin, end
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kBeginEndToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			//writeln('tV: ', tokenValue);
			pos := pos + 1;
		end
		else
		if data[pos] = '#' then // preprocessor junk
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kPreprocessorToken;
//			tokenValue := data[pos]; // Ord(data[pos]);

			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos;// - 1;
			
			//SetLength(tokenValue, tokenEnd - tokenStart);
			tokenValue:= Copy(data, tokenStart, tokenEnd-tokenStart);
			//BlockMove(@data[tokenStart], @tokenValue[1], tokenEnd - tokenStart);
			// MoveBytes
			
			pos := pos + 1;
		end
		else
		if data[pos] = '(' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kStartParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = ')' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kEndParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '/') then // Line-comment
		begin
			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos - 1;
			tokenType := kCommentToken;
			tokenValue := 'Comment';
			
			// Check for LWP specials
			if tokenEnd - tokenStart > 3 then
			begin
				tokenValue:= Copy(data, tokenStart, tokenEnd-tokenStart+1);
				//SetLength(tokenValue, tokenEnd - tokenStart + 1);
				//BlockMove(@data[tokenStart], @tokenValue[1], tokenEnd - tokenStart + 1);
				// MoveBytes
//				CheckSpecialComment(tokenValue);
			end;
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '*') then // Comment
		begin
			if (pos < bufferLength) then
				pos := pos + 2;
			while ( ((data[pos-1] <> '*') or (data[pos] <> '/')) and (pos < bufferLength) )do 
			begin
			pos := pos + 1;
			end;
			pos+=1;
			tokenEnd := pos-1;
			tokenType := kCommentToken;
			tokenValue := 'Comment';
		end
		else
		if data[pos] in ['+', '-', '*', '/', '=', '?'] then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kExpressionToken;
			tokenValue := data[pos]; 
			pos+=1;
		end
		else
		if (data[pos] = '"') then // String
		begin
			if (pos < bufferLength) then
				pos := pos + 1;
// Might get problems with \? Should handle that
			while not (data[pos] in ['"', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				if data[pos] = '\' then
					pos := pos + 1;
				pos := pos + 1;
			end;
			tokenEnd := pos;
			pos := pos + 1; // Skip "
			tokenType := kStringToken;

			//tokenStart := tokenStart; // + 1;
			{Must get string contents (for #include menu)}
			//tokenValue:= Copy(data, tokenStart, tokenEnd-tokenStart+1);


		end
		else
		if (data[pos] = '''') then // String (character)
		begin
			//writeln('tokenStart: ', tokenStart);
			(*if stringFlag then
			begin
				stringFlag:=false;
				stringEnd:=pos;
			end
			if  not (stringFlag) then
			begin
				stringFlag:=true;
				stringStart:= pos;
			end;*)
			
			if (pos < bufferLength) then
				pos := pos + 1;
			while (not (data[pos] in ['''', CR, LF]) and (pos < bufferLength)) do // When to stop?
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				if data[pos] = '\' then
					pos := pos + 1;
				
				pos := pos + 1;
			end;
			tokenEnd := pos;
			
			pos := pos + 1; // Skip '
			tokenType := kStringToken;
			
			tokenValue:= 'string';
			//tokenStart := tokenStart; // + 1;
			{Get string contents (not necessary for ')}
			//tokenValue:= Copy(data, tokenStart, tokenEnd-tokenStart-1);
			//writeln('tokenValue: ', tokenValue);
			//writeln();
	
		end
		else
		begin
		// Otherwise skip the symbol
		// Must do special case for most symbols! Or assume that they are single char!
		//	if data[pos] in ['+','-', ':', '=', '<', '>', '/', '*'] then // Single char token
		//	begin
		//	end;
			tokenEnd := tokenStart + 1;
			tokenType := kSingleCharToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			//writeln('*tV* ', tokenValue);
			pos := pos + 1;
			//while not (dataPtr in ['a'..'z', 'A'..'Z', '0'..'9', '_', CR, LF, ' ']) do pos := pos + 1;
		end;
	end;
 // writeln('Pos, tokenStart, tokenEnd, tokenType, tokenValue: ', pos, tokenStart,' ', tokenEnd, ' ', tokenType, ' ',tokenValue);

end;
var
	//elseFlag: Boolean;
	waitingForSemicolon: array of Boolean;
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

procedure ResetFlags(var level:LongInt);
begin
	ifWOBegin[level]:=false;
	elseWOBegin[level]:=false;
	loopWOBegin[level]:=false;
	elseWithBegin[level+1]:=false;
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
			SetLength(ifWOBegin, Length(ifWOBegin)*2);
			SetLength(elseWOBegin, Length(elseWOBegin)*2);
			SetLength(elseWithBegin, Length(elseWithBegin)*2);
			SetLength(loopWOBegin, Length(loopWOBegin)*2);
		end;
end;


type
	SwitchVariables = record
		caseSeen, caseWBegin, switchSeen, switchActive: Boolean;
 		switchLevel:LongInt;
 		switchParsing: Boolean;
	end;

var
	level:LongInt;
	gSwitch: array of SwitchVariables;

procedure IndentOnCRLF(var chars:AnsiString;  var bufferLength, pos: Longint; level: Longint); 
//Replace bufferlength with length(chars)
var
	pos1, diff, i: Longint;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
begin	
	// Vad är nästa token?
	//bufferLength:= Length(chars);
	//Writeln('Before Indent, chars: ', chars);
	//writeln('Length of chars: ', bufferLength);
	//writeln('level: ', level);
	//Writeln('A');
	pos1 := pos;
	//diff:=0
	GetCFormatToken(chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
	//writeln('tokenValue in IndentCRLF ', tokenValue);

	if tokenType = kBeginEndToken then
	begin
		if (tokenValue = '}') or (tokenValue = 'until') or (tokenValue = 'except') or (tokenValue = 'finally')
		or (tokenValue = 'private') or (tokenValue = 'public') then
		begin
			level -= 1; // Local modification only!		
			//writeln('Level down', level, ' in Indent due to ', tokenValue);
			
			if (tokenValue = '}') then
				if High(gSwitch)>=0 then
				with gSwitch[High(gSwitch)] do
				begin
					if (level = (switchLevel)) then
					begin
						level-=1;
						//writeln('Level down', level, ' in Indent due to Switch');
					end;
				end;
		end;
			
		if (tokenValue = 'else') then
		begin
			if elseWithBegin[level+1] then
		 	begin
		 		level-=1;
		 	 	//writeln('IFELSE level down in IndentCRLF becoz of elseWithBegin and level ',level);
			 	
		 	end;
		end;
		
		if (tokenValue='case') then
			with gSwitch[High(gSwitch)] do
			begin
				if caseSeen then
				begin
					if not caseWBegin then
						level-=1;
				end;
			end;
			
		(*if (tokenvalue='}') then
			if caseSeen then
			begin
				caseSeen:=false;
				if not caseWBegin then
					level-=1;
			end;*)
	end;
	
	//Writeln('LEVEL before indentation: ', level);
	{for i:=0 to level do
	begin
		writeln('ifWOBegin ', i, ' ', ifWOBegin[i]);
		writeln('elseWOBegin ', i, ' ', elseWOBegin[i]);
		writeln('elseWithBegin ', i, ' ', elseWithBegin[i]);
		writeln('loopWOBegin ', i, ' ', loopWOBegin[i]);

	end;}
	if level < 0 then
		level := 0;
		//Writeln('B');
	
	// Vad är nästa? Redan kollat, detta är CRFL som EJ följs av CRLF
	//Writeln('pos:', pos);
	pos1 := pos;
	//Writeln('tokenValue in IndentOnCRLF: ', tokenValue);
	// Hitta slut på indentering
	while (chars[pos1] in [' ', TAB]) and (pos1 < bufferLength) do pos1 := pos1 + 1;
	//Writeln('pos1:', pos1);
	// pos1-pos är nuvarande antal tecken
	// ersätts med "level" antal TAB
	diff := level - (pos1-pos);
	//WriteLn('Diff at CRLF: ', diff, ' at level ', level);
	if diff <= 0 then // mindre data - kopiera först, korta sedan    - take away uneccessary space/tab
	begin
		//BlockMoveData(@chars^[pos1], @chars^[pos1 + diff], bufferLength-pos1); //Replace with copy 
		chars:= Copy(chars, 1, pos - 1)+ Copy(#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9, 1, level) + Copy(chars, pos1, bufferLength-pos1+1);
		bufferLength += diff;
	end;
	if {pos1-pos < level} diff>0 then // mer data - läng först, kopiera sedan     - add more spaces/tabs
	begin
		chars:= Copy(chars, 1, pos-1) + Copy(#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9#9, 1, level) + Copy(chars, pos1, bufferLength-pos1+1);
		bufferLength += (diff-1);//(diff-1);
	end;
//		Writeln('LEVEL after indentation: ', level);

	bufferLength := Length(chars);
end;

var
	elseFlag: Boolean;
	
procedure ResetSwitchVariable(s: SwitchVariables);
begin
	with s do
	begin
		caseSeen:= false;
		caseWBegin:=false;
		switchSeen:=false;
		switchLevel:=0;
	end;
end;

var
bracketCount:LongInt;
bracketFound: Boolean;

procedure BracketCounter(chars:AnsiString; pos, bracketCount, bufferLength:LongInt);
begin
	repeat 
		GetCFormatToken(chars, pos, tokenStart, tokenEnd, tokenType, tokenValue);
		if (tokenValue= '(') then
			bracketCount+=1
		else
			if (tokenValue=')') then
				bracketCount-=1;
	
	until (bracketCount<=0) or (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength); 

end;

function CFormatter(teRec: AnsiString ):AnsiString;
var
	bufferLength, pos, pos1, i: LongInt;
	chars:AnsiString;

	waitingForCR: array of Boolean;
	pos2:LongInt;
	secondWaitForCR: Boolean;
	predictElse: Boolean;
	pauseSecondWaitForCR: Boolean;

	procedure FinishBracketsinLoops(chars:AnsiString; pos, bracketCount, bufferLength:LongInt);
	begin
		pos1:=pos;
		repeat
			repeat
				GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
			until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength); 
			bracketFound:= true;
			//writeln('For: tokenValue: ', tokenValue, ' bracketCount: ', bracketCount);
			if tokenValue='(' then
			begin
				bracketCount+=1;
			end;

			if tokenValue=')' then
			begin	
				bracketCount-=1;
				if bracketCount<=0 then
					bracketFound:= false;
				
			end;

		until not (bracketFound) or (pos1 >= bufferLength); 
		pos:=pos1;
	end;

begin
	chars:= teRec;
	bufferLength:=Length(chars);
	//writeln('bufferLength', bufferLength);
	pos:=1;
	level:=0;
	WriteLn('ENTER C FORMATTER');
	SetLength(waitingForSemicolon, kDefaultFlagArraySize);
	SetLength(waitingForCR, kDefaultFlagArraySize);
	SetLength(ifWOBegin, kDefaultFlagArraySize);
	SetLength(elseWOBegin, kDefaultFlagArraySize);
	SetLength(elseWithBegin, kDefaultFlagArraySize);
	SetLength(loopWOBegin, kDefaultFlagArraySize);
	SetLength(gSwitch, 0);
	//SetLength(offset, 20);
//	SetLength(elseFlag, 20);

	for i := 0 to kDefaultFlagArraySize do
	begin
		waitingForSemicolon[i] := false;
		waitingForCR[i] := false;
		ifWOBegin[i]:= false;
		elseWOBegin[i]:= false;
		elseWithBegin[i]:= false;
		loopWOBegin[i]:= false;
		//offset[i] := 0;
	end;
	elseFlag := false;

	
	repeat
		//if (pos <= bufferLength) then
		//begin
		GetCFormatToken(chars, pos, tokenStart, tokenEnd, tokenType, tokenValue);
		//writeln('Pos: ', pos , ' tokenStart: ', tokenStart,' tokenEnd: ', tokenEnd, ' tokenType: ', tokenType, 'tokenValue: ', tokenValue);
		//writeln(' level A ', level);
		case tokenType of
		
			kBeginEndToken:  //kBeginEndToken=2
				begin
					if {(tokenType=kRecordToken )} (tokenValue = '{') or (tokenValue = 'record') or (tokenValue = 'class') then
					begin
						level += 1;
						ResetFlags(level);
						//writeln('Level Up: ', level,  ' in kBiginToken due to: ', tokenValue);
						
						// Save the level when SwitchSeen, which will be used to level down in the end of switch.
						if (tokenValue = '{') then
							if High(gSwitch)>=0 then
							with gSwitch[High(gSwitch)] do
							begin
								if switchSeen then
								begin
									switchLevel:= level;
									switchSeen:= false;
									switchActive:= true;
									//writeln('Switch Level: ', switchLevel);

								end;
							end;
					end;
					if {(tokenType = kUntilToken)} (tokenValue = '}') or (tokenValue = 'until') then
					begin
							level -= 1;
							//writeln('Level down: ', level,  ' in kBiginToken due to: ', tokenValue);
								//Here Level down, when level = switchLevel.
								if (tokenValue = '}') then
								begin
									if High(gSwitch)>=0 then
									with gSwitch[High(gSwitch)] do
									begin
										//writeln('switchSeen: ', switchSeen, ' switchLevel: ', switchLevel);
										if (level = (switchLevel)) then
										begin
											level-=1;
											//reduce the length of gSwitch when the switch ends.
											SetLength(gSwitch, Length(gSwitch)-1);
										end;
									end;
								end;
								
							if (tokenValue='}') then
							begin
								pos1:=pos;
								repeat GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
								until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength); 
								//writeln('tokenValue in kBeginEnd: ', tokenValue);
								// This is for level down after '}', followed by 'else', (eg: '} else').
								if  (tokenValue <> 'else') then
									if FormatIFElse(level) then
									begin
										repeat 
											if FormatIFElse(level) then
											begin
												//ResetFlags(level);
												level-=1;
												//writeln('IFELSE level down in kBeginEndToken and level ',level);
											end;
										until not FormatIFElse(level);
									end;
									
									//If tokenValue is 'else' then It will check for the case (tikenValue = 'else'), which make level up unnecessarily. So null token value
									//if (tokenValue= 'else') then
										tokenValue:=' ';
							end;
								
					end;
					if {(tokenType = kCaseToken)} {(tokenValue = 'case')} (tokenValue = 'try') or (tokenValue = 'repeat') then // Anything that will lead to "end"
					begin
						level += 1;
						//writeln('level up in etry, repeat', level);

						ResetFlags(level);
					end;	
					
					// If tV = 'case', and caseSeen previously, level down.
					if (tokenValue='case') then
						with gSwitch[High(gSwitch)] do
							begin
								if caseSeen then
								begin
									caseSeen:=false;
										level-=1;
								end;
							end;
						
					// If tv='switch', then increase the length of gSwitch, and set switchSeen.
					if (tokenValue='switch') then
					begin
						SetLength(gSwitch, Length(gSwitch)+1);
						//writeln('length of gswitch: ',Length(gSwitch) );
						ResetSwitchVariable(gSwitch[High(gSwitch)]);
						with gSwitch[High(gSwitch)] do
						begin
							switchSeen:= true;
							switchActive:= true;
							//writeln('Switch seen: ', switchSeen);
						end;
					end;

					
					if {( (tokenType = kIfElseToken) or (tokenType = kForToken) )} (tokenValue = 'case') or (tokenValue = 'if') or (tokenValue = 'for') 
					or (tokenValue = 'else') or (tokenValue = 'do') or (tokenValue = 'while') then 
						// Dessa borde INTE indentera om det följs av begin... men det låter jobbigt att parsa efter
					begin
						//if tv= 'if', 'for', 'while' then search untill all brackets end using procedure 'FinishBracketsinLoops'.
						if tokenValue = 'if' then
						begin
							if elseWithBegin[level+1] then
						 		elseWithBegin[level+1]:=false;

							pos1:=pos;
							bracketFound:= False;
							bracketCount:=0;
						
							FinishBracketsinLoops(chars, pos1, bracketCount,bufferLength);
							pos:=pos1;
							
							
							
							//Search again for '{'
							repeat 
								GetCFormatToken({dataPtr} chars,  pos1, tokenStart, tokenEnd, tokenType, tokenValue);
								//writeln('IN IF LOOP1: ', 'tokenValue: ', tokenValue, ' tokenType: ', tokenType, ' pos1: ', pos1);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength);  
							//writeln('**tV**', tokenValue);
							//If 'if', 'for', 'while', 'do' followed by 'if..then' then do not set waitingForSemicolon, which make level down. Use pauseSecondWaitForCR for postpone that.
							if ( (tokenValue = 'if') or (tokenValue = 'for') or (tokenValue = 'while') or (tokenValue = 'do') or (tokenValue = 'case') ) then
							begin
								pauseSecondWaitForCR:= true;
								//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
							end
							else
							begin
								pauseSecondWaitForCR:= false; 
								//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
							end;
							
							if tokenValue <> '{' then
							begin
								level += 1;
								//writeln('level up in if', level);

								ResetFlags(level);
								ifWOBegin[level]:= true;
								//writeln('IN IF LOOP, LEVEL UP: ', level, ' DUE TO ', tokenValue);
								//IndentOnCRLF(chars, caseChars, bufferLength, pos, level);
								if not (pauseSecondWaitForCR) then
								begin
									CheckLevel(level);
									waitingForSemicolon[level] := true;
								end;
	
							end;
							
						end
						else if {(tokenType=kForToken)} (tokenValue = 'for') or (tokenValue='while') {or (tokenValue = 'do')} then
						begin
							pos1:=pos;
							bracketFound:= False;
							bracketCount:=0;
						
							FinishBracketsinLoops(chars, pos1, bracketCount,bufferLength);
							pos:=pos1;
							//Search again for '{'
							repeat
								GetCFormatToken({dataPtr} chars,  pos1, tokenStart, tokenEnd, tokenType, tokenValue)
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength);  
							//writeln('TOKEN VALUE: ', tokenValue);
							//If 'if', 'for', 'while', 'with' followed by 'for, or while, or with ...do ' then do not set waitingForSemicolon. Use pauseSecondWaitForCR for postpone that.
							if ( (tokenValue = 'if') or (tokenValue = 'for') or (tokenValue = 'while')  or (tokenValue = 'do') or (tokenValue = 'case') ) then
							begin
								pauseSecondWaitForCR:= true;
								//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
							end
							else
							begin
								pauseSecondWaitForCR:= false; 
								//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
							end;

							if tokenValue <> '{' then
							begin
								level+=1;
								//writeln('level up in loop', level);
								ResetFlags(level);
								loopWOBegin[level]:=true;
								if not (pauseSecondWaitForCR) then
								begin
									CheckLevel(level);
									waitingForSemicolon[level] := true;
								end;
							end;
						end
						else if (tokenValue = 'else') then
						begin
							// level down if already 'else' seen in the same level.
							if elseWithBegin[level+1] then
						 	begin
						 		level-=1;
						 	 //	writeln('IFELSE level down in kBeginEndToken becoz of elseWithBegin and level ',level);
							 	IndentOnCRLF(chars, bufferLength, pos, level);
						 	end;
							pos1:=pos;
							//Search for '{'
							repeat GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength);   
							//Writeln('tokenValue after else: ', tokenValue);
							//If 'if', 'for', 'while', 'with' followed by 'for, or while, or do ' then do not set waitingForSemicolon. Use pauseSecondWaitForCR for postpone that.
							if ( (tokenValue = 'if') or (tokenValue = 'for') or (tokenValue = 'while') or (tokenValue = 'do') or (tokenValue = 'case') ) then
							begin
								pauseSecondWaitForCR:= true;
								//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
							end
							else
							begin
								pauseSecondWaitForCR:= false; 
								//Writeln('pauseSecondWaitForCR: ', pauseSecondWaitForCR);
							end;
	
							if tokenValue <> '{' then
							begin
								level+=1;
								ResetFlags(level);
								//writeln('level up in else', level);
								elseWOBegin[level]:= true;
								elseFlag:= True; // It is used to level down when else if() statement;
								if not (pauseSecondWaitForCR) then
								begin
									CheckLevel(level);
									//Set both waitingForSemicolon and waitingForCR. One of them will be used and the other will be reset. 
									waitingForSemicolon[level] := true;
									//waitingForCR[level] := true;
								end;
			
							end
							else 
							begin
								if (tokenValue = '{') then
									elseWithBegin[level+1]:= true;
							end;
		
						end
						else if (tokenValue= 'case') then
							begin
								//Set CaseSeen
								with gSwitch[High(gSwitch)] do
								begin
									caseSeen:= true;
								end;
								pos1:=pos;
								//Search until ':'
								repeat GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
								until ((not (tokenType in [kCRLFToken, kCommentToken])) or (tokenValue=':') ) or (pos1 >= bufferLength);
								//Search for more Tv, which should be case name.
								repeat GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
								until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength);
								// level up for every case.
								level+=1;
								
							end;
						
					end;
					
			end;// kBeginEndToken
			
			kSingleCharToken: //kSingleCharToken=8
			begin
				if tokenValue = ';' then
					if level <= High(waitingForSemicolon) then
					begin
						if waitingForSemicolon[level] then
						begin
							waitingForSemicolon[level] := false;
							level -= 1;
							//Writeln('Level down due to wFsemicolon: ', level);

						end;
						
						//Check following to level down at ';'
						if (ifWOBegin[level] or elseWOBegin[level] or loopWOBegin[level]) then 
						begin
							//writeln('**ifWOBegin**.', ifWOBegin[level], ' **elseWOBegin**', elseWOBegin[level]);
							pos1:=pos;
							repeat GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength); 
							//check if tv<> 'else'.
							if (tokenValue<>'else') then
							begin
								//if ifWOBegin[level] and elseFlag (else was Seen before) then level down, eg: ..else (elseFlag:=true) if printf();
								if (ifWOBegin[level]) then
									if elseFlag then
									begin
										elseFlag:=false;
										level-=1;
									end;
								if FormatIFElse(level) then
								begin
									//repeat until FormatIFElse is false.
									repeat 
										if FormatIFElse(level) then
										begin
											//ResetFlags(level);
											level-=1;
											//writeln('IFELSE level down in kSingleCharToken and level ',level);
										end;
									until not FormatIFElse(level);
								end;
							end;
						end;
						
					end;
			end;
			
			kCRLFToken:// kCRLFToken=18 , calls for every line
			begin	
				IndentOnCRLF(chars, bufferLength, pos, level);	
			end;
			kCommentToken:
				IndentOnCRLF(chars, bufferLength,  pos, level);
		end; // case
		
	until pos >= bufferLength;

	CFormatter:= chars;

end;

function AddCR(chars: AnsiString; pos: LongInt; tokenValue: AnsiString):AnsiString;
var
bufferLength: LongInt;
begin
	bufferLength:= Length(chars);
	AddCR := Copy(chars, 1, pos-length(tokenValue)-1)+ #13 + Copy(chars, pos-length(tokenValue), bufferLength+1);
end;


function CCRFormatter(teRec: AnsiString):AnsiString;
var
	bufferLength, level: Longint;
	//err: OSErr;
	chars: AnsiString;
	pos, pos1, tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
	bracketFound: Boolean;
	//To count and finish all the brackets
	procedure FinishBracketsinLoops1(chars:AnsiString; pos, bracketCount, bufferLength:LongInt);
	begin
		pos1:=pos;
		repeat
			repeat GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue)
			until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength); 
			bracketFound:= true;
			//writeln('For: tokenValue: ', tokenValue);
			if tokenValue='(' then
			begin
				bracketCount+=1;
			end;

			if tokenValue=')' then
			begin	
				bracketCount-=1;
				if bracketCount<=0 then
					bracketFound:= false;
				
			end;
			//writeln('For: tokenValue: ', tokenValue, ' bracketCount: ', bracketCount, 'pos1: ', pos1);

		until not (bracketFound) or (pos1 >= bufferLength); 
		pos:=pos1;
	end;
begin
	WriteLn('ENTER CR FORMATTER');

	// Get the text from the text field
	chars := teRec;//^.text;
	
	// Change chars to lower case
	LowerCase(chars);
	
	bufferLength := Length(chars);
	pos := 1;
	level := 0;
	bracketCount := 0;

	repeat	
		//Writeln('previousTokenValue: ',previousTokenValue);
		GetCFormatToken(chars, pos, tokenStart, tokenEnd, tokenType, tokenValue);
		//Writeln('tV: ',tokenValue, ' tT: ', tokenType, ' pos: ', pos);
		
		//Add CR after tokenValue
		if ( ( (previousTokenValue=';') {or (previousTokenValue=':')} or (previousTokenValue='{') or (previousTokenValue='}') or 
		(previousTokenValue=')') or  (previousTokenValue='else') ) and (not (tokenValue= #13)) ) then
		begin
			//writeln('1 previousTokenValue: ', previousTokenValue, ' tokenValue: ', tokenValue);

			// Should not add CR between
			//1. ') and ;',  ') and (', ') and ,', ') and ;', ') and :'.
			//2.'kKnownType and ) , but not followed by {'. Ex1. : (void) {, need CR after (void). Ex2. k=(void)ptr, no need CR after (void)
			//3. '}while', '} and ;', '} and ,'.
			//4. ')kExpresstionType'.
			if (   not (  ( (previousTokenValue=')') and ( (tokenValue= '(') or (tokenValue= ')') or (tokenValue= ',') or (tokenValue= ';') or (tokenValue= ':')  ) )  or 
			( (secondPreviousTokenType = kKnownTypeToken) and (previousTokenValue=')') and (not(tokenValue= '{'))) or
			( (previousTokenValue='}') and ( (tokenValue= 'while') or 	(tokenValue= ';') or (tokenValue= ',') ) ) or
			 ( (previousTokenValue=')') and (tokenType= kExpressionToken))     )   )then
			begin
				chars := AddCR(chars, pos, tokenValue);
				pos:=pos-length(TokenValue);
				bufferLength+=1;
			end;
			
			//add CR between ') and {'
			{if ( (previousTokenValue=')' ) and (tokenValue='{') ) then
			begin
				writeln('2 previousTokenValue: ', previousTokenValue, ' tokenValue: ', tokenValue);
				chars := AddCR(chars, pos, tokenValue);
				pos:=pos-length(TokenValue);
				bufferLength+=1;
			end;}
		end
		//Add CR before tokenValue
		else 
		if ( (not (previousTokenValue=#13)) and ( (tokenValue='{') or (tokenValue='}') ) ) then
		begin
			//writeln('2 previousTokenValue: ', previousTokenValue, ' tokenValue: ', tokenValue);	
			chars := AddCR(chars, pos, tokenValue);
			pos:=pos-length(TokenValue);
			bufferLength+=1;
		end;
		//if tv='for', 'if', 'while', have to pass untill all brackets end.
		if ( (tokenValue= 'for') or (tokenValue= 'if') or (tokenValue= 'while') ) then
		begin
			pos1:=pos;
			bracketFound:= False;
			bracketCount:=0;
			(*repeat
				repeat GetCFormatToken({dataPtr} chars, pos1, tokenStart, tokenEnd, tokenType, tokenValue) // Skip over CRLF and comments
				until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength); 
				bracketFound:= true;
				writeln('For: tokenValue: ', tokenValue, ' bracketCount: ', bracketCount);
				if tokenValue='(' then
				begin
					bracketCount+=1;
					//bracketFlag:= true;
				//writeln('bracketFlag: ', bracketFlag);
				end;
		
				if tokenValue=')' then
				begin	
					bracketCount-=1;
					if bracketCount=0 then
						bracketFound:= false;
					
				end;
				
			until not (bracketFound) or (pos1 >= bufferLength); *)
			FinishBracketsinLoops1(chars, pos1, bracketCount,bufferLength);
			pos:=pos1;	
		end;
		
		if (tokenValue= '(') then
		begin
			pos1:=pos;
			
			bracketFound:= False;
			bracketCount:=0;
			//bracketCount+=1;
			//writeln('bracketCount: ', bracketCount, ' pos: ', pos);
			FinishBracketsinLoops1(chars, pos1, bracketCount,bufferLength);
			pos:=pos1;
		end;
		
	until pos >= bufferLength;
	CCRFormatter:= chars;
end;
	

procedure HashInit;
begin
// Must be done SOMEWHERE
	HashInitTable(CHashTable, kOtherToken, 'FAIL');
	InitCColorCodingTable;
end;

begin
HashInit;


end.

