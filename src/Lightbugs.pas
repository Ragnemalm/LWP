// Debugger interface
// Works close together with Breakpoints.pas, LWPEdit.pas and Backtrace.pas.
// Breakpoints keeps a list of breakpoints for each window.
// LWPEdit shows breakpoints and current position.
// Backtrace deals with the parsing of input for variable display.
// This unit communicates with GDB, handles the display in the Lightbugs window, and extracts information from its responses (although this resonsability is mostly left to Backtrace.pas).

// One more kind to catch:
//ParseReplyLLDB parses "test(11280,0xa04d31a8) malloc: *** error for object 0x1b9b000: pointer being freed was not allocated"
//ParseReplyLLDB parses "*** set a breakpoint in malloc_error_break to debug"


{$mode macpas}
unit Lightbugs;
interface
uses
	strutils, MacOSAll, ProcessUtils, LWPGlobals, TransDisplay, FileUtils, // SkelView,
	Console, TransSkel4, Settings, cprocintf, UtilsTypes,
	Splitter, Scroller, QDCG, Backtrace, SetVariableDialog,
	SimpleParser, sysutils;

procedure DoDebugMenu (item: integer); {Menu handler}
procedure BugsRun;
function BugsRunning: Boolean;

procedure BugsRunWithDebug(theSpec: FSSpecString);
procedure BugsChangedBreakpoints(theWind: WindowPtr; line: Longint; add: Boolean);
procedure BugsRunTo(theWind: WindowPtr; line: Longint); // Set temporary breakpoint and run
procedure BugsInit;
procedure BugsShowWindow;
procedure BugsShowObserveWindow;
procedure BugsShowGDBWindow;

//procedure BugsGDB;

const
	kBugsStepOver = 0;
	kBugsStepIn = 1;
	kBugsStepOut = 2;

procedure BugsDoStep(cmd: Longint);
procedure BugsStop;

var
	OKtoSetBreaks : boolean = true;

// Access local variables
function BugsGetPrompt: AnsiString;
function BugsDebuggerIsGDB: Boolean;

implementation

uses
	Breakpoints, LWPEdit, AlertsUtils, AboutWindowUnit, DirectGDB;

var
//	bugsWind: WindowPtr;
	bugs1Wind: WindowPtr;
	observeWind: WindowPtr;
//	directgdbWind: WindowPtr;
	
	gBugsRunning: Boolean;
	canNotContinue: Boolean;

// NEW THINGS for switching between GDB and LLDB
// These are set by BugsRunWithDebug
var
	lengthDBName: Longint = 3; // 3, 4
	debugName: AnsiString = 'gdb';
	gDebuggerPrompt: AnsiString = '(gdb) ';
	debugPath: AnsiString;

	gDebuggerIsGDB: Boolean = true;
	gPascalMode: Boolean = false;

// Access local variables
function BugsGetPrompt: AnsiString;
begin
	BugsGetPrompt := gDebuggerPrompt;
end;
function BugsDebuggerIsGDB: Boolean;
begin
	BugsDebuggerIsGDB := gDebuggerIsGDB;
end;

procedure ResizeVariables; forward;

// In some cases it is sufficient to build a list of simplistic tokens
// using a few delimiter characters
// Variant of SplitStringToList with other set.
// Make a common one with the list as parameter? Yes!
function ParseToWords(s1: AnsiString): StringArr;
var
	i: Longint;
	theStrings: StringArr;
	s, returnedLine: AnsiString;
begin
	SetLength(theStrings, 0);
	s := s1;
	i := 1;
	while i < Length(s) do
	begin
		if s[i] in [#10, #13, ' '] then
		begin
			// CRLF first on line, ignored
			if i = 1 then
			begin
				Delete(s, 1, 1);
				i := 1;
			end
			else
			// Not first in line, save line
			begin
				returnedLine := Copy(s, 1, i-1);
				Delete(s, 1, i);
//				WriteLn('TRÄFF i ', i, ':"', returnedLine, '"');
				SetLength(theStrings, Length(theStrings)+1);
				theStrings[Length(theStrings)-1] := returnedLine;
				i := 1;
			end
		end
		else
			i := i + 1;
	end;
	
	// No more CRLF, save the rest if not empty
	if s <> '' then
	begin
		SetLength(theStrings, Length(theStrings)+1);
		theStrings[Length(theStrings)-1] := s;
	end;
	return theStrings;
end;


procedure UpdateExpressions; forward;

type
	FileNameRec = record
		name: AnsiString;
		fileName: AnsiString;
		value: AnsiString;
		varType: AnsiString;
		// type?
		typeId: Longint; // array/pekare/record borde noteras
		// last known value?
	end;
var
	globalVariables: array of VarInfoRec; // FileNameRec;
	localVariables: array of VarInfoRec; // FileNameRec;

// Type numbers - moved to Backtrace
(*const
	kScalarOrUnknown = 0;
	kArray = 1;
	kPointer = 2;
	kRecord = 3;
	kObject = 4;
	kStringPointer = 5;*)

// Obsolete
procedure ParseType(ptypeReply: AnsiString; var typeNumber: Longint); // Skall till fältet ???
// ptypeReply är utan inledande och avslutande skrot
// Identify what type it is: scalar, array, pointer, record
var
	i: Longint;
	words: StringArr;
begin
	WriteLn(ptypeReply, ' should be parsed. --------------');
	
	if Copy(ptypeReply, 1, 5) = '^Char' then
	begin
		WriteLn(ptypeReply, ' It is a string pointer! --------------');
		typeNumber := kStringPointer;
	end
	else
		if ptypeReply[1] = '^' then
		begin
			WriteLn('It is a pointer! --------------');
			typeNumber := kPointer;
			// Fånga namnet? Men det har vi nog redan?
		end
	else
		if Copy(ptypeReply, 1, 5) = 'array' then
		begin
			WriteLn('It is an array! --------------');
			i := 8;
			while ptypeReply[i] in ['0'..'9'] do
				i+=1;
			WriteLn('It starts at ', Copy(ptypeReply, 8, i-8), ' --------------');		
			typeNumber := kArray;
//			arrayBase := 
		end
	else
	begin
		WriteLn('Trying ParseToWords on ', ptypeReply);
		words := ParseToWords(ptypeReply);
		WriteLn('ParseToWords gave ', Length(words), ' words.');
		// Kolla att words är lång nog
		if High(words) < 4 then
		begin
			WriteLn('It is probably a scalar since words is too short --------------');
			typeNumber := kScalarOrUnknown;
		end
		else
			if words[4] = 'record' then
			begin
				WriteLn(words[2], ' is an record ', ' --------------');
				typeNumber := kRecord;
			end
			else
			begin
				WriteLn('It is probably a scalar since ', words[4], ' was not record --------------');
				typeNumber := kScalarOrUnknown;
			end;
	end;
end;


	procedure BugsEnable;
	begin
		gBugsRunning := true;
		canNotContinue := false;
		EnableMenuItem(debugMenu, 4);
		EnableMenuItem(debugMenu, 5);
		EnableMenuItem(debugMenu, 6);
		EnableMenuItem(debugMenu, 7);
	end;

	procedure BugsDisable;
	begin
		gBugsRunning := false;
		canNotContinue := false;
		DisableMenuItem(debugMenu, 4);
		DisableMenuItem(debugMenu, 5);
		DisableMenuItem(debugMenu, 6);
		DisableMenuItem(debugMenu, 7);
	end;

// For the already available globals, get current values
procedure UpdateGlobalsValues;
var
	i: Longint;
	repl: AnsiString;
begin
	for i := 0 to High(globalVariables) do
	begin
		repl := ReadToPrompt(gCurrentProcess, 'print ' + globalVariables[i].name, gDebuggerPrompt);
		repl := Copy(repl, 1, Length(repl) - 4 - lengthDBName); // Remove (gdb)
(*		for j := 1 to Length(repl) do
			if repl[j] = '=' then
			begin
				repl := Copy(repl, j+2, Length(repl));
				break;
			end;*)
		globalVariables[i].value := repl;
		WriteLn('repl = ', repl, ' for ', globalVariables[i].name);
		globalVariables[i].value := ParseVariable(repl);
	end;
end;

// Called (once only) at launch of Pascal program. Globals are found with "info variables" and parsed here.
procedure ParseGlobals(s: AnsiString);
var
	l, words: StringArr;
	currentFile: AnsiString;
	i, k: Longint;
begin
	currentFile := 'None';

	// Delete everything after "Non-debugging"
	k := pos('Non-debugging', s);	// stuff after this is junk for us
	if k > 0 then
		delete(s, k, length(s) - k);

	l := SplitStringToList(s);
//	WriteLn('List length = ', Length(l));
	for i := Low(l) to High(l) do
	begin
(*		if l[i] = 'Non-debugging symbols:' then
		begin
			WriteLn('Non-debugging symbols found at ', i);
			Exit(ParseGlobals);
		end;*)
		
		words := ParseToWords(l[i]);
		if Length(words) >= 2 then
			if words[0] = 'File' then
			begin
				currentFile := words[1];
				WriteLn('Current file: ', currentFile);
			end;
		
		if Length(words) >= 3 then
		begin
			if words[0] = 'static' then // How about non-static variables?
			begin
				SetLength(globalVariables, Length(globalVariables)+1);
				globalVariables[High(globalVariables)].name := words[1];
//				globalVariables[High(globalVariables)].fileName := currentFile;
//				WriteLn('Found global variable: ', words[1], ' in ', currentFile);
				WriteLn('type: ', words[3]); // Borde ta resten av raden
				
				// Get type
				globalVariables[High(globalVariables)].varType := Copy(l[i], Length(words[0]) + Length(words[1]) + Length(words[2]) + 3, Length(l[i]));
			end;
		end;
	end;
end;

var
	gParseScopeNeeded: Boolean; // Do we need to run UpdateState?
	gParseScopeTimer: EventLoopTimerRef;

// Shall return file name, and NOT read the file.
procedure ParseReplyBAD(s: AnsiString; var filename: AnsiString; var linenumber: Longint; var linenumberstring: AnsiString);
var
	l: StringArr;
	path, rownumber, charnumber: AnsiString;
	i, p, oldp, rowNum, err: Longint;
	//fileSpec: FSSpecString;
	pos, tokenStart, tokenEnd, tokenType, editIndex: Longint;
	tokenValue: AnsiString;
begin
	filename := '';
	linenumber := 0;
	linenumberstring := '';

		// Parse s!
		// Likely contents:
		// SOMETIMES Breakpoint 2, ZAP (I=1) at forever.pas:10
		// SOMETIMES ZAP (I=0) at forever.pas:9
		// ALWAYS  /Users/ingemar/Lightweight IDE/The Bug/forever.pas:10:110:beg:0x189a
		// SOMETIMES Program exited normally. Done!
		
		// Sometimes (step):
		// 10		data := 'Hej hopp 123 -456 3.4 fil.namn'; (Only line number useful!)

// 1) Starts with space? Then it is an info-row. No, SUB SUB!
//    - Path up to ":"
//    - Row number
//    - Some other numberr???
//    - Some more than I don't understand yet.
// 2) Starts with Breakpoint?
//    - Breakpoint number, comma.
//    - File name, position
// 3) Starts with something else!
//    - Function name
//    - Argument in parentheses, should be saved, show
//    - File name, row number

// LLDB: * thread #1: tid = 0x5c41a, 0x00010f67 SimpleParserTest`PASCALMAIN + 23 at SimpleParserTest.pas:10, queue = 'com.apple.main-thread', stop reason = one-shot breakpoint 1
// GDB: Temporary breakpoint 1, PASCALMAIN () at SimpleParserTest.pas:10

// But for LLDB...
// Typical lines:
// At breakpoint:
//Process 3571 stopped
//* thread #1: tid = 0x42e42, 0x00011031 SimpleParserTest`PASCALMAIN + 225 at SimpleParserTest.pas:15, queue = 'com.apple.main-thread', stop reason = step over
//    frame #0: 0x00011031 SimpleParserTest`PASCALMAIN + 225 at SimpleParserTest.pas:15
//   12  		repeat
//   13  			SimpleParserGetToken(data, pos, tokenStart, tokenEnd, tokenType, tokenValue);
//   14  			WriteLn('"', tokenValue, '", ', tokenType);
//-> 15  		until pos >= Length(data);
//   16  	
//   17  		WriteLn('GDB style');
//   18  		data := 'Breakpoint 3 at 0x1164d: file test.pas, line 11.';
// 1) Starts with *: Search for "at", then get file name and line number!
// What more?
// -> shows line number only... Not so interesting.

// At step: Same! :)

// Lacks detection of end of program!
// Program end in GDB:
// Program exited? (Important! Old GDB vs new!)
// Program end in LLDB:
// Process 7858 exited with status = 0 (0x00000000)
// Should also detect crashes!
// Don't quit the debugger until something else happens, like a compilation. "run" or "continue" as far as possible.

	l := SplitStringToList(s);
	for i := 0 to High(l) do
	if Length(l[i]) > 0 then
	begin
HelpAppendLn('ParseReply parses "'+l[i]+'"');
		pos := 1;
		SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue);
		
		if tokenValue = #26 then
		begin
		// Old parser - parses "standard info line" of GDB
			p := 3;
			while p < Length(l[i]) do // Search to ':'
			begin
				p := p + 1;
				if l[i][p] = ':' then Leave;
			end;
//			WriteLn('Found : at ', p);
			path := Copy(l[i], 3, p-3);
			WriteLn('Found path = ', path);
			oldp := p;
			while p < Length(l[i]) do // Search to ':'
			begin
				p := p + 1;
				if l[i][p] = ':' then Leave;
			end;
			rownumber := Copy(l[i], oldp+1, p-oldp-1);
			WriteLn('Found row number = ', rownumber);
			Val(rownumber, rowNum, err);
			if err <> 0 then
			begin
				StringToNum(rowNumber, rowNum); // Always works (but may generate strange values)
				WriteLn('GARBAGE WARNING: ', rowNumber);
			end;
			oldp := p;
			while p < Length(l[i]) do // Search to ':'
			begin
				p := p + 1;
				if l[i][p] = ':' then Leave;
			end;
			charnumber := Copy(l[i], oldp+1, p-oldp-1);
			WriteLn('Found char number = ', charnumber);
			
			// Return result!
			filename := path;
			linenumber := rowNum;
			linenumberstring := rowNumber;
		end;
		
		if (tokenValue = '*') or (tokenValue = 'Breakpoint') or (tokenValue = 'Temporary') then // * for LLDB, "Breakpoint" for GDB. Then this works for both?
		begin
			// Search for "at"
			repeat
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue);
			until (pos >= Length(l[i])) or (tokenValue = 'at');
			if pos < Length(s) then
			begin
				// Now get:
				// Filename (may include "-" etc!)
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, filename);
				// Set gPascalMode depending on current file type!
				gPascalMode := GetExtensionType(filename) = kExtTypePascal;
				// Colon
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue);
				// Row number
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, linenumberstring);
				Val(linenumberstring, linenumber);
				HelpAppendLn('Found '+filename+' at '+linenumberstring);
				if Length(filename) > 0 then
				if linenumber > 0 then
				begin
					editIndex := FindOrOpen(fileName, debugPath);
					if editIndex > 0 then
					begin
						fileName := GetPathFromIndex(editIndex);
//						UpdateFinger(fileName, linenumber);
//						gParseScopeNeeded := true;
					end;
				end;

			end;
//			Exit(ParseReply);
		end;
//		else
//			HelpAppendLn('Found nothing');
	end;
end;




// OLD parser, works for GDB
procedure ParseReply(s: AnsiString; var filename: AnsiString; var linenumber: Longint; var linenumberstring: AnsiString);
var
	l: StringArr;
	path, rownumber, charnumber: AnsiString;
	i, p, oldp, rowNum, err: Longint;
	//fileSpec: FSSpecString;
	pos, tokenStart, tokenEnd, tokenType, editIndex: Longint;
	tokenValue: AnsiString;
begin
	filename := '';
	linenumber := 0;
	linenumberstring := '';

	// Parse s!
// Likely contents:
// SOMETIMES Breakpoint 2, ZAP (I=1) at forever.pas:10
// SOMETIMES ZAP (I=0) at forever.pas:9
// ALWAYS  /Users/ingemar/Lightweight IDE/The Bug/forever.pas:10:110:beg:0x189a
// SOMETIMES Program exited normally. Done!

// Sometimes (step):
// 10		data := 'Hej hopp 123 -456 3.4 fil.namn'; (Only line number useful!)

// 1) Starts with space? Then it is an info-row. No, SUB SUB!
//    - Path up to ":"
//    - Row number
//    - Some other numberr???
//    - Some more than I don't understand yet.
// 2) Starts with Breakpoint?
//    - Breakpoint number, comma.
//    - File name, position
// 3) Starts with something else!
//    - Function name
//    - Argument in parentheses, should be saved, show
//    - File name, row number
// GDB: Temporary breakpoint 1, PASCALMAIN () at SimpleParserTest.pas:10
// Program end in GDB:
// Program exited? (Important! Old GDB vs new!)

	l := SplitStringToList(s);
//	WriteLn('List length = ', Length(l));
	for i := Low(l) to High(l) do
	begin
		WriteLn('Parsing reply "', l[i], '"');
		
		if Ord(l[i][1]) = 26 then // Double SUB = standard info line
		begin
			p := 3;
			while p < Length(l[i]) do // Search to ':'
			begin
				p := p + 1;
				if l[i][p] = ':' then Leave;
			end;
//			WriteLn('Found : at ', p);
			path := Copy(l[i], 3, p-3);
			WriteLn('Found path = ', path);
			oldp := p;
			while p < Length(l[i]) do // Search to ':'
			begin
				p := p + 1;
				if l[i][p] = ':' then Leave;
			end;
			rownumber := Copy(l[i], oldp+1, p-oldp-1);
			WriteLn('Found row number = ', rownumber);
			Val(rownumber, rowNum, err);
			if err <> 0 then
			begin
				StringToNum(rowNumber, rowNum); // Always works (but may generate strange values)
				WriteLn('GARBAGE WARNING: ', rowNumber);
			end;
			oldp := p;
			while p < Length(l[i]) do // Search to ':'
			begin
				p := p + 1;
				if l[i][p] = ':' then Leave;
			end;
			charnumber := Copy(l[i], oldp+1, p-oldp-1);
			WriteLn('Found char number = ', charnumber);
			
			// Return result!
			filename := path;
			linenumber := rowNum;
			linenumberstring := rowNumber;
		end
		else
		if Length(l[i]) >= 14 then
		begin
//			if l[i]  = 'Program exited normally.' then // 'Program exited normally.'
			if Copy(l[i], 1, 14)  = 'Program exited' then // 'Program exited normally.'
			begin
				canNotContinue := true; // Indicated "run" instead of "continue"
//haltedButCanBeRestared := true; // WHat should I call this?
//				BugsDisable;
//				ProcessTerminate(gCurrentProcess);
			end;
			if l[i][1] = '[' then // Bracket indicates possible "exited normally" in official GDB
			begin
				if Copy(l[i], Length(l[i]) - 15, 15) = 'exited normally' then
				begin
					canNotContinue := true; // Indicated "run" instead of "continue"
//					haltedButCanBeRestared := true; // WHat should I call this?
				end;
			end;
			
			if Copy(l[i], 1, 23) = 'Program received signal' then // TEST FOR CRASH HERE
			begin
				canNotContinue := true; // Indicated "run" instead of "continue"
//				programCrashed := true;
			end;

		end
		else
		begin
//			WriteLn('Not interesting');
// Borde funktion och argument fångas upp?
		end;
	end;
end;

// Parses data coming from LLDB at any time. Especially stop at breakpoints.
// Shall return file name, and NOT read the file.
procedure ParseReplyLLDB(s: AnsiString; var filename: AnsiString; var linenumber: Longint; var linenumberstring: AnsiString);
var
	l: StringArr;
	path, rownumber, charnumber: AnsiString;
	i, p, oldp, rowNum, err: Longint;
	//fileSpec: FSSpecString;
	pos, tokenStart, tokenEnd, tokenType, editIndex: Longint;
	tokenValue: AnsiString;
begin
	filename := '';
	linenumber := 0;
	linenumberstring := '';

(*
START:
Process 45344 launched: '/Users/ingemar/test' (i386)

STOPP VID BRYTPUNKT:
Process 45344 stopped
* thread #1: tid = 0x5c288c, 0x00010fa8 test`PASCALMAIN + 24 at test.pas:32, queue = 'com.apple.main-thread', stop reason = breakpoint 1.1
    frame #0: 0x00010fa8 test`PASCALMAIN + 24 at test.pas:32

STOPP VID TEMPORÄR BRYTPUNKT (tbreak):
Process 45381 stopped
* thread #1: tid = 0x5c5fce, 0x00010f98 test`PASCALMAIN + 24 at test.pas:32, queue = 'com.apple.main-thread', stop reason = one-shot breakpoint 1
    frame #0: 0x00010f98 test`PASCALMAIN + 24 at test.pas:32

STOPP VID CTRL-C
Process 45352 stopped
* thread #1: tid = 0x5c2be8, 0x9040cf7a libsystem_kernel.dylib`mach_msg_trap + 10, queue = 'com.apple.main-thread', stop reason = signal SIGSTOP
    frame #0: 0x9040cf7a libsystem_kernel.dylib`mach_msg_trap + 10

STOPP VID BAD ACCESS
Process 45371 stopped
* thread #1: tid = 0x5c41a4, 0x0001110b test`PASCALMAIN + 395 at test.pas:44, queue = 'com.apple.main-thread', stop reason = EXC_BAD_ACCESS (code=2, address=0x0)
    frame #0: 0x0001110b test`PASCALMAIN + 395 at test.pas:44

STOPP FÖR ATT "RUN" KRÄVER BEKRÄFTELSE
There is a running process, kill it and restart?: [Y/n]
*)

// LLDB: * thread #1: tid = 0x5c41a, 0x00010f67 SimpleParserTest`PASCALMAIN + 23 at SimpleParserTest.pas:10, queue = 'com.apple.main-thread', stop reason = one-shot breakpoint 1
// Typical lines:
// At breakpoint:
//Process 3571 stopped
//* thread #1: tid = 0x42e42, 0x00011031 SimpleParserTest`PASCALMAIN + 225 at SimpleParserTest.pas:15, queue = 'com.apple.main-thread', stop reason = step over
//    frame #0: 0x00011031 SimpleParserTest`PASCALMAIN + 225 at SimpleParserTest.pas:15
//   12  		repeat
//   13  			SimpleParserGetToken(data, pos, tokenStart, tokenEnd, tokenType, tokenValue);
//   14  			WriteLn('"', tokenValue, '", ', tokenType);
//-> 15  		until pos >= Length(data);
//   16  	
//   17  		WriteLn('GDB style');
//   18  		data := 'Breakpoint 3 at 0x1164d: file test.pas, line 11.';
// 1) Starts with *: Search for "at", then get file name and line number!
// What more?
// -> shows line number only... Not so interesting.

// At step: Same! :)

// Lacks detection of end of program!
// Program end in LLDB:
// Process 7858 exited with status = 0 (0x00000000)
// Should also detect crashes!
// Don't quit the debugger until something else happens, like a compilation. "run" or "continue" as far as possible.

	l := SplitStringToList(s);
	for i := 0 to High(l) do
	if Length(l[i]) > 0 then
	begin
		HelpAppendLn('ParseReplyLLDB parses "'+l[i]+'"');
		pos := 1;
		SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue);
		
		if (tokenValue = '*') then // * for LLDB, "Breakpoint" for GDB. Then this works for both?
		begin
			// Search for "at"
			repeat
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue);
			until (pos >= Length(l[i])) or (tokenValue = 'at');
			if pos < Length(s) then
			begin
				// Now get:
				// Filename (may include "-" etc!)
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, filename);
				// Set gPascalMode depending on current file type!
				gPascalMode := GetExtensionType(filename) = kExtTypePascal;
				// Colon
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue);
				// Row number
				SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, linenumberstring);
				Val(linenumberstring, linenumber);
				HelpAppendLn('Found '+filename+' at '+linenumberstring);
				WriteLn('Found '+filename+' at '+linenumberstring);
				if Length(filename) > 0 then
				if linenumber > 0 then
				begin
					editIndex := FindOrOpen(fileName, debugPath);
					if editIndex > 0 then
					begin
						fileName := GetPathFromIndex(editIndex);
//						UpdateFinger(fileName, linenumber);
//						gParseScopeNeeded := true;
					end;
				end;
				
				// Sök till "stop", för att söka efter "stop reason".
				repeat
					SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue);
				until (pos >= Length(l[i])) or (tokenValue = 'stop');
				if pos < Length(l[i]) then // was "s"
				begin
					SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue); // reason
					SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue); // =
					// Resten är "stop reason"
					tokenValue := Copy(l[i], pos, Length(l[i]));
					WriteLn('FOUND stop reason: ', tokenValue);
					HelpAppendLn('FOUND stop reason: ' + tokenValue);
					WriteLn('FOUND stop reason: ', l[i]);
					HelpAppendLn('FOUND stop reason: ' + l[i]);
					// At this point we can report the stop reason as needed!
					// Mark the program as stopped? Mark as continuable or re-runnable?
//FOUND stop reason:  one-shot breakpoint 1
//FOUND stop reason:  EXC_BAD_ACCESS (code=2, address=0x0)
					
					// First word of stop reason
					SimpleParserGetToken(l[i], pos, tokenStart, tokenEnd, tokenType, tokenValue); // =
					if tokenValue = 'EXC_BAD_ACCESS' then
					begin
						canNotContinue := true;
						MessageAlert('Bad access exception', 'You can run to start over or cmd-. to halt.');
					end;
					// Detect SIGSTOP!
					// Where do I catch "one-shot breakpoint 1"?
				end;				
			end;
		end
		else
// Some messages to detect:
// "run" med körande program:
// There is a running process, kill it and restart?: [Y/n]
// "quit" med körande program:
// Quitting LLDB will kill one or more processes. Do you really want to proceed: [Y/n]
// "continue" när programmet inte körs -> run
// error: Process must be launched.
// Process exited:
// Process 45548 exited with status = 0 (0x00000000) 
		if tokenValue = 'Process' then
		begin
//			WriteLn('PROCESS: ', l[i]);
//			HelpAppendLn('PROCESS: ' + l[i]);

			SimpleParserGetToken(l[i], pos, tokenType, tokenValue); // process number
			SimpleParserGetToken(l[i], pos, tokenType, tokenValue); // exited
			if tokenValue = 'exited' then
			begin
				// Save status!
				canNotContinue := true;
			end;

			// Process number
			// exited
			// with
			// status
			// =
			// error number
			// (
			// error number in hex
			// )

			// If "exited", save status
			// Others?
		end
		else
		if tokenValue = 'There' then
		begin
//			WriteLn('THERE: ', l[i]);
//			HelpAppendLn('THERE: ' + l[i]);
			if Copy(l[i], 1, 55) = 'There is a running process, kill it and restart?: [Y/n]' then
			begin
				ProcessWrite(gCurrentProcess, 'Y' + Char(10));
				// Send "Y"!
			end;
		end
		else
		if tokenValue = 'error' then
		begin
//			WriteLn('ERROR: ', l[i]);
//			HelpAppendLn('ERROR: ' + l[i]);
			
			if l[i] = 'error: Process must be launched.' then
			begin
				ProcessWrite(gCurrentProcess, 'run' + Char(10));
				// Send "run"!
			end;
		end;
	end;
end;


procedure UpdateVariablesWindow;
var
	control: ControlRef;
begin
// I really just need to inval the variables view
//	GetWindowPortBounds(bugsWind, bounds);
//	InvalWindowRect(bugsWind, bounds);
	
	// Update data for variables field 2 (zoom in) and 3 (memory)
	ResizeVariables; // Calls UpdateVariables2Data, UpdateVariables3Data

	VMGetControl( bugs1Wind, 'Vari', 0, control);
	HIViewSetNeedsDisplay(control, true);
	VMGetControl( bugs1Wind, 'Vari', 1, control);
	HIViewSetNeedsDisplay(control, true);
	VMGetControl( bugs1Wind, 'Vari', 2, control);
	HIViewSetNeedsDisplay(control, true);
	VMGetControl( bugs1Wind, 'Vari', 3, control);
	HIViewSetNeedsDisplay(control, true);
end;

var
	backTraceVars: OutArrType;
	backTraceStack: StringArr;

(*
// Anropas av BugsData, typiskt när en ny plats i filen uppnås.
// Något av en central punkt för datasamling, men skumt namn för detta.
procedure ParseReplyScopeStatus(msg: AnsiString);
var
	s, rownumberstring, filename: AnsiString;
	rownumber: Longint;
begin
	ParseReply(msg, filename, rownumber, rownumberstring);
//	WriteLn('Parsed reply and got "', filename, '", (', rownumberstring, ')', rownumber);
	if filename = '' then Exit(ParseReplyScopeStatus);
	if rownumber <= 0 then Exit(ParseReplyScopeStatus);
//	WriteLn('Parsed reply OK, get backtrace and analyze');

// Set a flag that we want "info scope" and "backtrace full" within a certain time!
// And do it ONLY if nothing else happened!

// info scope - kan bli inaktuell av backtrace all
	s := ReadToPrompt(gCurrentProcess, 'info scope ' + rownumberstring, gDebuggerPrompt);
	ParseScope(s);

// NYTT: backtrace all
	s := ReadToPrompt(gCurrentProcess, 'backtrace full', gDebuggerPrompt);
//	WriteLn(s);
	ParseBacktrace(s, backTraceVars, backTraceStack);
//	WriteLn('Backtrace returned ', Length(backTraceVars), ' variables and ', Length(backTraceStack), ' stack levels.');

	GetStatus; // For additions above
	UpdateExpressions;
	UpdateGlobalsValues;
	UpdateVariablesWindow;
	
	// Finally, tell the editor. filename + rownumberstring
//	WriteLn('Please update finger to ', fileName);
	UpdateFinger(fileName, rownumber);
end;
*)


// Sreehari's LLDB functions
// rewritten by Ingemar to use the SimpleParser, then unified to a single function since the format is the same for both cases.

//Example:
//Global variables for /Users/ingemar/test.pas in /Users/ingemar/test:
//(ANSISTRING) S = 0x00000000
//(SMALLINT) X = 0
//(SMALLINT) Y = 0
//(SMALLINT) Z = 0

// (<anonymous struct>) R = (A = 0, B = 0, C = 0)
// (RECTYPE) RR = (D = 0, E = 0, F = 0)
// (SMALLINT [3]) ARR = ([0] = 0, [1] = 0, [2] = 0)

	// Make the value look nicer
	procedure BeautifyValue(var value: AnsiString);
	begin
		if gPascalMode then
		begin
			if value = '0x00000000' then
				value := 'nil';
			if Copy(value, 1, 2) = '0x' then
				value := '$' + Copy(value, 3, Length(value));
		end;
	end;

procedure ParseVariablesLLDB(s:AnsiString; var variableList: OutArrType);//; fileName: AnsiString);
var
	i:LongInt;
	position, tokenStart, tokenEnd, tokenType : LongInt;
	tokenValue, typeName, variableName, variableValue, stringValue: AnsiString;
	l: StringArr;
	
	procedure StoreVariable(typeId: Longint; value, name: AnsiString);
	var
		typeIds: AnsiString;
	begin
		SetLength(variableList, Length(variableList)+1);
		BeautifyValue(value);
		variableList[High(variableList)].name := name; // Probably upper case for FPC code
		if gPascalMode then MatchCase(name); // Will this know which file to search?
		variableList[High(variableList)].displayname := name; // Corrected to match the code
		variableList[High(variableList)].typeId := typeId;
		variableList[High(variableList)].value := value;
//		variableList[High(variableList)].fileName := fileName;
		Str(typeId, typeIds);
//		HelpAppendLn('ParseVariablesLLDB found: ' + name + ' with value ' + value + ' and type ' + typeIds);
	end;
begin
HelpAppendLn('ParseVariablesLLDB parses: '#13'-----------------'#13 + s + #13'-----------------');
	SetLength(variableList, 0);
	l := SplitStringToList(s);
	for i := 0 to High(l) do
	begin
		position := 1;
		SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, tokenValue);
		if tokenValue = '(' then
		begin
			typeName := '';
			// This assumed (SMALLINT) etc - but it might be more than one identifier! (E.g. *)
			while (position < Length(l[i])) and (tokenValue <> ')') and (tokenValue <> '[') do
			begin
				SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, tokenValue);
				typeName := typeName + tokenValue;
			end;
//			SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, typeName);
//			SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, tokenValue);
			if tokenValue = ')' then
			begin
				SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, variableName);
				SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, tokenValue); // =
				if tokenValue = '=' then
				begin
					SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, variableValue);
					// If the value is hex, then a string might follow!
					if tokenType = kNumericToken then // Scalar number!
					begin
						// Store!
						StoreVariable(kScalarOrUnknown, variableValue, variableName);
					end;
					if tokenType = kHexToken then 
					begin
						SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, stringValue);
						// Store!
						if tokenType = kStringToken then
							StoreVariable(kStringPointer, stringValue, variableName)
						else
							StoreVariable(kScalarOrUnknown, variableValue, variableName);
					end;
					// But how about records?! (RECTYPE) RR = (D = 0, E = 0, F = 0)
					// Can this be an object?
					if variableValue = '(' then
					begin
						// Store!
						StoreVariable(kRecord, '(record)', variableName);
					end;
					// { may be a complex struct.
					if variableValue = '{' then
					begin
						// Store!
						StoreVariable(kRecord, '(record)', variableName);
					end;
				end;
			end
			else
			if tokenValue = '[' then // array!
			begin
				// And arrays! (SMALLINT [3]) ARR = ([0] = 0, [1] = 0, [2] = 0)
				// Parse until we find the name!
				repeat
					SimpleParserGetToken(l[i], position, tokenStart, tokenEnd, tokenType, variableName);
				until (position >= Length(l[i])) or (tokenType = kAlphaNumericToken);
				// Store!
				StoreVariable(kArray, '(array)', variableName);
			end;
		end;
	end;
end;

// End of Sreehari's LLDB functions


// Replaces ParseReplyScopeStatus
// To be called some time after BugsData!
procedure UpdateState(theTimer: EventLoopTimerRef; userData: Pointer); MWPascal;
var
	s{, rownumberstring, filename}: AnsiString;
//	rownumber: Longint;
begin
	RemoveEventLoopTimer(gParseScopeTimer);
	gParseScopeTimer := nil;
	if not gParseScopeNeeded then Exit(UpdateState);

	if not ProcessRunning(gCurrentProcess) then Exit(UpdateState);
	
// info scope - replaced by backtrace full
//	s := ReadToPrompt(gCurrentProcess, 'info scope ' + rownumberstring, gDebuggerPrompt);
//	ParseScope(s);

//	if debugName = 'gdb' then
	if gDebuggerIsGDB then
	begin
// backtrace full for locals
		s := ReadToPrompt(gCurrentProcess, 'backtrace full', gDebuggerPrompt);
		ParseBacktrace(s, backTraceVars, backTraceStack);
	end
	else
	begin
		// ta v for globals
		// fr v for locals
		s := ReadToPrompt(gCurrentProcess, 'ta v', gDebuggerPrompt);
		ParseVariablesLLDB(s, globalVariables);

//(lldb) ta v
//Global variables for /Users/ingemar/SimpleParserTest.pas in /Users/ingemar/SimpleParserTest:
//(ANSISTRING) DATA = 0x000303fc "Hej hopp 123 -456 3.4 fil.namn"
//(LONGINT) POS = 0
//(LONGINT) TOKENSTART = 0
//(LONGINT) TOKENEND = 0
//(LONGINT) TOKENTYPE = 0
//(ANSISTRING) TOKENVALUE = 0x00000000

		s := ReadToPrompt(gCurrentProcess, 'fr v', gDebuggerPrompt);
		ParseVariablesLLDB(s, backTraceVars); // backTraceVars, backTraceStack);

// Varför inte globalVariables/localVariables VarInfoRec?
// Varför OutArrType + StringArr?
// Jo, OutArrType = array av VarInfoRec!

//(lldb) fr v
//(ANSISTRING) DATA = 0x000303fc "Hej hopp 123 -456 3.4 fil.namn"
//(LONGINT) POS = 1
//(LONGINT) TOKENSTART = 0
//(LONGINT) TOKENEND = 0
//(LONGINT) TOKENTYPE = 0
//(ANSISTRING) TOKENVALUE = 0x00000000
//(ANSISTRING) S = 0x00000000
//(LONGINT) BUFFERLENGTH = 2
//(BOOLEAN) HEXFLAG = false
//(CHAR) CR = <empty constant data>
//(CHAR) LF = <empty constant data>
//(CHAR) TAB = <empty constant data>

	// Can we also get the stack for LLDB? To put in the backTraceStack?
	end;
	
	DirectGDBGetStatus; // For DirectGDB
	UpdateExpressions;
	if gDebuggerIsGDB then
//	if debugName = 'gdb' then
		UpdateGlobalsValues; // For LLDB, globals are updated above by "ta v"
	UpdateVariablesWindow;
	
	gParseScopeNeeded := false;
end;

procedure QueueUpdateState;
begin
		if gParseScopeTimer <> nil then
			RemoveEventLoopTimer(gParseScopeTimer);
		InstallEventLoopTimer (GetMainEventLoop(), kEventDurationSecond, 0,
							UpdateState, nil, gParseScopeTimer);
end;


procedure BugsStop;
var
	pid: Longint;
begin
// Sending a plain ctrl-C does not help.
//	if gBugsRunning then
//		ProcessWrite(gCurrentProcess, #3);

// This is supposed to be a ctrl-C but doesn't work either.
// (but it works in 10.7!)
	if gBugsRunning then
		if gCurrentProcess <> nil then
		begin
			WriteLn('*** BugsStop! BugsStop! Waiting at the BugsStop!');
			pid := kill(gCurrentProcess^.pid, SIGINT);
//			pid := kill(gCurrentProcess^.pid, SIGTRAP); // Terminates
//			pid := kill(gCurrentProcess^.pid, SIGABRT); // Terminates
		end;
end;

procedure BugsDoStep(cmd: Longint);
var
	//s: AnsiString;
	cmdStr: AnsiString;
begin
	case cmd of
		kBugsStepOver: cmdStr := 'next';
		kBugsStepIn: cmdStr := 'step';
		kBugsStepOut: cmdStr := 'finish';
	end;
	
//	WriteLn(cmdStr);
	if gBugsRunning then
	if cmdStr <> '' then
	if gCurrentProcess <> nil then
		ProcessWrite(gCurrentProcess, cmdStr + Char(10));
// DUGER INTE! Ta svaret med vanlig körning?
//	s := ReadToPrompt(gCurrentProcess, cmdStr, gDebuggerPrompt);		
//	SetDView(bugsWind, 'Stat', 0);
//	DisplayString(s);
	
//	ParseReplyScopeStatus(s);
end;



	// Run data callback
	// This procedure collects data when running modelessly
	// Echoes to the message window.
	// Every line is fed to ParseReplyScopeStatus for analysis,
	// and then ParseReplyScopeStatus may issue more commands for
	// inspecting variables etc.
	// NO - we call ParseReply for imminent data, then queue a call to PasceScope!
	procedure BugsData(oneLineOfData: AnsiString; privateData: Pointer);
	var
//		{i,} limit: Longint;
		{s,} rownumberstring, filename: AnsiString;
		rownumber: Longint;
	const
		FF = Char(12); // Form feed = ASCII(12).
		BEL = Char(7); // Bell
	begin
WriteLn('BugsData "', oneLineOfData, '"');
//HelpAppendLn('BugsData "' + oneLineOfData + '"'); // Not line by line!

// Search for FF character!
// But why here, why not in Console? Moved there 160402.
(*		limit := 1;
		for i := 1 to Length(oneLineOfData) do
		begin
			if oneLineOfData[i] = FF then
			begin
				HelpClear;
				limit := i+1; // Send from here
			end;
			if oneLineOfData[i] = BEL then
				SysBeep(1);
		end;*)

//		if limit = 1 then
			HelpAppend(oneLineOfData); // if no FF
//		else
//			HelpAppend(Copy(oneLineOfData, limit, Length(oneLineOfData)));
		HelpForceUpdate;
		
//		ParseReplyScopeStatus(oneLineOfData);

// NEW: ParseReply, then queue a request to "info scope" and "backtrace full"

	if gDebuggerIsGDB then
		ParseReply(oneLineOfData, filename, rownumber, rownumberstring)
	else
		ParseReplyLLDB(oneLineOfData, filename, rownumber, rownumberstring);
	if filename = '' then Exit(BugsData);
	if rownumber <= 0 then Exit(BugsData);
	// Finally, tell the editor. filename + rownumberstring
	UpdateFinger(fileName, rownumber);
	gParseScopeNeeded := true;

// Set a flag that we want "info scope" and "backtrace full" within a certain time!
// And do it ONLY if nothing else happened!
	QueueUpdateState;

(*
// info scope - kan bli inaktuell av backtrace full
	s := ReadToPrompt(gCurrentProcess, 'info scope ' + rownumberstring, gDebuggerPrompt);
	ParseScope(s);
	s := ReadToPrompt(gCurrentProcess, 'backtrace full', gDebuggerPrompt);
	ParseBacktrace(s, backTraceVars, backTraceStack);
	GetStatus; // For additions above
	UpdateExpressions;
	UpdateGlobalsValues;
	UpdateVariablesWindow;
*)

	// Finally, tell the editor. filename + rownumberstring
//	UpdateFinger(fileName, rownumber);

	end;
	
	// Run finished callback
	procedure BugsDone(aborted: Boolean; result: Integer; privateData: Pointer);
	var
		cleanNamePtr: StringPtr;
	begin
	writeln('**bugs done **');
		cleanNamePtr := StringPtr(privateData);
		if aborted then
			HelpAppendLn('--- ' + debugName + '  ' + cleanNamePtr^ + ' stopped ---')
		else
			if result = 0 then
				HelpAppendLn('--- ' + debugName + '  ' + cleanNamePtr^ + ' finished ---')
			else
				HelpAppendLn('--- ' + debugName + '  ' + cleanNamePtr^ + ' finished with error ---');
		HelpMainMessage(''); // Reset
		
		// STÄNG AV DEBUGGER HÄR!
		BugsDisable;
		UpdateFinger(' ', -1); // No file should have the finger
	end;

procedure BugsShowWindow;
begin
	ShowWindow(bugs1Wind);
	SelectWindow(bugs1Wind);
end;

procedure BugsShowObserveWindow;
begin
	ShowWindow(observeWind);
	SelectWindow(observeWind);
end;

procedure BugsShowGDBWindow;
begin
	ShowWindow(directgdbWind);
	SelectWindow(directgdbWind);
end;

// Is this used at all?
procedure DoDebugMenu (item: integer); {Menu handler}
begin
	case item of
		1:
		begin
			ShowWindow(bugs1Wind);
			SelectWindow(bugs1Wind);
		end;
		3: // Step over
		begin
			BugsDoStep(kBugsStepOver);
			OKtoSetBreaks := false;
	UpdateFinger(' ', -1); // No file should have the finger while running
		end;
		4: // Step into
		begin
			BugsDoStep(kBugsStepIn);
			OKtoSetBreaks := false;
	UpdateFinger(' ', -1); // No file should have the finger while running
		end;
		5: // Step out
		begin
			BugsDoStep(kBugsStepOut);
			OKtoSetBreaks := false;
	UpdateFinger(' ', -1); // No file should have the finger while running
		end;
	end;
end;

// "run" command
procedure BugsRun;
//var
//	s: AnsiString;
begin
	if canNotContinue then
		ProcessWrite(gCurrentProcess, 'run' + Char(10))
	else
		ProcessWrite(gCurrentProcess, 'continue' + Char(10));

	canNotContinue := false;
	
//	s := ReadToPrompt(gCurrentProcess, 'run', gDebuggerPrompt);
	// Gör något för att veta att det rullar? Så man måste fånga upp prompt på annat ställe?

	UpdateFinger(' ', -1); // No file should have the finger while running

	OKtoSetBreaks := false;
end;

function BugsRunning: Boolean;
begin
	// True om debuggern är igång!
	return gBugsRunning;
end;

(*
// User set or reset breakpoint
procedure BugsChangedBreakpoints(theWind: WindowPtr; line: Longint; add: Boolean);
var
	theSpec: FSSpecString;
	str: Str255;
	err: OSErr;
	s: AnsiString;
	actualLine: Longint;
	brNumber: Longint;
begin
	if not BugsRunning then Exit(BugsChangedBreakpoints);
	
	// Filename or full path?
	err := GetEditFSSpec(theWind, theSpec);
	if err <> noErr then Exit(BugsChangedBreakpoints);
	
	NumToString(line, str);
	
//	WriteLn('BugsChangedBreakpoints ', line, GetLastToken(theSpec));
	
	// PROBLEM: Hangs if breakpoint set when running
	// Check if running?
	// (Fixed with timeout?)
	if gCurrentProcess <> nil then
		if add then
		begin
			if gDebuggerIsGDB then
//			if debugName = 'gdb' then
				s := ReadToPrompt(gCurrentProcess, 'break ' + GetLastToken(theSpec) + ':' + str, gDebuggerPrompt)
			else // lldb
				s := ReadToPrompt(gCurrentProcess, 'b ' + GetLastToken(theSpec) + ':' + str, gDebuggerPrompt);
			WriteLn('Breakpoint reply: '+ s);
			HelpAppendLn('Breakpoint reply: '+ s);
			// LLDB reply: 
			// Breakpoint 2: where = test`PASCALMAIN + 119 at test.pas:38, address = 0x00011007

			
			// Parse s to check where it REALLY ended up!
			// Two rows in reply, number is last in first and first in last (!)

			actualLine := GetBreakpointLineFromReply(s, brNumber);
//			if line <> actualLine then - anropa ALLTID för att spara brNumber!
			MoveBreakpoint(line, actualLine, theWind, brNumber);
			br^.brNumber := brNumber;
		end
		else
		if gDebuggerIsGDB then
		begin
			s := ReadToPrompt(gCurrentProcess, 'clear ' + GetLastToken(theSpec) + ':' + str, gDebuggerPrompt);
		end
		else
		begin
			// LLDB delete breakpoint!
			// br del NR
			// So what is the number?
			// Either save at other time, or br l and parse!
		end;
end;
*)

procedure BugsRunTo(theWind: WindowPtr; line: Longint); // Set temporary breakpoint and run
var
	theSpec: FSSpecString;
	str: Str255;
	err: OSErr;
	s: AnsiString;
begin
	if not BugsRunning then Exit(BugsRunTo);
	err := GetEditFSSpec(theWind, theSpec);
	if err <> noErr then Exit(BugsRunTo);
	
	NumToString(line, str);
	
	if gCurrentProcess <> nil then
	begin
		s := ReadToPrompt(gCurrentProcess, 'tbreak ' + GetLastToken(theSpec) + ':' + str, gDebuggerPrompt);
		BugsRun;
	end;
end;

// Check if a breakpoint was successfully set
function CheckTBreak(s: AnsiString): Boolean;
var
	strArr: StringArr;
	i: Longint;
begin
(*
Example run debugging GPC program:

(gdb) tbreak main
Breakpoint 1 at 0x2bf4: file <implicit code>, line 308.

(gdb) tbreak PASCALMAIN
Function "PASCALMAIN" not defined.
Make breakpoint pending on future shared library load? (y or [n]) 
*)
	strArr := ParseToWords(s);
	
	for i := Low(strArr) to High(strArr) do
		WriteLn('TBREAK LIST ', i:1, ':', strArr[i]);
	
	if Length(strArr) > 0 then
	begin
		if strArr[0] = 'Breakpoint' then return true;
		if strArr[0] = 'Function' then
		begin
	//		ProcessWrite(gCurrentProcess, Char(10)); // Skip question (see above)
			return false;
		end;
	end;
	
	return true;
end;

procedure BugsRunWithDebug(theSpec: FSSpecString);
	var
		cleanName: AnsiString;
		path, commandLine, pathOnly: AnsiString;
		cleanNamePtr: StringPtr;
		extType: Integer;
		i: Longint;
		bp: BreakpointPtr;
		lineStr, s: AnsiString;
		actualLine: Longint; // Used when correcting breakpoint positions
		brNumber: Longint;
begin
// Clear some old debugging data
	SetLength(globalVariables, 0);
	SetLength(localVariables, 0);
//	SetLength(variablePosToIndex, 0);

// Ungefär som Run, men genom gdb, och kopplas till Lightbugs
// Visa debuggerfönster, antar jag.
// Sätt brytpunkter!

// Sätt variabler som används för att växla mellan GDB och LLDB
	debugName := GetLastToken(gSettings.gdb);
	gDebuggerIsGDB := debugName = 'gdb';
	gDebuggerPrompt := '(' + debugName + ') '; // Works for both GDB and LLDB
	lengthDBName := Length(debugName);
	debugPath := theSpec;

	// Check if "which" can find the compiler if it is missing!
	// Only checks FPS PPC and 386, not iOS
	if gVersionGDB = kProcessUtilsFailedString then
	begin
		CheckCompilersExists; // Get fresh data.
		if gVersionGDB = kProcessUtilsFailedString then
		begin
			s := ProcessToString('which ' + debugName);
			if Length(s) > 1 then
			begin
				while s[Length(s)] in [#10, #13] do s := Copy(s, 1, Length(s)-1);
				if QuestionAlert(debugName + ' seems to be located at '+s, 'Correct settings?') then
				begin
				// Skall in i dialogen OCH variabeln ändras!
					gSettings.gdb := s; // Should trigger save settings!
					SetSettingsDialogItemString('GDBp', 0, s);
					CheckFPCVersion;
	//				MessageAlert('GDB now at "'+gSettings.gdb+'"', gVersionGDB);
				end;
			end;
		end;
	end;

	if gVersionGDB = kProcessUtilsFailedString then
	begin
		if not QuestionAlert(debugName + ' does not seem to be installed', 'Try anyway?') then
		begin
			Exit(BugsRunWithDebug);
		end
		else
		begin
		end;
	end;



// Make sure that no other process is already running
		if gCurrentProcess <> nil then
		begin
			if ProcessRunning(gCurrentProcess) then
			begin
				DebugStr('Another process is already running');
				Exit(BugsRunWithDebug);
			end
			else
			begin
				// Old process terminated but not disposed. This is normal!
				ProcessDispose(gCurrentProcess);
			end;
		end;
		
		path := theSpec;
		// We need the path without filename.
		pathOnly := TrimLastToken(path);
WriteLn('Try chdir to: "', pathOnly, '"');
//HelpAppendLn('Try chdir to: "'+ pathOnly+ '"');
		chdir({MacRomanToUTF8}(pathOnly)); // Set that as current dir
// Bugg? Är inte sökvägen redan UTF8? Ändrat 130729
WriteLn('chdir OK');
		
		cleanName := TrimExtension(GetLastToken(theSpec));
//		ConvertPathToCommandLineFormat(cleanName); // Allow spaces in name
		
		path := TrimExtension(path);
		
		// CHECK IF BINARY EXISTS! What is the easiest and safest way?
		
//		// Command-line arguments (0.8.2)
//		if gSettingsTargetArgs <> '' then
//			tcmd := ' ' + gSettingsTargetArgs
//		else
//			tcmd := '';
// DOES NOT WORK. How do you end command-line args to debugging? (Answer: set args)

WriteLn('TRIVIAL CHECKING');		
		// Bad feedback of stdio. Should work line by line - and be threaded properly.
		if IsCommandLineToolMode(theSpec) then
//		if gFlags^^.commandLineToolMode then
		begin
//			commandLine := '/usr/bin/gdb "' + path + {tcmd +} '"'; // INTE cleanName
			commandLine := gSettings.gdb + ' "' + cleanName + {tcmd +} '"'; // INTE cleanName -- why not?
//			commandLine := gSettings.gdb + ' "' + path + {tcmd +} '"'; // INTE cleanName -- why not?
			HelpAppendLn('COMMAND LINE TOOL MODE ACTIVE');
		end
		else
//			commandLine := gSettings.gdb + ' "' + path+'.app/Contents/MacOS/'+cleanName + {tcmd +} '"'; // Use " instead of shell-style \ - WORKS!
			commandLine := gSettings.gdb + ' "' + cleanName+'.app/Contents/MacOS/'+cleanName + {tcmd +} '"'; // Use " instead of shell-style \ - WORKS!
//			commandLine := '/usr/bin/gdb "' + path+'.app/Contents/MacOS/'+cleanName + {tcmd +} '"'; // Use " instead of shell-style \ - WORKS!

		if gDebuggerIsGDB then
//		if debugName = 'gdb' then
			commandLine := commandLine + ' -f -q -d "' + pathOnly + '"'
		else
			commandLine := commandLine + {' -f -q -d "' +} pathOnly + '"';
		// LÄGG TILL ALLA PATHS I SETTINGS!

// Java and makefile are not supported
		extType := GetExtensionType(theSpec);
		if extType in [kExtTypeJava, kExtTypeMakefile] then
		begin
			HelpAppendLn('Debugging not supported for target');
			Exit(BugsRunWithDebug);
		end;
		
		HelpMainMessage('Running/debugging ' + cleanName); // Show status
		
		HelpAppendLn('Command: '+commandLine);
		HelpAppendLn('--- ' + debugName + ' ' + cleanName + ' starts ---');
		
// now Run!
		cleanNamePtr := StringPtr(NewPtr(Length(cleanName) + 1));
		cleanNamePtr^:= cleanName;
		
		if gCurrentProcess <> nil then
			HelpAppendLn('SERIOUS PROCESS MANAGEMENT ERROR');

WriteLn('LAUNCHING');
		gCurrentProcess := ProcessLaunch({MacRomanToUTF8}(commandLine), true); // Bugfix 130729
		if gCurrentProcess <> nil then
		begin
			ProcessSetCallbacks(gCurrentProcess, @BugsData, @BugsDone, Pointer(cleanNamePtr));
			if gCurrentProcess^.doneCallback = nil then
				HelpAppendLn('NO DONE CALLBACK!');
			ProcessSetLineBuffering(gCurrentProcess, false);
//			SelectWindow(theErrorMsgWind);
			SelectWindow(helpWind);
			
			// New feature 080504: bring launched app to front
			// Not included here - we rather want the source in front. Copy to "run" command in debugger?
		end
		else
		begin
			HelpAppendLn('Launch failed - fork failed');
			HelpMainMessage(''); // Show status
			Exit(BugsRunWithDebug);
		end;
	
	BugsEnable;

	// Read startup messages
	// This should not be needed - but it is!
	// Insufficient readout of old data in ReadToPrompt?
	s := ReadToPrompt(gCurrentProcess, '', gDebuggerPrompt);
	if gDebuggerIsGDB then
//	if debugName = 'gdb' then
	begin
		s := ReadToPrompt(gCurrentProcess, 'set confirm off', gDebuggerPrompt);
		s := ReadToPrompt(gCurrentProcess, 'set pagination off', gDebuggerPrompt);
	end;

// Language is usually automatic, but we must always use Pascal style, since that's what we parse!
// Especially, we issue commands with Pascal syntax!
	if gDebuggerIsGDB then
//	if debugName = 'gdb' then
		s := ReadToPrompt(gCurrentProcess, 'set language pascal', gDebuggerPrompt);

//	s := ReadToPrompt(gCurrentProcess, 'handle SIGINT stop', gDebuggerPrompt); did not help

	if extType = kExtTypePascal then
	begin
// NEW
// Only for Pascal? No, always?
// THIS CRASHES GDB DUE TO A BUG!!! For large programs!
//		s := ReadToPrompt(gCurrentProcess, 'info variables', gDebuggerPrompt);
//		ParseGlobals(s);
	end;
	
// Set breakpoints!
// Go though all open windows and set any breakpoints!
	for i := 1 to kMaxEditWindows do
	begin
		if editWind[i] <> nil then
		begin
			bp := GetWindowBreakpoints(editWind[i]); // Getthe first in the list
			while bp <> nil do
			begin
				Str(bp^.line, lineStr);
				if bp^.enabled then
				begin
					if gDebuggerIsGDB then
						s := ReadToPrompt(gCurrentProcess, 'break "' + {MacRomanToUTF8}(bp^.filename) + '":' + lineStr, gDebuggerPrompt)
					else
						s := ReadToPrompt(gCurrentProcess, 'b"' + {MacRomanToUTF8}(bp^.filename) + '":' + lineStr, gDebuggerPrompt);
					
					// Check if breakpoint is in the right place
					actualLine := GetBreakpointLineFromReply(s, brNumber); // Also returns breakpoint number - do we need that?
					if bp^.line <> actualLine then
						MoveBreakpoint(bp^.line, actualLine, editWind[i], brNumber);

				end;
// UTF here8?! Not tested!!!
//				WriteLn('break ' + bp^.filename + ':' + lineStr);


				bp := bp^.next;
			end;
		end;
	end;

// Pascal and C starts differently!

// start/run to main
WriteLn('start/run to main');
//if not AtLeastOneBreakPoint then
begin
	if extType = kExtTypePascal then
	begin
		s := ReadToPrompt(gCurrentProcess, 'tbreak PASCALMAIN', gDebuggerPrompt); // Temporary breakpoint at Pascal main so we can run past init code.
		if not CheckTBreak(s) then
		begin
		// If it failed, it might be GPC code! Start as C.
			s := ReadToPrompt(gCurrentProcess, 'tbreak main', gDebuggerPrompt); // Temporary breakpoint at Pascal main so we can run past init code.
//			CheckTBreak(s);
		end
	end
	else
	if extType = kExtTypeC then
	begin
		s := ReadToPrompt(gCurrentProcess, 'tbreak main', gDebuggerPrompt); // Temporary breakpoint at Pascal main so we can run past init code.
//		CheckTBreak(s);
	end;
end;

	WriteLn('tbreak: ', s);
//	s := ReadToPrompt(gCurrentProcess, 'run ', gDebuggerPrompt); // Run to start of main
	ProcessWriteLn(gCurrentProcess, 'run');
OKtoSetBreaks := false;

//	if AtLeastOneBreakPoint then		// it's ok and probably desired to go run up to the first breakpoint, otherwise not
//		s := ReadToPrompt(gCurrentProcess, 'continue ', gDebuggerPrompt); // continue to first breakpoint

	WriteLn('run: ', s);

// It works WITHOUT this parsing - why?
//	ParseReplyScopeStatus(s);

	// Borde parsa svaret och visa!
//	s := ReadToPrompt(gCurrentProcess, 'start ', gDebuggerPrompt); // Run to start of main
//	WriteLn(s);
end;



// These should not be here but they should be somewhere. Maybe in View Manager.

	function CGRectToRect(cgr: CGRect): Rect;
	var
		r: Rect;
	begin
		r.left := Trunc(cgr.origin.x);
		r.top := Trunc(cgr.origin.y);
		r.right := Trunc(r.left + cgr.size.width);
		r.bottom := Trunc(r.top + cgr.size.height);
		return r;
	end;
	
	function RectToCGRect(r: Rect): CGRect;
	var
		cgr: CGRect;
	begin
		cgr.origin.x := r.left;
		cgr.origin.y := r.top;
		cgr.size.width := r.right - r.left;
		cgr.size.height := r.bottom - r.top;
		return cgr;
	end;


//type
//	FileNameRec = record
//		name: AnsiString;
//		fileName: AnsiString;
//		value: AnsiString;
//		varType: AnsiString;
//		// type?
//		// last known value?
//	end;
//var
//	globalVariables: array of FileNameRec;
//	localVariables: array of FileNameRec;

// "print" på alla globaler
// Övriga skrivs med värdet de fått tidigare
// Klickbara - skicka klickad till annan vy
// Hur gör man detta i flera nivåer?
// Klick på innehåll, minns vägen dit och gå in?

var
	gSelectedFrame: Longint;

// Stack view
//procedure VariablesDraw(theView: HIViewRef; viewRect: Rect; userData: Pointer);
procedure VariablesDraw(theView: HIViewRef; cgContext: CGContextRef; cgviewRect: CGRect; userData: Pointer);
var
	i: Longint;
//	s: String;
	viewRect, r: Rect;
begin
	viewRect := CGRectToRect(cgviewRect);
//	UseViewContext(cgContext, cgviewRect.size.height);
	CreatePort(cgContext, cgviewRect.size.height);

	BackColor(whiteColor);
	EraseRect(viewRect);
	QDCG.TextSize(12);

	// Boxes for stack frames to click in to select frame.
	for i := 0 to High(backTraceStack)+2 do
	begin
		SetRect(r, viewRect.left, 20*i, viewRect.right, 20*(i+1));
		if i = gSelectedFrame then // Mark selected (faked)
		begin
			ForeColor(lighterBlueColor);
			PaintRect(r);
		end;
		ForeColor(lighterGrayColor);
		FrameRect(r);
		
		ForeColor(blackColor);
		MoveTo(10, (i+1)*20-2);
		if i = 0 then
			DrawString('All')
		else
		if i = 1 then
			DrawString('Globals')
		else
			DrawString(backTraceStack[i-2]);
	end;
	ForeColor(blackColor);
	FinishPort;
end;

procedure VariablesMouse(theView: HIViewRef; cgwhere: CGPoint; mods, button: Longint; userData: Pointer);
var
	control: HIViewRef;
begin
	gSelectedFrame := Trunc(cgwhere.y / 20);
	WriteLn('VariablesMouse');
//	VMGetControl( bugsWind, 'Vari', 0, control);
	HIViewSetNeedsDisplay(theView, true);
	// Uppdatera även Vari 1 = variable view
	ResizeVariables; // Can be optimized to just #1
	VMGetControl( bugs1Wind, 'Vari', 0, control); // Samma som ovan?
	HIViewSetNeedsDisplay(control, true);
	VMGetControl( bugs1Wind, 'Vari', 1, control); // Nonexistent?! Or added in code?
	HIViewSetNeedsDisplay(control, true);
end;
procedure VariablesKey(theView: HIViewRef; key: Char; mods: Longint; userData: Pointer);
begin
	WriteLn('VariablesKey');
end;

var
	variablePosToIndex: array of Integer; // For knowing what we click!

procedure VariablesDraw1(theView: HIViewRef; cgContext: CGContextRef; cgviewRect: CGRect; userData: Pointer);
var
	i, j: Longint;
//	s: String;
	viewRect: Rect;
begin
	viewRect := CGRectToRect(cgviewRect);
//	UseViewContext(cgContext, cgviewRect.size.height);
	CreatePort(cgContext, cgviewRect.size.height);
	QDCG.BackColor(whiteColor);
	QDCG.EraseRect(viewRect);
	QDCG.TextSize(12);
	
	if gSelectedFrame = 0 then // All
	begin
		SetLength(variablePosToIndex, Length(backTraceVars) + Length(globalVariables));
		for i := 0 to High(globalVariables) do
		begin
			MoveTo(10, (i+1)*20);
//			DrawString(globalVariables[i].name + ' = ' + globalVariables[i].value+ ':'+ globalVariables[i].varType);
			DrawString(globalVariables[i].displayname + ' = ' + globalVariables[i].value);
			variablePosToIndex[i] := -i-1; // Negativt index = globals
		end;
// Important: The frameNumber is not yet supported by LLDB code!
		for i := 0 to High(backTraceVars) do
		begin
			MoveTo(10, (i+1 + Length(globalVariables))*20);
			if High(backTraceStack) >= backTraceVars[i].frameNumber then
				DrawString(backTraceVars[i].displayname + ' = ' + backTraceVars[i].value + ' (' + backTraceStack[backTraceVars[i].frameNumber] + ')')
			else
				DrawString(backTraceVars[i].displayname + ' = ' + backTraceVars[i].value);
			variablePosToIndex[i+Length(globalVariables)] := i;
		end;
	end
	else
	if gSelectedFrame = 1 then // Globals
	begin
		SetLength(variablePosToIndex, Length(globalVariables));
		for i := 0 to High(globalVariables) do
		begin
			MoveTo(10, (i+1)*20);
//			DrawString(globalVariables[i].name + ' = ' + globalVariables[i].value+ ':'+ globalVariables[i].varType);
			DrawString(globalVariables[i].displayname + ' = ' + globalVariables[i].value);
			// value sätts inte
			variablePosToIndex[i] := -i-1; // Negativt index = globals
		end;
	end
	else // Frame # 	if gSelectedFrame - 2
	begin
		SetLength(variablePosToIndex, Length(backTraceVars)); // Temporary
		j := 0;
		for i := 0 to High(backTraceVars) do
		if backTraceVars[i].frameNumber = gSelectedFrame - 2 then
		begin
			MoveTo(10, (j+1)*20);
//			DrawString(globalVariables[i].name + ' = ' + globalVariables[i].value+ ':'+ globalVariables[i].varType);
			DrawString(backTraceVars[i].displayname + ' = ' + backTraceVars[i].value);
			variablePosToIndex[j] := i;
			j += 1;
		end;
		SetLength(variablePosToIndex, j);
	end;
	
(*
	MoveTo(10, 3*20);
	LineTo(200, 3*20);
	for i := 0 to High(localVariables) do
	begin
		MoveTo(10, i*20 + 3*20);
		DrawString(localVariables[i].name + ' = ' + localVariables[i].value+ ':'+ localVariables[i].varType);
		// value sätts till name?
	end;
*)
	FinishPort;
end;

var
//	var2DisplayData: AnsiString;
	var2DisplayVariable: VarInfoRec;
	var2History: array of VarInfoRec; // History of previous variables in var2

procedure VariablesMouse1(theView: HIViewRef; where: CGPoint; mods, button: Longint; userData: Pointer);
var
	i: Longint;
//	repl: AnsiString;
	control: ControlRef;
	var2Edit: VarInfoRec;
begin
WriteLn(button, ',', mods);

	if (button <> 1) or (mods <> 0) then // Anything but a straight click
	begin
		i := Trunc(where.y / 20);
		if i <= High(variablePosToIndex) then
			if variablePosToIndex[i] < 0 then
			begin
				var2Edit := globalVariables[-variablePosToIndex[i]-1];
				// Open variable editor!
				ShowSetVariableDialog(var2Edit);
			end
			else
			begin
				var2Edit := backTraceVars[variablePosToIndex[i]];
				// Open variable editor!
				ShowSetVariableDialog(var2Edit);
			end;
	end;

// Open question: Should var2History be emptied when clicking here?

//	WriteLn('VariablesMouse1');
	// Vilken variabel är klickad på?
	i := Trunc(where.y / 20);
	if i <= High(variablePosToIndex) then
		if variablePosToIndex[i] < 0 then
		begin

// ATT FIXA: Peka in i pekare! (Funkar!)

			WriteLn('Global #', -variablePosToIndex[i]-1, '=', globalVariables[-variablePosToIndex[i]-1].name);
			var2DisplayVariable := globalVariables[-variablePosToIndex[i]-1];

//			repl := ReadToPrompt(gCurrentProcess, 'print ' + var2DisplayVariable.name, gDebuggerPrompt);
//			var2DisplayData := ParseExpandVariable(repl);
		end
		else
		begin
			WriteLn('Local #', variablePosToIndex[i], '=', backTraceVars[variablePosToIndex[i]].name);
			var2DisplayVariable := backTraceVars[variablePosToIndex[i]];
			
//			repl := ReadToPrompt(gCurrentProcess, 'print ' + var2DisplayVariable.name, gDebuggerPrompt);
//			var2DisplayData := ParseExpandVariable(repl);
		end
	else
		WriteLn('Hit no variable');
		
// Click means show expanded view in Vari 2
// Show memory in Vari 3?
// print?
// Pekare gör print med pek. (Dock ej sträng.)
	ResizeVariables;
	VMGetControl( bugs1Wind, 'Vari', 2, control);
	HIViewSetNeedsDisplay(control, true);
	VMGetControl( bugs1Wind, 'Vari', 3, control);
	HIViewSetNeedsDisplay(control, true);
end;
procedure VariablesKey1(theView: HIViewRef; key: Char; mods: Longint; userData: Pointer);
begin
	WriteLn('VariablesKey1');
end;

var
	// NEW:
	var2printArr: OutArrType;
	var2printScalar: AnsiString;
	var2EmergencyArr: StringArr; // For debugging of missing ParsePrint data!

procedure UpdateVariables2Data;
var
	repl: AnsiString;
	fr: String;
begin
	if var2DisplayVariable.name <> '' then
	begin
		Str(var2DisplayVariable.frameNumber, fr);
		repl := ReadToPrompt(gCurrentProcess, 'frame ' + fr, gDebuggerPrompt);
		if var2DisplayVariable.typeId = kPointer then
		begin
			// Note!!! ^ is wrong for C code! Must use *!
			if gDebuggerIsGDB and gPascalMode then
				repl := ReadToPrompt(gCurrentProcess, 'print ' + var2DisplayVariable.name + '^', gDebuggerPrompt)
			else
				repl := ReadToPrompt(gCurrentProcess, 'print (*' + var2DisplayVariable.name + ')', gDebuggerPrompt)
		end
		else
			repl := ReadToPrompt(gCurrentProcess, 'print ' + var2DisplayVariable.name, gDebuggerPrompt);

//WriteLn('**************UpdateVariables2Data before cleaning: "', repl, '"');

		if gDebuggerIsGDB then
		begin
			repl := Copy(repl, 1, Length(repl) - 7); // Tag bort (gdb) --- I wonder, is it really a "gdb" we want to remove?!
			ParsePrint(repl, var2printArr, var2printScalar);
		end
		else
			ParsePrintLLDB(repl, var2printArr, var2printScalar);			
//			CleanLLDBPrint(repl); // Remove (TYPE) och $NR (insignificant command number) The type may be saved and corrected.

//WriteLn('**************UpdateVariables2Data: "', repl, '"');
		
//		var2DisplayData := ParseExpandVariable(repl);
//		var2DisplayDataList := SplitStringToList(var2DisplayData); Obsolete?

		
		if Length(var2printArr) = 0 then
		begin
//			SetLength(var2printArr, 1);
			var2EmergencyArr := SplitStringToList(repl);
		end;
	end;
end;

var
	gVariables3Data: StringArr; // AnsiString;

// Make the Variables3 memory dump more readable
procedure CleanMemoryDump(var repl: AnsiString);
var
	i, j: Longint;
	prev: Char;
begin
	j := 1;
	prev := '-';
	for i := 1 to Length(repl) do
	begin
		repl[j] := repl[i];
		if repl[i] = #9 then repl[j] := ' ';
		if repl[i] = 'x' then
				if prev = '0' then
				begin
					repl[j-1] := '$';
					j -= 1;
				end;
		j += 1;
		prev := repl[i];
	end;
	SetLength(repl, j);
end;

procedure UpdateVariables3Data;
var
	repl, addr: AnsiString;
begin
	if BugsRunning then
	if var2DisplayVariable.name <> '' then
	begin
//		WriteLn('Getting address for Vari 3');
		if var2DisplayVariable.typeId = kPointer then
			repl := ReadToPrompt(gCurrentProcess, 'print ' + var2DisplayVariable.name, gDebuggerPrompt)
		else
			repl := ReadToPrompt(gCurrentProcess, 'print ' + '@' + var2DisplayVariable.name, gDebuggerPrompt);
//		WriteLn('reply = ', repl);
		addr := GetAHexValue(repl); // Kan få "address requested for identifier "var" which is in register"
//		WriteLn('addr = ', addr);
		if addr <> '' then
		begin
			repl := ReadToPrompt(gCurrentProcess, 'x/100bx ' + addr, gDebuggerPrompt);
//			DrawString(repl);
			CleanMemoryDump(repl);
			gVariables3Data := SplitStringToList(repl);
		end;
	end;
end;

procedure VariablesDraw2(theView: HIViewRef; cgContext: CGContextRef; cgviewRect: CGRect; userData: Pointer);
var
	i: Longint;
	s: String;
	viewRect: Rect;
//	repl: AnsiString;
//	list: StringArr; // Old raw output
	pol: PolyHandle;
begin
	viewRect := CGRectToRect(cgviewRect);
//	UseViewContext(cgContext, cgviewRect.size.height);
	CreatePort(cgContext, cgviewRect.size.height);
	QDCG.BackColor(whiteColor);
	QDCG.EraseRect(viewRect);
	QDCG.TextSize(12);

	if BugsRunning then
	if var2DisplayVariable.name <> '' then
	begin
		ForeColor(blackColor);
		if Length(var2History) > 0 then
		begin
			pol := OpenPoly;
			MoveTo(5, 13);
			LineTo(15, 6);
			LineTo(15, 20);
			ClosePoly;
			PaintPoly(pol);
			KillPoly(pol);
		end;
		
		MoveTo(20, 20); // Room for button
		DrawString(var2DisplayVariable.name + ':');
		if var2printScalar <> '' then
		begin
			MoveTo(10, 0*20 + 2*20);
//			DrawString('(scalar) "' + var2printScalar + '"');
			DrawString(var2printScalar);
		end
		else
		if Length(var2printArr) = 0 then
		begin
			MoveTo(10, 2*20); // Rad 2
//			DrawString('Got NOTHING from ParsePrint of "' + var2DisplayVariable.name + '"');
// Emergency output (e.g. Cannot access memory at address...):
			for i := 0 to High(var2EmergencyArr) do
			begin
				MoveTo(10, i*20 + 2*20);
				DrawString(var2EmergencyArr[i]);
			end;

//			WriteLn('PROBLEM:');
//			WriteLn(repl);
		end
		else
		for i := 0 to High(var2printArr) do
		begin
			MoveTo(10, i*20 + 2*20);
			if var2printArr[i].name = '' then
			begin
				Str(i, s);
				DrawString('['+ s+ '] ');
			end
			else
				DrawString(var2printArr[i].name + ': ');
			DrawString(var2printArr[i].value);
			// Debug:
//			Str(var2printArr[i].typeId, s);
//			DrawString(' Type: ' + s);
		end;
	end;
	FinishPort;
end;

procedure VariablesMouse2(theView: HIViewRef; where: CGPoint; mods, button: Longint; userData: Pointer);
var
	i: Longint;
	s: String;
	control: ControlRef;
	tmpName, tmpDisplayName: AnsiString;
//	localvar2DisplayVariable: VarInfoRec;
	procedure MoveToNewVariable(name, displayname, value: AnsiString; typeId: Longint);
	begin
		SetLength(var2History, Length(var2History)+1);
		var2History[High(var2History)] := var2DisplayVariable;

		var2DisplayVariable.name := name;
		var2DisplayVariable.displayname := displayname;
		var2DisplayVariable.value := value;
		var2DisplayVariable.typeId := typeId;
	end;
begin
	i := Trunc(where.y / 20) - 1;
	WriteLn('VariablesMouse2 at index ', i);
	
	// Modify var2DisplayVariable!
	// How depends on type.

	if i < 0 then // Back in history!
	begin
		if Length(var2History) > 0 then
		begin
			var2DisplayVariable := var2History[High(var2History)];
			SetLength(var2History, Length(var2History)-1);
			// Redisplay view 2 (self) and 3
			ResizeVariables;
			VMGetControl( bugs1Wind, 'Vari', 2, control);
			HIViewSetNeedsDisplay(control, true);
			VMGetControl( bugs1Wind, 'Vari', 3, control);
			HIViewSetNeedsDisplay(control, true);
			Exit(VariablesMouse2);
		end;
		Exit(VariablesMouse2); // Negative index never OK
	end;
	
	if Length(var2printArr) = 0 then
	begin
		// if scalar is a pointer
		// Händer bara vid pekare till pekare?
		if (Copy(var2DisplayVariable.value, 1, 2) = '0x') or (var2DisplayVariable.value = '(pointer)') then
		begin
			MoveToNewVariable('(*'+var2DisplayVariable.name + ')', var2DisplayVariable.displayname + '^', '', kScalarOrUnknown);
			WriteLn('Click on pointer ' + var2DisplayVariable.name + ' = ' + var2DisplayVariable.value);
		end
		else
			WriteLn('Click on scalar ' + var2DisplayVariable.value);
		// Ingen aning om typen den pekar på...
	end
	else
	begin
		if i > High(var2printArr) then
			Exit(VariablesMouse2);
		
		// Save to history
		SetLength(var2History, Length(var2History)+1);
		var2History[High(var2History)] := var2DisplayVariable;
		
//		if var2DisplayVariable.typeId = kPointer then
//			repl := ReadToPrompt(gCurrentProcess, 'print ' + var2DisplayVariable.name + '^', gDebuggerPrompt)
		tmpName := var2DisplayVariable.name;
		tmpDisplayName := var2DisplayVariable.displayname;
		if var2DisplayVariable.typeId = kPointer then
		begin
			tmpDisplayName := tmpDisplayName + '^';
//			tmpName := tmpName + '^'; if GDB and FPC!
			tmpName := '(*' + tmpName + ')';
			WriteLn('Click on pointer, got ', tmpName, ' (click on ', var2printArr[i].typeId, ')');
		end;
// Hur vet jag vad pekaren pekade på???
// Är det inte pekare längre efter ParseExpandVariable?

// print ra[2].d[3]^ funkar
		
		if var2printArr[i].name <> '' then // Name exists = record
		begin
			WriteLn('Record hit at ', i, '(', var2printArr[i].name, ')');
			tmpDisplayName := tmpDisplayName + '.' + var2printArr[i].displayname;
			// NOT ALWAYS RIGHT - Fails at array of record! ] at end means array. Right? Or [ at start?
//			if var2printArr[i].name[Length(var2printArr[i].name)] = ']' then
			if var2printArr[i].name[1] = '[' then
				tmpName := tmpName + var2printArr[i].name
			else
				tmpName := tmpName + '.' + var2printArr[i].name;
		end
		else
		begin
			WriteLn('Array hit at ', i);
			Str(i, s);
			tmpName := tmpName + '[' + s + ']';
			tmpDisplayName := tmpDisplayName + '[' + s + ']';
		end;

		if var2printArr[i].typeId = kPointer then // Was the clicked field a pointer?
		begin
			WriteLn('Clicked field ', i, ' is a pointer');
//			var2DisplayVariable.name := var2DisplayVariable.name + '^';
		end;
//		var2DisplayVariable.name := tmpName;
//		var2DisplayVariable.typeId := var2printArr[i].typeId;
//		var2DisplayVariable.value := var2printArr[i].value;
		MoveToNewVariable(tmpName, tmpDisplayName, var2printArr[i].value, var2printArr[i].typeId); // FIXA DISPLAYNAME!
	end;

//	var2DisplayVariable := localvar2DisplayVariable;
	
	// Redisplay view 2 (self) and 3
	ResizeVariables;
	VMGetControl( bugs1Wind, 'Vari', 2, control);
	HIViewSetNeedsDisplay(control, true);
	VMGetControl( bugs1Wind, 'Vari', 3, control);
	HIViewSetNeedsDisplay(control, true);
end;

procedure VariablesKey2(theView: HIViewRef; key: Char; mods: Longint; userData: Pointer);
begin
	WriteLn('VariablesKey');
end;


procedure VariablesDraw3(theView: HIViewRef; cgContext: CGContextRef; cgviewRect: CGRect; userData: Pointer);
var
	viewRect: Rect;
	i: Longint;
begin
	viewRect := CGRectToRect(cgviewRect);
//	UseViewContext(cgContext, cgviewRect.size.height);
	CreatePort(cgContext, cgviewRect.size.height);
	
	QDCG.TextSize(12);
//	ForeColor(lighterYellowColor);
	ForeColor(whiteColor);
	PaintRect(viewRect);
//	EraseRect(viewRect);

//	ForeColor(darkMagentaColor);
	ForeColor(blackColor);
	MoveTo(10, 30);
	
	if BugsRunning then
	if var2DisplayVariable.name <> '' then
	begin
//		UpdateVariables3Data; // Move to data in
		for i := 0 to High(gVariables3Data) do
		begin
			MoveTo(10, 30 + i * 20);
			DrawString(gVariables3Data[i]);
		end;
	end;
	FinishPort;
end;

procedure VariablesMouse3(theView: HIViewRef; where: CGPoint; mods, button: Longint; userData: Pointer);
begin
	WriteLn('VariablesMouse3');
end;
procedure VariablesKey3(theView: HIViewRef; key: Char; mods: Longint; userData: Pointer);
begin
	WriteLn('VariablesKey3');
end;

(*
Status view is removed!
procedure StatusDraw(theView: HIViewRef; viewRect: MacOSAll.Rect; userData: Pointer);
begin
	WriteLn('StatusDraw');
	MacOSAll.EraseRect(viewRect);
	MacOSAll.EraseRect(viewRect);
	MacOSAll.MoveTo(100, 100);
	MacOSAll.DrawString('Status');
end;

procedure StatusMouse(theView: HIViewRef; where: MacOSAll.Point; mods: Longint; userData: Pointer);
begin
	WriteLn('StatusMouse');
end;

procedure StatusKey(theView: HIViewRef; key: Char; mods: Longint; userData: Pointer);
begin
	WriteLn('StatusKey');
end;
*)


//const
//	kNumExpressions = 30;
var
	expressionString: array of AnsiString;
	expressionResults: array of AnsiString;
	expressionBox: array of ControlRef;
	expressionResultBox: array of ControlRef;

procedure UpdateExpression(i: Integer);
var
	j, c: Longint;
	s: AnsiString;
begin
	if i > 0 then
	if i <= High(expressionResultBox) then
	if Length(expressionString[i]) > 0 then
	begin
// If FPC, upper case on all expressions!
		if gPascalMode then
			s := ReadToPrompt(gCurrentProcess, 'print '+UpperCase(expressionString[i]), gDebuggerPrompt)
		else
			s := ReadToPrompt(gCurrentProcess, 'print '+expressionString[i], gDebuggerPrompt);
		if Length(s) > 0 then
		if s[1] = '$' then
		begin
			c := 1;
			repeat
				c += 1
			until (s[c] = '=') or (c > Length(s));
			s := Copy(s, c+2, Length(s)-c);
		end;
		// Tag bort (gdb) på slutet
		s := Copy(s, 1, Length(s)-6);
		// Tag bort alla vagnreturer
		// Möjlig felkällla? Nja, knappast.
		s := DelChars(s, #13);
		s := DelChars(s, #10);

//		for c := Length(s) downto 1 do
//		begin
//			if s[c] in [#10, #13] then
//				s := Copy(s, 1, c-1) + Copy(s, c+1, Length(s)-c);
//		end;
		VMSetStringValue(expressionResultBox[i], s);
	end;
end;

procedure UpdateExpressions;
var
	i: Longint;
begin
	for i := 0 to High(expressionBox) do
		UpdateExpression(i);
end;

// Problem: It seems impossible to know which text box that was changed. Why do I get
// another view reference than the one I put in? Answer: Old text boxes seem to be the problem.
// With the newer ones, it worked.
function ExpressionChanged(theView: HIViewRef; myPtr: Pointer): Boolean;
var
	i: Integer;
type
	AnsiStringPtr = ^AnsiString;
//var
//	theString: AnsiString;
//	theCFStr: CFStringRef;
begin
	for i := 0 to High(expressionBox) do
	if theView = expressionBox[i] then
		if BugsRunning then
			UpdateExpression(i);
end;

const
	kExpressionBoxHeight = 20;
var
	splitData, upperSplitData, lowerSplitData: SplitDataPtr;
	uplowScrollData, lowupScrollData, lowlowScrollData: ScrollDataPtr;

var
	gLastTotalLengthVariables1, gLastTotalLengthVariables2: Longint;
	
// Fungerar dåligt!
// Skalar inte om när jag vill.
procedure ResizeVariables; // Resize or data change
var
//	fr, pfr: HIRect;
//	root, parent: HIViewRef;
	r, pr: MacOSAll.Rect;
	totalLengthVariables1, totalLengthVariables2: Longint;
//	var0, var1, var2, var3: ControlRef;
begin
WriteLn('*** Anropar UpdateVariables2Data från ResizeVariables (rätt plats):');
	UpdateVariables2Data;
	UpdateVariables3Data;
	
// Inval properly? NOT enough!
//	HIViewSetNeedsDisplay(uplowScrollData^.contentView, true);
//	HIViewSetNeedsDisplay(lowupScrollData^.contentView, true);
//	HIViewSetNeedsDisplay(lowlowScrollData^.contentView, true);


//	VMGetControl( bugsWind, 'Vari', 0, var0);
//	VMGetControl( bugsWind, 'Vari', 1, var1);
//	VMGetControl( bugsWind, 'Vari', 2, var2);
//	VMGetControl( bugsWind, 'Vari', 3, var3);
//	VMGetControlId(upperSplitData^.upper, 'Vari', 0);
//	InstallSkelViewHandler(bugsWind, upperSplitData^.upper, VariablesDraw, VariablesMouse, VariablesKey, nil);
//	VMSetControlId(uplowScrollData^.contentView, 'Vari', 1);
//	InstallSkelViewHandler(bugsWind, uplowScrollData^.contentView, VariablesDraw1, VariablesMouse1, VariablesKey1, nil);
//	VMSetControlId(lowupScrollData^.contentView, 'Vari', 2);
//	InstallSkelViewHandler(bugsWind, lowupScrollData^.contentView, VariablesDraw2, VariablesMouse2, VariablesKey2, nil);
//	VMSetControlId(lowlowScrollData^.contentView, 'Vari', 3);

		// Jobbar från variablerna totalLengthVariables1, totalLengthVariables2
		// Måste matcha vad som ritas i VariablesDraw1, VariablesDraw2

		// variables 2: variable zoom view (lowUp)
		totalLengthVariables2 := 0;
		if var2printScalar <> '' then
			totalLengthVariables2 := 2
		else
			if Length(var2printArr) = 0 then
				totalLengthVariables2 := 2
			else
				totalLengthVariables2 := Length(var2printArr);
		
		GetControlBounds(lowUpScrollData^.contentView, r);
		GetControlBounds(lowUpScrollData^.peepHoleView, pr);
		if (totalLengthVariables2 <> gLastTotalLengthVariables2) or
			(pr.right - pr.left <> r.right - r.left) then
		begin
			// Variabelzoom, väl? (Variables2) Korrekt bredd, fel höjd.
			r.bottom := r.top + totalLengthVariables2 * 20 + 2*20;
			r.right := r.left + (pr.right - pr.left);  // Bredd skall anpassas efter innehållet!
			SetControlBounds(lowUpScrollData^.contentView, r);
			gLastTotalLengthVariables2 := totalLengthVariables2;
			HIViewSetNeedsDisplay(lowUpScrollData^.peepHoleView, true);
		end;

		// variables 1: variables list (upLow)
		totalLengthVariables1 := 0;
		if gSelectedFrame = 0 then // All
			totalLengthVariables1 := Length(globalVariables) + Length(backTraceVars)
		else
			if gSelectedFrame = 1 then // Globals
				totalLengthVariables1 := Length(globalVariables)
			else // Frame # 	if gSelectedFrame - 2
				totalLengthVariables1 := Length(backTraceVars);
WriteLn('totalLengthVariables1 = ', totalLengthVariables1);
		
		GetControlBounds(upLowScrollData^.contentView, r);
		GetControlBounds(upLowScrollData^.peepHoleView, pr);
		if (totalLengthVariables1 <> gLastTotalLengthVariables1) or
			(pr.right - pr.left <> r.right - r.left) then
		begin
			// Variables1, variabellistan
			r.bottom := r.top + totalLengthVariables1 * 20 + 1*20;
			r.right := r.left + (pr.right - pr.left); // Bredd skall anpassas efter innehållet!
			SetControlBounds(upLowScrollData^.contentView, r);
			gLastTotalLengthVariables1 := totalLengthVariables1;
			HIViewSetNeedsDisplay(upLowScrollData^.peepHoleView, true);
		end;
		
// uplowScrollData^.contentView = variables1?
// lowlowScrollData^.contentView = variables3=memory?
end;

// For Observe!
//procedure BugsResize(resized: Boolean);
procedure ObserveResize(theWind: WindowRef);
var
	i, oldCount, newCount: Longint;
	fr, wfr: HIRect;
	root, parent: HIViewRef;
	r: Rect;
	cr: CGRect;
begin
	if theWind <> observeWind then
		WriteLn('This should absolutely never happen!!?');
//	if resized then
	begin
		// Get size of window
		GetRootControl (observeWind, root);
		HIViewGetFrame (root, wfr);
		// Get parent of boxes
		//VMGetControl(observeWind, 'Expr', 0, parent);
		parent := SkelGetContentView(observeWind);
		HIViewGetFrame (parent, wfr);
		
//		WriteLn(Length(expressionBox));
//		WriteLn(wfr.size.height / kExpressionBoxHeight);
		
		// TEST: Kill old views!
		// Workaround for a bug that caused crashes after resize. Seems to work - but why?
		// NO - does not help!
//		for i := 0 to High(expressionBox) do
//		begin
//			DisposeControl(expressionBox[i]);
//			DisposeControl(expressionResultBox[i]);
//		end;
//		SetLength(expressionBox, 0);
//		SetLength(expressionResultBox, 0);
//		SetLength(expressionString, 0);
//		SetLength(expressionResults, 0);
		
		// Create subviews if needed
		if wfr.size.height / kExpressionBoxHeight > Length(expressionBox) then
		begin
			oldCount := Length(expressionBox);
			newCount := Trunc(wfr.size.height / kExpressionBoxHeight);
			i := High(expressionBox) +1;
			SetLength(expressionBox, newCount);
			SetLength(expressionResultBox, newCount);
			SetLength(expressionString, newCount);
			SetLength(expressionResults, newCount);
			for i := i to newCount-1 do
			begin
				expressionBox[i]:=nil;

				SetRect(r, 0,i*kExpressionBoxHeight, 300, (i+1)*kExpressionBoxHeight-1);
				cr := RectToCGRect(r);
				HITextViewCreate(HIRectPtr(@cr), 0, 0, expressionBox[i]);
				HIViewSetVisible(expressionBox[i], true);

				SetRect(r, 302,i*kExpressionBoxHeight, 600, (i+1)*kExpressionBoxHeight-1);
				cr := RectToCGRect(r);
				HITextViewCreate (HIRectPtr(@cr), 0, 0, expressionResultBox[i]);
				HIViewSetVisible(expressionResultBox[i], true);
				
				InstallViewHandlerByRef(observeWind, expressionBox[i], kViewDataString, @expressionString[i], ExpressionChanged, nil);
				VMSetStringValue(expressionBox[i], '');
				
				HIViewAddSubview (parent, expressionBox[i]);
				HIViewAddSubview (parent, expressionResultBox[i]);

			end;
		end;

		// Resizing of subviews
		for i := 0 to High(expressionBox) do
		begin
// OSStatus HIViewGetFrame (HIViewRef inView, HIRect *outRect);
// OSStatus HIViewSetFrame (HIViewRef inView, const HIRect *inRect);

			HIViewGetFrame (expressionBox[i], fr);
			// Modify
			fr.size.width := wfr.size.width / 2-1;
			HIViewSetFrame (expressionBox[i], fr);

			HIViewGetFrame (expressionResultBox[i], fr);
			// Modify
			fr.size.width := wfr.size.width / 2;
			fr.origin.x := wfr.size.width / 2+1;
			HIViewSetFrame (expressionResultBox[i], fr);
		end;
//		VMGetControl( bugsWind, 'Expr', 0, control);
		// Get the size
		// Get all
		
		// Resize variable views here?
		ResizeVariables;
	end;
end;

procedure ObserveUpdate(theView: HIViewRef; viewRect: QDCG.Rect; userData: Pointer);
begin
	ForeColor(lightGreyColor);
	PaintRect(viewRect);
end;


procedure BugsInit;
var
	control: HIViewRef;
	r: MacOSAll.Rect;
	rr: Rect;
begin
//	CreateWindowFromNib(SkelGetMainNib, CFSTR('Observe'), observeWind);
//	dummy := SkelWindow(observeWind, nil{@Mouse}, nil{@HelpKey}, BugsResize{@Update}, nil{@Activate}, nil{@DoClose}, nil{@Halt}, nil, true);
	SetRect(rr, 372, 65, 700, 250);
	observeWind := SkelNewWindow(rr, 'Observe', ObserveUpdate{theDrawProc: VMQDCGDrawProcPtr},
					nil {theMouseProc: VMQDCGMouseProcPtr}, nil{theKeyProc: TSKeyProcPtr}, nil {userData: Pointer},
					nil {pActivate: TSBooleanProcPtr}, nil{pClose}, nil{pClobber},
					nil {pIdle: TSNoArgProcPtr;}, true,
					0, nil, nil, 0, ObserveResize{resizeProc: TSResizeProcPtr});
//	observeWind := SkelNewWindow(rr, 'Observe');
//	dummy := SkelWindow(observeWind, nil{@Mouse}, nil{@HelpKey}, nil{@Update}, nil{@Activate}, nil{@DoClose}, nil{@Halt}, nil, true);

//	CreateWindowFromNib(SkelGetMainNib, CFSTR('LightBugs'), bugsWind);
	SetRect(rr, 272, 65, 900, 700);
	bugs1Wind := SkelNewWindow(rr, 'Light Bugs', nil{theDrawProc: VMQDCGDrawProcPtr},
					nil {theMouseProc: VMQDCGMouseProcPtr}, nil{theKeyProc: TSKeyProcPtr}, nil {userData: Pointer},
					nil {pActivate: TSBooleanProcPtr}, nil{pClose}, nil{pClobber},
					nil {pIdle: TSNoArgProcPtr;}, true,
					0, nil, nil, 0, nil{resizeProc: TSResizeProcPtr});
//	bugs1Wind := SkelNewWindow(rr, 'Light Bugs');
//	dummy := SkelWindow(bugs1Wind, nil{@Mouse}, nil{@HelpKey}, nil{@Update}, nil{@Activate}, nil{@DoClose}, nil{@Halt}, nil, true);

//	InstallAllTabs(bugsWind);
	
//	NewDView(bugsWind, 'Stac', 0); // Install HIView to be used as displayview
//	NewDView(bugsWind, 'Vari', 0); // Install HIView to be used as displayview
//	NewDView(bugsWind, 'Stat', 0); // Install HIView to be used as displayview

//	VMGetControl( bugs1Wind, 'Vari', 0, control);
	control := SkelGetContentView(bugs1Wind);
	VMSetControlId(control, '----', 100); // or dispose?
	splitData := EmbedInSplitter(nil, control, false, 0.4); // Split main Variables view
	upperSplitData := EmbedInSplitter(nil, splitData^.upper, true, 0.3); // Split upper sideways
	lowerSplitData := EmbedInSplitter(nil, splitData^.lower, false, 0.5); // Split main Variables view
	
	MacOSAll.SetRect(r, 0,0,600,800);
	uplowScrollData := EmbedInScroller(nil, upperSplitData^.lower, true, true, true, r);
	lowupScrollData := EmbedInScroller(nil, lowerSplitData^.upper, true, true, true, r);
	lowlowScrollData := EmbedInScroller(nil, lowerSplitData^.lower, true, true, true, r);

	VMSetControlId(upperSplitData^.upper, 'Vari', 0);
//	InstallQDSkelViewHandler(bugsWind, upperSplitData^.upper, VariablesDraw, VariablesMouse, VariablesKey, nil);
	InstallSkelViewHandler(bugs1Wind, upperSplitData^.upper, VariablesDraw, VariablesMouse, VariablesKey, nil);
	VMSetControlId(uplowScrollData^.contentView, 'Vari', 1);
//	InstallQDSkelViewHandler(bugsWind, uplowScrollData^.contentView, VariablesDraw1, VariablesMouse1, VariablesKey1, nil);
	InstallSkelViewHandler(bugs1Wind, uplowScrollData^.contentView, VariablesDraw1, VariablesMouse1, VariablesKey1, nil);
	VMSetControlId(lowupScrollData^.contentView, 'Vari', 2);
//	InstallQDSkelViewHandler(bugsWind, lowupScrollData^.contentView, VariablesDraw2, VariablesMouse2, VariablesKey2, nil);
	InstallSkelViewHandler(bugs1Wind, lowupScrollData^.contentView, VariablesDraw2, VariablesMouse2, VariablesKey2, nil);
	VMSetControlId(lowlowScrollData^.contentView, 'Vari', 3);
//	InstallQDSkelViewHandler(bugsWind, lowlowScrollData^.contentView, VariablesDraw3, VariablesMouse3, VariablesKey3, nil);
	InstallSkelViewHandler(bugs1Wind, lowlowScrollData^.contentView, VariablesDraw3, VariablesMouse3, VariablesKey3, nil);

	gLastTotalLengthVariables1 := 1;
	gLastTotalLengthVariables2 := -1;

// Bakåtknapp i var2
// lowupScrollData^.contentView
// Nej, det är lättare att bara rita den!

// Så här:
// Större vyer skall ligga i Scrollers
// Ytan delas flera gånger av Splitters
// gärna lite i stil med Lightsbug

// Status view is removed
//	VMGetControl( bugsWind, 'Stat', 0, control);
//	InstallQDSkelViewHandler(bugsWind, control, StatusDraw, StatusMouse, StatusKey, nil);

//	VMGetControl( bugsWind, 'Expr', 0, control);
//	BugsResize(true); // CreateExpressionControls(control);
	ObserveResize(observeWind);

// Init subunits
	DirectGDBInit; // Init DirectGDB - now in its own window
	InitSetVariableDialog;

// Skapa fönstret? Dolt?
	BugsDisable;
end;

// Nån pollning måste in. Vem gör den? Huvudprogrammets bakgrundsprocess?
// RunDone och RunData som i huvudprogrammet.

// RunData är viktigast! Tar emot data, parsar, lägger ut data i debugfönster,
// visar positionen (via anrop till LWPEdit), och gör fler anrop vid behov.

// RunDone bara stänger av debuggerinterfacet, inaktiverar allting.
// OBS att build+RunWithDebug måste kunna göras.

end.
