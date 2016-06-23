{$mode macpas}
unit Breakpoints;
interface
uses
	MacOSAll, LWPGlobals, LightBugs, UtilsTypes, FileUtils, SimpleParser, ProcessUtils, Console;

type
	BreakpointPtr = ^BreakpointRec;
	BreakpointRec = record
		line: Longint;
		filename: AnsiString;
		next: BreakpointPtr;
		enabled: Boolean;
		brNumber: Longint; // Needed for LLDB
	end;

procedure ToggleBreakpoint(theWind: WindowPtr; line, mods: Longint);
function GetWindowBreakpoints(theWind: WindowPtr): BreakpointPtr;
procedure ClearBreakpoints(editIndex: Integer);

function AtLeastOneBreakPoint : boolean;		// used to check if there are any breakpoints set at all

procedure InitBreakpoints;

// Breakpoint correction:
function GetBreakpointLineFromReply(s: AnsiString; var brNumber: Longint): Longint;
procedure MoveBreakpoint(line, actualLine: Longint; theWind: WindowPtr; brNumber: Longint);
//procedure MoveBreakpoint(line, actualLine: Longint; theWind: WindowPtr);

implementation

uses
	LWPEdit;
	
var
	gBreakpointList: array [1..kMaxEditWindows] of BreakpointPtr;

// User set or reset breakpoint
procedure ChangedBreakpoints(theWind: WindowPtr; line: Longint; add: Boolean; br: BreakpointPtr);
var
	theSpec: FSSpecString;
	numStr: Str255;
	err: OSErr;
	s: AnsiString;
	actualLine: Longint;
	brNumber: Longint;
	brStr: ShortString;
	editIndex: Longint;
begin
	if not BugsRunning then Exit(ChangedBreakpoints);
	
	editIndex := GetWRefCon(theWind);
	
	// Filename or full path?
	err := GetEditFSSpec(theWind, theSpec);
	if err <> noErr then Exit(ChangedBreakpoints);
	
	NumToString(line, numStr);
	
//	WriteLn('ChangedBreakpoints ', line, GetLastToken(theSpec));
	
	// PROBLEM: Hangs if breakpoint set when running
	// Check if running?
	// (Fixed with timeout?)
	if gCurrentProcess <> nil then
		if add then
		begin
			if BugsDebuggerIsGDB then
//			if debugName = 'gdb' then
				s := ReadToPrompt(gCurrentProcess, 'break ' + GetLastToken(theSpec) + ':' + numStr, BugsGetPrompt)
			else // lldb
				s := ReadToPrompt(gCurrentProcess, 'b ' + GetLastToken(theSpec) + ':' + numStr, BugsGetPrompt);
			WriteLn('Breakpoint reply: '+ s);
			HelpAppendLn('Breakpoint reply: '+ s);
			// LLDB reply: 
			// Breakpoint 2: where = test`PASCALMAIN + 119 at test.pas:38, address = 0x00011007

			
			// Parse s to check where it REALLY ended up!
			// Two rows in reply, number is last in first and first in last (!)

			actualLine := GetBreakpointLineFromReply(s, brNumber);
//			if line <> actualLine then - anropa ALLTID för att spara brNumber!
//			NumToString(brNumber, brStr);
//			WriteLn('Breakpoint number: '+ brStr);
//			HelpAppendLn('Breakpoint number: '+ brStr);
			MoveBreakpoint(line, actualLine, theWind, brNumber);

//			NumToString(brNumber, numStr);
//			WriteLn('br move got ' + numStr);
//			HelpAppendLn('br move got ' + numStr);
			br^.brNumber := brNumber;
//			br^.brNumber := brNumber; Must be saved SOMEWHERE - can MoveBreakpoint do it?
		end
		else
		if BugsDebuggerIsGDB then
		begin
			s := ReadToPrompt(gCurrentProcess, 'clear ' + GetLastToken(theSpec) + ':' + numStr, BugsGetPrompt);
		end
		else
		begin
			// LLDB delete breakpoint!
			// br del NR
			// So what is the number?
			// Either save at other time, or br l and parse!
			NumToString(br^.brNumber, numStr);
			WriteLn('br del ' + numStr);
			HelpAppendLn('br del ' + numStr);
			s := ReadToPrompt(gCurrentProcess, 'br del ' + numStr, BugsGetPrompt);
			WriteLn('br del reply: '+ s);
			HelpAppendLn('br del reply: '+ s);
			// We don't need to dispose br since it is done by ToggleBreakpoint.
		end;
end;

// Entry point for changing breakpoints! Called from the editor window at mouse down!
// We know which file, line and modifiers!
procedure ToggleBreakpoint(theWind: WindowPtr; line, mods: Longint);
var
	prev, br: BreakpointPtr;
	editIndex: Integer;
	err: OSErr;
	theSpec: FSSpecString;
begin
	prev := nil;

	editIndex := GetWRefCon(theWind);
	if editIndex <= 0 then Exit(ToggleBreakpoint);
	if editIndex > kMaxEditWindows then Exit(ToggleBreakpoint);
	
	err := GetEditFSSpec(theWind, theSpec);
	
	// Did we click an existing (known) breakpoint?
	br := gBreakpointList[editIndex];
	while br <> nil do
	begin
		if br^.line = line then
			break;
		prev := br;
		br := br^.next;
	end;
	// Now br and prev refers to the clicked and previous breakpoint.
	// br = nil if none is found.

	if mods = 0 then // Normal click (toggle)
	begin
		if br <> nil then
		begin
			// Found one! Ditch it.
			if prev = nil then // first
				gBreakpointList[editIndex] := br^.next
			else
				prev^.next := br^.next;
			ChangedBreakpoints(theWind, line, false, br); // Remove notification
			DisposePtr(Ptr(br));
		end
		else
		begin
			br := BreakpointPtr(NewPtrClear(SizeOf(BreakPointRec)));
			br^.next := gBreakpointList[editIndex];
			br^.line := line;
			br^.filename := GetLastToken(theSpec);
			br^.enabled := true;
			gBreakpointList[editIndex] := br;

			ChangedBreakpoints(theWind, line, true, br); // Set breakpoint notification
		end;
	end // End of normal click (toggle)
	else
	if (mods and optionKey) <> 0 then
	begin
		BugsRunTo(theWind, line); // Set temporary breakpoint and run
	end
	else
	if (mods and shiftKey) <> 0 then // Toggle disabled breakpoint
	begin
		if br <> nil then
		begin
			// Found one! Dis/enable it.
			br^.enabled := not br^.enabled;
			ChangedBreakpoints(theWind, line, br^.enabled, br); // Toggle breakpoint
		end
		else
		begin // Found none. Create it disabled.
			br := BreakpointPtr(NewPtrClear(SizeOf(BreakPointRec)));
			br^.next := gBreakpointList[editIndex];
			br^.line := line;
			br^.filename := GetLastToken(theSpec);
			br^.enabled := false;
			gBreakpointList[editIndex] := br;

			ChangedBreakpoints(theWind, line, true, br); // Set breakpoint notification
		end;
	end;
	
// TO DO: Notify debugger if it is active!
end;

// Hur ange fönster? WindowPtr? Index? FSSpecString?
function GetWindowBreakpoints(theWind: WindowPtr): BreakpointPtr;
var
	editIndex: Integer;
begin
	editIndex := GetWRefCon(theWind);
	if editIndex > 0 then
		if editIndex <= kMaxEditWindows then
			return gBreakpointList[editIndex];
	return nil;
end;

procedure ClearBreakpoints(editIndex: Integer);
var
	br, prev: BreakpointPtr;
begin
	br := gBreakpointList[editIndex];
	while br <> nil do
	begin
		prev := br;
		br := br^.next;
		DisposePtr(Ptr(prev));
	end;
	gBreakpointList[editIndex] := nil;
end;

function AtLeastOneBreakPoint : boolean;		
var
	i: Longint;
begin
	for i := 1 to kMaxEditWindows do
		if (gBreakpointList[i] <> nil) then return true;
	return false;
end;

procedure InitBreakpoints;
var
	i: Longint;
begin
	for i := 1 to kMaxEditWindows do
		gBreakpointList[i] := nil;
end;


// Correction of breakpoint positions

// Expected: Something like
// Breakpoint 3 at 0x1164d: file test.pas, line 11.
// (gdb)
// or for LLDB:
// Breakpoint 2: where = test`PASCALMAIN + 119 at test.pas:38, address = 0x00011007

function GetBreakpointLineFromReply(s: AnsiString; var brNumber: Longint): Longint;
var
	i, endpoint, line: Longint;
	nums: AnsiString;
	tokenType: Longint;
	tokenValue, fileName: AnsiString;
begin
	// NEW AND BETTER PARSING!
	// Parse to "at"
	// Then branch depending on next
	i := 0;
	SimpleParserGetToken(s, i, tokenType, tokenValue); // Breakpoint
	if tokenValue <> 'Breakpoint' then
		SimpleParserGetToken(s, i, tokenType, tokenValue); // Breakpoint
	WriteLn('Breakpoint = ',  tokenValue);
	HelpAppendLn('Breakpoint = ' + tokenValue);
	SimpleParserGetToken(s, i, tokenType, tokenValue); // number
	Val(tokenValue, brNumber);
	WriteLn('Br number ', brNumber, ' = ', tokenValue);
	HelpAppendLn('Br number ' + tokenValue);
	repeat
		SimpleParserGetToken(s, i, tokenType, tokenValue);
	until (i >= Length(s)) or (tokenValue = 'at');
	SimpleParserGetToken(s, i, tokenType, tokenValue);
	if tokenType = kHexToken then
	begin // gdb
		SimpleParserGetToken(s, i, tokenType, tokenValue); // :
		SimpleParserGetToken(s, i, tokenType, tokenValue); // "file"
		SimpleParserGetToken(s, i, tokenType, fileName); // filename
		SimpleParserGetToken(s, i, tokenType, tokenValue); // ,
		SimpleParserGetToken(s, i, tokenType, tokenValue); // "line"
		SimpleParserGetToken(s, i, tokenType, nums); // line number	
	end
	else
	begin // lldb
		SimpleParserGetToken(s, i, tokenType, fileName); // filename
		SimpleParserGetToken(s, i, tokenType, tokenValue); // :
		SimpleParserGetToken(s, i, tokenType, nums); // line number
	end;
	if Length(nums) > 0 then
		Val(nums, line)
	else
		line := -1;
	GetBreakpointLineFromReply := line;
	
	
(*	
	if Length(s) > 3 then // probably includes prompt
	begin
		i := 1;
		while not (s[i] in [#13, #10]) and (i < Length(s)) do
			i += 1;
		if (i < Length(s)) and (i > 2) then
		begin
			i -= 1;
			if s[i] = '.' then i -= 1; // Ends with '.' after number
			endpoint := i;
			while (s[i] in ['0'..'9']) and (i > 1) do
				i -= 1;
			nums := Copy(s, i+1, endpoint - i);
			WriteLn('GetBreakpointLineFromReply found "', nums, '"');
			WriteLn('when searching: "', s, '"');
			if Length(nums) > 0 then
				Val(nums, line)
			else
				line := -1;
			GetBreakpointLineFromReply := line;
		end
	end
	else
		GetBreakpointLineFromReply := -1;
*)
end;

procedure MoveBreakpoint(line, actualLine: Longint; theWind: WindowPtr; brNumber: Longint);
var
	br: BreakpointPtr;
	r: Rect;
	lineStr, numStr: ShortString;
begin
	NumToString(brNumber, numStr);
	NumToString(line, lineStr);
	WriteLn('MoveBreakpoint nr ' + numStr + ' by line ', lineStr);
	HelpAppendLn('MoveBreakpoint nr ' + numStr + ' by line '+ lineStr);

	if actualLine > 0 then
		if actualLine <> line then
		begin
			br := GetWindowBreakpoints(theWind); // Get the first in the list
			if br <> nil then
			// Stega igenom listan efter den med rätt rad!
			while br <> nil do
			begin
				if line = br^.line then
				begin
					br^.line := actualLine;
					if brNumber >= 0 then
					begin
						br^.brNumber := brNumber;
	WriteLn('Saved breakpoint nr ' + numStr);
	HelpAppendLn('Saved breakpoint nr ' + numStr);
					end;
					GetPortBounds(GetWindowPort(theWind), r);
					SetRect(r, 0, 0, 15, r.bottom);
					InvalWindowRect(theWind, r);
				end;
				br := br^.next;
			end;
		end;
end;

end.
