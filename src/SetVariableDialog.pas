// Small dialog for setting a variable though GDB.
// Activated by alt-clicking variables in LightBugs.

{$mode macpas}
unit SetVariableDialog;
interface
uses
	MacOSAll, TransSkel4, QDCG, Backtrace, ProcessUtils, LWPGlobals;

procedure ShowSetVariableDialog(var2Edit: VarInfoRec);
procedure InitSetVariableDialog;

implementation

uses
	LWPEdit;

var
	str: AnsiString;
	textControl, staticTextControl: ControlRef;
	gSetVariableWindow: WindowPtr;
	gVarEdit: VarInfoRec;

procedure DoClose;
begin
	HideWindow(gSetVariableWindow);
end;

function TextInput(theView: HIViewRef; myPtr: Pointer): Boolean;
begin
end;

procedure OKProc(cref: ControlRef; part: ControlPartCode);MWPascal;
var
	str: AnsiString;
	w: WindowPtr;
begin
// Fult sätt att markera hela texten
	SetKeyboardFocus(gSetVariableWindow, textControl, kControlFocusNoPart);
	SetKeyboardFocus(gSetVariableWindow, textControl, kControlFocusNextPart);

//	WriteLn(str);
//	str := VMGetStringValue(textControl);
	str := VMGetStringValue(textControl);
// Why do I need to get the value? It should be passed to str automatically!
	
	// Tell editor to go to line!
	HideWindow(gSetVariableWindow);
	// Send i to editor
	w := GetFrontEditWindow;
//	SetVariable(w, str, 0); // set var <name> = str
	if gCurrentProcess <> nil then
	begin
		ProcessWrite(gCurrentProcess, 'set var ' + gVarEdit.name + ':=' + str + #10);
//		s := ReadToPrompt(gCurrentProcess, 'set var ' + gVarEdit.name + ':=' + str + #10, '(gdb) ');
	end;
	// NOTE: Pascal only now!
	
	// Must also trigger an update!
end;

procedure CancelProc(cref: ControlRef; part: ControlPartCode);MWPascal;
begin
	HideWindow(gSetVariableWindow);
end;

function Filter(nextHandler: EventHandlerCallRef; inEvent: EventRef; inUserData: Pointer ):OSStatus; MWPascal;
var
	key: Char;
//	front: WindowPtr;
begin
	GetEventParameter (inEvent, kEventParamKeyMacCharCodes,
					typeChar, nil, SizeOf(Char), nil, @key);
	
	if key = Char(13) then
	begin
		OKProc(nil, 0);
		return noErr;
	end
	else if key = Char($27) then
	begin
		CancelProc(nil, 0);
		return noErr;
	end
	else if key in ['0'..'9'] then
	begin
//		CallNextEventHandler(nextHandler, inEvent);
		return eventNotHandledErr;
	end
	else
	begin
	// Should block if not cmd?
//		CallNextEventHandler(nextHandler, inEvent);
		return eventNotHandledErr;
	end;
//		return noErr;
end;

procedure ShowSetVariableDialog(var2Edit: VarInfoRec);
begin
	gVarEdit := var2Edit;
	
	ShowWindow(gSetVariableWindow);
	SetPortWindowPort(gSetVariableWindow);
	SelectWindow(gSetVariableWindow);

//	System.Str(row, str);
	VMSetStringValue(staticTextControl, var2Edit.name);
	VMSetStringValue(textControl, var2Edit.value);

// Fult sätt att markera hela texten
	SetKeyboardFocus(gSetVariableWindow, textControl, kControlFocusNoPart);
	SetKeyboardFocus(gSetVariableWindow, textControl, kControlFocusNextPart);
end;

procedure InitSetVariableDialog;
var
	window: WindowPtr;
	r: Rect;
	err: OSStatus;
begin
	SetRect(r, 200, 80, 400, 220);

	window := SkelNewWindow(r, 'Set variable', nil, nil, nil, nil,
		nil, @DoClose, nil, nil, true, 0, nil, nil, 0, nil {@DoResize});

// Create some simple controls
	SkelNewButton(window, 'Cancel', 20,100,60,20, CancelProc);
	SkelNewButton(window, 'OK', 100,100,60,20, OKProc);
	
	staticTextControl := SkelNewStaticTextField(window, 20, 20, 120, 20, 'no variable');
	textControl := SkelNewTextField(window, 20,60,120,20, 'no value');
	str := '(nothing)';
	InstallViewHandlerByRef(window, textControl, kViewDataString, @str, TextInput, Filter);
	
	SkelWindow(window, nil{@WindMouse}, nil{@WindKey},  nil{@WindUpdate}, nil{@WindActivate}, @DoClose, nil{@WindHalt}, nil, false);
	
	gSetVariableWindow := window;
end;

end.
