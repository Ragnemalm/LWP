{$mode macpas}
unit GoToLineDialog;
interface
uses
	MacOSAll, TransSkel4, QDCG;

procedure ShowGoToLineDialog(row: Longint);
procedure InitGoToLineDialog;

implementation

uses
	LWPEdit;

var
	str: AnsiString;
	textControl: ControlRef;
	gGoToLineWindow: WindowPtr;

procedure DoClose;
begin
	HideWindow(gGoToLineWindow);
end;

function TextInput(theView: HIViewRef; myPtr: Pointer): Boolean;
begin
end;

procedure OKProc(cref: ControlRef; part: ControlPartCode);MWPascal;
var
	i: Longint;
	w: WindowPtr;
begin
// Fult sätt att markera hela texten
	SetKeyboardFocus(gGoToLineWindow, textControl, kControlFocusNoPart);
	SetKeyboardFocus(gGoToLineWindow, textControl, kControlFocusNextPart);

//	WriteLn(str);
//	str := VMGetStringValue(textControl);
	i := VMGetNumStringValue(textControl);
// Why do I need to get the value? It should be passed to str automatically!
	
	// Tell editor to go to line!
	HideWindow(gGoToLineWindow);
//	Val(str, i);
	// Send i to editor
	w := GetFrontEditWindow;
	GoToLine(w, i, 0);
end;

procedure CancelProc(cref: ControlRef; part: ControlPartCode);MWPascal;
begin
	HideWindow(gGoToLineWindow);
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

procedure ShowGoToLineDialog(row: Longint);
begin
	ShowWindow(gGoToLineWindow);
	SetPortWindowPort(gGoToLineWindow);
	SelectWindow(gGoToLineWindow);

	System.Str(row, str);
	VMSetNumStringValue(textControl, row);

// Fult sätt att markera hela texten
	SetKeyboardFocus(gGoToLineWindow, textControl, kControlFocusNoPart);
	SetKeyboardFocus(gGoToLineWindow, textControl, kControlFocusNextPart);
end;

// New variant? SkelNewWindow needs to be a bit more flexible!
function SkelNewWindowAlt(r: QDCG.Rect; title: AnsiString; closeable, resizable, minimizable: Boolean): WindowRef;
var
	theWind: WindowRef;
	theWindowAttributes: WindowAttributes;
begin
	theWindowAttributes := 0;
	if closeable then
		theWindowAttributes := theWindowAttributes or kWindowCloseBoxAttribute;
	if resizable then
		theWindowAttributes := theWindowAttributes or kWindowResizableAttribute;
	if minimizable then
		theWindowAttributes := theWindowAttributes or kWindowCollapseBoxAttribute;
	CreateNewWindow ( kDocumentWindowClass, theWindowAttributes, RectToMacRect(r), theWind);
	SetWTitle(theWind, title);
	InstallStandardEventHandler(GetWindowEventTarget(theWind));
	ShowWindow(theWind);	
	SkelNewWindowAlt := theWind;

	SkelWindow(theWind, nil, nil, nil, nil,
					nil, nil, nil, nil, true,
					kEventDurationSecond, nil, nil, 0, nil);
end;

procedure InitGoToLineDialog;
var
	window: WindowPtr;
	r: Rect;
//	err: OSStatus;
begin
	SetRect(r, 200, 80, 400, 180);
//	err := CreateNewWindow ( kDocumentWindowClass, kWindowCloseBoxAttribute, r, window);
//	SetWTitle(window, 'Go to line number');
//	SetPortWindowPort(window);

	window := SkelNewWindowAlt(r, 'Go to line number', true, false, false);
	SkelWindow( window, nil, nil, nil, nil,
		nil, @DoClose, nil, nil, true, 0, nil, nil, 0, nil {@DoResize});
	HideWindow(window);

// Create some simple controls
//	SkelNewStaticTextField(window, 20, 20, 180, 20, 'Go to line number:');
	SkelNewButton(window, 'Cancel', 20,60,60,20, CancelProc);
	SkelNewButton(window, 'OK', 100,60,60,20, OKProc);
	
	textControl := SkelNewTextField(window, 20,20,120,20, '');
	str := '123';
	InstallViewHandlerByRef(window, textControl, kViewDataString, @str, TextInput, Filter);
	
	SkelWindow(window, nil{@WindMouse}, nil{@WindKey},  nil{@WindUpdate}, nil{@WindActivate}, @DoClose, nil{@WindHalt}, nil, false);
	
	gGoToLineWindow := window;
end;

end.
