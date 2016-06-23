// Simplified one-line standard alerts
{$mode macpas}

unit AlertsUtils;
interface
uses
	MacOSAll;

procedure MessageAlert(msg, expl: CFStringRef); overload;
procedure MessageAlert(msg, expl: AnsiString); overload;
function QuestionAlert(msg, expl: CFStringRef): Boolean; overload;
function QuestionAlert(msg, expl: AnsiString): Boolean; overload;
function YesNoQuestionAlert(msg, expl: CFStringRef): Boolean; overload;
function YesNoQuestionAlert(msg, expl: AnsiString): Boolean; overload;
function YesNoCancelQuestionAlert(msg, expl: CFStringRef): Integer; overload;
function YesNoCancelQuestionAlert(msg, expl: AnsiString): Integer; overload;
procedure MessageSheet(msg, expl: CFStringRef; theWind: WindowPtr); overload;
procedure MessageSheet(msg, expl: AnsiString; theWind: WindowPtr); overload;

// Gamla
procedure DebugStr(msg: AnsiString);
function QuestionStr(q: AnsiString): Boolean;
function YesNoQuestionStr(q: AnsiString): Boolean;
function YesNoCancelQuestionStr(q: AnsiString): Integer;


implementation

// ----------------

procedure MessageAlert(msg, expl: CFStringRef); overload;
var
	theDialog: DialogPtr;
	itemHit: DialogItemIndex; 
begin
	CreateStandardAlert(kAlertCautionAlert, msg, expl, nil, theDialog);
	RunStandardAlert(theDialog, nil, itemHit);
end;

procedure MessageAlert(msg, expl: AnsiString); overload;
var
	msgCFStr, explCFStr: CFStringRef;
begin
	// Konvertera till CFString, anropa andra, avallokera CFStrings
	msgCFStr := CFStringCreateWithBytes(nil, @msg[1], Length(msg), kCFStringEncodingMacRoman, false);
	if Length(expl) > 0 then
		explCFStr := CFStringCreateWithBytes(nil, @expl[1], Length(expl), kCFStringEncodingMacRoman, false)
	else
		explCFStr := nil;
	MessageAlert(msgCFStr, explCFStr);
	CFRelease(msgCFStr);
	if explCFStr <> nil then
		CFRelease(explCFStr);
end;

function QuestionAlert(msg, expl: CFStringRef): Boolean; overload;
var
	theDialog: DialogPtr;
	itemHit: DialogItemIndex;
	param: AlertStdCFStringAlertParamRec;
begin
	param.version := kStdCFStringAlertVersionOne;
	param.movable := true;
	param.helpButton := false;
	param.flags := 0;
	param.position := kWindowDefaultPosition;
	param.cancelButton := 2;
	param.defaultButton := 1;
	param.defaultText := CFSTR('OK');
	param.otherText := nil;
	param.cancelText := CFSTR('Cancel');
	
	CreateStandardAlert(kAlertCautionAlert, msg, expl, @param, theDialog);
	RunStandardAlert(theDialog, nil, itemHit);
	return itemHit = 1;
end;

function QuestionAlert(msg, expl: AnsiString): Boolean; overload;
var
	msgCFStr, explCFStr: CFStringRef;
begin
	// Konvertera till CFString, anropa andra, avallokera CFStrings
	msgCFStr := CFStringCreateWithBytes(nil, @msg[1], Length(msg), kCFStringEncodingMacRoman, false);
	if Length(expl) > 0 then
		explCFStr := CFStringCreateWithBytes(nil, @expl[1], Length(expl), kCFStringEncodingMacRoman, false)
	else
		explCFStr := nil;
	QuestionAlert := QuestionAlert(msgCFStr, explCFStr);
	CFRelease(msgCFStr);
	if explCFStr <> nil then
		CFRelease(explCFStr);
end;

function YesNoQuestionAlert(msg, expl: CFStringRef): Boolean; overload;
var
	theDialog: DialogPtr;
	itemHit: DialogItemIndex;
	param: AlertStdCFStringAlertParamRec;
begin
	param.version := kStdCFStringAlertVersionOne;
	param.movable := true;
	param.helpButton := false;
	param.flags := 0;
	param.position := kWindowDefaultPosition;
	param.cancelButton := 2;
	param.defaultButton := 1;
	param.defaultText := CFSTR('Yes');
	param.otherText := nil;
	param.cancelText := CFSTR('No');
	
	CreateStandardAlert(kAlertCautionAlert, msg, expl, @param, theDialog);
	RunStandardAlert(theDialog, nil, itemHit);
	return itemHit = 1;
end;

function YesNoQuestionAlert(msg, expl: AnsiString): Boolean; overload;
var
	msgCFStr, explCFStr: CFStringRef;
begin
	// Konvertera till CFString, anropa andra, avallokera CFStrings
	msgCFStr := CFStringCreateWithBytes(nil, @msg[1], Length(msg), kCFStringEncodingMacRoman, false);
	if Length(expl) > 0 then
		explCFStr := CFStringCreateWithBytes(nil, @expl[1], Length(expl), kCFStringEncodingMacRoman, false)
	else
		explCFStr := nil;
	YesNoQuestionAlert := YesNoQuestionAlert(msgCFStr, explCFStr);
	CFRelease(msgCFStr);
	if explCFStr <> nil then
		CFRelease(explCFStr);
end;


function YesNoCancelQuestionAlert(msg, expl: CFStringRef): Integer; overload;
var
	theDialog: DialogPtr;
	itemHit: DialogItemIndex;
	param: AlertStdCFStringAlertParamRec;
begin
	param.version := kStdCFStringAlertVersionOne;
	param.movable := true;
	param.helpButton := false;
	param.flags := 0;
	param.position := kWindowDefaultPosition;
	param.cancelButton := 3;
	param.defaultButton := 1;
	param.defaultText := CFSTR('Yes');
	param.otherText := CFSTR('Cancel');
	param.cancelText := CFSTR('No');
	
	CreateStandardAlert(kAlertCautionAlert, msg, expl, @param, theDialog);
	RunStandardAlert(theDialog, nil, itemHit);
	return itemHit;
end;


function YesNoCancelQuestionAlert(msg, expl: AnsiString): Integer; overload;
var
	msgCFStr, explCFStr: CFStringRef;
begin
	// Konvertera till CFString, anropa andra, avallokera CFStrings
	msgCFStr := CFStringCreateWithBytes(nil, @msg[1], Length(msg), kCFStringEncodingMacRoman, false);
	if Length(expl) > 0 then
		explCFStr := CFStringCreateWithBytes(nil, @expl[1], Length(expl), kCFStringEncodingMacRoman, false)
	else
		explCFStr := nil;
	YesNoCancelQuestionAlert := YesNoCancelQuestionAlert(msgCFStr, explCFStr);
	CFRelease(msgCFStr);
	if explCFStr <> nil then
		CFRelease(explCFStr);
end;

// ----------------

procedure MessageSheet(msg, expl: CFStringRef; theWind: WindowPtr); overload;
var
	theDialog: DialogPtr;
begin
	CreateStandardSheet(kAlertCautionAlert, msg,
			expl, nil, GetWindowEventTarget(theWind), theDialog);
	ShowSheetWindow(GetDialogWindow(theDialog), theWind);
end;

procedure MessageSheet(msg, expl: AnsiString; theWind: WindowPtr); overload;
var
	msgCFStr, explCFStr: CFStringRef;
begin
	// Konvertera till CFString, anropa andra, avallokera CFStrings
	msgCFStr := CFStringCreateWithBytes(nil, @msg[1], Length(msg), kCFStringEncodingMacRoman, false);
	if Length(expl) > 0 then
		explCFStr := CFStringCreateWithBytes(nil, @expl[1], Length(expl), kCFStringEncodingMacRoman, false)
	else
		explCFStr := nil;
	MessageSheet(msgCFStr, explCFStr, theWind);
	CFRelease(msgCFStr);
	if explCFStr <> nil then
		CFRelease(explCFStr);
end;

// ----------------

procedure DebugStr(msg: AnsiString);
begin
	MessageAlert(msg, '');
end;

function QuestionStr(q: AnsiString): Boolean;
begin
	return QuestionALert(q, '');
end;

function YesNoQuestionStr(q: AnsiString): Boolean;
begin
	return YesNoQuestionAlert(q, '');
end;

function YesNoCancelQuestionStr(q: AnsiString): Integer;
begin
	return YesNoCancelQuestionAlert(q, '');
end;

end.
