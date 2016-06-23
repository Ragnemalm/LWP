// Draft for a new TransSkel, version 5
// Old functionality remaining: easy handling of menus, mousedowns,
// keyboard, background task. Similar to old system.
// Functionality new in TS4, similar here: subset of ViewManager.
// Significant additions: Create menus etc, hiding stuff in Cocoa
// that were in Carbon. Partially similar to Carbon.

// This is not a straight plug-in rewrite, but similar enough to make
// porting fairly easy.

// Some parts in external units:
// SkelViewUnit. Similar to SkelView for Carbon. (Vital component.)
// NSStandardFile: File dialogs and utilities.
// SkelSheetAlertUnit: Easy sheet alerts.
// QDCG: QuickDraw through CG. Graphics with an easier API.
// SkelDialog: Classic text logging unit.

// TransSkel 5.0a1 First alpha.
// TransSkel 5.0a2 Clobber support. Menu searching functions. QDCG. SkelTextView.
// TransSkel 5.0a3 More NSStrings, fewer CFStrings, less memory leaks.
// TransSkel 5.0a4 Window closing and quitting rewritten and expanded.
// TransSkel 5.0a5 SkelNewScrollView, SkelModalDialog, SkelGetUserData.
// TransSkel 5.0a6 SkelAppendSubmenu, SkelBuildLongAppMenu, SkelHome
// TransSkel 5.0a7 SkelNewSlider works. SkelNewButton modified, works
// better with SkelSetBooleanViewHandler. Mouse enter and exit events now
// supported by SkelViews - but only for non-dragged. (You get a mouse exit
// event only after releasing the mouse. Is this good?)
// TransSkel 5.0a8: Added SkelWindowHasBeenAskedToClose. StupidEditWithSheet now
// has sheet-based save dialogs. Merged in SkelSheetAlertUnit. Added SkelSetSliderRange.
// Added TransEdit and several demos.
// TransSkel 5.0b1: Changes in external units, new demos.
// Built-in background color of SkelViews revised, NSColor replaced
// by separate color channels and CG calls. (Faster and more robust.)
// SkelList added.
// TransSkel 5.0b2: Background process now takes an optional time parameter.
// Added SkelSeconds and SkelResetTime.
// TransSkel 5.0b4: setAppleMenu reluctantly replaced by tryToPerform_with
// TransSkel 5.0b5: Better SkelInit, checks for nib reference properly.
// Added controlTextDidChange for the TSMenuController for catching editing
// of NSTextFields. (setDelegate in each string handler.)
// TransSkel 5.0b6: SkelCheckMenuItem. SkelGetStringValue/SkelSetStringValue
// are now overloaded for NSControl and NSTextView.
// The call setAppleMenu removed (not needed).
// Crash problem with timers fixed (caused by using "init"!)
// TransSkel 5.0b7: Changes to QDCG, including multi-page PDF "Picture" support,
// and a workaround for the deprecated CGContextDrawPDFDocument.
// The itemAtIndex method no longer needed (which means that no editing of the
// default interfaces are needed any more). Bug fix in TryQuit, for a serious
// bug that caused crashes for multi-window applications.
// New names for InstallSkelView* calls, now SkelSetView*.
// Introduced corresponding window-level calls, calls straight into SkelView:
//procedure SkelSetWindowExtraMouseHandlers(theWind: NSWindow; altMouseProc, mouseUpProc, altMouseUpProc: SkelMouseProcPtr);
//procedure SkelSetWindowMouseMovementHandlers(theWind: NSWindow; mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc: SkelMouseProcPtr);
//procedure SkelSetWindowExtraEventHandlers(theWind: NSWindow; keyUpProc: SkelKeyProcPtr; scrollWheelProc: ProcPtr);
// TransSkel 5.0b8:
// Added SkelNewRadioGroup. Added overloads for all control creation calls (SkelNewButton etc) plus SkelNewWindow
// to allow passing an NSRect directly. With NSMakeRect the old syntax feel superfluous.
// TransSkel 5.0b9:
// QDCG is now made integral part of TransSkel. Draw handlers can be specified to be for QDCG, which makes them
// more automatic, less code in the appliction layer.
// 140623: New callback installers, one per callback! (Also i SkelView.)
// 140704: SkelBringToFront should now work better.
// 140712: Added Stepper control.
// 140720: Added a timer for sheet replies, to avoid problems when an alert precedes a file dialog.
// 140721: WindowDelegate.windowShouldClose now calls SkelCloseWindow, so all ways to close a window
// will be processed the same way. (Second irritating bug that ruined the editing demos. Good enough now?)
// 140807: Added SkelSetViewResizing and SkelCreateTabGroup.
// 141107: Added setActivationPolicy. I should have listened more closely to Marcus Stenbäck - he pointed it out for MicroGlut but I forgot adding it here!
// 150511: A bunch of SkelSetWindow calls corrected to SkelSetView.
// 151201: Added SkelRemoveView.
// 160526: SkelNewButton, SkelNewSlider, SkelNewTextView, SkelNewTextField, SkelNewStaticTextField...: variants with Rect for most controls.
// QDCG Rect should become standard and replace the others!
// 160527: Added a filter proc feature, allowing inspection and filtering of events. SkelGetCurrentEvent., SkelPopupMenu. SkelSetContextualMenu.

{$mode objfpc}
{$modeswitch objectivec1}

unit TransSkel5;
interface

uses
	ctypes, MacOSAll, CocoaAll, SkelViewUnit, SysUtils, BaseUnix, Unix, QDCG;

type
	TSStrProcPtr = procedure(s: PChar);
	TSNoArgProcPtr = procedure();
	TSRealProcPtr = procedure(r: Real);
	TSDoubleProcPtr = procedure(r: Double);
	ExtViewProcPtr = procedure(sender: id; data: Pointer);
	VMPointerProcPtr = ExtViewProcPtr;
	StringArr = array of AnsiString;
	TSAEFileProcPtr = procedure(args: StringArr);
	TSActivateWindowProc = procedure(active: Boolean);

// Menus
function SkelNewMenu(title: shortstring): NSMenu;
function SkelAppendMenuItem(theMenu: NSMenu; ATitle: shortstring;
	ACallbackName: AnsiString; theKey: ShortString; callback: TSStrProcPtr): NSMenuItem;
//procedure SkelAppendMenuItem(theMenu: NSMenu; ATitle: shortstring;
//	ACallbackName: AnsiString; theKey: ShortString; callback: TSStrProcPtr);
procedure SkelSetMenuItemAction(Item1: NSMenuItem; ACallbackName: AnsiString; callback: TSStrProcPtr);
procedure SkelCreateMenuBar();
procedure SkelAddToMenubar(menu: NSMenu);
procedure SkelBuildAppMenu(myMenu: NSMenu);
procedure SkelBuildLongAppMenu(myMenu: NSMenu);
procedure SkelAppendSubmenu(toMenu: NSMenu; title: AnsiString; subMenu: NSMenu);
procedure SkelPopupMenu(menu: NSMenu; view: NSView; where: Point);
procedure SkelSetContextualMenu(menu: NSMenu; view: NSView);
procedure SkelApple(aboutText: AnsiString; aboutProc: TSStrProcPtr);
// See also menu access below

// Creation of buttons and some other controls (incomplete but easy to expand)
// SkelCreateButton renamed to SkelNewButton for symmetry
function SkelNewButton(parentView: NSView; ATitle: shortstring; buttonRect: NSRect;
	aCallbackName: string; callback: TSStrProcPtr): NSButton; overload;
function SkelNewButton(parentView: NSView; ATitle: shortstring;
	AX, AY, AWidth, AHeight: Double;
	ACallbackName: string; callback: TSStrProcPtr): NSButton; overload;
function SkelNewButton(parentView: NSView; ATitle: shortstring;
	r: Rect; ACallbackName: string; callback: TSStrProcPtr): NSButton; overload;

const
	kSkelButton = 0;
	kSkelCheckBox = 1;
	kSkelRadioButton = 2;
function SkelNewButton(parentView: NSView; ATitle: shortstring;
	r: Rect; buttonKind: Integer): NSButton; overload;

procedure SkelSetButtonTitle(bt: NSButton; ATitle: shortstring);
function SkelNewTextField(parentView: NSView; textFieldRect: NSRect; initialText: AnsiString): NSTextField;overload;
function SkelNewTextField(parentView: NSView; x,y,w,h: Real; initialText: AnsiString): NSTextField;overload;
function SkelNewTextField(parentView: NSView; r: Rect; initialText: AnsiString): NSTextField;overload;
function SkelNewStaticTextField(parentView: NSView; textFieldRect: NSRect; initialText: AnsiString): NSTextField; overload;
function SkelNewStaticTextField(parentView: NSView; x,y,w,h: Real; initialText: AnsiString): NSTextField; overload;
function SkelNewStaticTextField(parentView: NSView; r: Rect; initialText: AnsiString): NSTextField; overload;
function SkelNewTextView(parentView: NSView; scrollViewRect: NSRect; initialText: AnsiString): NSTextView; overload;
function SkelNewTextView(parentView: NSView; x,y,w,h: Real; initialText: AnsiString): NSTextView; overload;
function SkelNewTextView(parentView: NSView; r: Rect; initialText: AnsiString): NSTextView; overload;
function SkelNewSlider(parentView: NSView; sliderRect: NSRect): NSSlider;
function SkelNewSlider(parentView: NSView; x,y,w,h: Real): NSSlider;
function SkelNewSlider(parentView: NSView; r: Rect): NSSlider;
procedure SkelSetSliderRange(slider: NSSlider; min, max: Real);
function SkelNewScrollView(parentView: NSView; scrollViewRect: NSRect; scrolledView: NSView): NSScrollView;overload;
function SkelNewScrollView(parentView: NSView; x,y,w,h: Real; scrolledView: NSView): NSScrollView; overload;
function SkelNewScrollView(parentView: NSView; r: Rect; scrolledView: NSView): NSScrollView; overload;
function SkelNewRadioGroup(parent: NSView; matrixRect: NSRect; items: array of AnsiString): NSMatrix; overload;
function SkelNewRadioGroup(parent: NSView; x,y,w,h: Real; items: array of AnsiString): NSMatrix; overload;
function SkelNewRadioGroup(parent: NSView; r: Rect; items: array of AnsiString): NSMatrix; overload;
function SkelNewStepper(parentView: NSView; stepperRect: NSRect): NSStepper;
function SkelNewStepper(parentView: NSView; x,y,w,h: Real): NSStepper;
function SkelNewStepper(parentView: NSView; r: Rect): NSStepper;
procedure SkelSetStepperRange(stepper: NSStepper; min, max: Real);

function SkelCreateTabGroup(parent: NSView; titles: array of AnsiString; views: array of SkelView): NSTabView;

// Why did I comment this out? I don't remember. Useful for views in nibs, right?
// Replaced by SkelSetViewAction? No, a trivial version without callback name!
procedure SkelSetViewAction(theView: NSControl; aCallbackName: ansistring; callback: TSStrProcPtr);
procedure SkelSetButtonAction(button: NSControl; callback: TSStrProcPtr);

// Handlers for NSControls and other existing buttons. Handles 1-dimensional NSMatrix (radio button list) only.
//procedure AppendCommandCallback(theView: id; commandName: ShortString; callback: TSStrProcPtr);
//procedure AppendCommandCallback(theView: id; commandName: ShortString; callback: TSStrProcPtr;
//			dataPtr, variablePtr: Pointer; variableType: Integer; extCallback: ExtViewProcPtr);
{SkelSet*Handler calls for standard controls.}
procedure SkelSetNumViewHandler(theView: NSControl;
		var theViewData: Longint {NSInteger?}; callback: ExtViewProcPtr);
procedure SkelSetRealViewHandler(theView: NSControl;
		var theViewData: Real; callback: ExtViewProcPtr);
procedure SkelSetBooleanViewHandler(theView: NSControl;
		var theViewData: Boolean; callback: ExtViewProcPtr);
procedure SkelSetColorViewHandler(theView: NSColorWell;
		var theViewData: NSColor; callback: ExtViewProcPtr);
// Text boxes
procedure SkelSetStringViewHandler(theView: NSControl;
		var theViewData: AnsiString; callback: ExtViewProcPtr);
procedure SkelSetShortStringViewHandler(theView: NSControl;
		var theViewData: String; callback: ExtViewProcPtr);
// Text boxes for writing numbers in
procedure SkelSetNumStringViewHandler(theView: NSControl;
		var theViewData: Longint; callback: ExtViewProcPtr);
procedure SkelSetRealStringViewHandler(theView: NSControl;
		var theViewData: Real; callback: ExtViewProcPtr);
{Install*ViewHandler old names without Skel prefix}
procedure InstallNumViewHandler(theView: NSControl;
		var theViewData: Longint {NSInteger?}; callback: ExtViewProcPtr);
procedure InstallRealViewHandler(theView: NSControl;
		var theViewData: Real; callback: ExtViewProcPtr);
procedure InstallBooleanViewHandler(theView: NSControl;
		var theViewData: Boolean; callback: ExtViewProcPtr);

// Windows
function SkelNewWindow(theWindowRect: NSRect; title: String; front: Boolean): NSWindow;overload;
function SkelNewWindow(x, y, w, h: Real; title: String; front: Boolean): NSWindow; overload;
function SkelNewWindow(r: Rect; title: String; front: Boolean): NSWindow; overload; // New 160503
procedure SkelSetWindowTitle(w: NSWindow; title: String);
function SkelGetWindowTitle(w: NSWindow): AnsiString;
function SkelFrontWindow: NSWindow;
function SkelKeyWindow: NSWindow;
procedure SkelBringToFront(w: NSWindow);
procedure SkelShowWindow(theWindow: NSWindow); // Added 100621
procedure SkelSetFocus(w: NSWindow); overload;
procedure SkelSetFocus(view: NSView); overload;
procedure SkelCloseWindow(theWindow: NSWindow; conditionally: Boolean);
procedure SkelCancelCloseWindow(theWindow: NSWindow);
function SkelWindowHasBeenAskedToClose(theWindow: NSWindow): Boolean;
procedure SkelDisposeWindow(theWindow: NSWindow);
function SkelGetUserData(window: NSWindow): Pointer;
// See also SkelGetNewWindow below.

// Install handlers for the content view of a window.
// OBSOLETE - see below
procedure SkelSetWindowHandler(theWind: NSWindow;
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr;
		theCloseProc: SkelCloseWindowProc;
		theClobberProc: SkelClobberWindowProc; userData: Pointer); overload;
procedure SkelSetWindowHandler(theWind: NSWindow;
		theMouseProc: SkelQDCGMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelQDCGDrawProcPtr;
		theCloseProc: SkelCloseWindowProc;
		theClobberProc: SkelClobberWindowProc; userData: Pointer); overload;
procedure SkelWindow(theWind: NSWindow; // Old name of above call - avoid
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr;
		theCloseProc: SkelCloseWindowProc;
		theClobberProc: SkelClobberWindowProc; userData: Pointer);
procedure SkelSetWindowActivateProc(theWindow: NSWindow; proc: TSActivateWindowProc);
// The following should be split into one call per handler!
// OBSOLETE - see below
procedure SkelSetWindowExtraMouseHandlers(theWind: NSWindow; altMouseProc, mouseUpProc, altMouseUpProc: SkelMouseProcPtr);
procedure SkelSetWindowMouseMovementHandlers(theWind: NSWindow; mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc: SkelMouseProcPtr);
procedure SkelSetWindowExtraEventHandlers(theWind: NSWindow; keyUpProc: SkelKeyProcPtr; scrollWheelProc: ProcPtr);

// New callback installers, one per callback!
// This is a lot better than the ones above!
procedure SkelSetWindowMouseHandler(theWind: NSWindow; theMouseProc: SkelMouseProcPtr);overload;
procedure SkelSetWindowDrawHandler(theWind: NSWindow; theDrawProc: SkelQDCGDrawProcPtr);overload;
procedure SkelSetWindowDrawHandler(theWind: NSWindow; theDrawProc: SkelDrawProcPtr);overload;
procedure SkelSetWindowKeyDownHandler(theWind: NSWindow;theKeyProc: SkelKeyProcPtr);
procedure SkelSetWindowUserData(theWind: NSWindow; userData: Pointer);
procedure SkelSetWindowRightMouseHandler(theWind: NSWindow; altMouseProc: SkelMouseProcPtr);overload;
procedure SkelSetWindowMouseUpHandler(theWind: NSWindow; mouseUpProc: SkelMouseProcPtr);overload;
procedure SkelSetWindowRightMouseUpHandler(theWind: NSWindow; altMouseUpProc: SkelMouseProcPtr);overload;
procedure SkelSetWindowMouseEnterExitHandlers(theWind: NSWindow; mouseEnterProc, mouseExitProc: SkelMouseProcPtr);overload;
procedure SkelSetWindowMouseMovedHandler(theWind: NSWindow; mouseMovedProc: SkelMouseProcPtr);overload;
procedure SkelSetWindowMouseDraggedHandler(theWind: NSWindow; mouseDraggedProc: SkelMouseProcPtr);overload;
procedure SkelSetWindowKeyUpHandler(theWind: NSWindow; keyUpProc: SkelKeyProcPtr);
procedure SkelSetWindowScrollWheelHandler(theWind: NSWindow; scrollWheelProc: ProcPtr);
// And some more for QDCG. Preferred!
procedure SkelSetWindowMouseHandler(theWind: NSWindow; theMouseProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetWindowRightMouseHandler(theWind: NSWindow; altMouseProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetWindowMouseUpHandler(theWind: NSWindow; mouseUpProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetWindowRightMouseUpHandler(theWind: NSWindow; altMouseUpProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetWindowMouseEnterExitHandlers(theWind: NSWindow; mouseEnterProc, mouseExitProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetWindowMouseMovedHandler(theWind: NSWindow; mouseMovedProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetWindowMouseDraggedHandler(theWind: NSWindow; mouseDraggedProc: SkelQDCGMouseProcPtr);overload;




// Pictures
// Limited - replace by QDCG?
procedure SkelDrawPicturePart(img: NSImage; partOfPicture: NSRect; dest: NSRect);// override;
procedure SkelDrawPicture(img: NSImage; dest: NSRect); overload;
procedure SkelDrawPicture(img: NSImage; x,y,w,h: Real); overload;
procedure SkelDrawPicture(img: NSImage; x,y: Real); overload;
function SkelGetPicture(fileName: AnsiString): NSImage; // Assumes 3-letter extensions! Like ".png".


// Timer
procedure SkelBackgroundWithDuration(p: TSDoubleProcPtr; intervalTime: Double); overload;
procedure SkelBackgroundWithDuration(p: TSNoArgProcPtr; intervalTime: Double); overload;
procedure SkelBackground(p: TSDoubleProcPtr); overload;
procedure SkelBackground(p: TSNoArgProcPtr); overload;
//procedure SkelBackgroundWithDuration (p: TSDoubleProcPtr; intervalTime: Double);
//procedure SkelBackground (p: TSDoubleProcPtr);

// Overall control
procedure SkelInit;
procedure SkelMain;
procedure SkelClobber;
procedure SkelWhoa;
procedure SkelAppleEvent (openAppProc: TSNoArgProcPtr; openDocProc, printProc: TSAEFileProcPtr{; quitProc: TSNoArgProcPtr});
procedure SkelSetInternalOpenApp(openAppProc: TSNoArgProcPtr); // Internal use
procedure SkelHome; // Set directory to Resources

// View manager
function SkelGetStringValue(view: NSControl): AnsiString; overload;
function SkelGetStringValue(view: NSTextView): AnsiString; overload;
procedure SkelSetStringValue(view: NSControl; value: AnsiString); overload;
procedure SkelSetStringValue(view: NSTextView; value: AnsiString); overload;
function SkelGetNumValue(view: NSControl): Longint; overload;
procedure SkelSetNumValue(view: NSControl; value: Longint); overload;
function SkelGetNumValue(view: NSPopUpButton): Longint; overload;
procedure SkelSetNumValue(view: NSPopUpButton; value: Longint); overload;
function SkelGetRealValue(view: NSControl): Real; overload;
procedure SkelSetRealValue(view: NSControl; value: Real); overload;
function SkelGetNumStringValue(view: NSControl): Longint; overload;
procedure SkelSetNumStringValue(view: NSControl; value: Longint); overload;
// Old names of the functions above - avoid
function VMGetStringValue(view: NSControl): AnsiString;
procedure VMSetStringValue(view: NSControl; value: AnsiString);
function VMGetNumValue(view: NSControl): Longint; overload;
procedure VMSetNumValue(view: NSControl; value: Longint); overload;
function VMGetRealValue(view: NSControl): Real; overload;
procedure VMSetRealValue(view: NSControl; value: Real); overload;
function VMGetNumStringValue(view: NSControl): Longint; overload;
procedure VMSetNumStringValue(view: NSControl; value: Longint); overload;

// String conversion utilities
function CFStringToString(input: CFStringRef): AnsiString;
function CFStringToMacRomanString(input: CFStringRef): AnsiString;
function StringToCFString(input: AnsiString): CFStringRef;
function StringToNSString(input: AnsiString): NSString;
function NSStringToString(input: NSString): AnsiString;
function MacRomanStringToCFString(input: AnsiString): CFStringRef;
function UTF(s: AnsiString): AnsiString;

// Using nib files, creating windows and accessing views
function SkelGetNewWindow(nibName: String): NSWindow;
function SkelGetIndView(w: NSWindow; viewClass: String; index: Longint): NSView;
function SkelCountViews(w: NSWindow; viewClass: String): Longint;
function GetWindowContentView(w: NSWindow): NSView; // Replaced by SkelGetWindowContentView
function GetViewFrame(view: NSView): NSRect; // Replaced by SkelGetViewFrame
function SkelGetWindowContentView(w: NSWindow): NSView;
function SkelGetViewFrame(view: NSView): NSRect;
function SkelGetViewFromTag(w: NSWindow; tag: NSInteger): NSView;

// TransSkel menu access functions
function SkelSearchMenuItem(wantedMenuName, wantedItemName: AnsiString; itemIndex: Longint; doMacRoman, partialName: Boolean): NSMenuItem;
function SkelGetMenuItemByUTFName(itemName: AnsiString): NSMenuItem;
function SkelGetMenuItemByName(itemName: AnsiString): NSMenuItem;
function SkelGetMenuByName(menuName: AnsiString): NSMenuItem;
function SkelGetMenuItemByPartialName(itemName: AnsiString): NSMenuItem;
function SkelGetMenuItemByMenuNameAndIndex(menuName: AnsiString; itemIndex: Longint): NSMenuItem;
function SkelGetMenuItemByMenuNameAndItemName(menuName, itemName: AnsiString): NSMenuItem;

// Menu enabling
procedure EnableMenuItem( theMenu: NSMenu; index: Longint ); overload;
procedure EnableMenuItem( theMenu: NSMenu; title: AnsiString ); overload;
procedure EnableMenuItem( theMenuItem: NSMenuItem ); overload;
procedure DisableMenuItem( theMenu: NSMenu; index: Longint ); overload;
procedure DisableMenuItem( theMenu: NSMenu; title: AnsiString ); overload;
procedure DisableMenuItem( theMenuItem: NSMenuItem ); overload;
procedure EnableMenuItem( theMenu: NSMenu; index: Longint; enabled: Boolean); overload;
procedure EnableMenuItem( theMenu: NSMenu; title: AnsiString; enabled: Boolean); overload;
procedure EnableMenuItem( theMenuItem: NSMenuItem; enabled: Boolean); overload;

procedure SkelCheckMenuItem(theMenu: NSMenu; title: AnsiString; checked: Boolean); overload;
procedure SkelCheckMenuItem(theMenuItem: NSMenuItem; checked: Boolean); overload;
procedure SkelCheckMenuItem(theMenu: NSMenu; index: Longint; checked: Boolean); overload;


const
	kViewDataNone = -1; // No variable should be updated (same as pointer=nil)
	kViewDataString = 0;
	kViewDataLongint = 1;
	kViewDataBoolean = 2;
	kViewDataNumString = 3;
	kViewDataShortString = 4;
	kViewDataReal = 5;
	kViewDataColor = 6; // Controls returning an NSColor
	kViewDataRealString = 7;

type
	SkelApplication = objcclass(NSApplication)
	public
	    procedure terminate(sender: id); message 'terminate:'; override;
//		function init: id; message 'init'; override;
	end;

var
	NSApp: SkelApplication;



// ---------------- Alerts -----------------
// Modal alert and modal dialog
function SkelAlert(msg, okMsg, cancelMgs, altMsg, infoMsg: String): Integer;
procedure SkelModalDialog(w: NSWindow);

// Sheet alerts, formerly SkelSheetAlertUnit
type
	SkelSheetProc = procedure(returnCode: NSInteger; userData: Pointer);
	SkelSheetRec = record
		sheetProc: SkelSheetProc;
		userData: Pointer;
		returnCode: Longint;
	end;
	SkelSheetPtr = ^SkelSheetRec;

// Main call, with callback and up to three buttons.
procedure SkelSheetAlert(message, defaultButton, thirdButton, otherButton, addText: AnsiString;
		window: NSWindow; callbackProc: SkelSheetProc; userData: Pointer); overload;
// Simplified call for warnings/notifications, only one button and no callback
procedure SkelSheetAlert(message, defaultButton, addText: AnsiString; window: NSWindow); overload;

function SkelSeconds(): Double;
function SkelTickCount(): Double; // For compatibility with old TickCount
procedure SkelResetTime();

procedure SkelSetViewResizing(theView: NSView; minx, width, maxx, miny, height, maxy, subviews: Boolean);
procedure SkelRemoveView(view: NSView; removeSubViews: Boolean);


// NEW 160527: Event filter proc support!
// This makes it possible to inspect and event intercept an event
type
	EventDataRec = record
		what: Integer; // mouseDown, keyDown, updateEvt
		modifiers: Longint;
		message: Longint; // Key...
		keyCode: Integer;
		key: Char;
		where: Point;
		isRepeat: Boolean;
		window: NSWindow;
		delta: Point;
	end;
	EventFilterProc = function(event: EventDataRec): Boolean;
const
	kSkelEventUnknown = -1;
	kSkelEventMouseDown = 0;
	kSkelEventMouseUp = 1;
	kSkelEventLeftMouseDown = 0;
	kSkelEventLeftMouseUp = 1;
	kSkelEventRightMouseDown = 2;
	kSkelEventRightMouseUp = 3;
	kSkelEventKeyDown = 4;
	kSkelEventKeyUp = 5;
	kSkelScrollWheel = 6;

procedure SkelEventHook(proc: EventFilterProc);
function SkelGetEventHook: EventFilterProc;
function SkelGetCurrentEvent: NSEvent;
// End filter proc

implementation

// Filter proc
var
	gEventFilter: EventFilterProc;

function CFStringToString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	result := output;
end;

function NSStringToString(input: NSString): AnsiString;
begin
	result := CFStringToString(CFStringRef(input));
end;

function CF2String(theString: CFStringRef): AnsiString;
var
	charNum: Longint;
//	s: String;
//	str: AnsiString;
begin
	charNum := CFStringGetLength(theString);
	SetLength(result, charNum+1);
	CFStringGetCString (theString, @result[1], charNum+1, kCFStringEncodingUTF8);
end;

function CFStringToMacRomanString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	result := output;
end;

function StringToCFString(input: AnsiString): CFStringRef;
begin
	result :=  CFStringCreateWithCString(nil, @input[1], kCFStringEncodingUTF8);
	// Must be released
end;

function StringToNSString(input: AnsiString): NSString;
begin
	result := NSString.stringWithUTF8String(PChar(input));
	// Autoreleased
end;

function MacRomanStringToCFString(input: AnsiString): CFStringRef;
begin
	result :=  CFStringCreateWithCString(nil, @input[1], kCFStringEncodingMacRoman);
	// Must be released
end;

function UTF(s: AnsiString): AnsiString;
var
	cfs: CFStringRef;
begin
	cfs := MacRomanStringToCFString(s);
	UTF := CFStringToString(cfs);
	CFRelease(cfs);
end;

var
	gMainMenu: NSMenu;
	pool: NSAutoreleasePool;

// ------------------------ Apple event support -----------------------

type AppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
	public
//	function openFiles(filenames: NSArray): Boolean; message 'openFiles:';
	function application_openFiles(app: NSApplication; filenames: NSArray): Boolean; message 'application:openFiles:';
	function printFiles_withSettings_showPrintPanels(filenames: NSArray; settings: NSDictionary; showPrintPanels: Boolean): NSApplicationPrintReply; message 'printFiles:withSettings:showPrintPanels:';
	procedure applicationDidFinishLaunching(notification: NSNotification); message 'applicationDidFinishLaunching:';
  end;

var
	gOpenAppProc, gInternalOpenAppProc: TSNoArgProcPtr;
	gOpenFilesProc: TSAEFileProcPtr;
	gPrintFilesProc: TSAEFileProcPtr;
	gAppDelegate: AppDelegate;

// Works!
function AppDelegate.application_openFiles(app: NSApplication; filenames: NSArray): Boolean;
var
	i: Longint;
	args: StringArr;
begin
	if gOpenFilesProc <> nil then
	begin
		if filenames.count > 0 then
		begin
			// Debug
			WriteLn(filenames.count);
			for i := 0 to filenames.count-1 do
				WriteLn(NSStringToString(NSString(filenames.objectAtIndex(i))));
		
			SetLength(args, filenames.count);
			for i := 0 to filenames.count-1 do
				args[i] := NSStringToString(NSString(filenames.objectAtIndex(i)));
			
			gOpenFilesProc(args);
		end;
		result := true; // Assume success
	end
	else
		result := false; // No proc - failed!
end;

// Hard to test!
function AppDelegate.printFiles_withSettings_showPrintPanels(filenames: NSArray; settings: NSDictionary; showPrintPanels: Boolean): NSApplicationPrintReply;
var
	i: Longint;
	args: StringArr;
begin
	if gPrintFilesProc <> nil then
	begin
		if filenames.count > 0 then
		begin
			WriteLn(filenames.count);
			for i := 0 to filenames.count-1 do
				WriteLn(NSStringToString(NSString(filenames.objectAtIndex(i))));
			
			SetLength(args, filenames.count);
			for i := 0 to filenames.count-1 do
				args[i] := NSStringToString(NSString(filenames.objectAtIndex(i)));
			
			gPrintFilesProc(args);
		end;
		result := NSPrintingSuccess; // Assume success
	end
	else
		result := NSPrintingFailure; // No proc - failed!
end;

procedure AppDelegate.applicationDidFinishLaunching(notification: NSNotification);
begin
	WriteLn('Finished launching');
	if gOpenAppProc <> nil then
		gOpenAppProc();
	if gInternalOpenAppProc <> nil then
		gInternalOpenAppProc();
end;

// Install apple event handlers
procedure SkelAppleEvent (openAppProc: TSNoArgProcPtr; openDocProc, printProc: TSAEFileProcPtr{; quitProc: TSNoArgProcPtr});
begin
	gOpenAppProc := openAppProc;
	gOpenFilesProc := openDocProc;
	gPrintFilesProc := printProc;
end;

// For use by TransEdit (to avoid conflicts with gOpenAppProc).
// Didn't help though. Might remove it?
procedure SkelSetInternalOpenApp(openAppProc: TSNoArgProcPtr);
begin
	gInternalOpenAppProc := openAppProc;
end;

// Mini-mini class for collecting events
type
	TSMenuController = objcclass(NSObject, NSTextFieldDelegateProtocol)
	public
		{ Objective-c Methods }
		procedure doMenuCommand(sender: id); message 'doMenuCommand:';
		procedure viewAction(sender: id); message 'viewAction:';
	    procedure controlTextDidChange(obj: NSNotification); message 'controlTextDidChange:';override;
	end;
type
	CommandDataRec = record
		name: ShortString;
		ids: id;
		callback: TSStrProcPtr;
		variablePointer: Pointer;
		customDataPointer: Pointer;
		dataType: Integer; {0 = none, 1 = Longint, 2 = Boolean, 3 = Real}
		extCallback: ExtViewProcPtr;
	end;
var
	gCommandController: TSMenuController;
	commandData: array of CommandDataRec;
//	commandNames: array of ShortString;
//	ids: array of id;
//	commandCallbacks: array of TSStrProcPtr;
//	commandVariablePointer: array of Pointer;
//	commandDataPointer: array of Pointer;
//	commandDataType: array of Integer; {0 = none, 1 = Longint, 2 = Boolean, 3 = Real}

procedure TSMenuController.doMenuCommand(sender: id);
var
	i: Longint;
//	title: AnsiString;
	cn: AnsiString;
begin
//	WriteLn('doMenuCommand');
	for i := Low(commandData) to High(commandData) do
		if commandData[i].ids = sender then // Finns säkrare test på likhet?
		begin
//			title := CF2String(CFStringRef(NSMenuItem(sender).title));
//			WriteLn(title);
//			commandCallbacks[i](PChar(title)); // Skicka item text
			cn := commandData[i].name;
			commandData[i].callback(PChar(cn)); // Skicka item text
			// Vore det inte vettigare att skicka sender?
		end;
end;

procedure TSMenuController.viewAction(sender: id);
var
	i, ii: Longint;
	cn: AnsiString;
	s: AnsiString;
	r: Real;
type
	LongPtr = ^Longint;
	BooleanPtr = ^Boolean;
	RealPtr = ^Real;
	StringPtr = ^AnsiString;
	ShortStringPtr = ^String;
	ColorPtr = ^NSColor;
begin
//	WriteLn('viewAction');
	
	for i := Low(commandData) to High(commandData) do
		if commandData[i].ids = sender then
		begin
// Special case: NSMatrix
// No "value" in the usual sense, but it has selected element(s)
// By only supporting one column (most common case) then it is easy
			
			if commandData[i].variablePointer <> nil then
			begin
//				value := NSControl(sender).???
				case commandData[i].dataType of {0 = none, 1 = Longint, 2 = Boolean, 3 = Real}
					kViewDataNone: ; // Ingen typ - ignoreras
					kViewDataLongint:
					begin
						cn := CFStringToString(CFStringRef(NSStringFromClass(NSView(sender).classForCoder)));
						if cn = 'NSMatrix' then
						begin
							// Kolla om den är en enkolumnare eller något annat?
							LongPtr(commandData[i].variablePointer)^ := NSControl(NSMatrix(sender).selectedCell).tag;
						end
						else
						if cn = 'NSPopUpButton' then
							LongPtr(commandData[i].variablePointer)^ := NSPopUpButton(sender).indexOfSelectedItem
						else
							LongPtr(commandData[i].variablePointer)^ := NSControl(sender).intValue;
					end;
					kViewDataBoolean: BooleanPtr(commandData[i].variablePointer)^ := Boolean(NSControl(sender).intValue);
					kViewDataReal: RealPtr(commandData[i].variablePointer)^ := NSControl(sender).floatValue;
					kViewDataColor: ColorPtr(commandData[i].variablePointer)^ := NSColorWell(sender).color;
					kViewDataString:
					begin
						s := CFStringToString(CFStringRef(NSControl(sender).stringValue));
						StringPtr(commandData[i].variablePointer)^ := s;
					end;
					kViewDataShortString:
					begin
						s := CFStringToString(CFStringRef(NSControl(sender).stringValue));
						ShortStringPtr(commandData[i].variablePointer)^ := s;
					end;
					kViewDataNumString:
					begin
						s := CFStringToString(CFStringRef(NSControl(sender).stringValue));
						// Test string for non-numerical?
						Val(s, ii);
						LongPtr(commandData[i].variablePointer)^ := ii;
					end;
					kViewDataRealString:
					begin
						s := CFStringToString(CFStringRef(NSControl(sender).stringValue));
//WriteLn(s);
						// Test string for non-numerical?
						Val(s, r);
						RealPtr(commandData[i].variablePointer)^ := r;
					end;
				otherwise
				end;
			end;
			
			if commandData[i].callback <> nil then
			begin
				cn := commandData[i].name;
				commandData[i].callback(PChar(cn)); // Skicka item text
			end;
			if commandData[i].extCallback <> nil then
				commandData[i].extCallback(sender, commandData[i].customDataPointer);
			// Vore det inte vettigare att skicka sender? Och dataPtr.
		end;
end;

// text change delegate method for text fields
procedure TSMenuController.controlTextDidChange(obj: NSNotification);
//var
//	theString: NSString;
//	s: AnsiString;
begin
// Kolla att det är NSControlTextDidChangeNotification?
// Vad är obj för typ?
//	s := NSStringToString(NSStringFromClass(NSView(obj.object_).classForCoder));
//	WriteLn(s);
//	WriteLn(SkelGetStringValue(NSControl(obj.object_)));
//	theString := NSString(NSWindow(obj.object_));
//	s := NSStringToString(theString);

//	WriteLn(s);
	self.viewAction(id(obj.object_));
end;

// Counter for creating unique commands, "cmd1", "cmd2", "cmd12345"...
var
	gCommandNumber: Longint;
	
function GetAutoCommandName: ShortString;
var
	s: ShortString;
begin
	gCommandNumber += 1;
	NumToString(gCommandNumber, s);
//	Str(gCommandNumber, s);
	result := 'cmd' + s;
end;

// Main call to install a callback for a control.
procedure AppendCommandCallbackExt(theView: id; commandName: ShortString; callback: TSStrProcPtr;
			dataPtr, variablePtr: Pointer; variableType: Integer; extCallback: ExtViewProcPtr);overload;
var
	i: Longint;
//	obj: pobjc_class;
//	nss: NSString;
begin
//	WriteLn('Installing command "', commandName, '"');
	
	if theView = nil then
	begin
		WriteLn(commandName, ' with nil control');
		Exit;
	end;
	
	// Already in list?
	for i := Low(commandData) to High(commandData) do
		if commandData[i].ids = theView then
		begin
			if @callback <> @commandData[i].callback then
			begin
				WriteLn('Warning! Double command with different callbacks! Ignored.');
				// Men detta är ju fel! Lägg in den på samma plats i stället!
				// Eller tillåt dubbla callbacks!
				
//				WriteLn(Longint(theView));
//				obj := NSObject(theView).classForCoder;
//				nss := NSStringFromClass(obj);
//				WriteLn(CFStringToString(CFStringRef(NSStringFromClass(NSObject(theView).classForCoder))));
//				WriteLn(CFStringToString(CFStringRef(NSStringFromClass(NSObject(theView).classForCoder))));
//				WriteLn(CFStringToString(CFStringRef(NSStringFromClass(NSObject(commandData[i].ids).classForCoder))));

			end;
			Exit; // Big dialect problem here
		end;
	
	// If not, add it
	SetLength(commandData, Length(commandData)+1);
	i := High(commandData);
//	WriteLn('High(commandNames) = ', High(commandNames));
	commandData[i].ids := theView;
	commandData[i].name := commandName;
	commandData[i].callback := callback;
	commandData[i].variablePointer := variablePtr;
	commandData[i].customDataPointer := dataPtr;
	commandData[i].dataType := variableType;
	commandData[i].extCallback := extCallback;
end;


procedure AppendCommandCallback(theView: id; commandName: ShortString; callback: TSStrProcPtr);overload;
begin
//	WriteLn('Ready to install command "', commandName, '"');
	AppendCommandCallbackExt(theView, commandName, callback, nil, nil, 0, nil);
end;

// Now called SkelSetNumViewHandler
procedure InstallNumViewHandler(theView: NSControl {NSControl?};
		var theViewData: Longint {NSInteger?}; callback: ExtViewProcPtr);
begin
	SkelSetNumViewHandler(theView, theViewData, callback);
end;
// Now called SkelSetRealViewHandler
procedure InstallRealViewHandler(theView: NSControl {NSControl?};
		var theViewData: Real; callback: ExtViewProcPtr);
begin
	SkelSetRealViewHandler(theView, theViewData, callback);
end;
// Now called SkelSetBooleanViewHandler
procedure InstallBooleanViewHandler(theView: NSControl {NSControl?};
		var theViewData: Boolean; callback: ExtViewProcPtr);
begin
	SkelSetBooleanViewHandler(theView, theViewData, callback);
end;


procedure SkelSetNumViewHandler(theView: NSControl {NSControl?};
		var theViewData: Longint {NSInteger?}; callback: ExtViewProcPtr);
var
	cn: AnsiString;
	cells: NSArray;
	i: Longint;
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
	
	// Set the value to the current value
	cn := CFStringToString(CFStringRef(NSStringFromClass(NSView(theView).classForCoder)));
	if cn = 'NSMatrix' then
	begin
		// Assuming that it is a column, set tags to their row
		cells := NSMatrix(theView).cells;
		for i := 0 to cells.count-1 do
		begin
//			WriteLn(NSControl(cells.objectAtIndex(i)).tag);
			NSControl(cells.objectAtIndex(i)).setTag(i+1);
//			WriteLn(NSControl(cells.objectAtIndex(i)).tag);
		end;
		
		NSMatrix(theView).selectCellAtRow_column(theViewData, 0);
	end
	else
	if cn = 'NSPopUpButton' then
		NSPopUpButton(theView).selectItemAtIndex(theViewData)
	else
	begin
		theView.setIntValue(theViewData);
// TEST
//		NSTextField(theView).setDelegate(NSTextFieldDelegateProtocol(gCommandController));
	end;
	
	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataLongint, callBack);
end;

procedure SkelSetRealViewHandler(theView: NSControl {NSControl?};
		var theViewData: Real; callback: ExtViewProcPtr);
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
// TEST
//	NSTextField(theView).setDelegate(NSTextFieldDelegateProtocol(gCommandController));

	// Set the value to the current value
	theView.setFloatValue(theViewData);

	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataReal, callBack);
end;

procedure SkelSetBooleanViewHandler(theView: NSControl {NSControl?};
		var theViewData: Boolean; callback: ExtViewProcPtr);
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?

	// Set the value to the current value
	theView.setIntValue(Ord(theViewData));

	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataBoolean, callBack);	
end;

procedure SkelSetColorViewHandler(theView: NSColorWell; {NSColorWell}
		var theViewData: NSColor; callback: ExtViewProcPtr);
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?

	// Set the value to the current value
	theView.setColor(theViewData);
	
	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataColor, callBack);
end;


// NOTE: theViewData is expected to be encoded in UTF-8!
// If it is assigned a MacRoman string, it may fail.
procedure SkelSetStringViewHandler(theView: NSControl;
		var theViewData: AnsiString; callback: ExtViewProcPtr);
type
	SP = ^AnsiString;
var
	cfs: NSString;
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
// TEST
	NSTextField(theView).setDelegate(gCommandController);
	
	cfs := StringToNSString(theViewData);
	if cfs <> nil then	
	begin
		NSTextField(theView).setStringValue(cfs);
	end
	else
		WriteLn('Bad string to SkelSetStringViewHandler, could not enter in view');
	
	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataString, callBack);
end;

// Same for String
procedure SkelSetShortStringViewHandler(theView: NSControl;
		var theViewData: String; callback: ExtViewProcPtr);
var
	cfs: NSString;
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
// TEST
	NSTextField(theView).setDelegate(gCommandController);
	
	// Set the value to the current value
	// This may depend on the kind of control!

	cfs := StringToNSString(theViewData);
	if cfs <> nil then	
	begin
		NSTextField(theView).setStringValue(cfs);
	end
	else
		WriteLn('Bad string to SkelSetStringViewHandler, could not enter in view');
	
	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataShortString, callBack);
end;


procedure SkelSetNumStringViewHandler(theView: NSControl;
		var theViewData: Longint; callback: ExtViewProcPtr);
var
	cfs: NSString;
	s: String;
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
// TEST
	NSTextField(theView).setDelegate(gCommandController);
	
	// Set the value to the current value
	// This may depend on the kind of control!
	Str(theViewData, s);
	cfs := StringToNSString(s);
	if cfs <> nil then	
	begin
		NSTextField(theView).setStringValue(cfs);
	end;
	
	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataNumString, callBack);
end;

procedure SkelSetRealStringViewHandler(theView: NSControl;
		var theViewData: Real; callback: ExtViewProcPtr);
//var
//	cfs: NSString;
//	s: String;
begin
	theView.setTarget(gCommandController);
	theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
// TEST
	NSTextField(theView).setDelegate(gCommandController);
	
	// Set the value to the current value
	// This may depend on the kind of control!
//	Str(theViewData, s);
//	cfs := StringToNSString(s);
//	if cfs <> nil then	
//	begin
//		NSTextField(theView).setStringValue(cfs);
//	end;
	SkelSetRealValue(NSControl(theView), theViewData);
// SkelSetRealValue gives nicer reals than Str's default, but Str
// with some formatting would be even better.
	AppendCommandCallbackExt(theView, GetAutoCommandName, nil, nil, @theViewData, kViewDataRealString, callBack);
end;


function SkelNewMenu(title: shortstring): NSMenu;
var
//	Item1: NSMenuItem;
	menuTitle: NSString;
begin
//	MenuTitle := CFStringCreateWithPascalString(nil, title, kCFStringEncodingUTF8);
	menuTitle := StringToNSString(title);
	result := NSMenu.alloc;
	result.initWithTitle(menuTitle);
end;

function SkelAppendMenuItem(theMenu: NSMenu; ATitle: shortstring;
	ACallbackName: AnsiString; theKey: ShortString; callback: TSStrProcPtr): NSMenuItem;
var
	itemText: NSString;
	keyText: NSString;
	Item1: NSMenuItem;
	theMask: Longint;
	ch: Char;
//	index: NSInteger;
const
	NSAlphaShiftKeyMask = 1 shl	16;
	NSShiftKeyMask = 1 shl	17;
	NSControlKeyMask = 1 shl 18;
	NSAlternateKeyMask = 1	shl	19;
	NSCommandKeyMask = 1 shl 20;
	NSNumericPadKeyMask = 1	shl	21;
	NSHelpKeyMask = 1 shl 22;
	NSFunctionKeyMask = 1 shl 23;
	NSDeviceIndependentModifierFlagsMask = $ffff0000;
begin
	if Length(ACallbackName) = 0 then
		if @callback <> nil then // no command but callback - make a command!
			ACallbackName := GetAutoCommandName;
	
	// Special case: "-" means separator!
	if ATitle = '-' then
		if callback = nil then
		begin
			theMenu.addItem(NSMenuItem.separatorItem);
			Exit;
		end;
	
	theMask := NSCommandKeyMask;
	if Length(theKey) > 1 then theMask := 0;
	while Length(theKey) > 1 do
	begin
		ch := theKey[1];
//	WriteLn(theKey);
		theKey := Copy(theKey, 2, Length(theKey)-1);
		case ch of
			'^': 
				theMask := theMask or NSControlKeyMask;
			'%':
				theMask := theMask or NSCommandKeyMask;
			'/':
				theMask := theMask or NSAlternateKeyMask;
			'~':
				theMask := theMask or NSShiftKeyMask;
		otherwise
			theMask := theMask or NSCommandKeyMask;
		end;
	end;
//	WriteLn(theKey);
//	KeyText := CFStringCreateWithPascalString(nil, theKey, kCFStringEncodingUTF8);
//	ItemText := CFStringCreateWithPascalString(nil, ATitle, kCFStringEncodingUTF8);
	keyText := StringToNSString(theKey);
	if keyText = nil then
	begin
		keyText := NSString(MacRomanStringToCFString(theKey)); // Needs releasing
		keyText.autorelease;
	end;
	itemText := StringToNSString(ATitle);
	if itemText = nil then
	begin
		itemText := NSString(MacRomanStringToCFString(ATitle)); // Needs releasing
		itemText.autorelease;
	end;
	
//	WriteLn(' ItemText: ', IntToHex(Int64(ItemText), 8), ' ATitle: ', ATitle);
	
	Item1 := NSMenuItem.alloc;
	Item1.initWithTitle_action_keyEquivalent(itemText, nil, keyText);
// Hur får vi in alt, ctrl mm? Lägg på info på theKey?
	Item1.setKeyEquivalentModifierMask(theMask);
//	if gCommandController <> nil then
//	if theKey[] <> ':' then // eller ingen callback
	if callback <> nil then
	begin
		Item1.setTarget(gCommandController);
		
		AppendCommandCallback(Item1, ACallbackName, callback);
//		Item1.setTag(index);
		Item1.setAction(sel_registerName(PChar('doMenuCommand:')));
	end
	else
		Item1.setAction(sel_registerName(PChar(ACallbackName)));

// if not :
//	gCommandController.AddMethod(PChar(ACallbackName), 'v@:@', Pointer(gCommandController.doMenuCommand));
	theMenu.addItem(Item1);
	
	result := Item1;
end;

// Subset of SkelAppendMenuItem, intended for use with functions that get a menu item from nib.
// Should this functionality be removed from SkelAppendMenuItem?
procedure SkelSetMenuItemAction(Item1: NSMenuItem; ACallbackName: AnsiString; callback: TSStrProcPtr);
begin
	if Length(ACallbackName) = 0 then
		if @callback <> nil then // no command but callback - make a command!
			ACallbackName := GetAutoCommandName;
	
	if callback <> nil then
	begin
		Item1.setTarget(gCommandController);
		
		AppendCommandCallback(Item1, ACallbackName, callback);
//		Item1.setTag(index);
		Item1.setAction(sel_registerName(PChar('doMenuCommand:')));
	end
	else
		Item1.setAction(sel_registerName(PChar(ACallbackName)));
end;

procedure SkelCreateMenuBar();
begin
// Create main menu
	gMainMenu := NSMenu.alloc;
//	gMainMenu.initWithTitle(NSString(CFSTR('')));
	gMainMenu.initWithTitle(NSSTR(''));
	NSApp.setMainMenu(gMainMenu);
	
// Create apple menu - auto
end;

procedure SkelAddToMenubar(menu: NSMenu);
var
	dummyItem: NSMenuItem;
begin
	dummyItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(NSString(CFSTR('')), nil, NSString(CFSTR('')));
	dummyItem.setSubmenu(menu);
	gMainMenu.addItem(dummyItem);
//	dummyItem.Free;
end;

// Given the top parts of the app menu, add the rest and insert
procedure SkelBuildAppMenu(myMenu: NSMenu);
begin
	SkelAppendMenuItem(myMenu, 'Hide', 'hide:', 'h', nil);
	SkelAppendMenuItem(myMenu, 'Hide others', 'hideOtherApplications:', '%/h', nil);
	SkelAppendMenuItem(myMenu, 'Show all', 'unhideAllApplications:', '', nil);
	myMenu.addItem(NSMenuItem.separatorItem);
	SkelAppendMenuItem(myMenu, 'Quit', 'terminate:', 'q', nil);
	SkelAddToMenuBar(myMenu);

// It turned out setAppleMenu wasn't needed after all. First I
// thought it should be replaced by setMainMenu (which would be
// strange since it is used for another purpose) but now it seems
// like I can just ignore specifying an "apple" menu at all!

//	NSApp.setAppleMenu(myMenu); // You must both add it and make it "apple menu" (that is app menu).
//	NSApp.tryToPerform_with(objcselector('setAppleMenu:'), myMenu);
//	NSApp.setMainMenu(myMenu);

// MYSTERY: Sometimes setMainMenu works, sometimes not!
end;

procedure SkelAppendSubmenu(toMenu: NSMenu; title: AnsiString; subMenu: NSMenu);
var
	item: NSMenuItem;
	itemText: NSString;
begin
	itemText := StringToNSString(title);
	item := toMenu.addItemWithTitle_action_keyEquivalent(NSLocalizedString(itemText), nil, NSSTR(''));
//	item := SkelAppendMenuItem(toMenu, 'Services', '', '', nil);
	toMenu.setSubmenu_forItem(subMenu, item);
end;

procedure SkelPopupMenu(menu: NSMenu; view: NSView; where: Point);
var
	e: NSEvent;
begin
	e := SkelGetCurrentEvent;
//	e.location := PointToCGPoint(where);
	NSMenu.popUpContextMenu_withEvent_forView(menu, e, view);
end;

procedure SkelSetContextualMenu(menu: NSMenu; view: NSView);
begin
	view.setMenu(menu);
end;

procedure SkelBuildLongAppMenu(myMenu: NSMenu);
var
	serv: NSMenu;
begin
// Richard Ward has reported problems here, but I find none.
	serv := SkelNewMenu('Services');
	serv.autorelease;
	SkelAppendSubmenu(myMenu, 'Services', serv);
	NSApp.setServicesMenu(serv);
	
	SkelAppendMenuItem(myMenu, '-', '', '', nil); // Separator
	SkelAppendMenuItem(myMenu, 'Hide', 'hide:', 'h', nil);
	SkelAppendMenuItem(myMenu, 'Hide others', 'hideOtherApplications:', '%/h', nil);
	SkelAppendMenuItem(myMenu, 'Show all', 'unhideAllApplications:', '', nil);
	SkelAppendMenuItem(myMenu, '-', '', '', nil); // Separator
	SkelAppendMenuItem(myMenu, 'Quit', 'terminate:', 'q', nil);
	SkelAddToMenuBar(myMenu);
//	NSApp.setAppleMenu(myMenu); // You must both add it and make it "apple menu" (that is app menu).
	NSApp.tryToPerform_with(objcselector('setAppleMenu:'), myMenu);
end;

// For the simple case where you only want an About item in the app menu,
// this call gives you a simplified way to build it.
// Wasn't this called SkelAppleMenu before?
procedure SkelApple(aboutText: AnsiString; aboutProc: TSStrProcPtr);
var
	myMenu: NSMenu;
begin
	SkelCreateMenuBar();
	// Build app menu
	myMenu := SkelNewMenu('');
	SkelAppendMenuItem(myMenu, aboutText, 'aboutItem', '', aboutProc);
	SkelAppendMenuItem(myMenu, '-', '', '', nil); // Separator
//	SkelBuildAppMenu(myMenu); // Fix the rest
	SkelBuildLongAppMenu(myMenu);
end;

// -------------

// ------------------ Create controls -------------------

// Created a pushbutton NSButton
// Note: Use setButtonType to change type:
// NSSwitchButton = checkbox
// NSRadioButton = radio button (but you would rather have a group)

function SkelNewButton(parentView: NSView; ATitle: shortstring; buttonRect: NSRect;
	aCallbackName: string; callback: TSStrProcPtr): NSButton; overload;
var
	CFButtonText: NSString;
begin
	if Length(aCallbackName) = 0 then
		if callback <> nil then // no command but callback - make a command!
			aCallbackName := GetAutoCommandName;
	
	CFButtonText := StringToNSString(ATitle);
	Result := NSButton.alloc.initWithFrame(buttonRect);
	Result.setTitle(CFButtonText);
	Result.setBezelStyle(NSRoundedBezelStyle);
	
	if callback <> nil then // No callback, no action. (Change 100430)
		SkelSetViewAction(result, ACallbackName, callback);
		
	parentView.addSubview(Result);
end;

function SkelNewButton(parentView: NSView; ATitle: shortstring;
	AX, AY, AWidth, AHeight: Double;
	ACallbackName: string; callback: TSStrProcPtr): NSButton; overload;
begin
	Exit(SkelNewButton(parentView, ATitle, NSMakeRect(AX, AY, AWidth, AHeight), ACallbackName, callback));
end;

function SkelNewButton(parentView: NSView; ATitle: shortstring;
	r: Rect; ACallbackName: string; callback: TSStrProcPtr): NSButton; overload;
begin
	Exit(SkelNewButton(parentView, ATitle, RectToNSRect(r), ACallbackName, callback));
end;

// New SkelNewButton which can create several kinds!
// No callback; use SkelSetViewAction! SkelSetButtonAction?
// May replace all the above?
function SkelNewButton(parentView: NSView; ATitle: shortstring;
	r: Rect; buttonKind: Integer): NSButton; overload;
var
	b: NSButton;
begin
	b := SkelNewButton(parentView, ATitle, RectToNSRect(r), '', nil);
	case buttonKind of
		kSkelButton: b.setButtonType(NSMomentaryLightButton); // ???
		kSkelCheckBox: b.setButtonType(NSSwitchButton);
		kSkelRadioButton: b.setButtonType(NSRadioButton);
	end;
end;

procedure SkelSetButtonTitle(bt: NSButton; ATitle: shortstring);
var
	CFButtonText: NSString;
begin
	CFButtonText := StringToNSString(ATitle);
	bt.setTitle(CFButtonText);
end;

// Install action callback for any kind of NSControl.
procedure SkelSetViewAction(theView: NSControl; aCallbackName: ansistring; callback: TSStrProcPtr);
begin
	if callback <> nil then
	begin
		if Length(aCallbackName) = 0 then
			if callback <> nil then // no command but callback - make a command!
				aCallbackName := GetAutoCommandName;

		theView.setTarget(gCommandController);
		theView.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
		AppendCommandCallback(theView, aCallbackName, callback);
	end;
end;

procedure SkelSetButtonAction(button: NSControl; callback: TSStrProcPtr);
begin
	SkelSetViewAction(button, '', callback);
//	button.setTarget(gCommandController);
//	button.setAction(sel_registerName(PChar('viewAction:'))); // What method will handle it?
//	AppendCommandCallbackExt(button, GetAutoCommandName, callback, nil, nil, 0, nil);
end;

function SkelNewTextField(parentView: NSView; textFieldRect: NSRect; initialText: AnsiString): NSTextField;overload;
var
	CFMessage: NSString;
	TextField: NSTextField;
begin
	CFMessage := StringToNSString(initialText);
	TextField := NSTextField.alloc;
	TextField.initWithFrame(textFieldRect);
	TextField.setStringValue(CFMessage);
	parentView.addSubview(TextField);
	result := TextField;
end;
function SkelNewTextField(parentView: NSView; x,y,w,h: Real; initialText: AnsiString): NSTextField;overload;
begin
	Exit(SkelNewTextField(parentView, NSMakeRect(x,y,w,h), initialText));
end;
function SkelNewTextField(parentView: NSView; r: Rect; initialText: AnsiString): NSTextField;overload;
begin
	Exit(SkelNewTextField(parentView, RectToNSRect(r), initialText));
end;

function SkelNewStaticTextField(parentView: NSView; textFieldRect: NSRect; initialText: AnsiString): NSTextField; overload;
var
	statText: NSTextField;
begin
	statText := SkelNewTextField(parentView, textFieldRect, initialText);
	statText.setEditable(false);
	statText.setSelectable(false);
	result := statText;
end;
function SkelNewStaticTextField(parentView: NSView; x,y,w,h: Real; initialText: AnsiString): NSTextField; overload;
begin
	Exit(SkelNewStaticTextField(parentView, NSMakeRect(x,y,w,h), initialText));
end;
function SkelNewStaticTextField(parentView: NSView; r: Rect; initialText: AnsiString): NSTextField; overload;
begin
	Exit(SkelNewStaticTextField(parentView, RectToNSRect(r), initialText));
end;

// Builds a scrollable text view.
function SkelNewTextView(parentView: NSView; scrollViewRect: NSRect; initialText: AnsiString): NSTextView; overload;
var
	CFMessage: NSString;
	textViewRect: NSRect;
	textView: NSTextView;
	scrollView: NSScrollView;
begin
	CFMessage := StringToNSString(initialText);
	
	scrollView := NSScrollView.alloc;
	scrollView.initWithFrame(scrollViewRect);
	scrollView.setHasVerticalScroller(true);
	scrollView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
	scrollView.contentView.setAutoresizesSubviews(true);
	
	textViewRect.origin := NSMakePoint(0.0, 0.0);
	textViewRect.size := scrollView.contentSize;
	
	textView := NSTextView.alloc;
	textView.initWithFrame(textViewRect);
	textView.insertText(id(CFMessage)); // Funkar!
	textView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
	parentView.addSubview(scrollView);
	scrollView.addSubview(textView);
	scrollView.setDocumentView(textView);
	result := textView;
end;
function SkelNewTextView(parentView: NSView; x,y,w,h: Real; initialText: AnsiString): NSTextView; overload;
begin
	Exit(SkelNewTextView(parentView, NSMakeRect(x,y,w,h), initialText));
end;
function SkelNewTextView(parentView: NSView; r: Rect; initialText: AnsiString): NSTextView; overload;
begin
	Exit(SkelNewTextView(parentView, RectToNSRect(r), initialText));
end;

function SkelNewSlider(parentView: NSView; sliderRect: NSRect): NSSlider;
var
	slider: NSSlider;
begin
	{ Adds a NSSlider }
	slider := NSSlider.alloc.initWithFrame(sliderRect);
	slider.setMinValue(0);
	slider.setMaxValue(10);
	parentView.addSubview(slider);
	result := slider;
end;
function SkelNewSlider(parentView: NSView; x,y,w,h: Real): NSSlider;
begin
	Exit(SkelNewSlider(parentView, NSMakeRect(x,y,w,h)));
end;
function SkelNewSlider(parentView: NSView; r: Rect): NSSlider;
begin
	Exit(SkelNewSlider(parentView, RectToNSRect(r)));
end;

procedure SkelSetSliderRange(slider: NSSlider; min, max: Real);
begin
	slider.setMinValue(min);
	slider.setMaxValue(max);
end;

// Builds a scrollview containing an arbitrary view
function SkelNewScrollView(parentView: NSView; scrollViewRect: NSRect; scrolledView: NSView): NSScrollView;overload;
var
	scrollView: NSScrollView;
begin
	scrollView := NSScrollView.alloc;
	scrollView.initWithFrame(scrollViewRect);
	scrollView.setHasVerticalScroller(true);
	scrollView.setHasHorizontalScroller(true);
	scrollView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
	scrollView.contentView.setAutoresizesSubviews(true);
	
	parentView.addSubview(scrollView);
	scrollView.addSubview(scrolledView);
	scrollView.setDocumentView(scrolledView);
	result := scrollView;
end;
function SkelNewScrollView(parentView: NSView; x,y,w,h: Real; scrolledView: NSView): NSScrollView; overload;
begin
	Exit(SkelNewScrollView(parentView, NSMakeRect(x,y,w,h), scrolledView));
end;
function SkelNewScrollView(parentView: NSView; r: Rect; scrolledView: NSView): NSScrollView; overload;
begin
	Exit(SkelNewScrollView(parentView, RectToNSRect(r), scrolledView));
end;

function SkelNewRadioGroup(parent: NSView; matrixRect: NSRect; items: array of AnsiString): NSMatrix; overload;
var
	prototype: NSButtonCell;
	myMatrix: NSMatrix;
	cellArray: NSArray;
	i: Longint;
begin
	prototype := NSButtonCell.alloc.init;
	prototype.setTitle(NSSTR('Watermelons'));
	prototype.setButtonType(NSRadioButton);
	myMatrix := NSMatrix.alloc.initWithFrame_mode_prototype_numberOfRows_numberOfColumns(
							matrixRect, NSRadioModeMatrix, NSCell(prototype), Length(items), 1);
	parent.addSubview(myMatrix);
	cellArray := myMatrix.cells;
	for i := 0 to High(items) do
		cellArray.objectAtIndex(i).setTitle(StringToNSString(items[i]));
	prototype.release;
	myMatrix.release;
	Exit(myMatrix);
end;
function SkelNewRadioGroup(parent: NSView; x,y,w,h: Real; items: array of AnsiString): NSMatrix; overload;
begin
	Exit(SkelNewRadioGroup(parent, NSMakeRect(x,y,w,h), items));
end;
function SkelNewRadioGroup(parent: NSView; r: Rect; items: array of AnsiString): NSMatrix; overload;
begin
	Exit(SkelNewRadioGroup(parent, RectToNSRect(r), items));
end;


function SkelNewStepper(parentView: NSView; stepperRect: NSRect): NSStepper;
var
	stepper: NSStepper;
begin
	{ Adds a NSStepper }
	stepper := NSStepper.alloc.initWithFrame(stepperRect);
	stepper.setMinValue(0);
	stepper.setMaxValue(10);
	parentView.addSubview(stepper);
	result := stepper;
end;
function SkelNewStepper(parentView: NSView; x,y,w,h: Real): NSStepper;
begin
	Exit(SkelNewStepper(parentView, NSMakeRect(x,y,w,h)));
end;
function SkelNewStepper(parentView: NSView; r: Rect): NSStepper;
begin
	Exit(SkelNewStepper(parentView, RectToNSRect(r)));
end;

procedure SkelSetStepperRange(stepper: NSStepper; min, max: Real);
begin
	stepper.setMinValue(min);
	stepper.setMaxValue(max);
end;



// Glue for creating views in a window (content view of window)
// SKIPPED due to overloading problems (Can't determine which overloaded function to call).
(*
function SkelNewButton(parentWindow: NSWindow; ATitle: shortstring; buttonRect: NSRect;
	aCallbackName: string; callback: TSStrProcPtr): NSButton; overload;
begin
	SkelNewButton := SkelNewButton(parentWindow.contentView, ATitle, buttonRect,
	aCallbackName, callback);
end;
function SkelNewButton(parentWindow: NSWindow; ATitle: shortstring;
	AX, AY, AWidth, AHeight: Double;
	ACallbackName: string; callback: TSStrProcPtr): NSButton; overload;
begin
	SkelNewButton := SkelNewButton(parentWindow.contentView, ATitle,
	AX, AY, AWidth, AHeight,
	ACallbackName, callback);
end;
function SkelNewTextField(parentWindow: NSWindow; textFieldRect: NSRect; initialText: AnsiString): NSTextField;overload;
begin
	SkelNewTextField := SkelNewTextField(parentWindow.contentView, textFieldRect, initialText);
end;
function SkelNewTextField(parentWindow: NSWindow; x,y,w,h: Real; initialText: AnsiString): NSTextField;overload;
begin
	SkelNewTextField := SkelNewTextField(parentWindow.contentView, x,y,w,h, initialText);
end;
function SkelNewStaticTextField(parentWindow: NSWindow; textFieldRect: NSRect; initialText: AnsiString): NSTextField; overload;
begin
	SkelNewStaticTextField := SkelNewStaticTextField(parentWindow.contentView, textFieldRect, initialText);
end;
function SkelNewStaticTextField(parentWindow: NSWindow; x,y,w,h: Real; initialText: AnsiString): NSTextField; overload;
begin
	SkelNewStaticTextField := SkelNewStaticTextField(parentWindow.contentView, x,y,w,h, initialText);
end;
function SkelNewTextView(parentWindow: NSWindow; scrollViewRect: NSRect; initialText: AnsiString): NSTextView; overload;
begin
	SkelNewTextView := SkelNewTextView(parentWindow.contentView, scrollViewRect, initialText);
end;
function SkelNewTextView(parentWindow: NSWindow; x,y,w,h: Real; initialText: AnsiString): NSTextView; overload;
begin
	SkelNewTextView := SkelNewTextView(parentWindow.contentView, x,y,w,h, initialText);
end;
function SkelNewSlider(parentWindow: NSWindow; sliderRect: NSRect): NSSlider;
begin
	SkelNewSlider := SkelNewSlider(parentWindow.contentView, sliderRect);
end;
function SkelNewSlider(parentWindow: NSWindow; x,y,w,h: Real): NSSlider;
begin
	SkelNewSlider := SkelNewSlider(parentWindow.contentView, x,y,w,h);
end;
function SkelNewScrollView(parentWindow: NSWindow; scrollViewRect: NSRect; scrolledView: NSView): NSScrollView;overload;
begin
	SkelNewScrollView := SkelNewScrollView(parentWindow.contentView, scrollViewRect, scrolledView);
end;
function SkelNewScrollView(parentWindow: NSWindow; x,y,w,h: Real; scrolledView: NSView): NSScrollView; overload;
begin
	SkelNewScrollView := SkelNewScrollView(parentWindow.contentView, x,y,w,h, scrolledView);
end;
function SkelNewRadioGroup(parentWindow: NSWindow; matrixRect: NSRect; items: array of AnsiString): NSMatrix; overload;
begin
	SkelNewRadioGroup := SkelNewRadioGroup(parentWindow.contentView, matrixRect, items);
end;
function SkelNewRadioGroup(parentWindow: NSWindow; x,y,w,h: Real; items: array of AnsiString): NSMatrix; overload;
begin
	SkelNewRadioGroup := SkelNewRadioGroup(parentWindow.contentView, x,y,w,h, items);
end;
*)

// Make a tab view. Must embed a number of SkelViews (which may contain other views)
// FLAW: HARD-CODED SIZE!
function SkelCreateTabGroup(parent: NSView; titles: array of AnsiString; views: array of SkelView): NSTabView;
var
	i: Longint;
	tabView: NSTabView;
	item: NSTabViewItem;
begin
	tabView := NSTabView.alloc.initWithFrame(NSMakeRect(10,10,300,300)).autorelease;
	parent.addSubview(tabView);
	
	for i := 0 to High(titles) do
	begin
		item := NSTabViewItem.alloc.initWithIdentifier(StringToNSString(titles[i])).autorelease;
		item.setLabel(StringToNSString(titles[i]));
		tabView.addTabViewItem(item);
		if i <= High(views) then
			item.setView(views[i]);
	end;
	SkelCreateTabGroup := tabView;
end;


// ---------- Timer --------

// Mini-mini class for the timer
type
	TimerController = objcclass(NSObject)
	public
//		constructor Create; override;
		{ Objective-c Methods }
		procedure TimerAction(sender: objc.id); message 'timerAction:';
	end;

var
	gTimer: NSTimer;
	myTimerController: TimerController;
	gTimerCallback: TSNoArgProcPtr;
	gTimerCallback2: TSDoubleProcPtr;

procedure TimerController.TimerAction(sender: objc.id);
begin
	if gTimerCallback <> nil then
	begin
		gTimerCallback();
	end;
	if gTimerCallback2 <> nil then
	begin
		gTimerCallback2(SkelSeconds);
	end;
end;

// Pictures

procedure SkelDrawPicturePart(img: NSImage; partOfPicture: NSRect; dest: NSRect);
var
	botLeft: NSPoint;
	transform: NSAffineTransform;
	 ctx: NSGraphicsContext;
begin
//	WriteLn(bounds.size.height:1:0);
//	bounds.origin.x := 50;
//	bounds.origin.y := 50;
	
// Works
//	img.setFlipped(true);
//	partOfPicture := NSMakeRect(0, 0, skelImage.size.width , skelImage.size.height); // Entire image
//	botLeft := NSMakePoint(bounds.origin.x, bounds.origin.y);
//	img.drawAtPoint_fromrect_operation_fraction(botLeft, partOfPicture, NSCompositeCopy, 1.0);
	
	img.setFlipped(true);
	ctx := NSGraphicsContext.currentContext;
	ctx.saveGraphicsState;
	transform := NSAffineTransform.transform;
//	transform.translateXBy_yBy(bounds.origin.x, bounds.origin.y + img.size.height);
	transform.translateXBy_yBy(dest.origin.x, dest.origin.y);
	transform.concat;
//	transform.scaleXBy_yBy(1.0, -1.0);
//	transform.scaleXBy_yBy(0.5, 0.5);
	transform.scaleXBy_yBy(dest.size.width / img.size.width, dest.size.height / img.size.height);
//	transform.set_;
	transform.concat;
	partOfPicture := NSMakeRect(0, 0, img.size.width, img.size.height); // Entire image
	
// Scale to fit and match dest!
	// Compensate position for scaling
	botLeft := NSMakePoint(dest.origin.x * img.size.width / dest.size.width, dest.origin.y * img.size.height / dest.size.height);
	botLeft := NSMakePoint(0, 0);
	img.drawAtPoint_fromrect_operation_fraction(botLeft, partOfPicture, NSCompositeCopy, 1.0);
	ctx.restoreGraphicsState;
end;

procedure SkelDrawPicture(img: NSImage; dest: NSRect); overload;
var
	partOfPicture: NSRect;
begin
	partOfPicture := NSMakeRect(0, 0, img.size.width, img.size.height); // Entire image
	SkelDrawPicturePart(img, partOfPicture, dest);
end;

procedure SkelDrawPicture(img: NSImage; x,y,w,h: Real); overload;
var
	bounds: NSRect;
begin
	bounds.origin.x := x;
	bounds.origin.y := y;
	bounds.size.width := w;
	bounds.size.height := h;
	SkelDrawPicture(img, bounds);
end;

procedure SkelDrawPicture(img: NSImage; x,y: Real); overload;
var
	bounds: NSRect;
begin
	bounds.origin.x := x;
	bounds.origin.y := y;
	bounds.size.width := img.size.width;
	bounds.size.height := img.size.height;
	SkelDrawPicture(img, bounds);
end;

function SkelGetPicture(fileName: AnsiString): NSImage; // Assumes 3-letter extensions! Like ".png".
var
	imageName: NSString;
	namePart, extPart: String;
	CFName, CFExt: NSString;
begin
	namePart := Copy(fileName, 1, Length(fileName)-4);
	extPart := Copy(fileName, Length(fileName)-2, 3);
	CFName := StringToNSString(namePart);
	CFExt := StringToNSString(extPart);
	
	imageName := NSBundle.mainBundle.pathForResource_ofType(CFName, CFExt);
	result := NSImage.alloc.initWithContentsOfFile(imageName);
end;

// ------------------ Window data array ----------------

type
	SkelWindowDataRec = record
		theWindow: NSWindow;
		clobberProc: SkelClobberWindowProc;
		closeProc: SkelCloseWindowProc;
		userDataPtr: Pointer;
//		resizeProc: SkelResizeWindowProc; To do
		activateProc: TSActivateWindowProc;
		keepToQuit: Boolean; // Has returned -2 on close - may quit but don't remove
		hasBeenAskedToClose: Boolean; // To avoid calling the close proc more than once.
	end;

var
	skelWindows: array of SkelWindowDataRec;

// -------------------- Delegate ------------------------

type
	WindowDelegate = objcclass(NSObject,NSWindowDelegateProtocol)
	public
		function windowShouldClose (window: NSWindow): Boolean; message 'windowShouldClose:';
		procedure windowWillClose (note: NSNotification); message 'windowWillClose:';
		procedure windowDidBecomeKey (note: NSNotification); message 'windowDidBecomeKey:';
		procedure windowDidBecomeMain (note: NSNotification); message 'windowDidBecomeMain:';
		procedure windowDidResignKey (note: NSNotification); message 'windowDidResignKey:';
		procedure windowDidResignMain (note: NSNotification); message 'windowDidResignMain:';
		procedure windowDidResize (note: NSNotification); message 'windowDidResize:';
		// Timer message, for quitting properly:
		procedure TryQuitAgain(sender: objc.id); message 'TryQuitAgain:';
	end;
    
var
	gWindowDelegate: WindowDelegate = nil;
	gWindowTimer: NSTimer = nil;
// App control variables:
	shouldKeepRunning: Boolean = true;
	quitDesired: Boolean = false;

function GetSkelWindowIndex(window: NSWindow): Longint;
var
	i: Longint;
begin
	for i := Low(skelWindows) to High(skelWindows) do
	begin
		if skelWindows[i].theWindow = window then
		begin
			result := i;
			Exit;
		end;
	end;
end;
procedure DoActivate(note: NSNotification; active: Boolean);
var
	theWindow: NSWindow;
	i: Longint;
begin
	theWindow := NSWindow(note.object_);
	if theWindow <> nil then
	begin
		i := GetSkelWindowIndex(theWindow);
		if skelWindows[i].activateProc <> nil then
			skelWindows[i].activateProc(active);
	end;
end;

procedure WindowDelegate.windowDidBecomeKey (note: NSNotification);
begin
//	WriteLn('windowDidBecomeKey');
	DoActivate(note, true);
end;
procedure WindowDelegate.windowDidBecomeMain (note: NSNotification); // Identical to windowDidBecomeKey
begin
//	WriteLn('windowDidBecomeMain');
	DoActivate(note, true);
end;
procedure WindowDelegate.windowDidResignKey (note: NSNotification);
begin
//	WriteLn('windowDidResignKey');
	DoActivate(note, false);
end;
procedure WindowDelegate.windowDidResignMain (note: NSNotification);
begin
//	WriteLn('windowDidResignMain');
	DoActivate(note, false);
end;
procedure WindowDelegate.windowDidResize (note: NSNotification);
//var
//	theWindow: NSWindow;
//	i: Longint;
begin
	WriteLn('windowDidResize');
	
//	theWindow := NSWindow(note.object_);
//	if theWindow <> nil then
//	begin
//		i := GetSkelWindowIndex(theWindow);
//		if skelWindows[i].resizeProc <> nil then
//			skelWindows[i].resizeProc(theWindow);
//	end;
end;

procedure SkelCloseWindow(theWindow: NSWindow; conditionally: Boolean);
var
	i, closeResult: Longint;
begin
	if theWindow = nil then
		Exit;
	for i := High(skelWindows) downto Low(skelWindows) do
		if skelWindows[i].theWindow = theWindow then
		begin
			if (not conditionally) or (skelWindows[i].closeProc = nil) then
			// Unconditional or no close proc - just close it!
			begin
				// No proc, not released when closed - assume keepToQuit!
				if not skelWindows[i].theWindow.isReleasedWhenClosed then
					skelWindows[i].keepToQuit := true; // SPECIAL - don't dispose, but don't let it stop us quitting
				
				skelWindows[i].theWindow.close; // OK to close so get rid of it!

// BUGJAKT 110510
// Skall man göra så här för att avsluta?
	if quitDesired then
		gWindowDelegate.TryQuitAgain(nil);

				Exit;
			end
			else if not skelWindows[i].hasBeenAskedToClose then
			// If it has been asked before, the host is responsible to finish closing.
			begin
			// Conditional - ask the close proc!
				closeResult := 0;
				if skelWindows[i].closeProc <> nil then
					closeResult := skelWindows[i].closeProc(skelWindows[i].theWindow, skelWindows[i].userDataPtr);
				skelWindows[i].hasBeenAskedToClose := true; // Don't ask again!
				if closeResult = -1 then
					SkelCancelCloseWindow(skelWindows[i].theWindow);
//					quitDesired := false; // Cancel always stops this
				if closeResult = -2 then
					skelWindows[i].keepToQuit := true; // SPECIAL - don't dispose, but don't let it stop us quitting
				if closeResult = 1 then
					skelWindows[i].theWindow.close; // OK to close so get rid of it!
				// If 0, the host is responsible to finish whatever transaction it needs
				// before closing, typically a sheet. Once done, it should call SkelCloseWindow
				// unconditionally, or SkelCancelCloseWindow.
				Exit;
			end;
		end;
end;

// Response to cancellation of sheet for closing a window
// Cancels ALL window closing, since that is what cancel should mean.
procedure SkelCancelCloseWindow(theWindow: NSWindow);
var
	i: Longint;
begin
	for i := High(skelWindows) downto Low(skelWindows) do
//		if skelWindows[i].theWindow = theWindow then
		begin
			skelWindows[i].hasBeenAskedToClose := false;
		end;
	quitDesired := false;
end;

function SkelWindowHasBeenAskedToClose(theWindow: NSWindow): Boolean;
var
	i: Longint;
begin
	SkelWindowHasBeenAskedToClose := false;
	for i := High(skelWindows) downto Low(skelWindows) do
		if skelWindows[i].theWindow = theWindow then
		begin
			SkelWindowHasBeenAskedToClose := skelWindows[i].hasBeenAskedToClose;
			WriteLn('SkelWindowHasBeenAskedToClose found a window');
			if skelWindows[i].hasBeenAskedToClose then
				WriteLn('and it was true.')
			else
				WriteLn('and it was false.');
			Exit;
		end;
	WriteLn('SkelWindowHasBeenAskedToClose found no window');
end;

procedure TryQuit;
var
	i: Longint;
	listEmpty: Boolean;
begin
	WriteLn('Trying to quit');
	
// Try to close all windows - from top to bottom to avoid deletion problems
	for i := High(skelWindows) downto Low(skelWindows) do
	begin
//		skelWindows[i].theWindow.performClose(gWindowDelegate); // Bad since there is no sender?
		
		if i <= High(skelWindows) then // In case the array has shortened. Bugfix 111104
			SkelCloseWindow(skelWindows[i].theWindow, true);
		
		// If anyone cancels, stop!
		if not quitDesired then
		begin
			WriteLn('Quit was refused');
			// This only checks if we may close, it doesn't actually close.
			Exit;
		end;
	end;
	
	listEmpty := true;
	for i := High(skelWindows) downto Low(skelWindows) do
		if not skelWindows[i].keepToQuit then // keepToQuit don't count!
			listEmpty := false;
	
	if listEmpty then // High(skelWindows) = -1 then
		shouldKeepRunning := false
	else
	begin
	// Try again later!
		if gWindowTimer = nil then
			gWindowTimer := NSTimer.alloc.init;
			
		if gWindowDelegate = nil then
			WriteLn('gWindowDelegate error');

// TEST för denna kraschar!
//		gWindowTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
//			1, gWindowDelegate, sel_registerName(PChar('TryQuitAgain:')), nil, false);
	end;
end;

function SkelGetUserData(window: NSWindow): Pointer;
var
	i: Longint;
begin
	for i := Low(skelWindows) to High(skelWindows) do
		if skelWindows[i].theWindow = window then
		begin
			result := skelWindows[i].userDataPtr;
			Exit(SkelGetUserData);
		end;
	result := nil;
end;

// We are asked whether the window accepts to be closed or not.
// May be response to window.performClose.
// PLAN: To make the window remember this wish and close later
// when appropriate dialogs are answered.
function WindowDelegate.windowShouldClose (window: NSWindow): Boolean;
var
	i, reply: Longint;
begin
	WriteLn('delegate windowShouldClose');
	
	i := GetSkelWindowIndex(window);
	SkelCloseWindow(skelWindows[i].theWindow, true); // Same close handler as for "manual" close.
(*
	if skelWindows[i].closeProc <> nil then
	begin
		reply := skelWindows[i].closeProc(skelWindows[i].theWindow, skelWindows[i].userDataPtr);
		result := reply > 0; // 1 = OK
		
		if reply = -1 then
			quitDesired := false; // Cancel
		
		// MEN hur sparar jag info om att den bör stängas, och hur testar jag det senare?
		// Ansvarar close-procen för det? Ja.
	end
	else
	begin
		result := true; // No proc - always OK
		
		// No proc, not released when closed - assume keepToQuit!
		if not skelWindows[i].theWindow.isReleasedWhenClosed then
		begin
			skelWindows[i].keepToQuit := true; // SPECIAL - don't dispose, but don't let it stop us quitting
		end;
	end;
*)
end;

// Does this really close a window at any time?
procedure InternalSkelDisposeWindow(theWindow: NSWindow; doRelease: Boolean);
var
	found: Boolean;
	i: Longint;
begin
	// Integrated search and removal.
	found := false;
	for i := Low(skelWindows) to High(skelWindows) do
	begin
		if found then
		begin
			skelWindows[i-1] := skelWindows[i]; // Shift down to shorten the array
		end
		else
		if skelWindows[i].theWindow = theWindow then
//		if skelWindows[i].theWindow.isReleasedWhenClosed then // If not- no clobber! We don't know when it clobbers.
		begin
			if skelWindows[i].clobberProc <> nil then
				skelWindows[i].clobberProc(skelWindows[i].theWindow, skelWindows[i].userDataPtr);
			
			if doRelease then
				skelWindows[i].theWindow.release; // Dispose!
			skelWindows[i].theWindow := nil;
			
			// Removes element from skelWindows array
			// (See above)
			
			found := true;
		end;
	end;
	if found then
		SetLength(skelWindows, Length(skelWindows)-1);
	if not found then
		WriteLn('SkelDisposeWindow failed!')
	else
		WriteLn('SkelDisposeWindow OK');
end;

procedure SkelDisposeWindow(theWindow: NSWindow);
begin
	InternalSkelDisposeWindow(theWindow, true);
end;

procedure WindowDelegate.windowWillClose (note: NSNotification);
begin
	WriteLn('delegate windowWillClose');
	if NSWindow(note.object_).isReleasedWhenClosed then
		WriteLn('isReleasedWhenClosed');
//		SkelDisposeWindow(NSWindow(note.object_));
	InternalSkelDisposeWindow(NSWindow(note.object_), false); // Remove from list but don't release
	// InternalSkelDisposeWindow is passed false, since a release
	// would fire this message again! (fixed 100412)
end;

procedure WindowDelegate.TryQuitAgain(sender: objc.id);
begin
	if quitDesired then
		TryQuit;
end;

// NewWindow replacement
function SkelNewWindow(theWindowRect: NSRect; title: String; front: Boolean): NSWindow;overload;
var
	CFTitle: NSString;
	view: SkelView;
begin
	{ Creates a simple window }
	// Correct the height to refer to the top of the screen
	theWindowRect.origin.y := NSScreen.mainScreen.frame.size.height - theWindowRect.origin.y - theWindowRect.size.height;
	
	result := NSWindow.alloc;
	result.initWithContentRect_styleMask_backing_defer{initWithContentRect}(theWindowRect,
		NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
		NSBackingStoreBuffered, LongBool(0{NO}));
	
	{ Initializes the title of the window }
//		CFTitle := CFStringCreateWithPascalString(nil, title, kCFStringEncodingUTF8);
	CFTitle := StringToNSString(title);
	result.setTitle(CFTitle);
	
	view := SkelNewView(nil, theWindowRect); // Pass nil since this is not a subview
	result.setContentView(view);
	
	{ Put's the window on the front z-order }
	if front then
		result.orderFrontRegardless;
end;
function SkelNewWindow(x, y, w, h: Real; title: String; front: Boolean): NSWindow; overload;
begin
	Exit(SkelNewWindow(NSMakeRect(x, y, w, h), title, front));
end;
function SkelNewWindow(r: Rect; title: String; front: Boolean): NSWindow; overload;
begin
	Exit(SkelNewWindow(NSRect(RectToCGRect(r)), title, front));
end;

procedure SkelSetWindowTitle(w: NSWindow; title: String);
begin
	if w <> nil then
		w.setTitle(StringToNSString(title));
end;

function SkelGetWindowTitle(w: NSWindow): AnsiString;
begin
	if w <> nil then
		SkelGetWindowTitle := NSStringToString(w.title)
	else
		SkelGetWindowTitle := '';
end;

// Multi-window apps must know what the active window is!
// Wrappers for hiding NS.
function SkelFrontWindow: NSWindow;
var
	i: Longint;
begin
	result := NSApp.mainWindow;

// If a window is created before the nib is loaded, it will not register
// properly, but we can handle it like this:
	if result = nil then
	begin
		for i := Low(skelWindows) to High(skelWindows) do
		begin
			if skelWindows[i].theWindow.isMainWindow then
				result := skelWindows[i].theWindow;
		end;
	end;
end;
function SkelKeyWindow: NSWindow;
var
	i: Longint;
begin
	result := NSApp.keyWindow;
	
// If a window is created before the nib is loaded, it will not register
// properly, but we can handle it like this:
	if result = nil then
	begin
		for i := Low(skelWindows) to High(skelWindows) do
		begin
			if skelWindows[i].theWindow.isKeyWindow then
				result := skelWindows[i].theWindow;
		end;
	end;
end;
procedure SkelBringToFront(w: NSWindow);
begin
//	w.orderFrontRegardless;
//	w.makeKeyAndOrderFront(nil); // Added 100621
	
	// The above doesn't work! Added 140703:
	w.performSelector_withObject_afterDelay(sel_registerName(PChar('orderFrontRegardless')), nil, 0.0);
	w.performSelector_withObject_afterDelay(sel_registerName(PChar('makeKeyAndOrderFront:')), nil, 0.0);
// Awful fix but it seems to work. Suggested here:
// http://www.cocoabuilder.com/archive/cocoa/282127-nswindow-makekeyandorderfront-problem.html
end;
procedure SkelShowWindow(theWindow: NSWindow); // Added 100621, rewritten 100806 to remove window controller
begin
	theWindow.orderFrontRegardless;
end;

// SelectWindow?
procedure SkelSetFocus(w: NSWindow); overload;
begin
	w.makeFirstResponder(GetWindowContentView(w));
end;
procedure SkelSetFocus(view: NSView); overload;
begin
	view.window.makeFirstResponder(view);
end;

// Sub-function of SkelSetWindowHandler
procedure InstallWindowInList(theWind: NSWindow; theCloseProc: SkelCloseWindowProc;
											theClobberProc: SkelClobberWindowProc; userData: Pointer);
var
	i, ix: Longint;
begin
// Install window in window list
	ix := -1;	
	for i := Low(skelWindows) to High(skelWindows) do
	begin
		if skelWindows[i].theWindow = theWind then
			ix := i;
	end;
	if ix < 0 then // Not in list - allocate a new one
	begin
		SetLength(skelWindows, Length(skelWindows)+1);
		ix := High(skelWindows);
	end;
	skelWindows[ix].theWindow := theWind;
	skelWindows[ix].closeProc := theCloseProc;
	skelWindows[ix].clobberProc := theClobberProc;
	skelWindows[ix].userDataPtr := userData;
	skelWindows[ix].keepToQuit := false; // Special response to close
	skelWindows[ix].hasBeenAskedToClose := false; // Ask many times

	if gWindowDelegate = nil then
		gWindowDelegate := WindowDelegate.alloc.init;
	skelWindows[ix].theWindow.setDelegate(gWindowDelegate);
	
	// To do: resize
end;

// Install window
procedure SkelSetWindowHandler(theWind: NSWindow;
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr;
		theCloseProc: SkelCloseWindowProc;
		theClobberProc: SkelClobberWindowProc; userData: Pointer);
var
	cn: AnsiString;
begin
	if theWind = nil then
		Exit;//(SkelSetWindowHandler);
	if (theMouseProc<>nil) or (theKeyProc<>nil) or (theDrawProc<>nil) then
	begin
		cn := CFStringToString(CFStringRef(NSStringFromClass(NSView(theWind.contentView).classForCoder)));
		// NOTE! This has to be a window with a SkelView as content view! (Which is what SkelNewWindow creates!)
		if cn = 'SkelView' then
		begin
			// Install procs for the content view
			InstallSkelViewHandler(SkelView(theWind.contentView),
				theMouseProc, theKeyProc, theDrawProc, userData);
			if theDrawProc <> nil then
				SkelInvalWindow(theWind);
		end;
	end;

	InstallWindowInList(theWind, theCloseProc, theClobberProc, userData);
end;

procedure SkelSetWindowHandler(theWind: NSWindow;
		theMouseProc: SkelQDCGMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelQDCGDrawProcPtr;
		theCloseProc: SkelCloseWindowProc;
		theClobberProc: SkelClobberWindowProc; userData: Pointer); overload;
var
	cn: AnsiString;
begin
	if theWind = nil then
		Exit;//(SkelSetWindowHandler);
	if (theMouseProc<>nil) or (theKeyProc<>nil) or (theDrawProc<>nil) then
	begin
		cn := CFStringToString(CFStringRef(NSStringFromClass(NSView(theWind.contentView).classForCoder)));
		// NOTE! This has to be a window with a SkelView as content view! (Which is what SkelNewWindow creates!)
		if cn = 'SkelView' then
		begin
			// Install procs for the content view
			SkelSetViewHandler(SkelView(theWind.contentView),
				theMouseProc, theKeyProc, theDrawProc, userData);
			if theDrawProc <> nil then
				SkelInvalWindow(theWind);
		end;
	end;

	InstallWindowInList(theWind, theCloseProc, theClobberProc, userData);
end;





// Install window (Old name)
procedure SkelWindow(theWind: NSWindow;
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr;
		theCloseProc: SkelCloseWindowProc;
		theClobberProc: SkelClobberWindowProc;
		userData: Pointer);
begin
	WriteLn('Warning: replace SkelWindow by SkelSetWindowHandler');
	SkelSetWindowHandler(theWind, theMouseProc, theKeyProc,
		theDrawProc, theCloseProc, theClobberProc, userData);
end;

// Calls directly into SkelView
procedure SkelSetWindowExtraMouseHandlers(theWind: NSWindow; altMouseProc, mouseUpProc, altMouseUpProc: SkelMouseProcPtr);
begin
	SkelSetViewExtraMouseHandlers(theWind.contentView, altMouseProc, mouseUpProc, altMouseUpProc);
end;
procedure SkelSetWindowMouseMovementHandlers(theWind: NSWindow; mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc: SkelMouseProcPtr);
begin
	SkelSetViewMouseMovementHandlers(theWind.contentView, mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc);
	if mouseMovedProc <> nil then
		theWind.setAcceptsMouseMovedEvents(true);
end;
procedure SkelSetWindowExtraEventHandlers(theWind: NSWindow; keyUpProc: SkelKeyProcPtr; scrollWheelProc: ProcPtr);
begin
	SkelSetViewExtraEventHandlers(theWind.contentView, keyUpProc, scrollWheelProc);
end;

// New callback installers! One call for each!

procedure SkelSetWindowMouseHandler(theWind: NSWindow; theMouseProc: SkelMouseProcPtr);overload;
begin
	SkelSetViewMouseHandler(theWind.contentView, theMouseProc);
end;
procedure SkelSetWindowMouseHandler(theWind: NSWindow; theMouseProc: SkelQDCGMouseProcPtr);overload;
begin
	SkelSetViewMouseHandler(theWind.contentView, theMouseProc);
end;
procedure SkelSetWindowDrawHandler(theWind: NSWindow; theDrawProc: SkelQDCGDrawProcPtr);overload;
begin
	SkelSetViewDrawHandler(theWind.contentView, theDrawProc);
end;
procedure SkelSetWindowDrawHandler(theWind: NSWindow; theDrawProc: SkelDrawProcPtr);overload;
begin
	SkelSetViewDrawHandler(theWind.contentView, theDrawProc);
end;
procedure SkelSetWindowKeyDownHandler(theWind: NSWindow;theKeyProc: SkelKeyProcPtr);
begin
	SkelSetViewKeyDownHandler(theWind.contentView, theKeyProc);
end;
procedure SkelSetWindowUserData(theWind: NSWindow; userData: Pointer);
begin
	SkelSetViewUserData(theWind.contentView, userData);
end;
procedure SkelSetWindowRightMouseHandler(theWind: NSWindow; altMouseProc: SkelMouseProcPtr);overload;
begin
	SkelSetViewRightMouseHandler(theWind.contentView, altMouseProc);
end;
procedure SkelSetWindowRightMouseHandler(theWind: NSWindow; altMouseProc: SkelQDCGMouseProcPtr);overload;
begin
	SkelSetViewRightMouseHandler(theWind.contentView, altMouseProc);
end;
procedure SkelSetWindowMouseUpHandler(theWind: NSWindow; mouseUpProc: SkelMouseProcPtr);overload;
begin
	SkelSetViewMouseUpHandler(theWind.contentView, mouseUpProc);
end;
procedure SkelSetWindowMouseUpHandler(theWind: NSWindow; mouseUpProc: SkelQDCGMouseProcPtr);overload;
begin
	SkelSetViewMouseUpHandler(theWind.contentView, mouseUpProc);
end;
procedure SkelSetWindowRightMouseUpHandler(theWind: NSWindow; altMouseUpProc: SkelMouseProcPtr);overload;
begin
	SkelSetViewRightMouseUpHandler(theWind.contentView, altMouseUpProc);
end;
procedure SkelSetWindowRightMouseUpHandler(theWind: NSWindow; altMouseUpProc: SkelQDCGMouseProcPtr);overload;
begin
	SkelSetViewRightMouseUpHandler(theWind.contentView, altMouseUpProc);
end;
procedure SkelSetWindowMouseEnterExitHandlers(theWind: NSWindow; mouseEnterProc, mouseExitProc: SkelMouseProcPtr);overload;
begin
	SkelSetViewMouseEnterExitHandlers(theWind.contentView, mouseEnterProc, mouseExitProc);
end;
procedure SkelSetWindowMouseEnterExitHandlers(theWind: NSWindow; mouseEnterProc, mouseExitProc: SkelQDCGMouseProcPtr);overload;
begin
	SkelSetViewMouseEnterExitHandlers(theWind.contentView, mouseEnterProc, mouseExitProc);
end;
procedure SkelSetWindowMouseMovedHandler(theWind: NSWindow; mouseMovedProc: SkelMouseProcPtr);overload;
begin
	if mouseMovedProc <> nil then
		theWind.setAcceptsMouseMovedEvents(true);
	SkelSetViewMouseMovedHandler(theWind.contentView, mouseMovedProc);
end;
procedure SkelSetWindowMouseMovedHandler(theWind: NSWindow; mouseMovedProc: SkelQDCGMouseProcPtr);overload;
begin
	if mouseMovedProc <> nil then
		theWind.setAcceptsMouseMovedEvents(true);
	SkelSetViewMouseMovedHandler(theWind.contentView, mouseMovedProc);
end;
procedure SkelSetWindowMouseDraggedHandler(theWind: NSWindow; mouseDraggedProc: SkelMouseProcPtr);overload;
begin
	SkelSetViewMouseDraggedHandler(theWind.contentView, mouseDraggedProc);
end;
procedure SkelSetWindowMouseDraggedHandler(theWind: NSWindow; mouseDraggedProc: SkelQDCGMouseProcPtr);overload;
begin
	SkelSetViewMouseDraggedHandler(theWind.contentView, mouseDraggedProc);
end;
procedure SkelSetWindowKeyUpHandler(theWind: NSWindow; keyUpProc: SkelKeyProcPtr);
begin
	SkelSetViewKeyUpHandler(theWind.contentView, keyUpProc);
end;
procedure SkelSetWindowScrollWheelHandler(theWind: NSWindow; scrollWheelProc: ProcPtr);
begin
	SkelSetViewScrollWheelHandler(theWind.contentView, scrollWheelProc);
end;






// Should go in SkelSetWindowHandler, historically.
// I may do that move later.
procedure SkelSetWindowActivateProc(theWindow: NSWindow; proc: TSActivateWindowProc);
var
	i: Longint;
begin
	i := GetSkelWindowIndex(theWindow);
	if i >= 0 then // ?
		skelWindows[i].activateProc := proc;
end;


procedure InternalBackgroundWithDuration (p: TSNoArgProcPtr; p2: TSDoubleProcPtr; intervalTime: Double);
//procedure SkelBackgroundWithDuration (intervalTime: EventTime);
begin
	myTimerController := TimerController.alloc.init;
	
	if gTimer <> nil then
	begin
		gTimer.release;
		gTimer := nil;
	end;
	gTimerCallback := p;
	gTimerCallback2 := p2;
	
	if (p <> nil) or (p2 <> nil) then
	begin
		gTimer := NSTimer.alloc; // Note! NOT init! It krasches! 110516.
		gTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
			intervalTime, myTimerController, sel_registerName(PChar('timerAction:')), nil, true);
	end;
end;

procedure SkelBackgroundWithDuration(p: TSDoubleProcPtr; intervalTime: Double); overload;
begin
	InternalBackgroundWithDuration(nil, p, intervalTime);
end;

procedure SkelBackgroundWithDuration(p: TSNoArgProcPtr; intervalTime: Double); overload;
begin
	InternalBackgroundWithDuration(p, nil, intervalTime);
end;

procedure SkelBackground(p: TSDoubleProcPtr); overload;
begin
	InternalBackgroundWithDuration(nil, p, kEventDurationSecond);
end;

procedure SkelBackground(p: TSNoArgProcPtr); overload;
begin
	InternalBackgroundWithDuration(p, nil, kEventDurationSecond);
end;

//procedure Home;
procedure SkelHome;
var
	mainBundle: CFBundleRef;
	resourcesURL: CFURLRef;
	path: AnsiString;
//	err: OSErr;
	success: Boolean;
begin
	mainBundle := CFBundleGetMainBundle();
	resourcesURL := CFBundleCopyResourcesDirectoryURL(mainBundle);
	SetLength(path, PATH_MAX);
	success := CFURLGetFileSystemRepresentation(resourcesURL, TRUE, PChar(path), PATH_MAX);
	CFRelease(resourcesURL);
	if success then
	begin
		chdir(path);
//		WriteLn('Current Path: ', path);
	end;
end;

procedure SkelInit;
var
	infoDictionary: NSDictionary;
	mainNibName: NSString;
	mainNib: NSNib;
begin
	{ Creates a AutoreleasePool for this thread. Every thread must have one }
	pool := NSAutoreleasePool.new;
	
	{ Creates the application NSApp object }
	NSApp := SkelApplication(SkelApplication.sharedApplication);
	NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular);
	// Added 141107 - the reason why applications were not "nice" when launched without bundle?
	
	{Application delegate, for apple events}
	gAppDelegate := AppDelegate.alloc.init;
	NSApp.setDelegate(gAppDelegate);
	
	{ CommandController object for handling menus and simple buttons}
	gCommandController := TSMenuController.alloc.init;

	// Do "Home"
	SkelHome;
	
// Find the main nib file (usually "main.nib", LWP default)
	infoDictionary := NSBundle.mainBundle.infoDictionary;
	mainNibName := infoDictionary.objectForKey(NSSTR('NSMainNibFile'));
	if mainNibName <> nil then
		WriteLn('Found nib name: ', NSStringToString(mainNibName));
// Load the main nib file, if it exists
	if mainNibName <> nil then
	begin
		mainNib := NSNib.alloc.init;
		mainNib := mainNib.initWithNibNamed_bundle(mainNibName, NSBundle.mainBundle);
		mainNib.instantiateNibWithOwner_topLevelObjects(NSApp, nil);
		
		if mainNib = nil then
			WriteLn('Nib failed to load!');
//		else
//			mainNib := mainNib.initWithNibNamed_bundle(mainNibName, NSBundle.mainBundle);
	end
	else
		mainNib := nil;
	
	// Reset the timer
	SkelResetTime();
end;

// NEW 160527: Event filter proc support!
// This makes it possible to inspect and event intercept an event 

// Extract vital data from NSEvent to the open EventDataRec
function GetEventData(e: NSEvent): EventDataRec;
var
	evt: EventDataRec;
	s: AnsiString;
begin
	case e.type_ of
		NSLeftMouseDown:
			evt.what := kSkelEventMouseDown;
		NSLeftMouseUp:
			evt.what := kSkelEventMouseUp;
		NSRightMouseDown:
			evt.what := kSkelEventRightMouseDown;
		NSRightMouseUp:
			evt.what := kSkelEventRightMouseUp;
		NSKeyDown, NSKeyUp:
		begin
			if e.type_ = NSKeyDown then
				evt.what := kSkelEventKeyDown
			else
				evt.what := kSkelEventKeyUp;
			evt.keyCode := e.keyCode;
			evt.isRepeat := e.isARepeat;
			s := CFStringToString(CFStringRef(e.characters));
			if Length(s) > 0 then
				evt.key := s[1]
			else
				evt.key := Char(0);
		end;
		NSScrollWheel:
		begin
			evt.what := kSkelScrollWheel;
			evt.delta.h := e.deltaX;
			evt.delta.v := e.deltaY;
		end;
	otherwise
		evt.what := kSkelEventUnknown;
	end;
	evt.modifiers := e.modifierFlags;
	evt.where := CGPointToPoint(CGPoint(e.locationInWindow));
end;

procedure SkelEventHook(proc: EventFilterProc);
begin
	gEventFilter := proc;
end;

function SkelGetEventHook: EventFilterProc;
begin
	SkelGetEventHook := gEventFilter;
end;

var
	gCurrentEvent: NSEvent; // Backdoor to current NSEvent
function SkelGetCurrentEvent: NSEvent;
begin
	Exit(gCurrentEvent);
end;

// Replacement of NSApplication.run
// Replaced in order to get control over quitting
// and possible event filtering/interception.
// Better to handle window closing by NSWindow messages?
procedure SkelMain;
var
	event: NSEvent;
	eventData: EventDataRec;
	didHandleEvent: Boolean;
begin
//	NSApp.run;
	
    NSApp.finishLaunching;
	
    shouldKeepRunning := true;
    quitDesired := false;
    repeat
		pool.release;
		pool := NSAutoreleasePool.alloc.init;
		
		event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue
							(NSAnyEventMask, NSDate.distantFuture,
							NSDefaultRunLoopMode, true);
		
		gCurrentEvent := event; // Emergency copy if we need to get extra data from the current event.

		// NEW 160527: EVENT FILTER
		if gEventFilter <> nil then
		begin
		// Build event record from NSEvent
		// Pass to event filter
			eventData := GetEventData(event);
			didHandleEvent := gEventFilter(eventData);
			if not didHandleEvent then
			begin
		        NSApp.sendEvent(event);
		        NSApp.updateWindows;
			end
		end
		else
		begin
	        NSApp.sendEvent(event);
	        NSApp.updateWindows;
        end;
        
        if quitDesired then
        begin
        	TryQuit;
        end;
    until not shouldKeepRunning;
	WriteLn('Leaving SkelMain correctly');
end;

procedure SkelApplication.terminate(sender: id);
begin
	WriteLn('SkelApplication.terminate');
	
	// Ask all windows to close
	// If we have OK from all - close
	quitDesired := true;
	
//	shouldKeepRunning := false;
end;

procedure SkelWhoa;
begin
	quitDesired := true;
//	shouldKeepRunning := false;
end;

procedure SkelClobber;
var
	i: Longint;
begin
// Clobber all known TransSkel windows.
	for i := Low(skelWindows) to High(skelWindows) do
	begin
		if skelWindows[i].clobberProc <> nil then
			skelWindows[i].clobberProc(skelWindows[i].theWindow, skelWindows[i].userDataPtr);
	end;
	pool.release;

// NOTE: Desirable feature to add: Possibility to cancel clobber on standard quit!
// But not if we get here. Quit should be handled by SkelMain, and SkelMain
// continues if clobbering fails.
// Thus, clobbering should NOT be here but upon exiting SkelMain!
// Do it using NS's document classes? (Hard to replicate on other platforms?)

// Clobber only at closing of window not OK?
end;

// ------------------- View Manager -------------------
// Simple things so far, mostly about hiding the CFStrings.

function SkelGetStringValue(view: NSControl): AnsiString; overload;
var
	cfText: CFStringRef;
begin
	// Check for NSTextView, otherwise assume NSControl compatible class
//	if 'NSTextView' = CFStringToString(CFStringRef(NSStringFromClass(view.classForCoder))) then
//		cfText := CFStringRef(NSTextView(view).string_)
//	else
		cfText := CFStringRef(view.stringValue);
	result := CFStringToString(cfText);
end;

function SkelGetStringValue(view: NSTextView): AnsiString; overload;
var
	cfText: CFStringRef;
begin
	cfText := CFStringRef(NSTextView(view).string_);
	result := CFStringToString(cfText);
end;

procedure SkelSetStringValue(view: NSControl; value: AnsiString); overload;
var
	CFText: NSString;
begin
//	CFText := CFStringCreateWithPascalString(nil, value, kCFStringEncodingUTF8);
	CFText := StringToNSString(value);
	// Check for NSTextView, otherwise assume NSControl compatible class
//	if 'NSTextView' = CFStringToString(CFStringRef(NSStringFromClass(view.classForCoder))) then
//		NSTextView(view).setString(NSString(CFText))
//	else
		view.setStringValue(CFText);
//	CFRelease(CFText);
end;

procedure SkelSetStringValue(view: NSTextView; value: AnsiString); overload;
var
	CFText: NSString;
begin
	CFText := StringToNSString(value);
	view.setString(NSString(CFText))
end;

function SkelGetNumValue(view: NSControl): Longint; overload;
begin
	result := view.intValue;
end;

procedure SkelSetNumValue(view: NSControl; value: Longint); overload;
//var
//	max, min: Longint;
begin
	view.setIntValue(value);
end;


// NSPopUpButton support
function SkelGetNumValue(view: NSPopUpButton): Longint; overload;
begin
	result := view.indexOfSelectedItem;
end;

procedure SkelSetNumValue(view: NSPopUpButton; value: Longint); overload;
//var
//	max, min: Longint;
begin
	view.selectItemAtIndex(value);
end;


function SkelGetRealValue(view: NSControl): Real; overload;
begin
	result := view.floatValue;
end;

procedure SkelSetRealValue(view: NSControl; value: Real); overload;
//var
//	max, min: Longint;
begin
	view.setFloatValue(value);
end;

function SkelGetNumStringValue(view: NSControl): Longint; overload;
var
	theString: Str255;
	value: Longint;
begin
	theString := SkelGetStringValue(view);
	StringToNum(theString, value);
	result := value;
end;

procedure SkelSetNumStringValue(view: NSControl; value: Longint); overload;
var
	theStr: Str255;
	oldValue: Longint;
begin
	NumToString(value, theStr);
	oldValue := SkelGetNumStringValue(view);
	if oldValue <> value then
		SkelSetStringValue(view, theStr);
end;

// Old names (prefixed after View Manager, was separate in TS4)

function VMGetStringValue(view: NSControl): AnsiString;
var
	cfText: CFStringRef;
begin
	cfText := CFStringRef(view.stringValue);
	
	result := CFStringToString(cfText);
end;

procedure VMSetStringValue(view: NSControl; value: AnsiString);
var
	CFText: NSString;
begin
	CFText := StringToNSString(value);
	view.setStringValue(CFText);
end;

function VMGetNumValue(view: NSControl): Longint; overload;
begin
	result := view.intValue;
end;

procedure VMSetNumValue(view: NSControl; value: Longint); overload;
//var
//	max, min: Longint;
begin
	view.setIntValue(value);
end;

function VMGetRealValue(view: NSControl): Real; overload;
begin
	result := view.floatValue;
end;

procedure VMSetRealValue(view: NSControl; value: Real); overload;
//var
//	max, min: Longint;
begin
	view.setFloatValue(value);
end;

function VMGetNumStringValue(view: NSControl): Longint; overload;
var
	theString: Str255;
	value: Longint;
begin
	theString := VMGetStringValue(view);
	StringToNum(theString, value);
	result := value;
end;

procedure VMSetNumStringValue(view: NSControl; value: Longint); overload;
var
	theStr: Str255;
	oldValue: Longint;
begin
	NumToString(value, theStr);
	oldValue := VMGetNumStringValue(view);
	if oldValue <> value then
		VMSetStringValue(view, theStr);
end;



// ------------ Utilities for using nib files for layouts -------------

// Philosophy: We don't necessarily want Cocoa to make connections from
// the nibs (but we can if we like). But we want to use nibs for layouts.
// The following functions will help for using nibs just like you do in
// ResEdit or Carbon nibs.

// Get a window (corresponding to GetNewWindow in the Toolbox).
// This must be a separate nib file. You specify the nib name, not the window.
// NOTE: This has failed me for one window, and I have not been able
// to figure out why. If nothing else works, find a working example
// and copy the nib from there.
function SkelGetNewWindow(nibName: String): NSWindow;
var
	nibNameCF: NSString;
	wc: NSWindowController;
begin
	wc := NSWindowController.alloc.init;
//	nibNameCF := CFStringCreateWithPascalString(nil, nibName, kCFStringEncodingUTF8);
	nibNameCF := StringToNSString(nibName);
	wc := wc.initWithWindowNibName(nibNameCF);
	if wc <> nil then
		wc.showWindow(nil);
	if wc = nil then
		result := nil
	else
		result := wc.window;
	// Question: Does wc get released?
end;

// Get a view of a specified kind, indexed. 1-based search, so view
// number 1 is the first one.
function SkelGetIndView(w: NSWindow; viewClass: String; index: Longint): NSView;
var
	foundView: NSView;
	ix: Integer;
	
	procedure GetIndViewInView(cv: NSView);
	var
		v: NSView;
		subViewList: NSArray;
		i: Integer;
	begin
		subViewList := cv.subViews;
		for i := 0 to subViewList.count-1 do
		begin
			v := NSView(subViewList.objectAtIndex(i));
			
			// Använd NSStringFromClass från NSObjCRuntime och classForCoder från NSObject
			if viewClass = CFStringToString(CFStringRef(NSStringFromClass(v.classForCoder))) then
			begin
				ix += 1;
				if ix = index then
				begin
					foundView := v;
					WriteLn('Found view #', ix, viewClass);
				end;
			end;
			GetIndViewInView(v);
		end;
	end;
var
	cv: NSView;
begin
	ix := 0;
	cv := w.contentView;
	foundView := nil;
	GetIndViewInView(cv);
	result := foundView;
end;

// Count views of a specified type.
function SkelCountViews(w: NSWindow; viewClass: String): Longint;
	function CountViewsInView(cv: NSView): Longint;
	var
		i: Longint;
		subViewList: NSArray;
		v: NSView;
	begin
		result := 0;
		subViewList := cv.subViews;
		// Sök genom arrayen!
		for i := 0 to subViewList.count-1 do
		begin
			v := NSView(subViewList.objectAtIndex(i));
			WriteLn(i, ': ', CFStringToString(CFStringRef(NSStringFromClass(v.classForCoder))), '(', v.tag, ')');
			
			// Använd NSStringFromClass från NSObjCRuntime och classForCoder från NSObject
			if viewClass = CFStringToString(CFStringRef(NSStringFromClass(v.classForCoder))) then
			begin
				result += 1;
			end;
			// Sök subvyer
			result += CountViewsInView(v);
		end;
	end;
var
	cv: NSView;
begin
	result := 0;
	cv := w.contentView;
	result += CountViewsInView(cv);
end;

// Simple wrapper to hide NS for cross-platform use
// Should be renamed to SkelGetWindowContentView!
function GetWindowContentView(w: NSWindow): NSView;
begin
	result := w.contentView;
end;
function SkelGetWindowContentView(w: NSWindow): NSView;
begin
	result := w.contentView;
end;

// Should be renamed to SkelGetViewFrame!
function GetViewFrame(view: NSView): NSRect;
begin
	result := view.frame;
end;
function SkelGetViewFrame(view: NSView): NSRect;
begin
	result := view.frame;
end;


// Many views have a "tag" value. If you use unique tags, you can use
// them for identification and get them with this function.
function SkelGetViewFromTag(w: NSWindow; tag: NSInteger): NSView;
var
	foundView: NSView;
	
	procedure GetViewInView(cv: NSView);
	var
		subViewList: NSArray;
		i: Integer;
		v: NSView;
	begin
		subViewList := cv.subViews;
		for i := 0 to subViewList.count-1 do
		begin
			v := NSView(subViewList.objectAtIndex(i));
			if v.tag = tag then
				foundView := v;
			GetViewInView(v);
		end;
	end;
var
	cv: NSView;
begin
	cv := w.contentView;
	GetViewInView(cv);
	result := foundView;
end;


// ----------- TransSkel menu access functions ---------------
// These functions are useful when you want to find an existing menu item that has been loaded from a nib.

// Main function. Two levels, not recursive; It is ment for standard menus
function SkelSearchMenuItem(wantedMenuName, wantedItemName: AnsiString; itemIndex: Longint; doMacRoman, partialName: Boolean): NSMenuItem;
var
	menuHeadItem, it: NSMenuItem;
	i, j: Longint;
	menuBarItems, menuItems: NSArray;
//	mi: NSMenuItem;
	menuName, itemName: AnsiString;
begin
	result := nil;
	
	// get menu bar menu list from the NSApp
	menuBarItems := NSApp.mainMenu.itemArray;
	for i := 0 to menuBarItems.count-1 do
	begin
		// Get a menu from the list
		menuHeadItem := menuBarItems.objectAtIndex(i);
		if menuHeadItem <> nil then
		begin
//			menuHeadItem := NSMenuItem(menuHeadItem);
//			cft := mi.title;
			// Get name of menu
			if doMacRoman then
				menuName := CFStringToMacRomanString(CFStringRef(menuHeadItem.title))
			else
				menuName := CFStringToString(CFStringRef(menuHeadItem.title));
			
			// If partial name match desired, shorten the menuName
			if partialName then
				if menuName <> '' then
					if wantedMenuName <> '' then
						if Length(wantedMenuName) < Length(menuName) then
							menuName := Copy(menuName, 1, Length(wantedMenuName));
			
			//WriteLn('Item name:', CFStringToString(CFStringRef(menuHeadItem.title)));
			
			// Searching only for the menu
			if wantedMenuName <> '' then // has a menu name
				if wantedItemName = '' then // no item name
					if itemIndex < 0 then // no item number
						if menuName = wantedMenuName then
							result := menuHeadItem;
			
			if menuHeadItem.hasSubmenu then // It should have a submenu
			begin
				//WriteLn('Has submenu');
				
				// Get the menu items
				menuItems := menuHeadItem.subMenu.itemArray;
				
				// For each item...
				for j := 0 to menuItems.count-1 do
				begin
					it := menuItems.objectAtIndex(j);
					if it <> nil then
					begin
//						mi := NSMenuItem(it);
						if doMacRoman then
							itemName := CFStringToMacRomanString(CFStringRef(it.title))
						else
							itemName := CFStringToString(CFStringRef(it.title));

						// If partial name match desired, shorten the menuName
						if partialName then
							if itemName <> '' then
								if wantedItemName <> '' then
									if Length(wantedItemName) < Length(itemName) then
										itemName := Copy(itemName, 1, Length(wantedItemName));
						
						// Test for match
						if wantedMenuName <> '' then // Menu name + index
							if itemIndex >= 0 then
								if (menuName = wantedMenuName) and (j+1 = itemIndex) then
									result := it;
						if wantedMenuName = '' then // No menu name, item name
							if wantedItemName <> '' then
								if wantedItemName = itemName then
								result := it;
						if wantedMenuName <> '' then // Menu name + item name
							if wantedItemName <> '' then
								if (menuName = wantedMenuName) and (wantedItemName = itemName) then
									result := it;
					end;
				end;
			end;
		end;
	end;
end;

function SkelGetMenuItemByUTFName(itemName: AnsiString): NSMenuItem;
begin
	result := SkelSearchMenuItem('', itemName, -1, false, false);
end;

function SkelGetMenuItemByName(itemName: AnsiString): NSMenuItem;
begin
	result := SkelSearchMenuItem('', itemName, -1, true, false);
end;

function SkelGetMenuByName(menuName: AnsiString): NSMenuItem;
begin
	result := SkelSearchMenuItem(menuName, '', -1, true, false);
end;

function SkelGetMenuItemByPartialName(itemName: AnsiString): NSMenuItem;
begin
	result := SkelSearchMenuItem('', itemName, -1, true, true);
end;

function SkelGetMenuItemByMenuNameAndIndex(menuName: AnsiString; itemIndex: Longint): NSMenuItem;
begin
	result := SkelSearchMenuItem(menuName, '', itemIndex, true, false);
end;

function SkelGetMenuItemByMenuNameAndItemName(menuName, itemName: AnsiString): NSMenuItem;
begin
	result := SkelSearchMenuItem(menuName, itemName, -1, true, false);
end;



function ItemAtIndex(theMenu: NSMenu; index: Longint): NSMenuItem;
begin
	if theMenu <> nil then
		result := theMenu.itemArray.objectAtIndex(index)
	else
		result := nil;
end;

// Menu item enable/disable
// Doesn't work???

// Enable
procedure EnableMenuItem( theMenu: NSMenu; index: Longint ); overload;
begin
	theMenu.setAutoenablesItems(false);
// If this gives you an error, the itemAtIndex method might be missing in your NSMenu.inc.
//	theMenu.itemAtIndex(index).setEnabled(true);
	ItemAtIndex(theMenu, index-1).setEnabled(true); // -1?
end;
procedure EnableMenuItem( theMenu: NSMenu; title: AnsiString ); overload;
begin
	theMenu.setAutoenablesItems(false);
	theMenu.itemWithTitle(StringToNSString(title)).setEnabled(true);
end;
procedure EnableMenuItem( theMenuItem: NSMenuItem ); overload;
begin
	theMenuItem.menu.setAutoenablesItems(false);
	theMenuItem.setEnabled(true);
end;
// Disable
procedure DisableMenuItem( theMenu: NSMenu; index: Longint ); overload;
begin
	theMenu.setAutoenablesItems(false);
// If this gives you an error, the itemAtIndex method might be missing in your NSMenu.inc.
//	theMenu.itemAtIndex(index-1).setEnabled(false);
	ItemAtIndex(theMenu, index-1).setEnabled(false); // -1?
end;
procedure DisableMenuItem( theMenu: NSMenu; title: AnsiString ); overload;
begin
	theMenu.setAutoenablesItems(false);
	theMenu.itemWithTitle(StringToNSString(title)).setEnabled(false);
end;
procedure DisableMenuItem( theMenuItem: NSMenuItem ); overload;
begin
	theMenuItem.menu.setAutoenablesItems(false);
	theMenuItem.setEnabled(false);
end;
// Enable/disable in the same call, with a Boolean.
procedure EnableMenuItem( theMenu: NSMenu; index: Longint; enabled: Boolean); overload;
begin
	theMenu.setAutoenablesItems(false);
// If this gives you an error, the itemAtIndex method might be missing in your NSMenu.inc.
	ItemAtIndex(theMenu, index-1).setEnabled(enabled);
//	theMenu.itemAtIndex(index-1).setEnabled(enabled);
end;
procedure EnableMenuItem( theMenu: NSMenu; title: AnsiString; enabled: Boolean); overload;
begin
	theMenu.setAutoenablesItems(false);
	theMenu.itemWithTitle(StringToNSString(title)).setEnabled(enabled);
end;
procedure EnableMenuItem( theMenuItem: NSMenuItem; enabled: Boolean); overload;
begin
	theMenuItem.menu.setAutoenablesItems(false);
	theMenuItem.setEnabled(enabled);
end;

procedure SkelCheckMenuItem(theMenu: NSMenu; title: AnsiString; checked: Boolean); overload;
begin
	theMenu.itemWithTitle(StringToNSString(title)).setState(Ord(checked));
end;
procedure SkelCheckMenuItem(theMenuItem: NSMenuItem; checked: Boolean); overload;
begin
	theMenuItem.setState(Ord(checked));
end;
procedure SkelCheckMenuItem(theMenu: NSMenu; index: Longint; checked: Boolean); overload;
begin
// If this gives you an error, the itemAtIndex method might be missing in your NSMenu.inc.
//	theMenu.itemAtIndex(index-1).setState(Ord(checked));
	ItemAtIndex(theMenu, index-1).setState(Ord(checked));
end;



// -------------------- Alerts -----------------------

function SkelAlert(msg, okMsg, cancelMgs, altMsg, infoMsg: String): Integer;
var
	CFMsg, CFOK, CFCancel, CFAlt, CFInfoMsg: NSString;
begin
	if infoMsg = '' then infoMsg := ' ';
	CFMsg := StringToNSString(msg);
	CFOK := StringToNSString(okMsg);
	CFCancel := StringToNSString(cancelMgs);
	CFAlt := StringToNSString(altMsg);
	CFInfoMsg := StringToNSString(infoMsg);
	
	result := NSAlert.alertWithMessageText_defaultButton_alternateButton_otherButton_informativeTextWithFormat(
		CFMsg, CFOK, CFCancel, CFAlt, CFInfoMsg).runModal;
end;

// Copied with minimal changes from NoNib
procedure SkelModalDialog(w: NSWindow);
var
	app     : NSApplication;
	session : NSModalSession;
begin
	{Simulate TForm.ShowModal}
	app := NSApplication.sharedApplication;
	session := app.beginModalSessionForWindow(w);
	w.setReleasedWhenClosed(False);
	//Delay release so we can detect by calling window's isVisible.
	//Note: Investigate _NSWindowWillCloseNotification as a better approach
	// to just checking if window no longer visible.
	while w.isVisible do
	begin
		app.runModalSession(session);
		Sleep(100);		//To avoid tight loop that eats CPU time.
	end;
	w.setReleasedWhenClosed(True);
	 //Appears safe to call Close again even if window closed itself
	 // or user clicked close box.
	app.endModalSession(session);
end;

type SheetDelegate = objcclass(NSObject)
	public
		procedure sheetDidEnd_returnCode_contextInfo(sheet: NSWindow; returnCode: NSInteger; contextInfo: Pointer);
			message 'sheetDidEnd:returnCode:contextInfo:';
	end;

var
	internalSheetDelegate: SheetDelegate;

// The sheet timer seems to be necessary to avoid errors when two sheets follow immediately after each other.
type
	SheetTimerController = objcclass(NSObject)
	public
		procedure TimerAction(sender: objc.id); message 'timerAction:';
	end;
var
	gSheetTimer: NSTimer;
	mySheetTimerController: SheetTimerController;
	gCtx: SkelSheetPtr;
	
procedure SheetTimerController.TimerAction(sender: objc.id);
begin
	WriteLn('DING');
	if gCtx <> nil then
		if gCtx^.sheetProc <> nil then
			gCtx^.sheetProc(gCtx^.returnCode, gCtx^.userData);
	Dispose(gCtx);
	gCtx := nil;
//	gSheetTimer.release;
	mySheetTimerController.release;
end;

// Another timer, just to call the sheet result later (to circumvent the sheet-on-sheet problem).
procedure CallSheetLater(ctx: SkelSheetPtr);
begin
	WriteLn('DONG');
	mySheetTimerController := SheetTimerController.alloc.init;
	gCtx := ctx;
	gSheetTimer := NSTimer.alloc; // Note! NOT init! It krasches! 110516.
	gSheetTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
		0.1, mySheetTimerController, sel_registerName(PChar('timerAction:')), nil, false);
end;

procedure SheetDelegate.sheetDidEnd_returnCode_contextInfo(sheet: NSWindow; returnCode: NSInteger; contextInfo: Pointer);
var
	ctx: SkelSheetPtr;
begin
	ctx := SkelSheetPtr(contextInfo);
	ctx^.returnCode := returnCode;
	// The sheetProc causes problems if it fires up another sheet immediately.
	// A delay might help?
//	if ctx^.sheetProc <> nil then
//		ctx^.sheetProc(returnCode, ctx^.userData);
	if ctx^.sheetProc <> nil then
		CallSheetLater(ctx)
	else	
		Dispose(ctx);
end;

procedure SkelSheetAlert(message, defaultButton, thirdButton, otherButton, addText: AnsiString;
		window: NSWindow; callbackProc: SkelSheetProc; userData: Pointer); overload;
var
	ctx: SkelSheetPtr;
begin
	// Create sheet delegate!
	if internalSheetDelegate = nil then
		internalSheetDelegate := SheetDelegate.alloc.init;	
	
	ctx := New(SkelSheetPtr);
	ctx^.sheetProc := callbackProc;
	ctx^.userData := userData;
	
	NSBeginAlertSheet(
		StringToNSString(message),			// sheet message
		StringToNSString(defaultButton),	// default button label (1)
		StringToNSString(thirdButton),		// third button label (0)
		StringToNSString(otherButton),		// other button label (-1)
		window,					// window sheet is attached to
		internalSheetDelegate,	// delegate
		sel_registerName('sheetDidEnd:returnCode:contextInfo:'), // did-end selector
		nil,					// no need for did-dismiss selector
		ctx,			// context info - pointer to user data
		StringToNSString(addText),	// additional text
		[]);
end;

// Simplified call for warnings/notifications, only one button and no callback
procedure SkelSheetAlert(message, defaultButton, addText: AnsiString; window: NSWindow); overload;
begin
	SkelSheetAlert(message, defaultButton, '', '', addText, window, nil, nil);
end;


// Timer based on GetTimeOfDay
var
	timeStart: timeval;
	hasStart: Boolean = false;

function SkelSeconds(): Double;
var
	tv: timeval;
begin
	fpGetTimeOfDay(@tv, nil);
	if not hasStart then
	begin
		hasStart := true;
		timeStart := tv;
	end;
	SkelSeconds := Double(tv.tv_usec - timeStart.tv_usec) / 1000000.0 + double(tv.tv_sec - timeStart.tv_sec);
end;

function SkelTickCount(): Double; // For compatibility with old TickCount - but floating-point
begin
	SkelTickCount := SkelSeconds * 60;
end;

// If you want to start from right now.
procedure SkelResetTime();
var
	tv: timeval;
begin
	fpGetTimeOfDay(@tv, nil);
	hasStart := true;
	timeStart := tv;
end;



procedure SkelSetViewResizing(theView: NSView; minx, width, maxx, miny, height, maxy, subviews: Boolean);
var
	mask: Longint;
begin
	theView.setAutoresizesSubviews(subviews);
	
	mask := NSViewNotSizable;
	if minx then
		mask += NSViewMinXMargin;
	if width then
		mask += NSViewWidthSizable;
	if maxx then
		mask += NSViewMaxXMargin;
	if miny then
		mask += NSViewMinYMargin;
	if height then
		mask += NSViewHeightSizable;
	if maxy then
		mask += NSViewMaxYMargin;
	theView.setAutoresizingMask(mask);
end;

// New 151201 /Ingemar
procedure SkelRemoveView(view: NSView; removeSubViews: Boolean);
var
	i: Longint;
begin
	// Remove subviews too!
	if removeSubViews then
	begin
		WriteLn(view.subViews.count);
		for i := view.subViews.count-1 downto 0 do
		begin
			if view.subViews.objectAtIndex(i) = nil then;
			SkelRemoveView(view.subViews.objectAtIndex(i), true);
		end;
	end;
	// Remove view
	view.removeFromSuperview; // sends autoRelease!
//	view.release; // Not needed
end;



end.

