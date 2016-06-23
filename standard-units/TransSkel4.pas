{
TransSkel Pascal 4.1.1

A small and easy-to-use Mac application framework.

This is the very first version (2007) of the new Carbon Event-savy version of TransSkel.
There already was a Carbon version (TransSkel Pascal 2.7) but it used WaitNextEvent and
was very "classic" in its style overall.

This version uses Carbon Events, and is also designed to use command-based menus,
NIB files etc. "Classic" style is still possible in most aspects, but the important
thing is that it is redesigned to use the modern Carbon Events internally and that
the modern APIs should always be the preferred way.

The original TransSkel was written by Paul duBois. This version is based on TransSkel
Pascal 2.0-2.7, branched from TransSkel at TransSkel 2.0 when Owen Hartnett
translated the package to Pascal.

This version uses surprisingly much code from the original package. When I made the
modern "Skel" (not the TransSkel using one but the standalone demo), all code was
rewritten, but for TransSkel, the overall design is so good that only some parts needed
total replacement (the event loop and its nearest functions). Compatibility with old
TransSkel code is pretty good.

4.0a1 (summer 07): First version after revision
4.0a2 (march 08): Exported SkelBackgroundWithDuration
4.0 (080415): TransEdit is back (old-style). Fixed a serious bug when closing window.
4.0.1 (080603): Cleanup. kEventWindowClose always handled even if no close proc is provided.
4.0.2 (081030): Added SkelCallNextEventHandler, allows leaving an event to later handlers.
4.0.3 (090117): Added SkelAppleEventFS, to use FSRef instead of FSSpec.
4.0.4 (090227): HandlePrintDoc finally implemented.
4.0.5 (090421): FPCMacOSAll changed to MacOSAll
4.0.6 (091122): Home chdir's to resource folder in SkelInit.
4.0.7 (131228): Added new keyboard input method
4.1 (150325-): Major overhaul! Retina support, merged with SkelView and View Manager...
Much of this is accomplished with new variants of SkelWindow, the call I was going to
remove/rename... Suddenly it makes more sense, at least for now.

Retina support
Moved in SkelView and ViewManager into TransSkel.
QDCG supported.
SkelNewWindow added? No, this is also SkelWindow. Hm...
New SkelWindow - sends most events to the main SkelView!
Menu functions similar ti TS5
SkelAlert as TS5

maybe this?
$SETC supportQuickdraw = TRUE
$SETC supportQDGC = TRUE

// 150727: Split out ReinstallEvents to separate function to make it possible to install events later (more like TS5).
// 150728: Added a mouse button parameter to the mouse callbacks to SkelViews. (Could be added to the old-style window
// handlers as well but I consider these obsolete.) Sadly this is not compatible with the current TS5
// but since it is more compact I prefer it.
4.1.1 (160114): Added scroll view support.



Old notes before 4.1:

The few and small changes since 4.0a1 implies that the package works very well and seems
to have few bugs. :-)

NOT DONE YET (old list, mostly done):
- TransDisplay and TransEdit should be replaced by fairly small packages using MLTE.
(Done for TransDisplay.) New: MLTE is now scrapped! No TransEdit for TS4.
- Many demos can be revised to use newer APIs, especially CG and MLTE instead of
QuickDraw and TextEdit. (Note that the newer TransSkel 5 include solutions for this,
the QDCG package and a new TransEdit.) New: QDCG is now integrated with TS4 too!
- I would like to make additions to make modern dialog management easier. This is an
interesting problem that requires some new ideas. The unit "ViewManager" solves a lot
of this and makes dialog (actually view) management really nice and easy. (Integrated
with TransSkel 5.) New: Now integrated here too!
- Replace FSSpec with FSRef. In particular, Apple Event callbacks should be passed FSRefs.
(This is solved, 090117, version 4.0.3 above.)
}

{$mode macpas}
unit TransSkel4;
interface
uses MacOSAll, QDCG;

// Procedure pointer types for callbacks.
type
	TSMouseProcPtr = PROCEDURE(thePoint: MacOSAll.Point; theTime: UInt32; theMods: Integer);
	TSKeyProcPtr = PROCEDURE(theChar: char; theMods: integer);
	TSEventProcPtr = PROCEDURE(theitem: integer; var theEvent: EventRecord);
	TSOtherEventProcPtr = FUNCTION(var theEvent: EventRecord): Boolean;
	TSBooleanProcPtr = PROCEDURE(myBool: Boolean);
	TSIntProcPtr = PROCEDURE(myInt: integer);
	TSMenuProcPtr = PROCEDURE(myMenu: MenuHandle);
	TSNoArgProcPtr = PROCEDURE();
	TSFilterProcPtr = PROCEDURE(theDialog: DialogPtr; var theEvent: EventRecord; var result: Boolean);
	TSAEFileProcPtr = PROCEDURE(fs: FSSpec; isLastInBatch: Boolean);
	TSAEFSFileProcPtr = PROCEDURE(fs: FSRef; isLastInBatch: Boolean);
	TSLongProcPtr = FUNCTION(myInt: Longint): Boolean;

// Custom carbon events can be handled with the standard handler interface.
	TSCarbonEventProcPtr = EventHandlerUPP; // or maybe FUNCTION(theEvent: EventRef; ): Boolean;

{$SETC supportDialogs = TRUE}

	procedure SkelInit;
	function SkelGetMainNib: IBNibRef;
	procedure SkelMain; // Use RunApplicationEventLoop instead.
	procedure SkelWhoa;
	procedure SkelClobber;
	procedure SkelSetCommandProc(theCommandProc: TSLongProcPtr);
	function SkelMenu (theMenu: MenuHandle; pSelect: TSIntProcPtr; pClobber: TSMenuProcPtr; DrawBar: Boolean): Boolean;
	function SkelHMenu (theMenu: MenuHandle; pSelect: TSIntProcPtr; pClobber: TSMenuProcPtr): Boolean; {Added by Ingemar 22/8 -93}
	procedure SkelRmveMenu (theMenu: MenuHandle);
	procedure SkelApple (aboutTitle: Str255; aboutProc: TSIntProcPtr);
	function SkelWindow (theWind: WindowPtr; pMouse: TSMouseProcPtr; pKey: TSKeyProcPtr;
					pUpdate: TSBooleanProcPtr; pActivate: TSBooleanProcPtr;
					pClose, pClobber: TSNoArgProcPtr; pIdle: TSNoArgProcPtr; frontOnly: Boolean): Boolean; overload;
	function SkelCustomWindow (theWind: WindowPtr; pMouse: TSMouseProcPtr;
					pKey: TSKeyProcPtr; pUpdate: TSBooleanProcPtr;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean;
					idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
					customEventTypes: EventTypeSpecPtr; customEventCount: Longint): Boolean;
	// New name for SkelWindow
	function SkelSetWindowHandler (theWind: WindowPtr; pMouse: TSMouseProcPtr; pKey: TSKeyProcPtr; pUpdate: TSBooleanProcPtr;
			pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr; pIdle: TSNoArgProcPtr; frontOnly: Boolean): Boolean;
	procedure SkelRmveWind (theWind: WindowPtr);
{ $IFC supportDialogs }
	function SkelDialog (theDialog: DialogPtr; pEvent: TSEventProcPtr; pClose, pClobber: TSNoArgProcPtr; pFilter: TSFilterProcPtr): Boolean; {pFilter added by Ingemar 18/9-93}
	procedure SkelRmveDlog (theDialog: DialogPtr);
{ $ENDC}
	procedure SkelGrowBounds (theWind: WindowPtr; hLO, vLo, hHi, vHi: integer);
	procedure SkelEventMask (mask: integer);
	procedure SkelGetEventMask (var mask: integer);
	procedure SkelBackground (p: TSNoArgProcPtr);
	procedure SkelBackgroundWithDuration (p: TSNoArgProcPtr; intervalTime: EventTime);
	procedure SkelGetBackground (var p: TSNoArgProcPtr);
	procedure SkelEventHook (p: TSOtherEventProcPtr);
	procedure SkelGetEventHook (var p: TSOtherEventProcPtr);
{ $IFC supportDialogs }
	procedure SkelDlogMask (mask: integer);
	procedure SkelGetDlogMask (var mask: integer);
{ $ENDC}
{Two new procedures for WNE-support, added by Ingemar 12/11-93}
	procedure SkelSetSleep (newSleep: Longint);
	procedure SkelSetMouseRgn (newMouseRgn: RgnHandle);
{Utility function, added by Ingemar 13/7-94}
	function FindWindowByRefcon (theRefCon: Longint): WindowPtr;
{Suspend/resume, added by Ingemar 23/7 -94}
	procedure SkelSetSuspendResume (p: TSBooleanProcPtr);
	function SkelGetSuspendResume: TSBooleanProcPtr;
{Mouse moved events, added by Ingemar jan -95:}
	procedure SkelSetMouseMoved (p: TSNoArgProcPtr);
	function SkelGetMouseMoved: TSNoArgProcPtr;
{Apple Event support, by Ingemar 10/4 -02}
	procedure SkelAppleEvent (openAppProc: TSNoArgProcPtr; openDocProc, printProc: TSAEFileProcPtr; quitProc: TSNoArgProcPtr); {Old version, using FSSpec}
	procedure SkelAppleEventFS (openAppProc: TSNoArgProcPtr; openDocProc, printProc: TSAEFSFileProcPtr; quitProc: TSNoArgProcPtr); {New version, using FSRef}
	function SkelGetVersion: AnsiString; // Version string!
	procedure SkelCallNextEventHandler; // Leave current event to later handlers.
{Input Services keyboard input}
	function SkelGetEventKeyChar(theEvent: EventRef): Char;

{Late addition to conform with TS5}
	procedure SkelInvalWindow(wind: WindowPtr);


// -------SkelView ------

type
	TSDrawProcPtr = procedure(theView: HIViewRef; cgContext: CGContextRef; viewRect: CGRect; userData: Pointer);
	VMQDDrawProcPtr = procedure(theView: HIViewRef; viewRect: MacOSAll.Rect; userData: Pointer);
	VMQDCGDrawProcPtr = procedure(theView: HIViewRef; viewRect: QDCG.Rect; userData: Pointer);
	VMMouseProcPtr = procedure(theView: HIViewRef; where: HIPoint; mods, button: Longint; userData: Pointer);
	VMQDMouseProcPtr = procedure(theView: HIViewRef; where: MacOSAll.Point; mods, button: Longint; userData: Pointer);
	VMQDCGMouseProcPtr = procedure(theView: HIViewRef; where: QDCG.Point; mods, button: Longint; userData: Pointer);
	VMKeyProcPtr = procedure(theView: HIViewRef; key: Char; mods: Longint; userData: Pointer);

// Calls by sig/ID
procedure InstallSkelViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theDrawProc: TSDrawProcPtr; theMouseProc: VMMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);overload;
procedure InstallQDSkelViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theDrawProc: VMQDDrawProcPtr; theMouseProc: VMQDMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);overload;
procedure InstallQDCGSkelViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theDrawProc: VMQDCGDrawProcPtr; theMouseProc: VMQDCGMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);overload;
procedure InstallSkelViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theDrawProc: TSDrawProcPtr; theQDDrawProc: VMQDDrawProcPtr; theQDCGDrawProc: VMQDCGDrawProcPtr;
		theMouseProc: VMMouseProcPtr; theQDMouseProc: VMQDMouseProcPtr; theQDCGMouseProc: VMQDCGMouseProcPtr;
		theKeyProc: VMKeyProcPtr; userData: Pointer);overload;

// Calls by pointer
procedure InstallSkelViewHandler(theWind: WindowRef; control: ControlRef; theDrawProc: TSDrawProcPtr;
		theMouseProc: VMMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer); overload;
procedure InstallQDSkelViewHandler(theWind: WindowRef; control: ControlRef; theDrawProc: VMQDDrawProcPtr;
		theMouseProc: VMQDMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer); overload;
procedure InstallQDCGSkelViewHandler(theWind: WindowRef; control: ControlRef; theDrawProc: VMQDCGDrawProcPtr;
		theMouseProc: VMQDCGMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer); overload;
procedure InstallSkelViewHandler(theWind: WindowRef; control: ControlRef;
		theDrawProc: TSDrawProcPtr; theQDDrawProc: VMQDDrawProcPtr; theQDCGDrawProc: VMQDCGDrawProcPtr;
		theMouseProc: VMMouseProcPtr; theQDMouseProc: VMQDMouseProcPtr; theQDCGMouseProc: VMQDCGMouseProcPtr;
		theKeyProc: VMKeyProcPtr; userData: Pointer); overload;

// ------------------ View Manager -----------------

type
	ViewDataType = Integer;
	TSPointerProcPtr = FUNCTION(theView: HIViewRef; myPtr: Pointer): Boolean;
	// The ViewDataRec is visible for GetCurrentViewData.
	ViewDataRec = record
		window: WindowRef;
		viewControlID: HIViewID;
		
		dataPtr: Pointer;
		dataType: ViewDataType;
		theProc: TSPointerProcPtr;
		filter: EventHandlerUPP;
		
		viewRef: HIViewRef; // Added to support code-generated views 130315
	end;
	ViewDataPtr = ^ViewDataRec;

{ViewDataType values}
const
	kViewDataNone = -1; // No variable should be updated (same as pointer=nil)
	kViewDataString = 0;
	kViewDataLongint = 1;
	kViewDataBoolean = 2;
	kViewDataNumString = 3;
	kViewDataShortString = 4;
//	kViewDataSearchString = 5;
	kViewDataCustom = 5; // Unknown pointer

// View management functions
procedure InstallViewHandlerByRef(theWind: WindowRef; control:ControlRef; theViewDataType: ViewDataType;
		theViewData: Pointer; callback: TSPointerProcPtr; filterProc: EventHandlerUPP);
procedure InstallViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theViewDataType: ViewDataType; theViewData: Pointer; callback: TSPointerProcPtr);

procedure InstallTextViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: AnsiString; callback: TSPointerProcPtr); overload;
procedure InstallNumViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
procedure InstallBooleanViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Boolean; callback: TSPointerProcPtr); overload;
procedure InstallNumTextViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
procedure InstallStr255ViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Str255; callback: TSPointerProcPtr); overload;
procedure InstallTextViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: AnsiString; callback: TSPointerProcPtr); overload;
procedure InstallNumViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
procedure InstallBooleanViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Boolean; callback: TSPointerProcPtr); overload;
procedure InstallNumTextViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
procedure InstallStr255ViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Str255; callback: TSPointerProcPtr); overload;

// For handlers that need more information:
function GetCurrentViewData: ViewDataPtr;
function GetViewDataForView(control: ControlRef): ViewDataPtr;

(*
procedure InstallTextViewHandler(theWind: WindowRef; sig: OSType; id: Longint; var theViewData: AnsiString; callback: TSPointerProcPtr);
procedure InstallNumViewHandler(theWind: WindowRef; sig: OSType; id: Longint; var theViewData: Longint; callback: TSPointerProcPtr);
procedure InstallBooleanViewHandler(theWind: WindowRef; sig: OSType; id: Longint; var theViewData: Boolean; callback: TSPointerProcPtr);
procedure InstallNumTextViewHandler(theWind: WindowRef; sig: OSType; id: Longint; var theViewData: Longint; callback: TSPointerProcPtr);
procedure InstallStr255ViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Str255; callback: TSPointerProcPtr);
*)
		
// Scrollbar/slider helper:
procedure VMInstallDefaultScrollHandler(window:WindowRef; signature:OSType; id:SInt32; buttonStep, pageStep: Integer);overload;
procedure VMInstallDefaultScrollHandler(control: ControlRef; buttonStep, pageStep: Integer); overload;
procedure VMSetScrollSteps(control: ControlRef; buttonStep, pageStep: Integer);

// Tab management (was SmartTabs.pas)
procedure InstallTabHandler(theWind: WindowRef; sig: OSType; id: Longint);
procedure InstallAllTabs(window: WindowRef);

// Image and draw view namagement

type
	TSDrawViewProcPtr = procedure(theView: HIViewRef; cgContext: CGContextRef; viewRect: CGRect);
	VMPointerProcPtr = FUNCTION(theView: HIViewRef; myPtr: Pointer): Boolean;

procedure InstallDrawViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		useQD: Boolean; theDrawProc: TSDrawViewProcPtr);
procedure SetImageViewImage(theWind: WindowRef; sig: OSType; id: Longint; fileName: AnsiString);

// Utilities
function VMGetControl(window: WindowRef; signature: OSType; id: SInt32; Var control: ControlRef ):OSStatus; overload;
function VMGetControl(window: WindowRef; signature: OSType; id: SInt32): ControlRef; overload;
procedure VMSetControlID(control:ControlRef; signature:OSType; id:SInt32);	
function VMStringToNum(s: Str255): Longint;
function VMNumToString(n: Longint): Str255;
function VMGetNumValue(view: ControlRef): Longint; overload;
procedure VMSetNumValue(view: ControlRef; value: Longint); overload;
function VMGetNumStringValue(view: ControlRef): Longint; overload;
procedure VMSetNumStringValue(view: ControlRef; value: Longint); overload;
function VMGetBooleanValue(view: ControlRef): Boolean; overload; overload;
procedure VMSetBooleanValue(view: ControlRef; value: Boolean); overload;
procedure VMToggleBooleanValue(view: ControlRef; value: Boolean); overload;
procedure VMSetStringValue(view: ControlRef; value: AnsiString); overload;
function VMGetStringValue(view: ControlRef): AnsiString; overload;
procedure VMSetStringValue(view: ControlRef; value: CFStringRef); overload;
procedure VMGetStringValue(view: ControlRef; var value: CFStringRef); overload;

function VMGetNumValue(window: WindowRef; signature: OSType; id: SInt32): Longint; overload;
procedure VMSetNumValue(window: WindowRef; signature: OSType; id: SInt32; value: Longint); overload;
function VMGetNumStringValue(window: WindowRef; signature: OSType; id: SInt32): Longint; overload;
procedure VMSetNumStringValue(window: WindowRef; signature: OSType; id: SInt32; value: Longint); overload;
function VMGetBooleanValue(window: WindowRef; signature: OSType; id: SInt32): Boolean; overload;
procedure VMSetBooleanValue(window: WindowRef; signature: OSType; id: SInt32; value: Boolean); overload;
procedure VMToggleBooleanValue(window: WindowRef; signature: OSType; id: SInt32; value: Boolean); overload;
procedure VMSetStringValue(window: WindowRef; signature: OSType; id: SInt32; value: CFStringRef); overload;
procedure VMGetStringValue(window: WindowRef; signature: OSType; id: SInt32; var value: CFStringRef); overload;
procedure VMSetStringValue(window: WindowRef; signature: OSType; id: SInt32; value: AnsiString); overload;
function VMGetStringValue(window: WindowRef; signature: OSType; id: SInt32): AnsiString; overload;

// Added 130411. (Useful and also fairly compatible with TS5.)
function SkelNewButtonInView(parentView: ControlRef; title: String; x,y,w,h: Longint; callback: ControlActionUPP): ControlRef;
function SkelNewButton(parentWindow: WindowPtr; title: String; x,y,w,h: Longint; callback: ControlActionUPP): ControlRef;
function SkelNewTextField(window: WindowPtr; x,y,w,h: Longint; initialText: AnsiString): ControlRef;
function SkelNewStaticTextField(window: WindowPtr; x,y,w,h: Longint; initialText: AnsiString): ControlRef;

type	
	LongPtr = ^Longint;
	AnsiStringPtr = ^AnsiString;
	BooleanPtr = ^Boolean;



// Retina support - cover the window with a view!

type
	TSResizeProcPtr = PROCEDURE(theWind: WindowRef);

procedure SkelWindow(theWind: WindowRef; theDrawProc: VMQDCGDrawProcPtr;
					theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean;
					idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
					customEventTypes: EventTypeSpecPtr; customEventCount: Longint; resizeProc: TSResizeProcPtr);overload;
procedure SkelWindow(theWind: WindowRef; theDrawProc: VMQDCGDrawProcPtr;
					theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean);overload;
function SkelNewWindow(r: QDCG.Rect; title: AnsiString; theDrawProc: VMQDCGDrawProcPtr;
					theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean;
					idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
					customEventTypes: EventTypeSpecPtr; customEventCount: Longint; resizeProc: TSResizeProcPtr): WindowRef;overload;
function SkelNewWindow(r: QDCG.Rect; title: AnsiString): WindowRef;overload;
function SkelGetContentView(theWind: WindowRef): HIViewRef;

function SkelNewWindow(left, top, width, height: Real; title: AnsiString; theDrawProc: VMQDCGDrawProcPtr;
					theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean;
					idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
					customEventTypes: EventTypeSpecPtr; customEventCount: Longint; resizeProc: TSResizeProcPtr): WindowRef;overload;
function SkelNewWindow(left, top, width, height: Real; title: AnsiString): WindowPtr; overload;

// Added 160114, but not tested yet
procedure SkelSetResizeProc(theWind: WindowPtr; theResizeProc: TSResizeProcPtr);

// TS5-like menu functions
procedure SkelCreateMenuBar();
function SkelNewMenu(menuId: Longint; title: AnsiString): MenuRef; overload;
function SkelNewMenu(title: AnsiString): MenuRef; overload;
procedure SkelAppendMenuItem(myMenu: MenuRef; title: AnsiString; commandId: MenuCommand; commandKey: Char); // Optional command?
procedure SkelBuildAppMenu(theMenu: MenuHandle; pSelect: TSIntProcPtr; pClobber: TSMenuProcPtr; DrawBar: Boolean); // Fix the rest
function SkelAlert(msg, okMsg, cancelMgs, altMsg, infoMsg: String): Integer;

// Scrollwheel support
type
	TSScrollProcPtr = procedure (affectedWindow: WindowPtr; axis: EventMouseWheelAxis; wheelDelta: Longint);

procedure SkelSetScrollProc(theWind: WindowPtr; theScrollProc: TSScrollProcPtr);

implementation

const
	kSkelVersion = '4.1.1';

{	Window and Menu handler types, constants, variables.}

{	whList and mhList are the lists of window and menu handlers.}
{	whClobOnRmve and mhClobOnRmve are true if the handler disposal proc}
{	is to be called when a handler is removed.  They are temporarily set}
{	false when handlers are installed for windows or menus that already}
{	have handlers - the old handler is removed WITHOUT calling the}
{	disposal proc.}

{	Default lower limits on window sizing of 80 pixels both directions is}
{	sufficient to allow text windows room to draw a grow box and scroll}
{	bars without having the thumb and arrows overlap.  These values may}
{	be changed if such a constraint is undesirable with SkelGrowBounds.}
{	Default upper limits are for the Macintosh, not the Lisa, but are set}
{	per machine in SkelInit.}

// Dynamic array for event list built with AddEvent
	type
		EventTypeSpecList = array of EventTypeSpec;
	type
	// Fixed-size for custom event list
		EventTypeSpecListArr = array [0..100] of EventTypeSpec;
		EventTypeSpecListPtr = ^EventTypeSpecListArr;

	type
		WHandlerHnd = ^WHandlerPtr;
		WHandlerPtr = ^WHandler;
		WHandler = record
				whWind: WindowPtr;	{window/dialog to be handled	}
				whClobber: TSNoArgProcPtr;	{ data structure disposal proc	}
				whMouse: TSMouseProcPtr;		{ mouse-click handler proc		}
				whKey: TSKeyProcPtr;		{ key-click handler proc			}
				whUpdate: TSBooleanProcPtr;		{ update handler proc				}
				whActivate: TSBooleanProcPtr;	{ activate event handler proc	}
				whClose: TSNoArgProcPtr;		{ close "event" handler proc		}
				whIdle: TSNoArgProcPtr;			{ main loop proc					}
{ $IFC supportDialogs }
				whEvent: TSEventProcPtr;		{ dialog event proc				}
				whFilter: TSFilterProcPtr;		{ dialog filter proc ADDED BY INGEMAR 18/9 -93}
{ $ENDC }
				whHasGrow: Boolean;	{ can window grow?				}
//				whGrow: Rect;			{ limits on window sizing		}
				whSized: Boolean;		{ true = window was resized	}
				whFrontOnly: Boolean;	{ true = idle only when active	}
				whNext: WHandlerHnd;	{ next window handler			}
				whIdleTimer: EventLoopTimerRef; // Timer for whIdle!
				whCustomEvent: TSCarbonEventProcPtr; {Handler for any other Carbon events than the supported ones}
				whHandlerRef: EventHandlerRef; // For uninstalling the events upon subsequent calls

//				whSkelContentView: HIViewRef; {The main view of the window, always covering the whole window.}
				whResizeProc: TSResizeProcPtr;
				whIdleTime: EventTime;
				whScrollProc: TSScrollProcPtr; // Added 160114
//				whCustomEventTypes: EventTypeSpecList; // Remember what custom event that have been specified
				whCustomEventTypes: EventTypeSpecListPtr;
				whCustomEventCount: Longint;
			end;
		
		MHandlerPtr = ^MHandler;
		MHandlerHnd = ^MHandlerPtr;
		MHandler = record
				mhID: integer;				{ menu id									}
				mhSelect: TSIntProcPtr;			{ item selection handler proc			}
				mhClobber: TSMenuProcPtr;		{ menu disposal handler proc			}
				mhNext: MHandlerHnd;		{ next menu handler						}
			end;
			
	var
		whList: WHandlerHnd;				{ list of menu handlers }
		whClobOnRmve: Boolean;
//		growRect: Rect;
		mhList: MHandlerHnd;
		mhClobOnRmve: Boolean;

{	Variables for default Apple menu handler.  appleID is set to 1 if}
{	SkelApple is called and is the id of the Apple menu, appleAboutProc}
{	is the procedure to execute if there is an About... item and it's}
{	chosen from the Apple menu.  If doAbout is true, then the menu}
{	contains the About... item, otherwise it's just desk accessories.}
// OBSOLETE - almost. A typical nib-using app will not use these.

		appleMenu: MenuHandle;
		appleID: integer;
		//appleAboutProc: ProcPtr;
		doAbout: Boolean;

{	"caching" global variables, for speeding up GetWDHandler.}
		oldWindow: WindowPtr;
		oldWDHandler: WHandlerHnd;

{ "command" events (identifier-based menu commands etc) are all passed to this proc: }
		gCarbonMenuCommandProc: TSLongProcPtr;
		
{ Background proc }
		pBkgnd: TSNoArgProcPtr;
		gBackgroundTimer: EventLoopTimerRef;

{ "About" menu selection }
		gAboutProc: TSIntProcPtr; // TSNoArgProcPtr; Takes integer in case you want the classic-style menu management

{	Get handler associated with user or dialog window.}
{	Return nil if window doesn't belong to any known handler.}
{	This routine is absolutely fundamental to TransSkel.}

	function GetWDHandler (theWind: WindowPtr): WHandlerHnd;
		var
			h: WHandlerHnd;
	begin
		h := WhList;
		GetWDHandler := nil;
		if theWind = oldWindow then			{  caching code 	}
			GetWDHandler := oldWDHandler
		else
			while h <> nil do
				if h^^.whWind = theWind then
					begin
						oldWindow := theWind;			{ Load in new values for new window }
						oldWDHandler := h;
						GetWDHandler := h;
						h := nil;
					end
				else
					h := WHandlerHnd(h^^.whNext);
	end;

{ Get Handler associated with user window.  Return nil if window doesn't}
{  have a Handler. }

	function GetWHandler (theWind: WindowPtr): WHandlerHnd;
		var
			h: WHandlerHnd;
	begin
{BUG FIXED by Ingemar 19/9-93. This function retured garbage when passed a dialog}
		h := GetWDHandler(theWind);
		GetWHandler := nil; {default, moved up by Ingemar}
		if h <> nil then
			begin
				if GetWindowKind(theWind) <> dialogKind then
					GetWHandler := h;
			end;
	end;


{	Get handler associated with dialog window.}
{	Return nil if window doesn't belong to any known handler.}

	function GetDHandler (theDialog: WindowPtr): WHandlerHnd;
		var
			h: WHandlerHnd;
	begin
{BUG FIXED by Ingemar 19/9-93. This function retured garbage when passed a non-dialog}
		h := GetWDHandler(theDialog);
		GetDHandler := nil; {default - moved up by Ingemar}
		if h <> nil then
			begin
				if GetWindowKind(theDialog) = dialogKind then
					GetDHandler := h;
			end;
	end;



{ -------------------------------------------------------------------- }
{							Apple Event support							}
{ -------------------------------------------------------------------- }

	var
		gOpenAppProc: TSNoArgProcPtr;
		gOpenDocProc, gPrintProc: TSAEFileProcPtr; {Old, using FSSpec}
		gOpenDocFSProc, gPrintFSProc: TSAEFSFileProcPtr; {New, using FSRef}
		gQuitProc: TSNoArgProcPtr;

	function HandleOpenDoc (var theAE: AppleEvent; var reply: AppleEvent; refcon: LongInt): OSErr; MWPascal;
		var
			myErr: OSErr;						{ errors from the system }
			myDocList: AEDescList;				{ the list of descriptors }
			myFSSpec: FSSpec;					{ an FSSpec record for files  (Change to FRef? See below.) }
			numItems: LONGINT;					{ how many files are there? }
			myKeyword: AEKeyword;				{ ignored -- for AEGetNthPtr }
			myType: DescType;					{ ignored -- real type of data returned }
			realSize: Size;						{ ignored -- real size of data returned }
			count: INTEGER;						{ loop variable counter }
			myFSRef: FSRef;						{ an FSRef for files  (New.) }
	begin
		if (gOpenDocProc = nil) and (gOpenDocFSProc = nil) then
			myErr := errAEEventNotHandled
		else
			begin
				myErr := AEGetParamDesc(theAE, keyDirectObject, typeAEList, myDocList);
				if myErr = noErr then
					begin
						myErr := AECountItems(myDocList, numItems);
						if myErr = noErr then
							begin
								for count := 1 to numItems do
									begin
										if gOpenDocProc <> nil then
										begin
											myErr := AEGetNthPtr(myDocList, count, typeFSS, @myKeyword, @myType, @myFSSpec, sizeof(FSSpec), @realSize);
											if myErr = noErr then
												gOpenDocProc(myFSSpec, count = numItems);
										end;
										
										// New 090117: FSRef-based callback.
										if gOpenDocFSProc <> nil then
										begin
											myErr := AEGetNthPtr(myDocList, count, typeFSRef, @myKeyword, @myType, @myFSRef, sizeof(FSSpec), @realSize);
											if myErr = noErr then
												gOpenDocFSProc(myFSRef, count = numItems);
										end;
										
										{procedure MyOpenDocProc(fs: FSSpec; isLastInBatch: Boolean);}
									end;
							end;
					end;
			end;
		HandleOpenDoc := myErr;
	end; { HandleOpenDoc }

{Almost the same as HandleOpenDoc!}
	function HandlePrintDoc (var theAE: AppleEvent; var reply: AppleEvent; refcon: LongInt): OSErr; MWPascal;
		var
			myErr: OSErr;						{ errors from the system }
			myDocList: AEDescList;				{ the list of descriptors }
			myFSSpec: FSSpec;					{ an FSSpec record for files  (Change to FRef? See below.) }
			numItems: LONGINT;					{ how many files are there? }
			myKeyword: AEKeyword;				{ ignored -- for AEGetNthPtr }
			myType: DescType;					{ ignored -- real type of data returned }
			realSize: Size;						{ ignored -- real size of data returned }
			count: INTEGER;						{ loop variable counter }
			myFSRef: FSRef;						{ an FSRef for files  (New.) }
	begin
		if (gOpenDocProc = nil) and (gOpenDocFSProc = nil) then
			myErr := errAEEventNotHandled
		else
			begin
				myErr := AEGetParamDesc(theAE, keyDirectObject, typeAEList, myDocList);
				if myErr = noErr then
					begin
						myErr := AECountItems(myDocList, numItems);
						if myErr = noErr then
							begin
								for count := 1 to numItems do
									begin
										if gOpenDocProc <> nil then
										begin
											myErr := AEGetNthPtr(myDocList, count, typeFSS, @myKeyword, @myType, @myFSSpec, sizeof(FSSpec), @realSize);
											if myErr = noErr then
												gPrintProc(myFSSpec, count = numItems);
										end;
										
										// New 090117: FSRef-based callback.
										if gOpenDocFSProc <> nil then
										begin
											myErr := AEGetNthPtr(myDocList, count, typeFSRef, @myKeyword, @myType, @myFSRef, sizeof(FSSpec), @realSize);
											if myErr = noErr then
												gPrintFSProc(myFSRef, count = numItems);
										end;
										
										{procedure MyOpenDocProc(fs: FSSpec; isLastInBatch: Boolean);}
									end;
							end;
					end;
			end;
		HandlePrintDoc := myErr;
	end; { HandlePrintDoc }

	function HandleOpenApp (var theAE: AppleEvent; var reply: AppleEvent; refcon: LongInt): OSErr; MWPascal;
	begin
		if gOpenAppProc <> nil then
			gOpenAppProc();
		HandleOpenApp := noErr;
	end;

	function HandleQuit (var theAE: AppleEvent; var reply: AppleEvent; refcon: LongInt): OSErr; MWPascal;
	begin
//WriteLn('Quit Apple Event');

		if gQuitProc = nil then
			SkelWhoa
		else
			gQuitProc();
		HandleQuit := noErr;
	end; {HandleQuit}

	procedure InitTSAE;
	begin
		AEInstallEventHandler(kCoreEventClass, kAEOpenApplication, NewAEEventHandlerUPP(AEEventHandlerProcPtr(@HandleOpenApp)), 0, False);
		AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments, NewAEEventHandlerUPP(AEEventHandlerProcPtr(@HandleOpenDoc)), 0, False);
		AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments, NewAEEventHandlerUPP(AEEventHandlerProcPtr(@HandlePrintDoc)), 0, False);
		AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, NewAEEventHandlerUPP(AEEventHandlerProcPtr(@HandleQuit)), 0, False);
	end;
	
	procedure SkelAppleEvent (openAppProc: TSNoArgProcPtr; openDocProc, printProc: TSAEFileProcPtr; quitProc: TSNoArgProcPtr);
	begin
		gOpenAppProc := openAppProc;
		gOpenDocProc := openDocProc;
		gPrintProc := printProc;
		gQuitProc := quitProc;
	end;
	
	// New 090117: FSRef-based callback.
	procedure SkelAppleEventFS (openAppProc: TSNoArgProcPtr; openDocProc, printProc: TSAEFSFileProcPtr; quitProc: TSNoArgProcPtr); {New version, using FSRef}
	begin
		gOpenAppProc := openAppProc;
		gOpenDocFSProc := openDocProc;
		gPrintFSProc := printProc;
		gQuitProc := quitProc;
	end;
	
{*** End Apple Event support ***}


{*** Input Services keyboard input ***}

//  Table to convert the Mac OS Roman characters 0x80-0xFF to Unicode (not UTF-8).
//  Derived from the table at:  http://alanwood.net/demos/macroman.html
var
	MacRomanToUnicode: array[0..127] of UInt16 =
(
  196  ,  197,  199,  201,  209,  214,  220,  225,  224,  226,  228,  227,  229,  231,   233,   232,
  234  ,  235,  237,  236,  238,  239,  241,  243,  242,  244,  246,  245,  250,  249,   251,   252,
  8224 ,  176,  162,  163,  167, 8226,  182,  223,  174,  169, 8482,  180,  168, 8800,   198,   216,
  8734 ,  177, 8804, 8805,  165,  181, 8706, 8721, 8719,  960, 8747,  170,  186,  937,   230,   248,
  191  ,  161,  172, 8730,  402, 8776, 8710,  171,  187, 8230,  160,  192,  195,  213,   338,   339,
  8211 , 8212, 8220, 8221, 8216, 8217,  247, 9674,  255,  376, 8260, 8364, 8249, 8250, 64257, 64258,
  8225 ,  183, 8218, 8222, 8240,  194,  202,  193,  203,  200,  205,  206,  207,  204,   211,   212,
  63743,  210,  218,  219,  217,  305,  710,  732,  175,  728,  729,  730,  184,  733,   731,   711
);
	UnicodeToMacRoman: array[0..64258] of Char;


// Convert a character from Mac OS Roman text to Unicode.

function ConvertMacCharToUnicode(c: Char): UInt16;
begin
	if Ord(c) >= 128 then
		ConvertMacCharToUnicode := MacRomanToUnicode[Ord(c) - 128]
	else
		ConvertMacCharToUnicode := Ord(c);
end;

var
    layoutData: CFDataRef;
    keyboardLayout: UCKeyboardLayoutPtr;
    currentKeyboard: TISInputSourceRef;

procedure InitUnicodeToMacRomanArray;
var
	i: Longint;
begin
	for i := Low(UnicodeToMacRoman) to High(UnicodeToMacRoman) do
		UnicodeToMacRoman[i] := '?';
	for i := 0 to 127 do
		UnicodeToMacRoman[i] := Char(i);
	for i := Low(MacRomanToUnicode) to High(MacRomanToUnicode) do
		if MacRomanToUnicode[i] >= Low(UnicodeToMacRoman) then
		if MacRomanToUnicode[i] <= High(UnicodeToMacRoman) then
		UnicodeToMacRoman[MacRomanToUnicode[i]] := Char(i + 128)
		else // Missar pga 16-bit overflow
		WriteLn('Can not init with ', i, '(', MacRomanToUnicode[i], ')')
		else
		WriteLn('Can not init with ', i, '(', MacRomanToUnicode[i], ')');

	 currentKeyboard := TISCopyCurrentKeyboardInputSource();
    layoutData := CFDataRef(TISGetInputSourceProperty(currentKeyboard, kTISPropertyUnicodeKeyLayoutData));
    keyboardLayout := UCKeyboardLayoutPtr(CFDataGetBytePtr(layoutData));
end;

function ConvertUnicodeCharToMacRoman(c: UInt16): Char;
begin
	ConvertUnicodeCharToMacRoman := UnicodeToMacRoman[Ord(c)];
//	WriteLn('Converts ', c, ' to ', UnicodeToMacRoman[Ord(c)]);
end;

// New way, using Text Input Services
function SkelGetEventKeyChar(theEvent: EventRef): Char;
var
    keyCode: UInt16;
    keysDown: UInt32;
    chars: UniChar;
    realLength: UniCharCount;
    theMods: UInt32;
begin
     GetEventParameter(theEvent, kEventParamKeyCode, typeUInt16, nil,
   		sizeof(keycode), nil, @keycode);
    
    keysDown := 0; // Shift?
    theMods := GetCurrentEventKeyModifiers;
    
    UCKeyTranslate(keyboardLayout^,
                   keyCode,
                   kUCKeyActionDisplay,
                   theMods shr 8,
                   LMGetKbdType(),
                   kUCKeyTranslateNoDeadKeysBit,
                   keysDown,
                   1,
                   realLength,
                   @chars);
//    CFRelease(currentKeyboard);

    SkelGetEventKeyChar := ConvertUnicodeCharToMacRoman(chars);
end;

{*** end of Input Services input ***}



{For debugging}
function IToString(i: UInt32): Str255;
var
	s: Str255;
begin
	s := '    ';
	BlockMove(@i, @s[1], 4);
	return s;
end;

	procedure DoClobber (h: WHandlerHnd);
		var
			p: TSNoArgProcPtr;
			curPort: MacOSAll.GrafPtr;
			windowPort: CGrafPtr;
	begin
		if (h <> nil) then
			begin
				MacOSAll.GetPort(curPort);
				MacOSAll.SetPortWindowPort(h^^.whWind);
				windowPort := GetWindowPort(h^^.whWind); // Must get this BEFORE calling the clobber proc! - fixed 080415
				p := h^^.whClobber;
				if p <> nil then
					p();
				
				// Remove timer!
				if h^^.whIdleTimer <> nil then
					RemoveEventLoopTimer(h^^.whIdleTimer);

				{Change 040901:}
				{If h^^.whWind = curPort, set the port to something valid!}
				if windowPort = curPort then
				begin
					if FrontWindow <> nil then
						SetPortWindowPort(FrontWindow)
					else
					begin
					{Sadly, Carbon has no Window Manager port!}
{$IFC TARGET_API_MAC_CARBON = FALSE}
						GetCWMgrPort(curPort);
						MacOSAll.SetPort(curPort);
{$ENDC}
					end;
				end
				else
					MacOSAll.SetPort(curPort);
			end;
	end;

// Globals for SkelCallNextEventHandler, see below
	var
		gCurrentEventRef: EventRef;
		gCurrentNextHandler: EventHandlerCallRef;

// New function 081030, for "aborting" event processing and leave it to the next handler.
// Use with caution. It should only be called from inside a TransSkel event handling callback,
// which TransSkel will assume handles the event.
	procedure SkelCallNextEventHandler;
	var
		err: OSErr;
	begin
		if gCurrentEventRef <> nil then
			if gCurrentNextHandler <> nil then
			begin
				err := CallNextEventHandler(gCurrentNextHandler, gCurrentEventRef);
//				WriteLn('SkelCallNextEventHandler called CallNextEventHandler, ', err);
			end;
	end;


procedure PrintEvent(inEvent: EventRef);
var
	eventClass: UInt32;
	eventKind: UInt32;
begin
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	
	case eventClass of
		kEventClassMouse: WriteLn('kEventClassMouse');
		kEventClassKeyboard: WriteLn('kEventClassKeyboard');
		kEventClassTextInput: WriteLn('kEventClassTextInput');
		kEventClassApplication: WriteLn('kEventClassApplication');
		kEventClassAppleEvent: WriteLn('kEventClassAppleEvent');
		kEventClassMenu: WriteLn('kEventClassMenu');
		kEventClassWindow: WriteLn('kEventClassWindow');
		kEventClassControl: WriteLn('kEventClassControl');
		kEventClassCommand: WriteLn('kEventClassCommand');
		kEventClassTablet: WriteLn('kEventClassTablet');
		kEventClassVolume: WriteLn('kEventClassVolume');
		kEventClassAppearance: WriteLn('kEventClassAppearance');
		kEventClassService: WriteLn('kEventClassService');
		kEventClassToolbar: WriteLn('kEventClassToolbar');
		kEventClassToolbarItem: WriteLn('kEventClassToolbarItem');
		kEventClassToolbarItemView: WriteLn('kEventClassToolbarItemView');
		kEventClassAccessibility: WriteLn('kEventClassAccessibility');
		kEventClassSystem: WriteLn('kEventClassSystem');
		kEventClassInk: WriteLn('kEventClassInk');
		kEventClassTSMDocumentAccess: WriteLn('kEventClassTSMDocumentAccess');
		otherwise WriteLn('Unknown event class ', eventClass);
	end;
	
	if eventClass = kEventClassMouse then
	case eventKind of
		kEventMouseDown: WriteLn('kEventMouseDown');
		kEventMouseUp: WriteLn('kEventMouseUp');
		kEventMouseMoved: WriteLn('kEventMouseMoved');
		kEventMouseDragged: WriteLn('kEventMouseDragged');
		kEventMouseEntered: WriteLn('kEventMouseEntered');
		kEventMouseExited: WriteLn('kEventMouseExited');
		kEventMouseWheelMoved: WriteLn('kEventMouseWheelMoved');
//		kEventMouseButtonPrimary: WriteLn('kEventMouseButtonPrimary');
//		kEventMouseButtonSecondary: WriteLn('kEventMouseButtonSecondary');
		kEventMouseButtonTertiary: WriteLn('kEventMouseButtonTertiary');
		kEventMouseWheelAxisX: WriteLn('kEventMouseWheelAxisX');
//		kEventMouseWheelAxisY: WriteLn('kEventMouseWheelAxisY');
		otherwise WriteLn('Unknown mouse event kind ', eventClass);
	end
	else    
	if eventClass = kEventClassWindow then
	case eventKind of
		kEventWindowUpdate: WriteLn('kEventWindowUpdate');
		kEventWindowDrawContent: WriteLn('kEventWindowDrawContent');
		kEventWindowGetClickActivation: WriteLn('kEventWindowGetClickActivation');
		kEventWindowActivated: WriteLn('kEventWindowActivated');
		kEventWindowDeactivated: WriteLn('kEventWindowDeactivated');
		kEventWindowShown: WriteLn('kEventWindowShown');
		kEventWindowHidden: WriteLn('kEventWindowHidden');
		kEventWindowBoundsChanging: WriteLn('kEventWindowBoundsChanging');
		kEventWindowBoundsChanged: WriteLn('kEventWindowBoundsChanged');
		kEventWindowResizeStarted: WriteLn('kEventWindowResizeStarted');
		kEventWindowResizeCompleted: WriteLn('kEventWindowResizeCompleted');
		kEventWindowDragStarted: WriteLn('kEventWindowDragStarted');
		kEventWindowDragCompleted: WriteLn('kEventWindowDragCompleted');
		
		kEventWindowHandleActivate: WriteLn('kEventWindowHandleActivate');
		kEventWindowHandleDeactivate: WriteLn('kEventWindowHandleDeactivate');

		kEventWindowClickDragRgn: WriteLn('kEventWindowClickDragRgn');
		kEventWindowClickResizeRgn: WriteLn('kEventWindowClickResizeRgn');
		kEventWindowClickCollapseRgn: WriteLn('kEventWindowClickCollapseRgn');
		kEventWindowClickCloseRgn: WriteLn('kEventWindowClickCloseRgn');
		kEventWindowClickZoomRgn: WriteLn('kEventWindowClickZoomRgn');
		kEventWindowClickContentRgn: WriteLn('kEventWindowClickContentRgn');
		kEventWindowClickProxyIconRgn: WriteLn('kEventWindowClickProxyIconRgn');
		kEventWindowClickToolbarButtonRgn: WriteLn('kEventWindowClickToolbarButtonRgn');
		kEventWindowClickStructureRgn: WriteLn('kEventWindowClickStructureRgn');
		kEventWindowExpand: WriteLn('kEventWindowExpand');
		kEventWindowClose: WriteLn('kEventWindowClose');
		kEventWindowHandleContentClick: WriteLn('kEventWindowHandleContentClick');
		kEventWindowSheetOpening: WriteLn('kEventWindowSheetOpening');
		kEventWindowSheetOpened: WriteLn('kEventWindowSheetOpened');
		kEventWindowSheetClosing: WriteLn('kEventWindowSheetClosing');
		kEventWindowSheetClosed: WriteLn('kEventWindowSheetClosed');
		kEventWindowDrawerOpening: WriteLn('kEventWindowDrawerOpening');
		kEventWindowDrawerOpened: WriteLn('kEventWindowDrawerOpened');
		kEventWindowDrawerClosing: WriteLn('kEventWindowDrawerClosing');
		kEventWindowDrawerClosed: WriteLn('kEventWindowDrawerClosed');
		kEventWindowDrawGrowBox: WriteLn('kEventWindowDrawGrowBox');
		kEventWindowGetGrowImageRegion: WriteLn('kEventWindowGetGrowImageRegion');
		kEventWindowPaint: WriteLn('kEventWindowPaint');
		otherwise WriteLn('Unknown event kind ', eventKind);
	end
	else
	if eventClass = kEventClassKeyboard then
	case eventKind of
		kEventRawKeyDown: WriteLn('kEventRawKeyDown');
		kEventRawKeyRepeat: WriteLn('kEventRawKeyRepeat');
		kEventRawKeyUp: WriteLn('kEventRawKeyUp');
		kEventRawKeyModifiersChanged: WriteLn('kEventRawKeyModifiersChanged');
		kEventHotKeyPressed: WriteLn('kEventHotKeyPressed');
		kEventHotKeyReleased: WriteLn('kEventHotKeyReleased');
		otherwise WriteLn('Unknown keyboard event kind ', eventClass);
	end
	else
	case eventKind of
		kEventAppActivated: WriteLn('kEventAppActivated');
		kEventAppDeactivated: WriteLn('kEventAppDeactivated');
		kEventAppQuit: WriteLn('kEventAppQuit');
		kEventAppHidden: WriteLn('kEventAppHidden');
		kEventAppShown: WriteLn('kEventAppShown');
		otherwise WriteLn('Unknown event kind ', eventClass);
	end

end;



// Main event handler for a window. Calls your window event callbacks.
	function WindowEventHandler (nextHandler: EventHandlerCallRef; theEvent: EventRef;
							userData: Pointer): OSStatus; MWPascal;
	var
		eventClass: UInt32;
		eventKind: UInt32;
		theTime: EventTime;
		where: MacOSAll.Point;
		key: Char;
		err: OSErr;
		wasHandled: Boolean;
		theMods: Longint; {UInt32?}
		theWind: WindowPtr;
		h, otherWindowHandler: WHandlerHnd;
		curPort: MacOSAll.CGrafPtr;
		
		keyFunc: TSKeyProcPtr;
		
		// For scroll wheel support 160114
		wheelDelta: Longint;
		axis: EventMouseWheelAxis;
		foundWindow: WindowPtr;
	begin
		// For SkelCallNextEventHandler
		gCurrentEventRef := theEvent;
		gCurrentNextHandler := nextHandler;

		eventClass := GetEventClass(theEvent);
		eventKind  := GetEventKind(theEvent);
		
//		PrintEvent(theEvent);
		
		theTime := GetEventTime(theEvent);
		theMods := GetCurrentEventKeyModifiers;
		
		err := GetEventParameter (theEvent, kEventParamDirectObject,
			typeWindowRef, nil, SizeOf(MacOSAll.WindowPtr), nil, @theWind);
		
		if err <> noErr then // Happens for keyboard funcs
		begin
//			WriteLn('eventClass = ', eventClass, ', eventKind = ', eventKind);
			theWind := MacOSAll.FrontWindow;
//			if eventClass <> kEventClassKeyboard then
//				WriteLn('Error getting window: ', err);
		end;
		
		h := GetWDHandler(theWind); // Dialog or Window!
		if h = nil then
		begin
//WriteLn('Event without handler ', Ord(theWind));
			return eventNotHandledErr;
		end;
		wasHandled := false;

//		WriteLn('Event, has handler');

		// Custom event support
		// Can filter an event by returning noErr;
		// Any event that should use standard handling should return something else (e.g. eventNotHandledErr)
		if h^^.whCustomEvent <> nil then
		begin
//WriteLn('Event for window with custom event handler');
			err := h^^.whCustomEvent(nextHandler, theEvent, userData);
			if err = noErr then
				return noErr;
		end;

//		WriteLn('Event, not handled by custom');

		if eventClass = kEventClassKeyboard then
		if (eventKind = kEventRawKeyDown) or (eventKind = kEventRawKeyRepeat) then
		begin
			keyFunc := h^^.whKey;
			if keyFunc <> nil then // If we have a callback installed for the window
			begin
				// Change 140108: Set the window port. Did not work?!
				MacOSAll.GetPort(curPort);
				MacOSAll.SetPortWindowPort(h^^.whWind); // Set the port to theWind so the callee can figure it out
				key := SkelGetEventKeyChar(theEvent);
				MacOSAll.SetPort(curPort);
				keyFunc(key, theMods);
//				err := GetEventParameter (theEvent, kEventParamKeyMacCharCodes,
//					typeChar, nil, SizeOf(Char), nil, @key);
//				key := SkelGetEventKeyChar(theEvent);
//				{h^^.whKey}keyFunc(key, theMods);
				wasHandled := true;
			end;
		end;

// This event not used. I use window content clicks instead.
//		if (eventClass = kEventClassMouse) and (eventKind = kEventMouseDown) then
//		begin
//		end;

		if eventClass = kEventClassWindow then
		begin
			case eventKind of
			kEventWindowClose:
			begin
					if h^^.whClose <> nil then // If we have a callback installed for the window
					begin
						MacOSAll.GetPort(curPort);
						MacOSAll.SetPortWindowPort(h^^.whWind); // Set the port to theWind so the callee can figure it out
						h^^.whClose;
						MacOSAll.SetPort(curPort);
						wasHandled := true;
					end
					else
					begin
// This never happened before, but should work now. 080603
						HideWindow(h^^.whWind);
						wasHandled := true;
						Return noErr;
					end;
			end;
			{kEventWindowUpdate,} kEventWindowDrawContent:
			begin
					if h^^.whUpdate <> nil then // If we have a callback installed for the window
					begin
						MacOSAll.GetPort(curPort);
						MacOSAll.SetPortWindowPort(h^^.whWind);
						
//						if eventKind = kEventWindowUpdate then
//							BeginUpdate(h^^.whWind);
						h^^.whUpdate(h^^.whSized);
						h^^.whSized := false; // Reset - we must have a new resize event to pass true again
//						if eventKind = kEventWindowUpdate then
//							EndUpdate(h^^.whWind);
						MacOSAll.SetPort(curPort);
						wasHandled := true;
					end;
			end;
//			kEventWindowClickDragRgn:
//			begin
//WriteLn('click drag event');
//			end;

			// Activation dead for compositing windows? No... but something is wrong.
			kEventWindowActivated, kEventWindowDeactivated:
//			kEventWindowHandleActivate, kEventWindowHandleDeactivate:
			begin
//WriteLn('activate event');
//				if h <> nil then
					if h^^.whActivate <> nil then // If we have a callback installed for the window
					begin
						MacOSAll.GetPort(curPort);
						MacOSAll.SetPortWindowPort(h^^.whWind);
						h^^.whActivate((eventKind = kEventWindowActivated) or (eventKind = kEventWindowHandleActivate));
						MacOSAll.SetPort(curPort);
						wasHandled := true;
					end;
			end;
			
			kEventWindowBoundsChanged:
			// kEventWindowResizeStarted?
			// kEventWindowResizeCompleted?
			begin
			// Live resize support!
			//WriteLn('kEventWindowBoundsChanged');
					if h^^.whUpdate <> nil then // If we have a callback installed for the window
					begin
						h^^.whSized := true; // Should be cleared at update
						
						MacOSAll.GetPort(curPort);
						MacOSAll.SetPortWindowPort(h^^.whWind);
						h^^.whUpdate(h^^.whSized);
						h^^.whSized := false; // Reset - we must have a new resize event to pass true again
						
						MacOSAll.SetPort(curPort);
						wasHandled := true;
					end;
					if h^^.whResizeProc <> nil then
					begin
						h^^.whResizeProc(h^^.whWind); //, h^^.whSkelContentView); //, h^^.userData);
						wasHandled := true;
					end;
			end;
			
			kEventWindowClickContentRgn:
			begin
				if h^^.whMouse <> nil then // If we have a callback installed for the window
				begin
					err := GetEventParameter (theEvent, kEventParamMouseLocation,
						typeQDPoint, nil, SizeOf(MacOSAll.Point), nil, @where);
					
					MacOSAll.GetPort(curPort);
					MacOSAll.SetPortWindowPort(theWind);
					GlobalToLocal(where);
					
					h^^.whMouse(where, Trunc(theTime*60), theMods); // Hur skall theTime skalas? Eller skall den skickas som double?
					wasHandled := true;
					MacOSAll.SetPort(curPort);
		// Possible improvement: new-style point option?
				end;
			end;
			
			otherwise
//				WriteLn('unknown window event ', eventKind);
				// Call handler for other events here?
			
			end; {case}
		end;

		if eventClass = kEventClassMouse then
		if eventKind = kEventMouseWheelMoved then
		begin
			if GetEventParameter(theEvent, kEventParamMouseWheelDelta, typeLongInteger,
				nil, sizeof(Longint), nil, @wheelDelta ) = noErr then
			begin
				err := GetEventParameter (theEvent, kEventParamMouseLocation,
					typeQDPoint, nil, SizeOf(MacOSAll.Point), nil, @where);
				err := GetEventParameter(theEvent, kEventParamMouseWheelAxis, typeMouseWheelAxis,
					nil, sizeof(EventMouseWheelAxis), nil, @axis);
				err := GetEventParameter(theEvent, kEventParamMouseWheelDelta, typeLongInteger,
								nil, sizeof(Longint), nil, @wheelDelta );
				// But which windows should REALLY be affected?
				FindWindow(where, foundWindow);
				if foundWindow <> nil then
				begin
					otherWindowHandler := GetWHandler(foundWindow); // Get the other window's handler!
					if otherWindowHandler^^.whScrollProc <> nil then
					begin
						otherWindowHandler^^.whScrollProc(foundWindow, axis, wheelDelta);
						wasHandled := true;
					end;
				end;
			end;
		end;
		
//		if not wasHandled then
//			CallNextEventHandler(nextHandler, theEvent);
		
		// For SkelCallNextEventHandler
		gCurrentEventRef := nil;
		gCurrentNextHandler := nil;
		
		if wasHandled then
			WindowEventHandler := noErr
		else
			WindowEventHandler := eventNotHandledErr;
	end; {WindowEventHandler}
	
	{Classic-style menu handler}
	function DoMenuCommand (menu, item: Longint): Boolean;
	var
		mh: MHandlerHnd;
		p: TSIntProcPtr;
	begin
		DoMenuCommand := false; // false indicates no success
//		WriteLn('TRYING MENU');
		
		mh := mhList;
		while (mh <> nil) do
			begin
				p := mh^^.mhSelect;
				if ((menu = mh^^.mhID) and (p <> nil)) then
					begin
//						WriteLn('FOUND MENU');
						p(item);
						mh := nil;
						return true; // Found something
					end
				else
					mh := mh^^.mhNext;
			end;
//		HiliteMenu(0);
	end;

// Temporary - For debugging commands
function LongToString(l: Longint): Str255;
type
	Chars = packed record
	a,b,c,d: Char;
	end;
var
	s: Str255;
begin
	s := '    ';
	s[1] := Chars(l).d;
	s[2] := Chars(l).c;
	s[3] := Chars(l).b;
	s[4] := Chars(l).a;
	return s;
end;
	
	{Menu command and other command events}
	function CommandEventHandler (nextHandler: EventHandlerCallRef;
							theEvent: EventRef;
							userData: Pointer): OSStatus;MWPascal;
	var
		aCommand: HICommand;
		status: OSStatus;
		wasHandled: Boolean;
		menuItem, menuID: Integer;
	begin
		wasHandled := false;
		status := eventNotHandledErr;
		GetEventParameter(theEvent, kEventParamDirectObject, typeHICommand, nil, sizeof(HICommand), nil, @aCommand);

		if (kHICommandFromMenu and aCommand.attributes) <> 0 then
		begin
			menuItem := aCommand.menuItemIndex;
			menuID := GetMenuID(aCommand.menuRef);
			// Look for this in menu handler list.
			wasHandled := DoMenuCommand (menuID, menuItem);
		end;

		if not wasHandled then
		if aCommand.commandID = kHICommandAbout then
		if gAboutProc <> nil then
		begin
			gAboutProc(1);
			wasHandled := true;
		end;
		
		if not wasHandled then
		if gCarbonMenuCommandProc <> nil then
		begin
			{Global callback for all HICommands}
			wasHandled := gCarbonMenuCommandProc(aCommand.commandID);
		end;
		
		if wasHandled then
			status := noErr;
		CommandEventHandler := status;
	end;

	procedure SkelSetCommandProc(theCommandProc: TSLongProcPtr);
	begin
		gCarbonMenuCommandProc := theCommandProc;
	end;

	procedure InstallApplicationCarbonEventHandlers;
	var
		typeList: array [0..2] of EventTypeSpec =
		(
			(eventClass: kEventClassCommand; eventKind: kEventCommandProcess),
			(eventClass: kEventClassMenu; eventKind: kEventMenuTargetItem),
			(eventClass: kEventClassMenu; eventKind: kEventMenuMatchKey) // ???
		);
		err: OSErr;
	begin
		err := InstallEventHandler(GetApplicationEventTarget(), CommandEventHandler, Length(typeList), @typeList[0], nil, nil);
		if  (err <> noErr) Then Halt(err); // Return status;
	end;


procedure Home;
const
	PATH_MAX = 1024;
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
	if not success then Exit(home);
	chdir(path);
//	WriteLn('Current Path: ', path);
end;


// Global ref to main.nib (if any)
	var
		gNibRef: IBNibRef;
		gGotAutoMenu: Boolean;
	
	// SkelInit loads main.nib if it exists, and a menubar called MenuBar.
	// If this is not desired, simply use other names.
	procedure SkelInit;
	var
		err: OSErr;
	begin
		InstallApplicationCarbonEventHandlers;
		SkelAppleEvent(nil, nil, nil, nil); // Onödigt
		InitTSAE;
		
		// By default, try to open main.nib and load MenuBar.
		gNibRef := nil;
		err := CreateNibReference(CFSTR('main'), gNibRef);
		gGotAutoMenu := false;
		if err = noErr then
		begin
			err := SetMenuBarFromNib(gNibRef, CFSTR('MenuBar'));
			gGotAutoMenu := err = noErr;
		end
		else
			WriteLn('No main nib');
		
		Home; // Move to resources folder
		InitUnicodeToMacRomanArray;
	end;
	
	// Get the reference to main.nib, for loading nib resources.
	function SkelGetMainNib: IBNibRef;
	begin
		return gNibRef;
	end;
	
	procedure SkelMain;
	begin
		RunApplicationEventLoop;
	end;
	
	procedure SkelWhoa;
	begin
		QuitApplicationEventLoop;
	end;

{	Clobber all the menu, window and dialog handlers}
	procedure SkelClobber;
	begin
		oldWDHandler := nil;
		oldWindow := nil;
		while (whList <> nil) do
			begin
				SkelRmveWind(whList^^.whWind);
			end;
		while (mhList <> nil) do
			begin
				SkelRmveMenu(GetMenuHandle(mhList^^.mhID));
			end;
		WriteLn('Done Clobbering');
	end;

{-- Menu management --}
{SkelMenu and its cousins should usually not be needed.}
{If you read your menus from nib, and managed with commands,}
{all you need is a command handler callback (SkelSetCommandProc).}
{However, for old code, or code structured by menu or dynamic menus,}
{this is still practical! So use these whenever suitable.}

	function CommonSkelMenu (theMenu: MenuHandle; pSelect: TSIntProcPtr; pClobber: TSMenuProcPtr): Boolean;
		var
			mh: MHandlerHnd;
			myHand: Handle;
	begin
		mhClobOnRmve := false;
		SkelRmveMenu(theMenu);
		mhClobOnRmve := true;
		myHand := NewHandle(Sizeof(MHandler));
		CommonSkelMenu := false;
		if myHand <> nil then
			begin
				CommonSkelMenu := true;					{ show we really got the memory }
				mh := MHandlerHnd(myHand);
				mh^^.mhNext := mhList;
				mhList := MHandlerHnd(myHand);
				mh^^.mhID := GetMenuID(theMenu);	{ get menu id number }
				mh^^.mhSelect := pSelect;			{ install selection handler }
				mh^^.mhClobber := pClobber;		{ install disposal handler }
			end;
	end;

	function SkelMenu (theMenu: MenuHandle; pSelect: TSIntProcPtr; pClobber: TSMenuProcPtr; DrawBar: Boolean): Boolean;
		var
			success: Boolean;
	begin
		success := CommonSkelMenu(theMenu, pSelect, pClobber);
		SkelMenu := success;
		if success then
			begin
				InsertMenu(theMenu, 0);			{ put menu at end of menu bar }
				if DrawBar then
					DrawMenuBar;
			end;
	end;

	function SkelHMenu (theMenu: MenuHandle; pSelect: TSIntProcPtr; pClobber: TSMenuProcPtr): Boolean; {Added by Ingemar 22/8 -93}
		var
			success: Boolean;
	begin
		success := CommonSkelMenu(theMenu, pSelect, pClobber);
		SkelHMenu := success;
		if success then
			begin
				InsertMenu(theMenu, -1);			{ put menu at end of menu bar }
			end;
	end;

	procedure SkelRmveMenu (theMenu: MenuHandle);
		var
			mID: integer;
			h, h2: MHandlerHnd;
			p: TSMenuProcPtr;
			returnflag: Boolean;
	begin
		mID := GetMenuID(theMenu);
		returnflag := false;
		if mhlist <> nil then
			begin
				if mhList^^.mhID = mID then
					begin
						h2 := mhlist;
						mhList := h2^^.mhNext;
					end
				else
					begin
						h := mhList;
						while (h <> nil) and not returnflag do
							begin
								h2 := h^^.mhNext;
								if (h2 = nil) then
									begin
										h := nil;
										returnflag := true;
									end
								else if h2^^.mhID = mID then
									begin
										h^^.mhNext := h2^^.mhNext;
										h := nil;
									end;
								if h <> nil then
									h := h2;
							end;
					end;
				if not returnflag then
					begin
						DeleteMenu(mID);
						DrawMenuBar;
						p := h2^^.mhClobber;
						if mhClobOnRmve and (p <> nil) then
							p(theMenu);
						DisposeHandle(Handle(h2));
					end;
			end;
	end;

	procedure DoAppleItem (item: integer);
	begin
		if doAbout then
			if gAboutProc <> nil then
				gAboutProc(item);
	end;

	procedure DoAppleClobber(ignoredMenu: MenuHandle); // ignoredMenu = appleMenu
	begin
		DisposeMenu(appleMenu);
	end;

	procedure SkelApple (aboutTitle: Str255; aboutProc: TSIntProcPtr);
		var
			appleTitle: Str255;
	begin
		gAboutProc := aboutProc;
		
		// If gNibRef is nil, we can assume that no menu bar has been loaded.
		// If there is a main nib, the program is responsible for creating an Apple menu.
		if not gGotAutoMenu {gNibRef = nil} then
		begin
			WriteLn('No auto-menu, making apple menu');
			
			appleTitle := ' ';
			appleTitle[1] := char($14);
			appleID := 1;
			appleMenu := NewMenu(appleID, appleTitle);
			if aboutTitle <> '' then
				begin
					doAbout := true;
					AppendMenu(appleMenu, aboutTitle);
					AppendMenu(appleMenu, '(-');
				end;
			AppendResMenu(appleMenu, 'DRVR');
			SkelMenu(appleMenu, DoAppleItem, DoAppleClobber, false);
		end;
	end;
	
//	procedure DoWindowTimer (theTimer: EventLoopTimerRef; inAction: EventLoopIdleTimerMessage; userData: Pointer); MWPascal;
	procedure DoWindowTimer (theTimer: EventLoopTimerRef; userData: Pointer); MWPascal;
	var
		hHand: WhandlerHnd;
		savePort: CGrafPtr;
	begin
		{We currently have no mechanism for reporting end of idle time}
//		if inAction = kEventLoopIdleTimerStopped then Exit(DoWindowTimer);
		
		hHand := WhandlerHnd(userData);
		with hHand^^ do
		begin
			if whIdle = nil then
				Exit(DoWindowTimer);
			if whFrontOnly then
				if FrontWindow <> whWind then
					Exit(DoWindowTimer);
			MacOSAll.GetPort(savePort);
			MacOSAll.SetPortWindowPort(whWind);
			whIdle;
			MacOSAll.SetPort(savePort);
		end;
	end;
	
	{Utility function for building a list of events}
	{Assumes that the list is big enough to never overflow!}
	procedure AddEvent(eventClass, eventKind: UInt32; var eventList: EventTypeSpecList);
	begin
		SetLength(eventList, Length(eventList)+1);
		eventList[Length(eventList)-1].eventClass := eventClass;
		eventList[Length(eventList)-1].eventKind := eventKind;
	end;

// Event reinstaller!
	procedure ReinstallEvents(hHand: WhandlerHnd);
	var
		eventList: EventTypeSpecList;
		err: OSErr;
		i: Longint;
		handlerRef: EventHandlerRef;
	begin
// Remove event loop timer, if present
				if hHand^^.whIdleTimer <> nil then
					RemoveEventLoopTimer(hHand^^.whIdleTimer);
				hHand^^.whIdleTimer := nil;
// Remove event handler
				if hHand^^.whHandlerRef <> nil then
					err := RemoveEventHandler(hHand^^.whHandlerRef);

// Install timer
				if hHand^^.whIdle <> nil then
				begin
					InstallEventLoopTimer ( GetMainEventLoop(), hHand^^.whIdleTime, hHand^^.whIdleTime, DoWindowTimer, Pointer(hHand), hHand^^.whIdleTimer);
//					InstallEventLoopIdleTimer (GetMainEventLoop(), idleTime, idleTime,
//						DoWindowTimer, Pointer(hHand), hHand^^.whIdleTimer);
				end
				else
					hHand^^.whIdleTimer := nil;
				
// Inspect all callbacks and install corresponding events
				SetLength(eventList, 0);
								
				if hHand^^.whMouse <> nil then
					AddEvent(kEventClassWindow, kEventWindowClickContentRgn, eventList);
				if hHand^^.whKey <> nil then
					AddEvent(kEventClassKeyboard, kEventRawKeyDown, eventList);
				if hHand^^.whKey <> nil then
					AddEvent(kEventClassKeyboard, kEventRawKeyRepeat, eventList);

				// Always add this? Or only when we have an update callback?
				// if pUpdate <> nil then
				AddEvent(kEventClassWindow, kEventWindowBoundsChanged, eventList);

				if hHand^^.whUpdate <> nil then
				begin
					//AddEvent(kEventClassWindow, kEventWindowUpdate, eventList);
					AddEvent(kEventClassWindow, kEventWindowDrawContent, eventList);
				end;
				if hHand^^.whActivate <> nil then
				begin
					AddEvent(kEventClassWindow, kEventWindowActivated, eventList);
//					AddEvent(kEventClassWindow, kEventWindowHandleActivate, eventList);
				end;
				if hHand^^.whActivate <> nil then
				begin
					AddEvent(kEventClassWindow, kEventWindowDeactivated, eventList);
//					AddEvent(kEventClassWindow, kEventWindowHandleDeactivate, eventList);
				end;
				if hHand^^.whScrollProc <> nil then
				begin
					AddEvent(kEventClassMouse, kEventMouseWheelMoved, eventList);
				end;

//				if pClose <> nil then
// Change by Ingemar 080603: Always install kEventWindowClose, to avoid that the OS disposes the window.
// TransSkel will only close it, so the WindowPtr stays valid. This simplifies thing like About boxes.
					AddEvent(kEventClassWindow, kEventWindowClose, eventList);
	
	// These are the events for the standard procs.
	// It is also possible to pass an array of other events that should
	// be processed by a custom procedure, so any event can be handled.
	// The custom event proc is called before TransSkel's standard processing.
					
	// Add custom list and custom handler, if they exist!
				if hHand^^.whCustomEvent <> nil then
					if hHand^^.whCustomEventCount-1 > 0 then
//						if customEventCount > 0 then
						begin
//							e := EventTypeSpecListPtr(customEventTypes);
							for i := 0 to hHand^^.whCustomEventCount-1 do
							begin
								AddEvent(hHand^^.whCustomEventTypes^[i].eventClass, hHand^^.whCustomEventTypes^[i].eventKind, eventList);
							end;
						end;
				
				err := InstallEventHandler(GetWindowEventTarget(hHand^^.whWind), WindowEventHandler, Length(eventList), @eventList[0], nil, @handlerRef);
				if err <> noErr then
				begin
					WriteLn('InstallEventHandler err ', err);
					if err = eventHandlerAlreadyInstalledErr then
						WriteLn('eventHandlerAlreadyInstalledErr');
				end;
				hHand^^.whHandlerRef := handlerRef;
	end;
	
{Window management}
{New version of SkelWindow, with duration of the timer as well as custom event type list}
// 150727: Split out ReinstallEvents to separate function to make it possible to install events later (more like TS5).
	function SkelCustomWindow (theWind: WindowPtr; pMouse: TSMouseProcPtr;
						pKey: TSKeyProcPtr; pUpdate: TSBooleanProcPtr;
						pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
						pIdle: TSNoArgProcPtr; frontOnly: Boolean;
						idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
						customEventTypes: EventTypeSpecPtr; customEventCount: Longint): Boolean;
// Adds several arguments more over SkelWindow
		var
			hHand: WhandlerHnd;
			i: Integer;
			list: EventTypeSpecListPtr;
	begin
		whClobOnRmve := false;
		SkelRmveWind(theWind);
		whClobOnRmve := true;

{	Get new handler, attach to list of handlers.  It is attached to the beginning of the list, which is simpler;}
{	The order should be irrelevant to the host, anyway. }

		hHand := WHandlerHnd(NewHandle(Sizeof(WHandler)));
		SkelCustomWindow := false;
		if hHand <> nil then
			begin
				hHand^^.whNext := whList;
				whList := hHand;
				with hHand^^ do
					begin
						SkelCustomWindow := true;		{ Show that we got the memory }
						whWind := theWind;
						whMouse := pMouse;
						whKey := pKey;
						whUpdate := pUpdate;
						whActivate := pActivate;
						whClose := pClose;
						whClobber := pClobber;
						whIdle := pIdle;
						whFrontOnly := frontOnly;
						whSized := false;
//						whGrow := GrowRect;
						whIdleTimer := nil;
						whCustomEvent := customEventProc;
						whResizeProc := nil;
						whScrollProc := nil; // Note! All nil-events here will destroy any existing event! Thus, SkelCustomWindow really assumes that it is the first event installer for the window.
						
						whIdleTime := idleTime;
						list := EventTypeSpecListPtr(customEventTypes);
						whCustomEventTypes := EventTypeSpecListPtr(NewPtr(SizeOf(EventTypeSpec) * customEventCount));
						whCustomEventCount := customEventCount;
//						SetLength(whCustomEventTypes, customEventCount);
						for i := 0 to customEventCount-1 do
							whCustomEventTypes^[i] := list^[i];
					end;
				InstallStandardEventHandler(GetWindowEventTarget(theWind));
				hHand^^.whIdleTimer := nil;
				hHand^^.whHandlerRef := nil;				
			// Call event reinstaller above
				ReinstallEvents(hHand);
				MacOSAll.SetPortWindowPort(theWind); {Is this allowed for hidden windows? I thought so, but… /Ingemar, dec 93}
				return true;
			end;
end;


	// Possible improvement:
	// Judging from the callbacks installed for a window, install event handlers
	// This allows handlers to be added or removed any time, so we don't have to set
	// them all at once
//	procedure InstallEvents(theWind: WindowPtr);
//	begin
//	end;
	
	function SkelWindow (theWind: WindowPtr; pMouse: TSMouseProcPtr; pKey: TSKeyProcPtr; pUpdate: TSBooleanProcPtr;
			pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr; pIdle: TSNoArgProcPtr; frontOnly: Boolean): Boolean;
	begin
		return SkelCustomWindow(theWind, pMouse, pKey, pUpdate, pActivate, pClose, pClobber,
							pIdle, frontOnly, kEventDurationSecond, nil, nil, 0);
	end;

	// New name for SkelWindow
	function SkelSetWindowHandler (theWind: WindowPtr; pMouse: TSMouseProcPtr; pKey: TSKeyProcPtr; pUpdate: TSBooleanProcPtr;
			pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr; pIdle: TSNoArgProcPtr; frontOnly: Boolean): Boolean;
	begin
		return SkelCustomWindow(theWind, pMouse, pKey, pUpdate, pActivate, pClose, pClobber,
							pIdle, frontOnly, kEventDurationSecond, nil, nil, 0);
	end;

	procedure SkelRmveWind;
	var
		h, h2: WHandlerHnd;
		returnflag: Boolean;
	begin
		if theWind = oldWindow then
			begin
				oldWindow := nil;
{•    oldWDHandler := nil;•}
			end;

		if (whList <> nil) then
			begin
				returnflag := false;
				if whList^^.whWind = theWind then
					begin
RemoveEventHandler(whList^^.whHandlerRef);
						h2 := whlist;
						whList := whList^^.whNext;
					end
				else
					begin
						h := whList;
						while (h <> nil) and not returnflag do
							begin
								h2 := h^^.whNext;
								if (h2 = nil) then
									begin
										h := nil;
										returnflag := true;
									end
								else if h2^^.whWind = theWind then
									begin
RemoveEventHandler(h2^^.whHandlerRef);
										h^^.whNext := h2^^.whNext;
										h := nil;
									end;
								if h <> nil then
									h := h2;
							end;
					end;
				if not returnflag then
					begin
						if (whClobOnRmve) then
							DoClobber(h2);
						DisposeHandle(Handle(h2));
					end;
			end;
	end;

	function DoDialog (theEvent: EventRecord): Boolean;
		var
			dh: WHandlerHnd;
			theDialog: DialogPtr;
			what: integer;
			item: integer;
			tmpPort: MacOSAll.GrafPtr;
			hasfilter, filtered: Boolean;
	begin
		what := theEvent.what;
{Filter procedure, Added by Ingemar 18/9 -93:}
		if (what = updateEvt) or (what = activateEvt) then
			theDialog := GetDialogFromWindow(WindowPtr(theEvent.message)) { Big bad bug fixed 030712 / Ingemar}
		else
			theDialog := GetDialogFromWindow(FrontWindow);

		dh := WHandlerHnd(GetDHandler(GetDialogWindow(theDialog)));
		filtered := false;
		hasfilter := dh <> nil;
		if hasfilter then
			hasfilter := dh^^.whFilter <> nil;
		if hasfilter then
			dh^^.whFilter(theDialog, theEvent, filtered);
		DoDialog := filtered;
		if not filtered then
{end of filter proc handling}
		begin
			if (what = updateEvt) then // Skall inte behövas!!!
			begin
				MacOSAll.SetPortDialogPort(theDialog);
				DrawDialog(theDialog);
			end;

				if IsDialogEvent(theEvent) then
					begin
						if DialogSelect(theEvent, theDialog, item) then
									begin
										dh := WHandlerHnd(GetDHandler(GetDialogWindow(theDialog)));
										if (dh <> nil) then
											if (dh^^.whEvent <> nil) then
												begin
													MacOSAll.GetPort(tmpPort);
													MacOSAll.SetPortDialogPort(theDialog);
													dh^^.whEvent(item, theEvent);
													MacOSAll.SetPort(tmpPort);
												end;
									end;
						DoDialog := true;
					end
				else
					DoDialog := false;
		end;
	end;

	procedure ConvertWindowClickContentToEventRecord(theEvent: EventRef; var myEvent: EventRecord);
	var
		err: OSErr;
		myWindow: WindowRef;
		myModifiers: Longint;
	begin
		err := GetEventParameter(theEvent, kEventParamDirectObject, typeWindowRef, nil, 
				SizeOf(WindowRef), nil, @myWindow);
		if err = noErr then
		begin
			GetEventParameter(theEvent, 
				kEventParamKeyModifiers, typeUInt32, nil,
				SizeOf(myModifiers), nil, @myModifiers);
			GetEventParameter(theEvent, 
				kEventParamMouseLocation, typeQDPoint, nil,
				SizeOf(MacOSAll.Point), nil, @myEvent.where);

			myEvent.what := mouseDown;
			myEvent.message := Longint(myWindow);
			myEvent.modifiers := myModifiers;
			myEvent.when := Trunc(GetCurrentEventTime * 60); {EventTimeToTicks}
		end
		else
			myEvent.what := nullEvent;
	end;
	
	procedure ConvertDrawContentToEventRecord(theEvent: EventRef; var myEvent: EventRecord);
	var
		err: OSErr;
		myWindow: WindowRef;
		myModifiers: Longint;
	begin
		err := GetEventParameter(theEvent, kEventParamDirectObject, typeWindowRef, nil, 
				SizeOf(WindowRef), nil, @myWindow);
		if err = noErr then
		begin
			GetEventParameter(theEvent, 
				kEventParamKeyModifiers, typeUInt32, nil,
				SizeOf(myModifiers), nil, @myModifiers);
			
			myEvent.what := updateEvt;
			myEvent.message := Longint(myWindow);
			myEvent.modifiers := myModifiers;
			myEvent.when := Trunc(GetCurrentEventTime * 60); {EventTimeToTicks}
		end
		else
			myEvent.what := nullEvent;
	end;
	
	
	// Internal handler of Carbon Events in classic dialogs
	function DialogEventHandler (nextHandler: EventHandlerCallRef; inEvent: EventRef;
							userData: Pointer): OSStatus; MWPascal;
	var
		eventClass: UInt32;
		eventKind: UInt32;
		classicEvent: EventRecord;
		success: Boolean;
	begin
		eventClass := GetEventClass(inEvent);
		eventKind  := GetEventKind(inEvent);
		
//		WriteLn('Dialog event incoming ', IToString(eventClass), ' ', eventKind);
		
		if	((eventClass = kEventClassWindow) and (eventKind = kEventWindowClickContentRgn)) then
		begin
			ConvertWindowClickContentToEventRecord(inEvent, classicEvent);
			DoDialog(classicEvent);
			return noErr;
		end;
		
		if	((eventClass = kEventClassWindow) and (eventKind = kEventWindowDrawContent)) then
		begin
			ConvertDrawContentToEventRecord(inEvent, classicEvent);
			DoDialog(classicEvent);
			return noErr;
		end;
		
		if	((eventClass = kEventClassWindow) and (eventKind = kEventWindowClickContentRgn)) or
			((eventClass = kEventClassKeyboard) and (eventKind = kEventRawKeyDown)) or
			((eventClass = kEventClassKeyboard) and (eventKind = kEventRawKeyRepeat)) or
//			((eventClass = kEventClassWindow) and (eventKind = kEventWindowDrawContent)) or
			((eventClass = kEventClassWindow) and (eventKind = kEventWindowUpdate))
// Why did I use kEventWindowUpdate instead of kEventWindowDrawContent? I don't remember,
// But maybe it doesn't matter. The old-style dialog routines are not vital, pure backwards
// compatibility, so it will not make a big difference.
		then
			begin
				success := ConvertEventRefToEventRecord(inEvent, classicEvent);
				if success then
					DoDialog(classicEvent)
				else
					return eventNotHandledErr;
				return noErr;
			end;
		return eventNotHandledErr;
	end; {DialogEventHandler}

{Dialog management - old style!}
{SkelDialog is for legacy code only. Use HIView (e.g. with ViewManager) for new dialogs.}
	function SkelDialog (theDialog: DialogPtr; pEvent: TSEventProcPtr; pClose, pClobber: TSNoArgProcPtr; pFilter: TSFilterProcPtr): Boolean; {pFilter added by Ingemar 18/9-93}
	var
		wh: WHandlerHnd;
		aBool: Boolean;
	const
		customEventTypes: array [0..4] of EventTypeSpec =
		(
			( eventClass: kEventClassWindow; eventKind: kEventWindowClickContentRgn ),		//	Sent before the tab control switches its control value
			( eventClass: kEventClassKeyboard; eventKind: kEventRawKeyDown ),
			( eventClass: kEventClassKeyboard; eventKind: kEventRawKeyRepeat ),
			( eventClass: kEventClassWindow; eventKind: kEventWindowUpdate ),
			( eventClass: kEventClassWindow; eventKind: kEventWindowDrawContent )
		);
	begin
		aBool := SkelCustomWindow (GetDialogWindow(theDialog), nil, nil, nil,
						nil, pClose, pClobber,
						nil, false, 0, @DialogEventHandler,
						@customEventTypes[0], Length(customEventTypes));
		if aBool then
			WriteLn('Dialog installed');
		
		if aBool <> false then
			begin
				wh := GetWDHandler(GetDialogWindow(theDialog));
				wh^^.whEvent := pEvent;
{Added by Ingemar 18/9 -93:}
				wh^^.whFilter := pFilter; {Install a filter function to be called *before* IsDialogEvent!}
				
				if wh^^.whCustomEvent = nil then
					WriteLn('NIL!!!');
			end;
		SkelDialog := aBool;
	end;

	procedure SkelRmveDlog (theDialog: DialogPtr);
	begin
		SkelRmveWind(GetDialogWindow(theDialog));
	end;

{OBSOLETE?}
	procedure SkelGrowBounds (theWind: WindowPtr; hLO, vLo, hHi, vHi: integer);
	begin
	end;

{OBSOLETE?}
	procedure SkelEventMask (mask: integer);
	begin
	end;

{OBSOLETE?}
	procedure SkelGetEventMask (var mask: integer);
	begin
	end;

	procedure DoBackgroundTimer(theTimer: EventLoopTimerRef; userData: Pointer); MWPascal;
	begin
		if pBkgnd <> nil then
			pBkgnd;
	end;

	procedure SkelBackgroundWithDuration (p: TSNoArgProcPtr; intervalTime: EventTime);
	begin
		// Uninstall any previous timer
		if pBkgnd <> nil then
		if gBackgroundTimer <> nil then
		begin
			RemoveEventLoopTimer(gBackgroundTimer);
			gBackgroundTimer := nil;
		end;

		pBkgnd := p;
		if p <> nil then
		begin
			// Install timer!
			InstallEventLoopTimer (GetMainEventLoop(), intervalTime, intervalTime,
				DoBackgroundTimer, nil, gBackgroundTimer);
		end;
	end;

// The default gives a background process at 1 Hz.
	procedure SkelBackground (p: TSNoArgProcPtr);
	begin
		SkelBackgroundWithDuration(p, kEventDurationSecond);
	end;

	procedure SkelGetBackground (var p: TSNoArgProcPtr);
	begin
		p := pBkgnd;
	end;

{Hook for other events - needs activator and must be processed differently! Unnecessary?}
	procedure SkelEventHook (p: TSOtherEventProcPtr);
	begin
	end;

	procedure SkelGetEventHook (var p: TSOtherEventProcPtr);
	begin
	end;

	procedure SkelDlogMask (mask: integer);
	begin
	end;

	procedure SkelGetDlogMask (var mask: integer);
	begin
	end;

{Two new procedures for WNE-support, added by Ingemar 12/11-93}
	procedure SkelSetSleep (newSleep: Longint);
	begin
	end;

	procedure SkelSetMouseRgn (newMouseRgn: RgnHandle);
	begin
	end;

{Utility function, added by Ingemar 13/7-94}
	function FindWindowByRefcon (theRefCon: Longint): WindowPtr;
	begin
		return nil;
	end;

{Suspend/resume, added by Ingemar 23/7 -94}
	procedure SkelSetSuspendResume (p: TSBooleanProcPtr);
	begin
	end;

	function SkelGetSuspendResume: TSBooleanProcPtr;
	begin
		return nil;
	end;

{Mouse moved events, added by Ingemar jan -95:}
{OBSOLETE?}
	procedure SkelSetMouseMoved (p: TSNoArgProcPtr);
	begin
	end;

	function SkelGetMouseMoved: TSNoArgProcPtr;
	begin
		return nil;
	end;
	
	function SkelGetVersion: AnsiString;
	begin
		return kSkelVersion;
	end;

	procedure SkelInvalWindow(wind: WindowPtr);
	var
		bounds: MacOSAll.Rect;
	begin
		GetWindowPortBounds(wind, bounds);
		InvalWindowRect(wind, bounds);
	end;

// ------------------ SkelView ------------------

// The SkelView, HIView similar to a TransSkel window.
// That means that the API is callback-based, and that a lot
// of repetitive code is eliminated from the interesting code.
// The HIView is typically a user pane.
// Note that you can't draw in mouse and key callbacks. Call HIViewSetNeedsDisplay to redraw.
// One thing is missing so far: The ability to install and handle other events like TransSkel window handlers can.
// Written by Ingemar Ragnemalm march-april 2008
// Version 1.1, april 2009. Added the option of installing a view by pointer instead of sig/id.

// These should not be here but they should be somewhere. Maybe in View Manager.

	function CGRectToRect(cgr: CGRect): MacOSAll.Rect;
	var
		r: MacOSAll.Rect;
	begin
		r.left := Trunc(cgr.origin.x);
		r.top := Trunc(cgr.origin.y);
		r.right := Trunc(r.left + cgr.size.width);
		r.bottom := Trunc(r.top + cgr.size.height);
		return r;
	end;
	
	function RectToCGRect(r: MacOSAll.Rect): CGRect;
	var
		cgr: CGRect;
	begin
		cgr.origin.x := r.left;
		cgr.origin.y := r.top;
		cgr.size.width := r.right - r.left;
		cgr.size.height := r.bottom - r.top;
		return cgr;
	end;	
	
type	
	SkelViewDataRec = record
		window: WindowRef;
//		viewControlID: HIViewID;
		viewControl: ControlRef;
		
		drawProc: TSDrawProcPtr;
		drawQDProc: VMQDDrawProcPtr;
		drawQDCGProc: VMQDCGDrawProcPtr;
		mouseProc: VMMouseProcPtr;
		mouseQDProc: VMQDMouseProcPtr;
		mouseQDCGProc: VMQDCGMouseProcPtr;
		
		// NEW - not yet implemented
//		altMouseProc: VMMouseProcPtr;
//		altMouseQDProc: VMQDMouseProcPtr;
//		altMouseQDCGProc: VMQDCGMouseProcPtr;
// Quick fix: Send alt key to ordinary mouse down!
		
		keyProc: VMKeyProcPtr;
		focused: Boolean;
		
		userDataPtr: Pointer;
	end;
	SkelViewDataPtr = ^SkelViewDataRec;
	
function SkelViewEventHandlerProc(nextHandler: EventHandlerCallRef; inEvent: EventRef;
								inUserData: Pointer ):OSStatus; MWPascal;
var
	myViewDataPtr: SkelViewDataPtr;
	control: ControlRef;
	err: OSErr;
	
// For filtering
	eventClass: UInt32;
	eventKind: UInt32;
	key: Char;
	where: MacOSAll.Point;
	mouseLoc: HIPoint;
	
	tempCG: Boolean;
	savePort, viewPort: MacOSAll.GrafPtr;
	cgc: CGContextRef;
	outRect: CGRect;
	outQDRect: MacOSAll.Rect;
	theMods: Longint;
	dragColor, saveColor: RGBColor;
	inPart: HIViewPartCode;
	qdcgPort: QDCG.GrafPtr;
	mouseBtn: UInt16;
begin
//	WriteLn('Draw view event');
	
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	theMods := GetCurrentEventKeyModifiers;

	gCurrentEventRef := inEvent;
	gCurrentNextHandler := nextHandler;

	// Standard bye-bye
	if eventClass = kEventClassControl then
		if eventClass = kEventControlDispose then
		begin
			DisposePtr(inUserData);
			Return noErr;
		end;
	
	myViewDataPtr := SkelViewDataPtr(inUserData);
	with myViewDataPtr^ do
	begin
		// Hämta ref till view
//		err := GetControlByID( window, viewControlID, control );
		// Är inte detta fånigt? Varför lagrar jag inte det i recorden?
		control := viewControl;
		
		if eventClass = kEventClassControl then
		begin
			if eventKind = kEventControlDraw then
			begin
				// If nobody will handle it, return eventNotHandledErr!
				if (drawProc = nil) and (drawQDProc = nil) and (drawQDCGProc = nil) then
					return eventNotHandledErr;

				MacOSAll.GetPort(savePort);
				tempCG := false;
				if (drawProc = nil) and (drawQDCGProc = nil) then
				begin // no CG callback - use QD
					err := GetEventParameter (inEvent, kEventParamGrafPort,
						typeGrafPtr, nil, SizeOf(MacOSAll.GrafPtr), nil, @viewPort);
					if err = noErr then
						MacOSAll.SetPort(viewPort);
					cgc := nil;
				end
				else
				begin // CG callback, draw with CG
					err := GetEventParameter (inEvent, kEventParamCGContextRef,
						typeCGContextRef, nil, SizeOf(CGContextRef), nil, @cgc);
					if err <> noErr then
					begin
						// No CG parameter available, use QuickDraw port and make CG ref from there
						err := GetEventParameter (inEvent, kEventParamGrafPort,
							typeGrafPtr, nil, SizeOf(MacOSAll.GrafPtr), nil, @viewPort);
						if err <> noErr then
							viewPort := savePort;
						QDBeginCGContext(viewPort, cgc);
				
						tempCG := true;
					end;
				end;
				
				err := HIViewGetBounds(control, outRect);
				if err <> noErr then WriteLn('HIViewGetBounds ', err);
				
				// It the view can accept keyboard input,
				// make room for a focus frame, and draw if focused!
				if keyProc <> nil then
				begin
					//Inset CGRect
					outRect := CGRectInset(outRect, 1, 1);
					
					if focused then
					begin
//						WriteLn('Drawing selection border');
						err := GetThemeBrushAsColor( kThemeBrushFocusHighlight, 32, true, dragColor );
						if cgc = nil then
						begin
							// QD focus frame
							outQDRect := CGRectToRect(outRect);
							MacOSAll.GetForeColor(saveColor);
							MacOSAll.RGBForeColor(dragColor);
							MacOSAll.PenSize(2, 2);
							MacOSAll.FrameRect(outQDRect);
							MacOSAll.RGBForeColor(saveColor);							
							MacOSAll.PenSize(1, 1);
						end
						else
						begin
							// CG focus frame
							CGContextSetRGBStrokeColor( cgc, dragColor.red/65535, dragColor.green/65535,
									dragColor.blue/65535, 1 );
							CGContextSetLineWidth( cgc, 2 );
							CGContextStrokeRect(cgc, outRect);
						end;
					end;
					
					//Inset CGRect
					outRect := CGRectInset(outRect, 1, 1);
				end;

				outQDRect := CGRectToRect(outRect);
		
				// Call drawing callback
				if drawProc <> nil then
					drawProc(control, cgc, outRect, userDataPtr);
				if drawQDProc <> nil then
					drawQDProc(control, outQDRect, userDataPtr);
				if drawQDCGProc <> nil then
				begin
					qdcgPort := CreatePort(cgc, outRect.size.height);
					drawQDCGProc(control, MacRectToRect(outQDRect), userDataPtr);
					FinishPort;
				end;
		
				MacOSAll.SetPort(savePort);
				if tempCG then
					QDEndCGContext(viewPort, cgc);
			end
			else
			if eventKind = kEventControlSetFocusPart then
			begin
//				WriteLn('Got focus');
				GetEventParameter( inEvent, kEventParamControlPart, typeControlPartCode, nil, sizeof( HIViewPartCode ), nil, @inPart );
				case inPart of
					kControlFocusNextPart, kControlFocusPrevPart:
					if myViewDataPtr^.focused then
						return eventNotHandledErr // Can't move to next inside - go somewhere else! (Necessary for tab.)
					else
						myViewDataPtr^.focused := true;
					kControlFocusNoPart:
						myViewDataPtr^.focused := false;
				end;// case
				// Signal redraw:
				HIViewSetNeedsDisplay( control, true); // or HIViewRender for immediate action
			end
			else
			if eventKind = kEventControlClick then
			begin
				// Set focus! Important!
				if keyProc <> nil then
				if not focused then
				begin
//					err := GetControlByID(myViewDataPtr^.window, myViewDataPtr^.viewControlID, control );
					control := myViewDataPtr^.viewControl;
					SetKeyboardFocus(myViewDataPtr^.window, control, kControlFocusNoPart);
					SetKeyboardFocus(myViewDataPtr^.window, control, kControlFocusNextPart);
				end;
				
				// Get mouse location
				err := GetEventParameter(inEvent, kEventParamMouseLocation,
						typeHIPoint, nil, SizeOf(mouseLoc), nil, @mouseLoc);
				
				// Get mouse button(s)
				err := GetEventParameter(inEvent, kEventParamMouseButton,
						typeMouseButton, nil, SizeOf(UInt16), nil, @mouseBtn);
				//WriteLn(mouseBtn);
				// Map alternate buttons to control and option keys!
				// This is not desirable if we need both for different functionality!
				// Thus, I may want to add a separate parameter.
				//if mouseBtn = 2 then
				//	theMods := theMods or optionKey;
				//if mouseBtn = 3 then
				//	theMods := theMods or controlKey;
				
				HIPointConvert( mouseLoc, kHICoordSpace72DPIGlobal, nil, kHICoordSpaceView, control );
				
				where.h := Trunc(mouseLoc.x);
				where.v := Trunc(mouseLoc.y);
				// Port?
//				GlobalToLocal(where);
				if mouseProc <> nil then
					mouseProc(control, mouseLoc, theMods, mouseBtn, userDataPtr)
				else
				if mouseQDProc <> nil then
					mouseQDProc(control, where, theMods, mouseBtn, userDataPtr)
				else
				if mouseQDCGProc <> nil then
					mouseQDCGProc(control, CGPointToPoint(mouseLoc), theMods, mouseBtn, userDataPtr)
				else
					return eventNotHandledErr;
				return eventNotHandledErr;
			end;
		end
		else
		if {(eventClass = kEventClassTextInput) or} (eventClass = kEventClassKeyboard) then
		begin {eventKind = kEventRawKeyDown}
//WriteLn('KEYDOWN!!!');
			if keyProc = nil then
				return eventNotHandledErr; // CallNextEventHandler(nextHandler, inEvent);
			err := GetEventParameter (inEvent, kEventParamKeyMacCharCodes,
					typeChar, nil, SizeOf(Char), nil, @key);
			if Ord(key) = kTabCharCode then
				return eventNotHandledErr; // for tab focusing
			keyProc(control, key, theMods, userDataPtr);
		end;
	end;
	
	return noErr;
end;

procedure InternalInstallSkelViewHandler(theWind: WindowRef; control: ControlRef;
		theDrawProc: TSDrawProcPtr; theQDDrawProc: VMQDDrawProcPtr; theQDCGDrawProc: VMQDCGDrawProcPtr;
		theMouseProc: VMMouseProcPtr; theQDMouseProc: VMQDMouseProcPtr; theQDCGMouseProc: VMQDCGMouseProcPtr;
		theKeyProc: VMKeyProcPtr; userData: Pointer);
var
//	control: ControlRef;
	err: OSStatus;
	myDrawViewDataPtr: SkelViewDataPtr;
const
	viewEvents: array [0..4] of EventTypeSpec =
	(
		( eventClass: kEventClassControl; eventKind: kEventControlDispose ),
		( eventClass: kEventClassControl; eventKind: kEventControlDraw ),
		( eventClass: kEventClassControl; eventKind: kEventControlClick ),
		( eventClass: kEventClassControl; eventKind: kEventControlSetFocusPart ),
//		( eventClass: kEventClassControl; eventKind: kEventControlActivate), // Unnecessary?
//		( eventClass: kEventClassControl; eventKind: kEventControlDeactivate), // Unnecessary?
//		( eventClass: kEventClassControl; eventKind: kEventControlGetClickActivation), // Unnecessary?
//		( eventClass: kEventClassTextInput; eventKind: kEventTextInputUnicodeForKeyEvent), // Unnecessary?
		( eventClass: kEventClassKeyboard; eventKind: kEventRawKeyDown )
// kEventClassMouse are not sent to views! :(
	);
begin
	myDrawViewDataPtr := SkelViewDataPtr(NewPtrClear(SizeOf(SkelViewDataRec)));
	with myDrawViewDataPtr^ do
	begin
		viewControl := control;
		
		window := theWind;
		drawProc := theDrawProc;
		drawQDProc := theQDDrawProc;
		drawQDCGProc := theQDCGDrawProc;
		mouseProc := theMouseProc;
		mouseQDProc := theQDMouseProc;
		mouseQDCGProc := theQDCGMouseProc;
		keyProc := theKeyProc;
		focused := false;
		userDataPtr := userData;
		
		err	:= InstallEventHandler( GetControlEventTarget(control), SkelViewEventHandlerProc, Length(viewEvents), viewEvents, myDrawViewDataPtr, nil );
		if err <> noErr then WriteLn('Failed to install event handler ', err);
		
		HIViewSetNeedsDisplay(control, true);
	end;
end;

procedure InstallSkelViewHandler(theWind: WindowRef; control: ControlRef;
		theDrawProc: TSDrawProcPtr; theMouseProc: VMMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);
		overload;
begin
	InternalInstallSkelViewHandler(theWind, control,
		theDrawProc, nil, nil, theMouseProc, nil, nil, theKeyProc, userData);
end;

procedure InstallQDSkelViewHandler(theWind: WindowRef; control: ControlRef;
		theDrawProc: VMQDDrawProcPtr; theMouseProc: VMQDMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);
		overload;
begin
	InternalInstallSkelViewHandler(theWind, control,
		nil, theDrawProc, nil, nil, theMouseProc, nil, theKeyProc, userData);
end;

procedure InstallQDCGSkelViewHandler(theWind: WindowRef; control: ControlRef;
		theDrawProc: VMQDCGDrawProcPtr; theMouseProc: VMQDCGMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);
		overload;
begin
	InternalInstallSkelViewHandler(theWind, control,
		nil, nil, theDrawProc, nil, nil, theMouseProc, theKeyProc, userData);
end;

procedure InstallSkelViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theDrawProc: TSDrawProcPtr; theMouseProc: VMMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);
		overload;
var
	viewControlID: HIViewID;
	err: OSErr;
	control: ControlRef;
begin
	viewControlID.signature	:= OSType(sig);
	viewControlID.id		:= id;
	err := GetControlByID( theWind, viewControlID, control);
	InternalInstallSkelViewHandler(theWind, control,
		theDrawProc, nil, nil, theMouseProc, nil, nil, theKeyProc, userData);
end;

procedure InstallQDSkelViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theDrawProc: VMQDDrawProcPtr; theMouseProc: VMQDMouseProcPtr; theKeyProc: VMKeyProcPtr; userData: Pointer);
		overload;
var
	viewControlID: HIViewID;
	err: OSErr;
	control: ControlRef;
begin
	viewControlID.signature	:= OSType(sig);
	viewControlID.id		:= id;
	err := GetControlByID( theWind, viewControlID, control );
	InternalInstallSkelViewHandler(theWind, control,
		nil, theDrawProc, nil, nil, theMouseProc, nil, theKeyProc, userData);
end;

// ----------------- View Manager ---------------------

// View Manager, a HIView module managment unit to accompany TransSkel.
// With View Manager, dialog elements (HIViews) can be associated with
// variables. You can have the variables set directly upon changes,
// and you can get notified by a callback. Either or both may be used
// as appropriate.

// This unit includes three parts:
// View manager. Automates handling of controls/views.
// Tabs manager. Handles tabs the way the system shoiuld have done.
// Draw and image view manager. Simplifies draw views and makes it easy to put images in image view.
// View utilities. Simple getters/setters for views.

// View Manager:
// InstallViewHandler installs handling for a control.
// Events arrive to the internal handler StdViewEventHandlerProc,
// which takes needed action.
// The following types are supported:
// Text (AnsiString)
// Integer values (Longint)
// Boolean
// (More variants are being considered!)
// For scrollbars and sliders, you should also use VMInstallDefaultScrollHandler
// to automate them fully.

// Draw and image views:
// These are two special-pupose handlers for displaying an image or
// getting a view to draw in.

// Tabs manager:
// This is a no-worry handler. Just ask VM to handle one or all tabs for
// a window and it will. The only demand is that your tabs are numbered
// correctly. (Doesn't this make you wonder why Apple tells you to do)
// it the hard way?)

// View utilities. This is a bunch of setters and getters that just
// simplify the task of setting or getting the value of a control.

// Version history:
// June 2007, first draft
// 071114: Three bug fixes:
// Range check for SetControlValue,
// better filtering for numerical fields
// SetImageViewImage checks for missing image
// 080309: Added VMInstallDefaultScrollHandler, for even easier scrollbar handling.
// 080625: Some fixes in VMSetStringValue functions, so all use HIViewSetText.
// 090422: Replaced obsolete 16-bit calls by 32-bit.
// 130323: (Back after 4 years!) Added VMSetControlID. Added variant of VMInstallDefaultScrollHandler;
// 130325: Changed VMGetControl to function.
// 130411: Added some control creation functions similar to TS5
// 131213: Added VMSetScrollSteps
// 140603: Added GetCurrentViewData
// 140806: Added GetViewDataForView and a local list to support it.

// Open questions:
// Change name to ViewMaster? :-)
// Prefix VM for all exported functions?
// Add the SkelView view handler for a view handling similar to TransSkel windows.

{ ------------------------ View manager ------------------------- }

var
	viewDataArray: array of ViewDataPtr;

function GetViewDataForView(control: ControlRef): ViewDataPtr;
var
	i: Longint;
begin
	GetViewDataForView := nil;
	for i := 0 to High(viewDataArray) do
	if viewDataArray[i]^.viewRef = control then
	begin
		GetViewDataForView := viewDataArray[i];
		Exit(GetViewDataForView);
	end;
end;

{Data needed for the callback}

var
	currentViewData: ViewDataPtr;

function GetCurrentViewData: ViewDataPtr;
begin
	GetCurrentViewData := currentViewData;
end;

function StdViewEventHandlerProc(nextHandler: EventHandlerCallRef; inEvent: EventRef; inUserData: Pointer ):OSStatus; MWPascal;
var
	myViewDataPtr: ViewDataPtr;
	control: ControlRef;
	err: OSErr;
	
	theString: AnsiString;
	theCFStr: CFStringRef;
	theLong: Longint;
	theBoolean: Boolean;
	localData: Pointer;
	theString255: Str255;

// For filtering
	eventClass: UInt32;
	eventKind: UInt32;
	key: Char;
const
	BS = Char(8);

begin
//	WriteLn('View event');
	
	myViewDataPtr := ViewDataPtr(inUserData);
	currentViewData := myViewDataPtr;
	if myViewDataPtr = nil then
		WriteLn('No view data in StdViewEventHandlerProc');
//		else
//		WriteLn('View data OK in StdViewEventHandlerProc');
	
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	
	// Standard bye-bye (bug fixed 110509)
	if eventClass = kEventClassControl then
		if eventKind = kEventControlDispose then
		begin
			DisposePtr(inUserData);
			Return noErr;
		end;

	with myViewDataPtr^ do
	begin
		// Hämta ref till view
//		err := GetControlByID( window, viewControlID, control );
//		if err <> 0 then
		control := viewRef;
		
// Filter här?	Om man vill påverka det som skall in.
		if filter <> nil then
		begin
			err := filter(nextHandler, inEvent, inUserData);
			if err = noErr then return noErr; // Är detta vettigt?
		end
		else
// Character filter for numerical text fields
		if dataType = kViewDataNumString then
		begin
			// Filtrera icke-numeriska tecken
//			eventClass := GetEventClass(inEvent);
//			eventKind  := GetEventKind(inEvent);
			if eventClass = kEventClassKeyboard then
			begin
				err := GetEventParameter (inEvent, kEventParamKeyMacCharCodes,
					typeChar, nil, SizeOf(Char), nil, @key);
	
				if key in ['0'..'9', '-', BS, Char(31), Char(30), Char(29), Char(28)] then
//				if key in ['0'..'9', '-'] then
				begin
					//WriteLn('I like "', key, '"')
				end
				else
				begin
					//WriteLn('Ignored "', key, '"');
					return noErr; // Trash disallowed characters
				end;
			end;
		end;
(*		if dataType = kViewDataSearchString then
		begin
			if eventClass = kEventClassKeyboard then
			begin
				err := GetEventParameter (inEvent, kEventParamKeyMacCharCodes,
					typeChar, nil, SizeOf(Char), nil, @key);
	
				if key = CR then
				begin
					if theProc <> nil then
						theProc(control, localData);
					
					return noErr; // Trash disallowed characters
				end;
			end;			
		end;*)
	

		// Måste anropa nästa INNAN vi hämtar värdet! (För strängar)
		// Finns filterfunktion så läggs ansvaret på den?
		if filter = nil then
			CallNextEventHandler(nextHandler, inEvent);

		// Hämta värdet som söks
		// GetControlValue eller HIViewCopyText
		case dataType of
			kViewDataString:
			begin
				theCFStr := HIViewCopyText(control);
				theString := CFStringGetCStringPtr(theCFStr, CFStringGetSystemEncoding);
				CFRelease(theCFStr);
				localData := @theString;
			end;
			kViewDataLongint:
			begin
				theLong := GetControl32bitValue(control); // 32bit??? GetControlValue
				localData := @theLong;
			end;
			kViewDataBoolean:
			begin
				theBoolean := GetControlValue(control) <> 0;
				localData := @theBoolean;
			end;
			kViewDataNumString:
			begin
				theCFStr := HIViewCopyText(control);
				theString255 := CFStringGetCStringPtr(theCFStr, CFStringGetSystemEncoding);
				CFRelease(theCFStr);
//				theString255 := theString;
				StringToNum(theString255, theLong);
				localData := @theLong;
			end;
			kViewDataShortString:
			begin
				theCFStr := HIViewCopyText(control);
				theString255 := CFStringGetCStringPtr(theCFStr, CFStringGetSystemEncoding);
				CFRelease(theCFStr);
				localData := @theString255;
			end;
			otherwise
				localData := nil;
		end; {case}
		
		// Lagra i refererad variabel
		if dataPtr <> nil then
			case dataType of
				kViewDataString:
					AnsiStringPtr(dataPtr)^ := theString;
				kViewDataLongint, kViewDataNumString:
					LongPtr(dataPtr)^ := theLong;
				kViewDataBoolean:
					BooleanPtr(dataPtr)^ := theBoolean;
				kViewDataShortString:
					StringPtr(dataPtr)^ := theString255;
			end; {case}

		// Skicka callback
		if theProc <> nil then
		if dataType = kViewDataCustom then // Fix för att kunna få över helt egna data
			theProc(control, dataPtr)
		else
			theProc(control, localData);
			// Möjligen ta emot svar. Mer parametrar? Eventfilteringsmöjligheter?
	end;
//	currentViewData := nil; // No current view outside the callback system!
	
	StdViewEventHandlerProc:=( eventNotHandledErr );
end;


// Lowest-level view handler installer. Note the extra "filter" parameter, missing in other calls.
// It allows custom pre-processing for special-purpose views. Return noErr if the view manager
// should continue as usual.
// BUG NOTICE: This call allocates memory that it can not dispose. Therefore, you should not call
// this several times for the same control. This is a flaw that should be fixed.
procedure InstallViewHandlerByRef(theWind: WindowRef; control:ControlRef; theViewDataType: ViewDataType; theViewData: Pointer; callback: TSPointerProcPtr; filterProc: EventHandlerUPP);
var
	err:OSStatus;
	myViewDataPtr: ViewDataPtr;
	theString: AnsiString;
	theLong: Longint;
	theBoolean: Boolean;
	theCFStr: CFStringRef;
	theString255: Str255;
	found: Boolean;
	i: Longint;
const
	textViewEvents: array [0..6] of EventTypeSpec =
	(
		( eventClass: kEventClassControl; eventKind: kEventControlClick ),		//	Sent before the tab control switches its control value
		( eventClass: kEventClassControl; eventKind: kEventControlHit ),
		( eventClass: kEventClassControl; eventKind: kEventControlValueFieldChanged ), // Fångar det mesta
//		( eventClass: kEventClassControl; eventKind: kEventControlTitleChanged ),
		( eventClass: kEventClassCommand; eventKind: kEventCommandProcess ),
		( eventClass: kEventClassKeyboard; eventKind: kEventRawKeyDown ),
		( eventClass: kEventClassKeyboard; eventKind: kEventRawKeyRepeat ),
		( eventClass: kEventClassControl; eventKind: kEventControlDispose )

		// kEventControlTrack, kEventControlValueFieldChanged, kEventControlTitleChanged
		// kEventControlHit och kEventControlSimulateHit?
		// Lite väl lågnivå: kEventRawKeyDown, kEventRawKeyRepeat
		// Plus tangentbordsevents!
	);
	viewEvents: array [0..0] of EventTypeSpec =
	(
		( eventClass: kEventClassControl; eventKind: kEventControlValueFieldChanged ) // Fångar det mesta
	);
begin
	myViewDataPtr := ViewDataPtr(NewPtrClear(SizeOf(ViewDataRec)));
// Tab panes must have user pane IDs that match the tab number
	with myViewDataPtr^ do
	begin
		window := theWind;
		HIViewGetID(control, viewControlID);
		dataPtr := theViewData;
		dataType := theViewDataType;
		theProc := callback;
		filter := filterProc;
		viewRef := control;// WriteLn('Input control: ', Ord(control));
		
		// Sätt kontrollens värde till värdet som theViewData pekar på.
		if dataPtr <> nil then
		begin
			case theViewDataType of
				kViewDataString:
				begin
					theString := AnsiStringPtr(dataPtr)^;
					theCFStr := CFStringCreateWithBytes(nil, @theString[1], Length(theString), kCFStringEncodingMacRoman, false);
					HIViewSetText(control, theCFStr);
					CFRelease(theCFStr);
				end;
				kViewDataLongint:
				begin
					//SetControlValue
					theLong := LongPtr(dataPtr)^;
					SetControl32BitValue(control, theLong);
				end;
				kViewDataBoolean:
				begin
					//SetControlValue
					theBoolean := BooleanPtr(dataPtr)^;
					SetControl32BitValue(control, Ord(theBoolean));
				end;
				kViewDataNumString:
				begin
					theLong := LongPtr(dataPtr)^;
					NumToString(theLong, theString255);
					theCFStr := CFStringCreateWithBytes(nil, @theString255[1], Length(theString255), kCFStringEncodingMacRoman, false);
					HIViewSetText(control, theCFStr);
					CFRelease(theCFStr);
				end;
				kViewDataCustom:
				begin
//					SetControl32BitValue(control, Longint(dataPtr)); wrong
				end;
			end; {case}
		end;
		
		// Save myViewDataPtr to array
		found := false;
		for i := 0 to High(viewDataArray) do
			if not found then
				if viewDataArray[i]^.viewRef = control then
				begin
					found := true;
					DisposePtr(Ptr(viewDataArray[i]));
					viewDataArray[i] := myViewDataPtr;
				end;
		if not found then
		begin
			SetLength(viewDataArray, Length(viewDataArray)+1);
			viewDataArray[High(viewDataArray)] := myViewDataPtr;
		end;
		
//		err	:= InstallEventHandler( GetControlEventTarget(control), NewEventHandlerUPP( StdTabEventHandlerProc ), Length(tabControlEvents), tabControlEvents, myTabDataPtr, nil );
		if (theViewDataType = kViewDataString) or (theViewDataType = kViewDataNumString) then
			err	:= InstallEventHandler( GetControlEventTarget(control), StdViewEventHandlerProc, Length(textViewEvents), textViewEvents, myViewDataPtr, nil )
		else
			err	:= InstallEventHandler( GetControlEventTarget(control), StdViewEventHandlerProc, Length(viewEvents), viewEvents, myViewDataPtr, nil );
		if err <> noErr then WriteLn('Failed to install event handler ', err);
		
	end;
end;

// Specify a HIView tab by signature and id and install standard handler
procedure InstallViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		theViewDataType: ViewDataType; theViewData: Pointer; callback: TSPointerProcPtr);
var
	tabControlID: HIViewID;
	control:ControlRef;
	err:OSStatus;
	helpStr: Str255;
begin
	tabControlID.signature	:= sig;
	tabControlID.id		:= id;
	err := GetControlByID( theWind, tabControlID, control );
	if err <> noErr then
	begin
		helpStr[0] := Char(4);
		BlockMoveData(@sig, @helpStr[1], 4);
		WriteLn('Failed to get control (InstallViewHandler)', err, ' ', helpStr, ' ', id);
	end
	else
	begin
//		helpStr[0] := Char(4);
//		BlockMoveData(@sig, @helpStr[1], 4);
//		WriteLn('Got control OK (InstallViewHandler) ', helpStr);
		InstallViewHandlerByRef(theWind, control, theViewDataType, theViewData, callback, nil);
	end;
end;


// InstallTextViewHandler
procedure InstallTextViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: AnsiString; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandler(theWind, sig, id, kViewDataString, @theViewData, callback);
end;

// InstallNumViewHandler
procedure InstallNumViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandler(theWind, sig, id, kViewDataLongint, @theViewData, callback);
end;

// InstallBooleanViewHandler
procedure InstallBooleanViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Boolean; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandler(theWind, sig, id, kViewDataBoolean, @theViewData, callback);
end;

// InstallNumTextViewHandler
procedure InstallNumTextViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandler(theWind, sig, id, kViewDataNumString, @theViewData, callback);
end;

// InstallTextViewHandler
procedure InstallStr255ViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		var theViewData: Str255; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandler(theWind, sig, id, kViewDataShortString, @theViewData, callback);
end;

// InstallSearchTextViewHandler
//procedure InstallSearchTextViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
//		var theViewData: AnsiString; callback: TSPointerProcPtr);
//begin
//	InstallViewHandler(theWind, sig, id, kViewDataSearchString, @theViewData, callback);
//end;



// InstallTextViewHandler
procedure InstallTextViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: AnsiString; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandlerByRef(theWind, control, kViewDataString, @theViewData, callback, nil);
end;

// InstallNumViewHandler
procedure InstallNumViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandlerByRef(theWind, control, kViewDataLongint, @theViewData, callback, nil);
end;

// InstallBooleanViewHandler
procedure InstallBooleanViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Boolean; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandlerByRef(theWind, control, kViewDataBoolean, @theViewData, callback, nil);
end;

// InstallNumTextViewHandler
procedure InstallNumTextViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Longint; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandlerByRef(theWind, control, kViewDataNumString, @theViewData, callback, nil);
end;

// InstallTextViewHandler
procedure InstallStr255ViewHandler(theWind: WindowRef; control:ControlRef;
		var theViewData: Str255; callback: TSPointerProcPtr); overload;
begin
	InstallViewHandlerByRef(theWind, control, kViewDataShortString, @theViewData, callback, nil);
end;






// Help for scrollbars and sliders:
// InstallNumViewHandler manages scrollbars great, except for one thing:
// there must be a scroll proc that handles up/down buttons and page up/page down.
// VMInstallDefaultScrollHandler installs a default handler an sets the step values.

const
	kVMStep = 'VmmV';

procedure VMLiveScrollProc (control: ControlHandle; part: SInt16); MWPascal;
var
	currentValue{, min, max, delta}: SInt16;
	steps: MacOSAll.Point;
	err: OSErr;
begin	
	currentValue := GetControl32BitValue (control);
//	min := GetControl32BitMinimum (control);
//	max := GetControl32BitMaximum (control);
//	steps := MacOSAll.Point(GetControlReference(control));

	err := GetControlProperty(control, kVMStep, 
		0, SizeOf(steps.h), nil, @steps.h);
 	if err <> noErr then WriteLn('GetControlProperty ', err);
	err := GetControlProperty(control, kVMStep, 
		1, SizeOf(steps.v), nil, @steps.v);
 	if err <> noErr then WriteLn('GetControlProperty ', err);

//	delta := 0;
	case part of
	kControlUpButtonPart:
		VMSetNumValue(control, currentValue - steps.h);
	kControlDownButtonPart:
		VMSetNumValue(control, currentValue + steps.h);
	kControlPageUpPart:
		VMSetNumValue(control, currentValue - steps.v);
	kControlPageDownPart: 
		VMSetNumValue(control, currentValue + steps.v);
	// Thumb not needed? Seems not.
	otherwise
	end; // case
	
	// Callback not needed - the normal VM handler handles that.
end;
	
procedure VMInstallDefaultScrollHandler(control: ControlRef; buttonStep, pageStep: Integer); overload;
var
//	theControlID: ControlID;
	err: OSErr;
	steps: MacOSAll.Point;
begin 
	SetControlAction (control, VMLiveScrollProc);
	
	steps.h := buttonStep;
	steps.v := pageStep;
	
	SetControlReference(control, Longint(steps)); // Obsolete?

	err := SetControlProperty(control, kVMStep, 
		0,  sizeof(steps.h), @steps.h);
 	if err <> noErr then WriteLn('SetControlProperty ', err);
	err := SetControlProperty(control, kVMStep, 
		1,  sizeof(steps.v), @steps.v);
 	if err <> noErr then WriteLn('SetControlProperty ', err);
end;

// Part of above (most of it - this is really superfluous)
procedure VMSetScrollSteps(control: ControlRef; buttonStep, pageStep: Integer);
var
	steps: MacOSAll.Point;
	err: OSErr;
begin
	steps.h := buttonStep;
	steps.v := pageStep;
	SetControlReference(control, Longint(steps)); // Obsolete?
	err := SetControlProperty(control, kVMStep, 
		0,  sizeof(steps.h), @steps.h);
 	if err <> noErr then WriteLn('SetControlProperty ', err);
	err := SetControlProperty(control, kVMStep, 
		1,  sizeof(steps.v), @steps.v);
 	if err <> noErr then WriteLn('SetControlProperty ', err);
end;

procedure VMInstallDefaultScrollHandler(window:WindowRef; signature:OSType; id:SInt32; buttonStep, pageStep: Integer); overload;
var
	theControlID: ControlID;
	err: OSErr;
	control: ControlRef;
begin
	theControlID.id		:= id;
	theControlID.signature	:= signature;
	err := GetControlByID( window, theControlID, control);
	if err <> noErr then
		WriteLn('VMInstallDefaultScrollHandler failed ', err)
	else
		VMInstallDefaultScrollHandler(control, buttonStep, pageStep);
end;


{ ------------------------ Draw/Image view manager ------------------------- }

//type
//	TSDrawViewProcPtr = procedure(theView: HIViewRef; cgContext: CGContextRef; viewRect: CGRect);


type	
	DrawViewDataRec = record
		window: WindowRef;
		viewControlID: HIViewID;
		
		drawProc: TSDrawViewProcPtr;
		useQDflag: Boolean;
	end;
	DrawViewDataPtr = ^DrawViewDataRec;
	
function StdDrawViewEventHandlerProc(nextHandler: EventHandlerCallRef; inEvent: EventRef; inUserData: Pointer ):OSStatus; MWPascal;
var
	myViewDataPtr: DrawViewDataPtr;
	control: ControlRef;
	err: OSErr;
	
// For filtering
	eventClass: UInt32;
	eventKind: UInt32;
//	key: Char;
	
	tempCG: Boolean;
	savePort, viewPort: MacOSAll.GrafPtr;
	cgc: CGContextRef;
	outRect: CGRect;

begin
//	WriteLn('Draw view event');
	
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	
	// Standard bye-bye
	if eventClass = kEventClassControl then
		if eventKind = kEventControlDispose then
		begin
			DisposePtr(inUserData);
			Return noErr;
		end;	
		
	myViewDataPtr := DrawViewDataPtr(inUserData);
	with myViewDataPtr^ do
	begin
		// Hämta ref till view
		err := GetControlByID( window, viewControlID, control );

		eventClass := GetEventClass(inEvent);
//		eventKind  := GetEventKind(inEvent);
		
		MacOSAll.GetPort(savePort);
		tempCG := false;
		if useQDflag then
		begin
			err := GetEventParameter (inEvent, kEventParamGrafPort,
					typeGrafPtr, nil, SizeOf(MacOSAll.GrafPtr), nil, @viewPort);
			if err = noErr then
				MacOSAll.SetPort(viewPort);
			cgc := nil;
		end
		else
		begin
			err := GetEventParameter (inEvent, kEventParamCGContextRef,
					typeCGContextRef, nil, SizeOf(CGContextRef), nil, @cgc);
			if err <> noErr then
			begin
				// No CG parameter available, use QuickDraw port and make CG ref from there
				err := GetEventParameter (inEvent, kEventParamGrafPort,
						typeGrafPtr, nil, SizeOf(MacOSAll.GrafPtr), nil, @viewPort);
				if err <> noErr then
					viewPort := savePort;
				QDBeginCGContext(viewPort, cgc);
				
				tempCG := true;
			end;
		end;
		
//		HIViewGetFrame(control, outRect);
		err := HIViewGetBounds(control, outRect);
		if err <> noErr then WriteLn('HIViewGetBounds ', err);

		// Skicka callback
		if drawProc <> nil then
			drawProc(control, cgc, outRect);
		
		MacOSAll.SetPort(savePort);
		if tempCG then
			QDEndCGContext(viewPort, cgc);
	end;
	
	return noErr;
end;

procedure InstallDrawViewHandler(theWind: WindowRef; sig: OSType; id: Longint;
		useQD: Boolean; theDrawProc: TSDrawViewProcPtr);
var
//	tabControlID: HIViewID;
	control: ControlRef;
	err: OSStatus;
	myDrawViewDataPtr: DrawViewDataPtr;
const
	viewEvents: array [0..1] of EventTypeSpec =
	(
		( eventClass: kEventClassControl; eventKind: kEventControlDraw ), // Fångar det mesta
		( eventClass: kEventClassControl; eventKind: kEventControlDispose )
	);
begin
	myDrawViewDataPtr := DrawViewDataPtr(NewPtrClear(SizeOf(DrawViewDataRec)));
// Tab panes must have user pane IDs that match the tab number
	with myDrawViewDataPtr^ do
	begin
		viewControlID.signature	:= OSType(sig);
		viewControlID.id		:= id;
		err := GetControlByID( theWind, viewControlID, control );
	
		window := theWind;
//		HIViewGetID(control, viewControlID);
		drawProc := theDrawProc;
		useQDflag := useQD;
		
		err	:= InstallEventHandler( GetControlEventTarget(control), StdDrawViewEventHandlerProc, Length(viewEvents), viewEvents, myDrawViewDataPtr, nil );
		if err <> noErr then WriteLn('Failed to install event handler ', err);
		
		HIViewSetNeedsDisplay(control, true);
	end;
	
end;


// To display a given image (stored in "resources" in the bundle), we only
// need a standard image view and the name of the picture (JPEG or PNG).
// This does ONLY work on image views!
procedure SetImageViewImage(theWind: WindowRef; sig: OSType; id: Longint; fileName: AnsiString);

	function GetExtension(cleanName: Str255): Str255;
	var
		dotPos: Integer;
		n: Longint;
	begin
		GetExtension := '';
		dotPos := 0;
		for n := 1 to Length(cleanName) do
			if cleanName[n] = '.' then
				dotPos := n;
		if dotPos > 0 then
			GetExtension := Copy(cleanName, dotPos, Length(cleanName) - dotPos + 1);
	end; {GetExtension}
var
	control: ControlRef;
	viewControlID: HIViewID;
	err: OSErr;
	fileNameCFStr: CFStringRef;
	theURL: CFURLRef;
	image: CGImageRef;
	provider: CGDataProviderRef;
	ext: AnsiString;
begin
//	WriteLn('Loading ', fileName);
	viewControlID.signature	:= OSType(sig);
	viewControlID.id		:= id;
	err := GetControlByID( theWind, viewControlID, control );
	
	image := nil;
	fileNameCFStr := CFStringCreateWithBytes(nil, @fileName[1], Length(fileName), kCFStringEncodingMacRoman, false);
	theURL := CFBundleCopyResourceURL(CFBundleGetMainBundle, fileNameCFStr, nil, nil);
	if theURL <> nil then
	begin
		provider := CGDataProviderCreateWithURL(theURL);
		if provider <> nil then
		begin
			// Using the file name extension, load JPEG or PNG
			ext := GetExtension(fileName);
			if (ext = '.jpg') or (ext = '.JPG') or (ext = '.jpeg') or (ext = '.JPEG') then
				begin
					image := CGImageCreateWithJPEGDataProvider(provider, nil, 1, kCGRenderingIntentDefault);
				end
			else
			if (ext = '.png') or (ext = '.PNG') then
				begin
					image := CGImageCreateWithPNGDataProvider(provider, nil, 1, kCGRenderingIntentDefault);
				end
			else
				image := CGImageCreateWithJPEGDataProvider(provider, nil, 1, kCGRenderingIntentDefault);
			CGDataProviderRelease(provider);
		end;
		CFRelease(theURL);
	end;
	CFRelease(fileNameCFStr);
	
	if image <> nil then
	begin
		err := HIImageViewSetImage(control, image);
		if err <> noErr then
			WriteLn('HIImageViewSetImage ', err);
		CGImageRelease(image);
	end;
	
	HIViewSetNeedsDisplay(control, true);
end;



{ ------------------------ Tabs manager ------------------------- }


type
	TabDataRec = record
		window: WindowRef;
		tabControlID: HIViewID;
	end;
	TabDataPtr = ^TabDataRec;

procedure ShowHideTabs(myTabDataPtr: TabDataPtr);
var
	tabControl: ControlRef;
//	lastTabIndex: Integer;
	controlValue: Integer;
//	window: WindowRef;
	hv1: HIViewRef;
	paneID: ControlID;
	err: OSStatus;
begin
	with myTabDataPtr^ do
	begin
		err := GetControlByID( window, tabControlID, tabControl );
		if err <> noErr then WriteLn('tab not found');
		controlValue	:= GetControlValue( tabControl );
//		WriteLn('controlValue ', controlValue);

		// Gå igenom dess barn och matcha mot controlValue!
		
		// Loop through all subviews
		hv1 := HIViewGetFirstSubview(tabControl);
		while hv1 <> nil do
		begin
			// Get its ID
			HIViewGetID(hv1, paneID);
//			WriteLn('Subpane ', paneID.id);
		
			// Compare ID to controlValue and set visibility
//			if paneID.id > controlValue then ;
			SetControlVisibility( hv1, paneID.id = controlValue, true );
			
			// Get next
			hv1 := HIViewGetNextView(hv1);
		end;

	end;
end;

{	Event handler for the Tab control}
{	Derrived from sample in O'Reilly's "Learning Carbon" book, page 298}
function StdTabEventHandlerProc( inCallRef:EventHandlerCallRef; inEvent:EventRef; inUserData: Pointer ):OSStatus; MWPascal;
var
	myTabDataPtr: TabDataPtr;
	eventClass: UInt32;
	eventKind: UInt32;
begin
//	WriteLn('Tab event');

	myTabDataPtr := TabDataPtr(inUserData);
	
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	
	// Standard bye-bye
	if eventClass = kEventClassControl then
		if eventKind = kEventControlDispose then
		begin
			DisposePtr(inUserData);
			Return noErr;
		end;

//	window := WindowRef(inUserData);
	ShowHideTabs(myTabDataPtr);
	
	StdTabEventHandlerProc:=( eventNotHandledErr );
end;


procedure InstallTabHandlerByRef(theWind: WindowRef; control:ControlRef);
var
	err:OSStatus;
	myTabDataPtr: TabDataPtr;
const
	tabControlEvents: array [0..3] of EventTypeSpec =
	(
		( eventClass: kEventClassControl; eventKind: kEventControlClick ),		//	Sent before the tab control switches its control value
		( eventClass: kEventClassControl; eventKind: kEventControlHit ),
		( eventClass: kEventClassCommand; eventKind: kEventCommandProcess ),
		( eventClass: kEventClassControl; eventKind: kEventControlDispose )
	);
begin
	myTabDataPtr := TabDataPtr(NewPtrClear(SizeOf(TabDataRec)));
// Tab panes must have user pane IDs that match the tab number
	with myTabDataPtr^ do
	begin
		window := theWind;
		HIViewGetID(control, tabControlID);
	
//		err	:= InstallEventHandler( GetControlEventTarget(control), NewEventHandlerUPP( StdTabEventHandlerProc ), Length(tabControlEvents), tabControlEvents, myTabDataPtr, nil );
		err	:= InstallEventHandler( GetControlEventTarget(control), StdTabEventHandlerProc, Length(tabControlEvents), tabControlEvents, myTabDataPtr, nil );
		if err <> noErr then WriteLn('Failed to install event handler ', err);
		
		// Call it once to hide inactive tabs
		ShowHideTabs(myTabDataPtr);
	end;
end;


// Specify a HIView tab by signature and id and install standard handler
procedure InstallTabHandler(theWind: WindowRef; sig: OSType; id: Longint);
var
	tabControlID: HIViewID;
	control:ControlRef;
	err:OSStatus;
begin
	tabControlID.signature	:= OSType(sig);
	tabControlID.id		:= id;
	err := GetControlByID( theWind, tabControlID, control );
	if err <> noErr then WriteLn('Failed to get control (InstallTabHandler)', err);
	InstallTabHandlerByRef(theWind, control);
end;


// Search all HIViews of a window and install standard handler
procedure InstallAllTabs(window: WindowRef);

	procedure SearchViews(hv: HIViewRef; level: Integer);
	var
		hv1: HIViewRef;
		classID: CFStringRef;
//		i: Integer;
	begin
//		for i := 1 to level do Write('.');
//		WriteLn('SearchViews');
	
		hv1 := HIViewGetFirstSubview(hv);
		while hv1 <> nil do
		begin
			classID := HIObjectCopyClassID(HIObjectRef(hv1));
			if CFStringCompare( CFSTR('com.apple.HITabbedView'), classID, kCFCompareBackwards ) = kCFCompareEqualTo then
				InstallTabHandlerByRef(window, hv1);
//			WriteLn(CFStringGetCStringPtr(classID, CFStringGetSystemEncoding));
			CFRelease(classID);
			
//	pathStr := CFStringGetCStringPtr(pathCFStr, CFStringGetSystemEncoding);

			SearchViews(hv1, level+1);
			hv1 := HIViewGetNextView(hv1);
		end;
	end;

var
	hv: HIViewRef;
begin
	hv := HIViewGetRoot(window);
	
	SearchViews(hv, 0); // Recursive search
end; {InstallAllTabs}



{ ------------------------ View utilities ------------------------- }

{Utilities}
{The utilities are NOT vital. Many are very simple mappings on Get/SetControlValue,}
{HIViewCopyText, HIViewSetText, but collected here with a common interface and some}
{function overloading.}

function VMGetControl( window:WindowRef; signature:OSType; id:SInt32; Var control:ControlRef ):OSStatus; overload;
var
	theControlID: ControlID;
begin 
	theControlID.id		:= id;
	theControlID.signature	:= signature;
	Return( GetControlByID( window, theControlID, control ) );
end;

// This is often more convenient, use the one above for catching errors better
function VMGetControl(window: WindowRef; signature: OSType; id: SInt32): ControlRef; overload;
var
	theControlID: ControlID;
	err: OSErr;
	control: ControlRef;
begin 
	theControlID.id		:= id;
	theControlID.signature	:= signature;
	err := GetControlByID( window, theControlID, control);
	if err <> noErr then WriteLn(err);
	return control;
end;

procedure VMSetControlID(control:ControlRef; signature:OSType; id:SInt32);	
var
	theControlID: ControlID;
begin 
	theControlID.id		:= id;
	theControlID.signature	:= signature;
	SetControlID(control, theControlID);	
end;

function VMStringToNum(s: Str255): Longint;
var
	n: Longint;
begin
	StringToNum(s, n);
	return n;
end;

function VMNumToString(n: Longint): Str255;
var
	s: Str255;
begin
	NumToString(n, s);
	return s;
end;

function VMGetNumValue(view: ControlRef): Longint; overload;
begin
	return GetControl32bitValue(view); // GetControlValue
end;

procedure VMSetNumValue(view: ControlRef; value: Longint); overload;
var
	max, min: Longint;
//	err: OSErr;
begin
// Fix 071114: Don't set value out of bounds
	max := GetControl32BitMaximum(view);
	min := GetControl32BitMinimum(view);
	if value > max then value := max;
	if value < min then value := min;
	SetControl32BitValue(view, value);
//	if err <> noErr then
//		WriteLn('SetControl32BitValue error ', err);
end;

function VMGetNumStringValue(view: ControlRef): Longint; overload;
var
	cf: CFStringRef;
	theString: Str255;
	value: Longint;
begin
	cf := HIViewCopyText(view);
	theString := CFStringGetCStringPtr(cf, CFStringGetSystemEncoding); // Memory leak?
	CFRelease(cf);
	
	StringToNum(theString, value);
	return value;
end;

procedure VMSetNumStringValue(view: ControlRef; value: Longint); overload;
var
	theStr: Str255;
	oldValue: Longint;
begin
	NumToString(value, theStr);
	oldValue := VMGetNumStringValue(view);
	if oldValue <> value then
		VMSetStringValue(view, theStr);
end;

function VMGetBooleanValue(view: ControlRef): Boolean; overload; overload;
begin
	return GetControlValue(view) <> 0;
end;

procedure VMSetBooleanValue(view: ControlRef; value: Boolean); overload;
begin
	SetControl32bitValue(view, Ord(value));
end;

procedure VMToggleBooleanValue(view: ControlRef; value: Boolean); overload;
begin
	SetControl32BitValue(view, Ord(GetControlValue(view) = 0));
end;

procedure VMSetStringValue(view: ControlRef; value: AnsiString); overload;
var
	theCFString: CFStringRef;
	err: OSErr;
begin
	theCFString := CFStringCreateWithBytes(nil, @value[1], Length(value), kCFStringEncodingMacRoman, false);
	err := HIViewSetText(view, theCFString);
	if err <> noErr then
		WriteLn('SetControl32BitValue error ', err);
	CFRelease(theCFString);
end;

function VMGetStringValue(view: ControlRef): AnsiString; overload;
var
	theString: AnsiString;
	cf: CFStringRef;
begin
	cf := HIViewCopyText(view);
	theString := CFStringGetCStringPtr(cf, CFStringGetSystemEncoding); // Memory leak?
	CFRelease(cf);
	return theString;
end;

{ String getter/setter for CFString - trivial}

procedure VMSetStringValue(view: ControlRef; value: CFStringRef); overload;
begin
	HIViewSetText(view, value);
//	Draw1Control( view );	
end;

procedure VMGetStringValue(view: ControlRef; var value: CFStringRef); overload;
begin
	value := HIViewCopyText(view);
end;

// Same utilities as above taking a window/signature/id triplet

function VMGetNumValue(window: WindowRef; signature: OSType; id: SInt32): Longint; overload;
var
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err <> noErr then
		return 0
	else
		return GetControl32bitValue(view); // GetControlValue
end;

procedure VMSetNumValue(window: WindowRef; signature: OSType; id: SInt32; value: Longint); overload;
var
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
		VMSetNumValue(view, value); // With range check
//		SetControl32BitValue(view, value);
end;

function VMGetNumStringValue(window: WindowRef; signature: OSType; id: SInt32): Longint; overload;
var
	theStr: Str255;
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
	cf: CFStringRef;
	value: Longint;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
	begin
		cf := HIViewCopyText(view);
		theStr := CFStringGetCStringPtr(cf, CFStringGetSystemEncoding); // Memory leak?
		CFRelease(cf);
		StringToNum(theStr, value);
		return value;
//		return GetControlValue(view);
	end
	else
		return 0;
end;

procedure VMSetNumStringValue(window: WindowRef; signature: OSType; id: SInt32; value: Longint); overload;
var
	theStr: Str255;
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
	oldValue: Longint;
begin
//WriteLn('VMSetNumStringValue');
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
	begin
		NumToString(value, theStr);
		oldValue := VMGetNumStringValue(view);
		if oldValue <> value then
		begin
			VMSetStringValue(view, theStr);
//			WriteLn('old ', oldValue);
//			WriteLn('new ', value);
		end;
	end;
end;

function VMGetBooleanValue(window: WindowRef; signature: OSType; id: SInt32): Boolean; overload;
var
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
		return GetControlValue(view) <> 0
	else
		return false;
end;

procedure VMSetBooleanValue(window: WindowRef; signature: OSType; id: SInt32; value: Boolean); overload;
var
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
		SetControl32BitValue(view, Ord(value));
end;

procedure VMToggleBooleanValue(window: WindowRef; signature: OSType; id: SInt32; value: Boolean); overload;
var
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
		SetControl32BitValue(view, Ord(GetControlValue(view) = 0));
end;

{ String getter/setter for CFString - trivial}

procedure VMSetStringValue(window: WindowRef; signature: OSType; id: SInt32; value: CFStringRef); overload;
var
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
		HIViewSetText(view, value);
//	Draw1Control( view );	
end;

procedure VMGetStringValue(window: WindowRef; signature: OSType; id: SInt32; var value: CFStringRef); overload;
var
	theControlID: ControlID;
	view: ControlRef;
	err: OSErr;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	err := GetControlByID( window, theControlID, view);
	if err = noErr then
		value := HIViewCopyText(view)
	else
		value := nil;
end;


procedure VMSetStringValue(window: WindowRef; signature: OSType; id: SInt32; value: AnsiString); overload;
var
	theControlID: ControlID;
	view: ControlRef;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	{err :=} GetControlByID( window, theControlID, view);

	VMSetStringValue(view, value);

//	SetControlData( view, 0, kControlStaticTextTextTag, Length(value), @value[1]);
//	Draw1Control( view );	
end;

function VMGetStringValue(window: WindowRef; signature: OSType; id: SInt32): AnsiString; overload;
var
	theControlID: ControlID;
	view: ControlRef;
	theString: AnsiString;
	cf: CFStringRef;
begin
	theControlID.id := id;
	theControlID.signature := signature;
	{err :=} GetControlByID( window, theControlID, view);
//	GetControlDataSize(view, 0, kControlStaticTextTextTag, dataSize); Men funkar denna på edittext?
//	SetLength(theString, dataSize);

	cf := HIViewCopyText(view);
	theString := CFStringGetCStringPtr(cf, CFStringGetSystemEncoding); // Memory leak?
	CFRelease(cf);
	return theString;
end;



// Added 130411

// Made to be similar to TS5
// Untested
function SkelNewButtonInView(parentView: ControlRef; title: String; x,y,w,h: Longint; callback: ControlActionUPP): ControlRef;
var
	r: MacOSAll.Rect;
	button: ControlRef;
begin
	MacOSAll.SetRect(r, x, y, x+w, y+h);
	{err :=} CreatePushButtonControl(nil, r, nil, button);
	SetControlTitle(button, title);
	if callback <> nil then
		SetControlAction(button, callback);
	if parentView <> nil then
		{err :=} HIViewAddSubview (parentView, button);
	EnableControl(button);
	SkelNewButtonInView := button;
end;

function SkelNewButton(parentWindow: WindowPtr; title: String; x,y,w,h: Longint; callback: ControlActionUPP): ControlRef;
var
	r: MacOSAll.Rect;
	button: ControlRef;
	err: OSErr;
begin
	MacOSAll.SetRect(r, x, y, x+w, y+h);
	err := CreatePushButtonControl(parentWindow, r, nil, button);
	if err <> noErr then
		WriteLn(err);
	SetControlTitle(button, title);
	if callback <> nil then
		SetControlAction(button, callback);
//	if parentView <> nil then
//		err := HIViewAddSubview (parentView, button);
	EnableControl(button);
	SkelNewButton := button;

// It was hard to get the content view.
//	HIViewFindByID(HIViewGetRoot(parentWindow), kHIViewWindowContentID, parentView);
//	parentView := HIViewGetFirstSubview(HIViewGetRoot(parentWindow));
//	if parentView = nil then
//		Writeln('Ick');
//	SkelNewButton2 := SkelNewButton(parentView, title, x,y,w,h, callback);
end;

function SkelNewTextField(window: WindowPtr; x,y,w,h: Longint; initialText: AnsiString): ControlRef;
var
	r: MacOSAll.Rect;
	textControl: ControlRef;
	err: OSErr;
begin
	MacOSAll.SetRect(r, x, y, x+w, y+h);
//	err := CreateEditTextControl(window, r, nil, false, false, nil, textControl);
	err := CreateEditUnicodeTextControl(window, r, CFSTR('-'), false {isPassword}, nil, textControl);

	err := SetKeyboardFocus(window, textControl, kControlFocusNextPart);
	SkelNewTextField := textControl;
end;

function SkelNewStaticTextField(window: WindowPtr; x,y,w,h: Longint; initialText: AnsiString): ControlRef;
var
	r: MacOSAll.Rect;
	statTextControl: ControlRef;
	err: OSErr;
begin
	MacOSAll.SetRect(r, x, y, x+w, y+h);
	err := CreateStaticTextControl(window, r, nil, nil, statTextControl);
	VMSetStringValue(statTextControl, initialText);
	SkelNewStaticTextField := statTextControl;
end;




// Retina support - a "skel window" must be a window with a view!
// Asking for a root view must return THAT view! Right?

// SkelGetContentView
// SkelWindow

const
	kHIWindowBitHighResolutionCapable = 21;
	kWindowHighResolutionCapableAttribute = (1 shl
		(kHIWindowBitHighResolutionCapable - 1));


// Install handlers and top SkelView
procedure SkelWindow(theWind: WindowRef; theDrawProc: VMQDCGDrawProcPtr; theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean;
					idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
					customEventTypes: EventTypeSpecPtr; customEventCount: Longint; resizeProc: TSResizeProcPtr);overload;
var
	control: HIViewRef;
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	
	HIViewFindByID(HIViewGetRoot(theWind), kHIViewWindowContentID, control);

(*
	if hdlr <> nil then
	begin
		control := hdlr^^.whSkelContentView;
	end
	else
	begin

	//	GetControlBounds(HIViewGetRoot( theWind ), r);
		GetWindowBounds( theWind, kWindowStructureRgn, rStructure );
		GetWindowBounds( theWind, kWindowContentRgn, rContent );
	//	r.top+= rContent.top - rStructure.top;
		
		r := rContent;
		OffsetRect(r, -rStructure.left, -rStructure.top);

	//	GetControlBounds(HIViewGetRoot( theWind ), r);

		CreateUserPaneControl(theWind, r, {features} 0, control);
		HIViewAddSubview( HIViewGetRoot( theWind ), control);

		// Auto-position
		layout.version := kHILayoutInfoVersionZero;
		layout.binding.top.toView := HIViewGetRoot(theWind);
		layout.binding.top.kind := kHILayoutBindTop;
		layout.binding.left.toView := HIViewGetRoot(theWind);
		layout.binding.left.kind := kHILayoutBindLeft;
		layout.binding.right.toView := HIViewGetRoot(theWind);
		layout.binding.right.kind := kHILayoutBindright;
		layout.binding.bottom.toView := HIViewGetRoot(theWind);
		layout.binding.bottom.kind := kHILayoutBindBottom;
		layout.scale.x.kind := kHILayoutBindNone;
		layout.scale.y.kind := kHILayoutBindNone;
		layout.position.x.kind := kHILayoutBindNone;
		layout.position.y.kind := kHILayoutBindNone;

		HIViewSetLayoutInfo(control, layout);
	//	HIViewApplyLayout(control);

		// Put the view below all existing ones!
		otherControl := HIViewGetFirstSubview(HIViewGetRoot(theWind));
		while otherControl <> nil do
		begin
			WriteLn('SUB');
			HIViewGetBounds (otherControl, outRect);
			WriteLn(outRect.size.height);
			WriteLn(outRect.size.width);
			if outRect.size.height < 100 then
			if outRect.size.width < 100 then
			if otherControl <> control then
				HIViewSetZOrder(control, kHIViewZOrderBelow, otherControl);
			otherControl := HIViewGetNextView(otherControl);
		end;

	end;
*)
	InstallQDCGSkelViewHandler(theWind, control, theDrawProc, theMouseProc, nil, userData);
//pMouse: TSMouseProcPtr;
//					pKey: TSKeyProcPtr
	SkelCustomWindow (theWind, nil, theKeyProc, nil,
					pActivate, pClose, pClobber, pIdle, frontOnly,
					idleTime, customEventProc, customEventTypes, customEventCount);
//	InstallQDCGSkelViewHandler(theWind, control, theDrawProc, theMouseProc, theKeyProc, userData);
//	SkelCustomWindow (theWind, nil, nil, nil,
//					pActivate, pClose, pClobber, pIdle, frontOnly,
//					idleTime, customEventProc, customEventTypes, customEventCount);

	hdlr := GetWHandler (theWind);
	// Store the control!
//	hdlr^^.whSkelContentView := control;
	hdlr^^.whResizeProc := resizeProc;
end;

procedure SkelWindow(theWind: WindowRef; theDrawProc: VMQDCGDrawProcPtr;
					theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean);overload;
begin
	SkelWindow(theWind, theDrawProc, theMouseProc, theKeyProc, userData,
					pActivate, pClose, pClobber, pIdle, frontOnly,
					kEventDurationSecond, nil, nil, 0, nil);
end;

// Create a new window and install handlers and top SkelView
function SkelNewWindow(r: QDCG.Rect; title: AnsiString; theDrawProc: VMQDCGDrawProcPtr;
					theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean;
					idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
					customEventTypes: EventTypeSpecPtr; customEventCount: Longint; resizeProc: TSResizeProcPtr): WindowRef;overload;
var
	theWind: WindowRef;
	theWindowAttributes: WindowAttributes;
begin
	theWindowAttributes := kWindowStandardDocumentAttributes or
		kWindowHighResolutionCapableAttribute
		or kWindowCompositingAttribute;
	CreateNewWindow ( kDocumentWindowClass, theWindowAttributes, RectToMacRect(r), theWind);
	SetWTitle(theWind, title);
	InstallStandardEventHandler(GetWindowEventTarget(theWind));
//	ShowWindow(theWind);	
	SkelNewWindow := theWind;

	SkelWindow(theWind, theDrawProc, theMouseProc, theKeyProc, userData,
					pActivate, pClose, pClobber, pIdle, frontOnly,
					idleTime, customEventProc, customEventTypes, customEventCount, resizeProc);
end;

function SkelNewWindow(r: QDCG.Rect; title: AnsiString): WindowRef;overload;
var
	theWind: WindowRef;
	theWindowAttributes: WindowAttributes;
begin
	theWindowAttributes := kWindowStandardDocumentAttributes or
		kWindowHighResolutionCapableAttribute
		or kWindowCompositingAttribute;
	CreateNewWindow ( kDocumentWindowClass, theWindowAttributes, RectToMacRect(r), theWind);
	SetWTitle(theWind, title);
	InstallStandardEventHandler(GetWindowEventTarget(theWind));
	ShowWindow(theWind);	
	SkelNewWindow := theWind;

	SkelWindow(theWind, nil, nil, nil, nil,
					nil, nil, nil, nil, true,
					kEventDurationSecond, nil, nil, 0, nil);
end;

function SkelNewWindow(left, top, width, height: Real; title: AnsiString; theDrawProc: VMQDCGDrawProcPtr;
					theMouseProc: VMQDCGMouseProcPtr; theKeyProc: TSKeyProcPtr; userData: Pointer;
					pActivate: TSBooleanProcPtr; pClose, pClobber: TSNoArgProcPtr;
					pIdle: TSNoArgProcPtr; frontOnly: Boolean;
					idleTime: EventTime; customEventProc: TSCarbonEventProcPtr;
					customEventTypes: EventTypeSpecPtr; customEventCount: Longint; resizeProc: TSResizeProcPtr): WindowRef;overload;
var
	r: QDCG.Rect;
begin
	SetRect(r, left, top, left+width, top + height);
	SkelNewWindow := SkelNewWindow(r, title, theDrawProc, theMouseProc, theKeyProc, userData,
					pActivate, pClose, pClobber,
					pIdle, frontOnly,
					idleTime, customEventProc,
					customEventTypes, customEventCount, resizeProc);
end;

function SkelNewWindow(left, top, width, height: Real; title: AnsiString): WindowPtr; overload;
var
	r: QDCG.Rect;
begin
	SetRect(r, left, top, left+width, top + height);
	SkelNewWindow := SkelNewWindow(r, title);
end;


function SkelGetContentView(theWind: WindowRef): HIViewRef;
var
	control: HiViewRef;
begin
//	hdlr := GetWHandler (theWind);
//	if hdlr <> nil then
//		SkelGetContentView := hdlr^^.whSkelContentView;
	HIViewFindByID(HIViewGetRoot(theWind), kHIViewWindowContentID, control);
	SkelGetContentView := control;
end;

function SkelGetWindowFromContentView(inView: HIViewRef): WindowRef;
begin
	SkelGetWindowFromContentView := HIViewGetWindow(inView);
end;


// TS5-like menu handling

procedure SkelCreateMenuBar();
begin
	// Does nothing; just conforms with TS5
end;

var
	gMenuId: Integer = 2128; // Default menu id, if we don't care. Beware of collisions!
	
function SkelNewMenu(menuId: Longint; title: AnsiString): MenuRef; overload;
var
	myMenu: MenuRef;
	titleCF: CFStringRef;
begin
	CreateNewMenu (menuId, 0 {MenuAttributes}, myMenu);
	if Length(title) > 0 then
	begin
		titleCF := CFStringCreateWithBytes(nil, @title[1], Length(title), kCFStringEncodingMacRoman, false);
		SetMenuTitleWithCFString(myMenu, titleCF);
		CFRelease(titleCF);
	end;
	SkelNewMenu := myMenu;
end;

function SkelNewMenu(title: AnsiString): MenuRef; overload;
begin
	SkelNewMenu := SkelNewMenu(gMenuId, title);
	gMenuId += 1;
end;

procedure SkelAppendMenuItem(myMenu: MenuRef; title: AnsiString; commandId: MenuCommand; commandKey: Char); // Optional command?
var
	titleCF: CFStringRef;
	item: MenuItemIndex;
begin
	if Length(title) > 0 then
	begin
		titleCF := CFStringCreateWithBytes(nil, @title[1], Length(title), kCFStringEncodingMacRoman, false);
		AppendMenuItemTextWithCFString( myMenu, titleCF, 0, 0, @item);
		CFRelease(titleCF);
	end;
	SetMenuItemCommandID(myMenu, item, commandId);
	SetMenuItemCommandKey(myMenu, item, false, UInt16(commandKey));
end;

procedure SkelBuildAppMenu(theMenu: MenuHandle; pSelect: TSIntProcPtr; pClobber: TSMenuProcPtr; DrawBar: Boolean); // Fix the rest
begin
	// SkelMenu clone to conform with TS5.
	SkelMenu (theMenu, pSelect, pClobber, DrawBar);
end;


function SkelAlert(msg, okMsg, cancelMgs, altMsg, infoMsg: String): Integer;
var
	theDialog: DialogPtr;
	itemHit: DialogItemIndex;
	param: AlertStdCFStringAlertParamRec;
var
	msgCFStr, explCFStr, okCFStr, cancelCFStr, altCFStr: CFStringRef;
begin
	msgCFStr := CFStringCreateWithBytes(nil, @msg[1], Length(msg), kCFStringEncodingMacRoman, false);
	if Length(infoMsg) > 0 then
		explCFStr := CFStringCreateWithBytes(nil, @infoMsg[1], Length(infoMsg), kCFStringEncodingMacRoman, false)
	else
		explCFStr := nil;
	okCFStr := CFStringCreateWithBytes(nil, @okMsg[1], Length(okMsg), kCFStringEncodingMacRoman, false);
	if Length(cancelMgs) > 0 then
		cancelCFStr := CFStringCreateWithBytes(nil, @cancelMgs[1], Length(cancelMgs), kCFStringEncodingMacRoman, false)
	else
		cancelCFStr := nil;
	if Length(altMsg) > 0 then
		altCFStr := CFStringCreateWithBytes(nil, @altMsg[1], Length(altMsg), kCFStringEncodingMacRoman, false)
	else
		altCFStr := nil;
	param.version := kStdCFStringAlertVersionOne;
	param.movable := true;
	param.helpButton := false;
	param.flags := 0;
	param.position := kWindowDefaultPosition;
	param.cancelButton := 2;
	param.defaultButton := 1;
	param.defaultText := okCFStr;
	param.otherText := altCFStr; // CFSTR('ALT');
	param.cancelText := cancelCFStr;
	
	CreateStandardAlert(kAlertCautionAlert, msgCFStr, explCFStr, @param, theDialog);
	RunStandardAlert(theDialog, nil, itemHit);

	CFRelease(msgCFStr);
	if explCFStr <> nil then
		CFRelease(explCFStr);
	if okCFStr <> nil then
		CFRelease(okCFStr);
	if cancelCFStr <> nil then
		CFRelease(cancelCFStr);
	if altCFStr <> nil then
		CFRelease(altCFStr);

	SkelAlert := itemHit;
end;

// Added 160114
procedure SkelSetScrollProc(theWind: WindowPtr; theScrollProc: TSScrollProcPtr);
var
	hHand: WhandlerHnd;
begin
	hHand := GetWHandler(theWind);
	if hHand <> nil then
		begin
			hHand^^.whScrollProc := theScrollProc;
			ReinstallEvents(hHand);
		end;
end;

// Added 160114, but not tested yet
procedure SkelSetResizeProc(theWind: WindowPtr; theResizeProc: TSResizeProcPtr);
var
	hHand: WhandlerHnd;
begin
	hHand := GetWHandler(theWind);
	if hHand <> nil then
		begin
			hHand^^.whResizeProc := theResizeProc;
			ReinstallEvents(hHand);
		end;
end;


end.










// All these should be added to conform better with TS5
// But do I care? With the new content views, it becomes even hairier.
// The same calls are needed for views!
// Should a proc go to the window or the view?
// Surely we also need ReinstallEvents for these unless a proc was already there?
// And we need a reinstall for views as well.
procedure SkelSetWindowMouseHandler(theWind: WindowRef; theMouseProc: VMQDCGMouseProcPtr);
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.whMouse := theMouseProc;
	end;
end;

procedure SkelSetWindowDrawHandler(theWind: WindowRef; theDrawProc: VMQDCGDrawProcPtr);
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.whUpdate := theDrawProc;
	end;
end;

procedure SkelSetWindowKeyDownHandler(theWind: WindowRef; theKeyProc: VMKeyProcPtr);
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.whKey := theKeyProc;
	end;
end;

procedure SkelSetWindowActivateProc(theWind: WindowRef; pActivate: TSBooleanProcPtr);
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.whActivate := pActivate;
	end;
end;

procedure SkelSetWindowUserData(theWind: WindowRef; userData: Pointer);
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.userData := userData;
	end;
end;

procedure SkelSetWindowRightMouseHandler(theWind: WindowRef; altMouseProc: SkelMouseProcPtr);overload;
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.??? := altMouseProc;
	end;
end;

procedure SkelSetWindowMouseUpHandler(theWind: WindowRef; mouseUpProc: SkelMouseProcPtr);overload;
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.??? := mouseUpProc;
	end;
end;

procedure SkelSetWindowRightMouseUpHandler(theWind: WindowRef; altMouseUpProc: SkelMouseProcPtr);overload;
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.??? := altMouseUpProc;
	end;
end;

//procedure SkelSetWindowMouseEnterExitHandlers(theWind: WindowRef; mouseEnterProc, mouseExitProc: SkelMouseProcPtr);overload;
//procedure SkelSetWindowMouseMovedHandler(theWind: WindowRef; mouseMovedProc: SkelMouseProcPtr);overload;
//procedure SkelSetWindowMouseDraggedHandler(theWind: WindowRef; mouseDraggedProc: SkelMouseProcPtr);overload;

procedure SkelSetWindowKeyUpHandler(theWind: WindowRef; keyUpProc: SkelKeyProcPtr);
var
	hdlr: WHandlerHnd;
begin
	hdlr := GetWHandler (theWind);
	if hdlr <> nil then
	begin
		hdlr^^.??? := keyUpProc;
	end;
end;

//procedure SkelSetWindowScrollWheelHandler(theWind: WindowRef; scrollWheelProc: ProcPtr);

// Set custom events? idletime? idle proc?
// Close proc? Halt proc?
// Resize proc!

end.
