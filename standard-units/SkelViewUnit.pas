// Part of TransSkel5, implements callback-driven handling of custom views.
// 120423: Straightened up the names of event callback installers, to SkelSet*
// instead of InstallSkel*. This is more in the line of the rest of TransSkel.
// 131002: Added alternate declaration for SkelNewView, with an NSRect (conforming
// with several calls in the main TransSkel).
// 131116: Added the "SkelQDCGDrawProcPtr" callback type for drawing handlers that
// explicitly use QDCG. Thereby all CG can be hidden.
// 140623: Callback installers spit up as one call for each, which makes the code
// cleaner IMHO.
// 150821: Fixed QDCG callback handling for most events. (Many not tested yet.)
// 160525: SkelNewView now has a variant taking QDCG.Rect.

{$mode objfpc}
{$modeswitch objectivec1}

unit SkelViewUnit;
interface
uses
	CocoaAll, MacOSAll, QDCG;

type
	SkelViewPtr = ^SkelView;
	SkelDrawProcPtr = procedure(theView: NSView; cgContext: CGContextRef; viewRect: CGRect; userData: Pointer);
	SkelQDCGDrawProcPtr = procedure(theView: NSView; viewRect: QDCG.Rect; userData: Pointer);
	SkelMouseProcPtr = procedure(theView: NSView; where: HIPoint; mods: Longint; userData: Pointer);
	SkelQDCGMouseProcPtr = procedure(theView: NSView; where: QDCG.Point; mods: Longint; userData: Pointer);
	SkelKeyProcPtr = procedure(theView: NSView; key: Char; mods: Longint; userData: Pointer);
	SkelClobberWindowProc = procedure(theWindow: NSWindow; userData: Pointer);
	SkelCloseWindowProc = function(theWindow: NSWindow; userData: Pointer): Longint; {0 = Undecided, ask again later. 1 = OK to close. -1 = cancel.}
		{One more? - ignore this, don't bother but don't close until quit. Good for settings etc.}
		{Not necessary - can be done by not releasing on close?}
		{That is, as long as it is not deleted from TS list of windows!}
	
	SkelView = objcclass (NSView)
	public
//		backgroundColor: NSColor;
		gRed, gGreen, gBlue, gAlpha: Real;
		lastTrack: NSTrackingRectTag;
		
		drawProc: SkelDrawProcPtr;
		drawQDCGProc: SkelQDCGDrawProcPtr;
		mouseProc: SkelMouseProcPtr;
		mouseQDCGProc: SkelQDCGMouseProcPtr;
		keyProc: SkelKeyProcPtr;
		focused: Boolean;
		userDataPtr: Pointer;
		flipped: Boolean;
		
		mouseUpQDCGProc, rightMouseQDCGProc, rightMouseUpQDCGProc: SkelQDCGMouseProcPtr;
		mouseUpProc, rightMouseProc, rightMouseUpProc: SkelMouseProcPtr;
		
		// All the following are hardly tested yet!
		keyUpProc: SkelKeyProcPtr;
		mouseDraggedQDCGProc, mouseEnteredQDCGProc, mouseExitQDCGProc, mouseMovedQDCGProc: SkelQDCGMouseProcPtr;
		mouseDraggedProc, mouseEnteredProc, mouseExitProc, mouseMovedProc: SkelMouseProcPtr;
		scrollWheelProc: ProcPtr;
		
		{ Override methods }
		
		// Basic, most common events
		procedure drawRect (rect: NSRect); override;
		procedure mouseDown (theEvent: NSEvent); override;
		procedure keyDown (theEvent: NSEvent); override;
		// Additional events
	    procedure rightMouseDown(theEvent: NSEvent); override;
//	    procedure otherMouseDown(theEvent: NSEvent); override;
	    procedure mouseUp(theEvent: NSEvent); override;
	    procedure rightMouseUp(theEvent: NSEvent); override;
	    procedure otherMouseUp(theEvent: NSEvent); override;
	    procedure mouseMoved(theEvent: NSEvent); override;
	    procedure mouseDragged(theEvent: NSEvent); override;
	    procedure scrollWheel(theEvent: NSEvent); override;
//	    procedure rightMouseDragged(theEvent: NSEvent); override;
//	    procedure otherMouseDragged(theEvent: NSEvent); override;
	    procedure mouseEntered(theEvent: NSEvent); override;
	    procedure mouseExited(theEvent: NSEvent); override;
	    procedure keyUp(theEvent: NSEvent); override;
		
//	protected
		function acceptsFirstResponder: Boolean; message 'acceptsFirstResponder'; override;
		function becomeFirstResponder: Boolean; message 'becomeFirstResponder'; override;
		function resignFirstResponder: Boolean; message 'resignFirstResponder'; override;
		function isFlipped: Boolean; message 'isFlipped'; override;
		
		procedure viewBoundsDidChange(notification: NSNotification); message 'viewBoundsDidChange:';
		
//	    procedure setColor(color_: NSColor); message 'setColor:';
	end;

function SkelNewView(theWind: NSWindow; x, y, w, h: Real): SkelView;overload;
function SkelNewView(theWind: NSWindow; viewRect: NSRect): SkelView;overload;
function SkelNewView(theWind: NSWindow; r: Rect): SkelView;overload;
// Would you rather read from a nib? You can't define a SkelView in a
// nib, but you can use SkelCloneSkelView.

// Install handlers for a view.
procedure InstallSkelViewHandler(control: SkelView; // Old name
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr; userData: Pointer);
procedure SkelSetViewHandler(control: SkelView;
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr; userData: Pointer); overload;
procedure SkelSetViewHandler(control: SkelView;
		theMouseProc: SkelQDCGMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelQDCGDrawProcPtr; userData: Pointer); overload;
		
// Install additional, less common handlers:
// Old names
procedure InstallSkelViewExtraMouseHandlers(control: SkelView; altMouseProc, mouseUpProc, altMouseUpProc: SkelMouseProcPtr);
procedure InstallSkelViewMouseMovementHandlers(control: SkelView; mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc: SkelMouseProcPtr);
procedure InstallSkelViewExtraEventHandlers(control: SkelView; keyUpProc: SkelKeyProcPtr; scrollWheelProc: ProcPtr);
// New names, more in the line of the rest
procedure SkelSetViewExtraMouseHandlers(control: SkelView; altMouseProc, mouseUpProc, altMouseUpProc: SkelMouseProcPtr);
procedure SkelSetViewMouseMovementHandlers(control: SkelView; mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc: SkelMouseProcPtr);
procedure SkelSetViewExtraEventHandlers(control: SkelView; keyUpProc: SkelKeyProcPtr; scrollWheelProc: ProcPtr);
// Maybe there should be one call for each callback. I will add that if it becomes interesting.

// Why not just do like this?
(*
procedure SkelSetViewDrawHandler();
procedure SkelSetViewMouseDownHandler(); // Alt replaced by info of key in same call?
procedure SkelSetViewAltMouseDownHandler(); // ???
procedure SkelSetViewMouseUpHandler();
procedure SkelSetViewMouseEnterHandler();
procedure SkelSetViewMouseExitHandler();
procedure SkelSetViewMouseMovedHandler();
procedure SkelSetViewMouseDraggedHandler();
procedure SkelSetViewKeyDownHandler();
procedure SkelSetViewKeyUpHandler();
procedure SkelSetViewScrollWheelHandler();
*)
// New callback installers!
procedure SkelSetViewMouseHandler(control: SkelView; theMouseProc: SkelMouseProcPtr);overload;
procedure SkelSetViewDrawHandler(control: SkelView; theDrawProc: SkelQDCGDrawProcPtr);overload;
procedure SkelSetViewDrawHandler(control: SkelView; theDrawProc: SkelDrawProcPtr);overload;
procedure SkelSetViewKeyDownHandler(control: SkelView;theKeyProc: SkelKeyProcPtr);
procedure SkelSetViewUserData(control: SkelView; userData: Pointer);
procedure SkelSetViewRightMouseHandler(control: SkelView; altMouseProc: SkelMouseProcPtr);overload;
procedure SkelSetViewMouseUpHandler(control: SkelView; mouseUpProc: SkelMouseProcPtr);overload;
procedure SkelSetViewRightMouseUpHandler(control: SkelView; altMouseUpProc: SkelMouseProcPtr);overload;
procedure SkelSetViewMouseEnterExitHandlers(control: SkelView; mouseEnterProc, mouseExitProc: SkelMouseProcPtr);overload;
procedure SkelSetViewMouseMovedHandler(control: SkelView; mouseMovedProc: SkelMouseProcPtr);overload;
procedure SkelSetViewMouseDraggedHandler(control: SkelView; mouseDraggedProc: SkelMouseProcPtr);overload;
procedure SkelSetViewKeyUpHandler(control: SkelView; keyUpProc: SkelKeyProcPtr);
procedure SkelSetViewScrollWheelHandler(control: SkelView; scrollWheelProc: ProcPtr);
// More QDCG-style variants 140703
procedure SkelSetViewMouseHandler(control: SkelView; theMouseProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetViewRightMouseHandler(control: SkelView; altMouseProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetViewMouseUpHandler(control: SkelView; mouseUpProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetViewRightMouseUpHandler(control: SkelView; altMouseUpProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetViewMouseEnterExitHandlers(control: SkelView; mouseEnterProc, mouseExitProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetViewMouseMovedHandler(control: SkelView; mouseMovedProc: SkelQDCGMouseProcPtr);overload;
procedure SkelSetViewMouseDraggedHandler(control: SkelView; mouseDraggedProc: SkelQDCGMouseProcPtr);overload;




// callbacks:
// mouse, key, update. focus? drag?
// inval (needsToDrawRect, needsDisplay?)
// focus (focusView?)

// Redrawing (trivial wrappers)
procedure SkelInvalRect(theView: NSView; invalidRect: NSRect);
procedure SkelInvalView(theView: NSView);
procedure SkelInvalWindow(theWindow: NSWindow);

// Set the background color
procedure SkelViewSetColor(v: SkelView; theColor: NSColor); overload;
procedure SkelViewSetColor(v: SkelView; r, g, b: Real); overload;
procedure SkelViewSetColor(v: SkelView; r, g, b, a: Real); overload;

// Replace a view with a SkelView of the same size
function SkelCloneSkelView(oldView: NSView): SkelView;

// Ignore repeats (global setting only).
procedure SkelSetIgnoreRepeat(doIgnore: Boolean);

// Arrow keys map to the rarely used 28 to 31 (just before space).
const
	kSkelLeft = Char(28);
	kSkelUp = Char(29);
	kSkelDown = Char(30);
	kSkelRight = Char(31);

implementation

uses
	TransSkel5;

// Obsolete
procedure InstallSkelViewHandler(control: SkelView; // Old name
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr; userData: Pointer);
begin
	SkelSetViewHandler(control, theMouseProc, theKeyProc, theDrawProc, userData);
end;

// Obsolete
procedure SkelSetViewHandler(control: SkelView;
		theMouseProc: SkelMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelDrawProcPtr; userData: Pointer);
begin
	if control <> nil then
	with control do
	begin
//		window := theWind;
		drawProc := theDrawProc;
		mouseProc := theMouseProc;
		keyProc := theKeyProc;
		focused := false;
		userDataPtr := userData;
		
		if keyProc <> nil then
			SkelSetFocus(control);
	end;
//	control.AddMethods; // Update
end;

// Obsolete
procedure SkelSetViewHandler(control: SkelView;
		theMouseProc: SkelQDCGMouseProcPtr; theKeyProc: SkelKeyProcPtr;
		theDrawProc: SkelQDCGDrawProcPtr; userData: Pointer); overload;
begin
	if control <> nil then
	with control do
	begin
//		window := theWind;
		drawQDCGProc := theDrawProc;
		mouseQDCGProc := theMouseProc;
		keyProc := theKeyProc;
		focused := false;
		userDataPtr := userData;
		
		if keyProc <> nil then
			SkelSetFocus(control);
	end;
//	control.AddMethods; // Update
end;

// Simple protective wrappers, toolbox-style
procedure SkelInvalRect(theView: NSView; invalidRect: NSRect);
begin
   theView.setNeedsDisplayInRect(invalidRect);
end;

procedure SkelInvalView(theView: NSView);
begin
	theView.setNeedsDisplay_(true);
end;

procedure SkelInvalWindow(theWindow: NSWindow);
begin
	SkelView(theWindow.contentView).setNeedsDisplay_(true);
end;


procedure SkelView.drawRect (rect: NSRect);
var
	ctx: NSGraphicsContext;
	cgc: CGContextRef;
	r: QDCG.Rect;
begin
	ctx := NSGraphicsContext.currentContext;
	
	cgc := CGContextRef(ctx.graphicsPort);
	CGContextSaveGState(cgc); // Added 100413, probably not necessary
	
	if drawProc <> nil then
		drawProc(self, cgc, CGRect(rect), userDataPtr)
	else
	if drawQDCGProc <> nil then
	begin
//		QDCG.UseNSViewContext(self);
		CreatePort(self);
		r := CGRectToRect(CGRect(rect));
		drawQDCGProc(self, r, userDataPtr);
		QDCG.FinishPort; // Not currently necessary but it is good to give QDCG the chance to clean up.
	end
	else
	begin
		CGContextSetRGBFillColor(cgc, gRed, gGreen, gBlue, gAlpha);
		CGContextFillRect(cgc, CGRect(bounds));

// The code I started from used this NSColor:
//		backgroundColor.setFill;
//		NSRectFill(bounds);
		
		if focused then
		begin
//			NSColor.blueColor.setFill;
//			NSFrameRect(bounds);
		end;
	end;
	CGContextRestoreGState(cgc); // Added 100413, probably not necessary
end;

procedure SkelView.mouseDown (theEvent: NSEvent);
var
	p: NSPoint;
//	f: NSRect;
	pp: QDCG.Point;
begin
	if mouseProc <> nil then
	begin
		// Convert location in window to location in view
		p := theEvent.locationInWindow;
		p := self.convertPoint_fromView(p, nil); // Apple's way
// My own way (which should be equivalent):
//		f := self.frame;
//		p.x -= f.origin.x;
//		p.y -= f.origin.y;
// except that I don't handle axis flipping.
		mouseProc(self, CGPoint(p), theEvent.modifierFlags, userDataPtr);
	end;
	if mouseQDCGProc <> nil then
	begin
		// Convert location in window to location in view
		p := theEvent.locationInWindow;
		p := self.convertPoint_fromView(p, nil); // Apple's way
		pp.h := p.x;
		pp.v := p.y;
		mouseQDCGProc(self, pp, theEvent.modifierFlags, userDataPtr);
	end;
end;

var
	gIgnoreRepeat: Boolean = false;

procedure SkelView.keyDown (theEvent: NSEvent);
var
	cfstr: CFStringRef;
	s: String;
begin
//	WriteLn('key!');
//	WriteLn('key code = ', theEvent.keyCode);
	if gIgnoreRepeat then
		if theEvent.isARepeat then
			Exit;

	case theEvent.keyCode of
		123: //, NSLeftArrowFunctionKey:
			 s := kSkelLeft;
		124: //, NSRightArrowFunctionKey:
			 s := kSkelRight;
		125: //, NSDownArrowFunctionKey:
			 s := kSkelDown;
		126: //, NSUpArrowFunctionKey:
			 s := kSkelUp;
		otherwise
		begin
			cfstr := CFStringRef(theEvent.characters);
//	s := CFStringGetCStringPtr(cfstr, CFStringGetSystemEncoding);
			s := CFStringToString(cfstr);
//	s := CFStringGetCStringPtr(cfstr, kCFStringEncodingMacRoman);
		end;
	end;

// Borde byta till UTF8? Inställbart?
//	WriteLn('Key  = ', Ord(s[1]));
	if keyProc <> nil then
		keyProc(self, s[1], theEvent.modifierFlags, userDataPtr);
end;

function SkelView.acceptsFirstResponder: Boolean;
begin
//	WriteLn('Got asked');
//	if keyProc <> nil then
//		result := TRUE;
//	else result := FALSE;
	result := keyProc <> nil;
end;

function SkelView.becomeFirstResponder: Boolean;
begin
// Redraw control as activated!
//	WriteLn('Got focus');
	focused := true;
	result := TRUE;
	setNeedsDisplay_(true);
end;

function SkelView.resignFirstResponder: Boolean;
begin
// Redraw control as deactivated!
//	WriteLn('Lost focus');
	focused := false;
	result := TRUE;
	setNeedsDisplay_(true);
end;

function SkelView.isFlipped: Boolean;
begin
	result := flipped;
end;

function SkelNewView(theWind: NSWindow; viewRect: NSRect): SkelView;overload;
var
	mainWindowView: NSView;
begin
	result := SkelView(SkelView.alloc);
	result.initWithFrame(viewRect);
    result.setAutoresizesSubviews(true); // Added 100621 - I think this should be default
	if theWind <> nil then
	begin
		mainWindowView := NSView(theWind.contentView);
		mainWindowView.addSubview(result);
	end;
	
// Set up so we can get mouse enter and mouse exit
// This does NOT work if theWind = nil!
// Thus, it is added when a corresponding proc is passed to InstallSkelViewMouseMovementHandlers.
//	result.lastTrack := result.addTrackingRect_owner_userData_assumeInside(viewRect, result, nil, false);
//	NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(result,
//			sel_registerName(PChar('viewBoundsDidChange:')), NSViewFrameDidChangeNotification, result);
	
	result.flipped := true; // Default
//	result.backgroundColor := NSColor.whiteColor;
	result.gRed := 0.9;
	result.gGreen := 0.9;
	result.gBlue := 0.9;
	result.gAlpha := 1.0;
end;

function SkelNewView(theWind: NSWindow; x, y, w, h: Real): SkelView;overload;
begin
	Exit(SkelNewView(theWind, NSMakeRect(x, y, w, h)));
end;

function SkelNewView(theWind: NSWindow; r: Rect): SkelView;overload;
begin
	Exit(SkelNewView(theWind, RectToNSRect(r)));
end;

procedure SkelViewSetColor(v: SkelView; theColor: NSColor); overload;
begin
//	v.backgroundColor := theColor;
	theColor.getRed_green_blue_alpha(@v.gRed, @v.gGreen, @v.gBlue, @v.gAlpha);
	SkelInvalView(v);
end;
procedure SkelViewSetColor(v: SkelView; r, g, b: Real); overload;
begin
//	v.backgroundColor := NSColor.colorWithDeviceRed_green_blue_alpha(r, g, b, 1.0);
	v.gRed := r;
	v.gGreen := g;
	v.gBlue := b;
	v.gAlpha := 1.0;
	SkelInvalView(v);
end;
procedure SkelViewSetColor(v: SkelView; r, g, b, a: Real); overload;
begin
//	v.backgroundColor := NSColor.colorWithDeviceRed_green_blue_alpha(r, g, b, a);
	v.gRed := r;
	v.gGreen := g;
	v.gBlue := b;
	v.gAlpha := a;
	SkelInvalView(v);
end;

// Create a SkelView by replacing an existing view
// This can be handy when using custom views in nibs.
function SkelCloneSkelView(oldView: NSView): SkelView;
var
	viewRect: NSRect;
	autoSub: Boolean;
	mask: NSUInteger;
begin
    autoSub := oldView.autoresizesSubviews; // Not yet meaningful
    mask := oldView.autoresizingMask;
	
	// Get the position for the NSView.
	viewRect := oldView.frame;
	// Create a new one with the same viewRect.
	// Can't use SkelNewView since we don't know the window!
	result := SkelView(SkelView.alloc);
	result.initWithFrame(viewRect);
	oldView.superView.addSubview(result);
	oldView.removeFromSuperview;

    result.setAutoresizesSubviews(autoSub);
    result.setAutoresizingMask(mask);

// Set up so we can get mouse enter and mouse exit
// This does NOT work if the view is not yet connected to a window!
// Thus, it is added when a corresponding proc is passed to InstallSkelViewMouseMovementHandlers.
//	result.lastTrack := result.addTrackingRect_owner_userData_assumeInside(viewRect, result, nil, false);
//	NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(result,
//			sel_registerName(PChar('viewBoundsDidChange:')), NSViewFrameDidChangeNotification, result);
	
	result.flipped := true; // Default
//	result.backgroundColor := NSColor.whiteColor;
	result.gRed := 0.9;
	result.gGreen := 0.9;
	result.gBlue := 0.9;
	result.gAlpha := 1.0;
	
	// Note: no subview support yet
end;

//		mouseUpProc, rightMouseProc, rightMouseUpProc: SkelMouseProcPtr;
//		keyUpProc: SkelKeyProcPtr;
//		mouseDraggedProc, mouseEnteredProc, mouseExitProc: SkelMouseProcPtr;
//		mouseMovedProc: SkelMouseProcPtr;

procedure SkelView.rightMouseDown(theEvent: NSEvent);
begin
	if rightMouseProc <> nil then
		rightMouseProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
	if rightMouseQDCGProc <> nil then
		rightMouseQDCGProc(self, CGPointToPoint(CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil))), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.mouseUp(theEvent: NSEvent);
begin
writeln('mouseUp');
	if mouseUpProc <> nil then
		mouseUpProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
	if mouseUpQDCGProc <> nil then
		mouseUpQDCGProc(self, CGPointToPoint(CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil))), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.rightMouseUp(theEvent: NSEvent);
begin
	if rightMouseUpProc <> nil then
		rightMouseUpProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
	if rightMouseUpQDCGProc <> nil then
		rightMouseUpQDCGProc(self, CGPointToPoint(CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil))), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.otherMouseUp(theEvent: NSEvent);
begin
//	if rightMouseUpProc <> nil then
//		rightMouseUpProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.mouseMoved(theEvent: NSEvent);
begin
	if mouseMovedProc <> nil then
		mouseMovedProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
// NOTE: Many QDCG procs are missing!
//writeln('mouseMoved');
	if mouseMovedQDCGProc <> nil then
		mouseMovedQDCGProc(self, CGPointToPoint(CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil))), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.mouseDragged(theEvent: NSEvent);
begin
writeln('mouseDragged');
	if mouseDraggedProc <> nil then
		mouseDraggedProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
	if mouseDraggedQDCGProc <> nil then
		mouseDraggedQDCGProc(self, CGPointToPoint(CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil))), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.scrollWheel(theEvent: NSEvent);
begin
// NOT YET IMPLEMENTED
end;
procedure SkelView.mouseEntered(theEvent: NSEvent);
begin
	if mouseEnteredProc <> nil then
		mouseEnteredProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.mouseExited(theEvent: NSEvent);
begin
	if mouseExitProc <> nil then
		mouseExitProc(self, CGPoint(self.convertPoint_fromView(theEvent.locationInWindow, nil)), theEvent.modifierFlags, userDataPtr);
end;
procedure SkelView.keyUp(theEvent: NSEvent);
var
	cfstr: CFStringRef;
	s: String;
begin
	cfstr := CFStringRef(theEvent.characters);
	s := CFStringGetCStringPtr(cfstr, CFStringGetSystemEncoding);
	if keyUpProc <> nil then
		keyUpProc(self, s[1], 0, userDataPtr);
end;

procedure SkelView.viewBoundsDidChange(notification: NSNotification); // viewBoundsDidChange:
var
	theBounds: NSRect;
begin
	theBounds := self.bounds;
	// we set up a tracking region so we can get mouseEntered and mouseExited events
	removeTrackingRect(lastTrack);
	lastTrack := self.addTrackingRect_owner_userData_assumeInside(theBounds, self, nil, false);
end;

// Old names, phase out
procedure InstallSkelViewExtraMouseHandlers(control: SkelView; altMouseProc, mouseUpProc, altMouseUpProc: SkelMouseProcPtr);
begin
	SkelSetViewExtraMouseHandlers(control, altMouseProc, mouseUpProc, altMouseUpProc);
//	control.rightMouseProc := altMouseProc;
//	control.mouseUpProc := mouseUpProc;
//	control.rightMouseUpProc := altMouseUpProc;
end;
// Obsolete
procedure InstallSkelViewMouseMovementHandlers(control: SkelView; mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc: SkelMouseProcPtr);
begin
	SkelSetViewMouseMovementHandlers(control, mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc);
//	control.mouseEnteredProc := mouseEnterProc;
//	control.mouseExitProc := mouseExitProc;
//	control.mouseMovedProc := mouseMovedProc;
//	control.mouseDraggedProc := mouseDraggedProc;
// This is the right place, I think
//	if (mouseEnterProc <> nil) or (mouseExitProc <> nil) then
//	begin
//		control.lastTrack := control.addTrackingRect_owner_userData_assumeInside(control.frame, control, nil, false);
//		NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(control,
//			sel_registerName(PChar('viewBoundsDidChange:')), NSViewFrameDidChangeNotification, control);
//	end;
end;
// Obsolete
procedure InstallSkelViewExtraEventHandlers(control: SkelView; keyUpProc: SkelKeyProcPtr; scrollWheelProc: ProcPtr);
begin
	SkelSetViewExtraEventHandlers(control, keyUpProc, scrollWheelProc);
//	control.keyUpProc := keyUpProc;
//	control.scrollWheelProc := scrollWheelProc;
end;

// Obsolete
procedure SkelSetViewExtraMouseHandlers(control: SkelView; altMouseProc, mouseUpProc, altMouseUpProc: SkelMouseProcPtr);
begin
	control.rightMouseProc := altMouseProc;
	control.mouseUpProc := mouseUpProc;
	control.rightMouseUpProc := altMouseUpProc;
end;
// Obsolete
procedure SkelSetViewMouseMovementHandlers(control: SkelView; mouseEnterProc, mouseExitProc, mouseMovedProc, mouseDraggedProc: SkelMouseProcPtr);
begin
	control.mouseEnteredProc := mouseEnterProc;
	control.mouseExitProc := mouseExitProc;
	control.mouseMovedProc := mouseMovedProc;
	control.mouseDraggedProc := mouseDraggedProc;

//	if mouseMovedProc <> nil then
//		window.setAcceptsMouseMovedEvents(true);

// This is the right place, I think
	if (mouseEnterProc <> nil) or (mouseExitProc <> nil) then
	begin
		control.lastTrack := control.addTrackingRect_owner_userData_assumeInside(control.frame, control, nil, false);
		NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(control,
			sel_registerName(PChar('viewBoundsDidChange:')), NSViewFrameDidChangeNotification, control);
	end;
end;
// Obsolete
procedure SkelSetViewExtraEventHandlers(control: SkelView; keyUpProc: SkelKeyProcPtr; scrollWheelProc: ProcPtr);
begin
	control.keyUpProc := keyUpProc;
	control.scrollWheelProc := scrollWheelProc;
end;
procedure SkelSetIgnoreRepeat(doIgnore: Boolean);
begin
	gIgnoreRepeat := doIgnore;
end;





// New handler installers, one for each
// Replaces SkelSetViewHandler:

procedure SkelSetViewMouseHandler(control: SkelView;
		theMouseProc: SkelMouseProcPtr); overload;
begin
	if control <> nil then
		control.mouseProc := theMouseProc;
end;

procedure SkelSetViewMouseHandler(control: SkelView;
		theMouseProc: SkelQDCGMouseProcPtr); overload;
begin
	if control <> nil then
		control.mouseQDCGProc := theMouseProc;
end;

procedure SkelSetViewDrawHandler(control: SkelView;
		theDrawProc: SkelQDCGDrawProcPtr);overload;
begin
	if control <> nil then
		control.drawQDCGProc := theDrawProc;
end;

procedure SkelSetViewDrawHandler(control: SkelView;
		theDrawProc: SkelDrawProcPtr);overload;
begin
	if control <> nil then
		control.drawProc := theDrawProc;
end;

procedure SkelSetViewKeyDownHandler(control: SkelView;
		theKeyProc: SkelKeyProcPtr);
begin
	if control <> nil then
	begin
		control.keyProc := theKeyProc;
		if theKeyProc <> nil then
			SkelSetFocus(control);
	end;
end;

procedure SkelSetViewUserData(control: SkelView; userData: Pointer);
begin
	if control <> nil then
		control.userDataPtr := userData;

//	control.AddMethods; // Update
end;

// Replacements of SkelSetViewExtraMouseHandlers, SkelSetViewMouseMovementHandlers, SkelSetViewExtraEventHandlers

procedure SkelSetViewRightMouseHandler(control: SkelView; altMouseProc: SkelMouseProcPtr);overload;
begin
	if control <> nil then
		control.rightMouseProc := altMouseProc;
end;
procedure SkelSetViewRightMouseHandler(control: SkelView; altMouseProc: SkelQDCGMouseProcPtr);overload;
begin
	if control <> nil then
		control.rightMouseQDCGProc := altMouseProc;
end;
procedure SkelSetViewMouseUpHandler(control: SkelView; mouseUpProc: SkelMouseProcPtr);overload;
begin
	if control <> nil then
		control.mouseUpProc := mouseUpProc;
end;
procedure SkelSetViewMouseUpHandler(control: SkelView; mouseUpProc: SkelQDCGMouseProcPtr);overload;
begin
	if control <> nil then
		control.mouseUpQDCGProc := mouseUpProc;
end;
procedure SkelSetViewRightMouseUpHandler(control: SkelView; altMouseUpProc: SkelMouseProcPtr);overload;
begin
	if control <> nil then
		control.rightMouseUpProc := altMouseUpProc;
end;
procedure SkelSetViewRightMouseUpHandler(control: SkelView; altMouseUpProc: SkelQDCGMouseProcPtr);overload;
begin
	if control <> nil then
		control.rightMouseUpQDCGProc := altMouseUpProc;
end;
procedure SkelSetViewMouseEnterExitHandlers(control: SkelView; mouseEnterProc, mouseExitProc: SkelMouseProcPtr);overload;
begin
	if control <> nil then
	begin
		control.mouseEnteredProc := mouseEnterProc;
		control.mouseExitProc := mouseExitProc;

	// This is the right place, I think
		if (mouseEnterProc <> nil) or (mouseExitProc <> nil) then
		begin
			control.lastTrack := control.addTrackingRect_owner_userData_assumeInside(control.frame, control, nil, false);
			NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(control,
				sel_registerName(PChar('viewBoundsDidChange:')), NSViewFrameDidChangeNotification, control);
		end;
	end;
end;
procedure SkelSetViewMouseEnterExitHandlers(control: SkelView; mouseEnterProc, mouseExitProc: SkelQDCGMouseProcPtr);overload;
begin
	if control <> nil then
	begin
		control.mouseEnteredQDCGProc := mouseEnterProc;
		control.mouseExitQDCGProc := mouseExitProc;

	// This is the right place, I think
		if (mouseEnterProc <> nil) or (mouseExitProc <> nil) then
		begin
			control.lastTrack := control.addTrackingRect_owner_userData_assumeInside(control.frame, control, nil, false);
			NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(control,
				sel_registerName(PChar('viewBoundsDidChange:')), NSViewFrameDidChangeNotification, control);
		end;
	end;
end;
procedure SkelSetViewMouseMovedHandler(control: SkelView; mouseMovedProc: SkelMouseProcPtr);overload;
begin
	if control <> nil then
		control.mouseMovedProc := mouseMovedProc;
end;
procedure SkelSetViewMouseMovedHandler(control: SkelView; mouseMovedProc: SkelQDCGMouseProcPtr);overload;
begin
	if control <> nil then
		control.mouseMovedQDCGProc := mouseMovedProc;
end;
procedure SkelSetViewMouseDraggedHandler(control: SkelView; mouseDraggedProc: SkelMouseProcPtr);overload;
begin
	if control <> nil then
		control.mouseDraggedProc := mouseDraggedProc;
end;
procedure SkelSetViewMouseDraggedHandler(control: SkelView; mouseDraggedProc: SkelQDCGMouseProcPtr);overload;
begin
	if control <> nil then
		control.mouseDraggedQDCGProc := mouseDraggedProc;
end;
procedure SkelSetViewKeyUpHandler(control: SkelView; keyUpProc: SkelKeyProcPtr);
begin
	if control <> nil then
		control.keyUpProc := keyUpProc;
end;
procedure SkelSetViewScrollWheelHandler(control: SkelView; scrollWheelProc: ProcPtr);
begin
	if control <> nil then
		control.scrollWheelProc := scrollWheelProc;
end;


end.
