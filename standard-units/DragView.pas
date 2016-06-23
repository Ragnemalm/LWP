// Draggable view. Should I better make this with a SkelView? Maybe.

{$mode macpas}
unit DragView;
interface
uses MacOSAll, TransSkel4, QDCG;

type
	VMDragProcPtr = procedure(theView: HIViewRef; diff: MacOSAll.Point; userData: Pointer);

procedure InstallDragViewHandler(inControl: ControlRef; useQD: Boolean; theDrawProc: TSDrawProcPtr; theDragProc: VMDragProcPtr; theUserData: Pointer);

implementation

type	
	DragViewDataRec = record
//		window: WindowRef;
		control: ControlRef;
		focused: Boolean;
		
		drawProc: TSDrawProcPtr;
		drawQDCGProc: VMQDCGDrawProcPtr;
		useQDflag: Boolean;
		
		dragProc: VMDragProcPtr;
		userData: Pointer;
	end;
	DragViewDataPtr = ^DragViewDataRec;
	
function StdDragViewEventHandlerProc(nextHandler: EventHandlerCallRef; inEvent: EventRef; inUserData: Pointer ):OSStatus; MWPascal;
var
	myViewDataPtr: DragViewDataPtr;
	err: OSErr;
	
	eventClass: UInt32;
	eventKind: UInt32;
	tempCG: Boolean;
	savePort, viewPort: MacOSAll.GrafPtr;
	cgc: CGContextRef;
	outRect: CGRect;
	qdpt, diff, oldqdpt: MacOSAll.Point;
	trackResult: MouseTrackingResult;
	qdcgp: QDCG.GWorldPtr;
begin
//	WriteLn('Drag view event');
	
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	
	// Standard bye-bye
	if eventClass = kEventClassControl then
		if eventKind = kEventControlDispose then
		begin
			DisposePtr(inUserData);
			Return noErr;
		end;	
		
	myViewDataPtr := DragViewDataPtr(inUserData);
	with myViewDataPtr^ do
	begin
		eventClass := GetEventClass(inEvent);
		eventKind  := GetEventKind(inEvent);

		if eventKind = kEventControlClick then
		begin
		// This doesn't look good. Better to use a callback from TransSkel!
		// But it works...
			TrackMouseLocation(nil, oldqdpt, trackResult);
			while trackResult <> kMouseTrackingMouseUp do
			begin
				TrackMouseLocation(nil, qdpt, trackResult);
				diff.h := qdpt.h - oldqdpt.h;
				diff.v := qdpt.v - oldqdpt.v;
				if dragProc <> nil then
					dragProc(control, diff, userData);
				oldqdpt := qdpt;
			end;
		end;

		if (eventClass = kEventClassControl) and (eventKind = kEventControlDraw) then
		begin
			GetPort(savePort);
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
			
			err := HIViewGetBounds(control, outRect);
			if err <> noErr then WriteLn('HIViewGetBounds ', err);

			// Skicka callback
			if drawProc <> nil then
				drawProc(control, cgc, outRect, userData);
			if drawQDCGProc <> nil then
			begin
				// Create port
				qdcgp := CreatePort(cgc, outRect.size.height);
				drawQDCGProc(control, CGRectToRect(outRect), userData);
// VMQDCGDrawProcPtr = procedure(theView: HIViewRef; viewRect: QDCG.Rect; userData: Pointer);
				ClosePort(qdcgp);
			end;
			
			MacOSAll.SetPort(savePort);
			if tempCG then
				QDEndCGContext(viewPort, cgc);
		end;
	end;
	
	return noErr;
end;

procedure InstallDragViewHandler(inControl: ControlRef; useQD: Boolean; theDrawProc: TSDrawProcPtr; theDragProc: VMDragProcPtr; theUserData: Pointer);
//InstallDragViewHandler(inControl: ControlRef; useQD: Boolean; theDrawProc: VMDrawProcPtr);
var
	err: OSStatus;
	myDragViewDataPtr: DragViewDataPtr;
const
	viewEvents: array [0..2] of EventTypeSpec =
	(
		( eventClass: kEventClassControl; eventKind: kEventControlDraw ), // FŒngar det mesta
		( eventClass: kEventClassControl; eventKind: kEventControlClick ),
		( eventClass: kEventClassControl; eventKind: kEventControlDispose )
	);
begin
	myDragViewDataPtr := DragViewDataPtr(NewPtrClear(SizeOf(DragViewDataRec)));
// Tab panes must have user pane IDs that match the tab number
	with myDragViewDataPtr^ do
	begin
		control := inControl;
		drawProc := theDrawProc;
		useQDflag := useQD;
		dragProc := theDragProc;
		userData := theUserData;
		
		err	:= InstallEventHandler( GetControlEventTarget(control), StdDragViewEventHandlerProc, Length(viewEvents), viewEvents, myDragViewDataPtr, nil );
		if err <> noErr then WriteLn('Failed to install event handler ', err);
		
		HIViewSetNeedsDisplay(control, true);
	end;
end;

end.
