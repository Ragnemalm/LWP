// Project overview based on files referenced by the main program.
// Working draft 16/8 -13

// IdŽer:
// Ikoner eller meny fšr att Šndra ordning
// Bokstavsordning
// Sorterat efter mappar
// Mer?

unit ProjectWindow;
interface
uses
	MacOSAll, ColorCoding, TransSkel4, Scroller,
	UtilsTypes, ProcessUtils, FileUtils, IncludeAnalyzer,
	Classes, QDCG;

procedure InitProjectWindow;
//procedure OpenFileToProject();
procedure ShowProjectWindow;
procedure RefreshProjectWindow;

implementation

uses
	Settings, LWPEdit;
const
	kSpacing = 15;
	kTextSize = 10;

var
	gProjectWindow: WindowPtr;
	sd: ScrollDataPtr;

procedure SortFileArr(var theFileArr: FileArr; fileNameOnly: Boolean);
var
	l: TStringList;
	i: Longint;
begin
	l := TStringList.Create;
//	if fileNameOnly then
//	begin
//		for i := 0 to High(theFileArr) do
//			WriteLn(l.Add(GetLastToken(theFileArr[i])));
//	end
//	else
//	for i := 0 to High(theFileArr) do
//		WriteLn(l.Add(theFileArr[i]));
	if fileNameOnly then
	begin
		for i := 0 to High(theFileArr) do
			l.Add(GetLastToken(theFileArr[i]));
	end
	else
	for i := 0 to High(theFileArr) do
		l.Add(theFileArr[i]);
	l.Sort;
	
	for i := 0 to High(theFileArr) do
//		theFileArr[i] := l.Values.GetValue(i);
		theFileArr[i] := l.Strings[i];
	
	l.Destroy;
end;

var
//	arr: AnsiStringArray;
	theFileArr: FileArr;


procedure DrawView(theView: HIViewRef; cgContext: CGContextRef; cgviewRect: CGRect; userData: Pointer);
var
	viewRect{, r}: Rect;
	i: Longint;
begin
	viewRect := CGRectToRect(cgviewRect);

//	UseViewContext(cgContext, cgviewRect.size.height);
	CreatePort(cgContext, cgviewRect.size.height);
//	UseViewContext(cgContext, cgviewRect.size.height);

	BackColor(whiteColor);
	EraseRect(viewRect);
	
	TextSize(kTextSize);
	PenSize(1);
	if Length(theFileArr) > 0 then
	begin
		for i := 0 to High(theFileArr) do
		begin
			MoveTo(10, kSpacing*(i+1)); DrawString(GetLastToken(theFileArr[i]));
			// Check if path differs
			if i < High(theFileArr) then
				if TrimLastToken(theFileArr[i]) <> TrimLastToken(theFileArr[i+1]) then
				begin
					ForeColor(blueColor);
					MoveTo(0, kSpacing*(i+1)+4);
					LineTo(1000, kSpacing*(i+1)+4);
					ForeColor(blackColor);
				end;
		end;
	end
{	else
	if Length(arr) > 0 then
	begin
		for i := 0 to High(arr) do
		begin
			MoveTo(10, kTextSize*(i+1)); DrawString(arr[i]);			
		end;
	end}
	else
	begin
		MoveTo(10, 15); DrawString('(No main program file open)');
	end;
	FinishPort;
end;

var
	gLastSpec: FSSpecString;

procedure UpdateFileList;
var
	front: WindowPtr;
	theSpec: FSSpecString;
//	fileData: AnsiString;
	err: OSErr;
	i: Longint;
//	windBounds, r: MacOSAll.Rect;
begin
	// Don't do anything if project window is not visible
	//if not IsWindowVisible (gProjectWindow) then
		//Exit(UpdateFileList);
	front := GetFrontMainWindow;
	err := GetEditFSSpec(front, theSpec);
	WriteLn('Front = ', theSpec);

	if theSpec <> gLastSpec then
	begin
		gLastSpec := theSpec;
		if theSpec <> '' then
		begin
			theFileArr := GetIncludeFileList(theSpec, true {include header files});
			SetLength(theFileArr, Length(theFileArr)+1);
			for i := High(theFileArr)-1 downto 0 do
				theFileArr[i+1] := theFileArr[i];
			theFileArr[0] := theSpec;
			SetWTitle(gProjectWindow, 'Project '+GetLastToken(theSpec));
//			WriteLn('UpdateFileList: got ', Length(theFileArr), ' items from GetIncludeFileList');

			// Add extra files!
			WriteLn('Add extra files to project!');
			WriteLn('Add ', + Length(gSettings.fileListArray), ' extra files to project!');
			SetLength(theFileArr, Length(theFileArr)+Length(gSettings.fileListArray));
			for i := 0 to High(gSettings.fileListArray) do
			begin
//				extraFileName := gSettings.fileListArray[i];
//				if extraFileName = GetLastToken(extraFileName) then // extra file given as file name without path
//				Must search all paths for file!
//				I save this for later, this is relatively complicated and I should try to reuse similar functions.
				theFileArr[i + Length(theFileArr)-Length(gSettings.fileListArray)] := gSettings.fileListArray[i];
				WriteLn('Adding ', gSettings.fileListArray[i], ' to project window!');
			end;
		end
		else
		begin
			SetWTitle(gProjectWindow, 'Project (none)');
			SetLength(theFileArr, 0);
		end;
		
		// Sort the strings
		SortFileArr(theFileArr, false);
//		WriteLn('UpdateFileList: has ', Length(theFileArr), ' items after SortFileArr');


		// Better place to resize??!
//		windBounds := GetWindowPortBounds(gProjectWindow, windBounds)^;
//		GetControlBounds(sd^.contentView, r);
//		r.right := windBounds.right;
//		if Length(theFileArr) > 0 then
//			r.bottom := r.top + kSpacing * Length(theFileArr) + 5;
//		SetControlBounds(sd^.contentView, r);
	
		
		// Inval view
		HIViewSetNeedsDisplay(sd^.contentView, true);
	end;
end;

procedure MouseView(theView: HIViewRef; where: HIPoint; mods, button: Longint; userData: Pointer);
var
	i: Longint;
begin
	UpdateFileList;
	
	//if FrontWindow <> gProjectWindow then
		//Exit(MouseView); // Funkar inte?!
	
	i := Trunc(where.y / kSpacing);
	if i > High(theFileArr) then
		WriteLn('click outside list')
	else
	begin
//		WriteLn(theFileArr[i]);
//		FindOrOpen(theFileArr[i], '');
//		FindOrOpen(GetLastToken(theFileArr[i]), TrimLastToken(theFileArr[i]));
		OpenFileToWindow(theFileArr[i]);
	end;
	
(*
	fileData := ReadFileToString(theSpec);
	// Get array of filenames as strings
//	arr := GetUsesListFromText(@fileData[1], Length(fileData), GetLastToken(theSpec));
	WriteLn(fileData);
//	WriteLn(Length(arr));
*)
end;

procedure KeyView(theView: HIViewRef; key: Char; mods: Longint; userData: Pointer);
begin
end;


//procedure WindMouse (thePt: MacOSAll.Point; t: UInt32; mods: integer);
procedure WindMouse(theView: HIViewRef; thePt: QDCG.Point; mods, button: Longint; userData: Pointer);
begin
end;

procedure WindKey (ch: char; mods: integer);
begin
end;

procedure RecalcProjectWindow;
var
	windBounds: MacOSAll.Rect;
	r: MacOSAll.Rect;
//	viewPort: MacOSAll.GrafPtr;
//	cgc: CGContextRef;
begin
		windBounds := GetWindowPortBounds(gProjectWindow, windBounds)^;
//		SetRect(r, windBounds.left, windBounds.top, windBounds.right, windBounds.bottom);
		windBounds.top += 20;
//		ResizeScroller(sd, windBounds);

		GetControlBounds(sd^.contentView, r);
		r.right := windBounds.right;
		if Length(theFileArr) > 0 then
			r.bottom := r.top + kSpacing * Length(theFileArr) + 5;
		SetControlBounds(sd^.contentView, r);

		ResizeScroller(sd, windBounds);
end;

procedure WindUpdate (resized: Boolean);
begin
	if resized then
		RecalcProjectWindow;
end;

procedure WindActivate (active: Boolean);
begin
	if active then
		UpdateFileList;
	RecalcProjectWindow;
end;

procedure WindResize(theEditWind: WindowRef);
begin
	RecalcProjectWindow;
end;

procedure DoClose;
begin
	HideWindow(gProjectWindow);
end;

procedure WindHalt;
begin
end;

// Denna finns numera i QDCG
(*function QDCGRectToMacOSAllRect(qdcgr: QDCG.Rect): MacOSAll.Rect;
var
	r: MAcOSAll.Rect;
begin
	r.left := Trunc(qdcgr.left);
	r.top := Trunc(qdcgr.top);
	r.right := Trunc(qdcgr.right);
	r.bottom := Trunc(qdcgr.bottom);
	QDCGRectToMacOSAllRect := r;
end;*)

procedure InitProjectWindow;
var
	window: WindowPtr;
	mainFrame: MacOSAll.Rect;
	r: Rect;
//	err: OSStatus;
	hv, masterView: ControlRef;
begin
WriteLn('InitProjectWindow 1');
	InitGraf; // Otherwise qd.screenBits.bounds may not exist
//	r := QDCGRectToMacOSAllRect(qd.screenBits.bounds); // qd.screenBits is in QDCG!
	r := qd.screenBits.bounds; // qd.screenBits is in QDCG!
//	WriteLn(r.left, ',', r.top, ',', r.right, ',', r.bottom);
	r.left := r.right - 210;
	r.right := r.right - 10;
	r.top := r.top + 60;
	r.bottom := r.bottom - 50;
//	SetRect(r, 1000, 80, 1200, 500);

	window := SkelNewWindow(r, 'Project <project name>', nil {@WindUpdate}, @WindMouse, @WindKey, nil,
		@WindActivate, @DoClose, @WindHalt, nil, true, 0, nil, nil, 0, @WindResize);

WriteLn('InitProjectWindow 2');

//	err := CreateNewWindow ( kDocumentWindowClass, kWindowStandardDocumentAttributes, r, window);
//	SetWTitle(window, 'Project <project name>');
//	HideWindow(window);
//	SetPortWindowPort(window);

WriteLn('InitProjectWindow 3');

	hv := HIViewGetRoot(window);
	GetControlBounds(hv, mainFrame);
	mainFrame.top += 20;
	mainFrame.bottom -= 20;
	CreateUserPaneControl(window, mainFrame, kControlSupportsEmbedding, masterView); // Top view, connects to parent
	mainFrame.bottom := mainFrame.top + 700;
	sd := EmbedInScroller(hv, masterView, true, false, true, mainFrame);

WriteLn('InitProjectWindow 4');

	InstallSkelViewHandler(window, sd^.contentView, @DrawView, @MouseView, @KeyView, nil);
//	SkelWindow(window, @WindMouse, @WindKey, @WindUpdate, @WindActivate, @DoClose, @WindHalt, nil, false);
	gProjectWindow := window;
	gLastSpec := '';
WriteLn('InitProjectWindow 5');
end;

procedure ShowProjectWindow;
begin
//	WriteLn('Show project window selected');
	ShowWindow(gProjectWindow);
	SelectWindow(gProjectWindow);
end;

procedure RefreshProjectWindow;
begin
	UpdateFileList;
	RecalcProjectWindow;	
end;

end.
