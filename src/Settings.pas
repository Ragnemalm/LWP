// Lightweight Pascal IDE, settings unit
// © Ingemar Ragnemalm 2006-2013
// 130120: Introduced IsCommandLineToolMode to support auto-commandline-mode (switching to command-line mode if no resource folder is around)

{$mode macpas}
unit Settings;
interface
uses
	MacOSAll, FileUtils, TransSkel4,
	LWPGlobals, AlertsUtils, TXNUtilities, LWPColorGlobals, UtilsTypes, CarbonStandardFile;

const
	kDefaultPPCCompiler = '/usr/local/bin/ppcppc';
	kDefault386Compiler = '/usr/local/bin/ppc386';
	kDefaultGPCCompiler = '/usr/bin/gp';
	kDefaultCCompiler = '/usr/bin/gcc';
	kDefaultCudaCompiler = '/usr/local/cuda/bin/nvcc';
	kDefaultCudaOptions = '-L /usr/local/cuda/lib -lcudart';
	
	// ObjPas for OSX, simulator and iOS device, 0.8.11
	kDefaultiOSCompiler = '/usr/local/lib/fpc/2.5.1/ppcrossarm'; // iOSc
	kDefaultiPhoneSimulatorCompiler = '/usr/local/lib/fpc/2.5.1/ppcross386'; // SIMc
// Special compiler for ObjP no longer needed! Should be removed.
//	kDefaultPPCCompiler2 = '/usr/local/lib/fpc/2.5.1/ppcppc';
//	kDefault386Compiler2 = '/usr/local/lib/fpc/2.5.1/ppc386';
	kDefaultPPCCompiler2 = '/usr/local/bin/ppcppc';
	kDefault386Compiler2 = '/usr/local/bin/ppc386';
	kDefaultiOSInterfacesPath = '/Developer/ObjectivePascal/uikit-skel/src';
	
	// ObjC compilers for iOS and iPhone?
	kDefaultiOSCCompiler = 'arm-apple-darwin10-gcc-4.2.1'; // iOSm
	// 4.2
	// Borde leta upp en som finns?
//	kDefaultiOSCodesign = 'Default iOS developer';
//	kDefaultiOSCodesign = 'Ingemar Ragnemalm';
	kDefaultiOSCodesign = 'Your iOS developer ID here';

//	kDefaultGDB1 = '/usr/bin/gdb'; // Older OSX
//	kDefaultGDB2 = '/sw/gdb'; // 10.9 and up (or /usr/local/bin/gdb?!)
// No, we assume LLDB at 10.9 and up!
	// Check GDB with "which gdb"?


// A global record with all settings! This is written by the dialog item callbacks.
var
	gSettings: record
		// Longints
		errorParser, CCompileStrategy, target: Longint;
		
		// Booleans
		autoSave, autoIndent, autoFormat: Boolean;
//		colorCodeOnOpen, colorCodeOnSave, colorCodeOnTimer, colorCodeOnMenu: Boolean;
		commandLineToolModeOBSOLETE: Boolean; // OBSOLETE
		autoFramework: Boolean;
		
		textSize: Longint;
//		lobjcOBSOLETE: Boolean; // Objc AND C++
//		lcppOBSOLETE: Boolean; // UNUSED
		
		// Save search settings?
		tabSize: Longint;
		textSaveFormat: Longint;
		textSaveFont: LongInt;// 08/07/19 Modify for Font
		textSaveColorCoding: LongInt;// Modify for color 
		EditWindowPosition: LongInt;// D'S 08/09/07 Window Position
		HelpErrorWindowPosition: LongInt;
		pascalMode: Longint; // 1 = FPC, 2 = GPC, added by Ingemar 090122
		
//		updateColorCodingOBSOLETE: LongInt; // OBSOLETE! Settings for when to update color coding
		
		// Former separate globals:
		PPCCompiler: AnsiString;
		x86Compiler: AnsiString;
		Options: AnsiString;
		LinkOptions: AnsiString; {New in 0.3.2}
		DebugOptions: AnsiString; {New in 0.5.0}
		ReleaseOptions: AnsiString; {New in 0.5.0}
		CDebugOptions: AnsiString; {New in 0.5.0}
		CReleaseOptions: AnsiString; {New in 0.5.0}
		Paths: AnsiString;
		LibPaths: AnsiString;
		Frameworks: AnsiString;
		
		GPCCompiler: AnsiString; {New in 0.6}
		GPCOptions: AnsiString; {New in 0.6}
		
		ShInterpreter: AnsiString;
		CCompiler: AnsiString;
		COptions: AnsiString;
		
		JavaCompiler: AnsiString;
		JavaInterpreter: AnsiString;
		JavaOptions: AnsiString;
		
		AdaCompiler: AnsiString;
		AdaOptions: AnsiString;
		
		CudaCompiler: AnsiString; {New in 0.8.5}
		CudaOptions: AnsiString; {New in 0.8.5}

		gdb: AnsiString; {New in 0.9.8p?}
		
		TargetArgs: AnsiString;
		
//		PPCCompiler2: AnsiString; {New in 0.8.11}
//		386Compiler2: AnsiString;
		iOSCompiler: AnsiString;
		iPhoneSimulatorCompiler: AnsiString;
		iOSInterfacesPath: AnsiString;
		iOSObjCCompiler: AnsiString;
		iOSCodesign: AnsiString; {0.9.1}
		
		// NEW for list of additional files.
		fileList: AnsiString; // Set directly from dialog (TO DO)
		fileListArray: AnsiStringArray; // Split to array (should be done immediately on edit)

		customInfoPlist: AnsiString;
		customResources: AnsiString;
		customResourceArray: AnsiStringArray;

		searchIgnoreCase, searchEntireWord, searchWrap: Boolean;
	end;

//function GetSettingsTabSize: Longint;

const
	kPPCTarget = 1;
	k386Target = 2;
	kUniversalTarget = 3;
	
	kSingleFileStrategy = 1;
	kFileListStrategy = 2;
	kAllFilesInPathsStrategy = 3;
	kAnalyzeInclude = 4;

	kSaveMac = 1;
	kSaveUnix = 2;
	kSavePC = 3;
	
	kFPCmode = 1;
	kGPCmode = 2;
	
	kUpdateCCOnOpenSaveOnly = 1;
	kUpdateCCOnTimer = 2;
	kUpdateCCOnNewline = 3;
	kUpdateCCMax = 3; // Maximum

procedure DoSettings;
procedure DoAdvancedSettings;
function InitSettings: Boolean; // Create settings window and load global prefs
procedure LoadSettingsForWindow(editIndex: Longint); // Load project local prefs

// Hits on buttons
// Should these really be decided on global level?
// There should be a local callback!
procedure DefaultPascalCompilerSettings;
procedure DefaultCCompilerSettings;
//procedure ExperimentalPascalCompilerSettings;
procedure DoSettingsAddFile(var data: AnsiString; resCode: OSType);

//function GetSettingsTabSize: Longint;

function GetSettingsTextSize: Longint;
function IsCommandLineToolMode(mainProgramSpec: FSSpecString): Boolean;
function UpdateTextSize(theView: HIViewRef; myPtr: Pointer): Boolean;

procedure SetSettingsDialogItemString(signature: OSType; id: SInt32; value: AnsiString);
procedure SetSettingsDialog2ItemString(signature: OSType; id: SInt32; value: AnsiString);

const
	kNewPrefsName = 'Lightweight IDE prefs.txt';
// Constant used by SettingsFile

implementation
	
uses ColorCoding, LWPEdit, SettingsFile, FindReplaceDialog, AboutWindowUnit;// , BufferedTXNColorCodingUnit;

const
// Note: "Lightweight IDE 0.2.6 preferences" is too long for FSMakeFSSpec!
{$ifc defined CPUI386}
	kPrefsName = 'Lightweight IDE prefsH';
{$ELSEC}
	kPrefsName = 'Lightweight IDE prefsP'; // Other name for PPC version to avoid endian problems
{$ENDC}

var
//	appFile, prefFile: Integer;
	newSettingsDialog, newSettingsDialog2: WindowRef;
	
(*
const
	kCompilerPPCSTR = 128;
	kOptionsSTR = 129;
	kPathsSTR = 130;
	kLibPathsSTR = 131;
	kCCompilerSTR = 132;
	kCOptionsSTR = 133;
	kCompiler386STR = 134;
	kFrameworksSTR = 135;

	kJavaCompilerSTR = 136;
	kJavaInterpreterSTR = 137;
	kJavaOptionsSTR = 138;
	kOptionsLinkSTR = 139; {FPC linker options}
	kOptionsDebugSTR = 140; {FPC debug options}
	kOptionsReleaseSTR = 141; {FPC release options}
	kCOptionsDebugSTR = 142; {C debug options}
	kCOptionsReleaseSTR = 143; {C release options}

	kGPCCompilerSTR = 144;
	kGPCOptionsSTR = 145;

	kAdaCompilerSTR = 146;
	kAdaOptionsSTR = 147;
	kTargetArgsSTR = 148;

	kCudaCompilerSTR = 149;
	kCudaOptionsSTR = 150;

	kCompilerPPCSTR2 = 151; // ObjP
	kCompiler386STR2 = 152; // ObjP
	kCompileriOSSTR = 153; // iOS device
	kCompileriPhoneSimulatorSTR = 154; // iPhone simulator
	kiOSInterfacesPathSTR = 155; // iPhone interfaces path
	kCCompileriOSSTR = 156; // iPhone ObjC compiler version
	kiOSCodesignSTR = 157; // iPhone developer codesigning signature 0.9.1
*)

procedure DoSettings;
begin
	ShowWindow(newSettingsDialog);
	SelectWindow(newSettingsDialog);
end;

procedure DoAdvancedSettings;
begin
	ShowWindow(newSettingsDialog2);
	SelectWindow(newSettingsDialog2);
end;

function UpdateExtraFiles(theView: HIViewRef; myPtr: Pointer): Boolean; forward;

function OpenFilter(pb: CInfoPBPtr): Boolean; MWPascal;
begin
	return false; // false = don't skip
end;

procedure DoSettingsAddFile(var data: AnsiString; resCode: OSType);
// Modified 2016-05-18 to handle multiple text boxes
// choice 0 = gSettings.fileList and Xfil 0
// choice 1 = gSettings.customInfoPlist and CIPL 0
// choice 2 = gSettings.customResources and Xres 0
// Called from DoCommand. (Should be changed to button-specific callback in TS5.)
var
	reply: ReplyArray;
	typeList: SFTypeList;
	i: Longint;
	filePath, mainFile: FSSpecString;
	fw: WindowRef;
begin
	// Add to gSettings.fileList
	typeList[0] := FOUR_CHAR_CODE('TEXT');
	typeList[1] := FOUR_CHAR_CODE('****');
	StandardGetFiles(@OpenFilter, 1, @typeList, reply);
	for i := 0 to High(reply) do
		begin
			if reply[i].sfGood then
			begin
				WriteLn(reply[i].filePath);
				filePath := reply[i].filePath;
				// Cut away path if same as main file!
				fw := GetFrontMainWindow;
				if GetEditFSSpec(fw, mainFile) = noErr then
				begin
					mainFile := TrimLastToken(mainFile); // Don't match the file name!
					WriteLn('Comparing "', mainFile, '" to "', Copy(filePath, 1, Length(mainFile)), '"');
					if mainFile = Copy(filePath, 1, Length(mainFile)) then
						filePath := Copy(filePath, Length(mainFile)+2, Length(filePath)); // +1 to get past last character, +1 to get past "/"
					WriteLn('Main file: ', mainFile);
					WriteLn('Shortened: ', filePath);
				end;
				
				if Length(data) = 0 then
					data := filePath
				else
					if data[Length(data)] = #13 then
						data += filePath
					else
						data := data + #13 + filePath;
			end;
			VMSetStringValue(newSettingsDialog, resCode, 0, data); // Xfil 0 old case
			UpdateExtraFiles(VMGetControl(newSettingsDialog, resCode, 0), @data);
		end;
end;

function GetViewValueString(theView: HIViewRef; theViewData: ViewDataPtr): AnsiString;
var
//	err: OSErr;	
	theString: AnsiString;
	theCFStr: CFStringRef;
	theLong: Longint;
//	theBoolean: Boolean;
//	localData: Pointer;
//	control: ControlRef;
begin
	WriteLn('GetViewValueString');
	if theViewData <> nil then
	case theViewData^.dataType of
		kViewDataString, kViewDataNumString, kViewDataShortString:
		begin
//			WriteLn('GetViewValueString for string type');

			theCFStr := HIViewCopyText(theView);
			if theCFStr = nil then WriteLn('No text!');
//			WriteLn('HIViewCopyText');
			theString := GetStringFromCFString(theCFStr);
//			theString := CFStringGetCStringPtr(theCFStr, CFStringGetSystemEncoding);
			GetViewValueString := theString;
//			WriteLn('GetStringFromCFString returned length ', Length(theString));
			CFRelease(theCFStr);
		end;
		kViewDataLongint, kViewDataBoolean:
		begin
//			WriteLn('GetViewValueString for integer or boolean type');

			theLong := GetControlValue(theView);
			Str(theLong, theString);
			GetViewValueString := theString;
		end;
		otherwise
		begin
			WriteLn('GetViewValueString for unknown type');
			GetViewValueString := 'UNDEFINED';
		end;
	end {case}
	else
		WriteLn('No view data');
end;

// Variant of UpdateSettings for settings that are always global
function UpdateGlobalSettings(theView: HIViewRef; myPtr: Pointer): Boolean;
var
	controlID: HIViewID;
	tagString, valueString: AnsiString;
	theViewData: ViewDataPtr;
	err: OSErr;
	idStr, ss: String;
	p: PChar;
begin
	// NEW:
	// Create tag from control id
	// Save to settings array
	// Save to file
	err := GetControlID(theView, controlID);
	if err <> noErr then
		WriteLn('GetControlID err ', err);
	
	// Why does this work while the BuildControlTagString function does not?
	Str(controlID.id, idStr);
	p := PChar(@controlID.signature);
	ss := p[3] + p[2] + p[1] + p[0] + idStr; // This might be endian sensitive!
	WriteLn(ss);
	tagString := ss;
//	tagString := BuildControlTagString(controlID.id, controlID.signature); // ss + idStr;
	WriteLn('UpdateGlobalSettings for ', ss);
	
	// But what is the data?
	// Get value of control?
	// myPtr points to it, but what is the TYPE?
	// Worthless! New handler which only gives string output.
	// Or if we get the ViewDataRec? Or ask for it?
	theViewData := GetViewDataForView(theView); // GetCurrentViewData;
	if theViewData = nil then
		WriteLn('Noooo');
	valueString := GetViewValueString(theView, theViewData);
	// Save to current document's settings
	SetGlobalSetting(tagString, valueString); // save to global
	SaveGlobalSettings(kPrefsName);
	WriteLn('Saved global settings!');

	return true; // eller false?
end;

// Not working?
function BuildControlTagString(signature: OSType; id: SInt32): AnsiString;
var
	idStr, ss: String;
	p: PChar;
begin
	Str(id, idStr);
	p := PChar(@signature);
	ss := p[3] + p[2] + p[1] + p[0] + idStr; // This might be endian sensitive!
	BuildControlTagString := ss;
end;

// Callback for view, called when it is changed.
function UpdateSettings(theView: HIViewRef; myPtr: Pointer): Boolean;
var
	controlID: HIViewID;
	tagString, valueString: AnsiString;
	theViewData: ViewDataPtr;
	err: OSErr;
	editIndex: Longint;
	idStr, ss: String;
	p: PChar;
begin
	// NEW:
	// Create tag from control id
	// Save to settings array
	// Save to file
	err := GetControlID(theView, controlID);
	if err <> noErr then
		WriteLn('GetControlID err ', err);
	
	// Why does this work while the BuildControlTagString function does not?
	Str(controlID.id, idStr);
	p := PChar(@controlID.signature);
	ss := p[3] + p[2] + p[1] + p[0] + idStr; // This might be endian sensitive!
	WriteLn(ss);
	tagString := ss;
//	tagString := BuildControlTagString(controlID.id, controlID.signature); // ss + idStr;
	
	// But what is the data?
	// Get value of control?
	// myPtr points to it, but what is the TYPE?
	// Worthless! New handler which only gives string output.
	// Or if we get the ViewDataRec? Or ask for it?
	theViewData := GetViewDataForView(theView); // GetCurrentViewData didn't work since this can be called without a valid current view.
	if theViewData = nil then
		WriteLn('Noooo');
	valueString := GetViewValueString(theView, theViewData);
	WriteLn('valueString=', valueString); // Correct up to here
	// Save to current document's settings
	editIndex := GetWindowNumber(GetFrontMainWindow);
	if editIndex < 1 then
	begin
		SetGlobalSetting(tagString, valueString); // save to global
		SaveGlobalSettings(kPrefsName);
	end
	else
	begin
		SetSetting(editIndex, tagString, valueString); // save to editIndex
		SaveSettings(editIndex);
	end;

	return true; // eller false?
end;

function UpdateExtraFiles(theView: HIViewRef; myPtr: Pointer): Boolean;
begin
	WriteLn('Extra file saves:', gSettings.fileList);
	WriteLn('Extra file saves:', VMGetStringValue(theView));
	WriteLn('Extra res saves:', gSettings.customResources);
	// Convert fileList to fileArray
	BuildListFromText(gSettings.fileList, gSettings.fileListArray);
	BuildListFromText(gSettings.customResources, gSettings.customResourceArray);

	UpdateExtraFiles := UpdateSettings(theView, myPtr);
end;

function UpdateTextSize(theView: HIViewRef; myPtr: Pointer): Boolean;
var
//	typeAttr: array [1..1] of TXNTypeAttributes;
//	myTextSize: Longint;
	i: Longint;
//	err: OSStatus;
//	controlTag: TXNControlTag;
//	controlData: TXNControlData;
begin
	WriteLn('UpdateTextSize');
	// Global setting - call UpdateGlobalSettings
	UpdateGlobalSettings(theView, myPtr); // Also update //	ChangedResource(Handle(gFlags)); // UpdateResFile(prefFile);
//	FontCode:=gSettings.textSaveFont;//08/07/20 DS For Font
// Removed by Ingemar; Read directly from the settings. Caused problem if left uninitialized.
	SetSintaxColor(gSettings.textSaveColorCoding);
		
	// Update text size in all editor windows AND the message window!
	for i := 1 to kMaxEditWindows do
	if teEdit[i] <> nil then
	begin
		WriteLn('The text size is ', GetSettingsTextSize);
		SetTextToNormal(teEdit[i], false, GetSettingsTextSize);
		WriteLn('The text size was changed');

//		SetTextToNormalBuffered(teEdit[i], false, GetSettingsTextSize);
//		BetterColorCoder(teEdit[i],i);
//		SetTXNVisibility(teEdit[i], false);
		WriteLn('The tab size is ', gSettings.tabSize);
		SetTabSize(teEdit[i], gSettings.tabSize);
		WriteLn('The tab size was changed to ', gSettings.tabSize);
		TouchColorCoder(i, 1);
		// Could be optimized by only changing size and not re-color
		// Tried it - dit it work?
	end;
	
	// Message window
	WriteLn('Text size of console set to ', GetSettingsTextSize);
	SetTextToNormal(teHelp^.h, true, GetSettingsTextSize);
	WriteLn('so now it is ', teHelp^.h^.fontSize);
	
	return true; // eller false?
end;


{ DS 08/09/06 }
{//Move a single window and call into order }
Procedure MoveOneWindow(Wind: WindowRef; WinPos, Position: integer);
const
	kOffsetDistance=20;
var 
    screenBits: BitMap;
    ScreenBounds,WinRect: Rect;
    Width,Height,hCount,vCount{,cCount}: integer;
begin 
	if Wind = nil then Exit(MoveOneWindow);
	screenBits := GetQDGlobalsScreenBits(screenBits)^;
	ScreenBounds := screenBits.bounds;
	case Position of
		1: SetRect(WinRect,355,45,900,700);//Standard LWP window size
		2: SetRect(WinRect,20,45,ScreenBounds.right-60,(ScreenBounds.bottom div 10)*4); //Middle Edit Up;
		3: SetRect(WinRect,20,(ScreenBounds.bottom div 10)*4+45,ScreenBounds.right-60,ScreenBounds.bottom-68);
		4:
		begin
			if WinPos Mod 2 = 1 then
				SetRect(WinRect,20,45,ScreenBounds.right-60,(ScreenBounds.bottom div 10)*4)
			else
				SetRect(WinRect,20,(ScreenBounds.bottom div 10)*4+45,ScreenBounds.right-60,ScreenBounds.bottom-68);   
		end;
		5:
		begin
			if WinPos Mod 2 = 1 then
				SetRect(WinRect,20,45,(ScreenBounds.right Div 2)-60,ScreenBounds.bottom-68)
			else
				SetRect(WinRect,(ScreenBounds.right Div 2)+20,45,ScreenBounds.right-60,ScreenBounds.bottom-68);     
		end;
		otherwise
			SetRect(WinRect,355,45,900,700);//Standard LWP window size
			// Default added by Ingemar 081009
	end; {case}
	
	width := screenBounds.right - winrect.right;
	height := screenBounds.bottom - winrect.bottom;
	if Position >= 4 then
		WinPos := (WinPos div 2) + 1;
	hCount := width div kOffsetDistance;
	vCount := height div kOffsetDistance;
	hCount := (winpos-1) mod hCount;
	vCount := (winpos-1) mod vCount;
	
	MoveWindow(wind,
		WinRect.left + hCount * kOffsetDistance,
		WinRect.top + vCount * kOffsetDistance, true);
	SizeWindow(Wind,
		(WinRect.Right-WinRect.left), 
		(WinRect.Bottom-WinRect.top),true);
End;
//DS
//Move all windows. it s called in Init settings

function MoveAllWindows(theView:HIViewRef; myPtr:Pointer):boolean;
var
	I,count:Integer;
begin
	Count:=0;
	for I:=1 to kMaxEditWindows do
	begin
		if editwind[I]<>nil then 
		Begin
			count:=Count+1; 
			MoveOneWindow(editWind[i],Count,gSettings.EditWindowPosition);
		End;
	End;

	// Must also save the setting!
	UpdateSettings(theView, myPtr);

	Return True;
End;

procedure DoClose;
var
	thePort: GrafPtr;
begin
	GetPort(thePort);			{ grafport of window to be closed }
	HideWindow(GetWindowFromPort(thePort));
end;

procedure DefaultPascalCompilerSettings;
var
	c: ControlRef;
begin
	SysBeep(1);
	
	gSettings.PPCCompiler := kDefaultPPCCompiler;
	VMSetStringValue(newSettingsDialog2, 'PPCc', 0, gSettings.PPCCompiler);
	gSettings.x86Compiler := kDefault386Compiler;
	VMSetStringValue(newSettingsDialog2, 'x86c', 0, gSettings.x86Compiler);
	gSettings.GPCCompiler := kDefaultGPCCompiler;
	VMSetStringValue(newSettingsDialog2, 'GPCc', 0, gSettings.GPCCompiler);

// ObjP
//	gSettings.PPCCompiler2 := kDefaultPPCCompiler2; // '/usr/local/lib/fpc/2.5.1/ppcppc';
//	VMSetStringValue(newSettingsDialog2, 'PPCc', 1, gSettings.PPCCompiler2);
//	gSettings.386Compiler2 := kDefault386Compiler2; // '/usr/local/lib/fpc/2.5.1/ppc386';
//	VMSetStringValue(newSettingsDialog2, 'x86c', 1, gSettings.386Compiler2);

// iOS/iPhone
	gSettings.iOSCompiler := kDefaultiOSCompiler;
	VMSetStringValue(newSettingsDialog2, 'iOSc', 0, gSettings.iOSCompiler);
	gSettings.iPhoneSimulatorCompiler := kDefaultiPhoneSimulatorCompiler;
	VMSetStringValue(newSettingsDialog2, 'SIMc', 0, gSettings.iPhoneSimulatorCompiler);
	gSettings.iOSInterfacesPath := kDefaultiOSInterfacesPath;
	VMSetStringValue(newSettingsDialog2, 'iOSp', 0, gSettings.iOSInterfacesPath);

	gSettings.iOSObjCCompiler := kDefaultiOSCCompiler;
	VMSetStringValue(newSettingsDialog2, 'iOSm', 0, gSettings.iOSObjCCompiler);

	gSettings.iOSCodesign := kDefaultiOSCodesign;

// TO DO
	VMSetStringValue(newSettingsDialog2, 'cSgn', 0, gSettings.iOSCodesign);
//	c := VMGetControl(newSettingsDialog2, 'cSgn', 0);
//	VMSetStringValue(c, gSettings.iOSCodesign);

//	UpdateSettings(newSettingsDialog2. nil); INCORRECT - takes a view!
// Should not be needed? Automatic on VMSetStringValue?
end;

(*
procedure ExperimentalPascalCompilerSettings;
begin
	gSettings.PPCCompiler := '/usr/local/lib/fpc/2.5.1/ppcppc';
	VMSetStringValue(newSettingsDialog2, 'PPCc', 0, gSettings.PPCCompiler);
	gSettings.386Compiler := '/usr/local/lib/fpc/2.5.1/ppc386';
	VMSetStringValue(newSettingsDialog2, 'x86c', 0, gSettings.386Compiler);

	UpdateSettings(newSettingsDialog2, nil);
end;
*)

procedure DefaultCCompilerSettings;
begin
	SysBeep(1);
	
	gSettings.CCompiler := kDefaultCCompiler;
	VMSetStringValue(newSettingsDialog2, 'GCCc', 0, gSettings.CCompiler);

	gSettings.CudaCompiler := kDefaultCudaCompiler;
	VMSetStringValue(newSettingsDialog2, 'CUDc', 0, gSettings.CudaCompiler);
	gSettings.CudaOptions := kDefaultCudaOptions;
	VMSetStringValue(newSettingsDialog2, 'CUDo', 0, gSettings.CudaOptions);

//	UpdateSettings(newSettingsDialog2, nil); INCORRECT - takes a view!
// Should not be needed? Automatic on VMSetStringValue?
end;


procedure InstallSettingsControl(dlg: WindowRef; signature: OSType; id: SInt32; theViewDataType: ViewDataType;
								editIndex: Longint; callback: VMPointerProcPtr; defaultValue: AnsiString; variablePtr: Pointer);
var
	postValue: AnsiString;
	control: ControlRef;
	postNumValue: Longint;
	tagString: String;
	p:PChar;
	ss: AnsiString;
	idStr: String;
begin
//	tagString := BuildControlTagString(signature, id);
	
	// Why does this work while the BuildControlTagString function does not?
	Str(id, idStr);
	p := PChar(@signature);
	ss := p[3] + p[2] + p[1] + p[0] + idStr; // This might be endian sensitive!
//	WriteLn(ss);
	tagString := ss;
//	WriteLn('InstallSettingsControl for ', ss);

	if editIndex < 1 then
		postValue := GetGlobalSetting(tagString, defaultValue) // Get the value from the settings
	else
		postValue := GetSetting(editIndex, tagString); // Get the value from the settings
	control := VMGetControl(dlg, signature, id); // Get the control reference
	if control = nil then
		WriteLn('No control in InstallSettingsControl (', tagString, ')');
	case theViewDataType of
		kViewDataString:
		begin
			VMSetStringValue(control, postValue); // Set the value of the control
//			WriteLn('Set text control to ', postValue);
			if variablePtr <> nil then
				AnsiStringPtr(variablePtr)^ := postValue; // Maybe the initial value of the variable overwrites the setting?
	//	kViewDataNumString: // Numeric data shown as text
	//		VMSetNumStringValue(control, postValue); // Set the value of the control
		end;
		kViewDataNumString: // Number in text box (i.e. tab size)
		begin
			Val(postValue, postNumValue);
			VMSetStringValue(control, postValue); // Set the value of the control
//			WriteLn('Set text control to ', postValue);
			if variablePtr <> nil then
				LongPtr(variablePtr)^ := postNumValue;
	//	kViewDataNumString: // Numeric data shown as text
	//		VMSetNumStringValue(control, postValue); // Set the value of the control
		end;
		kViewDataLongint:
		begin
			Val(postValue, postNumValue);
//			WriteLn('Set numeric control to ', postNumValue);
			SetControl32BitValue(control, postNumValue);
			VMSetNumValue(control, postNumValue);
			if variablePtr <> nil then
				LongPtr(variablePtr)^ := postNumValue;
		end;
		kViewDataBoolean:
		begin
//			WriteLn('Set boolean control to ', postValue);
			if postValue = '0' then
				VMSetBooleanValue(control, false)
			else
				VMSetBooleanValue(control, true);
			if variablePtr <> nil then
				BooleanPtr(variablePtr)^ := postValue = '1';
		end;
	end; // case
	
	// We assume that all controls are installed when loading global data
	if editIndex < 1 then
		InstallViewHandlerByRef(dlg, control, theViewDataType, variablePtr, callback, nil);
end;


// Loads settings for window or global settings.
// editIndex < 1 means that we load global settings.
procedure LoadSettingsForWindow(editIndex: Longint);
var
	major, minor, bugfix: Integer;
begin
	// Load all settings, set value of each control
	// Can be automated! Each setting is coded by its control ID!
	
	if editIndex >= 1 then
		LoadSettings(editIndex)
	else
		LoadGlobalSettings(kPrefsName);

//WriteLn('Did we get here 10?');
	
	// For each control, set value from setting (and install handler - only needed first time).
	// Tells data type (how to handle the control), how to find it in settings (sig+id), what to call when it changes.
	// InstallSettingsControl(dlg: WindowRef; signature: OSType; id: SInt32; theViewDataType: ViewDataType; editIndex, @UpdateSettings);
	// VERIFY: Is it legal to install handlers several times for the same control?
	// NO - it allocates memory. Must dispose old data!
	// Change InstallViewHandlerByRef to keep pointer and dispose when needed?
	// Or just skip installation for non-global. Easier but uglier.

// Settings for Search/replace
	InstallSettingsControl(searchWind, 'Igno', 0, kViewDataBoolean, editIndex, @UpdateGlobalSettings, '0', @gSettings.searchIgnoreCase);
	InstallSettingsControl(searchWind, 'Enti', 0, kViewDataBoolean, editIndex, @UpdateGlobalSettings, '0', @gSettings.searchEntireWord);
	InstallSettingsControl(searchWind, 'Wrap', 0, kViewDataBoolean, editIndex, @UpdateGlobalSettings, '0', @gSettings.searchWrap);

// Global settings
	InstallSettingsControl(newSettingsDialog, 'Save', 0, kViewDataBoolean, editIndex, @UpdateGlobalSettings, '0', @gSettings.autoSave);
	InstallSettingsControl(newSettingsDialog, 'Inde', 0, kViewDataBoolean, editIndex, @UpdateGlobalSettings, '1', @gSettings.autoIndent); // Auto-indent
	InstallSettingsControl(newSettingsDialog, 'Form', 0, kViewDataBoolean, editIndex, @UpdateGlobalSettings, '0', @gSettings.autoFormat); // Auto-format

// Compiler settings
	InstallSettingsControl(newSettingsDialog, 'ClOp', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.options); // FPC Command-line options
	InstallSettingsControl(newSettingsDialog, 'LiOp', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.linkOptions); // FPC linker options
	InstallSettingsControl(newSettingsDialog, 'DbOp', 0, kViewDataString, editIndex, @UpdateSettings, '-gw', @gSettings.debugOptions); // Debug build options
	InstallSettingsControl(newSettingsDialog, 'RlOp', 0, kViewDataString, editIndex, @UpdateSettings, '-O2 -XX', @gSettings.releaseOptions); // Release build options
	InstallSettingsControl(newSettingsDialog, 'DbCO', 0, kViewDataString, editIndex, @UpdateSettings, '-g', @gSettings.CDebugOptions); // C debug build options
	InstallSettingsControl(newSettingsDialog, 'RlCO', 0, kViewDataString, editIndex, @UpdateSettings, '-O2', @gSettings.CReleaseOptions); // C release build options

	InstallSettingsControl(newSettingsDialog, 'CClO', 0, kViewDataString, editIndex, @UpdateSettings, '-fpascal-strings', @gSettings.COptions); // C options (removed -wall 150127)
	InstallSettingsControl(newSettingsDialog, 'CCSt', 0, kViewDataLongint, editIndex, @UpdateSettings, '4', @gSettings.CCompileStrategy); // C compile strategy

// Pick a reasonable default
{$IFDEF CPUPOWERPC}
	InstallSettingsControl(newSettingsDialog, 'Targ', 0, kViewDataLongint, editIndex, @UpdateSettings, '1', @gSettings.target); // Target options 1 = PPC
{$ELSE}
	GetSystemVersion(major, minor, bugfix);
	if minor > 6 then
		InstallSettingsControl(newSettingsDialog, 'Targ', 0, kViewDataLongint, editIndex, @UpdateSettings, '2', @gSettings.target) // Target options 2 = 386
	else
		InstallSettingsControl(newSettingsDialog, 'Targ', 0, kViewDataLongint, editIndex, @UpdateSettings, '3', @gSettings.target); // Target options 3 = Universal
{$ENDIF}

	InstallSettingsControl(newSettingsDialog, 'GClO', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.GPCOptions); // GPC options
	
	InstallSettingsControl(newSettingsDialog, 'Javc', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/bin/javac', @gSettings.JavaCompiler); // Java compiler
	InstallSettingsControl(newSettingsDialog, 'Java', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/bin/java', @gSettings.JavaInterpreter); // Target interpreter
	InstallSettingsControl(newSettingsDialog, 'JavO', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.JavaOptions); // Java options

	InstallSettingsControl(newSettingsDialog, 'Adac', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/local/gnat/bin/gnatmake', @gSettings.AdaCompiler); // Ada compiler
	InstallSettingsControl(newSettingsDialog, 'AdaO', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.AdaOptions); // Ada options

	InstallSettingsControl(newSettingsDialog, 'Tcmd', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.targetArgs); // Command-line arguments to target

	InstallSettingsControl(newSettingsDialog, 'FrmW', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.frameworks); // Frameworks
	InstallSettingsControl(newSettingsDialog, 'Path', 0, kViewDataString, editIndex, @UpdateSettings, '/Developer/MyStandardUnits', @gSettings.paths); // Paths
	InstallSettingsControl(newSettingsDialog, 'LibP', 0, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.libPaths); // Library paths
	InstallSettingsControl(newSettingsDialog, 'AFra', 0, kViewDataBoolean, editIndex, @UpdateSettings, '1', @gSettings.autoFramework); // Auto-framework


	GetSystemVersion(major, minor, bugfix);
	// Bug fix: The check below was incorrect. Now it should be OK. 160318
	if minor < 9 then
		InstallSettingsControl(newSettingsDialog, 'GDBp', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/bin/gdb', @gSettings.gdb) // Path to GDB
		//InstallSettingsControl(newSettingsDialog, 'GDBp', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/bin/gdb', @gSettings.gdb) // Path to GDB
	else
		InstallSettingsControl(newSettingsDialog, 'GDBp', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/bin/lldb', @gSettings.gdb); // Path to LLDB
	
// These are global with special-purpose callbacks
	InstallSettingsControl(newSettingsDialog, 'Tpop', 0, kViewDataLongint, editIndex, @UpdateTextSize, '2', @gSettings.textSize); // Text size
	InstallSettingsControl(newSettingsDialog, 'Pfon', 0, kViewDataLongint, editIndex, @UpdateTextSize, '1', @gSettings.textSaveFont); // Font
	InstallSettingsControl(newSettingsDialog, 'ewis', 0, kViewDataLongint, editIndex, @MoveAllWindows, '1', @gSettings.editWindowPosition); // Move windows
	InstallSettingsControl(newSettingsDialog, 'phew', 0, kViewDataLongint, editIndex, @MoveAllWindows, '1', @gSettings.helpErrorWindowPosition); // Window position
	
	InstallSettingsControl(newSettingsDialog, 'PCol', 0, kViewDataLongint, editIndex, @UpdateTextSize, '1', @gSettings.textSaveColorCoding); // Color coding mode? 1 = LWP_ColorCoding
	InstallSettingsControl(newSettingsDialog, 'TxFm', 0, kViewDataLongint, editIndex, @UpdateTextSize, '2', @gSettings.textSaveFormat); // Text save format 160622 Changed to 2 = Unix
	InstallSettingsControl(newSettingsDialog, 'Ttab', 0, kViewDataNumString, editIndex, @UpdateTextSize, '4', @gSettings.tabSize); // Tab size

	InstallSettingsControl(newSettingsDialog, 'Pmod', 0, kViewDataLongint, editIndex, @UpdateSettings, '1', @gSettings.pascalMode); // Pascal mode, FPC (1) or GPC (2)
	InstallSettingsControl(newSettingsDialog, 'Xfil', 0, kViewDataString, editIndex, @UpdateExtraFiles, '', @gSettings.fileList); // Extra files
	// New 2016-05-18
	InstallSettingsControl(newSettingsDialog2, 'CIPL', 0, kViewDataString, editIndex, @UpdateExtraFiles, '', @gSettings.customInfoPlist); // Custom info.plist
	InstallSettingsControl(newSettingsDialog2, 'Xres', 0, kViewDataString, editIndex, @UpdateExtraFiles, '', @gSettings.customResources); // Custom res
//	UpdateExtraFiles(VMGetControl(newSettingsDialog, 'Xfil', 0), @gSettings.fileList);
// TEST: Why is this needed?

//WriteLn('Did we get here 90?');

// Advanced settings
	InstallSettingsControl(newSettingsDialog2, 'PPCc', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/local/bin/ppcppc', @gSettings.PPCCompiler); // Pascal compiler, PPC
	InstallSettingsControl(newSettingsDialog2, 'x86c', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/local/bin/ppc386', @gSettings.x86Compiler); // Pascal compiler, Intel
	InstallSettingsControl(newSettingsDialog2, 'GCCc', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/bin/gcc', @gSettings.CCompiler); // C compiler
	InstallSettingsControl(newSettingsDialog2, 'GPCc', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/bin/gp', @gSettings.GPCCompiler); // GPC compiler
	InstallSettingsControl(newSettingsDialog2, 'CUDc', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/local/cuda/bin/nvcc', @gSettings.CudaCompiler); // CUDA compiler
	InstallSettingsControl(newSettingsDialog2, 'CUDo', 0, kViewDataString, editIndex, @UpdateSettings, '-L /usr/local/cuda/lib -lcudart', @gSettings.CudaOptions); // Pascal mode, FPC (1) or GPC (2)
	
//	InstallSettingsControl(newSettingsDialog2, 'PPCc', 1, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.PPCCompiler2); // Pascal compiler, PPC (obsolete???)
//	InstallSettingsControl(newSettingsDialog2, 'x86c', 1, kViewDataString, editIndex, @UpdateSettings, '', @gSettings.x86Compiler2); // Pascal compiler, Intel (obsolete???)
	InstallSettingsControl(newSettingsDialog2, 'iOSc', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/local/lib/fpc/2.5.1/ppcrossarm', @gSettings.iOSCompiler); // iOS compiler
	InstallSettingsControl(newSettingsDialog2, 'SIMc', 0, kViewDataString, editIndex, @UpdateSettings, '/usr/local/lib/fpc/2.5.1/ppcross386', @gSettings.iPhoneSimulatorCompiler); // iOS simulator compiler
	InstallSettingsControl(newSettingsDialog2, 'iOSp', 0, kViewDataString, editIndex, @UpdateSettings, '/Developer/ObjectivePascal/uikit-skel/src', @gSettings.iOSInterfacesPath); // CUDA compiler
	InstallSettingsControl(newSettingsDialog2, 'iOSm', 0, kViewDataString, editIndex, @UpdateSettings, 'arm-apple-darwin10-gcc-4.2.1', @gSettings.iOSObjCCompiler); // iOS ObjC compiler
//WriteLn('Did we get here 100?');
	InstallSettingsControl(newSettingsDialog2, 'cSgn', 0, kViewDataString, editIndex, @UpdateSettings, 'Your developer signature here', @gSettings.iOSObjCCompiler); // Codesigning string

// This must be re-run to test if the compiler settings are usable!
	CheckCompilersExists;
end;


// SETTINGS SHOULD BE MOVED TO FILES, NOT RESOURCES
function InitSettings: Boolean;
//var
//	err: OSErr;
//	foundVRefNum: Integer;
//	foundDirID: Longint;
//	prefSpec: FSSpec; // BORDE BYTAS
//	s: Str255;
//	major, minor, bugfix: Integer;
begin
	InitSettings := false;


	// New settings dialog!
	CreateWindowFromNib(SkelGetMainNib, CFSTR('LWPSettings'), newSettingsDialog);
	InstallAllTabs(newSettingsDialog);
	SkelWindow(newSettingsDialog, nil, nil, nil, nil, @DoClose, nil, nil, false); // Activate event at edit windows vital for updating current main window!

	// NEW in 0.8: Additional, advanced settings dialog!
	CreateWindowFromNib(SkelGetMainNib, CFSTR('LWPSettings2'), newSettingsDialog2);
	InstallAllTabs(newSettingsDialog2);
	SkelWindow(newSettingsDialog2, nil, nil, nil, nil, @DoClose, nil, nil, false);

//	// New way to init settings
//	LoadGlobalSettings(kPrefsName);
//	// For all controls, get the global setting, set the value to that and install the view handler
//	InstallSettingsControl(dlg: WindowRef; signature: OSType; id: SInt32; controlType: Integer, @UpdateSettings);
// SEE BELOW - LoadSettingsForWindow
	LoadSettingsForWindow(-1);
	
	// Bug fix?
	SetSintaxColor(gSettings.textSaveColorCoding);	
	end;



function GetSettingsTextSize: Longint;
begin
	case gSettings.textSize of
		1: GetSettingsTextSize := 9;
		2: GetSettingsTextSize := 10;
		3: GetSettingsTextSize := 11;
		4: GetSettingsTextSize := 12;
		5: GetSettingsTextSize := 13;
		6: GetSettingsTextSize := 14;
		7: GetSettingsTextSize := 16;
		otherwise
			GetSettingsTextSize := 18;
	end;
end;

//function GetSettingsTabSize: Longint;
//begin
//	WriteLn(gSettings.tabSize);
//	return gSettings.tabSize;
//end;	

//Begin


function IsCommandLineToolMode(mainProgramSpec: FSSpecString): Boolean;
begin
//		if gSettings.commandLineToolMode then
//		if FileExists(TrimExtension(theSpec) + ' resources') then

// The way it should work (once I can fix the GUI - except if we just keep auto):
//	case gSettings.commandLineToolMode of
//		kCommandLineToolModeOff: return(false);
//		kCommandLineToolModeOn: return(true);
//		kCommandLineToolModeAuto: return(not FileExists(TrimExtension(mainProgramSpec) + ' resources'));
//	end;
// I decided to make it all auto!

// Should test for more than the folder: .rsrc, alternate folder names (all resources, all.resources, name.resources, icon...)

// Old style:
//		return(gSettings.commandLineToolMode);

	if GetExtensionType(mainProgramSpec) = kExtTypeCuda then
		return false; // CUDA really likes to be in an application bundle!
	
	
	if not FileExists(TrimExtension(mainProgramSpec) + ' resources') then
		WriteLn('IsCommandLineToolMode: ', TrimExtension(mainProgramSpec) + ' resources', ' does not exist');

	if FileExists(TrimExtension(mainProgramSpec) + ' resources') then
		return false;
	if FileExists(TrimExtension(mainProgramSpec) + ' frameworks') then
		return false;
	if FileExists(TrimExtension(mainProgramSpec) + '.nib') then
		return false;
	if FileExists(TrimExtension(mainProgramSpec) + '.xib') then
		return false;
	if FileExists(TrimExtension(mainProgramSpec) + '.rsrc') then
		return false;
	if FileExists(TrimExtension(mainProgramSpec) + '.icns') then
		return false;
	return true;

// Auto only:
//	return(not FileExists(TrimExtension(mainProgramSpec) + ' resources'));
end;

procedure SetSettingsDialogItemString(signature: OSType; id: SInt32; value: AnsiString);
begin
	VMSetStringValue(newSettingsDialog, signature, id, value);
end;
procedure SetSettingsDialog2ItemString(signature: OSType; id: SInt32; value: AnsiString);
begin
	VMSetStringValue(newSettingsDialog, signature, id, value);
end;


end.

// Fundering om settings.
// Fyra olika saker hŠnger ihop: defaultvŠrde, variabel, resursnummer och kontroll-ID.
// Till exempel kDefaultiOSCCompiler, gSettingsiOSObjCCompiler, iOSm, kCCompileriOSSTR
// AnvŠnds i olika situationer:
// - Installera kontrollen
// - LŠs in gamla vŠrdet eller skapa spardata
// - SŠtt till default
// - Uppdatera vid Šndring (via callback nŠr kontrollen installeras)
// Kan man inte fixa nŒgot sŠtt att definiera allihop pŒ en enda gŒng, i ett anrop?
// Kanske en datastruktur som skapar per instŠllning?




// Tillagt delvis fšr 0.9.1
// Delvis eftersom IB krŒnglar
//gSettingsiOSCodesign: AnsiString; {0.9.1}
//kDefaultiOSCodesign
//kiOSCodesignSTR
