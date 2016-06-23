// Utilities for working with TXN (aka MLTE).
// Updated 081030: Added row-based functions (for breakpoints)
// originating in the MoreTXN unit in the CustomTXN demos.

// Some functions might survive for Halda but most will be scrapped.
// 141118: Cleaned out all TXN junk.

{$mode macpas}
{$setc IncludePotentiallyBuggyStuff = true}

{$ifc IncludePotentiallyBuggyStuff}
{$endc}

unit TXNUtilities;
interface
uses
	MacOSAll, LWPColorGlobals, Halda, HaldaTypes, HaldaSysDept;

procedure SetTextToNormal(teRec: HaldaPtr; setColorBlack: Boolean; theTextSize: Longint);

//procedure SetTabSize(te: TXNObject; tabSize: Longint);
procedure SetTabSize(te: HaldaPtr; tabSize: Longint);

const
	{kColorBlue = 0;
	kColorRed = 1;
	kColorGrey = 2;
	kColorGreen = 3;
	kColorBlack = 4;
	kColorYellow=5;}
	kStyleNormal = 0;
	kStyleBold = 1;
	kStyleItalic = 2;
//	FFONT:Array[1..5] of Str255=(('Monaco'),('Geneva'),('Times New Roman'),('Courier'),('Andale Mono'));// 08/07/19 For coding font
	FFONT:Array[1..3] of Str255=(('Monaco'),('Courier'),('Andale Mono'));// 08/07/19 For coding font 160622: Removed non-monospace fonts


//Var FontCode:LongInt;// 08/07/19 For coding font

// procedure SetInterval(teRec: TXNObject; tokenStart, tokenEnd, colorNumber, styleNumber: Longint);
procedure SetInterval(teRec: HaldaPtr; tokenStart, tokenEnd, colorNumber, styleNumber: Longint);

// Workaround for 10.5 TXN bug
procedure GetSystemVersion(var major, minor, bugfix: Integer);

implementation

uses
	Settings;

procedure SetTextToNormal(teRec: HaldaPtr; setColorBlack: Boolean; theTextSize: Longint);
var
//	typeAttr: array [1..4] of TXNTypeAttributes;
//	err: OSStatus;
//	myTextSize: Longint;
	color: HaldaColor;
//	typeCount: Longint;
	fontNum: Integer;
	fontName: Str255;
//	TXNBackColor: TXNBackground;//08/07/18 DS For backgroud edit window
begin
	// Font!
	GetFNum(FFONT[gSettings.textSaveFont], fontNum);
	// Hard-coded! Font selection?
	fontName := FFONT[gSettings.textSaveFont];

//WriteLn('SetTextToNormal 1');
	
//	SetPortWindowPort(view.window);
	color.col := 0; {blackColor}
	if setColorBlack then
		HSetStyle(teRec, 1, Length(teRec^.text), 0 {style}, color);
// Set background too?
//WriteLn('SetTextToNormal 2');
	HSetFont(teRec, fontName, theTextSize);
//WriteLn('SetTextToNormal 2.1');
	HInvalViews(teRec);

//WriteLn('SetTextToNormal 3');

//	WriteLn('SetTextToNormal');
//	SetTabSize(teRec, GetSettingsTabSize);
end;

// Primarily for color coding

	procedure SetInterval(teRec: HaldaPtr; tokenStart, tokenEnd, colorNumber, styleNumber: Longint);
	var
//		typeAttr: array [1..3] of TXNTypeAttributes;
		color: RGBColor;
//		err: OSErr;
		theStyle: Style;
	begin
{		color.red := 0;
		color.green := 0;
		color.blue := 0;}
//		typeAttr[1].data.dataValue := Longint(styleNumber);
		
		Color:=ColorList[LWPSintax.VForeColor];
		If colorNumber<>LWPSintax.VForeColor then
			Color:=ColorList[ColorNumber];
		
		theStyle := 0;
		case styleNumber of
			kStyleNormal: ;
			kStyleBold: theStyle := theStyle + bold;
			kStyleItalic: theStyle := theStyle + italic;
		end;

		HSetStyle(teRec, tokenStart, tokenEnd, theStyle, HSDRGBColorToHaldaColor(color));

// Style is not correct, and not all that needed.
//		typeAttr[1].tag := kTXNQDFontColorAttribute; // FŠrg
//		typeAttr[1].size := kTXNQDFontColorAttributeSize;
//		typeAttr[1].data.dataValue := Longint(@color); // svart!
//		typeAttr[2].tag := kTXNQDFontStyleAttribute;
//		typeAttr[2].size := kTXNQDFontStyleAttributeSize;
//		typeAttr[2].data.dataValue := theStyle; // Longint(@theStyle);
//{$ifc IncludePotentiallyBuggyStuff}
//			err := TXNSetTypeAttributes(teRec, 2, @typeAttr[1], tokenStart, tokenEnd+1);
//{$endc}
	end;

procedure SetTabSize(te: HaldaPtr; tabSize: Longint);
//var
//	controlTag: TXNControlTag;
//	controlData: TXNControlData;
//	err: OSErr;
begin
	if tabSize <= 1 then
		tabSize := 1;
	if tabSize > 8 then
		tabSize := 8;
	te^.tabWidth := tabSize;
//	controlTag := kTXNTabSettingsTag;
//	controlData.tabValue.value := tabSize;
//	controlData.tabValue.tabType := kTXNLeftTab;
//	controlData.tabValue.filler := 0;
//	err := TXNSetTXNObjectControls (te, false, 1, @controlTag, @controlData);

//	TXNRecalcTextLayout(te);
end;

// Utilty function that can be moved to some reusable code module (copied here from ProcessUtils)
procedure GetSystemVersion(var major, minor, bugfix: Integer);
// Missing in FPCMacOSAll:
const
	gestaltSystemVersionMajor     = 'sys1'; //* The major system version number; in 10.4.17 this would be the decimal value 10 */
	gestaltSystemVersionMinor     = 'sys2'; //* The minor system version number; in 10.4.17 this would be the decimal value 4 */
	gestaltSystemVersionBugFix    = 'sys3'; //* The bug fix system version number; in 10.4.17 this would be the decimal value 17 */
var
	vers: Longint;
	err: OSErr;
begin
	// Hope for 10.4 or better, check for gestaltSystemVersionMajor/Minor/Bugfix
	err := Gestalt(gestaltSystemVersionMajor, vers);
	major := vers;
	if err = noErr then
	begin
		err := Gestalt(gestaltSystemVersionMinor, vers);
		minor := vers;
	end;
	if err = noErr then
	begin
		err := Gestalt(gestaltSystemVersionBugfix, vers);
		bugfix := vers;
	end;
	if err <> noErr then {Pre-10.4 must use gestaltSystemVersion}
	begin
		err := Gestalt(gestaltSystemVersion, vers);
		major := (vers SHR 8) and $ff;
		minor := (vers SHR 4) and $f;
		bugfix := vers and $f;
	end;
end;

end.
