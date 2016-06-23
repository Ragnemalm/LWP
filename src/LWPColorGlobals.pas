unit LWPColorGlobals;
{
DS

ColorGlobals.p

NOTE. 
is a unit ke sect colors of LWP, has a list of colors 
Pre and set a record of variables used by functions 
of ColorCoding.p. 

For now the colors are predefined but in the future will be programmable 
through an external file

NOTA.
è una unit ke setta i colori di LWP, dispone di una lista di colori 
pre impostati e un record di variabili riutilizate dalle funzioni
di ColorCoding.p.

Per il momento i colori sono predefiniti ma in futuro saranno programmabili
tramite un file esterno
0904??: Richard Ward added a second LWP color scheme

090416: Cleaned up a bit to conform with the rest. Changed the new to "2". /Ingemar
121109: Added support for individual styles in color schemes
}

Interface

Uses
	MacOSAll;

Const
	MaxColor=27;

	ColorList:array [0..MaxColor] of RGBColor=(
		(red:$0000; Green:$0000; Blue:$0000), //Black 0
		(red:$0000; Green:$0000; Blue:$c000), //Blue 1
		(red:$0000; Green:$c000; Blue:$0000), //Green 2
		(red:$0000; Green:$c000; Blue:$c000), //Cyan 3
		(red:$c000; Green:$0000; Blue:$0000), //Red 4
													 
		(red:$c000; Green:$0000; Blue:$c000), //Magenta 5
		(red:$6000; Green:$3400; Blue:$1700), //Brown 6 
		(red:$d000; Green:$d000; Blue:$d000), //LightGray 7
		(red:$7fff; Green:$7fff; Blue:$7fff), //DarkGray 8 
		(red:$0000; Green:$0000; Blue:$ffff), //LightBlue 9
													 
		(red:$0000; Green:$ffff; Blue:$0000), //LightGreen 10
		(red:$0000; Green:$ffff; Blue:$ffff), //LightCyan 11
		(red:$ffff; Green:$0000; Blue:$0000), //LightRed 12
		(red:$ffff; Green:$0000; Blue:$ffff), //LightMagenta 13 
		(red:$ffff; Green:$ffff; Blue:$0000), //Yellow 14
													 
		(red:$ffff; Green:$ffff; Blue:$ffff), //White 15 								   	
		(red:$7fff; Green:$7fff; Blue:$0000), //Yellow Ocra 16
		(red:$ff00; Green:$ff00; Blue:$ff00), //White Paper 17
		(red:$1300; Green:$1300; Blue:$1300), //Black Anticranice 18
		(red:$0000; Green:$0000; Blue:$eeee), //Blue Night 19

		(red:$0000; Green:$8000; Blue:$0000), //Dark Green 20
		(red:$0000; Green:$8800; Blue:$8800), //Dark Cyan 21
		(red:$9000; Green:$0000; Blue:$1000), //Reddish Brown 22 
		(red:$3fff; Green:$5fff; Blue:$6fff), //BlueGray 23 
		(red:$7fff; Green:$5fff; Blue:$0000), //Darker Yellow Ocra 24
		(red:$dfff; Green:$5000; Blue:$0000), //Orange 25
		(red:$8000; Green:$2000; Blue:$ffff),	//Purple 26		
		(red:$aaaa; Green:$7888; Blue:$aaaa)		//Pink 27
	); 									   			      

	kColorBlack=0;
	kColorBlue=1;
	kColorGreen=2;
	kColorCyan=3;
	kColorRed=4;
	kColorMagenta=5;
	kColorBrown=6;
	kColorLightGray=7;
	kColorDarkGray=8;
	kColorLightBlue=9;
	kColorLightGreen=10;
	kColorLightCyan=11;
	kColorLightRed=12;
	kColorLightMagenta=13;
	kColorYellow=14;
	kColorWhite=15;
	kColorYellowOcra=16;
	kColorWhitePaper=17;
	kColorBlackAnticranice=18;
	kColorBlueNight=19;

	kColorDarkGreen=20;
	kColorDarkCyan=21;
	kColorRedBrown=22;
	kColorBlueGray=23;
	kColorDarkYellow=24;
	kColorOrange=25;
	kColorPurple=26;
	kColorPink=27;

	LWP_ColorCoding=1;
	LWP2_ColorCoding=2;
	BP3_ColorCoding=3;
	BP7_ColorCoding=4;
	WOB_ColorCoding=5;
	BOW_ColorCoding=6;
	GRAY_ColorCoding=7;



Type SintaxColor=record
	VForeColor,
	VFunctionColor,
	VBeginEndColor,
	VReservedColor,
	VKnowTypeColor,
	VCommentColor,
	VStringColor,
	VSpecialColor,
	VOtherColor,
	VLibraryColor,
	VClassColor,
	VSingleCharColor,
	VCarbonColor,
	VCocoaColor,
	VCompilerDirectiveColor,
	VBackgroundColor:LongInt;
// Added 121109: Data for style (kStyleNormal, kStyleBold, kStyleItalic)
	VForeStyle,
	VFunctionStyle,
	VBeginEndStyle,
	VReservedStyle,
	VKnowTypeStyle,
	VCommentStyle,
	VStringStyle,
	VSpecialStyle,
	VOtherStyle,
	VLibraryStyle,
	VClassStyle,
	VSingleCharStyle,
	VCarbonStyle,
	VCocoaStyle,
	VCompilerDirectiveStyle,
	VBackgroundStyle:LongInt;
End;

Var LWPSintax:SintaxColor;


Procedure SetSintaxColor(Coding:LongInt);

Procedure LoadSintaxSet;

Function TestBackColorForCaret(Color:RGBColor):Boolean;

implementation

uses
	TXNUtilities;

Procedure SetSintaxColor(Coding:LongInt);
begin
	case Coding Of
   	LWP2_ColorCoding: // Richard's variant:
   	begin
			LWPSintax.VForeColor := kColorBlack;
			LWPSintax.VFunctionColor := kColorLightBlue;
			LWPSintax.VBeginEndColor := kColorLightBlue;
			LWPSintax.VKnowTypeColor := kColorDarkGreen;
			LWPSintax.VReservedColor := kColorMagenta;
			LWPSintax.VCommentColor := kColorBlueGray;
			LWPSintax.VStringColor := kColorDarkYellow;
			LWPSintax.VSpecialColor := kColorOrange;
			LWPSintax.VOtherColor := kColorBlack;
			LWPSintax.VLibraryColor := kColorRedBrown;
			LWPSintax.VClassColor := kColorDarkCyan;
			LWPSintax.VSingleCharColor := kColorPurple;
			LWPSintax.VCarbonColor := kColorBrown;
			LWPSintax.VCocoaColor := kColorRedBrown;
			LWPSintax.VCompilerDirectiveColor := kColorPink;
			LWPSintax.VBackgroundColor := kColorWhite;

	// Styles:
			LWPSintax.VForeStyle := kStyleNormal;
			LWPSintax.VFunctionStyle := kStyleNormal;
			LWPSintax.VBeginEndStyle := kStyleNormal;
			LWPSintax.VKnowTypeStyle := kStyleNormal;
			LWPSintax.VReservedStyle := kStyleNormal;
			LWPSintax.VCommentStyle := kStyleNormal;
			LWPSintax.VStringStyle := kStyleNormal;
			LWPSintax.VSpecialStyle := kStyleNormal;
			LWPSintax.VOtherStyle := kStyleNormal;
			LWPSintax.VLibraryStyle := kStyleNormal;
			LWPSintax.VClassStyle := kStyleNormal;
			LWPSintax.VSingleCharStyle := kStyleNormal;
			LWPSintax.VCarbonStyle := kStyleNormal;
			LWPSintax.VCocoaStyle := kStyleNormal;
			LWPSintax.VCompilerDirectiveStyle := kStyleNormal;
			LWPSintax.VBackgroundStyle := kStyleNormal;
			
   end;
   LWP_ColorCoding: // Ingemar's original
   begin
			LWPSintax.VForeColor := kColorBlack;
			LWPSintax.VFunctionColor := kColorBlue; // KColorLightBlue
			LWPSintax.VBeginEndColor := kColorBlue; // KColorLightBlue
			LWPSintax.VKnowTypeColor := kColorDarkGreen; // kColorGreen;
			LWPSintax.VReservedColor := kColorDarkGreen; // kColorGreen;
			LWPSintax.VCommentColor := kColorLightRed;
			LWPSintax.VStringColor := kColorDarkGray;
			LWPSintax.VOtherColor := kColorDarkGreen; // kColorGreen
			LWPSintax.VLibraryColor := kColorBlueNight; // NEW
			LWPSintax.VClassColor := kColorDarkGreen; // NEW
			LWPSintax.VCocoaColor := KColorDarkGreen; // NEW
			LWPSintax.VCompilerDirectiveColor := kColorPink; // NEW
			LWPSintax.VBackgroundColor := kColorWhite;

	// Styles:
			LWPSintax.VForeStyle := kStyleNormal;
			LWPSintax.VFunctionStyle := kStyleBold; // Changed 1401 since it looks better with Halda
			LWPSintax.VBeginEndStyle := kStyleBold;
			LWPSintax.VKnowTypeStyle := kStyleNormal;
			LWPSintax.VReservedStyle := kStyleBold;
			LWPSintax.VCommentStyle := kStyleNormal;
			LWPSintax.VStringStyle := kStyleNormal;
			LWPSintax.VSpecialStyle := kStyleNormal;
			LWPSintax.VOtherStyle := kStyleNormal;
			LWPSintax.VLibraryStyle := kStyleNormal;
			LWPSintax.VClassStyle := kStyleNormal;
			LWPSintax.VSingleCharStyle := kStyleNormal;
			LWPSintax.VCarbonStyle := kStyleNormal;
			LWPSintax.VCocoaStyle := kStyleNormal;
			LWPSintax.VCompilerDirectiveStyle := kStyleNormal;
			LWPSintax.VBackgroundStyle := kStyleNormal;
   end;

		BP3_ColorCoding:
		begin
			LWPSintax.VForeColor := kColorYellow;
			LWPSintax.VFunctionColor := kColorYellowOcra;
			LWPSintax.VBeginEndColor := kColorYellowOcra;
			LWPSintax.VKnowTypeColor := kColorLightRed;
			LWPSintax.VReservedColor := kColorLightRed;
			LWPSintax.VCommentColor := kColorLightGray;
			LWPSintax.VStringColor := kColorLightGreen;
			LWPSintax.VOtherColor := kColorLightRed;
			LWPSintax.VLibraryColor := kColorLightRed; // NEW
			LWPSintax.VClassColor := kColorLightRed; // NEW
			LWPSintax.VCocoaColor := kColorLightRed; // NEW
			LWPSintax.VCompilerDirectiveColor := kColorLightRed; // NEW
			LWPSintax.VBackgroundColor := kColorBlackAnticranice;

// Styles:
			LWPSintax.VForeStyle := kStyleNormal;
			LWPSintax.VFunctionStyle := kStyleNormal;
			LWPSintax.VBeginEndStyle := kStyleNormal;
			LWPSintax.VKnowTypeStyle := kStyleNormal;
			LWPSintax.VReservedStyle := kStyleNormal;
			LWPSintax.VCommentStyle := kStyleNormal;
			LWPSintax.VStringStyle := kStyleNormal;
			LWPSintax.VSpecialStyle := kStyleNormal;
			LWPSintax.VOtherStyle := kStyleNormal;
			LWPSintax.VLibraryStyle := kStyleNormal;
			LWPSintax.VClassStyle := kStyleNormal;
			LWPSintax.VSingleCharStyle := kStyleNormal;
			LWPSintax.VCarbonStyle := kStyleNormal;
			LWPSintax.VCocoaStyle := kStyleNormal;
			LWPSintax.VCompilerDirectiveStyle := kStyleNormal;
			LWPSintax.VBackgroundStyle := kStyleNormal;
		End;
		BP7_ColorCoding:
		Begin
			LWPSintax.VForeColor := kColorYellow;
			LWPSintax.VFunctionColor := kColorYellowOcra;
			LWPSintax.VBeginEndColor := kColorYellowOcra;
			LWPSintax.VKnowTypeColor := kColorLightRed;
			LWPSintax.VReservedColor := kColorYellow;
			LWPSintax.VCommentColor := kColorLightGray;
			LWPSintax.VStringColor := kColorLightGreen;
			LWPSintax.VOtherColor := kColorLightRed;
			LWPSintax.VLibraryColor := kColorLightRed; // NEW
			LWPSintax.VClassColor := kColorLightRed; // NEW
			LWPSintax.VCocoaColor := kColorLightRed; // NEW
			LWPSintax.VCompilerDirectiveColor := kColorLightRed; // NEW
			LWPSintax.VBackgroundColor := kColorBlueNight;

// Styles:
		LWPSintax.VForeStyle := kStyleNormal;
		LWPSintax.VFunctionStyle := kStyleNormal;
		LWPSintax.VBeginEndStyle := kStyleNormal;
		LWPSintax.VKnowTypeStyle := kStyleNormal;
		LWPSintax.VReservedStyle := kStyleNormal;
		LWPSintax.VCommentStyle := kStyleNormal;
		LWPSintax.VStringStyle := kStyleNormal;
		LWPSintax.VSpecialStyle := kStyleNormal;
		LWPSintax.VOtherStyle := kStyleNormal;
		LWPSintax.VLibraryStyle := kStyleNormal;
		LWPSintax.VClassStyle := kStyleNormal;
		LWPSintax.VSingleCharStyle := kStyleNormal;
		LWPSintax.VCarbonStyle := kStyleNormal;
		LWPSintax.VCocoaStyle := kStyleNormal;
		LWPSintax.VCompilerDirectiveStyle := kStyleNormal;
		LWPSintax.VBackgroundStyle := kStyleNormal;
		End;
		WOB_ColorCoding:
		Begin
			LWPSintax.VForeColor := kColorWhite;
			LWPSintax.VFunctionColor := kColorWhitePaper;
			LWPSintax.VBeginEndColor := kColorDarkGray;
			LWPSintax.VKnowTypeColor := kColorLightGray;
			LWPSintax.VReservedColor := kColorLightGray;
			LWPSintax.VCommentColor := kColorLightGray;
			LWPSintax.VStringColor := kColorLightGray;
			LWPSintax.VOtherColor := kColorLightGray;
			LWPSintax.VLibraryColor := kColorDarkGray; // NEW
			LWPSintax.VClassColor := kColorDarkGray; // NEW
			LWPSintax.VCocoaColor := kColorDarkGreen; // NEW
			LWPSintax.VCompilerDirectiveColor := kColorPink; // NEW
			LWPSintax.VBackgroundColor := kColorBlackAnticranice;

// Styles:
			LWPSintax.VForeStyle := kStyleNormal;
			LWPSintax.VFunctionStyle := kStyleNormal;
			LWPSintax.VBeginEndStyle := kStyleNormal;
			LWPSintax.VKnowTypeStyle := kStyleNormal;
			LWPSintax.VReservedStyle := kStyleNormal;
			LWPSintax.VCommentStyle := kStyleNormal;
			LWPSintax.VStringStyle := kStyleNormal;
			LWPSintax.VSpecialStyle := kStyleNormal;
			LWPSintax.VOtherStyle := kStyleNormal;
			LWPSintax.VLibraryStyle := kStyleNormal;
			LWPSintax.VClassStyle := kStyleNormal;
			LWPSintax.VSingleCharStyle := kStyleNormal;
			LWPSintax.VCarbonStyle := kStyleNormal;
			LWPSintax.VCocoaStyle := kStyleNormal;
			LWPSintax.VCompilerDirectiveStyle := kStyleNormal;
			LWPSintax.VBackgroundStyle := kStyleNormal;
		End;
		BOW_ColorCoding:
		Begin
			LWPSintax.VForeColor := kColorBlack;
			LWPSintax.VFunctionColor := kColorBlackAnticranice;
			LWPSintax.VBeginEndColor := kColorBlackAnticranice;
			LWPSintax.VKnowTypeColor := kColorDarkGray;
			LWPSintax.VReservedColor := kColorDarkGray;
			LWPSintax.VCommentColor := kColorDarkGray;
			LWPSintax.VStringColor := kColorLightGray;
			LWPSintax.VOtherColor := kColorDarkGray;
			LWPSintax.VLibraryColor := kColorDarkGray; // NEW
			LWPSintax.VClassColor := kColorDarkGray; // NEW
			LWPSintax.VCocoaColor := kColorDarkGray; // NEW
			LWPSintax.VCompilerDirectiveColor := kColorDarkGray; // NEW
			LWPSintax.VBackgroundColor := kColorWhite;

// Styles:
			LWPSintax.VForeStyle := kStyleNormal;
			LWPSintax.VFunctionStyle := kStyleNormal;
			LWPSintax.VBeginEndStyle := kStyleNormal;
			LWPSintax.VKnowTypeStyle := kStyleNormal;
			LWPSintax.VReservedStyle := kStyleNormal;
			LWPSintax.VCommentStyle := kStyleNormal;
			LWPSintax.VStringStyle := kStyleNormal;
			LWPSintax.VSpecialStyle := kStyleNormal;
			LWPSintax.VOtherStyle := kStyleNormal;
			LWPSintax.VLibraryStyle := kStyleNormal;
			LWPSintax.VClassStyle := kStyleNormal;
			LWPSintax.VSingleCharStyle := kStyleNormal;
			LWPSintax.VCarbonStyle := kStyleNormal;
			LWPSintax.VCocoaStyle := kStyleNormal;
			LWPSintax.VCompilerDirectiveStyle := kStyleNormal;
			LWPSintax.VBackgroundStyle := kStyleNormal;
		End;
		GRAY_ColorCoding:
	 	Begin
			LWPSintax.VForeColor := kColorWhite;
			LWPSintax.VFunctionColor := kColorWhite;
			LWPSintax.VBeginEndColor := kColorWhite;
			LWPSintax.VKnowTypeColor := kColorBlack;
			LWPSintax.VReservedColor := kColorBlack;
			LWPSintax.VCommentColor := kColorLightGray;
			LWPSintax.VStringColor := kColorLightGray;
			LWPSintax.VOtherColor := kColorWhitePaper;
			LWPSintax.VLibraryColor := kColorBlack; // NEW
			LWPSintax.VClassColor := kColorBlack; // NEW
			LWPSintax.VCocoaColor := kColorBlack; // NEW
			LWPSintax.VCompilerDirectiveColor := kColorBlack; // NEW
			LWPSintax.VBackgroundColor := kColorDarkGray;

// Styles:
			LWPSintax.VForeStyle := kStyleNormal;
			LWPSintax.VFunctionStyle := kStyleNormal;
			LWPSintax.VBeginEndStyle := kStyleNormal;
			LWPSintax.VKnowTypeStyle := kStyleNormal;
			LWPSintax.VReservedStyle := kStyleNormal;
			LWPSintax.VCommentStyle := kStyleNormal;
			LWPSintax.VStringStyle := kStyleNormal;
			LWPSintax.VSpecialStyle := kStyleNormal;
			LWPSintax.VOtherStyle := kStyleNormal;
			LWPSintax.VLibraryStyle := kStyleNormal;
			LWPSintax.VClassStyle := kStyleNormal;
			LWPSintax.VSingleCharStyle := kStyleNormal;
			LWPSintax.VCarbonStyle := kStyleNormal;
			LWPSintax.VCocoaStyle := kStyleNormal;
			LWPSintax.VCompilerDirectiveStyle := kStyleNormal;
			LWPSintax.VBackgroundStyle := kStyleNormal;
		End;
	End;
End;

Function TestBackColorForCaret(Color:RGBColor):Boolean;
Begin
	TestBackColorForCaret := false;
	If (Color.red<=$A000) and
		(Color.green<=$A000) and
		(Color.blue<=$eeee) then
			TestBackColorForCaret := true;
End;

Begin

End.
