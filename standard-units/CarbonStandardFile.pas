{Re-implementation of old File Manager and Standard File calls}
{by Ingemar Ragnemalm}

{Standard File is useful for making old code work nice and easy! StandardGet/PutFile}
{are simple and practical. With this glue, you can keep using them, even in new code.}
{Even code using old wdRefNum's will work. Rewriting for every little change Apple does}
{isn't THAT fun! Now even much of the oldest code will work!}

{No "custom" variants at this time.}
{Note: The OSType array in NavTypeList in Navigation.p must be PACKED! (Compiler bug.)}

{Slightly improved 2006: Now all the four standard file calls are implemented}
{(StandardPutFile was missing)}
{More mature versions in 2008:}
{080410: Fixed bug in GetWDInfo, didn't return correct error codes.}
{080412: SFGetFile got file filter support (preliminary, full of debugging WriteLn's).}
{Should be easy to add in the others too.}
{080926: SFGetFile filter support now working!}
{081002: StandardGetFile also has filter support.}
{081008: Default filter handles standard types better.}
{081226: Significant revision! Now I think the filters finally work as intended.}
{My mistake was to think that the type list would override the filter. It is}
{applied beforehand. Now, the type list is applied by the default filter.}
{The drawback is that this makes it more complicated to write a custom}
{filter, but if the custom filter calls the default (which it overrides)}
{then it shouldn't be hard. Also, the useless StandardPutFileSheet is removed.}
{Preliminary FSRef support in StandardGet/PutFile. Not yet in other calls. Untested!}
{080828/090116: Added OpenRFPerm2, since OpenRFPerm seems to misbehave.}
{(I wonder how it can be present at all!)}
{090615: FSRef support works! StandardFileReply now includes fiel ref}
{(get only), parent and name as CFString.}
{090731: Added NewFileName and OldFileName, convenient TP functions.}
{090801: Serious bug introduced in StandardPutFile, now fixed!}
{(Still a mystery though, but worked around.)}
{090801 #2: One more related bug fixed. Added GetStringFromCFString in this}
{unit, to put a MacRoman string in the FSSpec. Also fixed a stupid bug that}
{deallocated the CFString by mistake.}
{090919: Added ".cc" to text types.}
{091007: FSSpec2 built-in. Not that the oldest calls do not support them,}
{or any new fields. REMOVED since it doesn't work well!}
{110107: Mode switch (below) added. Exports ChdirToFSRef. Added StandardGetFolder}
{from other version.}
{111205: Multiple file open, StandardGetFiles and StandardGetFolders. Single-file should now not allow multile selection.}
{120306: File filter code working for new calls.}
{130409: Fixed support for the sfReplacing field.}
{131105: Killed the last FSSpec's from StandardFile calls (not old-style SF calls).}
{Not tested with filters yet!}
{140325: Used with simple filters and it seems OK.}
{140327: Removed fileNameMacRoman since it caused problems.}

{$mode macpas}

unit CarbonStandardFile;
interface
	uses
		MacOSAll; //, FSSpec2Unit; //, FakeAlertUnit;//, StandardFile;

// This is in StandardFile???
type
	SFTypeList = ARRAY [0..3] OF OSType;

	SFReplyPtr = ^SFReply;
	SFReply = RECORD
		good:				BOOLEAN;
		copy:				BOOLEAN;
		fType:				OSType;
		vRefNum:			INTEGER;
		version:			INTEGER;
		fName:				StrFileName;		{ a Str63 on MacOS }
	END;

	StandardFileReplyPtr = ^StandardFileReply;
	StandardFileReply = RECORD
		sfGood:				BOOLEAN;
		sfReplacing:			BOOLEAN;
		sfType:				OSType;
//		sfFile:				FSSpec;
		sfScript:			ScriptCode;
		sfFlags:				INTEGER;
		sfIsFolder:			BOOLEAN;
		sfIsVolume:			BOOLEAN;
//		sfReserved1:			LONGINT;
//		sfReserved2:			INTEGER;

		// FSRef support
		getFileFSRef: FSRef; // Only for StandardGetFile!
		parentDirFSRef: FSRef; // Parent dir
		fileNameCF: CFStringRef; // Name
		fileName: AnsiString; // Name in UTF-8
//		fileNameMacRoman: AnsiString; // Name in MacRoman
		filePath: AnsiString;
//		sfFile2: FSSpec2;
	END;
	DlgHookUPP = UniversalProcPtr;
	FileFilterUPP = UniversalProcPtr;

// This is not in StandardFile
	SFTypeListPtr = ^SFTypeList;
	ConstSFTypeListPtr = SFTypeListPtr;

PROCEDURE SFPutFile(where: Point; prompt: Str255; origName: Str255; dlgHook: DlgHookUPP; VAR reply: SFReply);
PROCEDURE SFGetFile(where: Point; prompt: Str255; fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: SFTypeListPtr; dlgHook: DlgHookUPP; VAR reply: SFReply);
PROCEDURE StandardPutFile(prompt: ConstStringPtr; defaultName: Str255; VAR reply: StandardFileReply);
//PROCEDURE StandardGetFile(fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: ConstSFTypeListPtr; VAR reply: StandardFileReply);
PROCEDURE StandardGetFile(fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: ConstSFTypeListPtr; VAR reply: StandardFileReply);
PROCEDURE StandardGetFolder(VAR reply: StandardFileReply);

// New: Multi-file getters
type
	ReplyArray = array of StandardFileReply;
PROCEDURE StandardGetFiles(fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: ConstSFTypeListPtr; VAR reply: ReplyArray);
PROCEDURE StandardGetFolders(VAR reply: ReplyArray);


// Default file filter for SFGetFile and StandardGetFile. Should be useful to call from
// custom filters. (As yet untested in that situation.)
function SFDefaultFilter(pb: CInfoPBPtr): Boolean;

//PROCEDURE StandardPutFileSheet(prompt: ConstStringPtr; defaultName: Str255; VAR reply: StandardFileReply; parentWindow: WindowPtr);

{Missing File Manager calls (wdRefNum-users)}
FUNCTION FSOpen(fileName: Str255; vRefNum: INTEGER; VAR refNum: INTEGER): OSErr;
FUNCTION OpenDF(fileName: Str255; vRefNum: INTEGER; VAR refNum: INTEGER): OSErr;
FUNCTION GetVInfo(drvNum: INTEGER; volName: StringPtr; VAR vRefNum: INTEGER; VAR freeBytes: LONGINT): OSErr;
FUNCTION GetFInfo(fileName: Str255; vRefNum: INTEGER; VAR fndrInfo: FInfo): OSErr;
FUNCTION Create(fileName: Str255; vRefNum: INTEGER; creator: OSType; fileType: OSType): OSErr;
FUNCTION FSDelete(fileName: Str255; vRefNum: INTEGER): OSErr;
FUNCTION OpenRF(fileName: Str255; vRefNum: INTEGER; VAR refNum: INTEGER): OSErr;
//FUNCTION Rename(oldName: Str255; vRefNum: INTEGER; newName: Str255): OSErr;
//FUNCTION SetFInfo(fileName: Str255; vRefNum: INTEGER; {CONST}VAR fndrInfo: FInfo): OSErr;
//FUNCTION SetFLock(fileName: Str255; vRefNum: INTEGER): OSErr;
//FUNCTION RstFLock(fileName: Str255; vRefNum: INTEGER): OSErr;
//FUNCTION OpenWD(vRefNum: INTEGER; dirID: LONGINT; procID: LONGINT; VAR wdRefNum: INTEGER): OSErr;
//FUNCTION CloseWD(wdRefNum: INTEGER): OSErr;
FUNCTION GetWDInfo(wdRefNum: INTEGER; VAR vRefNum: INTEGER; VAR dirID: LONGINT; VAR procID: LONGINT): OSErr;

{Utility (OpenWD replacement) used by SegLoadPPC}
function MakeWDRefNum(anFSSpec: FSSpec): Longint;
function ChdirToFSRef(ref: FSRef): OSErr;

// Doesn't work yet at all:
FUNCTION GetVol(volName: StringPtr; VAR vRefNum: INTEGER): OSErr;
FUNCTION SetVol(volName: ConstStringPtr; vRefNum: INTEGER): OSErr;

PROCEDURE CreateResFile(fileName: Str255);
FUNCTION OpenResFile(fileName: Str255): INTEGER;
// But OpenRFPerm exists!

// OpenRFPerm exists! But doesn't work properly?
function OpenRFPerm2(fileName: Str255; wd: SInt16; perm: SInt8): SInt16;

//Utility functions (needed when parts of the code is modernized)
//function NameAndWDToSpec(fileName: Str63; vRefNum: Integer): FSSpec;

function OldFileName: AnsiString; overload;
function OldFileName(prompt: AnsiString): AnsiString; overload;
function NewFileName(prompt, defaultName: Str255): AnsiString; overload;
function NewFileName(prompt: Str255): AnsiString; overload;

implementation

function OSTypeToString(a: OSType): Str255;
var
	s, res: Str255;
	i: Integer;
begin
	BlockMove(@a, @s[1], 4);
	s[0] := Char(4);
{Om PPC, bara kopiera}
{$IFDEF ENDIAN_BIG}
	return s;
{$ELSE}
{Om Intel, reversera ordning}
	res[0] := Char(4);
	for i := 1 to 4 do
		res[i] := s[5-i];
	OSTypeToString := res;
{$ENDC}
end;

type
	WDPtr = ^WDRec;
	WDRec = record
		vRefNum: Integer;
		parID: Longint;
		next: WDPtr;
		wdRefNum: Integer;
	end;
var
	wdRoot: WDPtr;
	wdCount: Integer;
{Maybe a dynamically allocated array would be faster. Sure it would.}

function MakeWDRefNum(anFSSpec: FSSpec): Longint;
var
	wd: WDPtr;
begin
	wd := wdRoot;
	while wd <> nil do
	begin
		if anFSSpec.vRefNum = wd^.vRefNum then
			if anFSSpec.parID = wd^.parID then
			begin
				MakeWDRefNum := wd^.wdRefNum;
				Exit(MakeWDRefNum);
			end;
		wd := wd^.next;
	end;
	
	{Create a new one!}
	wd := WDPtr(NewPtr(SizeOf(WDRec)));
	wd^.vRefNum := anFSSpec.vRefNum;
	wd^.parID := anFSSpec.parID;
	wd^.next := wdRoot;
	wd^.wdRefNum := wdCount;
	wdCount := wdCount + 1;
	wdRoot := wd;
	MakeWDRefNum := wd^.wdRefNum;
end;

function NameAndWDToSpec(fileName: Str63; vRefNum: Integer): FSSpec;
var
	wd: WDPtr;
	sp: FSSpec;
	err: OSErr;
begin
	wd := wdRoot;
	while wd <> nil do
	begin
		if wd^.wdRefNum = vRefNum then
		begin
			sp.vRefNum := wd^.vRefNum;
			sp.parID := wd^.parID;
			sp.name := fileName;
			NameAndWDToSpec := sp;
			Exit(NameAndWDToSpec);
		end;
		wd := wd^.next;
	end;
	
	// Important change 0705, gives working defaults:
	err := FindFolder(0, kDesktopFolderType, false, sp.vRefNum, sp.parID);
	if err <> noErr then
		WriteLn('FindFolder error! ', err);
	sp.name := fileName;
	NameAndWDToSpec := sp;
end;

FUNCTION GetWDInfo(wdRefNum: INTEGER; VAR vRefNum: INTEGER; VAR dirID: LONGINT; VAR procID: LONGINT): OSErr;
var
	wd: WDPtr;
begin
	wd := wdRoot;
	while wd <> nil do
	begin
		if wd^.wdRefNum = wdRefNum then
		begin
			vRefNum := wd^.vRefNum;
			dirID := wd^.parID;
			procID := 0;
			GetWDInfo := noErr; // Corrected 080410
			Exit(GetWDInfo);
		end;
		wd := wd^.next;
	end;
	GetWDInfo := nsvErr{-1}; {Error}
end;


PROCEDURE SFPutFile(where: Point; prompt: Str255; origName: Str255; dlgHook: DlgHookUPP; VAR reply: SFReply);
var
	err: OSErr;
	navReply: NavReplyRecord;
	dialogOptions: NavDialogOptions;
	
	theKeyword: AEKeyword;
	actualType: DescType;
	actualSize: Size;
	documentFSSpec: FSSpec;
begin
// This should be rewritten to conform with StandardPutFile
	err := NavGetDefaultDialogOptions(dialogOptions);
	dialogOptions.dialogOptionFlags := BitAnd(dialogOptions.dialogOptionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}
	dialogOptions.savedFileName := origName;
	
	err:= NavPutFile(nil, navReply, @dialogOptions, nil, '????', '????', nil);
	
	{Post processing: Put results into an SFReply}
	reply.good := false;
	if navReply.validRecord then
	begin
		if navReply.translationNeeded then
			;
		
		// Get a pointer to selected file
		err := AEGetNthPtr(navReply.selection, 1,
				typeFSS, @theKeyword, @actualType,
				@documentFSSpec, SizeOf(documentFSSpec), @actualSize);
		if err = noErr then
		begin
			{err := DoOpenFile(&documentFSSpec);}
			
			reply.fType:= '????';
			reply.vRefNum:= MakeWDRefNum(documentFSSpec);
			reply.fName:= documentFSSpec.name;				{  a Str63 on MacOS  }
			reply.good := true;
			
			// MISSING NEW PARTS
			
			err := NavDisposeReply(navReply); {rensar pekare i denna?}
		end;
	end;
	{Men vRefNum finns inte längre! Jag måste stoppa in nånting där som räcker?}
	{Referens till lista av FSp i denna modul?}
	
end;

type
	FileFilterProcPtr = FUNCTION(pb: CInfoPBPtr): Boolean; MWPascal;

// MYSTERY! How could it be THIS wrong???
//	FileFilterProcPtr = NavObjectFilterProcPtr;
//	NavObjectFilterProcPtr = function( var theItem: AEDesc; info: NavFileOrFolderInfoPtr; callBackUD: UnivPtr; filterMode: NavFilterModes ): Boolean;

var
	gGetFileFilter: FileFilterProcPtr; // Old-style filter
	gTypes: SFTypeList; // global för SFDefaultFilter
	gNumTypes: Integer; // global för SFDefaultFilter

{Note: If no file type list is provided, ALL files are accepted.}

{The default filter is applied if no custom one is provided. It should be useful}
{to call from a custom routine, too, for example in order to apply the type list}
{before examining further.}

	function SFDefaultFilter(pb: CInfoPBPtr): Boolean;
// MYSITERIUM! Är denna (nedan) rätt plötsligt?! 
//	function SFDefaultFilter( var theItem: AEDesc; info: NavFileOrFolderInfoPtr; callBackUD: UnivPtr; filterMode: NavFilterModes ): Boolean;
	var
		i, t: Longint;
		filnamn, fileext: Str255;
		
//		debugNumTypes: Integer;
//		debugTypes: SFTypeList;
	begin
//		debugNumTypes := gNumTypes;
//		debugTypes := gTypes;
		
		// Testa om filtyp lika med gTyp eller om extension verkar rätt.
		// TEXT = text txt pas c och kanske några till
		// PICT = pict pic
		
		for t := 0 to gNumTypes-1 do
			if pb^.ioFlFndrInfo.fdType = gTypes[t] then
			begin
//				WriteLn('Filter on ', pb^.ioNamePtr^, ' OK on type.');
				return false; // false = filtrera ej bort, så false = OK
			end;
		filnamn := pb^.ioNamePtr^;
//		WriteLn('Default filter tests ', filnamn);

		i := Length(filnamn);
		while i > 1 do
		begin
			// Find the extension!
			if filnamn[i] = '.' then
			begin
				// Found the period. Get the extension:
				fileext := Copy(filnamn, i, Length(filnamn) - i + 1);
				
				// För alla typer i typlistan,
				// testa om känd filtyp, kolla ändelsen
				for t := 0 to gNumTypes-1 do
				begin
					if gTypes[t] = FOUR_CHAR_CODE('TEXT') then
					begin
						if fileext = '.txt' then return false;
						if fileext = '.text' then return false;
						if fileext = '.p' then return false;
						if fileext = '.pas' then return false;
						if fileext = '.PAS' then return false;
						if fileext = '.c' then return false;
						if fileext = '.cc' then return false;
						if fileext = '.cpp' then return false;
						if fileext = '.h' then return false;
						if fileext = '.java' then return false;
						if fileext = '.html' then return false;
						if fileext = '.ada' then return false;
						if fileext = '.adb' then return false;
						if fileext = '.py' then return false;
						if fileext = '.sh' then return false;
						if fileext = '.vert' then return false;
						if fileext = '.frag' then return false;
						if fileext = '.vs' then return false;
						if fileext = '.fs' then return false;
						if fileext = '.gs' then return false;
						if fileext = '.ts' then return false;
						if fileext = '.cu' then return false;
					end;
					if gTypes[t] = FOUR_CHAR_CODE('PICT') then
					begin
						if fileext = '.pic' then return false;
						if fileext = '.pict' then return false;
						if fileext = '.PICT' then return false;
					end;
					if gTypes[t] = FOUR_CHAR_CODE('JPEG') then
					begin
						if fileext = '.jpeg' then return false;
						if fileext = '.jpg' then return false;
						if fileext = '.JPEG' then return false;
						if fileext = '.JPG' then return false;
					end;
					if gTypes[t] = FOUR_CHAR_CODE('MOOV') then
					begin
						if fileext = '.mov' then return false;
						if fileext = '.MOV' then return false;
					end;
					
					// Special match, makes it valid to enter extensions (.txt)
					// directly in the type list.
					// Extend fileext to 4 characters for this purpose
					while Length(fileext) < 4 do
						fileext := fileext + ' ';
					if OSTypeToString(gTypes[t]) = fileext then // För att kunna lägga in extensions
						return false;
				end;
				
//				WriteLn(filnamn, ' blocked');
				return true; // Unknown extension, refuse
			end;
			i := i - 1;
		end;
		return false; // No extension at all, do not refuse. (Questionable, but better than refusing too much.)
	end;

// Navigation services filter proc, get data and pass to old-style filter.
//FUNCTION GetFileFilter(var theItem: AEDesc; info: UNIV Ptr; callBackUD: UNIV Ptr; filterMode: NavFilterModes): BOOLEAN;
FUNCTION GetFileFilter(var theItem: AEDesc; info: NavFileOrFolderInfoPtr; callBackUD: Pointer; filterMode: NavFilterModes): BOOLEAN; MWPascal;
var
	err: OSStatus;
	display: Boolean = true;
	theInfo: NavFileOrFolderInfoPtr;
	ref: FSRef;
//	itemUTI: CFStringRef = nil;
	outName: HFSUniStr255;
	thefsSpec: FSSpec;
//	fp: FileParam;
//	i: Integer;
	catalogInfo: FSCatalogInfo;
	cipb: CInfoPBRec;
begin
	theInfo := info;

//	if theInfo^.isFolder = true then // 1
//	begin
// Should a folder go to the filter too?
//		return true;
//	end;
	AECoerceDesc (theItem, typeFSRef, theItem); // 2
	if AEGetDescData (theItem, @ref, sizeof (FSRef)) = noErr then
	begin
// Nu har vi en FSRef, väl?
// Här borde man alternativt kunna jobba från filens namn mm.

		// Bara filnamn
		err := FSGetCatalogInfo (ref, kFSCatInfoNone, nil,
								@outName, @thefsSpec, nil);
		if err <> noErr then
			WriteLn('FSGetCatalogInfo error ', err);

		// Annan info? Allt! Optimera till det som faktiskt hämtas?
		err := FSGetCatalogInfo (ref, kFSCatInfoSettableInfo, @catalogInfo,
								@outName, @thefsSpec, nil);
		if err <> noErr then
			WriteLn('FSGetCatalogInfo error ', err);
		
		// Packa in info i FileParam
		// Varför en FileParam? Är det samma?
//		fp.ioNamePtr := @thefsSpec.name;
//		fp.ioVRefNum := thefsSpec.vRefNum;
//		fp.??? := thefsSpec.parID; Behövs för full info!
		// Vad mer vill vi ha? Filstorlek mm.
		// En del finns nog i "info" ovan!

		// Nej, packa från catalogInfo (FSCatalogInfo) till CInfoPBRec
		cipb.ioNamePtr := @thefsSpec.name;
		cipb.ioVRefNum := thefsSpec.vRefNum;
		cipb.ioDirID := thefsSpec.parID; // ???
		cipb.ioFlParID := thefsSpec.parID; // ???
//		cipb.ioFlFndrInfo := ???; // ???
//		cipb.ioFlXFndrInfo := ???; // ???
		cipb.ioFlLgLen := catalogInfo.dataLogicalSize;
		cipb.ioFlPyLen := catalogInfo.dataPhysicalSize;
		cipb.ioFlRLgLen := catalogInfo.rsrcLogicalSize;
		cipb.ioFlRPyLen := catalogInfo.rsrcPhysicalSize;
//		cipb.ioFlCrDat := catalogInfo.createDate; // ???
//		cipb.ioFlMdDat := catalogInfo.contentModDate; // ???
// FSCatalogInfo questionable? We should pack in FSRef and other "modern" info.
		
		BlockMove(@catalogInfo.finderInfo, @cipb.ioFlFndrInfo, SizeOf(FInfo));
//		WriteLn('Type: ', PChar(@cipb.ioFlFndrInfo.fdType));
//		WriteLn('Creator: ', PChar(@cipb.ioFlFndrInfo.fdCreator));
		
		// nil = default
		// tom lista = släpp igenom allt!
		// Annars callback
//		if gNumTypes <= 0 then
//			return true
//		else
		if gGetFileFilter = nil then
			return not SFDefaultFilter(@cipb)
		else
			return not gGetFileFilter(@cipb);
//	NavObjectFilterProcPtr = function( var theItem: AEDesc; info: NavFileOrFolderInfoPtr; callBackUD: UnivPtr; filterMode: NavFilterModes ): Boolean;
	end
	else
	begin
//		WriteLn('No AEGetDescData');
	end;
	return display;
end;


// Ny och bättre SFGetFile:
// fileFilter vekar funka! Se "TestarCarbonStandardFile.p"
// Om tom typlista och inget filter, acceptera ALLT!
PROCEDURE SFGetFile(where: Point; prompt: Str255; fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: SFTypeListPtr; dlgHook: DlgHookUPP; VAR reply: SFReply);
var
	err: OSErr;
	navReply: NavReplyRecord;
	dialogOptions: NavDialogOptions;
	
	theKeyword: AEKeyword;
	actualType: DescType;
	actualSize: Size;
	documentFSSpec: FSSpec;
begin
	// Globaler för filter
	if typeList = nil then
		gNumTypes := 0
	else
	begin
		gTypes := typeList^;
		gNumTypes := numTypes;
	end;
	
	err := NavGetDefaultDialogOptions(dialogOptions);
	dialogOptions.dialogOptionFlags := BitAnd(dialogOptions.dialogOptionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}
	
	gGetFileFilter := FileFilterProcPtr(fileFilter);
	err:= NavGetFile(nil, navReply, @dialogOptions, nil, nil, GetFileFilter, nil{myNavTypeList}, nil);
	gGetFileFilter := nil;
	
	reply.good := false;
	
	if err = noErr then
	begin
		{Post processing: Put results into an SFReply}
		if navReply.validRecord then
		begin
			if navReply.translationNeeded then
				;
			
			// Get a pointer to selected file
			err := AEGetNthPtr(navReply.selection, 1,
					typeFSS, @theKeyword, @actualType,
					@documentFSSpec, SizeOf(documentFSSpec), @actualSize);
			if err = noErr then
			begin
				{err := DoOpenFile(&documentFSSpec);}
				
				reply.fType:= '????';
				reply.vRefNum:= MakeWDRefNum(documentFSSpec);
				reply.fName:= documentFSSpec.name;				{  a Str63 on MacOS  }
				reply.good := true;
				
				// MISSING NEW PARTS HERE
			end;
			err := NavDisposeReply(navReply); {rensar pekare i denna?}
		end;
	end;
	{But vRefNum doesn't exist any more! I must put in something that works.}
	{Reference to list of FSp in this unit?}
end;


// Get a UTF-8 AnsiString from a CFString
// (Also in FSSpec2Unit)
function GetUTF8StringFromCFString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	// Get UTF8 string
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	CFRelease(input);
	return output;
end;

// Get a MacRoman AnsiSting from a CFString
// (Also in FSRefStuff)
function GetStringFromCFString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	// Get MacRoman string
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	return output;
end;

// Utility for getting the path
function FSRefToPath(theRef: FSRef): AnsiString;
var
	fileURL: CFURLRef;
	cf: CFStringRef;
begin
	fileURL := CFURLCreateFromFSRef (nil, theRef);
	cf := CFURLCopyFileSystemPath(fileURL, kCFURLPOSIXPathStyle);
	FSRefToPath := GetStringFromCFString(cf); // This is MacRoman
	CFRelease(cf);
	CFRelease(fileURL);
end;

function FSRefToPathUTF8(theRef: FSRef): AnsiString;
var
	fileURL: CFURLRef;
	cf: CFStringRef;
begin
	fileURL := CFURLCreateFromFSRef (nil, theRef);
	cf := CFURLCopyFileSystemPath(fileURL, kCFURLPOSIXPathStyle);
	FSRefToPathUTF8 := GetUTF8StringFromCFString(cf); // Bugfix 130425: UTF 8 please! But that crashes even worse!
	CFRelease(cf);
	CFRelease(fileURL);
end;


PROCEDURE StandardPutFile(prompt: ConstStringPtr; defaultName: Str255; VAR reply: StandardFileReply);
var
	err: OSErr;
	navReply: NavReplyRecord;
//	dialogOptions: NavDialogOptions; {NavDialogCreationOptions? (parentWindow)}
	
//	theKeyword: AEKeyword;
//	actualType: DescType;
//	actualSize: Size;
//	documentFSSpec: FSSpec;
	
	theAEDesc: AEDesc;
	theDialog: NavDialogRef;
	
	dialogCreaOptions: NavDialogCreationOptions;
	defaultNameCF, promptCF: CFStringRef;
begin
//	err := NavGetDefaultDialogOptions(dialogOptions);
//	dialogOptions.dialogOptionFlags := BitAnd(dialogOptions.dialogOptionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}
//	dialogOptions.savedFileName := defaultName;
//	err := NavPutFile(nil, navReply, @dialogOptions, nil, '????', '????', nil);
	
// Switch to NavCreatePutFileDialog/NavDialogRun/NavDialogGetReply/NavDialogDispose?
	
//	if oldFileNameCF <> nil then
//	begin
//		CFRelease(oldFileNameCF);
//		oldFileNameCF := nil;
//	end;

	err := NavGetDefaultDialogCreationOptions(dialogCreaOptions);
	// clientName, windowTitle, saveFileName, message
	
	dialogCreaOptions.optionFlags := BitAnd(dialogCreaOptions.optionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}
	
	defaultNameCF := CFStringCreateWithBytes(nil, @defaultName[1], Length(defaultName), kCFStringEncodingMacRoman, false);
	
	dialogCreaOptions.saveFileName := defaultNameCF;
	if prompt <> nil then
	begin
		promptCF := CFStringCreateWithBytes(nil, @prompt^[1], Length(prompt^), kCFStringEncodingMacRoman, false);
		dialogCreaOptions.message := promptCF;
	end
	else
		dialogCreaOptions.message := nil;
	
	err := NavCreatePutFileDialog(@dialogCreaOptions, '????', '????',
		nil, nil, theDialog);
//	err := NavCreatePutFileDialog(nil, '????', '????',
//		nil, nil, theDialog);
	NavDialogRun(theDialog);
	err := NavDialogGetReply ( theDialog, navReply);
	NavDialogDispose(theDialog);
	
	{Post processing: Put results into an StandardFileReply}
	reply.sfGood := false;
	reply.sfReplacing := false;
	if navReply.validRecord then
	begin
		if navReply.translationNeeded then
			;
		
		// FSRef support, parent directory and file name
//		err := AEGetNthPtr(navReply.selection, 1,
//					typeFSRef, nil, nil,
//					@reply.parentDirFSRef, SizeOf(reply.parentDirFSRef), nil);
		reply.fileNameCF := navReply.saveFileName; // Copy?
		reply.fileName := GetUTF8StringFromCFString(reply.fileNameCF); // Convert to UTF-8
//		reply.fileNameMacRoman := GetStringFromCFString(reply.fileNameCF);
		reply.sfReplacing := navReply.replacing;
		
//		if err <> noErr then
//			WriteLn('FSRef error ', err);
		
		// OVAN FEL! Så här kanske?
		err := AECoerceDesc(navReply.selection, typeFSRef, theAEDesc);
		if err <> noErr then
			WriteLn('AECoerceDesc error ', err);
			// -1700 = errAECoercionFail
		err := AEGetDescData(theAEDesc, @reply.parentDirFSRef, sizeof(FSRef));
 		if err <> noErr then
			WriteLn('AEGetDescData error ', err);
			// -50 = paramErr

		reply.filePath := FSRefToPath(reply.parentDirFSRef) + '/' + reply.fileName;

		// TEST: FSSpec2 built-in!
//		err := FSMakeFSSpec2 (reply.fileName, reply.parentDirFSRef, reply.sfFile2);
		
		// Get FSSpec for file (since old way doesn't work - why???)
		// Advantage of this: More tightly coupled to the FSRef.
//		err := FSGetCatalogInfo (reply.parentDirFSRef, kFSCatInfoNone,
//									nil, nil, @documentFSSpec, nil); // Get FSSpec from FSRef
//		err := FSMakeFSSpec(documentFSSpec.vRefNum, documentFSSpec.parID, ConCat(':', documentFSSpec.name,
//									':' + reply.fileNameMacRoman), documentFSSpec); // Get spec for underlying file.
//		if err = fnfErr then err := noErr; // The file doesn't have to exist!
		
		// Get a pointer to selected file
		// WHY does this SUDDENLY return the parent folder???
//		err := AEGetNthPtr(navReply.selection, 1,
//				typeFSS, @theKeyword, @actualType,
//				@documentFSSpec, SizeOf(documentFSSpec), @actualSize);
		if err = noErr then
		begin
			reply.sfType:= '????';
//			reply.sfFile := documentFSSpec;
			reply.sfGood := true;
			
//			reply.sfFile.name := ':'+reply.sfFile.name+':'+reply.fileName; // ??? HORRIBLE HACK!
			
//			err := NavDisposeReply(navReply); {Is contents disposed?}
// This crashes!!! Why?
// Not disposing may cause a leak.
		end;
	end;
	
	// Manual cleanup of those impractical CFStrings!
	CFRelease(defaultNameCF);
	if prompt <> nil then
		CFRelease(promptCF);
end;


PROCEDURE StandardGetFileFolder(fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: ConstSFTypeListPtr; VAR reply: StandardFileReply; isFolder: Boolean);
var
	err: OSErr;
	navReply: NavReplyRecord;
//	dialogOptions: NavDialogOptions;
	
	unicodeName: HFSUniStr255;
	
	theDialog: NavDialogRef;
	inOptions: NavDialogCreationOptions;
begin
	// Globaler för filter
	if typeList = nil then
		gNumTypes := 0
	else
	begin
		gTypes := typeList^;
		gNumTypes := numTypes;
	end;
	
//	err := NavGetDefaultDialogOptions(dialogOptions);
//	dialogOptions.dialogOptionFlags := BitAnd(dialogOptions.dialogOptionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}

//	gGetFileFilter := FileFilterProcPtr(fileFilter);
//	err:= NavGetFile(nil, navReply, @dialogOptions, nil, nil, GetFileFilter, nil, nil);
//	gGetFileFilter := nil;

// Should replace the above NavGetFile with newer calls
// MEN HUR LÄGGER MAN IN FILTRET NU DÅ? TESTAS!
// OCH HUR FÅR MAN DefaultFolder ATT FUNKA MED DEN?
// VILKA OPTIONS?
// Varför funkar LWP men inte TestFSRef? Har jag pajat något?
	gGetFileFilter := FileFilterProcPtr(fileFilter);
	NavGetDefaultDialogCreationOptions(inOptions);
	// Added 1112, mask out multiple files option
	inOptions.optionFlags := BitAnd(inOptions.optionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}
	
	if isFolder then
		err := NavCreateChooseFolderDialog(@inOptions, nil, nil, nil, theDialog)
	else
// MYSTERIUM!!! Det KAN ju inte vara en NavObjectFilterProcPtr! Väl?
// Jag har ju testat med denna. Väl?
		err := NavCreateGetFileDialog(@inOptions, 
			nil, nil, nil, NavObjectFilterProcPtr(GetFileFilter), nil, theDialog);
	
	NavDialogRun(theDialog);
	err := NavDialogGetReply ( theDialog, navReply); 
	NavDialogDispose(theDialog);
	gGetFileFilter := nil;
	
	
	reply.sfGood := false;
	
//	if oldFileNameCF <> nil then
//	begin
//		CFRelease(oldFileNameCF);
//		oldFileNameCF := nil;
//	end;
	
	if err = noErr then
	begin
		{Post processing: Put results into an SFReply}
		if navReply.validRecord then
		begin
			if navReply.translationNeeded then
				;
			
			// Get a pointer to selected file
			// This might be better to get from FSRef (as in StandardPutFile).
			// NOTE - COULD SUPPORT MULTIPLE FILES HERE.
			// Length of AEDescList from AECountItems.
//			SetLength(reply, AECountItems(navReply.selection));
			
//			err := AEGetNthPtr(navReply.selection, 1,
//					typeFSS, @theKeyword, @actualType,
//					@documentFSSpec, SizeOf(documentFSSpec), @actualSize);
//			if err = noErr then
//			begin
//				reply.sfType:= '????';
//				reply.sfFile := documentFSSpec;
				reply.sfGood := true;
//			end;
			// FSRef support - get the file FSRef!
			err := AEGetNthPtr(navReply.selection, 1,
					typeFSRef, nil, nil,
					@reply.getFileFSRef, SizeOf(FSRef), nil);
			if err <> noErr then
				WriteLn('AEGetNthPtr ', err);
			
			// Get parent too!
			err := FSGetCatalogInfo(reply.getFileFSRef, kFSCatInfoParentDirID,
								nil, nil, nil, @reply.parentDirFSRef);
			if err <> noErr then
				WriteLn('FSGetCatalogInfo ', err);
			
// File name in CFString format
			err := FSGetCatalogInfo(reply.getFileFSRef, kFSCatInfoParentDirID,
							nil, @unicodeName, nil, nil);
		    reply.fileNameCF := CFStringCreateWithCharacters( nil, unicodeName.unicode, unicodeName.length );
//			oldFileNameCF := reply.fileNameCF;
			reply.fileName := GetUTF8StringFromCFString(reply.fileNameCF); // Convert to UTF-8
//			reply.fileNameMacRoman := GetStringFromCFString(reply.fileNameCF);

			reply.filePath := FSRefToPath(reply.parentDirFSRef) + '/' + reply.fileName;
			WriteLn('StandardGetFile path = ', reply.filePath);
			WriteLn('Length of path = ', Length(reply.filePath));

//			reply.fileNameCF := navReply.saveFileName; // Copy?

//WriteLn('CF filename = ', GetStringFromCFString(reply.fileNameCF));

			// TEST: FSSpec2 built-in!
//			err := FSMakeFSSpec2 (reply.fileName, reply.parentDirFSRef, reply.sfFile2);
			
			err := NavDisposeReply(navReply); {dispose pointers in this?}
		end;
	end;
end;

PROCEDURE StandardGetFile(fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: ConstSFTypeListPtr; VAR reply: StandardFileReply);
//{$IFDEF FPC}
//cdecl; alias: '_StandardGetFile';
//{$ENDIF}
begin
	StandardGetFileFolder(fileFilter, numTypes, typeList, reply, false);
end;

PROCEDURE StandardGetFolder(VAR reply: StandardFileReply);
//{$IFDEF FPC}
//cdecl; alias: '_StandardGetFolder';
//{$ENDIF}
begin
	StandardGetFileFolder(nil, 0, nil, reply, true);
end;



// Multi-file getters

PROCEDURE StandardGetFileFolders(fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: ConstSFTypeListPtr; VAR reply: ReplyArray; isFolder: Boolean);
var
	err: OSErr;
	navReply: NavReplyRecord;
//	dialogOptions: NavDialogOptions;
	
	unicodeName: HFSUniStr255;
	
	theDialog: NavDialogRef;
	inOptions: NavDialogCreationOptions;

	replyNum, theCount: SInt32;
begin
	// Globaler för filter
	if typeList = nil then
		gNumTypes := 0
	else
	begin
		gTypes := typeList^;
		gNumTypes := numTypes;
	end;
	
//	err := NavGetDefaultDialogOptions(dialogOptions);
//	dialogOptions.dialogOptionFlags := BitAnd(dialogOptions.dialogOptionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}

//	gGetFileFilter := FileFilterProcPtr(fileFilter);
//	err:= NavGetFile(nil, navReply, @dialogOptions, nil, nil, GetFileFilter, nil, nil);
//	gGetFileFilter := nil;

// Should replace the above NavGetFile with newer calls
// MEN HUR LÄGGER MAN IN FILTRET NU DÅ? TESTAS!
// OCH HUR FÅR MAN DefaultFolder ATT FUNKA MED DEN?
// VILKA OPTIONS?
// Varför funkar LWP men inte TestFSRef? Har jag pajat något?
	gGetFileFilter := FileFilterProcPtr(fileFilter);
	NavGetDefaultDialogCreationOptions(inOptions);
	// Added 1112, mask out multiple files option
//	inOptions.optionFlags := BitAnd(inOptions.optionFlags, $ffffffff - kNavAllowPreviews - kNavAllowMultipleFiles); {Nollställ kNavAllowPreviews och kNavAllowMultipleFiles}
	
	if isFolder then
		err := NavCreateChooseFolderDialog(@inOptions, nil, nil, nil, theDialog)
	else
// MYSTERIUM!!! Det KAN ju inte vara en NavObjectFilterProcPtr! Väl?
// Jag har ju testat med denna. Väl?
		err := NavCreateGetFileDialog(@inOptions, 
			nil, nil, nil, NavObjectFilterProcPtr(GetFileFilter), nil, theDialog);
	
	NavDialogRun(theDialog);
	err := NavDialogGetReply ( theDialog, navReply); 
	NavDialogDispose(theDialog);
	gGetFileFilter := nil;
	
	if err = noErr then
	begin
		{Post processing: Put results into an SFReply}
		if navReply.validRecord then
		begin
			WriteLn('valid');
			if navReply.translationNeeded then
				;
			
			// Get a pointer to selected file
			// This might be better to get from FSRef (as in StandardPutFile).
			// NOTE - COULD SUPPORT MULTIPLE FILES HERE.
			// Length of AEDescList from AECountItems.
			AECountItems(navReply.selection, theCount);
			SetLength(reply, theCount);
			
			for replyNum := 0 to High(reply) do
//			begin
//				reply[replyNum].sfGood := false;
				
//				err := AEGetNthPtr(navReply.selection, replyNum+1,
//						typeFSS, @theKeyword, @actualType,
//						@documentFSSpec, SizeOf(documentFSSpec), @actualSize);
//				if err = noErr then
//				begin
//					reply[replyNum].sfType:= '????';
//					reply[replyNum].sfFile := documentFSSpec;
					reply[replyNum].sfGood := true;
//				end
//				else
//					WriteLn('AEGetNthPtr FSSpec error = ', err);
//			end;
			for replyNum := 0 to High(reply) do
			begin
				// FSRef support - get the file FSRef!
				err := AEGetNthPtr(navReply.selection, replyNum+1,
						typeFSRef, nil, nil,
						@reply[replyNum].getFileFSRef, SizeOf(FSRef), nil);
				if err <> noErr then
					WriteLn('AEGetNthPtr FSRef error = ', err);
				
				// Get parent too!
				err := FSGetCatalogInfo(reply[replyNum].getFileFSRef, kFSCatInfoParentDirID,
									nil, nil, nil, @reply[replyNum].parentDirFSRef);
				if err <> noErr then
					WriteLn('FSGetCatalogInfo ', err);
				
	// File name in CFString format
				err := FSGetCatalogInfo(reply[replyNum].getFileFSRef, kFSCatInfoParentDirID,
								nil, @unicodeName, nil, nil);
				if err = noErr then
				begin
//					WriteLn('Danger');
					reply[replyNum].fileNameCF := CFStringCreateWithCharacters( nil, unicodeName.unicode, unicodeName.length );
//					WriteLn('Danger!');
					if reply[replyNum].fileNameCF = nil then
						WriteLn('Danger nil');
					reply[replyNum].fileName := GetUTF8StringFromCFString(reply[replyNum].fileNameCF); // Convert to UTF-8
//					WriteLn('Got "', reply[replyNum].fileName, '"');
//					WriteLn('Almost out of danger!');
//					reply[replyNum].fileNameMacRoman := GetStringFromCFString(reply[replyNum].fileNameCF);
//					WriteLn('No danger');
				end
				else
					WriteLn('FSGetCatalogInfo error ', err, ' at ', replyNum);
				
				reply[replyNum].filePath := FSRefToPath(reply[replyNum].parentDirFSRef) + '/' + reply[replyNum].fileName;
				WriteLn('StandardGetFile path = ', reply[replyNum].filePath);
				WriteLn('Length of path = ', Length(reply[replyNum].filePath));

			end;
			err := NavDisposeReply(navReply); {dispose pointers in this?}
		end;
	end;
end;

PROCEDURE StandardGetFiles(fileFilter: FileFilterUPP; numTypes: INTEGER; typeList: ConstSFTypeListPtr; VAR reply: ReplyArray);
//{$IFDEF FPC}
//cdecl; alias: '_StandardGetFiles';
//{$ENDIF}
begin
	StandardGetFileFolders(fileFilter, numTypes, typeList, reply, false);
end;

PROCEDURE StandardGetFolders(VAR reply: ReplyArray);
//{$IFDEF FPC}
//cdecl; alias: '_StandardGetFolders';
//{$ENDIF}
begin
	StandardGetFileFolders(nil, 0, nil, reply, true);
end;





{Missing File Manager calls (wdRefNum-using calls)}

FUNCTION FSOpen(fileName: Str255; vRefNum: INTEGER; VAR refNum: INTEGER): OSErr;
var
	mySpec: FSSpec;
	err: OSErr;
begin
	mySpec := NameAndWDToSpec(fileName, vRefNum);
	err := FSpOpenDF(mySpec, fsCurPerm, refNum);
	FSOpen := err;
end;

FUNCTION OpenDF(fileName: Str255; vRefNum: INTEGER; VAR refNum: INTEGER): OSErr;
var
	mySpec: FSSpec;
	err: OSErr;
begin
	mySpec := NameAndWDToSpec(fileName, vRefNum);
	err := FSpOpenDF(mySpec, fsCurPerm, refNum); // Added 081129
	OpenDF := err;
end;

FUNCTION GetVInfo(drvNum: INTEGER; volName: StringPtr; VAR vRefNum: INTEGER; VAR freeBytes: LONGINT): OSErr;
//var
//	mySpec: FSSpec;
//	err: OSErr;
begin
	return noErr;
end;

FUNCTION GetFInfo(fileName: Str255; vRefNum: INTEGER; VAR fndrInfo: FInfo): OSErr;
var
	mySpec: FSSpec;
begin
	mySpec := NameAndWDToSpec(fileName, vRefNum);
	GetFInfo := FSpGetFInfo(mySpec, fndrInfo);
end;

FUNCTION Create(fileName: Str255; vRefNum: INTEGER; creator: OSType; fileType: OSType): OSErr;
var
	mySpec: FSSpec;
	err: OSErr;
begin
	mySpec := NameAndWDToSpec(fileName, vRefNum);
	
	err := FSpCreate(mySpec, creator, fileType, smSystemScript); {smCurrentScript?}
	Create := err;
end;

FUNCTION FSDelete(fileName: Str255; vRefNum: INTEGER): OSErr;
var
	mySpec: FSSpec;
begin
	mySpec := NameAndWDToSpec(fileName, vRefNum);
	FSDelete := FSpDelete(mySpec);
end;

FUNCTION OpenRF(fileName: Str255; vRefNum: INTEGER; VAR refNum: INTEGER): OSErr;
var
	mySpec: FSSpec;
begin
	mySpec := NameAndWDToSpec(fileName, vRefNum);
	OpenRF := FSpOpenRF(mySpec, fsRdWrperm, refNum);
end;


{Are GetVol/SetVol at all interesting any more?}

FUNCTION GetVol(volName: StringPtr; VAR vRefNum: INTEGER): OSErr;
begin
	{I guess it should return (create if necessary) a wd which is the application folder?}
	vRefNum := 0;
	GetVol := noErr;
end;

var
	gDefaultVRefNum: integer;

// I assume that this is called to give calls like CreateResFile a specified place.
FUNCTION SetVol(volName: ConstStringPtr; vRefNum: INTEGER): OSErr;
begin
	gDefaultVRefNum := vRefNum;
	return noErr;
end;


PROCEDURE CreateResFile(fileName: Str255);
var
	mySpec: FSSpec;
	err: OSErr;
	fndrInfo: FInfo;
begin
	mySpec := NameAndWDToSpec(fileName, gDefaultVRefNum);
	{If the file already exists, get type and creator from it!}
	err := FSpGetFInfo(mySpec, fndrInfo);
	if err = noErr then
	begin
		FSpCreateResFile(mySpec, fndrInfo.fdCreator, fndrInfo.fdType, smRoman);
	end
	else
		FSpCreateResFile(mySpec, 'RSED', 'rsrc', smRoman);
end;

FUNCTION OpenResFile(fileName: Str255): INTEGER;
var
	mySpec: FSSpec;
begin
	mySpec := NameAndWDToSpec(fileName, gDefaultVRefNum);
	OpenResFile := FSpOpenResFile(mySpec, fsCurPerm);
end;

// OpenRFPerm not working?
function OpenRFPerm2(fileName: Str255; wd: SInt16; perm: SInt8): SInt16;
var
	mySpec: FSSpec;
begin
	mySpec := NameAndWDToSpec(fileName, wd);
	OpenRFPerm2 := FSpOpenResFile(mySpec, perm);
end;


// Use these calls? NO - all code should move to FSRefs and possibly
// the future FSSpec2 (UTF8+FSRef). Avoid old FSSpec.
//FUNCTION HOpenResFile(vRefNum: INTEGER; dirID: LONGINT; fileName: Str255; permission: SInt8): INTEGER;
//PROCEDURE HCreateResFile(vRefNum: INTEGER; dirID: LONGINT; fileName: Str255);
//FUNCTION FSpOpenResFile({CONST}VAR spec: FSSpec; permission: SignedByte): INTEGER;
//PROCEDURE FSpCreateResFile({CONST}VAR spec: FSSpec; creator: OSType; fileType: OSType; scriptTag: ScriptCode);


// Set the current directory to an FSRef.
function ChdirToFSRef(ref: FSRef): OSErr;
var
	err: OSErr;
	p: PChar;
begin
	p := PChar(NewPtrClear(1024)); // calloc?
	err := FSRefMakePath(ref, p, 1024);
	if err = noErr then
		chdir(p);

	DisposePtr(Ptr(p));
	
	return err;
end;

function OldFileName: AnsiString; overload;
var
	reply: StandardFileReply;
	err: OSErr;
begin
	StandardGetFile(nil, -1, nil, reply);
	if reply.sfGood then
	begin
		err := ChdirToFSRef(reply.parentDirFSRef);
		if err <> noErr then
			return '';
		return reply.fileName;
	end
	else
		return '';
end;

function OldFileName(prompt: AnsiString): AnsiString; overload;
begin
	return OldFileName();
end;

function NewFileName(prompt, defaultName: Str255): AnsiString; overload;
var
	reply: StandardFileReply;
	err: OSErr;
begin
	StandardPutFile(@prompt, defaultName, reply);
	if reply.sfGood then
	begin
		err := ChdirToFSRef(reply.parentDirFSRef);
		if err <> noErr then
			return '';
		return reply.fileName;
	end
	else
		return '';
end;

function NewFileName(prompt: Str255): AnsiString; overload;
begin
	return NewFileName(prompt, '');
end;

end.
