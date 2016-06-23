// Lightweight Pascal IDE, file name/paths utility unit
// and some file utilities
// © Ingemar Ragnemalm 2006-2007

// Maybe it should be called PathUtils? Much is about paths.
// This also includes some other file utilities and a couple of simplified alerts.
// These may fit better in other units.

// Various versions 2006-2008 not documented.
// 081013: Some bug fixes. Most Str255 replaced by AnsiString.
// 081014: Cleaned out some old junk. Independent of LWP. Added UtilsTypes. Last Str255's removed.
// 081025: Put back some Str255's, the ones that go to StringPtr pointers!
// 1409xx: Added MyAppendMenu, corrections in ListAllFilesOfFolder.
// 150630: Moved SplitStringToList here. Collides with BuildListFromText?
// 151009: Rewrote file i/o to use Clib file i/o, which makes reding of locked files work properly.
// This feels a bit sad, moving from portable FPC file i/o, but since that is buggy/hard to make
// work properly, I switched to something that is both easy and working!

{$I-}
{$mode objfpc} // Do not forget this ever

unit FileUtils;
interface
uses
	MacOSAll, UtilsTypes, BaseUnix, cprocintf; // , LWPGlobals, AlertsUtils;
// LWPGlobals included for the kPathMenuID constant - move it here or add as argument?

//function FSSpecToPathName (var theFile: FSSpecString): Handle;
function FSSpecToPathNameString (theSpec: FSSpec): AnsiString; // Str255;
function FSSpecToPathNameMenu (theSpec: FSSpecString; pathMenuID: Integer): MenuHandle; // Str255;
function FSRefToPath(theRef: FSRef): AnsiString;
function FSRefToPathUTF8(theRef: FSRef): AnsiString;

function MacRomanToUTF8(indata: AnsiString): AnsiString;
function UTF8ToMacRoman(indata: AnsiString): AnsiString;
//function FSSpecPathNameSecurityCheck (var theSpec: FSSpecString): OSErr; // No longer needed - MacRomanToUTF8 should do it
//procedure ConvertPathToCommandLineFormat(var s: AnsiString);

//function AbsolutePathToFSSpec (path: AnsiString): FSSpec;
function RelativePathToFSSpec(homeFile: FSSpecString; relativePath: AnsiString): FSSpecString;
function GetDirName (vol: Integer; dirId: Longint): AnsiString;
function GetIndexedFileName (vol: Integer; dirId: Longint; index: Longint; var name: Str255; var isDirectory: Boolean): OSErr;


{Simpler utilities}
//procedure AppendLn(destRefNum: Integer; theString: AnsiString);
procedure AppendLn(var destRefNum: CharFile; theString: AnsiString);
	function WriteStringToFile(theString: AnsiString; pathSpec: FSSpecString): OSErr; // 120304
	function AppendStringToFile(theString: AnsiString; pathSpec: FSSpecString): OSErr; // 120304
	function OpenFile(fileName: AnsiString; overwrite: Boolean; var fd: CharFile): OSErr;
//function ReadFileToString(pathSpec: FSSpecString): AnsiString;
//function ReadFileToString(pathSpec: FSSpecString; var err: OSErr; var readOnlyFlag: Boolean): AnsiString;
function ReadFileToString(pathSpec: FSSpecString; var err: OSErr): AnsiString;
function FileExists(path: AnsiString): Boolean;
function CreateFolder(path: AnsiString): Longint;
function GetFileSize(path: AnsiString): Longint;
procedure MoveBytes(src, dest: Pointer; amount: Longint);

function TrimExtension(s: AnsiString): AnsiString;
function TrimLastToken(s: AnsiString): AnsiString;
function GetLastToken(s: AnsiString): AnsiString;
//function CleanFileName(cleanName: Str255): Str255;
function GetExtension(cleanName: AnsiString): AnsiString;

function GetExtensionType(fileName: AnsiString): Integer;

{File list utilities}
procedure ListAllFilesOfFolder(theSpec: FSSpecString; var fileList: AnsiStringArray); {parent of spec!}
function ListAllFilesOfFolderFSSpec(theSpec: FSSpecString): FileArr;


const
	kExtTypeUnknown = -1;
	kExtTypePascal = 0;
	kExtTypeC = 1;
	kExtTypeScript = 2; // was kExtTypeShell
	
	kExtTypeObjC = 3;
	kExtTypeCPP = 4;
	kExtTypeJava = 5;
	kExtTypeMakefile = 6;
	kExtTypeAda = 7;
	kExtTypeCuda = 8;
	kExtTypeGLSL = 9;
	kExtTypeCL = 10;
	kExtTypeText = 11; // Other text file
	kExtTypeObj = 12; // Binary file
	kExtTypePHP = 13; // PHP file

// File utilities
function GetFileModTime (theSpec: FSSpecString; var modTime: UInt32): OSErr;
procedure CopyResourceFileToDataFile(srcSpecName, destSpecName: FSSpecString);

//procedure DebugStr(msg: AnsiString);
//function QuestionStr(q: AnsiString): Boolean;
//function YesNoQuestionStr(q: AnsiString): Boolean;
//function YesNoCancelQuestionStr(q: AnsiString): Integer;

// 130318
function GetPreferencesFolder: AnsiString;

// Aren't these two the same?
procedure BuildListFromText(settingsPaths: AnsiString; var theList: AnsiStringArray);
function SplitStringToList(s1: AnsiString): StringArr;

// Get a MacRoman AnsiSting from a CFString
// (Also in FSRefStuff and CarbonStandardFile)
function GetStringFromCFString(input: CFStringRef): AnsiString;
// Get a UTF-8 AnsiString from a CFString
// (Also in FSSpec2Unit and CarbonStandardFile)
function GetUTF8StringFromCFString(input: CFStringRef): AnsiString;

function MyAppendMenu(m: MenuRef; theTitle: AnsiString; commandKey: Char): Longint;


implementation

function FSSpecToPathNameStringaaa (var theSpec: FSSpecString): AnsiString; // Str255;
begin
	FSSpecToPathNameStringaaa := theSpec;
end;

// Jag behöver kanske samma för FSRef - men det har jag nog
function FSSpecToPathNameString (theSpec: FSSpec): AnsiString; // Str255;
VAR
	tempName : Str255;
	vParams : CInfoPBRec;
	theError : OSErr;
	thePath: AnsiString;
BEGIN
		thePath := '';
		tempName := '';
		WITH vParams DO
			BEGIN
				ioCompletion := NIL;
				ioNamePtr := @tempName;
				ioVRefNum := theSpec.vRefNum;
				ioFDirIndex := -1;
				ioDrDirID := theSpec.parID;
				REPEAT
					theError := PBGetCatInfoSync(@vParams);
					IF (theError = noErr) THEN
						BEGIN
							ioDRDirID := ioDRParID;
							thePath := concat(tempName, '/', thePath);
							tempName := '';
						END;
				UNTIL (theError <> noErr);
			END;
		thePath := '/Volumes/' + thePath + theSpec.name;
		FSSpecToPathNameString := thePath;
END;

// Get a MacRoman AnsiSting from a CFString
// (Also in FSRefStuff and CarbonStandardFile)
function GetStringFromCFString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	// Get MacRoman-sträng
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	GetStringFromCFString := output;
end;

// Get a UTF-8 AnsiString from a CFString
// (Also in FSSpec2Unit and CarbonStandardFile)
function GetUTF8StringFromCFString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	// Hämta MacRoman-sträng
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	CFRelease(input); // ??? Can cause problems!
	GetUTF8StringFromCFString := output;
end;

// Utility for getting the path
function FSRefToPath(theRef: FSRef): AnsiString;
var
	fileURL: CFURLRef;
	cf: CFStringRef;
begin
	fileURL := CFURLCreateFromFSRef (nil, theRef);
	cf := CFURLCopyFileSystemPath(fileURL, kCFURLPOSIXPathStyle);
	FSRefToPath := GetStringFromCFString(cf);
	CFRelease(cf);
	CFRelease(fileURL);
end;

// Utility for getting the path
function FSRefToPathUTF8(theRef: FSRef): AnsiString;
var
	fileURL: CFURLRef;
	cf: CFStringRef;
begin
	fileURL := CFURLCreateFromFSRef (nil, theRef);
	cf := CFURLCopyFileSystemPath(fileURL, kCFURLPOSIXPathStyle);
	FSRefToPathUTF8 := GetUTF8StringFromCFString(cf);
//	CFRelease(cf); incorrect release?
	CFRelease(fileURL);
end;

function FSSpecToPathNameMenu (theSpec: FSSpecString; pathMenuID: Integer): MenuHandle;
VAR
	pathMenu: MenuHandle;
BEGIN
	pathMenu := NewMenu(pathMenuID, ''); // Vilket ID???
	while Length(theSpec) > 0 do
	begin
		AppendMenu(pathMenu, GetLastToken(theSpec));
		theSpec := TrimLastToken(theSpec);
	end;
	FSSpecToPathNameMenu := pathMenu;
END;


// Convert string from MacRoman to UTF-8!
function MacRomanToUTF8(indata: AnsiString): AnsiString;
var
	input: CFStringRef;
	output: AnsiString;
	used: Longint;
begin
// Create CFString using kCFStringEncodingMacRoman
//	input :=  CFStringCreateWithCString(nil, @indata[1], kCFStringEncodingMacRoman);
	input := CFStringCreateWithBytes(nil, @indata[1], Length(indata), kCFStringEncodingMacRoman, false);
	
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	CFRelease(input);
	MacRomanToUTF8 := output;
end;

// Convert string from UTF-8 to MacRoman!
function UTF8ToMacRoman(indata: AnsiString): AnsiString;
var
	input: CFStringRef;
	output: AnsiString;
	used: Longint;
begin
// Create CFString using kCFStringEncodingUTF8
//	input :=  CFStringCreateWithCString(nil, @indata[1], kCFStringEncodingUTF8);
	input := CFStringCreateWithBytes(nil, @indata[1], Length(indata), kCFStringEncodingUTF8, false);

	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	CFRelease(input);
	UTF8ToMacRoman := output;
end;

{ Insert "\" before every space. But what should be done about special characters? (ÅÄÖ etc)}
// Meaningless - exec uses separate arguments.
procedure ConvertPathToCommandLineFormatOBSOLETE(var s: AnsiString);
var
	origLength: Longint;
	i: Longint;
	spaceCount: Longint;
	s2: AnsiString;
begin
	origLength := Length(s);
	spaceCount := 0;
	// Count number of spaces
	for i := 1 to origLength do
		if s[i] = ' ' then spaceCount := spaceCount+1;
	// Insert backslashes!
	s2 := s;
	SetLength(s, origLength + spaceCount);
	spaceCount := 0;
	for i := 1 to origLength do
	begin
		if s2[i] = ' ' then
		begin
			s[i + spaceCount] := '\';
			spaceCount := spaceCount+1;
		end;
		s[i + spaceCount] := s2[i];
		//s[i] := 'X';
	end;
end;



{absolut, inte relativ}
{Den behöver väl eventuellt konverteras FRÅN kommandoradsformat?}
{Obsolete, should be replaced by RelativePathToFSSpec in any case}
{where we have a home directory.}
// Kan vara nyttig om den görs om till FSRef?
 function AbsolutePathToFSSpec (path: AnsiString): FSSpec;
  var
   i: Integer;
   spec: FSSpec;
   dirName: AnsiString;
   err: OSErr;
 begin
WriteLn('WARNING! THIS SHOULD BE OBSOLETE!');


{Dela upp i "/", gör CDdown och CDup beroende på vad det står.}
{Första tecknet avgör om man startar från roten (dirID = 2?) eller från lokal plats.}
{Fast sökvägarna skall väl vara globala? Jag vet inte riktigt var jag står?}

{Gå till roten:}
  err := FSMakeFSSpec(0, 2, 'dummy', spec);
{Dela upp strängen i varje /}
  if path[1] = '/' then {Fel om den inte är det!}
   path := Copy(path, 2, Length(path) - 1); {Korta med 1}

  repeat
   i := 1;
   while (path[i] <> '/') and (i < Length(path)) do
    i := i + 1;
   if path[i] = '/' then
    dirName := Copy(path, 1, i - 1)
   else
    dirName := Copy(path, 1, i);
   path := Copy(path, i + 1, Length(path) - i); {Korta med i}

   if dirName = '..' then
    dirName := ''; // No name gives :: = CD up!

   err := FSMakeFSSpec(spec.vRefNum, spec.parID, ConCat(':', dirName, ':dummy'), spec);
   {WriteLn;}
   {LS(spec.vRefNum, spec.parID);}

  until Length(path) = 0;
	if err <> noErr then WriteLn(err);
  AbsolutePathToFSSpec := spec;
 end;

{Annat som vore kul att kunna:}
{Givet vRefNum+dirID, vad heter mappen?}
function GetDirName (vol: Integer; dirId: Longint): AnsiString;
var
	pb: CInfoPBRec;
	err: OSErr;
	name: Str255;
begin
	pb.ioNamePtr := @name;
	pb.ioVRefNum := vol;
	name := '';

	pb.ioFDirIndex := -1; {Viktigt! Styr att det är ioDirID som vi vill ha info om!}
	pb.ioDirID := dirID;
	err := PBGetCatInfoSync(@pb);
	if err <> noErr then WriteLn(err);
		GetDirName := name;
end;

{Högnivåvariant av PBGetCatInfo, som givet vRefNum+dirID + index ger namn på en fil i mappen}
{samt meddelar om det är en mapp.}
// OBSOLETE?
function GetIndexedFileName (vol: Integer; dirId: Longint; index: Longint; var name: Str255; var isDirectory: Boolean): OSErr;
var
	pb: CInfoPBRec;
	err: OSErr;
begin
	pb.ioNamePtr := @name;
	pb.ioVRefNum := vol;
	name := '';

{Index skall här vara mellan 1 och antalet filer.}
	pb.ioFDirIndex := index;
	pb.ioDirID := dirID;
	err := PBGetCatInfoSync(@pb);
	if err = noErr then
//		isDirectory := BAnd(pb.ioFlAttrib, $010) <> 0;
		isDirectory := (pb.ioFlAttrib and $010) <> 0;
	GetIndexedFileName := err;
end;

procedure CleanPath(var path: AnsiString);	
var
	i, p, hit, offs: Longint;
begin
	offs := 0;
	// Split to parts?
	for i := 1 to Length(path)-3 do
		if Copy(path, i+offs, 4) = '/../' then
		begin
			hit := i+offs;
			// Go backwards to previous /
			p := i+offs;
			repeat
				p -= 1;
				if p < 1 then Exit;
			until path[p] = '/';
			WriteLn(Copy(path, 1, p-1));
			WriteLn(Copy(path, hit+3, Length(path)));
			path := Copy(path, 1, p-1) +
					Copy(path, hit+3, Length(path));
			WriteLn(path);
			offs -= (hit - p + 2); // Jump back far enough
		end;
end;

// Create an FSpec from a path. The path may be local to homeFile, or global.
// New task: home + relative
// 141118: No also "cleans" the path from ".." parts.
function RelativePathToFSSpec(homeFile: FSSpecString; relativePath: AnsiString): FSSpecString;
var
	path: FSSpecString;
begin
//WriteLn('RelativePathToFSSpec got: "', homeFile, '", "', relativePath, '"!');
//	homeFile := TrimLastToken(homeFile);
	if relativePath = '' then // Can't use empty string
	begin
		RelativePathToFSSpec := '';
		Exit(RelativePathToFSSpec);
	end;
	if relativePath[1] = '/' then // Absolute path! Don't relate to home file!
		path := relativePath
	else
	begin
		if Length(homeFile) = 0 then // No home file
			path := relativePath
		else
		begin
			// Trim ./ - that means local
			if Length(relativePath) > 2 then
			begin
				if Copy(relativePath, 1, 2) = './' then
					relativePath := Copy(relativePath, 3, Length(relativePath)-2);
				if relativePath[Length(relativePath)] = '/' then
					relativePath := Copy(relativePath, 1, Length(relativePath)-1);
			end;
			homeFile := TrimLastToken(homeFile); // 141110: If it is a FILE, then we only want the path to it - right?
							// HOW CAN THIS EVER HAVE WORKED?
			if homeFile[Length(homeFile)] = '/' then
				path := homeFile + relativePath
			else
				path := homeFile + '/' + relativePath;
		end;
	end;
	CleanPath(path);
	RelativePathToFSSpec := path;
end;

{Append string to open file}
	procedure AppendLn(var destRefNum: CharFile; theString: AnsiString);
	var
//		err: OSErr;
//		inOutCount: Longint;
		theResult: Int64;
	begin
		theString := theString + Char(13);
		try
			BlockWrite(destRefNum, theString[1], Length(theString), theResult);
		except
			WriteLn('AppendLn failed!');
		end;
	end;
//	
//	function WriteStringToFileOBSOLETE(theString: AnsiString; pathSpec: FSSpecString): OSErr;
//	var
//		fd : CharFile;
//		theResult: Int64; // ???
//	begin
//		{$I+}
//		WriteStringToFile := 0;
//		try
//			assign(fd, pathSpec); // Can cause exception?
//			rewrite(fd);                   { open it without breaking for error }
//			BlockWrite(fd, theString[1], Length(theString), theResult);
//			Close(fd);
//		except
//			WriteStringToFile := -1; // IOResult won't work
//			WriteLn('FAIL in WriteStringToFile assign');
//		end;
//		{$I-}
//	end;
//	
//	function AppendStringToFileOBSOLETE(theString: AnsiString; pathSpec: FSSpecString): OSErr;
//	var
//		fd : CharFile;
//	begin
//		{$I+}
//		try
//			assign(fd, pathSpec);
//			reset(fd);                   { open it without breaking for error }
//			AppendLn(fd, theString);
//			Close(fd);
//			AppendStringToFile := 0;
//		except
//			AppendStringToFile := -1; // IOResult won't work
//		end;
//		{$I-}
//	end;

// Trivial but a workaround for strange errors with assign	
	function OpenFile(fileName: AnsiString; overwrite: Boolean; var fd: CharFile): OSErr;
	begin
		{$I+}
		try
			assign(fd, fileName);
			if overwrite then
				rewrite(fd)                   { open it without breaking for error }
			else
				reset(fd);                   { open it without breaking for error }
		except
			OpenFile := IoResult;
		end;
		{$I-}
		OpenFile := IoResult;
	end;
	
// Overload simpler call for cases when we don't need the error check?
//	function ReadFileToString(pathSpec: FSSpecString): AnsiString;
//	function ReadFileToStringOBSOLETE(pathSpec: FSSpecString; var err: OSErr; var readOnlyFlag: Boolean): AnsiString;
//	type
////		CharFile = file of Char;
//		cint = Longint;
//	var
//		FD : CharFile;
//		data: AnsiString;
//		amount: cint;
//	const
//		kMaxAmount = 1024;
//	begin
//		data := '';
//		err := 0;
//		readOnlyFlag := false; // How do I know?

//		{$I+}
//		try
//			assign(fd, pathSpec);
//		except
//		end;
//		{$I-}
//		{$I+}
//		try
//			reset(fd);
//		except
//		end;
//		{$I-}

//		{$I+}
//		try
////			OpenFile(pathSpec, false, fd);
//// Why did this help?! Because we fell out from "try" for read-only?
////			assign(fd, pathSpec);
////WriteLn('assign ', IOResult);
////			reset(fd);                   { open it without breaking for error }
////WriteLn('reset ', IOResult);
//			repeat
//				SetLength(data, Length(data) + kMaxAmount);
//				BlockRead(fd, data[Length(data) - kMaxAmount + 1], kMaxAmount, amount);
//WriteLn('BlockRead ', IOResult, ',', amount);
//			until amount < kMaxAmount;
//			// Correct length for last step
//			if amount > 0 then
//				SetLength(data, Length(data) + amount - kMaxAmount) // Some read last time
//			else
//				SetLength(data, Length(data) + kMaxAmount); // Nothing read last time
//			Close(FD);
//		except
//WriteLn('Oops');
//			err := -1; // IOResult won't work
//			ReadFileToString := data;
//			Close(FD);
//		end;
//		{$I-}
//		ReadFileToString := data;
//	end;

//procedure AppendLn(destRefNum: PFILE; theString: AnsiString);overload;
//begin
//	theString := theString + Char(13);
//	fwrite(@theString[1], 1, Length(theString), destRefNum);
//end;

// Funkar
function WriteStringToFile(theString: AnsiString; pathSpec: FSSpecString): OSErr; // 120304
var
	fptr: PFILE;
begin
	fptr := fopen(PCHAR(pathSpec), 'wb'); // Open file for writing */
	if fptr = nil then
		Exit(-1);
	fwrite(@theString[1], 1, Length(theString), fptr);
	fclose(fptr); // Close the file */
	Exit(0);
end;

// Funkar!
function AppendStringToFile(theString: AnsiString; pathSpec: FSSpecString): OSErr; // 120304
var
	fptr: PFILE;
begin
	fptr := fopen(PCHAR(pathSpec), 'ab'); // Open file for appending */
	if fptr = nil then
		Exit(-1);
	fwrite(@theString[1], 1, Length(theString), fptr);
	fclose(fptr); // Close the file */
	Exit(0);
end;

// Funkar - även på låsta filer!
function ReadFileToString(pathSpec: AnsiString; var err: OSErr): AnsiString;
var
	fptr: PFILE;
	length: Longint;
	buf: AnsiString;
begin
	fptr := fopen(PCHAR(pathSpec), 'rb'); // Open file for reading */
	if fptr = nil then // Return NULL on failure */
	begin
		err := -1;
		exit('');
	end;
	fseek(fptr, 0, SEEK_END); // Seek to the end of the file */
	length := ftell(fptr); // Find out how many bytes into the file we are */
	SetLength(buf, length); // Allocate a buffer for the entire length of the file and a null terminator */
	fseek(fptr, 0, SEEK_SET); // Go back to the beginning of the file */
	fread(@buf[1], length, 1, fptr); // Read the contents of the file in to the buffer */
	fclose(fptr); // Close the file */
	err := 0;
	Exit(buf); // Return the buffer */
end;


function FileExists(path: AnsiString): Boolean;
var
	info: Stat;
	err: Longint;
begin
	err:=fpStat(path, info);
	FileExists := err = 0;
end;

function CreateFolder(path: AnsiString): Longint;
var
	err: Longint;
begin
	err := FpMkdir(path, 16877);
	CreateFolder := err;
//	WriteLn('*** Creating folder: ', path, ' ', err);
//	CreateFolder := FpMkdir(path, 16877);
// FPMkdir is not portable

//	Mkdir(path);
//	CreateFolder := IOResult;
end;

function GetFileSize(path: AnsiString): Longint;
var
	info: Stat;
begin
	fpStat(path, info);
	GetFileSize := info.st_size;
end;

// Pointer based wrapper on Move
procedure MoveBytes(src, dest: Pointer; amount: Longint);
begin
	System.Move(src^, dest^, amount);
end;




		
	{Klipp bort .p, .pas mm}
	function TrimExtension(s: AnsiString): AnsiString;
	var
		i: Longint;
	begin
		i := Length(s);
		while i > 0 do
		begin
			if s[i] = '.' then
			begin
//				return Copy(s, 1, i-1);
				TrimExtension := Copy(s, 1, i-1);
				Exit(TrimExtension);
//				SetLength(s, i-1);
//				return s;
			end;
			i := i - 1;
		end;
		TrimExtension := s;
	end;
	
	{Cut away the file name from a path}
	function TrimLastToken(s: AnsiString): AnsiString;
	var
		i: Longint;
	begin
		i := Length(s);
		while i > 0 do
		begin
			if s[i] = '/' then
			begin
				TrimLastToken := Copy(s, 1, i-1);
				Exit(TrimLastToken);
//				SetLength(s, i-1);
//				return s;
			end;
			i := i - 1;
		end;
		TrimLastToken := s ;
	end;

	{Get the file name from a path}
	function GetLastToken(s: AnsiString): AnsiString;
	var
		i: Longint;
	begin
		i := Length(s);
		while i > 0 do
		begin
			if s[i] = '/' then
			begin
//				SetLength(s, i-1);
				GetLastToken := Copy(s, i+1, Length(s) - i);
				Exit(GetLastToken);
			end;
			i := i - 1;
		end;
		GetLastToken := s;
	end;

	{Cut away the extension}
	{Men är inte detta samma??? Jo, men för Str255.}
	{BORDE ERSÄTTAS!}
	{Replace by TrimExtension}
//	function CleanFileName(cleanName: Str255): Str255;
//	var
//		dotPos: Integer;
//		n: Longint;
//	begin
//		dotPos := 0;
//		for n := 1 to Length(cleanName) do
//			if cleanName[n] = '.' then
//				dotPos := n;
//		if dotPos > 0 then
//			cleanName[0] := Char(dotPos - 1); {Funkar på Str255 men inte andra}
//		CleanFileName := cleanName;
//	end;
	
	function GetExtension(cleanName: AnsiString): AnsiString;
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
	end;



	// Get extension type - configurable extension list can  be implemented here
	function GetExtensionType(fileName: AnsiString): Integer;
	var
		ext: AnsiString;
		fileNameCopy: AnsiString;
//		testlength: Longint;
	begin
		GetExtensionType := kExtTypeUnknown;
		ext := GetExtension(fileName);
		
//		testLength := Length(ext);
//		WriteLn(ext);
//		WriteLn('.c');
		
		// Dirty tricks to force FPC to make a copy of the string (it is by reference if not changed).
		// This is done since LowercaseText changes the string behind FPC's back!
//		fileNameCopy := fileName + ' ';
//		fileNameCopy := Copy(fileNameCopy, 1, Length(fileNameCopy)-1);
		
//		LowercaseText(@ext[1], Length(ext), smRoman); {All uppercase variants taken care of!}
//		LowercaseText(@fileNameCopy[1], Length(fileNameCopy), smRoman); {All uppercase variants taken care of!}
		if (ext = '.p') or (ext = '.pas') or (ext = '.pp') or (ext = '.dpr') or (ext = '.inc') then
			GetExtensionType := kExtTypePascal
		else
//		if (ext = '.c') or (ext = '.cpp') or (ext = '.cp') or (ext = '.h') or (ext = '.m') then
		if (ext = '.c') or (ext = '.h') then
			GetExtensionType := kExtTypeC
		else
		if (ext = '.cpp') or (ext = '.cp') or (ext = '.cc') then
			GetExtensionType := kExtTypeCPP
		else
		if (ext = '.ada') or (ext = '.adb') or (ext = '.ads') then
			GetExtensionType := kExtTypeAda
		else
		if (ext = '.m') then
			GetExtensionType := kExtTypeObjC
		else
		if (ext = '.cu') then
			GetExtensionType := kExtTypeCuda
		else
		// This is a mess and a good reason to make a hash table for extensions
		if (ext = '.vs') or (ext = '.fs') or (ext = '.vert') or (ext = '.frag') or (ext = '.gs') or (ext = '.geom') or (ext = '.glsl') then
			GetExtensionType := kExtTypeGLSL
		else
		if (ext = '.cl') or (ext = '.opencl') then
			GetExtensionType := kExtTypeCL
		else
		if (ext = '.java') then
			GetExtensionType := kExtTypeJava
		else
		if (ext = '.sh') or (ext = '.pl') or (ext = '.rb') or (ext = '.py') or (ext = '.js')  or (ext = '.lua') then
			GetExtensionType := kExtTypeScript
		else
		if (fileNameCopy = 'makefile') then
			GetExtensionType := kExtTypeMakefile
		else
		// Common text file extensions
		if (ext = '.txt') or (ext = '.paths') or (ext = '.libpaths') or (ext = '.options') or (ext = '.frameworks') or (ext = '.link') or (ext = '.html') or (ext = '.xml') then
			GetExtensionType := kExtTypeText
		else
		if (ext = '.php') then
			GetExtensionType := kExtTypePHP
		else
		if (ext = '.o') or (ext = '.a') or (ext = '.lib') or (ext = '.dylib') or (ext = '.bin') then
			GetExtensionType := kExtTypeObj;
	end;



// File utilities

	function GetFileModTime (theSpec: FSSpecString; var modTime: UInt32): OSErr;
	var
		err: OSErr;
		Info: Stat;
	begin
		err := FPStat(theSpec, Info);
		if err = noErr then
			modTime := Info.st_mtime;
		GetFileModTime := err;
	end;



// Is this needed?
// This seems so marginal that I don't care about getting rid of FSSpec - I might
// ditch the whole function instead
	procedure CopyResourceFileToDataFile(srcSpecName, destSpecName: FSSpecString);
	var
		srcSpec, destSpec: FSSpec;
		err: OSErr;
		fndrInfo: FInfo;
		srcRefNum, destRefNum: SmallInt;
		inOutCount: Longint;
		buffer: Ptr;
	begin
		srcSpec := AbsolutePathToFSSpec(srcSpecName);
		destSpec := AbsolutePathToFSSpec(destSpecName);
		
		// Kopiera filinnehåll RF->DF
		// FSpOpenDF
		// GetEOF
		// FSRead
		// FSWrite
		// FSClose
		{Get file info (to get type/creator)}
		err := FSpGetFInfo(srcSpec,fndrInfo);
		{Open file}
		if err = noErr then
			err := FSpOpenRF(srcSpec, fsRdPerm, srcRefNum); // Enda skillnaden mot ResLess?
		if err = noErr then
			err := GetEOF(srcRefNum, inOutCount);

// Support for DF resource files:
		if (err <> noErr) or (inOutCount <= 0) then
		begin
			err := FSClose(srcRefNum);
				err := FSpOpenDF(srcSpec, fsRdPerm, srcRefNum);
			if err = noErr then
				err := GetEOF(srcRefNum, inOutCount);
		end;

		if (err = noErr) and (inOutCount > 0) then
		begin
			buffer := NewPtr(inOutCount);
//WriteLn('buffer allocated to', inOutCount, ' bytes');
			{Create copy}
			if err = noErr then
				err := FSpCreate(destSpec, fndrInfo.fdCreator, fndrInfo.fdType, 0);
			err := FSpOpenDF(destSpec, fsWrPerm, destRefNum);
			{Copy RF to DF}
			if err = noErr then
			//repeat
			begin
				inOutCount := GetPtrSize(buffer);
				err := FSRead(srcRefNum, inOutCount, buffer);
				err := FSWrite(destRefNum, inOutCount, buffer);
			//until inOutCount = 0;
			end;
			{Close}
			err := FSClose(srcRefNum);
			err := FSClose(destRefNum);
			DisposePtr(buffer);
		end;
	end;


// BuildListFromText is a utility for breaking down a text (AnsiString) to separate
// strings for each row

procedure BuildListFromText(settingsPaths: AnsiString; var theList: AnsiStringArray);
var
	addPath: AnsiString;
	i: Longint;
const
	CR = Char(13);
	LF = Char(10);
begin
	SetLength(theList, 0);
//	WriteLn('settingsPaths: "', settingsPaths, '"');
	while Length(settingsPaths)>0 do
	begin
		i := 1;
		while (not (settingsPaths[i] in [CR, LF])) and (i <= Length(settingsPaths)) do
			i := i + 1;
			
		addPath := Copy(settingsPaths, 1, i-1);
		
		if Length(addPath) > 0 then
		begin
			SetLength(theList, Length(theList) + 1);
			theList[Length(theList)-1] := addPath;
		end;
		
		settingsPaths := Copy(settingsPaths, i+1, Length(settingsPaths) - i);
	end;
end;



procedure ListAllFilesOfFolder(theSpec: FSSpecString; var fileList: AnsiStringArray); {parent of spec!}
var TheDir : PDir;
	ADirent : PDirent;
	fName: AnsiString;
begin
	SetLength(fileList, 0);

	TheDir:= fpOpenDir(TrimLastToken(theSpec));

	Repeat
		ADirent:=fpReadDir (TheDir^);
		If ADirent<>Nil then
		begin
			fName := pchar(@ADirent^.d_name[0]);
			if fName <> '.' then
				if fName <> '..' then
				begin
					SetLength(fileList, Length(fileList)+1);
					fileList[Length(fileList)-1] := fName;
					WriteLn('fname===', fName);
				end;
		end;
  Until ADirent=Nil;
  fpCloseDir (TheDir^);
end;


function ListAllFilesOfFolderFSSpec(theSpec: FSSpecString): FileArr;
var
	nameArr: AnsiStringArray;
	fileSpecList: FileArr;
	i: Longint;
begin
	ListAllFilesOfFolder(theSpec, nameArr);
	SetLength(fileSpecList, Length(nameArr));
	for i := 0 to Length(nameArr)-1 do
	begin
		fileSpecList[i] := TrimLastToken(theSpec) + '/' + nameArr[i]; // Borde '/' läggas till?
	end;
	ListAllFilesOfFolderFSSpec := fileSpecList;
end;





// Added 130318

function FindSystemFolderType(folderType: Longint; domain: Longint): AnsiString;
var
	folder: FSRef;
	err: OSErr = noErr;
	url: CFURLRef;
	cf: CFStringRef;
	s: AnsiString;
begin
	err := FSFindFolder(domain, folderType, false, folder);

	if err = noErr then
	begin
		url := CFURLCreateFromFSRef(kCFAllocatorDefault, folder);

		cf := CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle);
		s := GetStringFromCFString(cf);
		CFRelease(cf);
		CFRelease(url);
		FindSystemFolderType := s;
	end
	else
		FindSystemFolderType := '';
end;

function GetPreferencesFolder: AnsiString;
begin
	GetPreferencesFolder := FindSystemFolderType(kPreferencesFolderType, kUserDomain);
end;

function MyAppendMenu(m: MenuRef; theTitle: AnsiString; commandKey: Char): Longint;
var
	item: MenuItemIndex;
	theCF: CFStringRef;
begin
	theCF := CFStringCreateWithBytes(nil, @theTitle[1], Length(theTitle), kCFStringEncodingUTF8, false);
	AppendMenuItemTextWithCFString(m, theCF, 0, 0, @item);
	if Ord(commandKey) > 32 then
		SetMenuItemCommandKey(m, item, FALSE, ORD(commandKey));
	CFRelease(theCF);
//	SetMenuItemCommandKey(m, item, Boolean inSetVirtualKey, UInt16 inKey);
	MyAppendMenu := item;
{OSStatus AppendMenuItemTextWithCFString (
   MenuRef inMenu,
   CFStringRef inString,
   MenuItemAttributes inAttributes,
   MenuCommand inCommandID,
   MenuItemIndex *outNewItem
);
AppendMenu(windowMenu, theTitle + '/' + Char(count + Ord('0')) )}
end;



// Split a string with CR/LF into separate strings
function SplitStringToList(s1: AnsiString): StringArr;
var
	i: Longint;
	theStrings: StringArr;
	s, returnedLine: AnsiString;
begin
	SetLength(theStrings, 0);
	s := s1;
	i := 1;
	while i < Length(s) do
	begin
		if s[i] in [#10, #13] then
		begin
			// CRLF first on line, ignored
			if i = 1 then
			begin
				Delete(s, 1, 1);
				i := 1;
			end
			else
			// Not first in line, save line
			begin
				returnedLine := Copy(s, 1, i-1);
				Delete(s, 1, i);
//				WriteLn('TRÄFF i ', i, ':"', returnedLine, '"');
				SetLength(theStrings, Length(theStrings)+1);
				theStrings[Length(theStrings)-1] := returnedLine;
				i := 1;
			end
		end
		else
			i := i + 1;
	end;
	
	// No more CRLF, save the rest if not empty
	if s <> '' then
	begin
		SetLength(theStrings, Length(theStrings)+1);
		theStrings[Length(theStrings)-1] := s;
	end;
	SplitStringToList := theStrings;
end;


end.
