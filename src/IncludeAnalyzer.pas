// SKRIV OM DENNA
// utnyttja ColorCodings fillista, använd den rekursivt.
// Bättre, stöd för kommentarer mm. Stöd för Pascal.
// Kanske lättare att anpassa för att testa kompileringsbehov?


{$mode macpas}
unit IncludeAnalyzer;
interface
uses
	MacOSAll, FileUtils, Settings, ColorCoding, Console, UtilsTypes, LWPGlobals,
	ProcessUtils, BaseUnix;
	{Settings included for gSettingsPaths. ColorCoding for BuildMenuFromUses.}

{Returnera sträng med alla filnamn som filen beror av med #include.}
//function GetIncludeFileList(theSpec: FSSpecString): AnsiString;

function GetIncludeFileList(theSpec: FSSpecString; includeHeaderFiles: Boolean): FileArr; overload;
function GetIncludeFileList(theSpec: FSSpecString): FileArr; overload;
function GetIncludeFileListString(theSpec: FSSpecString): AnsiString;
function GetDisplayIncludeFileListString(theSpec: FSSpecString): AnsiString; // For display only

procedure SetFindFileMainSpec(mainSpec: FSSpecString);
function FindFileInPaths(fileName: AnsiString; homeSpec: FSSpecString; var foundSpec: FSSpecString): Boolean;

// New variant: Only searches header files
function GetIncludeOnlyFileList(theSpec: FSSpecString): FileArr;

{Att fixa (för andra behov):}
{Returnera array av FSSpecString (FSRef) till alla filer som filen anger med #include}
{eller uses (inklusive filen själv?).}
{Kräver att hashtabellen kompletteras med FSSpecar (gFileTable)}
{Användbart för multi-file-search och kompileringsbehovtest.}

// For BuildWithPascal:
function AnalyzeLinklib(theSpec: FSSpecString): StringArr;

implementation

uses
	BuildWithPascal;

const
	kHashLength = 1023; {Skall vara tvåpotens minus ett}

var
	gGeneratedIncludeList: AnsiString;
//	gGeneratedIncludeList: AnsiStringArray;
	gHashTable: array[0..kHashLength] of AnsiString;
//	gFileTable: array[0..kHashLength] of FSSpecString; // Switch to FSRef?
	gHashMembers: Longint;
	extension: AnsiString;
	
	fileList: FileArr;
	gMainSpec: FSSpecString;
	
	gIncludeHeaderFiles: Boolean; // Argument to GetIncludeFileList

// In order to use FindFileInPaths from other code, we must set the
// main file.
procedure SetFindFileMainSpec(mainSpec: FSSpecString);
begin
	gMainSpec := mainSpec;
end;

// Move this to reusable code?
// WHY is there both a homeSpec and gMainSpec?
// homeSpec = previous file in include analysis search. Irrelevant for this?
// Or what *is* it??? If I want to search in paths I must have a home folder
// for relative searches. What else is a "homeSpec"?
// A relative path is relative to the main file - right?
// Why Str255?
function FindFileInPaths(fileName: AnsiString{Str255}; homeSpec: FSSpecString; var foundSpec: FSSpecString): Boolean;
var
	pathList: AnsiStringArray;
	i: Longint;
	err: OSErr;
	pathSpec: FSSpecString;
	pathString: AnsiString;
	info: Stat;
begin
//		WriteLn('FindFileInPaths looking for "' + fileName + '"');
//	Get the path string and build a list of paths (again)
// Borde göras en gång för alla! Dvs en gång för varje anrop till denna modul.
	BuildListFromText(gSettings.Paths, pathList);
	AppendCustomPaths('.paths', pathList, gMainSpec); // Main file may reside in folder with a matching .paths
	// Append intermediate_files too? Will find .o files better then.
	for i := -1 to Length(pathList) -1 do
	begin
		if i = -1 then
		begin
//			WriteLn('gMainSpec = ', gMainSpec);
			pathString := TrimLastToken(gMainSpec);
//			WriteLn('Trimmed to ', pathString);
		end
		else
			pathString := pathList[i];
//		WriteLn('Trying "' + pathString + '"');
//		build FSSpecString for file in the path (using RelativePathToFSSpec)
		pathSpec := RelativePathToFSSpec(gMainSpec, pathString) + '/' + fileName;
//		WriteLn('RelativePathToFSSpec(gMainSpec, pathString) = ', RelativePathToFSSpec(gMainSpec, pathString));
		err:=fpStat(pathSpec, info); // Does the file exist?
		
		if err = noErr then
		begin
//			WriteLn('Found it!', pathSpec);
			foundSpec := pathSpec;
			return true;
		end;
//		else
//			WriteLn('Did not find ', fileName, ' in ', pathString, ' (', pathSpec, ')');
	end;
//	WriteLn('Found nothing: ', pathSpec, '??????');
	return false;
end;

// Check if string is in gHashTable. If not, add it and return false
// Why Str255?
function FileInList(fileName: Str255): Boolean;
var
	key, i: Longint;
begin
	key := 0;
	// Generate hash key
	for i := 1 to Length(fileName) do
	begin
		key := key * 2 + Ord(fileName[i]);
	end;
	key := key and kHashLength;
//	WriteLn('Startkey ', fileName, key);
	
	// Sök från key och framåt i gHashTable tills samma hittas eller en tom plats.
	// Den tomma platsen sätts till fileName.

	if gHashMembers >= kHashLength then
	begin
		WriteLn('Fatal error - hash table full');
		Return true; // Enda sättet att slinka undan
	end;

	while true do
	begin
		if Length(ghashTable[key]) = 0 then {Ledig plats}
		begin
//			WriteLn('Added ', fileName, key);
			gHashTable[key] := fileName;
			gHashMembers := gHashMembers + 1;
			Return false; {Fanns inte i listan innan!}
		end;
		if gHashTable[key] = fileName then
		begin
//			WriteLn('Already in list ', fileName, key);
			Return true;
		end;
		key := (key + 1) and kHashLength;
	end;
end;

// NEW 071017: GetIncludeFileListForFile, GetIncludeFileList, GetIncludeFileListString
// Read list of names from file (using GetUsesListFromFile), check them,
// add to fileList if unique and existing
procedure GetIncludeFileListForFile(theSpec: FSSpecString; gIncludeHeaderFiles: Boolean );
var
	fileData: AnsiString;
	//pos: Longint;
	{found,} foundInPaths: Boolean;
	//fileName: AnsiString;
	fileSpec: FSSpecString;
	{cleanName,} cName: AnsiString;
	arr: AnsiStringArray;
	extType: Integer;
	i: Longint;
	err: OSErr;
begin
//	WriteLn('Searching ', theSpec.name);

	extType := GetExtensionType(theSpec);
	//extension := GetExtension(theSpec.name);
	fileData := ReadFileToString(theSpec, err);
	// Get array of filenames as strings
	arr := GetUsesListFromText(fileData, Length(fileData), GetLastToken(theSpec){title});

	//WriteLn('Uses list of ', theSpec, ' has length ', Length(arr));
	
	// Processa namnen från .h till .c, .cpp mm? Hitta en källfil som existerar.
	
	for i := 0 to Length(arr)-1 do
	begin
		if not FileInList(arr[i]) then
		begin
			if extType in [kExtTypeC, kExtTypeCPP, kExtTypeObjC] then
			begin
			// Add .h file to list if we wish .h files too.
				if gIncludeHeaderFiles then
					if FindFileInPaths(arr[i], theSpec, fileSpec) then
					begin
						SetLength(fileList, Length(fileList)+1);
						fileList[Length(fileList)-1] := fileSpec;
					end;
			// C: Change extension (e.g. .h to .c)
				cName := TrimExtension(arr[i]) + extension;
				
				// If we miss, try others
				foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
				if not foundInPaths then
				begin
					cName := TrimExtension(arr[i]) + '.c';
					foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
				end;
				if not foundInPaths then
				begin
					cName := TrimExtension(arr[i]) + '.m';
					foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
					// NOTE: ObjC runtime!
				end;
				if not foundInPaths then
				begin
					cName := TrimExtension(arr[i]) + '.cpp';
					foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
					// NOTE: C++ runtime!
				end;
				if not foundInPaths then
				begin
					cName := TrimExtension(arr[i]) + '.cc';
					foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
					// NOTE: C++ runtime!
				end;
				
			end;
// FEL! SKall söka BÅDE .h och .c! Eller gör den? Nix!
			
			if extType = kExtTypePascal then
			begin
			// Pascal: Add extension
				cName := arr[i] + extension;
//				WriteLn('####GetIncludeFileListForFile testing ', cName);

			// Check that the file exists using FindFileInPaths
				foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
			// POSSIBLE IMPROVEMENT: Test several extensions here?
			end;

			// An attempt: Try .a and .o (to support libs)
			// Added 080208
			if not foundInPaths then
			begin
				cName := TrimExtension(arr[i]) + '.o';
				foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
			end;
			if not foundInPaths then
			begin
				cName := TrimExtension(arr[i]) + '.a';
				foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
			end;
			if not foundInPaths then
			begin
				cName := TrimExtension(arr[i]) + '.dylib'; // Added 110312
				foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
			end;
// LÄGG TILL HÄR: PASCAL-EXTENSION FÖR ATT FÖRSÖKA KOMPILERA PASCALFILER
// SOM DEL AV C-PROJEKT!
// Inte klart än. C-build måste bli smartare och ta hand om dessa.
// Följande lägger in pas och p i listan:
(*
			if not foundInPaths then
			begin
				cName := TrimExtension(arr[i]) + '.pas';
				foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
			end;
			if not foundInPaths then
			begin
				cName := TrimExtension(arr[i]) + '.p';
				foundInPaths := FindFileInPaths(cName, theSpec, fileSpec);
			end;
*)
			
			// THEN add it to list!
			if foundInPaths then
			begin
				SetLength(fileList, Length(fileList)+1);
				fileList[Length(fileList)-1] := fileSpec;
				
				// Search .c file recursively!
				//if foundInPaths then
				begin
// BORDE JAG INTE KOLLA ATT DET ÄR EN KÄLLFIL OCH INTE .o ELLER .a?
// Verkar dock inte ha varit något problem?
					if not (extType in [kExtTypeObj]) then
					GetIncludeFileListForFile(fileSpec, false);
					//HelpAppendLn('Searching .c: '+ fileSpec.name);
				end;
//				else
//					HelpAppendLn('Not found in paths: '+ fileSpec.name);
			end;
//			else
//				WriteLn(cName, ' not found.');
			
			// If C-style, find the .h file and analyze that too
			if extType in [kExtTypeC, kExtTypeCPP, kExtTypeObjC] then
			begin
				foundInPaths := FindFileInPaths(TrimExtension(arr[i]) + '.h', theSpec, fileSpec);
				if foundInPaths then
				begin
					GetIncludeFileListForFile(fileSpec, false);
//					HelpAppendLn('Searching .h: '+ fileSpec.name);
				end;
//				else
//					HelpAppendLn('Not found in paths: '+ fileSpec.name);
			end;
		end;
	end;
end;

function GetIncludeFileList(theSpec: FSSpecString; includeHeaderFiles: Boolean): FileArr; overload;
var
	i: Longint;
	//hName: AnsiString;
	//s: Str255;
begin
	gMainSpec := theSpec; // For local path files
	gIncludeHeaderFiles := includeHeaderFiles;

	SetLength(fileList, 0); // Clear the file list
	extension := GetExtension(theSpec);

	// Zero the list and the hash table
	SetLength(gGeneratedIncludeList, 0);
	for i := 0 to kHashLength do
		gHashTable[i] := '';
	gHashMembers := 0;

	// Do recursive call to GetIncludeFileListForFile
//	WriteLn('GetIncludeFileList trying ', theSpec);

	GetIncludeFileListForFile(theSpec, includeHeaderFiles);
	
//	WriteLn('GetIncludeFileList found ', Length(fileList), ' items for ', theSpec);
	
	return fileList;
end;

function GetIncludeFileList(theSpec: FSSpecString): FileArr; overload;
begin
	return GetIncludeFileList(theSpec, false);
end;

// Utility that, like everything else, should be changed to FSRef
function SamePlace(a, b: FSSpecString): Boolean;
begin
	if TrimLastToken(a) = TrimLastToken(b) then
//	if a.parID = b.parID then
//		if a.vRefNum = b.vRefNum then
			return true;
	return false;
end;

function GetIncludeFileListString(theSpec: FSSpecString): AnsiString;
var
	generatedIncludeListString: AnsiString;
	arr: FileArr;
	i: Longint;
	//s: Str255;
	pathName: AnsiString;
begin
	arr := GetIncludeFileList(theSpec); {Anropa GetIncludeFileList för att bygga listan}
	
// VAD GÖR DETTA EGENTLIGEN?
// Block the main file - always included
//	hName := CleanFileName(theSpec.name) + '.h';
//	if not FileInList(hName) then
//		gGeneratedIncludeList := theSpec.name + gGeneratedIncludeList;
	
//Str(Length(arr), s);
//HelpAppendLn('Array length = '+ s);
	
	extension := GetExtension(theSpec);
	
	generatedIncludeListString := GetLastToken(theSpec);
	for i := 0 to Length(arr)-1 do
	begin
// För C, byt alla .h mot .c (eller vad vi nu har)
// För Pascal, bara slå ihop
// MEN DET ÄR C VI BEHÖVER JUST DENNA FÖR
		if TrimExtension(GetLastToken(arr[i]))+extension <> GetLastToken(theSpec) then
		begin
			pathName := arr[i];
//HelpAppendLn('File name "' + arr[i].name + '" got path "' + pathName + '"');
			if SamePlace(arr[i], theSpec) then
				generatedIncludeListString := generatedIncludeListString + ' ' + GetLastToken(arr[i])
			else
				generatedIncludeListString := generatedIncludeListString + ' "' + pathName + '"';
//generatedIncludeListString := generatedIncludeListString + ' ' + CleanFileName(arr[i].name)+extension
		end;
	end;
	
	return generatedIncludeListString;
end;

// Variant intended for display
function GetDisplayIncludeFileListString(theSpec: FSSpecString): AnsiString;
var
	generatedIncludeListString: AnsiString;
	arr: FileArr;
	i: Longint;
	//s: Str255;
	pathName: AnsiString;
begin
	arr := GetIncludeFileList(theSpec); {Anropa GetIncludeFileList för att bygga listan}
	
	extension := GetExtension(theSpec);
	
	generatedIncludeListString := GetLastToken(theSpec);
	for i := 0 to Length(arr)-1 do
	begin
		if TrimExtension(GetLastToken(arr[i]))+extension <> GetLastToken(theSpec) then
		begin
			pathName := arr[i];
//			if i > 0 then
//				if SamePlace(arr[i], arr[i-1]) then
//					pathName := '---' + arr[i].name;
//			if SamePlace(arr[i], theSpec) then
//				generatedIncludeListString := generatedIncludeListString + #13 + arr[i].name
//			else
//				generatedIncludeListString := generatedIncludeListString + #13 + pathName;

				generatedIncludeListString := generatedIncludeListString + ' ' + GetLastToken(arr[i])
//generatedIncludeListString := generatedIncludeListString + ' ' + CleanFileName(arr[i].name)+extension
		end;
	end;
	
	return generatedIncludeListString;
end;







// ***************************************************************

// Simple variant, only searches include files!
// Could possibly be integrated in GetIncludeFileListForFile
// by additional flags. But is it worth it?
// Or can we simplify the former by keeping two separate calls?
// That sounds sensible!

procedure GetIncludeOnlyFileListForFile(theSpec: FSSpecString);
var
	fileData: AnsiString;
	foundInPaths: Boolean;
	fileSpec: FSSpecString;
	cName: AnsiString;
	arr: AnsiStringArray;
	extType: Integer;
	i: Longint;
	err: OSErr;
begin
//	WriteLn('Searching ', theSpec.name);

	extType := GetExtensionType(theSpec);
	fileData := ReadFileToString(theSpec, err);
	// Get array of filenames as strings
	arr := GetUsesListFromText(fileData, Length(fileData), GetLastToken(theSpec));

	for i := 0 to Length(arr)-1 do
	begin
		if not FileInList(arr[i]) then
		begin
			if extType in [kExtTypeC, kExtTypeCPP, kExtTypeObjC] then
			begin
				if FindFileInPaths(arr[i], theSpec, fileSpec) then
				begin
					SetLength(fileList, Length(fileList)+1);
					fileList[Length(fileList)-1] := fileSpec;
				end;
				foundInPaths := FindFileInPaths(TrimExtension(arr[i]) + '.h', theSpec, fileSpec);
				if foundInPaths then
					GetIncludeOnlyFileListForFile(fileSpec);
			end;
		end;
	end;
end;

function GetIncludeOnlyFileList(theSpec: FSSpecString): FileArr;
var
	i: Longint;
begin
	gMainSpec := theSpec; // For local path files

	SetLength(fileList, 0); // Clear the file list
	extension := GetExtension(theSpec);

	// Zero the list and the hash table
	SetLength(gGeneratedIncludeList, 0);
	for i := 0 to kHashLength do
		gHashTable[i] := '';
	gHashMembers := 0;

	// Do recursive call to GetIncludeFileListForFile
	GetIncludeOnlyFileListForFile(theSpec);
	return fileList;
end;

// TO DO
function AnalyzeLinklib(theSpec: FSSpecString): StringArr;
var
	stringList: StringArr;
	fileData, name: AnsiString;
	i, start, ending: Longint;
	foundInPaths: Boolean;
	fileSpec: FSSpecString;
	err: OSErr;
begin
	SetLength(stringList, 0);
	fileData := ReadFileToString(theSpec, err);
	// scan for {$linklib
	for i := 1 to Length(fileData) - Length('{$linklib') do
	begin
		gMainSpec := theSpec; // Bug in FindFileInPaths! Why not use argument?
		
		if fileData[i] = '{' then
		if fileData[i+1] = '$' then
//		WriteLn(Lowercase(Copy(fileData, i+2, 7)));
		if Lowercase(Copy(fileData, i+2, 7)) = 'linklib' then
		begin
			i := i + 9;
			while (fileData[i] in [' ', #9]) and (i < Length(fileData)) do i := i+1;
			start := i;
			i := i+1;
			while not (fileData[i] in [' ', #9, '}']) and (i < Length(fileData)) do i := i+1;
			ending := i;
			name := Copy(fileData, start, ending-start);
			if name[1] = '''' then name := Copy(name, 2, Length(name)-1);
			if name[Length(name)] = '''' then name := Copy(name, 1, Length(name)-1);
			if Copy(name, Length(name) - 5, 6) <> '.dylib' then
				name := name + '.dylib';
			if Copy(name, 1, 3) <> 'lib' then
				name := 'lib' + name;
			
			// Test if any of these exist:
			// name, libname, name.dylib, libname.dylib
			// But really only if it the actual file name ends with dylib
	// check for files
			foundInPaths := FindFileInPaths(name, theSpec, fileSpec);
	// put on fileList with actual name
			if foundInPaths then
			begin
				SetLength(stringList, Length(stringList)+1);
				stringList[High(stringList)] := name;
//				WriteLn('Found dylib =', name);
			end;
//			else
//				WriteLn(name, ' not found');
		end;
	end;
	
	return stringList;
end;


end.
