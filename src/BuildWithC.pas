// Lightweight Pascal IDE, GCC compilation interface unit
// © Ingemar Ragnemalm 2006-2007

// Big revision 090415: CompileCSmart compiles one unit at a time, and
// compares to change date of .o files.
// 2012: Revised for paths instead of FSSpec
// 121009: Fixed bug in NewerObjectFile

{$mode macpas}
unit BuildWithC;
interface
uses
	MacOSAll, LWPGlobals, UtilsTypes, FileUtils, Classes, SysUtils, ProcessUtils,
	ErrorMessageWindow, FindReplaceDialog, AlertsUtils,
	Settings, ColorCoding, Console, BuildWithPascal, BuildWithAda, IncludeAnalyzer,
	BaseUnix;

function CompileC(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
function CompileCSmart(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
function CompileCuda(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
procedure CompileCforPrerunner(mainTargetSpec, theSpec: FSSpecString; buildFinal: Boolean;
				compileCallback: CompilationDoneProc);

// CompileC is called at cmd-B, and rebuilds the entire project.
// CompileCSmart is called at cmd-R, and compiles every file separately.

implementation

uses
	LWPEdit, AboutWindowUnit;

procedure CompileCData(oneLineOfData: AnsiString; privateData: Pointer);
begin
	HelpAppendLn(oneLineOfData);
	ParseGCCErrorMessage(oneLineOfData);
	HelpForceUpdate;
end;

{ListAllFilesOfFolder moved to File(Name)Utils!}

// PRELIMINÄR VERSION FÖR ATT FÅ FSSpecString-LISTA I ST FÖR STRÄNG
// SOM SEDAN LÄTTARE KAN DATUMTESTAS!
function BuildCFileArr(theSpec: FSSpecString): FileArr;
var
	extension, thisExt: Str255;
	{fileList,} pathList: AnsiStringArray;
	listString: AnsiString;
	thePath: AnsiString;
	i, ii, j: Longint;
	pathSpec: FSSpecString;
	fileList, singleList{, tempFileList}: FileArr;
	mainIsInList: Boolean;
	err: OSErr;
	flag, hit: Boolean;
const
	CR = Char(13);
begin
	WriteLn('************* BuildCFileArr', gSettings.CCompileStrategy);
	
	SetLength(singleList, 1);
	singleList[0] := theSpec;
	
	case gSettings.CCompileStrategy of
	kSingleFileStrategy:
		return singleList;
	kFileListStrategy:
		begin
		// "use file list" av nåt slag? Ja, en fil med samma namn som programfilen men extension ".files".
			WriteLn('kFileListStrategy');
			// Öppna fil filnamn.files
//			pathSpec := theSpec;
//			thePath := GetLastToken(pathSpec); // ??? Why???
			thePath := TrimExtension(theSpec);
			pathSpec := thePath + '.files';
			listString := ReadFileToString(pathSpec, err);
			// Bygg om till sträng? Ja!
			for i := 1 to Length(listString) do
				if listString[i] = CR then
					listString[i] := ' ';

// ICKE KLART HÄR
// Att fixa:
// Gå igenom hela listan
// Om ej path, gör
// if FindFileInPaths(fileName, gLastMainFile; var foundSpec: FSSpecString): Boolean;
// och sedan FSSpecToPathNameString för att få en komplett sökväg.

			// Returnera innehållet
			if Length(fileList) > 0 then
				return fileList // MÅSTE BYGGAS
			else
				return singleList;
		end;
	kAllFilesInPathsStrategy:
		begin
			WriteLn('kAllFilesInPathsStrategy');
			extension := GetExtension(theSpec);
			
			listString := '';
			
			// Make list of all paths
			BuildListFromText(gSettings.Paths, pathList);
			AppendCustomPaths('.paths', pathList, theSpec);
			for i := 0 to Length(pathList)-1 do
				WriteLn('Path[', i, ']=', pathList[i]);
			for i := -1 to Length(pathList)-1 do
			begin
				// Add the current dir as path too
				if i = -1 then thePath := '.'
				else thePath := pathList[i];
				
				WriteLn('Searching path = ', thePath);
				
				// For all paths, get a list of all files
				if Length(thePath) > 0 then
				begin
					if i > -1 then pathSpec := RelativePathToFSSpec(theSpec, thePath)
					else pathSpec := theSpec;
					
//					ListAllFilesOfFolder(pathSpec, fileList);
//					tempFileList := ListAllFilesOfFolderFSSpec(pathSpec); never used
					
					// For all files, check the extension.
					for ii := 0 to Length(fileList)-1 do
					begin
						WriteLn('File: ', GetLastToken(fileList[ii]));
						thisExt := GetExtension(fileList[ii]);
						if (thisExt = extension) or (thisExt = '.o') or (thisExt = '.a') or (thisExt = '.dylib') then // Added dylib 110312
						begin
							// or .o or .a?
							// NEEDS TO ADD PATH IF NOT IN MAIN DIR! Added 080208
							SetLength(fileList, Length(fileList)+1);
							if i = -1 then
							begin
								fileList[Length(fileList)-1] := theSpec;
//								listString := listString + fileList[ii] + ' '
							end
							else
							begin
								fileList[Length(fileList)-1] := fileList[ii];
//								listString := listString + thePath+'/'+fileList[ii] + ' ';
							end;
						end;
					end;
					
				end;
			end;
			
			return fileList;
		end;
	kAnalyzeInclude:
		begin
//			WriteLn('*** kAnalyzeInclude - the glorious one!');
HelpAppendLn('Analyzing file dependencies...');
			fileList := GetIncludeFileList(theSpec);
//HelpAppendLn('FILE LIST = "'+ listString + '"');
			// Check if the main file is in the list. If not, add it.
			mainIsInList := false;
			for i := Low(fileList) to High(fileList) do
				if GetLastToken(fileList[i]) = GetLastToken(theSpec) then
					mainIsInList := true;
			if not mainIsInList then
			begin
				SetLength(fileList, Length(fileList)+1);
				fileList[High(fileList)] := theSpec;
			end;
			
			// Add extra files!
			for i := Low(gSettings.fileListArray) to High(gSettings.fileListArray) do
			begin
				hit := false;
				// Not if already in list
				for j := Low(fileList) to High(fileList) do
					if fileList[j] = gSettings.fileListArray[i] then
						hit := true;
				if not hit then
				begin
					SetLength(fileList, Length(fileList)+1);
					fileList[High(fileList)] := gSettings.fileListArray[i];
				end;
			end;
			
			WriteLn('--- FILE LIST:');
			HelpAppendLn('--- FILE LIST:');
			for i := Low(fileList) to High(fileList) do
			begin
				WriteLn(GetLastToken(fileList[i]));
				HelpAppendLn(fileList[i]);
			end;
			WriteLn('--- END OF LIST');
			HelpAppendLn('--- END OF LIST');
			// Den STORA grejen!
			//GetUnusedIncludesFromFile
			// Skriv en procedur som rekursivt samlar ihop alla #includes!
			// En sådan borde rentav kunna vara oberoende av kontext.
			
			if Length(fileList) > 0 then
				return {theSpec.name + ' ' +} fileList
			else
				return singleList;
		end;
	end;
end; {BuildCFileArr}

function BuildCFileList(theSpec: FSSpecString): AnsiString;
var
	extension: Str255;
	fileList, pathList: AnsiStringArray;
	listString: AnsiString;
	thePath: AnsiString;
	i, ii: Longint;
	pathSpec: FSSpecString;
	err: OSErr;
	flag: Boolean;
const
	CR = Char(13);
begin
//	WriteLn('************* BuildCFileList', gSettings.CCompileStrategy);
	
	case gSettings.CCompileStrategy of
	kSingleFileStrategy:
		return GetLastToken(theSpec);
	kFileListStrategy:
		begin
		// "use file list" av nåt slag? Ja, en fil med samma namn som programfilen men extension ".files".
			WriteLn('kFileListStrategy');
			// Öppna fil filnamn.files
			pathSpec := theSpec;
//			thePath := GetLastToken(pathSpec);
//			thePath := TrimExtension(thePath);
//			pathSpec.name := thePath + '.files';
			pathSpec := TrimExtension(theSpec) + '.files';
			listString := ReadFileToString(pathSpec, err);
			// Bygg om till sträng? Ja!
			for i := 1 to Length(listString) do
				if listString[i] = CR then
					listString[i] := ' ';

// Att fixa:
// Gå igenom hela listan
// Om ej path, gör
// if FindFileInPaths(fileName, gLastMainFile; var foundSpec: FSSpecString): Boolean;
// och sedan FSSpecToPathNameString för att få en komplett sökväg.

			// Returnera innehållet
			if Length(listString) > 0 then
				return listString
			else
				return GetLastToken(theSpec);
		end;
	kAllFilesInPathsStrategy:
		begin
			WriteLn('kAllFilesInPathsStrategy');
			extension := GetExtension(theSpec);
			
			listString := '';
			
			// Make list of all paths
			BuildListFromText(gSettings.Paths, pathList);
			AppendCustomPaths('.paths', pathList, theSpec);
			for i := 0 to Length(pathList)-1 do
				WriteLn('Path[', i, ']=', pathList[i]);
			for i := -1 to Length(pathList)-1 do
			begin
				// Add the current dir as path too
				if i = -1 then thePath := '.'
				else thePath := pathList[i];
				
				WriteLn('Searching path = ', thePath);
				
				// For all paths, get a list of all files
				if Length(thePath) > 0 then
				begin
					if i > -1 then pathSpec := RelativePathToFSSpec(theSpec, thePath)
					else pathSpec := theSpec;
					
					ListAllFilesOfFolder(pathSpec, fileList);
					
					// For all files, check the extension.
					for ii := 0 to Length(fileList)-1 do
					begin
						WriteLn('File: ', fileList[ii]);
						if GetExtension(fileList[ii]) = extension then
						// or .o or .a?
						// NEEDS TO ADD PATH IF NOT IN MAIN DIR! Added 080208
						if i = -1 then
							listString := listString + fileList[ii] + ' '
						else
							listString := listString + thePath+'/'+fileList[ii] + ' ';
					end;
					
				end;
			end;
			
			return listString;
		end;
	kAnalyzeInclude:
		begin
//			WriteLn('*** kAnalyzeInclude - the glorious one!');
			// Show file list
			HelpAppendLn('Analyzing file dependencies...');
			listString := GetDisplayIncludeFileListString(theSpec);
			HelpAppendLn('FILE LIST = "'+ listString + '"');
			listString := GetIncludeFileListString(theSpec);

			// Add extra files!
			for i := Low(gSettings.fileListArray) to High(gSettings.fileListArray) do
			begin
				listString += ' ' + gSettings.fileListArray[i];
			end;

			// Den STORA grejen!
			//GetUnusedIncludesFromFile
			// Skriv en procedur som rekursivt samlar ihop alla #includes!
			// En sådan borde rentav kunna vara oberoende av kontext.
			if Length(listString) > 0 then
				return {theSpec.name + ' ' +} listString
			else
				return GetLastToken(theSpec);
		end;
	end;
end; {BuildCFileList}



// I have this somewhere else but can not find it
// It is similar to AppendCustomPaths in BuildWithPascal
// and could replace its later 2/3.
procedure AppendList(src: AnsiStringArray; var dest: AnsiStringArray);
var
	i, j: Longint;
	add: Boolean;
begin
		for i := Low(src) to High(src) do
		begin
			add := true;
			for j := Low(dest) to High(dest) do
				if dest[j] = src[i] then
					add := false; // already in list, don't add
			if add then
			begin
				SetLength(dest, Length(dest)+1);
				dest[High(dest)] := src[i];
			end;
		end;
end;




// Old C-builder, rebuilds whole project.
// Used for "build final" and "compile unit"
function CompileC(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
// buildExecutable = link
// buildIntermediate = only one file, to intermediate-files for later linking step
	var
		cleanName: Str255;
		path, commandLine, pathOnly: AnsiString;
//		err: OSErr;
		i: Longint;
		pathList: AnsiStringArray;
		fileList: AnsiString;
		extType: Longint;
		editIndex: Longint;
		s: AnsiString;
const
	CR = Char(13);
begin
// Future plan: Check if it is a file with "main". Or always try to link? Or link with "run"?
	
// Compile with GCC, to object code!
// Parse error messages with special parser (not same as FPC)

// Borde testa gCurrentProcess? CompilePascal gör det

		if gVersionGCC = kProcessUtilsFailedString then
		begin
			s := ProcessToString('which gcc');
			if Length(s) > 1 then
			begin
				while s[Length(s)] in [#10, #13] do s := Copy(s, 1, Length(s)-1);
				if QuestionAlert('GCC seems to be located at '+s, 'Correct settings?') then
				begin
					gSettings.CCompiler := s; // Should trigger save settings!
					SetSettingsDialog2ItemString('GCCc', 0, s);
					CheckFPCVersion;
				end;
			end;
		end;
		
		if gVersionGCC = kProcessUtilsFailedString then
		begin
			if not QuestionAlert('GCC does not seem to be installed', 'Try compiling anyway?') then
			begin
				CompileC := -1; // Error
				Exit(CompileC);
			end;
		end;
		
		CompileC := noErr;
		
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;

		HelpMainMessage('Compiling ' + GetLastToken(theSpec)); // Show status
		
		// Create a full pathname to the file as AnsiString
		path := theSpec;
		pathOnly := path; // Needed for chdir
		pathOnly := TrimLastToken(pathOnly); // We need the path without filename.
		chdir((pathOnly)); // Set that as current dir
		
HelpAppendLn('Want to compile ' + GetLastToken(theSpec));
		
		
		fileList := BuildCFileList(theSpec);
		WriteLn('************* BuildCFileList', fileList);
		
		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
			if buildFinal then
				// Device
				commandLine := '/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/gcc'+ // gSettings.iOSObjCCompiler +
					' -isysroot /Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS' + giOSSDK + '.sdk ' +
					'-arch armv6 -Os -O3 -miphoneos-version-min=3.0 -x objective-c ' + fileList
			else
				// Simulator - never happens here
				commandLine := '/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/gcc'+ //gSettings.iOSObjCCompiler +
					' -isysroot /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator' + giOSSDK + '.sdk ' +
					'-arch i386 -miphoneos-version-min=3.0 -fobjc-abi-version=2 -fobjc-legacy-dispatch ' + fileList;
		end
		else // Normal
		if buildExecutable then
				commandLine := gSettings.CCompiler + ' ' + fileList{theSpec.name} // LOCAL
		else
				commandLine := gSettings.CCompiler + ' ' + GetLastToken(theSpec);
		
		// Options. Override if .options file exists
		SetLength(pathList, 0);
		AppendCustomPaths('.options', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Default options (according to settings).
			commandLine := commandLine + ' ' + gSettings.COptions; // LOCAL
		end
		else
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ' ' + pathList[i] + ' ';
			end;
		end;

		// ADD FINAL OR DEBUG OPTIONS HERE! DEPENDS ON buildFinal FLAG
		if buildFinal then
			commandLine := commandLine + ' ' + gSettings.CReleaseOptions
		else
			commandLine := commandLine + ' ' + gSettings.CDebugOptions;
		
		// Auto-link runtimes? Check extension for ObjC or C++!
//		if gSettings.lobjc then
		begin
			extType := GetExtensionType(theSpec);
			
			// Link with ObjC lib
			if extType = kExtTypeObjC then
				commandLine := commandLine + ' -lobjc ';
			// Link with ObjC lib
			if extType = kExtTypeCPP then
//				commandLine := commandLine + ' -shared-libgcc -lstdc++-static ';
				commandLine := commandLine + ' -shared-libgcc -lstdc++ ';
		end;
		
		// Add subfolder
		// Är -o rätt väg att visa var den skall lägga det, eller finns nåt i stil med -FU?
		cleanName := TrimExtension(GetLastToken(theSpec));
//		commandLine := commandLine + ' -o ' + './' + kObjectFolderName + '/' + cleanName + '.o -c'; // -c gör att man får objektkod
		
		if not buildExecutable then
//			if buildIntermediate then
				commandLine := commandLine + ' -o "' + kObjectFolderName + '/' + cleanName + '.o" -c'; // Only compile to .o file (in same folder). Do not link.
//			else
//				commandLine := commandLine + ' -c'; // Only compile to .o file (in same folder). Do not link.
		if buildExecutable then
			commandLine := commandLine + ' -o "' + cleanName + '"'; // Normal compile and link
		//commandLine := commandLine + ' -FU' + './' + kObjectFolderName;
		
		// Architectures is easy with GCC!
		if gSettings.target = kUniversalTarget then
			commandLine := commandLine + ' -arch ppc -arch i386 ';
		if gSettings.target = kPPCTarget then
			commandLine := commandLine + ' -arch ppc ';
		if gSettings.target = k386Target then
			commandLine := commandLine + ' -arch i386 ';
		
		// Add frameworks if app
		if buildExecutable then
		begin
			// -framework Carbon
			// When is prebind unnecessary?
			// Future improvement: Optionally auto-add frameworks from #include
			// But that is fixed in smart-build! Is this code dead or just outdated but used?
			// I should check that. Double code, bad.
			BuildListFromText(gSettings.Frameworks, pathList);
			AppendCustomPaths('.frameworks', pathList, theSpec);
	//		if gSettings.autoFramework then
	//			AppendList(frameworkList[], pathList, theSpec);
	// WHY was auto-framwork not included here???
			if gSettings.autoFramework then
			begin
				// We must get the frameworks info from the main file.
				editIndex := FileIsOpen(theSpec);
				if editIndex > 0 then
					AppendList(frameworkList[editIndex], pathList);
			end;
			if Length(pathList) = 0 then
			begin
			// Add Carbon anyway?
			end
			else
			begin
				for i := 0 to Length(pathList)-1 do
				begin
					WriteLn('Linking with framework "', pathList[i], '"');
					if Length(pathList[i]) > 0 then
						commandLine := commandLine + ' -framework ' + pathList[i] + ' ';
				end;
			end;

			// Other linking options, libraries (-l)
			// C link options don't exist yet!
			SetLength(pathList, 0);
			AppendCustomPaths('.link', pathList, theSpec);
			if Length(pathList) > 0 then
			begin
				for i := 0 to High(pathList) do // Length(pathList)-1 do
				begin
					if Length(pathList[i]) > 0 then
						commandLine := commandLine + pathList[i] + ' ';
				end;
			end;
		end;
		
		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -I' + pathList[i] + ' -L' + pathList[i];
		end;
		
		// Additional library paths from gSettings.LibPaths
		BuildListFromText(gSettings.LibPaths, pathList);
		AppendCustomPaths('.libPaths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -L' + pathList[i];
		end;

		HelpAppendLn('*** Command: '+commandLine);
		
//		commandLine := (commandLine); // Entire command line to UTF-8! Or is it better in separate parts?
// NOT NEEDED HERE - done by LaunchCompilation!
		
		// Create object code subfolder
		//cleanName := TrimExtension(theSpec);
//		folderSpec := theSpec;
//		folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
//		err := FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.

		LaunchCompilation(commandLine, theSpec, buildExecutable, buildFinal, compileCallback, @CompileCData, @CompilationDone);
		SelectWindow(helpWind);
end;




function BuildCCompilationCommandLine(theSpec: FSSpecString {final target name and location};
		fileList: AnsiString; buildExecutable, buildFinal, buildIntermediate: Boolean): AnsiString;
var
	extType: Longint;
	cleanName, cleanMainName: AnsiString;
	{path,} commandLine{, pathOnly}: AnsiString;
	{fileList,} pathList: AnsiStringArray;
	i: Longint;
	editIndex: Longint;
	shFileName, envLine, pathOnly, cdLine: AnsiString;
begin
		editIndex := FileIsOpen(theSpec);

		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
			if buildFinal then
				// Device
				commandLine := '/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/gcc'+ // gSettings.iOSObjCCompiler +
					' -isysroot /Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS' + giOSSDK + '.sdk ' +
					'-arch armv6 -Os -O3 -miphoneos-version-min=3.0 -x objective-c ' + fileList
			else
				// Simulator
				// /usr/bin/arm-apple-darwin10-gcc-4.2.1
//				commandLine := '/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/gcc'+ //gSettings.iOSObjCCompiler +
//					' -isysroot /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator' + giOSSDK + '.sdk ' +
//					'-arch armv6 -Os -O3 -miphoneos-version-min=3.0 -x objective-c ' + fileList;
				commandLine := '/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/gcc'+ //gSettings.iOSObjCCompiler +
					' -isysroot /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator' + giOSSDK + '.sdk ' +
					'-arch i386 -miphoneos-version-min=3.0 -fobjc-abi-version=2 -fobjc-legacy-dispatch ' + fileList;
		end
		else
//			commandLine := gSettings.CCompiler + ' "' + fileList{theSpec.name} + '"'; // LOCAL
			commandLine := gSettings.CCompiler + ' ' + fileList{theSpec.name}; // LOCAL

		// Options. Override if .options file exists
		SetLength(pathList, 0);
		AppendCustomPaths('.options', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Default options (according to settings).
			commandLine := commandLine + ' ' + gSettings.COptions + ' ' + extraOptions[editIndex]; // LOCAL + from //uses options
		end
		else
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ' ' + pathList[i] + ' ';
			end;
		end;
		
		// ADD FINAL OR DEBUG OPTIONS HERE! DEPENDS ON buildFinal FLAG
		if buildFinal then
			commandLine := commandLine + ' ' + gSettings.CReleaseOptions + ' ' + extraLinkOptions[editIndex]
		else
			commandLine := commandLine + ' ' + gSettings.CDebugOptions + ' ' + extraLinkOptions[editIndex];
		
		// Auto-link runtimes? Check extension for ObjC or C++!
//		if gSettings.lobjc then
		begin
			extType := GetExtensionType(theSpec); // SNARARE FIL FÖR FIL
			
			// Link with ObjC lib
			if extType = kExtTypeObjC then
				commandLine := commandLine + ' -lobjc ';
			// Link with ObjC lib
			if extType = kExtTypeCPP then
				commandLine := commandLine + ' -shared-libgcc -lstdc++ ';
//				commandLine := commandLine + ' -shared-libgcc -lstdc++-static ';
		end;
		
		// Add subfolder
		// Är -o rätt väg att visa var den skall lägga det, eller finns nåt i stil med -FU?
		cleanName := TrimExtension(GetLastToken(theSpec));
		
		// If intermediate, theSpec is not the actual file being compiled!
		if buildIntermediate then
		begin
			cleanName := TrimExtension(GetLastToken(fileList));
			// The file list may contain '"' delimiters. Make sure to trim them off för the .o file.
			if cleanName[1] = '"' then cleanName := Copy(cleanName, 2, Length(cleanName));
			if cleanName[Length(cleanName)] = '"' then cleanName := Copy(cleanName, 1, Length(cleanName)-1);
		end;
		
		if not buildExecutable then
		begin
			commandLine := commandLine + ' -o "' + kObjectFolderName + '/' + cleanName + '.o" -c'; // Only compile to .o file. Do not link.
//			WriteLn('PROBLEM PLACE? cleanName = ', cleanName);
//			HelpAppendLn('PROBLEM PLACE? cleanName = ' + cleanName);
//			HelpAppendLn('PROBLEM PLACE? GetLastToken(fileList) = ' + GetLastToken(fileList));
		end;
		if buildExecutable then
			commandLine := commandLine + ' -o "' + cleanName + '"'; // Normal compile and link
		
		// Architectures is easy with GCC! (But this does not apply for iPhone.)
		if not editWindIsiOSProgram[gMainFrontIndex] then
		begin
			if gSettings.target = kUniversalTarget then
				commandLine := commandLine + ' -arch ppc -arch i386 ';
			if gSettings.target = kPPCTarget then
				commandLine := commandLine + ' -arch ppc ';
			if gSettings.target = k386Target then
				commandLine := commandLine + ' -arch i386 ';
		end;
		
		if not buildIntermediate then
		begin
			// Add frameworks
			// -framework Carbon
			// When is prebind unnecessary? 10.3 and down?
			// Future improvement: Optionally auto-add frameworks from #include
			BuildListFromText(gSettings.Frameworks, pathList);
			AppendCustomPaths('.frameworks', pathList, theSpec);
			if gSettings.autoFramework then
			begin
				// We must get the frameworks info from the main file.
//				editIndex := FileIsOpen(theSpec);
				if editIndex > 0 then
					AppendList(frameworkList[editIndex], pathList);
			end;
			if Length(pathList) = 0 then
			begin
			// Add Carbon anyway?
			end
			else
			begin
				for i := 0 to Length(pathList)-1 do
				begin
					if Length(pathList[i]) > 0 then
						commandLine := commandLine + ' -framework ' + pathList[i] + ' ';
				end;
			end;
			
			// Other linking options, libraries (-l)
			// C link options don't exist yet!
			SetLength(pathList, 0);
			AppendCustomPaths('.link', pathList, theSpec);
			if Length(pathList) > 0 then
			begin
				for i := 0 to High(pathList) do // Length(pathList)-1 do
				begin
					if Length(pathList[i]) > 0 then
						commandLine := commandLine + pathList[i] + ' ';
				end;
			end;
		end;

		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
// 110209: Allow paths with spaces
//				commandLine := commandLine + ' -I' + pathList[i] + ' -L' + pathList[i];
				commandLine := commandLine + ' -I"' + pathList[i] + '" -L"' + pathList[i] + '"';
		end;
		
		// Additional library paths from gSettings.LibPaths
		BuildListFromText(gSettings.LibPaths, pathList);
		AppendCustomPaths('.libPaths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
// 110209: Allow paths with spaces
//				commandLine := commandLine + ' -L' + pathList[i];
				commandLine := commandLine + ' -L"' + pathList[i] + '"';
		end;

		// Add path to frameworks folder 131106
		cleanMainName := TrimExtension(GetLastToken(theSpec));
		if FileExists(cleanMainName + ' frameworks') then
		begin
			WriteLn('Found ' + cleanMainName + ' frameworks');
			commandLine := commandLine + ' -F"' + cleanMainName + ' frameworks' + '" ';
		end
		else
			WriteLn('Did not find ' + cleanMainName + ' frameworks for ', theSpec);
		// cleanName is NOT reliable here!

// iPhone compilation won't work by direct calls, try script		
		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
// setenv-rad krävs!
		pathOnly := theSpec;
		pathOnly := TrimLastToken(pathOnly); // We need the path without filename.

			cdLine := 'cd "' + pathOnly + '"';
//			envLine := 'setenv PATH "/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin:/Developer/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin"';
			envLine := 'export PATH=/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin:/Developer/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH';
			shFileName := 'temp-exec-'+cleanName+'.sh';
			SaveCommandToFile(cdLine + #10 + envLine + #10 + commandLine, kObjectFolderName + '/' + shFileName);

			if not buildExecutable then
				SaveCommandToFile(cdLine + #10 + envLine + #10 + commandLine, kObjectFolderName + '/1' + shFileName);

			commandLine := '/bin/sh ' + kObjectFolderName + '/' + shFileName;
		end;

		return commandLine;
end;

// True betyder bygg inte
function NewerObjectFile(mainSpec, srcSpec: FSSpecString): Boolean;
var
	err: OSErr;
	objSpec: FSSpecString;
	objTime, srcTime: UInt32;	// For testing modification time
//	testName: AnsiString;
	headersList: FileArr;
	i: Longint;
//	info:Stat;
begin
//	destSpec.name := ':' + appName + '.app:Contents:MacOS:' + appName; // Lokal path ner i bundle
//	testName := ':' + kObjectFolderName + ':' + TrimExtension(srcSpec.name)+'.o';
//	err := FSMakeFSSpec(mainSpec.vRefNum,mainSpec.parID,
//				':' + kObjectFolderName + ':' + TrimExtension(srcSpec)+'.o', objSpec);
	objSpec := TrimLastToken(mainSpec) + '/' + kObjectFolderName + '/' + GetLastToken(TrimExtension(srcSpec))+'.o';
//	err := fpStat(objSpec, info);
//	if err = fnfErr then
//		return false; // Binary does not exist
	
	err := GetFileModTime (objSpec, objTime);
	if err <> noErr then
		return false; // Could not get obj time
	err := GetFileModTime (srcSpec, srcTime);
	if err <> noErr then
		return false; // Could not get src time

	if srcTime > objTime then
		return false; // Got both and the source was newer!

// MÅSTE SÖKA H-FILER OCH TESTA DEM OCKSÅ!
// GetIncludeFileList fast enbart för h-filer.
// Ny: GetIncludeOnlyFileList
	headersList := GetIncludeOnlyFileList(srcSpec);
	for i := 0 to High(headersList) do
	begin
		err := GetFileModTime (headersList[i], srcTime);
		if err = noErr then
			if srcTime > objTime then
				return false;
	end;
	
	return true;
//	return srcTime < objTime; // Src newer than obj?
end;


// Append to the list that is used for final linking, but check if it already exists!
// 110208
procedure AppendToFileListString(var fileList: AnsiString; fileString, fileName: AnsiString);
begin
	// Check if the fileName already exists in the file!
	if Pos(fileString, fileList) <> 0 then
	begin
		ParseGCCErrorMessage(' :0: warning: duplicate file "' + fileName + '"');
	end
	// If it does, signal a warning and don't append.
	else
		fileList := fileList + fileString;
	// Otheriwse, append.
end;


var
	fileArray: FileArr;
	fileList: AnsiString;
	mainTargetSpec: FSSpecString;
	gCompileCallback: CompilationDoneProc;
//	gBuildFinal: Boolean;

procedure BuildNextFile(aborted: Boolean; result: Integer; privateData: Pointer);
var
	thisFile: AnsiString;
	ext: AnsiString;
	commandLine: AnsiString;
	thisFileSpec: FSSpecString;
	isPath: Boolean;
//	tempString: AnsiString; // Debug
begin
		// Pass failure to gCompileCallback!
		if aborted or (result <> 0) then
		begin
			gCompileCallback(1, mainTargetSpec);
			Exit(BuildNextFile); // Error, give up
		end;
		
		// File "list" is last item in array
		// Delete from array, append to final file list
		// if source file, check if .o exists, check date!
		// Do not compile if .o newer!
		
		if Length(fileArray) > 0 then
		begin
			// If in same folder, use only the name
			if TrimLastToken(fileArray[High(fileArray)]) = TrimLastToken(mainTargetSpec) then
//			if (fileArray[High(fileArray)].parID = mainTargetSpec.parID) and
//				(fileArray[High(fileArray)].vRefNum = mainTargetSpec.vRefNum) then
			begin
				thisFile := GetLastToken((fileArray[High(fileArray)]));
				isPath := false;
			end
			else
			begin
				thisFile := fileArray[High(fileArray)];
				isPath := true;
			end;
			
			thisFileSpec := fileArray[High(fileArray)]; // Save since it is deleted from list
			SetLength(fileArray, Length(fileArray)-1);
			
			// If .o or .a, put directly in fileList
			ext := GetExtension(thisFile);
			if (ext = '.o') or (ext = '.a') or (ext = '.dylib') then // Added dylib 110312
			begin
				fileList := fileList + ' "' + thisFile + '"';
				
				// Save frameworks to global list for later inclusion in bundle
				// The list needs to be global on some level, hard to pas through the system otherwise.
				if ext = '.dylib' then
				begin
					SetLength(gFrameworkList, Length(gFrameworkList)+1);
					gFrameworkList[High(gFrameworkList)] := thisFile;
					WriteLn('Put '+thisFile+' on gFrameworkList');
				end;
				
				BuildNextFile(false, 0, nil);
				Exit(BuildNextFile);
			end;
			
			// Otherwise, .o goes in fileList - but only if it isn't already there!
			// AppendToFileListString with test for old occurance 110208
			if isPath then
			begin
//				fileList := fileList + ' "./' + kObjectFolderName + '/'
//									+ TrimExtension(GetLastToken(thisFile)) + '.o"';
									
				AppendToFileListString(fileList, ' "./' + kObjectFolderName + '/'
									+ TrimExtension(GetLastToken(thisFile)) + '.o"', GetLastToken(thisFile));
			end
			else // Local file, keep it short
			begin
//				fileList := fileList + ' "./' + kObjectFolderName + '/'
//									+ TrimExtension(thisFile) + '.o"';
				AppendToFileListString(fileList, ' "./' + kObjectFolderName + '/'
									+TrimExtension(thisFile) + '.o"', thisFile);
			end;
			
//			commandLine := fileList; // DEBUG
			WriteLn(fileList); // DEBUG
			// Check if .o exists, and if it is newer than the source
			if NewerObjectFile(mainTargetSpec, thisFileSpec) then
			begin
				HelpAppendLn('Skipped ' + GetLastToken(thisFileSpec) + ' since object file is newer.');
				BuildNextFile(false, 0, nil);
				Exit(BuildNextFile);
			end;
			
			// Otherwise, compile the unit!
			
			// Future plan: To support Pascal units here, and pass them to compilation with FPC!
			
//			commandLine := BuildCCompilationCommandLine(thisFile);
			if isPath then
				commandLine := BuildCCompilationCommandLine(mainTargetSpec, '"'+thisFile+'"', false, false, true)
			else
				commandLine := BuildCCompilationCommandLine(mainTargetSpec, thisFile, false, false, true);
			
			HelpAppendLn('*** SINGLE-FILE COMPILATION Command: '+commandLine);
			
//			commandLine := (commandLine); // Entire command line to UTF-8! Or is it better in separate parts?
// NOT NEEDED HERE - done by LaunchCompilation!
			
			LaunchCompilation(commandLine, mainTargetSpec, true, false, gCompileCallback{not until finished ???}, @CompileCData, @BuildNextFile);
			SelectWindow(helpWind);
		end
		else
//		if gBuildFinal then // Final pass only if we asked for it - NOT HERE, done in CompileC
		begin
//			All done! Do final compilation!
			commandLine := BuildCCompilationCommandLine(mainTargetSpec, fileList, true, false, false);
			HelpAppendLn('*** Command: '+commandLine);
//			commandLine := (commandLine); // Entire command line to UTF-8! Or is it better in separate parts?
// NOT NEEDED HERE - done by LaunchCompilation!
			
			LaunchCompilation(commandLine, mainTargetSpec, true, false, gCompileCallback, @CompileCData, @CompilationDone);
			SelectWindow(helpWind);
		end;
end;

function CompileCSmart(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
// buildExecutable = link
// buildIntermediate = only one file, to intermediate-files for later linking step
	var
//		cleanName: Str255;
		path{, commandLine}, pathOnly: AnsiString;
//		err: OSErr;
//		i: Longint;
//		pathList: AnsiStringArray;
//		extType: Longint;
		folderSpec: FSSpecString;
//		appDirId: Longint;
//		editIndex: Longint;
const
	CR = Char(13);
begin
// Future plan: Check if it is a file with "main". Or always try to link? Or link with "run"?
	mainTargetSpec := theSpec;
	gCompileCallback := compileCallback;
//	gBuildFinal := buildFinal; // Pass buildFinal to the final pass.
	
// Compile with GCC, to object code!
// Parse error messages with special parser (not same as FPC)

// Borde testa gCurrentProcess? CompilePascal gör det
		
		if gVersionGCC = kProcessUtilsFailedString then
		begin
			if not QuestionAlert('GCC does not seem to be installed', 'Try compiling anyway?') then
			begin
				CompileCSmart := -1; // Error
				Exit(CompileCSmart);
			end;
		end;
		
		CompileCSmart := noErr;
		
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;

		HelpMainMessage('Compiling ' + GetLastToken(theSpec)); // Show status
		
		// Create a full pathname to the file as AnsiString
		path := theSpec;
		pathOnly := path; // Needed for chdir
		pathOnly := TrimLastToken(pathOnly); // We need the path without filename.
		chdir((pathOnly)); // Set that as current dir
		
//HelpAppendLn('Want to compile ' + theSpec.name);
		
		// Clean extraOptions and extraLinkOptions before building lists.
//		editIndex := FileIsOpen(theSpec);
//		if editIndex > 0 then
//		begin
//			extraOptions[editIndex] := '';
//			extraLinkOptions[editIndex] := '';
//		end;
		
		fileList := BuildCFileList(theSpec);
		WriteLn('************* BuildCFileList', fileList);
		
		fileArray := BuildCFileArr(theSpec);
		fileList := '';
		
		// Create object code subfolder
			//cleanName := TrimExtension(theSpec.name);
//			folderSpec := theSpec;
//			folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
			folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName;
//			{err :=} FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
			CreateFolder(folderSpec); // FpMkdir(folderSpec, $777);

		BuildNextFile(false, 0, nil);
end;





// Variant of CompileC.
// Temporary solution. The plan is to build unit by unit.
function CompileCuda(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
// buildExecutable = link
// buildIntermediate = only one file, to intermediate-files for later linking step
	var
		cleanName: Str255;
		path, commandLine, pathOnly: AnsiString;
		i: Longint;
		pathList: AnsiStringArray;
//		fileList: AnsiString;
//		extType: Longint;
		shCommandLine: AnsiString;
		folderSpec: FSSpecString;
//		appDirId: Longint;
		editIndex: Longint;
		firstFramework: Boolean;
const
	CR = Char(13);
begin
		if gVersionCUDA = kProcessUtilsFailedString then
		begin
			if not QuestionAlert('CUDA does not seem to be installed', 'Try compiling anyway?') then
			begin
				CompileCuda := -1; // Error
				Exit(CompileCuda);
			end;
		end;
		
		CompileCuda := noErr;
		
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;

		HelpMainMessage('Compiling ' + GetLastToken(theSpec)); // Show status
		
		// Create a full pathname to the file as AnsiString
		path := theSpec;
		pathOnly := path; // Needed for chdir
		pathOnly := TrimLastToken(pathOnly); // We need the path without filename.
		chdir((pathOnly)); // Set that as current dir
		
//		fileList := BuildCFileList(theSpec);
//		WriteLn('************* BuildCFileList', fileList);
//		commandLine := gSettings.CCompiler + ' ' + fileList{theSpec.name}; // LOCAL
		commandLine := gSettings.CudaCompiler + ' ' + GetLastToken(theSpec); // LOCAL
		
		// Options. Override if .options file exists
		SetLength(pathList, 0);
		AppendCustomPaths('.options', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Default options (according to settings).
			commandLine := commandLine + ' ' + gSettings.CudaOptions; // LOCAL
		end
		else
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ' ' + pathList[i] + ' ';
			end;
		end;

		// ADD FINAL OR DEBUG OPTIONS HERE! DEPENDS ON buildFinal FLAG
//		if buildFinal then
//			commandLine := commandLine + ' ' + gSettings.CReleaseOptions
//		else
//			commandLine := commandLine + ' ' + gSettings.CDebugOptions;
		
		// Add subfolder
		// Är -o rätt väg att visa var den skall lägga det, eller finns nåt i stil med -FU?
		cleanName := TrimExtension(GetLastToken(theSpec));
//		commandLine := commandLine + ' -o ' + './' + kObjectFolderName + '/' + cleanName + '.o -c'; // -c gör att man får objektkod
		
		if not buildExecutable then
//			if buildIntermediate then
				commandLine := commandLine + ' -o "' + kObjectFolderName + '/' + cleanName + '.o" -c'; // Only compile to .o file (in same folder). Do not link.
//			else
//				commandLine := commandLine + ' -c'; // Only compile to .o file (in same folder). Do not link.
		if buildExecutable then
			commandLine := commandLine + ' -o ' + cleanName; // Normal compile and link
		//commandLine := commandLine + ' -FU' + './' + kObjectFolderName;
		
		// Add link options
		if buildExecutable then
		begin
			// Other linking options, libraries (-l)
			// C link options don't exist yet!
			SetLength(pathList, 0);
			AppendCustomPaths('.link', pathList, theSpec);
			if Length(pathList) > 0 then
			begin
				for i := 0 to High(pathList) do // Length(pathList)-1 do
				begin
					if Length(pathList[i]) > 0 then
						commandLine := commandLine + pathList[i] + ' ';
				end;
			end;
		end;
		
		// Add frameworks if app
		// New 111120
		// Frameworks are included with additions like "-Xlinker -framework,OpenGL,-framework,GLUT"
		// Note that all other linking extras must go here too.
		if buildExecutable then
		begin
			BuildListFromText(gSettings.Frameworks, pathList);
			if gSettings.autoFramework then
			begin
				// We must get the frameworks info from the main file.
				editIndex := FileIsOpen(theSpec);
				if editIndex > 0 then
					AppendList(frameworkList[editIndex], pathList);
			end;
			AppendCustomPaths('.frameworks', pathList, theSpec);
			if Length(pathList) > 0 then
			begin
				commandLine := commandLine + ' -Xlinker ';
				firstFramework := true;
				for i := 0 to Length(pathList)-1 do
				begin
					if Length(pathList[i]) > 0 then
					begin
						if not firstFramework then
							commandLine := commandLine + ',';	
						commandLine := commandLine + '-framework,' + pathList[i];
						firstFramework := false;
					end;
				end;
				commandLine := commandLine + ' ';
			end;
		end;
		
		
		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -I' + pathList[i] + ' -L' + pathList[i];
		end;
		
		// Additional library paths from gSettings.LibPaths
		BuildListFromText(gSettings.LibPaths, pathList);
		AppendCustomPaths('.libPaths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -L' + pathList[i];
		end;

		HelpAppendLn('*** Command: '+commandLine);
		
		// Create object code subfolder
		//cleanName := TrimExtension(theSpec);
//		folderSpec := theSpec;
//		folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
//		{err :=} FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
		folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName;
		CreateFolder(folderSpec); // FpMkdir(folderSpec, $777);

// CUDA won't run programatically, but it works with sh.
// Isn't this incorrect? Old-style?
		SaveCommandToFile(commandLine, kObjectFolderName + '/lwp-cuda-build.sh');
		shCommandLine := '/bin/sh ' + kObjectFolderName + '/lwp-cuda-build.sh';
		LaunchCompilation(shCommandLine, theSpec, buildExecutable, buildFinal, compileCallback, @CompileCData, @CompilationDone);
//		LaunchCompilation(commandLine, theSpec, buildExecutable, buildFinal, compileCallback, @CompileCData, @CompilationDone);
		SelectWindow(helpWind);
end;

procedure CompileCforPrerunner(mainTargetSpec, theSpec: FSSpecString; buildFinal: Boolean;
				compileCallback: CompilationDoneProc);
var
	isPath, isLocal: Boolean;
	commandLine: AnsiString;
begin
	chdir(TrimLastToken(mainTargetSpec)); // Set as current dir

	isPath := TrimLastToken(theSpec) <> TrimLastToken(mainTargetSpec);
	isLocal := TrimLastToken(theSpec) = '';

	if isLocal then
		commandLine := BuildCCompilationCommandLine(mainTargetSpec, theSpec, false, buildFinal, true)
	else
	if isPath then
		commandLine := BuildCCompilationCommandLine(mainTargetSpec, theSpec, false, buildFinal, true)
	else
		commandLine := BuildCCompilationCommandLine(mainTargetSpec, GetLastToken(theSpec), false, buildFinal, true);
	
	WriteLn('Launch compilation "', commandLine, '"');
	HelpAppendLn('Compiling additional file '+ TrimLastToken(theSpec)+ ': '+commandLine);
// Possible error: Should not "buildApp" here, right? 141127
//	LaunchCompilation(commandLine, mainTargetSpec, true, false, compileCallback{not until finished ???}, @CompileCData, @CompilationDone);
	LaunchCompilation(commandLine, mainTargetSpec, false, false, compileCallback{not until finished ???}, @CompileCData, @CompilationDone);
end;


end.
