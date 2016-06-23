// Lightweight IDE, Pascal compilation interface unit
// This is the main build unit, including code that is used by
// other Build units, like BuildBundle.
// © Ingemar Ragnemalm 2006-2013

// 130120: Introduced IsCommandLineToolMode to support auto-commandline-mode (switching to command-line mode if no resource folder is around)

{$I-}

{$mode macpas}
unit BuildWithPascal;
interface
uses
	MacOSAll, LWPGlobals, FileUtils, ProcessUtils, AlertsUtils, UtilsTypes,
	SysUtils, ErrorMessageWindow, FindReplaceDialog, Settings, ColorCoding,
	Console, LWPEdit, IncludeAnalyzer, AboutWindowUnit;

type
	CompilationDoneProc = PROCEDURE(err: OSErr; theSpec: FSSpecString);

//procedure CompilePascal(theSpec: FSSpecString; buildApp: Boolean);
procedure CompilePascal(theSpec: FSSpecString; buildApp, buildFinal: Boolean; compileCallback: CompilationDoneProc);
procedure PascalPreRunner(theSpec: FSSpecString; buildApp, buildFinal: Boolean; compileCallback: CompilationDoneProc);
procedure LaunchCompilation(commandLine: AnsiString;
					theSpec: FSSpecString; buildApp, buildFinal: Boolean;
					compileCallback: CompilationDoneProc;
					dataProc{compilationDataCallback}: ProcessDataCallback; {Var det inte nåt skumt med namnen här?}
					doneProc{compilationDoneCallback}: ProcessDoneProc);
procedure CompilationDone(aborted: Boolean; result: Integer; privateData: Pointer);

// For use by other Build units as well.
procedure AppendCustomPaths(ext: AnsiString; var pathList: AnsiStringArray; targetSpec: FSSpecString);
function BuildBundle(theSpec: FSSpecString; finalBuild: Boolean): OSErr;

// For BuildWithC to record shlibs to be copied/modified in BuildBundle
var
	gFrameworkList: StringArr;

// All that we must remember until later
// Must be global since CompileJava uses a separate CompilationDone!
var
	gCompilationData: record
		theSpec: FSSpecString;
		buildApp, buildFinal: Boolean;
		compileCallback: CompilationDoneProc;
	end;

implementation
	uses
		BaseUnix, BuildWithC;

// Finish of both C and Pascal compilations, doing a BuildBundle
procedure CompilationDone(aborted: Boolean; result: Integer; privateData: Pointer);
var
	srcSpec: FSSpecString;
	cleanName: AnsiString;
	err: OSErr;
//	fndrInfo: FInfo;
	info: stat;
begin
	if aborted then
	begin
		HelpAppendLn('Compilation aborted');
		HelpMainMessage(''); // Status
		Exit(CompilationDone);
	end;

	if (not gCompilationData.buildApp) or (result <> noErr) or IsCommandLineToolMode(gCompilationData.theSpec) or (not editMainWindIsProgram[gMainFrontIndex]) then
	// not editMainWindIsProgram[gMainFrontIndex] new 110311, library
	begin
	// All reasons NOT to build a bundle
		HelpMainMessage(''); // Status

		// DEBUG
(*		if not gCompilationData.buildApp then
			HelpAppendLn('I was not told to build a bundle');
		if result <> noErr then
			HelpAppendLn('Error, no bundle');
		//if gFlags^^.commandLineToolMode then
		if IsCommandLineToolMode(theSpec) then
			HelpAppendLn('No bundle in command-line mode');
		if not editMainWindIsProgram[gMainFrontIndex] then
			HelpAppendLn('Library, no bundle');*)

	end
	else
	// If all right:
	// BuildBundle if it was supposed to be done
	begin
		// Cut away .p from the end
		cleanName := TrimExtension(GetLastToken(gCompilationData.theSpec));
		// Test if the file exists
		srcSpec := TrimExtension(gCompilationData.theSpec);
		//err := FSpGetFInfo(srcSpec,fndrInfo);
		err := fpStat(srcSpec, info); // File exists?
		if err <> noErr then
		begin
			// Don't build!
			HelpAppendLn('Can''t find executable "' + cleanName + '", no bundle build.');
			//NumToString(err,  tempStr);
			//HelpAppendLn('Missing executable: ' + cleanName + ' Error ' + tempStr);
		end;
		// Otherwise, build bundle!
		
// Icke-auto:
//		if not gFlags^^.commandLineToolMode then
// Auto:
//		if FileExists(TrimExtension(srcSpec) + ' resources') then

//		if not IsCommandLineToolMode(TrimExtension(gCompilationData.theSpec)) then
		if not IsCommandLineToolMode(gCompilationData.theSpec) then
			result := BuildBundle(gCompilationData.theSpec, gCompilationData.buildFinal)
		else
			HelpAppendLn('No bundle build, no resources for '+ TrimExtension(gCompilationData.theSpec));
		SetLength(gFrameworkList, 0); // Clear framework list
	end;
	
	if result <> noErr then
	begin
		HelpAppendLn('Compilation NOT successful!');
		HelpMainMessage(''); // Status
		Exit(CompilationDone);
	end;
	
	if gCompilationData.compileCallback <> nil then
		gCompilationData.compileCallback(result, gCompilationData.theSpec);
		// e.g. Run after compile
end; {CompilationDone}

procedure CompilePascalData(oneLineOfData: AnsiString; privateData: Pointer);
begin
	HelpAppendLn(oneLineOfData);
	ParseErrorMessage(oneLineOfData);
	HelpForceUpdate;
end;

procedure LaunchCompilation(commandLine: AnsiString;
					theSpec: FSSpecString; buildApp, buildFinal: Boolean;
					compileCallback: CompilationDoneProc;
					dataProc{compilationDataCallback}: ProcessDataCallback; {Var det inte nåt skumt med namnen här?}
					doneProc{compilationDoneCallback}: ProcessDoneProc);
begin
		// Save info in globals (or privateData pointer)
		gCompilationData.theSpec := theSpec;
		gCompilationData.buildApp := buildApp;
		gCompilationData.buildFinal := buildFinal;
		gCompilationData.compileCallback := compileCallback;
		
		if gCurrentProcess <> nil then
		begin
			if ProcessRunning(gCurrentProcess) then
				HelpAppendLn('Old running process disposed [THIS SHOULD NOT HAPPEN]')
			else
				;
			ProcessDispose(gCurrentProcess);
		end;
		
//		gCurrentProcess := ProcessLaunch(MacRomanToUTF8(commandline), true);
		gCurrentProcess := ProcessLaunch((commandline), true);
		ProcessSetCallbacks(gCurrentProcess, dataProc{@CompilePascalData}, doneProc{@CompilationDone}, nil);

// DEBUG
		if ProcessRunning(gCurrentProcess) then
//			HelpAppendLn('Process is now running')
		else
			HelpAppendLn('BUG ALERT! Process not running! [THIS SHOULD NOT HAPPEN]')
end;

procedure AppendCustomPaths(ext: AnsiString; var pathList: AnsiStringArray; targetSpec: FSSpecString);
var
	pathFileSpec: FSSpecString;
	pathList2: AnsiStringArray;
	add: Boolean;
	s: AnsiString;
	err: OSErr;
	i, j, k: Longint;
	info: stat;
	flag: Boolean;
begin
//	s := TrimExtension(targetSpec.name) + ext; // ' paths';
//	WriteLn('Paths file name = "', s, '"');

//	err := FSMakeFSSpec(targetSpec.vRefNum, targetSpec.parID, TrimExtension(targetSpec.name) + ext, pathFileSpec);
	pathFileSpec := TrimExtension(targetSpec) + ext;
	err:=fpStat(pathFileSpec, info);

	if err <> noErr then // file doesn't exist
	begin
//		s := 'all' + ext; // ' paths';
//		err := FSMakeFSSpec(targetSpec.vRefNum, targetSpec.parID, 'all' + ext, pathFileSpec);
		pathFileSpec := TrimLastToken(targetSpec) + 'all' + ext;
		err:=fpStat(pathFileSpec, info);
	end;
	if err = noErr then // file exists
	begin
		s := ReadFileToString(pathFileSpec, err);
		if Length(s) = 0 then Exit(AppendCustomPaths);
		
		BuildListFromText(s, pathList2);
		
		for i := Low(pathList2) to High(pathList2) do
		begin
//			WriteLn('pathlist2[', i, ']=', pathList2[i]);
			add := true;
			for j := Low(pathList) to High(pathList) do
			begin
				for k := 2 to Length(pathList2[i]) do // Skip comment
					if pathList2[i][k-1] = '/' then
						if pathList2[i][k] = '/' then
						begin
							pathList2[i] := Copy(pathList2[i], 1, k-2);
							Leave;
						end;
				if Length(pathList2[i]) > 0 then
					while pathList2[i][1] in [' ', Char(9)] do // Skip leading spaces
						pathList2[i] := Copy(pathList2[i], 2, Length(pathList2[i])-1);
				if Length(pathList2[i]) > 0 then
					while pathList2[i][Length(pathList2[i])] in [' ', Char(9)] do // Skip trailing spaces
						pathList2[i] := Copy(pathList2[i], 1, Length(pathList2[i])-1);
				if pathList2[i] = '' then
					add := false; // empty, don't add				
				if pathList[j] = pathList2[i] then
					add := false; // already in list, don't add
//				if Length(pathList2[i]) >= 2 then
//					if Copy(pathList2[i], 1, 2) = '//' then
//						add := false; // Comment!
			end;
			if add then
			begin
				SetLength(pathList, Length(pathList)+1);
				pathList[High(pathList)] := pathList2[i];
			end;
		end;
	end;
end;

(*
This shouldn't be needed, it is just a question of a bug-free CocoaAll.

procedure CheckForCocoaAll(var pathList: AnsiStringArray; targetSpec: FSSpecString);
var
	i, j, ii: Longint;
	allFiles: FileArr;
begin
	HelpAppendLn('CheckForCocoaAll');
	
		// Added 101011
		// ObjP special: find the index to global arrs, look for
		// "CocoaAll" in its usesList!
//		if buildApp then
//		begin
// Borde testa om det är Pascal.
			// Check if we already have it
			for i := Low(pathList) to High(pathList) do
			begin
				if pathList[i] = '/Developer/ObjectivePascal/Units' then
				begin
					HelpAppendLn('ALREADY INCLUDED NO MATCH /Developer/ObjectivePascal/Units');
					Exit(CheckForCocoaAll);
				end;
			end;
			
			// Get the list of all open files, find the target
			allFiles := GetAllEditWindows;
//			for i := 1 to kMaxEditWindows do
			for i := Low(allFiles) to High(allFiles) do
				if targetSpec.parID = allFiles[i].parID then
				if targetSpec.vRefNum = allFiles[i].vRefNum then
				if targetSpec.name = allFiles[i].name then
				begin
					HelpAppendLn('Found the file ' + targetSpec.name + ' in allFiles');
					if Length(usesList[i]) = 0 then
						HelpAppendLn('No usesList for ' + targetSpec.name);
				// Found the target, now check if it uses CocoaAll!
					for j := Low(usesList[i]) to High(usesList[i]) do
						if Copy(usesList[i][j], 1, 8) = 'CocoaAll' then
						begin
							SetLength(pathList, Length(pathList)+1);
							pathList[High(pathList)] := '/Developer/ObjectivePascal/Units';
							HelpAppendLn('HIT');
						end
						else
							HelpAppendLn('NO MATCH at: '+Copy(usesList[i][j], 1, 8));
				end;
//		end;
end;
*)

var
	gPreRunnerData: record
// Original command-line args given to PreRunner.
		theSpec: FSSpecString;
		buildApp, buildFinal: Boolean;
		compileCallback: CompilationDoneProc;
// More data generated/used by PreRunner.
		fileList: FileArr;
		listIndex: Longint;
		linkWithCPP: Boolean;
	end;

function BuildPascalCommandline(theSpec: FSSpecString; buildApp, buildFinal: Boolean; target: Longint; targetExtension: Boolean): AnsiString;
	var
		folderSpec: FSSpecString;
//		cleanName: Str255;
//		path, 
		commandLine{, pathOnly}: AnsiString;
//		err: OSErr;
		i: Longint;
//		appDirId: Longint;
		cliName: AnsiString;
//		settingsPaths: AnsiString;
//		addPath: AnsiString;
		pathList: AnsiStringArray;
const
	CR = Char(13);
begin
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;
		
		// Create a full pathname to the file as AnsiString
//		path := theSpec;
//		pathOnly := theSpec; // Needed for chdir
//		ConvertPathToCommandLineFormat(path); // Allow spaces in name ÄNDRA till ""
		
//WriteLn('theSpec ', theSpec.vRefNum, ' ', theSpec.parID);
//WriteLn('CLI format path: "', path, '"');
		
		cliName := GetLastToken(theSpec);
//		ConvertPathToCommandLineFormat(cliName); // Allow spaces in name ÄNDRA till ""
		
//WriteLn('CLI format name: "', cliName, '"');
		
		// We need the path without filename.
//		pathOnly := path;
//		pathOnly := TrimLastToken(pathOnly);
WriteLn('Try chdir to: "', TrimLastToken(theSpec), '"');
//		chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
		chdir(TrimLastToken(theSpec)); // Set that as current dir
WriteLn('chdir OK');
		
		//commandLine := '/usr/local/bin/ppcppc ' + path + ' -Mmacpas -Tdarwin -k"-framework Carbon -prebind" -Fi/Developer/Pascal/UPInterfaces -Fu/Developer/Pascal/UPInterfaces';
		//commandLine := gSettingsCompiler + ' ' + path + ' ' + gSettingsOptions; FULL PATH
		//commandLine := '/usr/local/bin/ppc386' + ' ' + cliName{theSpec.name} + ' ' + gSettingsOptions; // LOCAL
		
		// Preliminary, hard-coded JVM support!
		// Can we put class files in a folder? Build a bundle? .apk?
		if editWindIsJVMProgram[gMainFrontIndex] then
		begin
			HelpAppendLn('Auto-switch to JVM compiler!');
			commandLine := '/usr/local/bin/ppcjvm -O2 -g ' + cliName + ' -XP ' + gSettings.Options + ' -FD/usr/local/bin/ ';
			WriteLn(commandLine);
			HelpAppendLn(commandLine);
			return commandLine;
		end
		else
		if editWindIsAndroidProgram[gMainFrontIndex] then
		begin
			HelpAppendLn('Auto-switch to JVM Android compiler!');
			commandLine := '/usr/local/bin/ppcjvm -O2 -g ' + cliName + ' -Tandroid ' + gSettings.Options + ' -FD/usr/local/bin/ ';
			return commandLine;
		end
		else
		if editWindIsiOSProgram[gMainFrontIndex] then
		// Auto-switch to iPhone compiler
		begin
			HelpAppendLn('Auto-switch to iPhone compiler!');

			if (giOSSDK = '') or (giOSSDK = kProcessUtilsFailedString) then
				CheckiOSSDKVersion;
			if buildFinal then
			begin
				// Build for device
				// Some settings are still hard-coded. Do we need to change them?
				commandLine := gSettings.iOSCompiler + ' ' + 
					'-Cirot -XX -Cfvfpv2 -Fu'+gSettings.iOSInterfacesPath + ' '+
					'-FD/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin ' +
					'-XR/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS' + giOSSDK + '.sdk ' +
					cliName;
			end
			else
			begin
				// Build for simulator
				// Some settings are still hard-coded. Do we need to change them?
				commandLine := gSettings.iPhoneSimulatorCompiler + ' ' +
					'-Cirot -XX -Tiphonesim -Fu'+gSettings.iOSInterfacesPath +' '+
					'-XR/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator' + giOSSDK +'.sdk ' +
					cliName;
			end;
		end
		else
(*		if editWindIsCocoaProgram[gMainFrontIndex] then
		// Auto-switch to ObjP compatible compiler
		// (What do I mean by this? This looks strange.)
		begin
			if target = kPPCTarget then
				commandLine := gSettings.PPCCompiler2 + ' ' + cliName{theSpec.name}
			else {386}
				commandLine := gSettings.386Compiler2 + ' ' + cliName{theSpec.name};
		end
		else*)
		// Otherwise use normal FPC
		if target = kPPCTarget then
		begin
//			MessageAlert('Target seems to be PPC', 'when compiling');
			commandLine := gSettings.PPCCompiler + ' ' + cliName{theSpec.name}
		end
		else {386}
			commandLine := gSettings.x86Compiler + ' ' + cliName{theSpec.name};
		
		if not buildApp then
			commandLine := commandLine + ' -Cn ';
		
		// Options. Override if .options file exists
		SetLength(pathList, 0);
		AppendCustomPaths('.options', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Default options (according to settings).
			commandLine := commandLine + ' ' + gSettings.Options; // LOCAL
		end
		else
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ' ' + pathList[i] + ' ';
			end;
		end;

		// Final build or debug build?
		if buildFinal then
			commandLine := commandLine + ' ' + gSettings.ReleaseOptions
		else
			commandLine := commandLine + ' ' + gSettings.DebugOptions;
		
		// Add subfolder
		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
			if buildFinal then
				commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/ARM'
			else
//				commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/386';
				commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/386'
				  + ' -Fu' + './' + kObjectFolderName + '/386'; // Added 150223
		end
		else
			if target = kPPCTarget then
				commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/PPC'
				  + ' -Fu' + './' + kObjectFolderName + '/PPC' // Added 150223
			else
				commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/386'
				  + ' -Fu' + './' + kObjectFolderName + '/386'; // Added 150223
		
		// If we are building universal, build with target extension
		if targetExtension then
		begin
			// Get name without extension? Or just standard name for temporary binary
			// Append _ppc or _386
			if target = kPPCTarget then
				commandLine := commandLine + ' -o' + kObjectFolderName + '/LWP_U_TEMP_BINARY_PPC '
//				commandLine := commandLine + ' -o' + kObjectFolderName+'/PPC' + '/LWP_U_TEMP_BINARY_PPC '
			else
				commandLine := commandLine + ' -o' + kObjectFolderName + '/LWP_U_TEMP_BINARY_386 ';
//				commandLine := commandLine + ' -o' + kObjectFolderName+'/386' + '/LWP_U_TEMP_BINARY_386 ';
		end;
		
		// Add frameworks
		// -k"-framework Carbon -prebind"
		// When is prebind unnecessary?
		commandLine := commandLine + ' -k"' + gSettings.LinkOptions + ' '; {gSettings.LinkOptions new i 0.3.2}
		// If frameworks folder exists (NEW 131106)
		if FileExists(TrimExtension(cliname) + ' frameworks') then
		begin
			commandLine := commandLine + '-F''' + TrimExtension(cliname) + ' frameworks' + ''' ';
		end;
//		else
//			WriteLn('******* ', TrimExtension(cliname)+ ' frameworks does not exist');
		BuildListFromText(gSettings.Frameworks, pathList);
		AppendCustomPaths('.frameworks', pathList, theSpec);
		if Length(pathList) > 0 then
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + '-framework ' + pathList[i] + ' ';
			end;
		end;
		// Link options, for libraries etc
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

		// Add extra files (after processing with PreRunner)
		for i := 0 to High(gPreRunnerData.fileList) do
			if Length(gPreRunnerData.fileList[i]) > 0 then
				commandLine := commandLine + ' ''' + gPreRunnerData.fileList[i] + ''' ';
		// Fix 140909: Added ' for file names/paths with spaces. (Typically libraries.)
		
// prebind only 10.3 or older which is not supported anyway!
//			commandLine := commandLine + ' -prebind"';
		commandLine := commandLine + ' "';
		
		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to High(pathList) do // Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -Fi"' + pathList[i] + '" -Fu"' + pathList[i]+'"';
		end;
//		CheckForCocoaAll(pathList, theSpec);
		
		// Additional library paths from gSettings.LibPaths
		BuildListFromText(gSettings.LibPaths, pathList);
		AppendCustomPaths('.libPaths', pathList, theSpec);
		for i := 0 to High(pathList) do // Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -Fu"' + pathList[i]+'"';
		end;

HelpAppendLn('*** Command: '+commandLine);

		// Create object code subfolder
		//cleanName := TrimExtension(theSpec.name);
//		folderSpec := theSpec;
//		folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
		folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
//		{err :=} FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
//		FpMkdir(folderSpec, $777); // Mode???
		CreateFolder(folderSpec);
		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
			if buildFinal then
				folderSpec := folderSpec + '/ARM'
//				folderSpec.name := ':' + kObjectFolderName + ':ARM'
			else
				folderSpec := folderSpec + '/386';
//				folderSpec.name := ':' + kObjectFolderName + ':386';
		end
		else
			if target = kPPCTarget then
//				folderSpec.name := ':' + kObjectFolderName + ':PPC'
				folderSpec := folderSpec + '/PPC'
			else
//				folderSpec.name := ':' + kObjectFolderName + ':386';
				folderSpec := folderSpec + '/386';
//		FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
//		FpMkdir(folderSpec, $777); // Mode???
		CreateFolder(folderSpec);
		
		return commandLine;
	end; {BuildPascalCommandline}




// PPC and 386 done, build universal with lipo, build bundle!
procedure Compilation386Done(aborted: Boolean; result: Integer; privateData: Pointer);
var
	cleanName: Str255;
//	srcSpec: FSSpecString;
	commandLine: AnsiString;
	msg: StringArr;
	n: Longint;
begin
	if aborted then
	begin
		HelpAppendLn('386 compilation aborted');
		HelpMainMessage(''); // Status
		Exit(Compilation386Done);
	end;

	if result <> noErr then
	begin
		HelpAppendLn('386 compilation failed');
		HelpMainMessage(''); // Status
		Exit(Compilation386Done);
	end;
	
	// If "compile unit"...
	if not gCompilationData.buildApp then
		HelpAppendLn('386 done, now building universal object file...')
	else
		HelpAppendLn('386 done...');
	
	cleanName := TrimExtension(GetLastToken(gCompilationData.theSpec));
	// Test if the file exists
//	srcSpec := gCompilationData.theSpec;
//	srcSpec.name := cleanName;
	
	//LIPO från LWP_U_TEMP_BINARY_PPC och LWP_U_TEMP_BINARY_386
	if gCompilationData.buildApp then
		commandLine := '/usr/bin/lipo -create -arch i386 ' + kObjectFolderName+'/LWP_U_TEMP_BINARY_386 -arch ppc '+kObjectFolderName+'/LWP_U_TEMP_BINARY_PPC -output ' + cleanName
	else
		commandLine := '/usr/bin/lipo -create -arch i386 ' + kObjectFolderName+'/386/' + cleanName + '.o -arch ppc '
															 +kObjectFolderName+'/PPC/' + cleanName + '.o -output ' + kObjectFolderName+ '/' +cleanName + '.o';
	
	HelpAppendLn('lipo command: ' + commandLine);
//	commandLine := '/usr/bin/lipo -create -arch i386 intermediate-files/LWP_U_TEMP_BINARY_386 -arch ppc intermediate-files/LWP_U_TEMP_BINARY_PPC -output test';
//	HelpAppendLn('lipo command: ' + commandLine);
//	msg := ProcessLaunchToStringArr(MacRomanToUTF8(commandLine));
	msg := ProcessLaunchToStringArr((commandLine));
	for n := 0 to Length(msg)-1 do
		HelpAppendLn(msg[n]);

	CompilationDone(aborted, result, privateData);
end;

// PPC phase done, now do 386!
procedure CompilationPPCDone(aborted: Boolean; result: Integer; privateData: Pointer);
var
	commandLine: AnsiString;
begin
	if aborted then
	begin
		HelpAppendLn('PPC compilation aborted');
		HelpMainMessage(''); // Status
		Exit(CompilationPPCDone);
	end;
	
	if result <> noErr then
	begin
		HelpAppendLn('PPC compilation failed');
		HelpMainMessage(''); // Status
		Exit(CompilationPPCDone);
	end;
	
	HelpAppendLn('PPC done, now making 386...');
	
	commandLine := BuildPascalCommandline(gCompilationData.theSpec, gCompilationData.buildApp, gCompilationData.buildFinal, k386Target, true);
	
	LaunchCompilation(commandLine, gCompilationData.theSpec, gCompilationData.buildApp, gCompilationData.buildFinal, gCompilationData.compileCallback, @CompilePascalData, @Compilation386Done);
end;

// If the warning about missing compiler has been given, assume it is OK.
var
	warningDone: Boolean = false;

//procedure PrerunnerStep(aborted: Boolean; result: Integer; privateData: Pointer);
procedure PrerunnerStep(err: OSErr; theSpec: FSSpecString);
var
	ext: Integer;
	found, foundPotential: Boolean;
	fileName, outputFile, srcPath: AnsiString;
	inTime, outTime: UInt32;
begin
	WriteLn('*** PrerunnerStep running from ', gPrerunnerData.listIndex, ' to ', High(gPrerunnerData.fileList));
//	if aborted then Exit(PrerunnerStep);
//	if Length(gPrerunnerData.fileList) > 0 then
//	begin
		found := false; // Found something to compile
//		foundPotential := false; // Found something that should have been compiled (but not necessarily now)
		// Find the next "extra" file to compile
		while (not found) and (gPrerunnerData.listIndex <= High(gPrerunnerData.fileList)) do
		begin
			gPrerunnerData.listIndex += 1;
			WriteLn('gPrerunnerData.listIndex = ', gPrerunnerData.listIndex);
			if gPrerunnerData.listIndex <= High(gPrerunnerData.fileList) then
			begin
// srcPath replaces gPrerunnerData.fileList[gPrerunnerData.listIndex]
				srcPath := gPrerunnerData.fileList[gPrerunnerData.listIndex];
				srcPath := RelativePathToFSSpec(gPrerunnerData.theSpec, srcPath);
				
				ext := GetExtensionType(srcPath);
				WriteLn(srcPath, ' has extension ', ext);
				if ext in [kExtTypeC, kExtTypeObjC, kExtTypeCPP, kExtTypeCuda] then
				begin
					// Test change time of source and .o!
					fileName := srcPath;
					outputFile := TrimLastToken(gPrerunnerData.theSpec) + '/' + kObjectFolderName +
						'/' + TrimExtension(GetLastToken(fileName)) + '.o';
//					foundPotential := true; // Should change to .o
					err := GetFileModTime(fileName, inTime);
					if err = noErr then
					begin
						err := GetFileModTime(outputFile, outTime);
						if (err <> noErr) or (inTime > outTime) then // source newer or .o does not exist 
						begin
							WriteLn('TrimExtension(GetLastToken(fileName))=', TrimExtension(GetLastToken(fileName)));
							WriteLn('fileName=', fileName);
							if err <> noErr then WriteLn(outputFile, ' does not exist!');
							if inTime > outTime then WriteLn('Source of ', fileName, ' is newer than ', outputFile);
							found := true;
							if ext = kExtTypeCPP then
								gPrerunnerData.linkWithCPP := true;
							if ext = kExtTypeObjC then
								;
							if ext = kExtTypeCuda then
								;
						end
						else
						begin
							WriteLn('File test indicates ', fileName, ' does not need recompilation.');
							gPrerunnerData.fileList[gPrerunnerData.listIndex] := kObjectFolderName + '/' + TrimExtension(GetLastToken(fileName)) + '.o';
						end;
					end
					else
						WriteLn('Error in GetFileModTime - no input for ', srcPath, '?');
				end
				else
					WriteLn('Unknown extension on ', srcPath); // if ext
			end; // if gPrerunnerData.listIndex
		end; // while
		WriteLn('Done at = ', gPrerunnerData.listIndex);
		// Now compile it!
		if found then
		begin
WriteLn('*** PrerunnerStep at ', gPrerunnerData.listIndex);
			fileName := gPrerunnerData.fileList[gPrerunnerData.listIndex];
//			outputFile := TrimLastToken(gPrerunnerData.theSpec) + '/' + kObjectFolderName + '/' + TrimExtension(GetLastToken(fileName)) + '.o';
			outputFile := kObjectFolderName + '/' + TrimExtension(GetLastToken(fileName)) + '.o';
			gPrerunnerData.fileList[gPrerunnerData.listIndex] := outputFile;
//			TrimExtension(fileName) + '.o';
//			gPrerunnerData.fileList[gPrerunnerData.listIndex] := TrimLastToken(fileName) +  + '/' + kObjectFolderName + '/' + TrimExtension(GetLastToken(fileName)) + '.o';
//folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName;
	//		CompileC (separat ppc+386?, lägg i intermediate);
			WriteLn('Wants to compile ', fileName, ' to ', outputFile, ' for project ', gPrerunnerData.theSpec);
			CompileCforPrerunner(gPrerunnerData.theSpec, fileName, gPrerunnerData.buildFinal, PrerunnerStep);
	
			// DEBUG:
//			PrerunnerStep(aborted, result, privateData);
//			PrerunnerStep(0, fileName);
		end
		else
			CompilePascal(gPrerunnerData.theSpec, gPrerunnerData.buildApp, gPrerunnerData.buildFinal, gPrerunnerData.compileCallback);

// Kolla BuildNextFile!
//	end;
//	else
	// add .o's of list!
//		CompilePascal(gPrerunnerData.theSpec, gPrerunnerData.buildApp, gPrerunnerData.buildFinal, gPrerunnerData.compileCallback);
end;

// New entry point for compiling with a list of "extra"
procedure PascalPreRunner(theSpec: FSSpecString; buildApp, buildFinal: Boolean; compileCallback: CompilationDoneProc);
var
	folderSpec, pathOnly: FSSpecString;
begin
//	pathOnly := TrimLastToken(theSpec);
//WriteLn('Try chdir to: "', theSpec, '"');
//	chdir(pathOnly); // Set that as current dir
//WriteLn('chdir OK');

	WriteLn('Enter PreRunner');
// save arguments
	gPrerunnerData.theSpec := theSpec;
	gPrerunnerData.buildApp := buildApp;
	gPrerunnerData.buildFinal := buildFinal;
	gPrerunnerData.compileCallback := compileCallback;
// Build list
// Viktig fråga: Från vad? Från alla .c/.cpp/.m i nån lista av extra filer?
// Och dessa ändras till .o vid kompilering?
	gPrerunnerData.fileList := gSettings.fileListArray;
	
	gPrerunnerData.listIndex := -1;
	gPrerunnerData.linkWithCPP := false;

// Call PreRunnerStep
// In PreRunnerStep, call CompileC for any C file, change name to .o!
//	PrerunnerStep(false, 0, @gPrerunnerData);

// Create intermediate-files!
	folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName;
	CreateFolder(folderSpec); // FpMkdir(folderSpec, $777);

	PrerunnerStep(0, '');
end;

procedure CompilePascal(theSpec: FSSpecString; buildApp, buildFinal: Boolean; compileCallback: CompilationDoneProc);
	var
		commandLine: AnsiString;
//		err: OSErr;
		ok: Boolean;
		s: AnsiString;
begin
WriteLn('COMPILEPASCAL');
//if (gFlags^^.target = kUniversalTarget) then
//MessageAlert('Target seems to be Universal', 'when compiling');
		
		if editWindIsCocoaProgram[gMainFrontIndex] then
		begin
			// Check if target installed
			ok := true;
			if gVersionObjP386 = kProcessUtilsFailedString then
				if gSettings.target in [kUniversalTarget, k386Target] then
					ok := false;
			if gVersionObjPPPC = kProcessUtilsFailedString then
				if gSettings.target in [kUniversalTarget, kPPCTarget] then
					ok := false;
		end
		else
		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
			// Check if target installed
			ok := true;
			
			if buildFinal then
				ok := gVersioniOS <> kProcessUtilsFailedString
			else
				ok := gVersionSimulator <> kProcessUtilsFailedString;
		end
		else
		begin
			// Check if target installed
			ok := true;
			if gVersion386 = kProcessUtilsFailedString then
				if gSettings.target in [kUniversalTarget, k386Target] then
					ok := false;
			if gVersionPPC = kProcessUtilsFailedString then
				if gSettings.target in [kUniversalTarget, kPPCTarget] then
					ok := false;
		end;
		
		if gMainFrontIndex > 0 then
			if not editMainWindIsProgram[gMainFrontIndex] then
			begin
				// Library! Builds shared library!
				if gSettings.target = kUniversalTarget then
				begin
					HelpAppendLn('Can not (yet) build universal shared libraries. Sorry.');
					Exit(CompilePascal);
				end;
				commandLine := BuildPascalCommandline(theSpec, true, buildFinal, gSettings.target, false);
				LaunchCompilation(commandLine, theSpec, true, buildFinal, compileCallback, @CompilePascalData, @CompilationDone);
				SelectWindow(helpWind);
				Exit(CompilePascal);
			end;
			
		
		if not ok then
		if not warningDone then
		begin
			
			// Check if "which" can find the compiler if it is missing!
			// Only checks FPS PPC and 386, not iOS
			if gSettings.target in [kUniversalTarget, k386Target] then
			if gVersion386 = kProcessUtilsFailedString then
			begin
				s := ProcessToString('which ppc386');
				if Length(s) > 1 then
				begin
					while s[Length(s)] in [#10, #13] do s := Copy(s, 1, Length(s)-1);
					if QuestionAlert('FPC for 386 seems to be located at '+s, 'Correct settings?') then
					begin
						gSettings.x86Compiler := s; // Should trigger save settings!
						SetSettingsDialog2ItemString('x86c', 0, s);
						CheckFPCVersion;
					end;
				end;
			end;
			if gSettings.target in [kUniversalTarget, kPPCTarget] then
			if gVersionPPC = kProcessUtilsFailedString then
			begin
				s := ProcessToString('which ppcppc');
				if Length(s) > 1 then
				begin
					while s[Length(s)] in [#10, #13] do s := Copy(s, 1, Length(s)-1);
					if QuestionAlert('FPC for PPC seems to be located at '+s, 'Correct settings?') then
					begin
						gSettings.PPCCompiler := s; // Should trigger save settings!
						SetSettingsDialog2ItemString('PPCc', 0, s);
						CheckFPCVersion;
					end;
				end;
			end;

		
			if not QuestionAlert('FPC does not seem to be installed for the selected target', 'Try compiling anyway?') then
			begin
//				CompilePascal := -1; // Error
				Exit(CompilePascal);
			end
			else
				warningDone := true;
		end;


		if gCurrentProcess <> nil then
		begin
			if ProcessRunning(gCurrentProcess) then
			begin
//				DebugStr('Another process is already running');
//				Exit(CompilePascal);
				if YesNoQuestionAlert('Another process is already running. Quit it?', 'Data in the process may be lost.') then
					ProcessDispose(gCurrentProcess)
				else
					Exit(CompilePascal);
			end
			else
			begin
// Normal
				ProcessDispose(gCurrentProcess);
			end;
		end;
		
		// Scan main file for $LinkLib to find linked libraries and object files
		gFrameworkList := AnalyzeLinklib(theSpec);
		
		HelpMainMessage('Compiling ' + GetLastToken(theSpec)); // Status
//WriteLn('COMPILING');
		
		if (gSettings.target = kUniversalTarget) and not editWindIsiOSProgram[gMainFrontIndex] then
		begin
//MessageAlert('Target seems to be Universal', 'when compiling');

			HelpAppendLn('Doing PPC side...');
//WriteLn('UNIVERSAL');
	
			commandLine := BuildPascalCommandline(theSpec, buildApp, buildFinal, kPPCTarget, true);
			LaunchCompilation(commandLine, theSpec, buildApp, buildFinal, compileCallback, @CompilePascalData, @CompilationPPCDone);
			SelectWindow(helpWind);
		end
		else
		begin
//WriteLn('SINGLE TARGET');
			commandLine := BuildPascalCommandline(theSpec, buildApp, buildFinal, gSettings.target, false);
			LaunchCompilation(commandLine, theSpec, buildApp, buildFinal, compileCallback, @CompilePascalData, @CompilationDone);
			SelectWindow(helpWind);
		end;
		
// Now the compilation rolls until it terminates and CompilationDone is called!
	end; {CompilePascal}

// This is highly desirable!
//	procedure CompilePascalSmart;
//	begin
		// 1) Parse main program file for inclusion of C/C++/ObjC object code.
		// Check dates of source of these libraries against .o.
		// This assumes than any .o is from ONE file!
		// Recompile C files as needed.
		// 2) Compile Pascal program as usual.
//	end;

	// Check if there are dangerous characters in the path
	// NOT GOOD - most dangerous characters appear in some way!
	function CommandSecurityCheckOBSOLETE(commandLine: AnsiString): OSErr;
	var
		i: Longint;
	begin
		for i := 1 to Length(commandLine) do
		begin
			if commandLine[i] in ['~'] then
				return -1;
			// .. is the most dangerous one! /. is also suspect.
			if i > 1 then
				if commandLine[i] = '.' then
					if commandLine[i-1] in ['.', '/'] then
						return -1;
		end;
		return noErr;
	end;


// BuildBundle needs better error checking!
	function BuildiOSBundle(theSpec: FSSpecString; finalBuild: Boolean): OSErr;
	var
		srcSpec, destSpec, plistSpec: FSSpecString;
		cleanName, {resName,} appName{, nibName}: Str255;
		path, commandLine: AnsiString;
		err: OSErr;
		n: Longint;
		theStringArr: StringArr;
//		appDirId: Longint;
		destRefNum: CharFile;
		tempStr255: Str255;
//		trimpath: AnsiString;
		hasNib: Boolean;
		nibname: AnsiString;
		info: stat;
	begin
		cleanName := TrimExtension(GetLastToken(theSpec));
		
//		resName := cleanName + '.rsrc';
		appName := cleanName + '.app';
//		nibName := cleanName + '.nib';
		path := theSpec;
		path := TrimExtension(path);
		
		HelpMainMessage('Building bundle ' + cleanName); // Status

		//HelpAppendLn('Clean app name: ' + cleanName);

		// Copy the Resources folder to app bundle if it exists, otherwise just create the bundle folder
		// Check if the source exists
		tempStr255 := '' + cleanName + ' Resources';
//		err := FSMakeFSSpec(srcSpec.vRefNum, srcSpec.parID, tempStr255, destSpec);
		if (fpStat(path+' Resources', info) = noErr) then
//		if err = noErr then // no error if it exists!
		begin
			HelpAppendLn('Resources folder exists = ' + tempStr255);
			// Resources exists!
			// Remove bundle
			commandLine := '/bin/rm -r ' + ' "' + path + '.app"';
			ProcessToString({MacRomanToUTF8}(commandLine)); // Instead of ProcessLaunchSilent
//			HelpAppendLn('rm old bundle = ' + commandLine);
			commandLine := '/bin/cp -Rf "' + path+' Resources"' + ' "' + path + '.app"';
			ProcessToString({MacRomanToUTF8}(commandLine));
			HelpAppendLn('cp resources = ' + commandLine);
		end
		else
		begin
			HelpAppendLn('No resources folder = ' + tempStr255);
//			theSpec.name := appName; // Dumt att peta i theSpec!
//			err := FSpDirCreate(TrimLastToken(theSpec) + appName, 0, appDirId);
//			FpMkdir(TrimLastToken(theSpec) + appName, $777); // Mode???
			CreateFolder(TrimLastToken(theSpec) + '/' + appName);
		end;
		
		// New .app! Fill it with meaningful information!
		HelpAppendLn('Building application bundle "' + cleanName + '"');
			
		destSpec := theSpec;
		srcSpec := theSpec;
			
		// And two mandatory files! If they don't exist in bundle already (that is if they were in the Resources folder)
		// PkgInfo:
//		tempStr255 := ':' + cleanName + '.app:PkgInfo';
//		err := FSMakeFSSpec(srcSpec.vRefNum, srcSpec.parID, tempStr255, destSpec);
		destSpec := TrimLastToken(srcSpec) + '/' + cleanName + '.app/PkgInfo';

// Is there a PkgInfo nearby?
// E.g. for program.pas, it may be given as
// "program PkgInfo", "program.PkgInfo" or just "PkgInfo".
		plistSpec := TrimLastToken(srcSpec) + '/' + cleanName + ' PkgInfo';
		err:=fpStat(plistSpec, info);
		if err <> noErr then
		begin
			plistSpec := TrimLastToken(srcSpec) + '/' + cleanName + '.PkgInfo';
			err:=fpStat(plistSpec, info);
		end;
		if err <> noErr then
		begin
			plistSpec := TrimLastToken(srcSpec) + '/' + 'PkgInfo';
			err:=fpStat(plistSpec, info);
		end;
		if err = noErr then // Copy file!
		begin
			WriteLn('Found existing PkgInfo as "', plistSpec, '", copying to "', destSpec, '"');
			commandLine := '/bin/cp "' + plistSpec + '" "' + destSpec + '"';
			theStringArr := ProcessLaunchToStringArr({MacRomanToUTF8}(commandLine));
			for n := 0 to Length(theStringArr)-1 do
				HelpAppendLn(theStringArr[n]);
		end
		else
		begin
			err:=fpStat(destSpec, info);
			if err <> noErr then // did not exist!
			begin
				WriteStringToFile('APPL????', destSpec);
			end;
		end;
		
// The info.plist should go here!
		destSpec := TrimExtension(srcSpec) + '.app/Info.plist';

// Is there an info.plist nearby?
// E.g. for program.pas, it may be given as
// "program info.plist", "program.info.plist" or just "info.plist".
		plistSpec := TrimLastToken(srcSpec) + '/' + cleanName + ' info.plist';
		err:=fpStat(plistSpec, info);
		if err <> noErr then
		begin
			plistSpec := TrimLastToken(srcSpec) + '/' + cleanName + '.info.plist';
			err:=fpStat(plistSpec, info);
		end;
		if err <> noErr then
		begin
			plistSpec := TrimLastToken(srcSpec) + '/' + 'info.plist';
			err:=fpStat(plistSpec, info);
		end;
		if err = noErr then // Copy file!
		begin
			WriteLn('Found existing info.plist as "', plistSpec, '", copying to "', destSpec, '"');
			commandLine := '/bin/cp "' + plistSpec + '" "' + destSpec + '"';
			theStringArr := ProcessLaunchToStringArr({MacRomanToUTF8}(commandLine));
			for n := 0 to Length(theStringArr)-1 do
				HelpAppendLn(theStringArr[n]);
		end
		else
		begin
			// Create a default info.plist!
			err:=fpStat(destSpec, info);
			if err <> noErr then // did not exist!
			begin
				// Info-plist: now some kind of binary format?!
				// Note: use plutil to convert if needed!
				
				// plutil -convert xml1 some_file.plist
				// To convert an XML .plist file to binary for use:
				// plutil -convert binary1 some_other_file.plist

				assign(destRefNum, destSpec);
				{$I-}
				rewrite(destRefNum);                   { open it without breaking for error }
				{$I+}
	//			destSpec.name := ':' + appName + ':Info.plist';
	//			err := FSpCreate(destSpec, '????', 'TEXT', 0);
	//			err := FSpOpenDF(destSpec, fsWrPerm, destRefNum);
				if IOResult = 0 then
				begin
					AppendLn(destRefNum, '<?xml version="1.0" encoding="UTF-8"?>');
					AppendLn(destRefNum, '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
					AppendLn(destRefNum, '<plist version="1.0">');
					AppendLn(destRefNum, '<dict>');
					AppendLn(destRefNum, '<key>CFBundleDevelopmentRegion</key>');
					AppendLn(destRefNum, '<string>English</string>');
					AppendLn(destRefNum, '<key>CFBundleDisplayName</key>');
					AppendLn(destRefNum, '<string>'+cleanName+'</string>');
					AppendLn(destRefNum, '<key>CFBundleExecutable</key>');
					AppendLn(destRefNum, '<string>'+ cleanName +'</string>');
		// CFBundleIdentifier?
					AppendLn(destRefNum, '<key>CFBundleIdentifier</key>');
					AppendLn(destRefNum, '<string>com.democompany.'+ cleanName +'</string>');
					AppendLn(destRefNum, '<key>CFBundleInfoDictionaryVersion</key>');
					AppendLn(destRefNum, '<string>6.0</string>');
					AppendLn(destRefNum, '<key>CFBundleName</key>');
					AppendLn(destRefNum, '<string>'+ cleanName +'</string>');
					AppendLn(destRefNum, '<key>CFBundlePackageType</key>');
					AppendLn(destRefNum, '<string>APPL</string>');
					AppendLn(destRefNum, '<key>CFBundleSignature</key>');
					AppendLn(destRefNum, '<string>????</string>');

					AppendLn(destRefNum, '<key>CFBundleSupportedPlatforms</key>');
					AppendLn(destRefNum, '<array>');
					AppendLn(destRefNum, '<string>iPhoneSimulator</string>'); // Must be replaced on final build!
					AppendLn(destRefNum, '</array>');
					AppendLn(destRefNum, '<key>CFBundleVersion</key>');
					AppendLn(destRefNum, '<string>0.1</string>');
					AppendLn(destRefNum, '<key>DTPlatformName</key>');
					AppendLn(destRefNum, '<string>iphonesimulator</string>'); // Replace
					AppendLn(destRefNum, '<key>DTSDKName</key>');
					AppendLn(destRefNum, '<string>iphonesimulator'+ giOSSDK +'</string>'); // Replace
					AppendLn(destRefNum, '<key>LSRequiresIPhoneOS</key>');
					AppendLn(destRefNum, '<true/>');


					// New: Icon file support
		//			AppendLn(destRefNum, '<key>CFBundleIconFile</key>');
		//			AppendLn(destRefNum, '<string>'+ cleanName +'.icns</string>');

					// Test if a .nib or .xib exists!
					// main or mainMenu in Resources
					// or NAME.nib in the main folder
					// If no nib - don't say that we have one.
					hasNib := true;
					nibname := 'main';
					path := theSpec;
					path := TrimExtension(path);
					if (fpStat(path+' Resources/mainMenu.nib', info) = noErr)
						or (fpStat(path+' Resources/mainMenu.xib', info) = noErr) then
					begin
						hasNib := true;
						nibname := 'mainMenu';
					end
					else
					if fpStat(path+'.nib', info) <> noErr then
						if fpStat(path+'.xib', info) <> noErr then
							if fpStat(path+' Resources/main.nib', info) <> noErr then
								if fpStat(path+' Resources/main.xib', info) <> noErr then
									hasNib := false;
					if hasNib then
					begin
						AppendLn(destRefNum, '<key>NSMainNibFile</key>');
						AppendLn(destRefNum, '<string>'+nibname+'</string>');
						AppendLn(destRefNum, '<key>NSPrincipalClass</key>');
						AppendLn(destRefNum, '<string>NSApplication</string>');
					end;

						AppendLn(destRefNum, '<key>UIDeviceFamily</key>');
						AppendLn(destRefNum, '<array>');
						// This should depend on target
						AppendLn(destRefNum, '<string>1</string>');
						AppendLn(destRefNum, '<string>2</string>');
						AppendLn(destRefNum, '</array>');
					
					AppendLn(destRefNum, '</dict>');
					AppendLn(destRefNum, '</plist>');
					Close(destRefNum);
				end
				else
				begin
					// FELMEDDELANDE
					HelpAppendLn('Problems writing plist file in bundle!');
				end;
			end;
		end;
		
		// In any case, copy the program file!
//		srcSpec := TrimExtension(theSpec);
//		srcSpec.name := cleanName;
//		destSpec := TrimExtension(srcSpec) + '/' + ;
//		destSpec.name := ':' + appName + ':' + cleanName; // Får man göra så i FSp?
		// Kanske snyggare att be fpc skapa filen på rätt plats från början?
		// Move file with mv!
//		path := FSSpecToPathNameString(theSpec);
//		path := TrimExtension(path);
		path := TrimExtension(theSpec);
		commandLine := '/bin/mv "' + path + '" "' + path+'.app/"';
		theStringArr := ProcessLaunchToStringArr({MacRomanToUTF8}(commandLine));
		for n := 0 to Length(theStringArr)-1 do
			HelpAppendLn(theStringArr[n]);
		
		// Copy .icns file if it exists
		// OUTCOMMENTED FOR NOW BUT SOMETHING LIKE THIS SHOULD BE DONE
//		commandLine := '/bin/cp "' + path+'.icns"' + ' "' + path+'.app/Contents/Resources/"';
//		ProcessToString(MacRomanToUTF8(commandLine)); // Silent, we don't want to hear if it fails.

				
		// Copy the .nib package (which is a folder)
		// Or is it really on iPhone?
		// Since it is a folder, cp will fail! We must remove the old one first.


// THE FOLLOWING IS INCORRECT - no Contents
		// Is there a main.nib?	
//		tempStr255 := ':' + cleanName + '.nib';		// CHECK IF SOURCE EXISTS FIRST
//		err := FSMakeFSSpec(srcSpec.vRefNum, srcSpec.parID, tempStr255, destSpec); // Old check, should be changed
		destSpec := TrimExtension(srcSpec) + '.nib';
		err:=fpStat(destSpec, info);
		if err = noErr then // exists, delete old copy in bundle
		begin
			commandLine := '/bin/rm -r ' + ' "' + path + '.app/Contents/Resources/main.nib"';
			ProcessToString({MacRomanToUTF8}(commandLine));		
		end;
		commandLine := '/bin/cp -Rf "' + path+'.nib"' + ' "' + path + '.app/Contents/Resources/main.nib"';
		ProcessToString({MacRomanToUTF8}(commandLine));
		
		// Added 110401: xib support
		// Is there a main.xib? (Irrelevant?)
//		tempStr255 := ':' + cleanName + '.xib';		// CHECK IF SOURCE EXISTS FIRST
//		err := FSMakeFSSpec(srcSpec.vRefNum, srcSpec.parID, tempStr255, destSpec); // Old check, should be changed
		destSpec := TrimExtension(srcSpec) + '.xib';
		err:=fpStat(destSpec, info);
		if err = noErr then // exists, delete old copy in bundle
		begin
			commandLine := '/bin/rm -r ' + ' "' + path + '.app/Contents/Resources/main.xib"';
			ProcessToString({MacRomanToUTF8}(commandLine));
		end;
		commandLine := '/bin/cp -Rf "' + path+'.xib"' + ' "' + path + '.app/Contents/Resources/main.nib"';
		ProcessToString({MacRomanToUTF8}(commandLine));
		
		// Codesign here! If there is a codesigning string.
		if finalBuild then
		if gSettings.iOSCodesign <> '' then
		begin
			commandLine := '/usr/bin/codesign -f -s "' + gSettings.iOSCodesign + '" "' + path + '.app/' + cleanname + '"';
			WriteLn('CODESIGN: ' + commandLine);
//			ProcessToString(MacRomanToUTF8(commandLine));
			theStringArr := ProcessLaunchToStringArr({MacRomanToUTF8}(commandLine));
			for n := 0 to Length(theStringArr)-1 do
				HelpAppendLn(theStringArr[n]);
		end;
				
// The only error that we would need to report is if the binary could not be moved,
// and that one is checked for otherwise and hard to check here.
		BuildiOSBundle := noErr;
	end; {BuildiOSBundle}




// BuildBundle needs better error checking!
	function BuildBundle(theSpec: FSSpecString; finalBuild: Boolean): OSErr;
	var
		srcSpec, destSpec, plistSpec: FSSpecString;
		cleanName, resName, appName{, nibName}: Str255;
		path, commandLine: AnsiString;
		err: OSErr;
		n, i: Longint;
		theStringArr: StringArr;
//		appDirId: Longint;
		destRefNum: CharFile;
//		tempStr255: Str255;
		trimpath: AnsiString;
		hasNib: Boolean;
		nibname: AnsiString;
		info: stat;
		frameWorkName: AnsiString;
		customInfoPlistExists: Boolean;
	begin
		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
			return BuildiOSBundle(theSpec, finalBuild);
		end;
// TO DO:
// Separate functions for:
// JVM bundle
// Android JAR		
		
		cleanName := TrimExtension(GetLastToken(theSpec));
		
		resName := cleanName + '.rsrc';
		appName := cleanName + '.app';
//		nibName := cleanName + '.nib';
		
		HelpMainMessage('Building bundle ' + cleanName); // Status
		
		//HelpAppendLn('Clean app name: ' + cleanName);
		
		err:=fpStat(TrimLastToken(theSpec) + '/' + appName, info);
		WriteLn('fpStat on ' + TrimLastToken(theSpec) + '/' + appName + ' = ', err);
		if err = 0 then // = exists
			HelpAppendLn('Updating application bundle "' + cleanName + '"') // Bundle exists
		else
		//if err = noErr then
		begin
			//FpMkdir(TrimLastToken(theSpec) + appName, $777); // Mode???
			CreateFolder(TrimLastToken(theSpec) + '/' + appName);
			// New .app! Fill it with meaningful information!
			HelpAppendLn('Building new application bundle "' + cleanName + '"');
			
			destSpec := theSpec;
			
			err := CreateFolder(TrimLastToken(theSpec) + '/' + appName + '/Contents');
			if err <> noErr then
				HelpAppendLn('CreateFolder Contents failed (Contents)');
			err := CreateFolder(TrimLastToken(theSpec) + '/' + appName + '/Contents/MacOS');
			if err <> noErr then
				HelpAppendLn('CreateFolder Contents/MacOS failed (MacOS)');
			err := CreateFolder(TrimLastToken(theSpec) + '/' + appName + '/Contents/Resources');
			if err <> noErr then
				HelpAppendLn('CreateFolder Resources failed (MacOS)');
			// 110322: Frameworks folder
			err := CreateFolder(TrimLastToken(theSpec) + '/' + appName + '/Contents/Frameworks');
			if err <> noErr then
				HelpAppendLn('CreateFolder Frameworks failed (MacOS)');
			
			// And two mandatory files!
			// PkgInfo:
			WriteStringToFile('APPL????', TrimLastToken(theSpec) + '/' + appName + '/Contents/PkgInfo');
// WriteStringToFile behöver felhantering!
// An unhandled exception occurred at $00060077 : EInOutError : File not found
			
			// Info-plist:
			// Note: This auto-plist may be overwritten below if a custom one is provided!
			destSpec := TrimLastToken(theSpec) + '/' + appName + '/Contents/Info.plist';
			err := OpenFile(destSpec, true, destRefNum);
			if err = noErr then
			begin
				AppendLn(destRefNum, '<?xml version="1.0" encoding="UTF-8"?>');
				AppendLn(destRefNum, '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
				AppendLn(destRefNum, '<plist version="1.0">');
				AppendLn(destRefNum, '<dict>');
				AppendLn(destRefNum, '<key>CFBundleDevelopmentRegion</key>');
				AppendLn(destRefNum, '<string>English</string>');
				AppendLn(destRefNum, '<key>CFBundleExecutable</key>');
				AppendLn(destRefNum, '<string>'+ cleanName +'</string>');
				// New: Icon file support
				AppendLn(destRefNum, '<key>CFBundleIconFile</key>');
				AppendLn(destRefNum, '<string>'+ cleanName +'.icns</string>');

				// New: File types dummy (text)
				AppendLn(destRefNum, '<key>CFBundleDocumentTypes</key>');
				AppendLn(destRefNum, '<array>');
				
				AppendLn(destRefNum, '<dict>');
				AppendLn(destRefNum, '<key>CFBundleTypeExtensions</key>');
				AppendLn(destRefNum, '<array>');
				AppendLn(destRefNum, '<string>txt</string>');
				AppendLn(destRefNum, '<string>*</string>');
				AppendLn(destRefNum, '</array>');

				// New: Bundle type role dummy (text), to avoid error messages in 10.6
	//			AppendLn(destRefNum, '<key>CFBundleTypeRole</key>');
	//			AppendLn(destRefNum, '<string>None</string>');
	// Wrongie placie dearie!
				AppendLn(destRefNum, '<key>CFBundleTypeRole</key>');
				AppendLn(destRefNum, '<string>None</string>');

				AppendLn(destRefNum, '<key>CFBundleTypeMIMETypes</key>');
				AppendLn(destRefNum, '<array>');
				AppendLn(destRefNum, '<string>text/plain</string>');
				AppendLn(destRefNum, '</array>');

				// New: Bundle type role dummy (text), to avoid error messages in 10.6
	//			AppendLn(destRefNum, '<key>CFBundleTypeRole</key>');
	//			AppendLn(destRefNum, '<string>None</string>');

				AppendLn(destRefNum, '<key>CFBundleTypeOSTypes</key>');
				AppendLn(destRefNum, '<array>');
				AppendLn(destRefNum, '<string>****</string>');
				AppendLn(destRefNum, '</array>');

				// New: Bundle type role dummy (text), to avoid error messages in 10.6
	//			AppendLn(destRefNum, '<key>CFBundleTypeRole</key>');
	//			AppendLn(destRefNum, '<string>None</string>');

				AppendLn(destRefNum, '</dict>');
				AppendLn(destRefNum, '</array>');
				//
				AppendLn(destRefNum, '<key>CFBundleIdentifier</key>');
				AppendLn(destRefNum, '<string>'+ cleanName +'</string>'); // Changed in 0.8.3
	//			AppendLn(destRefNum, '<string>Built by Lightweight IDE</string>'); // because this caused problems sometimes.
				AppendLn(destRefNum, '<key>CFBundleInfoDictionaryVersion</key>');
				AppendLn(destRefNum, '<string>6.0</string>');
				
	// Sometimes needed (added in 0.8.3)
				AppendLn(destRefNum, '<key>CFBundleName</key>');
				AppendLn(destRefNum, '<string>'+ cleanName +'</string>');
				
				AppendLn(destRefNum, '<key>CFBundlePackageType</key>');
				AppendLn(destRefNum, '<string>APPL</string>');
				AppendLn(destRefNum, '<key>CFBundleSignature</key>');
				AppendLn(destRefNum, '<string>????</string>');
				AppendLn(destRefNum, '<key>CFBundleVersion</key>');
				AppendLn(destRefNum, '<string>0.1</string>');
				AppendLn(destRefNum, '<key>CSResourcesFileMapped</key>');
				AppendLn(destRefNum, '<true/>');
				AppendLn(destRefNum, '<key>NSHighResolutionCapable</key>');
				AppendLn(destRefNum, '<true/>');

	// Only needed for Cocoa:
				// Test if a .nib or .xib exists!
				// main or mainMenu in Resources
				// or NAME.nib in the main folder
				// If no nib - don't say that we have one.
				hasNib := true;
				nibname := 'main';
				path := theSpec;
				path := TrimExtension(path);
				if (fpStat(path+' Resources/mainMenu.nib', info) = noErr)
					or (fpStat(path+' Resources/mainMenu.xib', info) = noErr) then
				begin
					hasNib := true;
					nibname := 'mainMenu';
				end
				else
				if fpStat(path+'.nib', info) <> noErr then
					if fpStat(path+'.xib', info) <> noErr then
						if fpStat(path+' Resources/main.nib', info) <> noErr then
							if fpStat(path+' Resources/main.xib', info) <> noErr then
								hasNib := false;
				if hasNib then
				begin
					AppendLn(destRefNum, '<key>NSMainNibFile</key>');
					AppendLn(destRefNum, '<string>'+nibname+'</string>');
					AppendLn(destRefNum, '<key>NSPrincipalClass</key>');
					AppendLn(destRefNum, '<string>NSApplication</string>');
				end;
				
				AppendLn(destRefNum, '</dict>');
				AppendLn(destRefNum, '</plist>');
				Close(destRefNum);
			end
			else
				HelpAppendLn('Problems writing info.plist (' + destSpec + ')');
		end;
		
		// In any case, copy the program file!
		// Move file with mv!
		path := theSpec;
		path := TrimExtension(path);
		commandLine := '/bin/mv "' + path + '" "' + path+'.app/Contents/MacOS/"';
		theStringArr := ProcessLaunchToStringArr({MacRomanToUTF8}(commandLine));
		for n := 0 to Length(theStringArr)-1 do
			HelpAppendLn(theStringArr[n]);
		
		// Copy .icns file if it exists
		// LOCKUP?
		if FileExists(path+'.icns') then
		begin
			commandLine := '/bin/cp "' + path+'.icns"' + ' "' + path+'.app/Contents/Resources/"';
			ProcessToString({MacRomanToUTF8}(commandLine)); // Silent, we don't want to hear if it fails.
		end;

		// Copy the Resources folder if it exists
		// Check if the source exists
		if FileExists(path+' Resources') then
		begin
//			HelpAppendLn(tempStr255 + ' exists');
		// If it exists, delete the destination.
			commandLine := '/bin/rm -r ' + ' "' + path + '.app/Contents/Resources"';
			ProcessToString({MacRomanToUTF8}(commandLine));
		// Copy
			commandLine := '/bin/cp -Rf "' + path+' Resources"' + ' "' + path + '.app/Contents/Resources"';
			ProcessToString({MacRomanToUTF8}(commandLine));		
		end;

		// Copy frameworks folder to the frameworks folder 131106
		if FileExists(path + ' frameworks') then
		begin
		// rm frameworks?
			commandLine := '/bin/rm -r ' + ' "' + path + '.app/Contents/Frameworks"';
			ProcessToString(commandLine);
			commandLine := '/bin/cp -Rf "' + path+' frameworks"' + ' "' + path + '.app/Contents/Frameworks"';
			ProcessToString(commandLine);
		end;

		// Copy info.plist file to the bundle 131112
		// 140819: Now allows program.info.plist as well as just info.plist, plus the old "program info.plist".
		plistSpec := path + ' Info.plist';
		WriteLn('Testing ', plistSpec);
		customInfoPlistExists := FileExists(plistSpec);
		if not customInfoPlistExists then
		begin
			plistSpec := path + '.Info.plist';
			WriteLn('Testing ', plistSpec);
			customInfoPlistExists := FileExists(plistSpec);
		end;
		if not customInfoPlistExists then
		begin
			plistSpec := TrimLastToken(theSpec) + '/Info.plist';
			WriteLn('Testing ', plistSpec);
			customInfoPlistExists := FileExists(plistSpec);
		end;
		if customInfoPlistExists then
		begin
			WriteLn('Found ', plistSpec);
		// rm old file?
			commandLine := '/bin/rm ' + ' "' + path + '.app/Contents/Info.plist"';
			WriteLn(commandLine);
			WriteLn(ProcessToString(commandLine));
			commandLine := '/bin/cp "' + plistSpec + '" "' + path + '.app/Contents/Info.plist"';
			WriteLn(commandLine);
			WriteLn(ProcessToString(commandLine));
		end;
		
		// Should also copy PkgInfo
		plistSpec := path + ' PkgInfo';
		WriteLn('Testing ', plistSpec);
		customInfoPlistExists := FileExists(plistSpec);
		if not customInfoPlistExists then
		begin
			plistSpec := path + '.PkgInfo';
			WriteLn('Testing ', plistSpec);
			customInfoPlistExists := FileExists(plistSpec);
		end;
		if not customInfoPlistExists then
		begin
			plistSpec := TrimLastToken(theSpec) + '/PkgInfo';
			WriteLn('Testing ', plistSpec);
			customInfoPlistExists := FileExists(plistSpec);
		end;
		if customInfoPlistExists then
		begin
			WriteLn('Found ', plistSpec);
		// rm old file?
			commandLine := '/bin/rm ' + ' "' + path + '.app/Contents/PkgInfo"';
			WriteLn(commandLine);
			WriteLn(ProcessToString(commandLine));
			commandLine := '/bin/cp "' + plistSpec + '" "' + path + '.app/Contents/PkgInfo"';
			WriteLn(commandLine);
			WriteLn(ProcessToString(commandLine));
		end;

// *** Copy resource file ***
// (Long section that should be put in a separate procedure)
		// Copy resource file if it exists! Should be done every time - it might have changed!!
		// (Or check if it has changed lately)
		srcSpec := TrimLastToken(theSpec) + '/' + resName;
		destSpec := TrimLastToken(theSpec) + '/Contents/Resources/' + resName;
		CopyResourceFileToDataFile(srcSpec, destSpec); // Not working? FSSpec-relics!
// *** END of resource file copy ***
		
		// Copy the .nib package (which is a folder)
		// Since it is a folder, cp will fail! We must remove the old one first.

		// Is there a main.nib?
		err:=fpStat(TrimExtension(srcSpec) + '.nib', info);
		if err = noErr then // exists, delete old copy in bundle
		begin
			commandLine := '/bin/rm -r ' + ' "' + path + '.app/Contents/Resources/main.nib"';
			ProcessToString({MacRomanToUTF8}(commandLine));		
		end;
		commandLine := '/bin/cp -Rf "' + path+'.nib"' + ' "' + path + '.app/Contents/Resources/main.nib"';
		ProcessToString({MacRomanToUTF8}(commandLine));
		
		// Added 110401: xib support
		// Is there a main.xib?
		err:=fpStat(TrimExtension(srcSpec) + '.xib', info);
		if err = noErr then // exists, delete old copy in bundle
		begin
			commandLine := '/bin/rm -r ' + ' "' + path + '.app/Contents/Resources/main.xib"';
			ProcessToString({MacRomanToUTF8}(commandLine));
		end;
		commandLine := '/bin/cp -Rf "' + path+'.xib"' + ' "' + path + '.app/Contents/Resources/main.nib"';
		ProcessToString({MacRomanToUTF8}(commandLine));
		
		// Add frameworks
		// This includes changing the reference to the library according to
		// http://doc.qt.nokia.com/qq/qq09-mac-deployment.html#sharedlibraries
		for i := Low(gFrameworkList) to High(gFrameworkList) do
		if gFrameworkList[i] <> '' then
		begin

// Problem med dylib support: frameWorksList är aningen enbart filnamn eller hela sökvägen!
// Kan jag på något sätt garantera att det är sökväg?

//			commandLine := '/usr/bin/install_name_tool -D ' + frameWorksList[i];
			HelpAppendLn('DYLIB SUPPORT');
			HelpAppendLn('I would like to add the library "' + gFrameworkList[i] + '"');
			HelpAppendLn('APPNAME = ' + cleanName);
			HelpAppendLn('Path = ' + path);
			HelpAppendLn('Path without app = ' + TrimLastToken(path));
			frameWorkName := GetLastToken(gFrameworkList[i]);
			HelpAppendLn('Framework name without path = ' + frameWorkName);
			
			// Copy library
			trimpath := TrimLastToken(path);
			commandLine := '/bin/cp "' + {trimpath+'/'+}gFrameworkList[i] +'" "'+ path+'.app/Contents/Frameworks/'+frameWorkName+'"';
			HelpAppendLn('cp command = ' + commandline);
			ProcessToString({MacRomanToUTF8}(commandLine));


			// Get the built-in path
			commandLine := '/usr/bin/otool -D "' + {trimpath+'/'+}gFrameworkList[i]+'"';
			HelpAppendLn('otool command = ' + commandline);
			theStringArr := ProcessLaunchToStringArr({MacRomanToUTF8}(commandLine));
			HelpAppendLn('ID in library = ' + theStringArr[High(theStringArr)]);
			
			// Change the path
			commandLine := '/usr/bin/install_name_tool -change ' +
				'"'+theStringArr[High(theStringArr)]+'"' + ' '+ // Old built-in path
				'"@executable_path/../Frameworks/' + frameWorkName +'" "'+
//				trimpath+'/'+gFrameworkList[i] + ' '+
				path+'.app/Contents/MacOS/'+cleanName+'"';
			HelpAppendLn('change command = ' +  commandLine);
			ProcessToString({MacRomanToUTF8}(commandLine));
			
			// For all other libraries, change any reference to this
//			for j := Low(gFrameworkList) to High(gFrameworkList) do
//				if i <> j then
//				if gFrameworkList[i] <> gFrameworkList[j] then
//				begin
//					commandLine := '/usr/bin/install_name_tool -change ' +
//						'"'+theStringArr[High(theStringArr)]+'"' + ' '+ // Old built-in path
//						'"@executable_path/../Frameworks/' + gFrameworkList[i] +'" "'+
//						trimpath+'/'+gFrameworkList[j] + '"';
//					WriteLn('!!! change lib command = ' +  commandLine);
//					ProcessToString({MacRomanToUTF8}(commandLine));
//					// Funkar inte?
//				end;
// Tydligen inte. Behöver ändra innan?
// T.ex. install_name_tool -id @executable_path/../Frameworks/libavutil.dylib libavutil.dylib
		end;
		
		// If it is a CUDA program
		// add paths to libs in /usr/local/cuda/lib!
		// How about mixed source? For now, we support pure CUDA.
		// Change the path for its dylibs
		GetEditFSSpec(editWind[gMainFrontIndex], srcSpec);
		if GetExtensionType(srcSpec) = kExtTypeCuda then
		begin
(*
DIDN'T WORK
			if finalBuild then // Copy all CUDA dylibs - will this work???
			begin
				commandLine := '/bin/cp /usr/local/cuda/lib/libcuda.dylib "'+ path+'.app/Contents/Frameworks/libcuda.dylib"';
				HelpAppendLn('cp command = ' + commandline);
				ProcessToString({MacRomanToUTF8}(commandLine));
				commandLine := '/bin/cp /usr/local/cuda/lib/libcudart.dylib "'+ path+'.app/Contents/Frameworks/libcudart.dylib"';
				HelpAppendLn('cp command = ' + commandline);
				ProcessToString({MacRomanToUTF8}(commandLine));
				commandLine := '/bin/cp /usr/local/cuda/lib/libtlshook.dylib "'+ path+'.app/Contents/Frameworks/libtlshook.dylib"';
				HelpAppendLn('cp command = ' + commandline);
				ProcessToString({MacRomanToUTF8}(commandLine));
// libcublas.dylib
// libcublasemu.dylib
// libcudartemu.dylib
// libcufft.dylib
// libcufftemu.dylib

				// Now please search in the bundle
				commandLine := '/usr/bin/install_name_tool -add_rpath ' +
					'"@executable_path/../Frameworks" "'+
					path+'.app/Contents/MacOS/'+cleanName+'"';
				HelpAppendLn('add path command = ' +  commandLine);
				ProcessToString({MacRomanToUTF8}(commandLine));
			end
			else
*)
			begin // link to CUDA dylibs
				commandLine := '/usr/bin/install_name_tool -add_rpath ' +
					'"/usr/local/cuda/lib" "'+
					path+'.app/Contents/MacOS/'+cleanName+'"';
				HelpAppendLn('add path command = ' +  commandLine);
				ProcessToString({MacRomanToUTF8}(commandLine));
			end;
		end;
		
// The only error that we would need to report is if the binary could not be moved,
// and that one is checked for otherwise and hard to check here.
		BuildBundle := noErr;
	end; {BuildBundle}

end.
