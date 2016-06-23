// Lightweight IDE, main program
// © Ingemar Ragnemalm 2006-2016

{$mode macpas}
program LightweightIDE;

	uses
		MacOSAll, LWPGlobals, TransSkel4, LWPEdit, Console, UtilsTypes,
		FileUtils, Classes, SysUtils, AboutWindowUnit, ErrorMessageWindow,
		FindReplaceDialog, Settings, ColorCoding, LWPColorGlobals,
		BuildWithPascal, BuildWithC, BuildWithScript, BuildWithJava,
		ProcessUtils, AlertsUtils, IncludeAnalyzer, cprocintf, TXNUtilities,
		Lightbugs, Breakpoints, BuildWithGPC, BuildWithAda, math, BaseUnix,
		RecentFiles, ProjectWindow, GoToLineDialog, ClassBrowser, DirectGDB,
		AutoReopenUnit;
	
	var
		fileMenu: MenuHandle;

{	Handle selection of About item from Apple menu}
	
	procedure DoAbout(item: Integer);
	begin
		case item of
			1: ShowAbout; // ignore := Alert(aboutAlrt, nil);
// Settings menu items:
			3: DoSettings;
		end; {case}
	end;
	
	// Run data callback
	procedure RunData(oneLineOfData: AnsiString; privateData: Pointer);
	var
		i, limit: Longint;
	const
		FF = Char(12); // Form feed = ASCII(12).
		BEL = Char(7); // Bell
	begin
//WriteLn('RunData "', oneLineOfData, '"');
//HelpAppendLn(oneLineOfData); // Ej radvis!

// Search for FF character!
		limit := 1;
		for i := 1 to Length(oneLineOfData) do
		begin
			if oneLineOfData[i] = FF then
			begin
				HelpClear;
				limit := i+1; // Send from here
			end;
			if oneLineOfData[i] = BEL then
				SysBeep(1);
		end;

		if limit = 1 then
			HelpAppend(oneLineOfData) // if no FF
		else
			HelpAppend(Copy(oneLineOfData, limit, Length(oneLineOfData)));
		HelpForceUpdate;
	end;
	
	// Run finished callback
	procedure RunDone(aborted: Boolean; result: Integer; privateData: Pointer);
	var
		cleanNamePtr: StringPtr;
	begin
		cleanNamePtr := StringPtr(privateData);
		if aborted then
			HelpAppendLn('--- ' + cleanNamePtr^ + ' aborted ---')
		else
			if result = 0 then
				HelpAppendLn('--- ' + cleanNamePtr^ + ' finished ---')
			else
				HelpAppendLn('--- ' + cleanNamePtr^ + ' finished with error ---');
		HelpMainMessage(''); // Reset
	end;
	
	procedure Run(theSpec: FSSpecString);
	var
		cleanName: AnsiString;
		path, commandLine, pathOnly: AnsiString;
//		err: OSErr;
		cleanNamePtr: StringPtr;
		extType: Integer;
		psn: ProcessSerialNumber;
		err: OSErr;
		errs, pids: String;
		tries: Longint;
		
//		cudaScriptSpec: FSSpecString;
	begin
// Make sure that no other process is already running
		if gCurrentProcess <> nil then
		begin
			if ProcessRunning(gCurrentProcess) then
			begin
//				DebugStr('Another process is already running');
//				Exit(Run);
				if YesNoQuestionAlert('Another process is already running. Quit it?', 'Data in the process may be lost.') then
					ProcessDispose(gCurrentProcess)
				else
					Exit(Run);
			end
			else
			begin
				// Old process terminated but not disposed. This is normal!
				ProcessDispose(gCurrentProcess);
			end;
		end;
		
		path := theSpec;
		pathOnly := path; // Needed for chdir
		
		// We need the path without filename.

		// if iPhone, cddir into the bundle, otherwise
		// (command line mode or Mac app), chdir to same dir as source/binary,
// NO, iOS runner should not CD into bundle - that gives a false comfort
// since the device does not do that.
//		if editWindIsiOSProgram[gMainFrontIndex] then
//			pathOnly := TrimExtension(pathOnly) + '.app'
//		else
		if IsCommandLineToolMode(theSpec) then
//		if not gFlags^^.commandLineToolMode then
		begin
//			pathOnly := TrimExtension(pathOnly) + '.app';
			pathOnly := TrimLastToken(pathOnly);
//WriteLn('Try chdir to: "', pathOnly, '"');
//HelpAppendLn('Try chdir to: "'+ pathOnly+ '"');
		//WriteLn('chdir OK');
		end
		else
		begin
			pathOnly := TrimLastToken(pathOnly);
		end;
//		chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
		chdir((pathOnly)); // Set that as current dir

		path := theSpec;
		pathOnly := path; // Needed for chdir
		pathOnly := TrimLastToken(pathOnly);
		
		cleanName := GetLastToken(TrimExtension(theSpec));
//		ConvertPathToCommandLineFormat(cleanName); // Allow spaces in name
		
		path := TrimExtension(path);
		
		// CHECK IF BINARY EXISTS! What is the easiest and safest way?
		// fpStat! See about box unit!
		
		// Bad feedback of stdio. Should work line by line - and be threaded properly.
// Test for "auto-command line mode"
//		if gFlags^^.commandLineToolMode then
//		if FileExists(TrimExtension(theSpec) + ' resources') then
		if IsCommandLineToolMode(theSpec) then
		begin
			commandLine := '"' + path + '"';//+cleanName;
			HelpAppendLn('COMMAND LINE TOOL MODE ACTIVE');
		end
		else
			commandLine := '"' + path+'.app/Contents/MacOS/'+cleanName + '"'; // Use " instead of shell-style \ - WORKS!

// Synch gMainFrontIndex by calling GetFrontMainWindow!
// Good enough to know that we use the right one?
		GetFrontMainWindow;

// Java needs a special command-line
// It really needs different ones depending on whether it is an app or applet.
		extType := GetExtensionType(theSpec);
		if extType = kExtTypeJava then
		begin
			if editMainWindIsProgram[gMainFrontIndex] then
				commandLine := gSettings.JavaInterpreter + ' ' + cleanName // Has "main", application
			else
				commandLine := '/usr/bin/appletviewer' + ' ' + cleanName + '.html'; // Applet
				// NOTE: /usr/bin/appletviewer should be configurable!
		end;

// Makefiles simply "make run".
		if extType = kExtTypeMakefile then
			commandLine := '/usr/bin/make run';
		
		// Run CUDA program!
// NOTE! This method seems to be needed for old CUDA versions!
(*
		if extType = kExtTypeCuda then
		begin
			// Create shellscript! This is to set environment variables to make it find shared libraries.
			// NOT the best solution, I should change this.
//			if gSettings..commandLineToolMode then
			if IsCommandLineToolMode(theSpec) then
				cudaScript := 'export DYLD_LIBRARY_PATH=/usr/local/cuda/lib:$DYLD_LIBRARY_PATH' + #10 +
					'./'+cleanName
			else
				cudaScript := 'export DYLD_LIBRARY_PATH=/usr/local/cuda/lib:$DYLD_LIBRARY_PATH' + #10 +
					'./'+cleanName+'.app/Contents/MacOS/'+cleanName;
			// Add command-line args
			if gSettings.TargetArgs <> '' then
				cudaScript := cudaScript + ' ' + gSettings.TargetArgs;
			// Save in temporary-files

			// Create object code subfolder
			folderSpec := theSpec;
			folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
			{err :=} FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
			
			// Execute shellscript and thereby the program
			shFileName := 'temp-exec-'+cleanName+'.sh';
			SaveCommandToFile(cudaScript, kObjectFolderName + '/' + shFileName);
			commandLine := '/bin/sh ' + kObjectFolderName + '/' + shFileName;
//			Exit(Run);
		end
		else
*)

//		if (extType = kExtTypePascal) and editWindIsiOSProgram[gMainFrontIndex] then
		if gMainFrontIndex > 0 then
		if editWindIsiOSProgram[gMainFrontIndex] then
		begin
//			commandLine := '/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone\ Simulator.app/Contents/MacOS/iPhone\ Simulator -SimulateApplication ' + cleanName + '.app/' + cleanName; // command to run "cleanName" in simulator
			commandLine := '"/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone Simulator.app/Contents/MacOS/iPhone Simulator" -SimulateApplication ' + cleanName + '.app/' + cleanName; // command to run "cleanName" in simulator
			commandLine := '"/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone Simulator.app/Contents/MacOS/iPhone Simulator" -SimulateApplication "' + {MacRomanToUTF8}(pathOnly) + '/' + cleanName + '.app/' + cleanName + '"'; // command to run "cleanName" in simulator

			WriteLn('RUN IPHONE SIMULATOR');
		end
		else
		if editWindIsJVMProgram[gMainFrontIndex] then // JVM, hard-coded version for now!
		begin
			commandLine := '/usr/bin/java -Dfile.encoding=utf-8 -cp /usr/local/lib/fpc/3.0.0/units/jvm-java/rtl:. ' + cleanName;
		end
		else // Not CUDA not iPhone simulator; add command-line arguments to target program
		// (CUDA arguments are added above.)
		begin
			// Command-line arguments (0.8.2)
			if gSettings.TargetArgs <> '' then
				commandLine := commandLine + ' ' + gSettings.TargetArgs;
		end;

		HelpMainMessage('Running ' + cleanName); // Show status
		
		HelpAppendLn('Command: '+commandLine);
		HelpAppendLn('--- ' + cleanName + ' starts ---');
		
// now Run!
		cleanNamePtr := StringPtr(NewPtr(Length(cleanName) + 1));
		cleanNamePtr^:= cleanName;
//		err := StartProcess(commandLine, @RunData, @RunDone, Pointer(cleanNamePtr));

		if gCurrentProcess <> nil then
			HelpAppendLn('SERIOUS PROCESS MANAGEMENT ERROR');
		
		gCurrentProcess := ProcessLaunch({MacRomanToUTF8}(commandLine), true);
		if gCurrentProcess <> nil then
		begin
			ProcessSetCallbacks(gCurrentProcess, @RunData, @RunDone, Pointer(cleanNamePtr));
			if gCurrentProcess^.doneCallback = nil then
				HelpAppendLn('NO DONE CALLBACK!');
			ProcessSetLineBuffering(gCurrentProcess, false);
			SelectWindow(theErrorMsgWind);
			SelectWindow(helpWind);
			
			// New feature 080504: bring launched app to front
			if not IsCommandLineToolMode(theSpec) then // Corrected 140821
//			if not gSettings..commandLineToolMode then
			begin
				tries := 0;
				// GetProcessForPID may not work until after some time
				repeat
					err := GetProcessForPID (gCurrentProcess^.pid, psn);
					if err <> noErr then
//						sleep(1);
						usleep(20000); // sleep 20 ms
					tries := tries + 1;
//					if err <> noErr then
//						HelpAppendLn('trying again GetProcessForPID');
				until (err = noErr) or (tries > 40); // give up if it never works
				if err <> noErr then
				begin
					Str(err, errs);
					Str(gCurrentProcess^.pid, pids);
					WriteLn('GetProcessForPID failed! ' + errs + ' ' + pids);
				end;
				if err = noErr then
				begin
					err := SetFrontProcess (psn);
					Str(err, errs);
					if err <> noErr then
						WriteLn('SetFrontProcess failed! ' + errs);
				end;
			end;
		end
		else
		begin
			HelpAppendLn('Launch failed - fork failed');
			HelpMainMessage(''); // Show status
		end;
		
// Callback! RunDone(aborted: Boolean);

//		HelpAppendLn('--- ' + cleanName + ' finished ---');
	end; {Run}
	
	
// Given main source file theSpec, should it be built?
function CheckBuildNeed(theSpec: FSSpecString; checkAge: Boolean): Boolean;
var
	needsBuild: Boolean;
	destSpec: FSSpecString; // For testing that binary exists
	appName: AnsiString;
//	errStr: Str255;
	
	binTime, srcTime: UInt32;	// For testing modification time
//	ext: Str255;
	err: OSErr;
	
	fileArray: FileArr;
	i: Longint;
	info: Stat;
begin
		WriteLn('*** CheckBuildNeed ***');
// Test if binary exists!
		needsBuild := false;
//WriteLn('Checking build need');

		destSpec := theSpec;
//		appName := theSpec.name;
		appName := GetLastToken(TrimExtension(theSpec));
//		cleanName := TrimExtension(theSpec.name);
//		if gSettings..commandLineToolMode then
		if IsCommandLineToolMode(theSpec) then
		begin
			destSpec := TrimExtension(theSpec); // Just trim extension
			//err := FSMakeFSSpec(destSpec.vRefNum,destSpec.parID,destSpec.name, testSpec);
			err:=fpStat(destSpec, info);
		end
		else
		begin
			destSpec := TrimExtension(theSpec) + '.app/Contents/MacOS/' + appName; // Local path into bundle
//			err := FSMakeFSSpec(destSpec.vRefNum,destSpec.parID,destSpec.name, testSpec);
			err:=fpStat(destSpec, info);
			WriteLn('CheckBuildNeed fpStat: ', err);
			//if err in [fnfErr, dirNFErr] then
			if err <> noErr then
			// But it might be an iOS bundle!
			begin
				WriteLn('Did not find any binary, testing iOS');
				destSpec := TrimExtension(theSpec) + '.app/' + appName; // Local path into bundle
//				err := FSMakeFSSpec(destSpec.vRefNum,destSpec.parID,destSpec.name, testSpec);
				err:=fpStat(destSpec, info);
			end;
		end;
	if err <> noErr then
	//if err in [fnfErr, dirNFErr] then
		begin
			WriteLn('Did not find any binary!');
			needsBuild := true; // Binary does not exist
		end
		else
			WriteLn('No build need from binary check');

	if checkAge then
	begin
// Test modification time for binary vs src. (Test all open source files? Yes - MUST do that!)
//		err := GetFileModTime (testSpec, binTime);
//		if err <> noErr then
//		begin
//			WriteLn('GetFileModTime failed for testSpec!');
//			needsBuild := true;
//		end;
		err := GetFileModTime (destSpec, binTime);
		if err <> noErr then
		begin
			WriteLn('GetFileModTime failed for destSpec!');
			needsBuild := true;
		end;
		err := GetFileModTime (theSpec, srcTime);
		if err <> noErr then
		begin
			WriteLn('GetFileModTime failed for theSpec!');
			needsBuild := true;
		end;
		if needsBuild = false then {implies that both exists since both GetFileModTime worked}
		if binTime < srcTime then
		begin
			WriteLn('binTime < srcTime failed!');
			needsBuild := true;
		end;
		
		// Get other source files, check them too vs binTime!
		if not needsBuild then
		begin
			fileArray := GetIncludeFileList(theSpec, true);
			for i := 0 to Length(fileArray)-1 do
			begin
//WriteLn('CheckBuildNeed with ', fileArray[i].name);
				err := GetFileModTime (fileArray[i], srcTime);
				// If unsuccessful, ignore this file.
				if err = noErr then
					if binTime < srcTime then
					begin
						needsBuild := true;
						Leave; // Leave for loop
					end;
			end;
		end
		else
		WriteLn('No global check, since it hit on main file');
		
		if not needsBuild then
		begin
			// TESTA OM RESURSER €R €NDRADE!
			// Flera tester, alla resurser som kan finnas
			// .rsrc
			// .nib (rekursivt test )
			// .icns
			// NAMN Resources (rekursivt test)
			//
			// EJ FIXAT
		end;
	end;
//	WriteLn('CheckBuildNeed resulted ', Ord(needsBuild));

	CheckBuildNeed := needsBuild;
end;

// Given main source file theSpec, is there a binary file to run?
function DoesBinaryExist(theSpec: FSSpecString; checkAge: Boolean): Boolean;
begin
	DoesBinaryExist := CheckBuildNeed(theSpec, false);
end;


{Determine which file that should be the main target for Run/Compile/Compile unit}
function GetActionFile(var theSpec: FSSpecString; buildApp: Boolean): Boolean;
var
	front: WindowPtr;
	err: OSErr;
	extType: Integer;
//	et: Str255;
//	wname: Str255;
begin
WriteLn('GETACTIONFILE');

	front := GetFrontEditWindow;
	if front = nil then
	begin
		EditOpen;
		front := GetFrontEditWindow;
		if front = nil then
			Return false;
	end;
	
	// Auto-save all open windows!
	EditSaveAll;
	HelpClear;
	ClearErrorMessages; {And hide the window}

//WriteLn('SAVED AND CLEARED');
	
// Om front Šr shellscript - kšr det
// Om Java, klar. (Kanske vi kan smarta till Java senare)
// Smartare Java bšr testa pŒ existens och datum fšr NAMN.class
	
	err := GetEditFSSpec(front, theSpec);
	if err = noErr then
	begin
		extType := GetExtensionType(theSpec);
//		et := GetExtension(theSpec.name);
//		HelpAppendLn('Extension ' + et);
//		str(extType, et);
//		HelpAppendLn('Extension type ' + et);

		if extType = kExtTypeScript then
			return true;
		if extType = kExtTypeMakefile then
			return true;
		if extType = kExtTypeJava then
		begin
//			HelpAppendLn('Found Java file');
			return true;
		end;
	end
	else
	if not buildApp then
	begin
//		HelpAppendLn('Can not compile file (GetEditFSSpec error)');
		return false; // Ska inte detta vara true?
	end;

//WriteLn('GOT EXTENSION TYPE ', extType);
	
// Pascal och C - om buildApp, sšk "main program"
	if buildApp then
	begin
		front := GetFrontMainWindow;
		if front = nil then
		begin
			extType := GetExtensionType(theSpec); // theSpec skapas ovan
			
			HelpAppendLn('No main program found in open files. Can not build.');
			if extType in [kExtTypeC, kExtTypeCPP, kExtTypeObjC] then
				HelpAppendLn('C/C+/ObjC main programs must include a "main" function.')
			else
			if extType = kExtTypePascal then
				HelpAppendLn('Pascal main programs must include the "program" statement.');
			return false;
		end;
		err := GetEditFSSpec(front, theSpec);
		if err <> noErr then
		begin
			HelpAppendLn('Unknown error (Build, GetEditFSSpec)');
			return false;
		end;
		
		// Leading - check.
		if GetLastToken(theSpec)[1] = '-' then
		begin
			HelpAppendLn('File name starting with "-" not allowed.');
			return false;
		end;
	end;
	return true;
end; {GetActionFile}
	
// Callbacks for CompilePascal and CompileC!

// Callback for build before run. Do run.
	procedure BuildDoneThenRun(err: OSErr; theSpec: FSSpecString);
	begin
		if err = noErr then
		begin
			HelpMainMessage(''); // Reset
			Run(theSpec);
		end
		else
		begin
			HelpAppendLn('Failed!');
			HelpMainMessage(''); // Reset
		end;
	end;

// Same as above for debugging
	procedure BuildDoneThenDebug(err: OSErr; theSpec: FSSpecString);
	begin
		if err = noErr then
		begin
			HelpMainMessage(''); // Reset
			BugsRunWithDebug(theSpec);
		end
		else
		begin
			HelpAppendLn('Failed!');
			HelpMainMessage(''); // Reset
		end;
	end;

// Callback for build before run. Do run.
(*	procedure BuildDoneThenRunJava(err: OSErr; theSpec: FSSpecString);
	begin
		if err = noErr then
		begin
			HelpMainMessage(''); // Reset
			RunJava(theSpec)
		end
		else
		begin
			HelpAppendLn('Failed!');
			HelpMainMessage(''); // Reset
		end;
	end;*)

// Callback for build only. Done!
	procedure BuildDone(err: OSErr; theSpec: FSSpecString);
	begin
		if err = noErr then
			HelpAppendLn('Done!')
		else
			HelpAppendLn('Failed!');
		HelpMainMessage(''); // Reset
	end;
	
	// Select the proper Pascal mode
	procedure MainCompilePascal(theSpec: FSSpecString; buildApp, notdebug: Boolean; theBuildDoneCallback: CompilationDoneProc);
	begin
		if gSettings.pascalMode = kGPCmode then
			CompileGPC(theSpec, buildApp, notdebug, theBuildDoneCallback)
		else // FPC
//			CompilePascal(theSpec, buildApp, notdebug, theBuildDoneCallback);
			PascalPreRunner(theSpec, buildApp, notdebug, theBuildDoneCallback);
//			PascalPreRunner
	end;
	
	procedure DoRunCommand(debug: Boolean);
	var
		theSpec: FSSpecString;
		extType: Integer;
//		err: OSErr;
		ext: Str255;
		
		theBuildDoneCallback: procedure(err: OSErr; theSpec: FSSpecString);
	begin
// If the debugger is running, pass on to that.
// Note that this is done for both Run-variants.
		if BugsRunning then
		begin
			BugsRun;
			Exit(DoRunCommand);
		end;
		
		if not GetActionFile(theSpec, true) then Exit(DoRunCommand);
		extType := GetExtensionType(theSpec);
		
		// Kolla redan hŠr om det Šr Pascal-library.
		if extType = kExtTypePascal then
		if gMainFrontIndex > 0 then
		if not editMainWindIsProgram[gMainFrontIndex] then // gMainFrontIndex is set by GetFrontMainWindow
		begin
			HelpAppendLn('Front main file is a library, can not run that.');
			Exit(DoRunCommand);
		end;

		if extType = kExtTypeScript then
		begin
			{err :=} ExecuteScript(theSpec);
			Exit(DoRunCommand);
		end;
		if extType = kExtTypeMakefile then
		begin
//			err := RunMakefile(theSpec);
			Run(theSpec);
			Exit(DoRunCommand);
		end;
		
		// Make the debug selection with a proc ptr
		if debug then
			theBuildDoneCallback := BuildDoneThenDebug
		else
			theBuildDoneCallback := BuildDoneThenRun;
		
		if extType = kExtTypeJava then
		begin
			CompileJava(theSpec, true, not debug, BuildDoneThenRun); // should be theBuildDoneCallback in the future
			Exit(DoRunCommand);
//			RunJava(theSpec);
		end;

// Change below 081225: Build due to run command should always be debug style, not final.
		if CheckBuildNeed(theSpec, true) then
		begin
			case extType of
				kExtTypePascal: MainCompilePascal(theSpec, true, false{not debug}, theBuildDoneCallback);
				kExtTypeC, kExtTypeCPP, kExtTypeObjC: CompileCSmart(theSpec, true, false{not debug}, theBuildDoneCallback);
				kExtTypeCuda: CompileCuda(theSpec, true, false{not debug}, theBuildDoneCallback);
				kExtTypeAda: CompileAda(theSpec, true, false{not debug}, theBuildDoneCallback);
				//kExtTypeJava: CompileJava(theSpec, true, BuildDoneThenRun);
				//kExtTypeScript: ExecuteShellScript(theSpec) // Already covered above
			otherwise
				begin
					ext := GetExtension(theSpec);
					HelpAppendLn('Unknown file type, can not build ' + ext + ' files.');
//					Build := -1;
				end;
			end; {case}
		end
		else
		begin
			if debug then
				BugsRunWithDebug(theSpec)
			else
				Run(theSpec);
		end;
	end;
	
	procedure DoBuildCommand(buildApp: Boolean; buildFinal: Boolean);
	var
		theSpec: FSSpecString;
		extType: Integer;
		ext: Str255;
	begin
//WriteLn('DOBUILDCOMMAND');
		if not GetActionFile(theSpec, buildApp) then Exit(DoBuildCommand);
		
//		if FSSpecPathNameSecurityCheck(theSpec) <> noErr then
//		begin
//			MessageAlert('Invalid path', 'Some characters (/ .. ~ ") are not allowed in the path');
//			Exit(DoBuildCommand);
//		end;

//WriteLn('GETACTIONFILE DONE ', theSpec.name);

		extType := GetExtensionType(theSpec);
//WriteLn('GETEXTENSIONTYPE DONE');
		
//		case extType of
//			kExtTypePascal: WriteLn('Pascal');
//			kExtTypeC, kExtTypeCPP, kExtTypeObjC: WriteLn('C');
//			kExtTypeScript: WriteLn('sh'); // Hellre "run"!;
//			kExtTypeMakefile: WriteLn('make');
//			kExtTypeJava: WriteLn('Java');
//			kExtTypeAda: WriteLn('Ada');
//			kExtTypeCuda: Writeln('CUDA');
//		otherwise
//				WriteLn('WTF');
//		end; {case}
		
		case extType of
			kExtTypePascal: MainCompilePascal(theSpec, buildApp, buildFinal, BuildDone);
			kExtTypeC, kExtTypeCPP, kExtTypeObjC:
				if buildFinal or not buildApp then // Better single-file compilation
					CompileC(theSpec, buildApp, buildFinal, BuildDone)
				else
					CompileCSmart(theSpec, buildApp, buildFinal, BuildDone);
			kExtTypeScript: ExecuteScript(theSpec); // Hellre "run"!;
			kExtTypeMakefile: BuildMakefile(theSpec, buildApp, buildFinal); // Flags trigger different make targets.
			kExtTypeJava: CompileJava(theSpec, buildApp, buildFinal, BuildDone);
			kExtTypeAda: CompileAda(theSpec, buildApp, buildFinal, BuildDone);
			kExtTypeCuda: CompileCuda(theSpec, buildApp, buildFinal, BuildDone);
		otherwise
			begin
				ext := GetExtension(theSpec);
				HelpAppendLn('Unknown file type, can not build ' + ext + ' files.');
//				Build := -1;
			end;
		end; {case}
	end;
	
			procedure DoAbort;
			begin
				if AbortFind then
				begin
					WriteLn('Aborted search!');
					Exit(DoAbort);
				end;
				
				if gCurrentProcess <> nil then
				begin
					if ProcessRunning(gCurrentProcess) then
					begin
					// Testa om gCurrentProcess^.commandLine bšrjar med simulator!
						ProcessTerminate(gCurrentProcess);
						HelpAppendLn('Aborted running process');
						HelpMainMessage(''); // Reset
					end
					else
					begin
						if Length(gCurrentProcess^.inBuffer) = 0 then
						begin
							if gCurrentProcess^.doneCallback = nil then
								HelpAppendLn('Dead process has no doneCallback!');
							ProcessDispose(gCurrentProcess);
							HelpAppendLn('Disposed dead process');
							HelpMainMessage(''); // Reset
						end
						else
						begin
							// Flush input buffer on double abort
							SetLength(gCurrentProcess^.inBuffer, 0);
							HelpAppendLn('Flushed buffer of dead process');
						end;
					end;

{ProcessDispose? No, if we don't dispose, then the caller can catch the result.}
{So let the caller dispose. Or maybe we can dispose if it is already terminated?}
				end
				else
				begin
					HelpAppendLn('Nothing to abort');
					HelpMainMessage(''); // Reset
				end;
			end;

	
	procedure DoRunMenuOBSOLETE (item: integer);
	const
		kCompile = 1;
		kRun = 2;
		kGogo = 3; // Run in debugger
		kBuildUnit = 5;
		kBuildRelease = 6; // Debug off
		kAbort = 8;
//	var
//		front: WindowPtr;
//		err: OSErr;
	begin
WriteLn('DORUNMENU');
		case item of
			kCompile:
				DoBuildCommand(true, false); // Build(GetFrontEditWindow, true);
			kRun:
				DoRunCommand(false); // Run(GetFrontEditWindow);
			kBuildUnit:
				DoBuildCommand(false, false); // Build(GetFrontEditWindow, false);
			kGogo:
				DoRunCommand(true);
				//MessageAlert('Not yet implemented', ''); // Run in debugger!
			
// Gamla go-go:
// Obsolete - smarter "run" will do
//			begin
		//		front := GetFrontEditWindow;
		//		err := Build(front, true);
				// Should only continue on successful build!
		//		if err = noErr then
		//			Run(front); {Doesn't work well. Why?!}
//			end;
			kBuildRelease:
			begin
				DoBuildCommand(true, true); // Build app, with final options
//				MessageAlert('Not yet implemented', 'Planned when debugger support is added');
			end;
			
			kAbort: // Should be disabled when no process is running!
				DoAbort;
			otherwise
		end;
		
	end;

	procedure DoWindowMenu (item: integer);
	const
		kMessage = 1;
		kErrors = 2;
	//	kFirstwindow = 5; // Ersatt av gWindowMenuOriginalItemCount
	begin
		case item of
			kMessage: // Console
			begin
				ShowWindow(helpWind);
				SelectWindow(helpWind);
			end;
			kErrors:
			begin
				ShowWindow(theErrorMsgWind);
				SelectWindow(theErrorMsgWind);
			end;
			// These are a bit annoying; Why don't they work just by the commands?
			// "Bring all to front" does.
			3:	BugsShowWindow;
			4:	BugsShowObserveWindow;
			5:	BugsShowGDBWindow;
			6:	CBShowWindow;
			7: ShowProjectWindow;
			9: RotateWindows;
			otherwise
			begin
				// Find corresponding editWind
				EditSelectWindowNumber(item - gWindowMenuOriginalItemCount); // 5 mapped to 1!
			end;
		end;
	end;

{	Initialize menus.  Tell Skel to process the Apple menu automatically,}
{	and associate the proper procedures with the File and Edit menus.}

	procedure SetUpMenus;
	begin
		SkelApple('', @DoAbout);
		fileMenu := GetMenuRef(202);
		InitRecentMenu(fileMenu);
		editMenu := GetMenuRef(203);
		dummy := SkelMenu(fileMenu, @EditWindFileMenu {DoFile}, nil, false);
		dummy := SkelMenu(editMenu, @EditWindEditMenu, nil, false);
//		dummy := SkelMenu(runMenu, @DoRunMenu, nil, false);

// Temporary additions to edit menu. Should be in the menu resource.
//		AppendMenu(editMenu, 'Indent out/8');
//		AppendMenu(editMenu, 'Indent in/9');
//		SetMenuItemModifiers(editMenu, 11, kMenuShiftModifier);
//		SetMenuItemModifiers(editMenu, 12, kMenuShiftModifier);
		
//	kMenuNoModifiers = 0;    { Mask for no modifiers}
//	kMenuShiftModifier = 1 shl 0; { Mask for shift key modifier}
//	kMenuOptionModifier = 1 shl 1; { Mask for option key modifier}
//	kMenuControlModifier = 1 shl 2; { Mask for control key modifier}
//	kMenuNoCommandModifier = 1 shl 3; { Mask for no command key modifier}

		searchMenu := GetMenuRef(205);
		runMenu := GetMenuRef(204);
		debugMenu := GetMenuRef(206);
		windowMenu := GetMenuRef(207);
		gWindowMenuOriginalItemCount := CountMenuItems(windowMenu);
		dummy := SkelMenu(searchMenu, @DoSearchMenu, nil, false);
//		dummy := SkelMenu(debugMenu, @DoDebugMenu, nil, false);
		dummy := SkelMenu(windowMenu, @DoWindowMenu, nil, false);
		
		// debug and run work through commands!
	end;
	
	// Set target according to available FPC compilers
	procedure AdjustPrefs;
	var
		major, minor, bugfix: Integer;
	begin
		// No FPC - go for universal
		if gVersion386 = kProcessUtilsFailedString then
			if gVersionPPC = kProcessUtilsFailedString then
			begin
				gSettings.target := kUniversalTarget;
				GetSystemVersion(major, minor, bugfix);
				if minor > 6 then
					gSettings.target := k386Target;
			end;
		
		if gVersion386 = kProcessUtilsFailedString then
			if gVersionPPC <> kProcessUtilsFailedString then
				gSettings.target := kPPCTarget;

		if gVersion386 <> kProcessUtilsFailedString then
			if gVersionPPC = kProcessUtilsFailedString then
				gSettings.target := k386Target;
		
		if gVersion386 <> kProcessUtilsFailedString then
			if gVersionPPC <> kProcessUtilsFailedString then
			begin
				gSettings.target := kUniversalTarget;
				GetSystemVersion(major, minor, bugfix);
				if minor > 6 then
					gSettings.target := k386Target;
			end;
		
		// Warning if no GCC?
	end;

// This background proc is avoided. It should not run all the time! Instead,
// ProcessUtils lets each process run its own background process timer.
//procedure LWPBackground;
//begin
//	if gCurrentProcess.active then
//		HelpAppendLn('Polling');

//	ProcessPoll(gCurrentProcess);
//end;

function DoCommand (myCommand: Longint): Boolean;
begin
	DoCommand := true; // Event handled
	case myCommand of
//		kHICommandAbout: // Not needed, TransSkel handles that
		kHICommandPreferences: DoSettings;
		Longint('Prf2'): DoAdvancedSettings;
		Longint('Xbtn'): DoSettingsAddFile(gSettings.fileList, 'Xfil'); // Extra files
		Longint('Pbtn'): DoSettingsAddFile(gSettings.customInfoPlist, 'CIPL'); // Custom info.plist
		Longint('Xrbt'): DoSettingsAddFile(gSettings.customResources, 'Xres'); // Custom resource locations
		Longint('DEFp'): DefaultPascalCompilerSettings;
//		Longint('251p'): ExperimentalPascalCompilerSettings;
		Longint('DEFc'): DefaultCCompilerSettings;
		
		// Not used? DoSearchMenu? Used by dialog buttons!
		Longint('Find'): DoSearchFindAgain(false); // SearchFindAgain; // DoSearchMenu(kFind);
		Longint('FndB'): DoSearchFindAgain(false); // SearchFindAgain; // DoSearchMenu(kFind);
		Longint('Back'): DoSearchFindAgain(true); // SearchFindAgain; // DoSearchMenu(kFind);
		Longint('Ente'): DoSearchMenu(kEnterSelection); // EnterSelection;
		Longint('FBak'): DoSearchMenu(kFindBackwards); // NEW
		Longint('Repl'): DoSearchMenu(kReplaceAndFindAgain);
		Longint('Rall'): DoSearchMenu(kReplaceAll); // NEW
		
		Longint('JUpd'): CheckJavaVersion;

		// Not used? EditWindEditMenu?
		Longint('Iout'): EditWindEditMenu(kEditShiftLeft); // IndentSelection(false);
		Longint('Inin'): EditWindEditMenu(kEditShiftRight); // IndentSelection(true);
		Longint('Tog2'): EditWindEditMenu(kToggleDual); // ToggleDoubleView;
		
		Longint('Buil'): DoBuildCommand(true, false); // Build(GetFrontEditWindow, true);
		Longint('Run '): DoRunCommand(false);
		Longint('RunD'): DoRunCommand(true);
		Longint('BUni'): DoBuildCommand(false, false); // Build(GetFrontEditWindow, false);
		Longint('BRel'): DoBuildCommand(true, true); // Build app, with final options
		Longint('Abor'): DoAbort;
		Longint('StOv'): BugsDoStep(kBugsStepOver);
		Longint('StIn'): BugsDoStep(kBugsStepIn);
		Longint('StOu'): BugsDoStep(kBugsStepOut);
		Longint('Bugs'): BugsShowWindow;
		Longint('BStp'): BugsStop;

		Longint('ClBr'): CBShowWindow; // Class browser window
		Longint('Proj'): ShowProjectWindow; // Project window
		Longint('Rota'): WriteLn('Rotate windows selected');
// RotateWindows;

		Longint('GDB '): BugsGDB;

// The following are NOT needed for menu selections, only for commands from toolbars!

// EditWindFileMenu
		kHICommandNew: EditWindFileMenu(kFileNew);
		kHICommandOpen: EditWindFileMenu(kFileOpen);
// kFileOpenSelection finns ej som kommando?
		Longint('opeS'): EditWindFileMenu(kFileOpenSelection); // Was missing?! Added 130810
		Longint('opeB'): EditWindFileMenu(kFileBackSelection); // Added 150115
		kHICommandClose: EditWindFileMenu(kFileClose);
		kHICommandSave: EditWindFileMenu(kFileSave);
		kHICommandSaveAs: EditWindFileMenu(kFileSaveAs);
		kHICommandRevert: EditWindFileMenu(kFileRevert);
		kHICommandPrint: EditWindFileMenu(kFilePageSetup); // ???
		kHICommandPageSetup: EditWindFileMenu(kFilePrint); // ???
// EditWindEditMenu/DoDialogEditMenu
		kHICommandUndo: EditWindEditMenu(kEditUndo);
		kHICommandRedo: EditWindEditMenu(kEditRedo);
		kHICommandCut: EditWindEditMenu(kEditCut);
		kHICommandCopy: EditWindEditMenu(kEditCopy);
		kHICommandPaste: EditWindEditMenu(kEditPaste);
		kHICommandClear: EditWindEditMenu(kEditClear);
		kHICommandSelectAll: EditWindEditMenu(kEditSelectAll);
		
		otherwise return false; // Important!
	end;
end;

procedure QuitProc;
begin
	if gCurrentProcess <> nil then
		if ProcessRunning(gCurrentProcess) then
		begin
		// Kolla gCurrentProcess^.commandLine!
			ProcessTerminate(gCurrentProcess);
			HelpAppendLn('Aborted running process');
			Exit(QuitProc);
		end;
	
	SkelWhoa;
end;

procedure CatchUnhandledException(obj: TObject; addr: Pointer; frameCount: Longint; frames: PPointer);
begin
	WriteLn(Exception(obj).className +' : '+ Exception(obj).message);
end;

var
	isNew: Boolean;
begin
// FPU exceptions
//	SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
	
	System.ExceptProc := CatchUnhandledException;
	
	SkelInit; // (6, nil);
//	GetSystemVersion(gSystemVersion.major, gSystemVersion.minor, gSystemVersion.bugfix);
	SetUpMenus;			{ install menu handlers }
	SkelSetSleep(60);
	SkelSetCommandProc(DoCommand);
//	MyInitMLTE;
//WriteLn('Doing settings... ', TickCount);
	SearchWindInit; // Needs to be done before InitSettings!
	isNew := InitSettings;
//WriteLn('Doing help... ', TickCount);
	HelpWindInit;
//WriteLn('Doing error window... ', TickCount);
	ErrorMsgWindInit;
//WriteLn('Doing search... ', TickCount);
//	SearchWindInit;
//WriteLn('Doing tables... ', TickCount);
	InitColorCodingTables;
//WriteLn('Checking version... ', TickCount);
//	CheckFPCVersion;
	CheckCompilersExists;
//WriteLn('Doing AdjustPrefs and about... ', TickCount);
	if isNew then AdjustPrefs;
	InitAbout;
	InitBreakpoints;
//WriteLn('BugsInit ', TickCount);
	BugsInit;
//WriteLn('InitProjectWindow ', TickCount);
	InitProjectWindow;
//WriteLn('InitGoToLineDialog ', TickCount);
	InitGoToLineDialog;
	SkelAppleEventFS (nil, @EditAEOpen, @EditAEPrint, QuitProc);
//	SkelBackground(@LWPBackground);
WriteLn('Running... ', TickCount);
	DoAutoReopen;
	SkelMain;
	WriteLn('Left main');
	AutoReopenSave;
	SkelClobber;
	//SetSintaxColor(gSettings.textSaveColorCoding);
	   { throw away windows and menus }
end.
