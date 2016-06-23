// Lightweight Pascal IDE, Java compilation interface unit
// © Ingemar Ragnemalm 2007

unit BuildWithJava;
interface
uses
	MacOSAll, LWPGlobals, FileUtils, Classes, SysUtils, ProcessUtils,
	ErrorMessageWindow, FindReplaceDialog, UtilsTypes,
	Settings, ColorCoding, Console, BuildWithPascal, IncludeAnalyzer;

function CompileJava(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
//procedure RunJava(theSpec: FSSpecString);

implementation

// Finish of Java compilations
// NEW 130925: Applet support
procedure JavaCompilationDone(aborted: Boolean; result: Integer; privateData: Pointer);
var
	theHuttmullString, appletName: AnsiString;
	err: OSErr;
begin
	if aborted then
	begin
		HelpAppendLn('Compilation aborted');
		HelpMainMessage(''); // Status
		Exit(JavaCompilationDone);
	end;
	
	HelpAppendLn('Compilation done');
	HelpMainMessage(''); // Status
	
	if gMainFrontIndex > 0 then
	if not editMainWindIsProgram[gMainFrontIndex] then
	begin
		if not FileExists(TrimExtension(gLastMainFile) + '.html') then
		begin
			// Skapa HTML-fil, skriv standardinnehåll
			appletName := GetLastToken(TrimExtension(gLastMainFile));
			theHuttmullString := // A minimal HTML for running an applet
					'<title>' + appletName + '</title><hr>'#10+
					'<applet archive="AppletClasses.jar" code="' + appletName + '.class"'#10+
					'width=200 height=200>'+
					'</applet>';
			WriteStringToFile(theHuttmullString, TrimExtension(gLastMainFile) + '.html');
		end;
	end;
	
	if gCompilationData.compileCallback <> nil then
		gCompilationData.compileCallback(result, gCompilationData.theSpec);
end; {JavaCompilationDone}


procedure CompileJavaData(oneLineOfData: AnsiString; privateData: Pointer);
begin
	HelpAppendLn(oneLineOfData);
//	ParseGCCErrorMessage(oneLineOfData);
	HelpForceUpdate;
end;

function CompileJava(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
	var
//		srcSpec, 
//		folderSpec: FSSpecString;
		cleanName: Str255;
		path, commandLine, pathOnly: AnsiString;
//		err: OSErr;
		i: Longint;
//		appDirId: LongWord;
//		cliName: AnsiString;
		pathList: AnsiStringArray;
//		fileList: AnsiString;
//		extType: Longint;
const
	CR = Char(13);
begin
		CompileJava := noErr;
		
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;

		HelpMainMessage('Compiling ' + GetLastToken(theSpec)); // Status
		
		// Create a full pathname to the file as AnsiString
		path := theSpec;
		pathOnly := path; // Needed for chdir

		pathOnly := TrimLastToken(pathOnly);
//		chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
		chdir(pathOnly); // Set that as current dir

HelpAppendLn('Want to compile ' + GetLastToken(theSpec));
		
		commandLine := gSettings.JavaCompiler + ' ' + GetLastToken(theSpec) + ' ' + gSettings.JavaOptions; // LOCAL
		
		// Add subfolder
		// Är -o rätt väg att visa var den skall lägga det, eller finns nåt i stil med -FU?
		cleanName := TrimExtension(theSpec);

// SHOULD ADD THE FOLLOWING - but only for application builds with resources?
// Compile so .class files go into intermediate-files... or a subfolder of it?
// CREATE THESE FOLDERS!
//		commandLine := commandLine + ' -d ' + './' + kObjectFolderName + '/' + cleanName + ' ';
		
		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -sourcepath ' + pathList[i];
		end;
		// Added 100823: Always include local directory
		commandLine := commandLine + ' -sourcepath ./';
		
		// Additional library paths from gSettings.LibPaths
//		BuildListFromText(gSettings.LibPaths, pathList);
//		for i := 0 to Length(pathList)-1 do
//		begin
//			if Length(pathList[i]) > 0 then
//				commandLine := commandLine + ' -L' + pathList[i];
//		end;


HelpAppendLn('*** Command: '+commandLine);


		// Create object code subfolder
		//cleanName := TrimExtension(theSpec.name);
//		folderSpec := theSpec;
//		folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
//		err := FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.


//		LaunchCompilation(commandLine, theSpec, buildExecutable, buildFinal, compileCallback, @CompileJavaData, @JavaCompilationDone);
		LaunchCompilation(commandLine, theSpec, false, false, compileCallback, @CompileJavaData, @JavaCompilationDone);
		SelectWindow(helpWind);
// OBS! När man väl vill göra java-Bundles så måste denna ändras!
end;

(*procedure RunJava(theSpec: FSSpecString);
var
	cleanName: AnsiString;
	path, commandLine, pathOnly: AnsiString;
//	err: OSErr;
	cleanNamePtr: StringPtr;
begin
	HelpAppendLn('Running Java not yet implemented');	

		if gCurrentProcess <> nil then
		begin
			if ProcessRunning(gCurrentProcess) then
			begin
				DebugStr('Another process is already running');
				Exit(RunJava);
			end
			else
				ProcessDispose(gCurrentProcess);
		end;
		
		path := FSSpecToPathNameString(theSpec);
		pathOnly := path; // Needed for chdir
		// We need the path without filename.
		pathOnly := TrimLastToken(pathOnly);
		chdir(pathOnly); // Set that as current dir
		
		cleanName := TrimExtension(theSpec.name);
		path := TrimExtension(path);
		
		HelpMainMessage('Running ' + cleanName); // Show status
		
		commandLine := gSettings.JavaInterpreter + ' ' + cleanName;

		HelpAppendLn('Command: '+commandLine);
//		HelpAppendLn('--- ' + cleanName + ' starts ---');

		
		gCurrentProcess := ProcessLaunch(commandLine, true);
		if gCurrentProcess <> nil then
		begin
			ProcessSetCallbacks(gCurrentProcess, @RunData, @RunDone, Pointer(cleanNamePtr));
			if gCurrentProcess^.doneCallback = nil then
				HelpAppendLn('NO DONE CALLBACK!');
			ProcessSetLineBuffering(gCurrentProcess, false);
			SelectWindow(theErrorMsgWind);
			SelectWindow(helpWind);
		end
		else
		begin
			HelpAppendLn('Launch failed - fork failed');
			HelpMainMessage(''); // Show status
		end;
end;*)

end.


// Info about how to build a Java application:
// https://docs.oracle.com/javase/7/docs/technotes/guides/jweb/packagingAppsForMac.html
