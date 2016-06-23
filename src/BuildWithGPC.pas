// Lightweight Pascal IDE, GPC compilation interface unit
// © Ingemar Ragnemalm 2009

{$mode macpas}
unit BuildWithGPC;
interface
uses
	MacOSAll, LWPGlobals, UtilsTypes, FileUtils, Classes, SysUtils, ProcessUtils,
	ErrorMessageWindow, FindReplaceDialog, AlertsUtils,
	Settings, ColorCoding, Console, BuildWithPascal, IncludeAnalyzer,
	BaseUnix;

function CompileGPC(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;

implementation

// Same as C
procedure CompileCData(oneLineOfData: AnsiString; privateData: Pointer);
begin
	HelpAppendLn(oneLineOfData);
	ParseGCCErrorMessage(oneLineOfData);
	HelpForceUpdate;
end;

function BuildGnuPascalCommandline(theSpec: FSSpecString; buildApp, buildFinal: Boolean; target: Longint; targetExtension: Boolean): AnsiString;
	var
		folderSpec: FSSpecString;
		commandLine, pathOnly: AnsiString;
		err: OSErr;
		i: Longint;
		appDirId: Longint;
		cliName: AnsiString;
		pathList: AnsiStringArray;
const
	CR = Char(13);
begin
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;
		
		// Create a full pathname to the file as AnsiString
		//path := FSSpecToPathNameString(theSpec);
		
		cliName := GetLastToken(theSpec);
		
		// We need the path without filename.
		pathOnly := TrimLastToken(theSpec);
WriteLn('Try chdir to: "', pathOnly, '"');
		chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
WriteLn('chdir OK');
		
//		if target = kPPCTarget then
//			commandLine := gSettings.PPCCompiler + ' ' + cliName{theSpec.name} + ' ' + gSettings.Options
//		else {386}
//			commandLine := gSettings.386Compiler + ' ' + cliName{theSpec.name} + ' ' + gSettings.Options;
		commandLine := gSettings.GPCCompiler + ' ' + cliName{theSpec.name} + ' ' + gSettings.Options;
		WriteLn('CompileGPC ', gSettings.GPCCompiler);
		
// VAD HETER DETTA I GPC?
		if not buildApp then
			commandLine := commandLine + ' -c ';
		
		// Final build or debug build?
		if buildFinal then
			commandLine := commandLine + ' ' + gSettings.ReleaseOptions
		else
			commandLine := commandLine + ' ' + gSettings.DebugOptions;
		
		// Add subfolder
//		if target = kPPCTarget then
//			commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/PPC'
//		else
//			commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/386';
// Irrelevant fšr GPC? Eller?
		
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
		BuildListFromText(gSettings.Frameworks, pathList);
		AppendCustomPaths('.frameworks', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Add Carbon anyway?
			commandLine := commandLine + ' -Wl,' + gSettings.LinkOptions; {gSettings.LinkOptions new i 0.3.2}
		end
		else
		begin
			commandLine := commandLine + ' -Wl,' + gSettings.LinkOptions; {gSettings.LinkOptions new i 0.3.2}
			for i := 0 to Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ',-framework,' + pathList[i];
			end;
		end;
		
		// Other linking (untested)
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
// prebind only 10.3 or older which is not supported anyway!
//			commandLine := commandLine + ' -prebind"';
		commandLine := commandLine + ' "';
		
		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' --unit-path=' + pathList[i];
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
		//cleanName := TrimExtension(theSpec.name);
//		folderSpec := theSpec;
		folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
//		err := FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
		err := FpMkdir(folderSpec, $777); // Mode???
		if target = kPPCTarget then
			folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName + ':PPC'
		else
			folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName + ':386';
//		err := FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
		err := FpMkdir(folderSpec, $777); // Mode???
		
		return commandLine;
end; {BuildGnuPascalCommandline}

function CompileGPC(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
	var
		cleanName: Str255;
		path, commandLine, pathOnly: AnsiString;
//		err: OSErr;
		i: Longint;
		pathList: AnsiStringArray;
		//fileList: AnsiString;
		extType: Longint;
const
	CR = Char(13);
begin
		if gVersionGPC = kProcessUtilsFailedString then
			if not QuestionAlert('GPC does not seem to be installed', 'Try compiling anyway?') then
				return -1;
		
		CompileGPC := noErr;
		
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;

		HelpMainMessage('Compiling ' + GetLastToken(theSpec)); // Show status
		
		// Create a full pathname to the file as AnsiString
		path := theSpec;
		pathOnly := path; // Needed for chdir
		pathOnly := TrimLastToken(pathOnly); // We need the path without filename.
		chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
		
HelpAppendLn('Want to compile ' + GetLastToken(theSpec));
		
		
//		fileList := BuildCFileList(theSpec);
//		WriteLn('************* BuildCFileList', fileList);

		commandLine := gSettings.GPCCompiler + ' ' + GetLastToken(theSpec);
		
		// Options. Override if .options file exists
		SetLength(pathList, 0);
		AppendCustomPaths('.options', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Default options (according to settings).
			commandLine := commandLine + ' ' + gSettings.GPCOptions; // LOCAL
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
			commandLine := commandLine + ' ' + gSettings.ReleaseOptions
		else
			commandLine := commandLine + ' ' + gSettings.DebugOptions;
	
// Better?
// function BuildGnuPascalCommandline(theSpec: FSSpecString; buildApp, buildFinal: Boolean; target: Longint; targetExtension: Boolean): AnsiString;
		
		// Add subfolder
		// €r -o rŠtt vŠg att visa var den skall lŠgga det, eller finns nŒt i stil med -FU?
		cleanName := TrimExtension(theSpec);
//		commandLine := commandLine + ' -o ' + './' + kObjectFolderName + '/' + cleanName + '.o -c'; // -c gšr att man fŒr objektkod
		
		if not buildExecutable then
			commandLine := commandLine + ' -c'; // Only compile to .o file (in same folder). Do not link.
		if buildExecutable then
			commandLine := commandLine + ' -o ' + cleanName; // Normal compile and link
		//commandLine := commandLine + ' -FU' + './' + kObjectFolderName;
		
{		
		// Architectures is easy with GCC!
		if gFlags^^.target = kUniversalTarget then
			commandLine := commandLine + ' -arch ppc -arch i386 ';
		if gFlags^^.target = kPPCTarget then
			commandLine := commandLine + ' -arch ppc ';
		if gFlags^^.target = k386Target then
			commandLine := commandLine + ' -arch i386 ';
}		

// Is this correct?
// Change AFTER 0.6
		if gSettings.target = kUniversalTarget then
			commandLine := commandLine + ' --c=-arch --c=i386 --c=-arch --c=ppc ';
		if gSettings.target = kPPCTarget then
			commandLine := commandLine + ' --c=-arch --c=ppc ';
		if gSettings.target = k386Target then
			commandLine := commandLine + ' --c=-arch --c=i386 ';
		
		// Add frameworks
		// -k"-framework Carbon -prebind"
		// When is prebind unnecessary?
		BuildListFromText(gSettings.Frameworks, pathList);
		AppendCustomPaths('.frameworks', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Add Carbon anyway?
			commandLine := commandLine + ' -Wl';
			if Length(gSettings.LinkOptions) > 0 then
				commandLine := commandLine + ',' + gSettings.LinkOptions; {gSettings.LinkOptions new i 0.3.2}
		end
		else
		begin
			commandLine := commandLine + ' -Wl';
			if Length(gSettings.LinkOptions) > 0 then
				commandLine := commandLine + ',' + gSettings.LinkOptions; {gSettings.LinkOptions new i 0.3.2}
			for i := 0 to Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ',-framework,' + pathList[i];
			end;
		end;


		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' --unit-path=' + pathList[i];
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
		
		commandLine := MacRomanToUTF8(commandLine); // Entire command line to UTF-8! Or is it better in separate parts?
		
		// Create object code subfolder
		//cleanName := TrimExtension(theSpec);
//		folderSpec := theSpec;
//		folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
//		err := FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.

		LaunchCompilation(commandLine, theSpec, buildExecutable, buildFinal, compileCallback, @CompileCData, @CompilationDone);
		SelectWindow(helpWind);
end;

end.
