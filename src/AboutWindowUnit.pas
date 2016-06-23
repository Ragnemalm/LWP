{$mode macpas}
unit AboutWindowUnit;
interface
uses
	MacOSAll, TransSkel4, LWPGlobals, ProcessUtils, Settings, UtilsTypes;

procedure InitAbout;

procedure ShowAbout;

procedure CheckJavaVersion;
procedure CheckFPCVersion;
procedure CheckCompilersExists;
procedure CheckiOSSDKVersion;

implementation
	uses
		BaseUnix;
	
	// Faster startup by only checking if the compilers exist	
	procedure CheckCompilersExists;
	var
		err: OSErr;
		info: stat;
	begin
		err:=fpStat(gSettings.AdaCompiler, info);
		if err = noErr then
			gVersionAda := 'Installed'
		else
			gVersionAda := kProcessUtilsFailedString;

		err:=fpStat(gSettings.x86Compiler, info);
		if err = noErr then
			gVersion386 := 'Installed'
		else
			gVersion386 := kProcessUtilsFailedString;
		
		err:=fpStat(gSettings.PPCCompiler, info);
		if err = noErr then
			gVersionPPC := 'Installed'
		else
			gVersionPPC := kProcessUtilsFailedString;
		
		err:=fpStat(gSettings.JavaCompiler, info);
		if err = noErr then
			gVersionJava := 'Installed'
		else
			gVersionJava := kProcessUtilsFailedString;
		
		err:=fpStat(gSettings.CCompiler, info);
		if err = noErr then
			gVersionGCC := 'Installed'
		else
			gVersionGCC := kProcessUtilsFailedString;

		err:=fpStat(gSettings.gdb, info);
		if err = noErr then
		begin
			gVersionGDB := 'Installed';
			WriteLn('Debugger setting found from ', gSettings.gdb);
		end
		else
		begin
			gVersionGDB := kProcessUtilsFailedString;
			WriteLn('Debugger setting found from ', gSettings.gdb, ' FAILED');
		end;


		// And some more (0.8.11)
(*		err:=fpStat(gSettings386Compiler2, info);
		if err = noErr then
			gVersionObjP386 := 'Installed'
		else
			gVersionObjP386 := kProcessUtilsFailedString;
		
		err:=fpStat(gSettingsPPCCompiler2, info);
		if err = noErr then
			gVersionObjPPPC := 'Installed'
		else
			gVersionObjPPPC := kProcessUtilsFailedString;*)
		
		err:=fpStat(gSettings.CUDACompiler, info);
		if err = noErr then
			gVersionCUDA := 'Installed'
		else
			gVersionCUDA := kProcessUtilsFailedString;
		
		err:=fpStat(gSettings.iOSCompiler, info);
		if err = noErr then
			gVersioniOS := 'Installed'
		else
			gVersioniOS := kProcessUtilsFailedString;
		
		err:=fpStat(gSettings.iPhoneSimulatorCompiler, info);
		if err = noErr then
			gVersionSimulator := 'Installed'
		else
			gVersionSimulator := kProcessUtilsFailedString;
	end;
	
procedure CheckiOSSDKVersion;
var
	theList: StringArr;
	commandline: AnsiString;
begin
//	commandline := '/bin/ls /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs';
	commandline := '/bin/ls /Developer/Platforms/iPhoneOS.platform/Developer/SDKs';
	theList := ProcessLaunchToStringArr(commandline);
	if Length(theList) >= 1 then
	begin
		//gVersionGCC := theList[0];
		giOSSDK := theList[High(theList)];
		giOSSDK := Copy(giOSSDK, Length(giOSSDK)-6, 3);
	end
	else
		gVersionSimulator := kProcessUtilsFailedString;

	VMSetStringValue(aboutWind, 'SDKv', 0, giOSSDK);
end;

function CheckFPCCompiler(compiler: AnsiString): AnsiString;
var
	info: stat;
	theList: StringArr;
begin
	CheckFPCCompiler := kProcessUtilsFailedString;
	if fpStat(compiler, info) = noErr then
	begin
		theList := ProcessLaunchToStringArr(compiler + ' -iV');
		if Length(theList) >= 1 then
			CheckFPCCompiler := theList[0];
	end;
end;
	
	// Display compiler version. Strange bug?
	procedure CheckFPCVersion;
	var
		theList: StringArr;
//		adaStr, s: AnsiString;
		err: OSErr;
		info: stat;
		i: Longint;
	begin
	// Tre anrop:
	// Kolla version pŒ ppcppc, ppc386 och gcc!
		
//WriteLn('Checking GCC... ', TickCount);
//		theList := ProcessLaunchToStringArr(gSettings.CCompiler + ' --version');
		theList := ProcessLaunchToStringArr('/bin/ls -l ' + gSettings.CCompiler);
		if Length(theList) >= 1 then
		begin
			gVersionGCC := theList[0];
			
			for i := Length(gVersionGCC) downto 1 do
				if gVersionGCC[i] = ' ' then Leave;
			gVersionGCC := Copy(gVersionGCC, i+1, Length(gVersionGCC) - i);

			// Shorten the version string to the interesting part (fixed 100517)
//			for i := Length(gVersionGCC) downto 1 do
//				if gVersionGCC[i] = '(' then Leave;
//			j := i; // Save position of "("
//			if i > 1 then i := i - 2;
//			for i := i downto 1 do
//				if gVersionGCC[i] = ' ' then Leave;
//			if i > 1 then
//				gVersionGCC := Copy(gVersionGCC, i+1, j-i-1);
//			gVersionGCC := 'Damn GCC';
		end
		else
			gVersionGCC := kProcessUtilsFailedString;

WriteLn('Checking all FPC... ', TickCount);
	gVersion386 := CheckFPCCompiler(gSettings.x86Compiler);
	gVersionPPC := CheckFPCCompiler(gSettings.PPCCompiler);
	gVersionSimulator := CheckFPCCompiler(gSettings.iPhoneSimulatorCompiler);
	gVersioniOS := CheckFPCCompiler(gSettings.iOSCompiler);

WriteLn('Checking CUDA... ', TickCount);
		theList := ProcessLaunchToStringArr(gSettings.CudaCompiler + ' --version');
		if Length(theList) >= 1 then
		begin
			gVersionCUDA := theList[High(theList)]; // Last line
			// Remove everything before first number
			for i := 1 to Length(gVersionCUDA) do
				if gVersionCUDA[i] in ['0'..'9'] then Leave;
			gVersionCUDA := Copy(gVersionCUDA, i, Length(gVersionCUDA) - i);
//			gVersionCUDA := 'Frums';
		end
		else
			gVersionCUDA := kProcessUtilsFailedString;
		
//WriteLn('Checking Java... ', TickCount);'
// TOO SLOW
//		theList := ProcessLaunchToStringArr(gSettings.JavaCompiler + ' -version');
//		if Length(theList) >= 1 then
//		begin
//			gVersionJava := theList[0];
//			if copy(gVersionJava, 1, 6) = 'javac ' then
//				gVersionJava := copy(gVersionJava, 7, Length(gVersionJava) - 6);
//		end
//		else
//			gVersionJava := kProcessUtilsFailedString;
//		gVersionJava := ''; // 'Version number not checked';

//WriteLn('Checking ios version... ', TickCount);
		CheckiOSSDKVersion;

//WriteLn('Checking GDB... ', TickCount);
		err:=fpStat(gSettings.gdb, info);
		if err = noErr then
		begin
			theList := ProcessLaunchToStringArr(gSettings.gdb + ' --version');
			if Length(theList) >= 1 then
			begin
				gVersionGDB := theList[0];
				if copy(gVersionGDB, 1, 8) = 'GNU gdb ' then
					gVersionGDB := copy(gVersionGDB, 9, Length(gVersionGDB) - 9);
			end
			else
				gVersionGDB := 'Installed';
		end
		else
			gVersionGDB := kProcessUtilsFailedString;

// Och ADA
//WriteLn('Checking ADA... ', TickCount);
		err:=fpStat(gSettings.AdaCompiler, info);
		if err = noErr then
		begin
			theList := ProcessLaunchToStringArr(gSettings.AdaCompiler + ' --version');
			if Length(theList) >= 1 then
			begin
				gVersionAda := theList[0];
				if copy(gVersionAda, 1, 13) = 'GNATMAKE GPL ' then
					gVersionAda := copy(gVersionAda, 14, Length(gVersionAda) - 13);
			end
			else
				gVersionAda := 'Installed';
		end
		else
			gVersionAda := kProcessUtilsFailedString;

//WriteLn('All done!... ', TickCount);
end;

procedure CheckJavaVersion;
var
	theList: StringArr;
	control: ControlRef;
	err: OSErr;
	info: stat;
begin
	// Modified 090503: Check all!
	CheckFPCVersion;
	
	WriteLn(gSettings.JavaCompiler + ' -version');
	err:=fpStat(gSettings.JavaCompiler, info);
	if err = noErr then
	begin
		theList := ProcessLaunchToStringArr(gSettings.JavaCompiler + ' -version');
		if Length(theList) >= 1 then
		begin
			gVersionJava := theList[0];
			if copy(gVersionJava, 1, 6) = 'javac ' then
				gVersionJava := copy(gVersionJava, 7, Length(gVersionJava) - 6);
			WriteLn('Found ', gVersionJava);
		end
		else
			gVersionJava := 'Installed'; // Failed to get version
	end
	else
		gVersionJava := kProcessUtilsFailedString;
	SetLength(theList, 0);

// Java version does not work with ProcessUtils! (I have not figured out why yet.) Resort to just detecting it.
//		err:=fpStat(gSettings.JavaCompiler, info);
//		if err = noErr then
//			gVersionJava := 'Installed'
//		else
//			gVersionJava := kProcessUtilsFailedString;

	WriteLn('Putting it in again ', gVersionJava);

	VMSetStringValue(aboutWind, 'Javv', 0, gVersionJava);
	VMGetControl(aboutWind, 'JUpd', 0, control);
//	SetControlVisibility( control, false, true );

// Also, redo any missing one!
//		if gVersionGCC = kProcessUtilsFailedString then
//		begin
//			theList := ProcessLaunchToStringArr(gSettings.CCompiler + ' --version');
//			if Length(theList) >= 1 then
//				gVersionGCC := theList[0]
//			else
//				gVersionGCC := kProcessUtilsFailedString;
//		end;

(*	WriteLn('Messing with FPC again?');

		if gVersion386 = kProcessUtilsFailedString then
		begin
			theList := ProcessLaunchToStringArr(gSettings.x86Compiler + ' -iV');
			if Length(theList) >= 1 then
				gVersion386 := theList[0]
			else
				gVersion386 := kProcessUtilsFailedString;
		end;
		
		if gVersionPPC = kProcessUtilsFailedString then
		begin
			theList := ProcessLaunchToStringArr(gSettings.PPCCompiler + ' -iV');
			if Length(theList) >= 1 then
				gVersionPPC := theList[0]
			else
				gVersionPPC := kProcessUtilsFailedString;
		end;*)

	WriteLn('Slamming it in including ', gVersionJava);

	VMSetStringValue(aboutWind, 'FPCi', 0, gVersion386);
	VMSetStringValue(aboutWind, 'FPCp', 0, gVersionPPC);
	VMSetStringValue(aboutWind, 'GCCv', 0, gVersionGCC);
	VMSetStringValue(aboutWind, 'Javv', 0, gVersionJava);
	VMSetStringValue(aboutWind, 'Adav', 0, gVersionAda);
// 0.8.11
//	VMSetStringValue(aboutWind, '251i', 0, gVersionObjP386);
//	VMSetStringValue(aboutWind, '251p', 0, gVersionObjPPPC);
	VMSetStringValue(aboutWind, 'CDAv', 0, gVersionCUDA);
	VMSetStringValue(aboutWind, 'iPSv', 0, gVersionSimulator);
	VMSetStringValue(aboutWind, 'iOSv', 0, gVersioniOS);
	VMSetStringValue(aboutWind, 'SDKv', 0, giOSSDK);

	VMSetStringValue(aboutWind, 'GDBi', 0, gVersionGDB);
	
	WriteLn('Even more done');
end;

procedure ShowAbout;
begin
//	if gVersionJava = '' then
//	begin
//		CheckJavaVersion;
//		VMSetStringValue(aboutWind, 'Javv', 0, gVersionJava);
//	end;
	ShowWindow(aboutWind);
	SelectWindow(aboutWind);
end;

procedure DoClose;
var
	thePort: GrafPtr;
begin
	GetPort(thePort);			{ grafport of window to be closed }
	HideWindow(GetWindowFromPort(thePort));
end;

procedure InitAbout;
var
	hv: HIViewRef;
	txnObj: TXNObject;
	theURL: CFURLRef;
	C:array[0..3] of Double;	
begin
	CreateWindowFromNib(SkelGetMainNib, CFSTR('About'), aboutWind);
	InstallAllTabs(aboutWind);
	VMSetStringValue(aboutWind, 'LWPv', 0, gVersionLWP);
	VMSetStringValue(aboutWind, 'LWPv', 1, 'Version ' + gVersionLWP);
	VMSetStringValue(aboutWind, 'FPCi', 0, gVersion386);
	VMSetStringValue(aboutWind, 'FPCp', 0, gVersionPPC);
	VMSetStringValue(aboutWind, 'GCCv', 0, gVersionGCC);
	VMSetStringValue(aboutWind, 'Adav', 0, gVersionAda);
	VMSetStringValue(aboutWind, 'Javv', 0, gVersionJava);
//	VMSetStringValue(aboutWind, 'TrSv', 0, SkelGetVersion);
	VMSetStringValue(aboutWind, 'GDBi', 0, gVersionGDB);

//	VMSetStringValue(aboutWind, '251i', 0, gVersionObjP386);
//	VMSetStringValue(aboutWind, '251p', 0, gVersionObjPPPC);
	VMSetStringValue(aboutWind, 'CDAv', 0, gVersionCUDA);
	VMSetStringValue(aboutWind, 'iPSv', 0, gVersionSimulator);
	VMSetStringValue(aboutWind, 'iOSv', 0, gVersioniOS);
// This will currently not show anything at init
	VMSetStringValue(aboutWind, 'SDKv', 0, giOSSDK);
	
	
	VMGetControl(aboutWind, 'txt1', 0, hv);
	C[0]:=0;	C[1]:=0;	C[2]:=0;C[3]:=3;
	
	txnObj := HITextViewGetTXNObject(hv);
	theURL := CFBundleCopyResourceURL(CFBundleGetMainBundle(), CFSTR('about'), CFSTR('rtf'), nil);
	TXNSetDataFromCFURLRef(txnObj, theURL, kTXNStartOffset, kTXNEndOffset);
	CFRelease(theURL);
	Draw1Control( hv );
	
	VMGetControl(aboutWind, 'txt2', 0, hv);
	txnObj := HITextViewGetTXNObject(hv);
	theURL := CFBundleCopyResourceURL(CFBundleGetMainBundle(), CFSTR('contributors'), CFSTR('rtf'), nil);
	TXNSetDataFromCFURLRef(txnObj, theURL, kTXNStartOffset, kTXNEndOffset);
	Draw1Control( hv );
	CFRelease(theURL);

	SkelWindow(aboutWind, nil, nil, nil, nil, @DoClose, nil, nil, false);
end;

end.
