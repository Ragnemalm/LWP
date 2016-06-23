// New Toolbar Manager for Objective Pascal, part of TransSkel5 (but can// be used independently).// Based on Apple's ToolbarSample demo.// This is the first version, not yet perfect (customization has problems// with custom items) but mostly working. Usable but not perfect.// Will be heavily revised, based on the Nutshell demo instead, since// ToolbarSample is seriously flawed.// 101221: Added AnsiString-based variants of item creation calls.// Open question: Does the callback need the object (id) or is the string enough?{$mode objfpc}{$modeswitch objectivec1}unit ToolbarManager;interfaceuses	ctypes, MacOSAll, CocoaAll;type	SkelToolbar = objcclass;	TSIdStrProcPtr = procedure(sender: id; s: PChar);	ToolbarItemNSViewCreatorProc = function(tb: SkelToolbar; customData: Pointer): NSView;		// Really just a record, but defined as objcclass so it can go in	// a NSArray	ToolbarItemData = objcclass(NSObject)//		url: NSString;//		iconName: NSString;		icon: NSImage;		viewCreationCallback: ToolbarItemNSViewCreatorProc;		customData: Pointer;		itemLabel: NSString;		shortHelp{, longHelp}: NSString;		setAction: Boolean;				itemType: Integer;	end;		SkelToolbar = objcclass(NSToolbar, NSToolbarDelegateProtocol)	public		theWindow: NSWindow;		itemsArray: NSMutableDictionary;		allowedItemsArray, defaultItemsArray: NSMutableArray;		actionCallback: TSIdStrProcPtr;//Required NSToolbar delegate methods		function toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar(toolbar:NSToolbar; itemIdentifier: NSString; flag:Boolean): NSToolbarItem; message 'toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:';		function toolbarDefaultItemIdentifiers(toolbar: NSToolbar): NSArray; message 'toolbarDefaultItemIdentifiers:';		function toolbarAllowedItemIdentifiers(toolbar: NSToolbar): NSArray; message 'toolbarAllowedItemIdentifiers:';				procedure toolbarItemAction(sender: id); message 'toolbarItemAction:';		function validateToolbarItem(theItem: NSToolbarItem): Boolean; message 'validateToolbarItem:'; override; // Optional		procedure toolbarWillAddItem(notif: NSNotification); message 'toolbarWillAddItem:'; // Optional		procedure printDocument(sender: id); message 'printDocument:';	end;	ToolbarPtr = SkelToolbar;function StringToNSString(input: AnsiString): NSString;function ToolbarNew(url: AnsiString; actionCallback: TSIdStrProcPtr): ToolbarPtr; overload;function ToolbarNew(url: NSString; actionCallback: TSIdStrProcPtr): ToolbarPtr; overload;procedure ToolbarAddStandardItem(tb: ToolbarPtr; url: NSString; isDefault, isPermanent: Boolean); overload;procedure ToolbarAddIconItem(tb: ToolbarPtr; url: NSString; itemLabel, shortHelp: NSString;						myIcon: NSString; isDefault, isPermanent: Boolean); overload;procedure ToolbarAddView(tb: ToolbarPtr; url: NSString;						itemLabel, shortHelp: NSString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc); overload;procedure ToolbarAddView(tb: ToolbarPtr; url: NSString;						itemLabel, shortHelp: NSString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc;						customData: Pointer; setAction: Boolean); overload;// AnsiString variantsprocedure ToolbarAddStandardItem(tb: ToolbarPtr; url: AnsiString; isDefault, isPermanent: Boolean);overload;procedure ToolbarAddIconItem(tb: ToolbarPtr; url: AnsiString; itemLabel, shortHelp: AnsiString;						myIcon: AnsiString; isDefault, isPermanent: Boolean);overload;procedure ToolbarAddView(tb: ToolbarPtr; url: AnsiString;						itemLabel, shortHelp: AnsiString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc); overload;procedure ToolbarAddView(tb: ToolbarPtr; url: AnsiString;						itemLabel, shortHelp: AnsiString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc;						customData: Pointer; setAction: Boolean); overload;procedure ToolbarCreate(tb: ToolbarPtr; theWindow: NSWindow);function NSStringToString(input: NSString): AnsiString;implementationfunction CFStringToString(input: CFStringRef): AnsiString;var	output: AnsiString;	used: Longint;begin	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),			kCFStringEncodingUTF8, Ord('^'), false, nil, CFStringGetLength(input)*2, used);	SetLength(output, used);	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),			kCFStringEncodingUTF8, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);	result := output;end;function NSStringToString(input: NSString): AnsiString;begin	result := CFStringToString(CFStringRef(input));end;procedure AddItem(theDict: NSMutableDictionary;		identifier, itemLabel, toolTip: NSString;		iconName: NSString;		viewCreationCallback: ToolbarItemNSViewCreatorProc;		customData: Pointer;		setAction: Boolean);var	theItemData: ToolbarItemData;begin	theItemData := ToolbarItemData.alloc;		theItemData.icon := NSImage.imageNamed(iconName);	theItemData.viewCreationCallback := viewCreationCallback;	theItemData.customData := customData;	theItemData.itemLabel := itemLabel;	theItemData.shortHelp := toolTip;	theItemData.setAction := setAction;		theDict.setObject_forKey(theItemData, identifier);end;procedure SkelToolbar.toolbarItemAction(sender: id);var	s: AnsiString;begin// Check class name. If NSToolbarItem, send its identifier, otherwise its class name.	s := NSStringToString(NSStringFromClass(NSView(sender).classForCoder));	if s = 'NSToolbarItem' then		s := NSStringToString(NSToolbarItem(sender).itemIdentifier);	if actionCallback <> nil then		actionCallback(sender, PChar(s));end;function SkelToolbar.toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar(toolbar:NSToolbar; itemIdentifier: NSSTring; flag: Boolean): NSToolbarItem;var	newItem: NSToolbarItem;	item: ToolbarItemData;begin	// We create and autorelease a new NSToolbarItem, and then go through the process of setting up its	// attributes from the master toolbar item matching that identifier in our dictionary of items.	newItem := NSToolbarItem.alloc.initWithItemIdentifier(itemIdentifier);	newItem.autorelease;	item := itemsArray.objectForKey(itemIdentifier);	if item = nil then	begin		WriteLn('Toolbar Manager error: Illegal item: ', NSStringToString(itemIdentifier));		result := nil;		Exit;	end;		newItem.setLabel(item.itemLabel);	newItem.setPaletteLabel(item.itemLabel);	if item.viewCreationCallback <> nil then		newItem.setView(item.viewCreationCallback(self, item.customData))	else		newItem.setImage(item.icon);	newItem.setToolTip(item.shortHelp);//	newItem.setTarget(item.target);	newItem.setTarget(self);	newItem.setAction(sel_registerName(PChar('toolbarItemAction:')));//	newItem.setMenuFormRepresentation(item.menuFormRepresentation);	// If we have a custom view, we *have* to set the min/max size - otherwise, it'll default to 0,0 and the custom	// view won't show up at all!  This doesn't affect toolbar items with images, however.	if newItem.view <> nil then	begin		newItem.setMinSize(newItem.view.bounds.size);		newItem.setMaxSize(newItem.view.bounds.size);		if item.setAction then		begin			NSControl(newItem.view).setTarget(self);			NSControl(newItem.view).setAction(sel_registerName(PChar('toolbarItemAction:')));		end;	end;		result := newItem;end;function SkelToolbar.toolbarDefaultItemIdentifiers(toolbar: NSToolbar): NSArray;begin	result := defaultItemsArray;end;function SkelToolbar.toolbarAllowedItemIdentifiers(toolbar: NSToolbar): NSArray;begin	result := allowedItemsArray;end;function SkelToolbar.validateToolbarItem(theItem: NSToolbarItem): Boolean;begin	// You could check [theItem itemIdentifier] here and take appropriate action if you wanted to	result := true;end;procedure SkelToolbar.toolbarWillAddItem(notif: NSNotification);var	addedItem: NSToolbarItem;begin	addedItem := notif.userInfo.objectForKey(NSSTR('item'));// Is this the printing toolbar item?  If so, then we want to redirect it's action to ourselves// so we can handle the printing properly; hence, we give it a new target.    if NSString(addedItem.itemIdentifier).isEqualToString(NSToolbarPrintItemIdentifier) then	begin		addedItem.setToolTip(NSSTR('Print your document'));		addedItem.setTarget(self);	end;end;  procedure SkelToolbar.printDocument(sender: id);begin	if actionCallback <> nil then		actionCallback(sender, PChar('print'));end;function StringToNSString(input: AnsiString): NSString;begin	result := NSString.stringWithUTF8String(PChar(input));	// Autoreleasedend;//function ToolbarNew(url: AnsiString): ToolbarPtr; overload;function ToolbarNew(url: AnsiString; actionCallback: TSIdStrProcPtr): ToolbarPtr; overload;begin	result := ToolbarNew(StringToNSString(url), actionCallback);	// release NSString?end;//function ToolbarNew(url: NSString): ToolbarPtr; overload;function ToolbarNew(url: NSString; actionCallback: TSIdStrProcPtr): ToolbarPtr; overload;var	toolbar: SkelToolbar;begin	toolbar := SkelToolbar.alloc.initWithIdentifier(url);	toolbar.autoRelease;// Man beh�ver ett dictionary f�r items	toolbar.itemsArray := NSMutableDictionary.dictionary;	toolbar.itemsArray.retain;// Delegate to itself	toolbar.setDelegate(NSToolbarDelegateProtocol(toolbar));// Inst�llningar	toolbar.setAllowsUserCustomization(true);	toolbar.setAutosavesConfiguration(true); 	toolbar.setDisplayMode(2 {NSToolbarDisplayModeIconOnly});		toolbar.allowedItemsArray := NSMutableArray.new;	toolbar.defaultItemsArray := NSMutableArray.new;		toolbar.actionCallback := actionCallback;		result := toolbar;end;procedure ToolbarAddStandardItem(tb: ToolbarPtr; url: NSString; isDefault, isPermanent: Boolean);begin//	WriteLn('ToolbarAddStandardItem');//	WriteLn(tb.allowedItemsArray.count);//	WriteLn(tb.defaultItemsArray.count);	tb.allowedItemsArray.addObject(url); // Kan man casta till id s� h�r?	if isDefault then		tb.defaultItemsArray.addObject(url);	// Vad g�ra med isPermanent?end;procedure ToolbarAddIconItem(tb: ToolbarPtr; url: NSString; itemLabel, shortHelp: NSString;						myIcon: NSString; isDefault, isPermanent: Boolean);begin	// use addToolbarItem, saves directly to dict	// send action to the built-in toolbarItemAction	AddItem(tb.itemsArray, url, itemLabel, shortHelp,		myIcon, nil, nil, false);		tb.allowedItemsArray.addObject(url);	if isDefault then		tb.defaultItemsArray.addObject(url);end;// Here the view is expected to take care of itselfprocedure ToolbarAddView(tb: ToolbarPtr; url: NSString;						itemLabel, shortHelp: NSString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc;						customData: Pointer; setAction: Boolean); overload;begin	AddItem(tb.itemsArray, url, itemLabel, shortHelp,		nil, viewCreationCallback, customData, setAction);	tb.allowedItemsArray.addObject(url);	if isDefault then		tb.defaultItemsArray.addObject(url);end;procedure ToolbarAddView(tb: ToolbarPtr; url: NSString;						itemLabel, shortHelp: NSString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc); overload;begin	AddItem(tb.itemsArray, url, itemLabel, shortHelp,		nil, viewCreationCallback, nil, true);	tb.allowedItemsArray.addObject(url);	if isDefault then		tb.defaultItemsArray.addObject(url);end;// AnsiString variantsprocedure ToolbarAddStandardItem(tb: ToolbarPtr; url: AnsiString; isDefault, isPermanent: Boolean);overload;begin	ToolbarAddStandardItem(tb, StringToNSString(url), isDefault, isPermanent);end;procedure ToolbarAddIconItem(tb: ToolbarPtr; url: AnsiString; itemLabel, shortHelp: AnsiString;						myIcon: AnsiString; isDefault, isPermanent: Boolean);overload;begin	ToolbarAddIconItem(tb, StringToNSString(url), StringToNSString(itemLabel), StringToNSString(shortHelp),						StringToNSString(myIcon), isDefault, isPermanent);end;procedure ToolbarAddView(tb: ToolbarPtr; url: AnsiString;						itemLabel, shortHelp: AnsiString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc); overload;begin	ToolbarAddView(tb, StringToNSString(url), StringToNSString(itemLabel), StringToNSString(shortHelp),						isDefault, isPermanent, viewCreationCallback);end;						procedure ToolbarAddView(tb: ToolbarPtr; url: AnsiString;						itemLabel, shortHelp: AnsiString; isDefault, isPermanent: Boolean;						viewCreationCallback: ToolbarItemNSViewCreatorProc;						customData: Pointer; setAction: Boolean); overload;begin	ToolbarAddView(tb, StringToNSString(url),						StringToNSString(itemLabel), StringToNSString(shortHelp), isDefault, isPermanent,						viewCreationCallback, customData, setAction);end;// How about menus? Why would ToolbarManager care?(*procedure ToolbarAddMenu(tb: ToolbarPtr; url: NSString; theView: NSView; theMenu: NSMenu;						itemLabel, shortHelp, longHelp: NSString; isDefault, isPermanent: Boolean;						{messageTarget: id;} message: AnsiString); overload;begin	tb.allowedItemsArray.addObject(id(url));	if isDefault then		tb.defaultItemsArray.addObject(url);Create an NSPopUpButton		addToolbarItem(tb.toolbarItems, url, itemLabel, itemLabel{paletteLabel}, shortHelp,					nil {id(messageTarget)}, sel_registerName(PChar('setView:')), theView,					sel_registerName(PChar(message)), theMenu);end;*)procedure ToolbarCreate(tb: ToolbarPtr; theWindow: NSWindow);begin	theWindow.setToolbar(tb);end;end.