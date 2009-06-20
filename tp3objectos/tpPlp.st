<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of March 3, 2008 on June 20, 2009 at 3:48:38 pm</time-stamp>
<!-- Package Zplp* -->


<class>
<name>NeneDePrimaria</name>
<environment>Smalltalk</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>Zplp</package>
</attributes>
</class>

<comment>
<class-id>NeneDePrimaria</class-id>
<body>NeneDePrimaria has nt been commented.  The comment should state the purpose of the class and also explain any unobvious aspects of the implementation.

</body>
</comment>

<class>
<name>SelectorWrapper</name>
<environment>Smalltalk</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>Zplp</package>
</attributes>
</class>

<comment>
<class-id>SelectorWrapper</class-id>
<body>StringWrapper has not been commente.  The comment should state the purpose of the class and also explain any unobvious aspects of the implementation.

</body>
</comment>

<class>
<name>BackupCell</name>
<environment>Smalltalk</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>value backup </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>Zplp</package>
</attributes>
</class>

<class>
<name>SpecObjectWrapper</name>
<environment>Smalltalk</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>wrappedObject </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>Zplp</package>
</attributes>
</class>

<comment>
<class-id>SpecObjectWrapper</class-id>
<body>SpecObjectWrapper has not been commente.  The comment should state the purpose of the class and also explain any unobvious aspects of the implementation.

Instance Variables:
	wrappedObject	&lt;Object&gt;	description of wrappedObject

</body>
</comment>





<methods>
<class-id>SelectorWrapper class</class-id> <category>instance creation</category>

<body package="Zplp" selector="new">new
	"Answer a newly created and initialized instance."

	^super new initialize</body>
</methods>

<methods>
<class-id>SelectorWrapper class</class-id> <category>converting</category>

<body package="Zplp" selector="haceAfter:">haceAfter: aSelector
	
	| f aString|
	aString := aSelector asString.
	(aString at: aString size) = (':' at: 1)
		ifTrue: [f := 'withResult:on:']
		ifFalse: [f := 'WithResult:on:'].
	^('after' , (SelectorWrapper firstUpper: aString) , f) asSymbol</body>

<body package="Zplp" selector="haceBefore:">haceBefore: aSelector
	
	| aString |
	aString := aSelector asString.
	^('before' , (SelectorWrapper firstUpper: aString)) asSymbol</body>

<body package="Zplp" selector="firstUpper:">firstUpper: aString
	
	| newString |
	newString := String new: aString size.
	newString
		at: 1
		put: (aString at: 1) asUppercase.
	1
		to: aString size - 1
		do:
			[:i | 
			newString
				at: i + 1
				put: (aString at: i + 1)].
	^newString</body>
</methods>


<methods>
<class-id>SelectorWrapper</class-id> <category>initialize-release</category>

<body package="Zplp" selector="initialize">initialize
	"Initialize a newly created instance. This method must answer the receiver."

	" *** Replace this comment with the appropriate initialization code *** "
	^self</body>
</methods>


<methods>
<class-id>NeneDePrimaria class</class-id> <category>instance creation</category>

<body package="Zplp" selector="new">new
	"Answer a newly created and initialized instance."

	^super new initialize</body>
</methods>


<methods>
<class-id>NeneDePrimaria</class-id> <category>initialize-release</category>

<body package="Zplp" selector="beforeHacer:mas:">beforeHacer: x mas: y
	
	^x &gt; 0 &amp; (y &gt; 0)</body>

<body package="Zplp" selector="initialize">initialize
	"Initialize a newly created instance. This method must answer the receiver."

	" *** Replace this comment with the appropriate initialization code *** "
	^self</body>

<body package="Zplp" selector="hacer:mas:">hacer: x mas: y
	
	^x + y</body>
</methods>


<methods>
<class-id>BackupCell</class-id> <category>initialize-release</category>

<body package="Zplp" selector="initialize">initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Edit the following to properly initialize instance variables ***"
	value := nil.
	backup := nil.
	" *** And replace this comment with additional initialization code *** "
	^self</body>
</methods>

<methods>
<class-id>BackupCell</class-id> <category>accessing</category>

<body package="Zplp" selector="value:">value: anObject
	
	backup := value.
	value := anObject</body>

<body package="Zplp" selector="value">value
	^value</body>

<body package="Zplp" selector="restore">restore
	
	value := backup.
	backup := nil</body>
</methods>

<methods>
<class-id>BackupCell</class-id> <category>As yet unclassified</category>

<body package="Zplp" selector="beforeRestore">beforeRestore
	
	^backup ~= nil</body>

<body package="Zplp" selector="afterValue:withResult:on:">afterValue: aValue withResult: res on: old
	
	^value = aValue &amp; (backup = (old at: 'value'))</body>
</methods>


<methods>
<class-id>SpecObjectWrapper class</class-id> <category>instance creation</category>

<body package="Zplp" selector="wrap:">wrap: anObject
	
	^super new wrapObj: anObject</body>
</methods>


<methods>
<class-id>SpecObjectWrapper</class-id> <category>initialize-release</category>

<body package="Zplp" selector="wrapObj:">wrapObj: anObject
	
	wrappedObject := anObject</body>
</methods>

<methods>
<class-id>SpecObjectWrapper</class-id> <category>error handling</category>

<body package="Zplp" selector="doesNotUnderstand:">doesNotUnderstand: aMessage
	
	| old res |
	self checkearPre: aMessage.
	old := self haceOld.
	res := wrappedObject
		perform: aMessage selector
		withArguments: aMessage arguments.
	self checkearPost: aMessage res: res old: old.
	res = wrappedObject ifFalse: [^res]</body>
</methods>

<methods>
<class-id>SpecObjectWrapper</class-id> <category>private</category>

<body package="Zplp" selector="haceOld">haceOld
	
	| vars old |
	vars := wrappedObject class allInstVarNames.
	old := Dictionary new.
	1
		to: vars size
		do:
			[:i | 
			old
				at: (vars at: i)
				put: (wrappedObject instVarAt: i)].
	^old</body>

<body package="Zplp" selector="checkearPre:">checkearPre: aMessage
	
	(wrappedObject respondsTo: (SelectorWrapper haceBefore: aMessage selector))
		ifTrue: [self llamarPre: aMessage]</body>

<body package="Zplp" selector="checkearPost:res:old:">checkearPost: aMessage res: res old: old
	
	(wrappedObject respondsTo: (SelectorWrapper haceAfter: aMessage selector))
		ifTrue: [self llamarPost: aMessage res: res old: old]</body>

<body package="Zplp" selector="llamarPost:res:old:">llamarPost: aMessage res: res old: old
	
	| args s |
	args := aMessage arguments.
	s := args size.
	args changeSizeTo: s + 2.
	args
		at: s + 1
		put: res.
	args
		at: s + 2
		put: old.
	(wrappedObject
		perform: (SelectorWrapper haceAfter: aMessage selector)
		withArguments: args)
		ifFalse:
			[(Error new)
				errorString: 'No se cumple la postcondicion';
				raise]</body>

<body package="Zplp" selector="llamarPre:">llamarPre: aMessage
	
	(wrappedObject
		perform: (SelectorWrapper haceBefore: aMessage selector)
		withArguments: aMessage arguments)
		ifFalse:
			[(Error new)
				errorString: 'No se cumple la precondicion';
				raise]</body>
</methods>



</st-source>
