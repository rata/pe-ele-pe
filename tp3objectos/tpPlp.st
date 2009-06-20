<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of March 3, 2008 on June 19, 2009 at 5:41:11 pm</time-stamp>
<!-- Package tpPlp* -->


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
<package>tpPlp</package>
</attributes>
</class>

<comment>
<class-id>SpecObjectWrapper</class-id>
<body>SpecObjectWrapper has not been commente.  The comment should state the purpose of the class and also explain any unobvious aspects of the implementation.

Instance Variables:
	wrappedObject	&lt;Object&gt;	description of wrappedObject

</body>
</comment>

<class>
<name>BackUpCell</name>
<environment>Smalltalk</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>value backup </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>tpPlp</package>
</attributes>
</class>

<class>
<name>StringWrapper</name>
<environment>Smalltalk</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>tpPlp</package>
</attributes>
</class>

<comment>
<class-id>StringWrapper</class-id>
<body>StringWrapper has not been commente.  The comment should state the purpose of the class and also explain any unobvious aspects of the implementation.

</body>
</comment>

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
<package>tpPlp</package>
</attributes>
</class>





<methods>
<class-id>StringWrapper class</class-id> <category>instance creation</category>

<body package="tpPlp" selector="new">new
	"Answer a newly created and initialized instance."

	^super new initialize</body>
</methods>

<methods>
<class-id>StringWrapper class</class-id> <category>converting</category>

<body package="tpPlp" selector="firstUpper:">firstUpper: aString
	
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
<class-id>StringWrapper</class-id> <category>initialize-release</category>

<body package="tpPlp" selector="initialize">initialize
	"Initialize a newly created instance. This method must answer the receiver."

	" *** Replace this comment with the appropriate initialization code *** "
	^self</body>
</methods>


<methods>
<class-id>NeneDePrimaria class</class-id> <category>instance creation</category>

<body package="tpPlp" selector="new">new
	"Answer a newly created and initialized instance."

	^super new initialize</body>
</methods>


<methods>
<class-id>NeneDePrimaria</class-id> <category>initialize-release</category>

<body package="tpPlp" selector="initialize">initialize
	"Initialize a newly created instance. This method must answer the receiver."

	" *** Replace this comment with the appropriate initialization code *** "
	^self</body>

<body package="tpPlp" selector="hacer:mas:">hacer: x mas: y
	
	^x + y</body>

<body package="tpPlp" selector="beforeHacer:mas:">beforeHacer: x mas: y
	
	^x &gt; 0 &amp; (y &gt; 0)</body>
</methods>


<methods>
<class-id>SpecObjectWrapper class</class-id> <category>instance creation</category>

<body package="tpPlp" selector="wrap:">wrap: anObject
	
	^super new wrapObj: anObject</body>
</methods>


<methods>
<class-id>SpecObjectWrapper</class-id> <category>error handling</category>

<body package="tpPlp" selector="doesNotUnderstand:">doesNotUnderstand: aMessage
	
	^wrappedObject
		perform: aMessage selector
		withArguments: aMessage arguments</body>
</methods>

<methods>
<class-id>SpecObjectWrapper</class-id> <category>initialize-release</category>

<body package="tpPlp" selector="wrapObj:">wrapObj: anObject
	
	wrappedObject := anObject</body>
</methods>


<methods>
<class-id>BackUpCell</class-id> <category>initialize-release</category>

<body package="tpPlp" selector="initialize">initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Edit the following to properly initialize instance variables ***"
	value := nil.
	backup := nil.
	" *** And replace this comment with additional initialization code *** "
	^self</body>
</methods>

<methods>
<class-id>BackUpCell</class-id> <category>accessing</category>

<body package="tpPlp" selector="restore">restore
	
	value := backup.
	backup := nil</body>

<body package="tpPlp" selector="value:">value: anObject
	
	backup := value.
	value := anObject</body>

<body package="tpPlp" selector="value">value
	^value</body>
</methods>

<methods>
<class-id>BackUpCell</class-id> <category>As yet unclassified</category>

<body package="tpPlp" selector="beforeRestore">beforeRestore
	
	^backup ~= nil</body>

<body package="tpPlp" selector="afterValue:withResult:on:">afterValue: aValue withResult: res on: old
	
	^value == aValue &amp; (backup == old instVarAt: 1)</body>
</methods>



</st-source>
