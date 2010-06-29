

	var sasaction = "/STP/guest";
	var sasprogram = "/WebServer/Public/main";

	function config()
	{
		for(i=0;i<document.forms.length;i++)
		{
			document.forms[i].action=sasaction;
			document.forms[i]._PROGRAM.value=sasprogram;					
		}
	}
