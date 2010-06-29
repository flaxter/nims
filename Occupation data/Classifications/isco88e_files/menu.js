
	/*
	* Montre / Cache un div
	*/
	function DivStatus( nom, numero )
		{
			var divID = nom + numero;
			if ( document.getElementById && document.getElementById( divID ) ) // Pour les navigateurs r�cents
				{
					Pdiv = document.getElementById( divID );
					PcH = true;
		 		}
			else if ( document.all && document.all[ divID ] ) // Pour les veilles versions
				{
					Pdiv = document.all[ divID ];
					PcH = true;
				}
			else if ( document.layers && document.layers[ divID ] ) // Pour les tr�s veilles versions
				{
					Pdiv = document.layers[ divID ];
					PcH = true;
				}
			else
				{
					
					PcH = false;
				}
			if ( PcH )
				{
					Pdiv.className = ( Pdiv.className == 'cachediv' ) ? '' : 'cachediv';
				}
		}
		
	/*
	* Cache tous les divs ayant le m�me pr�fixe
	*/
	function CacheTout( nom )
		{	
			var NumDiv = 1;
			if ( document.getElementById ) // Pour les navigateurs r�cents
				{
					while ( document.getElementById( nom + NumDiv) )
						{
							SetDiv = document.getElementById( nom + NumDiv );
							if ( SetDiv && SetDiv.className != 'cachediv' )
								{
									DivStatus( nom, NumDiv );
								}
							NumDiv++;
						}
				}
			else if ( document.all ) // Pour les veilles versions
				{
					while ( document.all[ nom + NumDiv ] )
						{
							SetDiv = document.all[ nom + NumDiv ];
							if ( SetDiv && SetDiv.className != 'cachediv' )
								{
									DivStatus( nom, NumDiv );
								}
							NumDiv++;
						}
				}
			else if ( document.layers ) // Pour les tr�s veilles versions
				{
					while ( document.layers[ nom + NumDiv ] )
						{
							SetDiv = document.layers[ nom + NumDiv ];
							if ( SetDiv && SetDiv.className != 'cachediv' )
								{
									DivStatus( nom, NumDiv );
								}
							NumDiv++;
						}
				}
		}



