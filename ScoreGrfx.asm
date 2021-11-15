; Hex Numbers can be turned off from the compiler
; by adding the constant: NoHexNumbers
Z.NoHexNumbers

ScoreGrfx:
	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01000110 ;| X   XX |02|
	.byte %01000110 ;| X   XX |03|
	.byte %01000110 ;| X    X |04|
	.byte %01000010 ;| X    X |05|
	.byte %01000010 ;| X    X |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %00011000 ;|    XX  |01|
	.byte %00011000 ;|    XX  |02|
	.byte %00011000 ;|    XX  |03|
	.byte %00010000 ;|    X   |04|
	.byte %00010000 ;|    X   |05|
	.byte %00110000 ;|   XX   |06|
	.byte %00010000 ;|    X   |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01100110 ;| XX  XX |02|
	.byte %01100000 ;| XX     |03|
	.byte %01000000 ;| X      |04|
	.byte %01111110 ;| XXXXXX |05|
	.byte %00000010 ;|      X |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01100110 ;| XX  XX |02|
	.byte %00000110 ;|     XX |03|
	.byte %00000010 ;|      X |04|
	.byte %00011110 ;|   XXXX |05|
	.byte %00000010 ;|      X |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %00000110 ;|     XX |01|
	.byte %00000110 ;|     XX |02|
	.byte %00000100 ;|     X  |03|
	.byte %01111110 ;| XXXXXX |04|
	.byte %01000100 ;| X   X  |05|
	.byte %01000100 ;| X   X  |06|
	.byte %01000100 ;| X   X  |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01100110 ;| XX  XX |02|
	.byte %00000110 ;|     XX |03|
	.byte %00000010 ;|      X |04|
	.byte %01111110 ;| XXXXXX |05|
	.byte %01000000 ;| X      |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01000110 ;| X   XX |02|
	.byte %01000110 ;| X   XX |03|
	.byte %01000010 ;| X    X |04|
	.byte %01111110 ;| XXXXXX |05|
	.byte %01000000 ;| X      |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %00011100 ;|   XXX  |01|
	.byte %00011100 ;|   XXX  |02|
	.byte %00011000 ;|   XX   |03|
	.byte %00001000 ;|    X   |04|
	.byte %00000100 ;|     X  |05|
	.byte %01100010 ;| XX   X |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01000110 ;| X   XX |02|
	.byte %01000110 ;| X   XX |03|
	.byte %01000010 ;| X    X |04|
	.byte %00111100 ;|  XXXX  |05|
	.byte %01000010 ;| X    X |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01100110 ;| XX  XX |02|
	.byte %00000110 ;|     XX |03|
	.byte %00000010 ;|      X |04|
	.byte %01111110 ;| XXXXXX |05|
	.byte %01000010 ;| X    X |06|
	.byte %01111110 ;| XXXXXX |07|

 ifnconst Z.NoHexNumbers
	
	.byte %00000000 ;|        |00|
	.byte %01100010 ;| XX   X |01|
	.byte %01100010 ;| XX   X |02|
	.byte %01100010 ;| XX   X |03|
	.byte %01000010 ;| X    X |04|
	.byte %01111110 ;| XXXXXX |05|
	.byte %01000100 ;| X   X  |06|
	.byte %01111100 ;| XXXXX  |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01100010 ;| XX   X |02|
	.byte %01100010 ;| XX   X |03|
	.byte %01000010 ;| X    X |04|
	.byte %01111110 ;| XXXXXX |05|
	.byte %01000100 ;| X   X  |06|
	.byte %01111100 ;| XXXXX  |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01100110 ;| XX  XX |02|
	.byte %01100000 ;| XX     |03|
	.byte %01000000 ;| X      |04|
	.byte %01000000 ;| X      |05|
	.byte %01000110 ;| X   XX |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %01111100 ;| XXXXX  |01|
	.byte %01100010 ;| XX   X |02|
	.byte %01100010 ;| XX   X |03|
	.byte %01000010 ;| X    X |04|
	.byte %01000010 ;| X    X |05|
	.byte %01000010 ;| X    X |06|
	.byte %01111100 ;| XXXXX  |07|

	.byte %00000000 ;|        |00|
	.byte %01111110 ;| XXXXXX |01|
	.byte %01100000 ;| XX     |02|
	.byte %01100000 ;| XX     |03|
	.byte %01000000 ;| X      |04|
	.byte %01111000 ;| XXXX   |05|
	.byte %01000000 ;| X      |06|
	.byte %01111110 ;| XXXXXX |07|

	.byte %00000000 ;|        |00|
	.byte %01100000 ;| XX     |01|
	.byte %01100000 ;| XX     |02|
	.byte %01100000 ;| XX     |03|
	.byte %01000000 ;| X      |04|
	.byte %01111100 ;| XXXXX  |05|
	.byte %01000000 ;| X      |06|
	.byte %01111110 ;| XXXXXX |07|
	endif