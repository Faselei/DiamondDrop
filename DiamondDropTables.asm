;DiamondDropTables
;===============================================================================
;InactiveColorPallet for Load Color Attack colors subroutine
;===============================================================================
InactiveColorPallet:
;~~~Set 0~~~~~~~~~~~
	.byte $52	;ScoreBackgroundColor
	.byte $1c	;PFScoreColor
	.byte $0e	;ScoreColor
	.byte $00	;COLUBK
	.byte $52	;COLUPF
	.byte $42	;COLUP1
	.byte $1c	;COLUP0
;~~~Set 1~~~~~~~~~~~
	.byte $b8	;ScoreBackgroundColor
	.byte $1c	;PFScoreColor
	.byte $0e	;ScoreColor
	.byte $fe	;COLUBK
	.byte $B8	;COLUPF
	.byte $38	;COLUP1
	.byte $78	;COLUP0
;~~~Set 2~~~~~~~~~~~
	.byte $B6	;ScoreBackgroundColor
	.byte $18	;PFScoreColor
	.byte $0c	;ScoreColor
	.byte $fc	;COLUBK
	.byte $B6	;COLUPF
	.byte $36	;COLUP1
	.byte $76	;COLUP0
;~~~Set 3~~~~~~~~~~~
	.byte $B4	;ScoreBackgroundColor
	.byte $16	;PFScoreColor
	.byte $0a	;ScoreColor
	.byte $fa	;COLUBK
	.byte $B4	;COLUPF
	.byte $34	;COLUP1
	.byte $74	;COLUP0
;~~~Set 4~~~~~~~~~~~
	.byte $B2	;ScoreBackgroundColor
	.byte $14	;PFScoreColor
	.byte $08	;ScoreColor
	.byte $f8	;COLUBK
	.byte $B2	;COLUPF
	.byte $32	;COLUP1
	.byte $72	;COLUP0
;~~~Set 5~~~~~~~~~~~
	.byte $B1	;ScoreBackgroundColor
	.byte $12	;PFScoreColor
	.byte $06	;ScoreColor
	.byte $f6	;COLUBK
	.byte $B1	;COLUPF
	.byte $31	;COLUP1
	.byte $71	;COLUP0
;~~~Set 6~~~~B/W~~~~~
	.byte $04	;ScoreBackgroundColor
	.byte $0e	;PFScoreColor
	.byte $0e	;ScoreColor
	.byte $0e	;COLUBK
	.byte $04	;COLUPF
	.byte $08	;COLUP1
	.byte $00	;COLUP0
;~~~Set 7~~~~~~~~~~~
	.byte $06	;ScoreBackgroundColor
	.byte $0e	;PFScoreColor
	.byte $0e	;ScoreColor
	.byte $0d	;COLUBK
	.byte $06	;COLUPF
	.byte $0a	;COLUP1
	.byte $02	;COLUP0
;~~~Set 8~~~~~~~~~~~
	.byte $00	;ScoreBackgroundColor
	.byte $0e	;PFScoreColor
	.byte $0e	;ScoreColor
	.byte $0b	;COLUBK
	.byte $00	;COLUPF
	.byte $0c	;COLUP1
	.byte $04	;COLUP0
;~~~Set 9~~~~~~~~~~~
	.byte $02	;ScoreBackgroundColor
	.byte $0e	;PFScoreColor
	.byte $0e	;ScoreColor
	.byte $09	;COLUBK
	.byte $02	;COLUPF
	.byte $0e	;COLUP1
	.byte $06	;COLUP0
;~~~Set 10~~~~~~~~~~~
	.byte $0c	;ScoreBackgroundColor
	.byte $0e	;PFScoreColor
	.byte $0e	;ScoreColor
	.byte $07	;COLUBK
	.byte $0c	;COLUPF
	.byte $00	;COLUP1
	.byte $08	;COLUP0
;~~~Set 11~~~~~~~~~~~
	.byte $0e	;ScoreBackgroundColor
	.byte $00	;PFScoreColor
	.byte $00	;ScoreColor
	.byte $05	;COLUBK
	.byte $0e	;COLUPF
	.byte $02	;COLUP1
	.byte $0a	;COLUP0
;===============================================================================
; PlayerXY cord
;===============================================================================
PlayerXpos:
PlayerXOrigin = $4c
	.byte (PlayerXOrigin)
	.byte (PlayerXOrigin-1)
	.byte (PlayerXOrigin-2)
	.byte (PlayerXOrigin-3)
	.byte (PlayerXOrigin-4)
	.byte (PlayerXOrigin-5)
	.byte (PlayerXOrigin-6)
	.byte (PlayerXOrigin-7)
	.byte (PlayerXOrigin-8)
	.byte (PlayerXOrigin-9)
	.byte (PlayerXOrigin-10)
	.byte (PlayerXOrigin-11)
	.byte (PlayerXOrigin-12)
	.byte (PlayerXOrigin-13)
	.byte (PlayerXOrigin-14)
	.byte (PlayerXOrigin-15)
	.byte (PlayerXOrigin-16)
	.byte (PlayerXOrigin-17)
	.byte (PlayerXOrigin-18)
	.byte (PlayerXOrigin-19)
	.byte (PlayerXOrigin-20)
	.byte (PlayerXOrigin-21)
	.byte (PlayerXOrigin-22)
	.byte (PlayerXOrigin-23)
	.byte (PlayerXOrigin-24)
	.byte (PlayerXOrigin-23)
	.byte (PlayerXOrigin-22)
	.byte (PlayerXOrigin-21)
	.byte (PlayerXOrigin-20)
	.byte (PlayerXOrigin-19)
	.byte (PlayerXOrigin-18)
	.byte (PlayerXOrigin-17)
	.byte (PlayerXOrigin-16)
	.byte (PlayerXOrigin-15)
	.byte (PlayerXOrigin-14)
	.byte (PlayerXOrigin-13)
	.byte (PlayerXOrigin-12)
	.byte (PlayerXOrigin-11)
	.byte (PlayerXOrigin-10)
	.byte (PlayerXOrigin-9)
	.byte (PlayerXOrigin-8)
	.byte (PlayerXOrigin-7)
	.byte (PlayerXOrigin-6)
	.byte (PlayerXOrigin-5)
	.byte (PlayerXOrigin-4)
	.byte (PlayerXOrigin-3)
	.byte (PlayerXOrigin-2)
	.byte (PlayerXOrigin-1)
	.byte (PlayerXOrigin)
	.byte (PlayerXOrigin+1)
	.byte (PlayerXOrigin+2)
	.byte (PlayerXOrigin+3)
	.byte (PlayerXOrigin+4)
	.byte (PlayerXOrigin+5)
	.byte (PlayerXOrigin+6)
	.byte (PlayerXOrigin+7)
	.byte (PlayerXOrigin+8)
	.byte (PlayerXOrigin+9)
	.byte (PlayerXOrigin+10)
	.byte (PlayerXOrigin+11)
	.byte (PlayerXOrigin+12)
	.byte (PlayerXOrigin+13)
	.byte (PlayerXOrigin+14)
	.byte (PlayerXOrigin+15)
	.byte (PlayerXOrigin+16)
	.byte (PlayerXOrigin+17)
	.byte (PlayerXOrigin+18)
	.byte (PlayerXOrigin+19)
	.byte (PlayerXOrigin+20)
	.byte (PlayerXOrigin+21)
	.byte (PlayerXOrigin+22)
	.byte (PlayerXOrigin+23)
	.byte (PlayerXOrigin+24)
	.byte (PlayerXOrigin+23)
	.byte (PlayerXOrigin+22)
	.byte (PlayerXOrigin+21)
	.byte (PlayerXOrigin+20)
	.byte (PlayerXOrigin+19)
	.byte (PlayerXOrigin+18)
	.byte (PlayerXOrigin+17)
	.byte (PlayerXOrigin+16)
	.byte (PlayerXOrigin+15)
	.byte (PlayerXOrigin+14)
	.byte (PlayerXOrigin+13)
	.byte (PlayerXOrigin+12)
	.byte (PlayerXOrigin+11)
	.byte (PlayerXOrigin+10)
	.byte (PlayerXOrigin+9)
	.byte (PlayerXOrigin+8)
	.byte (PlayerXOrigin+7)
	.byte (PlayerXOrigin+6)
	.byte (PlayerXOrigin+5)
	.byte (PlayerXOrigin+4)
	.byte (PlayerXOrigin+3)
	.byte (PlayerXOrigin+2)
	.byte (PlayerXOrigin+1)
	.byte (PlayerXOrigin)
	.byte (PlayerXOrigin-1)
	.byte (PlayerXOrigin-2)
	.byte (PlayerXOrigin-3)
	.byte (PlayerXOrigin-4)
	.byte (PlayerXOrigin-5)
	.byte (PlayerXOrigin-6)
	.byte (PlayerXOrigin-7)
	.byte (PlayerXOrigin-8)
	.byte (PlayerXOrigin-9)
	.byte (PlayerXOrigin-10)
	.byte (PlayerXOrigin-11)
	.byte (PlayerXOrigin-12)
	.byte (PlayerXOrigin-13)
	.byte (PlayerXOrigin-14)
	.byte (PlayerXOrigin-15)
	.byte (PlayerXOrigin-16)
	.byte (PlayerXOrigin-17)
	.byte (PlayerXOrigin-18)
	.byte (PlayerXOrigin-19)
	.byte (PlayerXOrigin-20)
	.byte (PlayerXOrigin-21)
	.byte (PlayerXOrigin-22)
	.byte (PlayerXOrigin-23)
	.byte (PlayerXOrigin-24)
	.byte (PlayerXOrigin-23)
	.byte (PlayerXOrigin-22)
	.byte (PlayerXOrigin-21)
	.byte (PlayerXOrigin-20)
	.byte (PlayerXOrigin-19)
	.byte (PlayerXOrigin-18)
	.byte (PlayerXOrigin-17)
	.byte (PlayerXOrigin-16)
	.byte (PlayerXOrigin-15)
	.byte (PlayerXOrigin-14)
	.byte (PlayerXOrigin-13)
	.byte (PlayerXOrigin-12)
	.byte (PlayerXOrigin-11)
	.byte (PlayerXOrigin-10)
	.byte (PlayerXOrigin-9)
	.byte (PlayerXOrigin-8)
	.byte (PlayerXOrigin-7)
	.byte (PlayerXOrigin-6)
	.byte (PlayerXOrigin-5)
	.byte (PlayerXOrigin-4)
	.byte (PlayerXOrigin-3)
	.byte (PlayerXOrigin-2)
	.byte (PlayerXOrigin-1)
	.byte (PlayerXOrigin)
PlayerYpos:
PlayerYOrigin = $53
	.byte (PlayerYOrigin)
	.byte (PlayerYOrigin-1)
	.byte (PlayerYOrigin-2)
	.byte (PlayerYOrigin-3)
	.byte (PlayerYOrigin-4)
	.byte (PlayerYOrigin-5)
	.byte (PlayerYOrigin-6)
	.byte (PlayerYOrigin-7)
	.byte (PlayerYOrigin-8)
	.byte (PlayerYOrigin-9)
	.byte (PlayerYOrigin-10)
	.byte (PlayerYOrigin-11)
	.byte (PlayerYOrigin-12)
	.byte (PlayerYOrigin-13)
	.byte (PlayerYOrigin-14)
	.byte (PlayerYOrigin-15)
	.byte (PlayerYOrigin-16)
	.byte (PlayerYOrigin-17)
	.byte (PlayerYOrigin-18)
	.byte (PlayerYOrigin-19)
	.byte (PlayerYOrigin-20)
	.byte (PlayerYOrigin-21)
	.byte (PlayerYOrigin-22)
	.byte (PlayerYOrigin-23)
	.byte (PlayerYOrigin-24)
	.byte (PlayerYOrigin-25)
	.byte (PlayerYOrigin-26)
	.byte (PlayerYOrigin-27)
	.byte (PlayerYOrigin-28)
	.byte (PlayerYOrigin-29)
	.byte (PlayerYOrigin-30)
	.byte (PlayerYOrigin-31)
	.byte (PlayerYOrigin-32)
	.byte (PlayerYOrigin-33)
	.byte (PlayerYOrigin-34)
	.byte (PlayerYOrigin-35)
	.byte (PlayerYOrigin-36)
	.byte (PlayerYOrigin-37)
	.byte (PlayerYOrigin-38)
	.byte (PlayerYOrigin-39)
	.byte (PlayerYOrigin-40)
	.byte (PlayerYOrigin-41)
	.byte (PlayerYOrigin-42)
	.byte (PlayerYOrigin-43)
	.byte (PlayerYOrigin-44)
	.byte (PlayerYOrigin-45)
	.byte (PlayerYOrigin-46)
	.byte (PlayerYOrigin-47)
	.byte (PlayerYOrigin-48)
	.byte (PlayerYOrigin-47)
	.byte (PlayerYOrigin-46)
	.byte (PlayerYOrigin-45)
	.byte (PlayerYOrigin-44)
	.byte (PlayerYOrigin-43)
	.byte (PlayerYOrigin-42)
	.byte (PlayerYOrigin-41)
	.byte (PlayerYOrigin-40)
	.byte (PlayerYOrigin-39)
	.byte (PlayerYOrigin-38)
	.byte (PlayerYOrigin-37)
	.byte (PlayerYOrigin-36)
	.byte (PlayerYOrigin-35)
	.byte (PlayerYOrigin-34)
	.byte (PlayerYOrigin-33)
	.byte (PlayerYOrigin-32)
	.byte (PlayerYOrigin-31)
	.byte (PlayerYOrigin-30)
	.byte (PlayerYOrigin-29)
	.byte (PlayerYOrigin-28)
	.byte (PlayerYOrigin-27)
	.byte (PlayerYOrigin-26)
	.byte (PlayerYOrigin-25)
	.byte (PlayerYOrigin-24)
	.byte (PlayerYOrigin-23)
	.byte (PlayerYOrigin-22)
	.byte (PlayerYOrigin-21)
	.byte (PlayerYOrigin-20)
	.byte (PlayerYOrigin-19)
	.byte (PlayerYOrigin-18)
	.byte (PlayerYOrigin-17)
	.byte (PlayerYOrigin-16)
	.byte (PlayerYOrigin-15)
	.byte (PlayerYOrigin-14)
	.byte (PlayerYOrigin-13)
	.byte (PlayerYOrigin-12)
	.byte (PlayerYOrigin-11)
	.byte (PlayerYOrigin-10)
	.byte (PlayerYOrigin-9)
	.byte (PlayerYOrigin-8)
	.byte (PlayerYOrigin-7)
	.byte (PlayerYOrigin-6)
	.byte (PlayerYOrigin-5)
	.byte (PlayerYOrigin-4)
	.byte (PlayerYOrigin-3)
	.byte (PlayerYOrigin-2)
	.byte (PlayerYOrigin-1)
	.byte (PlayerYOrigin)
	.byte (PlayerYOrigin-1)
	.byte (PlayerYOrigin-2)
	.byte (PlayerYOrigin-3)
	.byte (PlayerYOrigin-4)
	.byte (PlayerYOrigin-5)
	.byte (PlayerYOrigin-6)
	.byte (PlayerYOrigin-7)
	.byte (PlayerYOrigin-8)
	.byte (PlayerYOrigin-9)
	.byte (PlayerYOrigin-10)
	.byte (PlayerYOrigin-11)
	.byte (PlayerYOrigin-12)
	.byte (PlayerYOrigin-13)
	.byte (PlayerYOrigin-14)
	.byte (PlayerYOrigin-15)
	.byte (PlayerYOrigin-16)
	.byte (PlayerYOrigin-17)
	.byte (PlayerYOrigin-18)
	.byte (PlayerYOrigin-19)
	.byte (PlayerYOrigin-20)
	.byte (PlayerYOrigin-21)
	.byte (PlayerYOrigin-22)
	.byte (PlayerYOrigin-23)
	.byte (PlayerYOrigin-24)
	.byte (PlayerYOrigin-25)
	.byte (PlayerYOrigin-26)
	.byte (PlayerYOrigin-27)
	.byte (PlayerYOrigin-28)
	.byte (PlayerYOrigin-29)
	.byte (PlayerYOrigin-30)
	.byte (PlayerYOrigin-31)
	.byte (PlayerYOrigin-32)
	.byte (PlayerYOrigin-33)
	.byte (PlayerYOrigin-34)
	.byte (PlayerYOrigin-35)
	.byte (PlayerYOrigin-36)
	.byte (PlayerYOrigin-37)
	.byte (PlayerYOrigin-38)
	.byte (PlayerYOrigin-39)
	.byte (PlayerYOrigin-40)
	.byte (PlayerYOrigin-41)
	.byte (PlayerYOrigin-42)
	.byte (PlayerYOrigin-43)
	.byte (PlayerYOrigin-44)
	.byte (PlayerYOrigin-45)
	.byte (PlayerYOrigin-46)
	.byte (PlayerYOrigin-47)
	.byte (PlayerYOrigin-48)
;===============================================================================
;Vram Word Table
;===============================================================================
; Vram Data is stored in this table in the format .word %0000000000000000
; remember that the right 8-bits are displayed  with bit 0 being the 
; far right, bit 7 as the middle right.
;-------------------------------------------------------------------------------
VramData: ;0123456776543210

	.word %0111011101110111
	.word %1111100111111001
	.word %0011111111111111
	.word %1110001111000111
	.word %1000000111111100
	.word %0111101000111101
	.word %1100101001100101
	.word %0100001101000011
	.word %0100011000100011
	.word %1000000010000000
	.word %1010010111100110
	.word %1000100001000100
	.word %0010001000010001
	.word %0010111110100011
	.word %1000100111100000
	.word %0111100000111100
	.word %0000111100001111
	.word %1100001001100001
	.word %1000010010000010
	.word %0010000110100100
	.word %0101001000101001
	.word %1010100001010000
	.word %0010100000010100
	.word %1001100111111000
	.word %1111000011110000
	.word %1111111111100000
	.word %0101010110011110
	.word %0100111110010011
	.word %1111110111001010
	.word %0110001010000110
	.word %0100001110010101
	.word %0011001110101101
	.word %1001001111111101
	.word %1110010111000110
	.word %1000010001000010
	.word %1001101001001101
	.word %0110011110000111
	.word %1000000110000001
	.word %1111111111001111
	.word %0111111101111111
	.word %0000000000000000
	.word %1111111111100000
	.word %0011110000011110
	.word %0000111110110011
	.word %1110110111000010
	.word %0110000110000100
	.word %0100001000100001
	.word %1010010001010010
	.word %1111001111110011
	.word %1100110111010010
	.word %0100110110010010
	.word %0110011110000111
	.word %1001010001001010
	.word %0010010110100110
	.word %0111011101110111
	.word %1111100111111001
	.word %0011111111111111
	.word %1110001111000111
	.word %1000000111111100
	.word %0111101000111101
	.word %1100101001100101
	.word %0100001101000011
	.word %0100011000100011
	.word %1000000010000000
	.word %1010010111100110
	.word %1000100001000100
	.word %0010001000010001
	.word %0010111110100011
	.word %1000100111100000
	.word %0111100000111100
	.word %0000111100001111
	.word %1100001001100001
	.word %1000010010000010
	.word %0010000110100100
	.word %0101001000101001
	.word %1010100001010000
	.word %0010100000010100
	.word %1001100111111000
	.word %1111000011110000
	.word %1111111111100000
	.word %0101010110011110
	.word %0100111110010011
	.word %1111110111001010
	.word %0110001010000110
	.word %0100001110010101
	.word %0111011101110111
	.word %1111100111111001
	.word %0011111111111111
	.word %1110001111000111
	.word %1000000111111100
	.word %0111101000111101
	.word %1100101001100101
	.word %0100001101000011
	.word %0100011000100011
	.word %1000000010000000
	.word %1010010111100110
	.word %1000100001000100
	.word %0010001000010001
	.word %0010111110100011
	.word %1000100111100000
	.word %0111100000111100
	.word %0000111100001111
	.word %1100001001100001
	.word %1000010010000010
	.word %0010000110100100
	.word %0101001000101001
	.word %1111001111110011
	.word %1100110111010010
	.word %0100110110010010
	.word %0110011110000111
	.word %1001010001001010
	.word %0010010110100110
	.word %0111011101110111
	.word %1111100111111001
	.word %0011111111111111
	.word %0010001000010001
	.word %0010111110100011
	.word %1000100111100000
	.word %0111100000111100
	.word %0000111100001111
	.word %1100001001100001
	.word %1000010010000010
	.word %0010000110100100
	.word %0101001000101001
	.word %1010100001010000
	.word %0010100000010100
	.word %1001100111111000
	.word %1111000011110000
;===============================================================================
;
;===============================================================================
;
;-------------------------------------------------------------------------------