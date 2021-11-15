;===============================================================================
;Diamond Drop
;By:Julian Zelazny
;Start: 5/16/18
;===============================================================================
;A simple 2k game where you must rotate 2 diamonds through barriers for as
;long as possible.
;-------------------------------------------------------------------------------
;Based loosely on the iOS game "Duet"
;
;Original game by:
;	Kumobius
;
;With music by:
;	Tim Shiel
;
;Atari 2600, Batari Basic port:
;	Steve Engelhardt
;
;Atari 2600, assembly port:
;	Julian Zelazny
;-------------------------------------------------------------------------------
	processor 6502
	include vcs.h			;TIA constants
	include SyntheticInstructions.h	;useful instructions
	include macro.h			;useful macros

;Rev 0.00	05/16/18,Project created
;Rev 1.00	??/??/??,finished
;Rev 1.01	08/31/18,cleaned up the code a bit total playfields 84
;Rev 1.02	10/12/18,cleaned the code more, total playfields 114
;Rev 1.03	10/16/18,added Mark Lesser's Lord of the Rings
;			,BRK Routine to save rom space
;			,Rewriting the kernel to be more efficient in resources
;			,used more advanced kernel techniques
;			,converted all clocks to use one world clock (fraim)
;			,total playfields 128 (max)
;Rev 1.04	10/23/18,changed the ballx pos from a table to an equation
;Rev 1.05	06/27/19,restructure of program to save space, 57 bytes free
;Rev 1.06	08/26/19,restructure of program to save space, 62 bytes free
;			,crunched all user code down to one file.

	seg.u Variables
	org $80
gameState	ds 1
;D7: Game active
;D6: Select Pressed
variation	ds 1
;D7:
;D6:
;D5:
;D4:
;D3:
;D2: Area Data, Set Table / Wrath of the rand
;D1: cource / smooth diamond movement
;D0: arena fall speed, slow / fast

fraim	ds 1			;master clock
diamondGrfxPointer	ds 2	;Indirect DiamondGrfx pointer
vram		ds 2		;Playfield Memory
;ObjectYpos~~~~~~~~~~
player0y	ds 1
player1y	ds 1
missile0y	ds 1
missile1y	ds 1
arenaY		ds 1
;ObjectXpos~~~~~~~~~~
player0x	ds 1
player1x	ds 1
missile0x	ds 1
missile1x	ds 1
ballx		ds 1
;Rand and temp ~~~~~~
rand		ds 1
temp1	ds 1
temp2	ds 1
temp3	ds 1
temp4	ds 1
temp5	ds 1
;score~~~~~~~~~~~~~~~
score	ds 2
scoreDigit1	ds 2
scoreDigit2	ds 2
scoreDigit3	ds 2
scoreDigit4	ds 2
scorecolor	ds 1	
pfscorecolor	ds 1
scoreBackgroundcolor	ds 1
pfscore1	ds 1
pfscore2	ds 1
;Clocks and Index~~~~
colorIndex	ds 1
missile0posIndex	ds 1
missile1posIndex	ds 1
playerIndex	ds 1
arenaClock	ds 1
bombCount	ds 1
bombSoundTimer	ds 1
;Sound~~~~~~~~~~~~~~~
SFX_LEFT	ds 1
SFX_RIGHT	ds 1
	seg
	org $f800
;===============================================================================
;Picture
;===============================================================================
;Draws the screen. This is a 2LK.
;Rev 1.00	??/??/18,Kernel created
;Rev 1.01	??/??/18,Hotfix of the exit routine
;Rev 2.00	10/16/18,Massive Kernel Overhaul to implement more advanced
;			,techniques to save ram and boost efficiency of
;			,the drawing routines
;-------------------------------------------------------------------------------
Picture: subroutine
	bit TIMINT		;4 So that we can burn the extra scanlines
	bpl Picture		;3 from the Vertical Blank	
	ldx #(170*76/64)	;2 Load the screen timer value
	stx TIM64T		;4 store in the RIOT timers
	lda #0			;2 a = 0 
	sta WSYNC		;~ Fresh Scanline
	sta VBLANK		;3 Turn the screen on
	tax			;2 Clear the X and Y index for the main kernel
	tay			;2 so we won't get a glitchy Display
	sta WSYNC		;~ Fresh Scanline, no sprite will be drawn 
				; on the first scanline of the display
DiamindDropKernel:	;writes to PF2 must be at cycle 48 for a 3 cycle store
;~~~Start of 1st Scanline~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	sty GRP0		;3 store X to the grfx registor for player 0
	sta ENAM0		;3 store A to the grfx registor for missile 0
;Draw Playfied
	lda #7			;2 load the playfield hight
	dcp temp5		;5 decrement & compare with A
	bcs DoDrawarena1	;2/3 if M < A then draw the arena
	ldy #0			;2 else draw blanks
	sty PF2			;3
	nop			;2
	beq DoNotDrawarena1	;3
DoDrawarena1:
	ldy vram		;3	load vram data to the first PF2
	sty PF2			;3
	ldy vram+1		;3	Load Y with the next PF2 write
DoNotDrawarena1:
;Drawmissile1
	lda #0			;2 load the missile hight
	dcp temp4		;5 decrement & compare with A
;	sbc #2			;2
	rol
	rol
	tax
	lda #7			;2   2
	dcp temp2		;5   7
;	nop			;2 wast 2 cycles for time 2nd PF2 write
	sty PF2			;3	store the 2nd VRAM byte to the playfield
;Draw player1
;PlayerProcessorTypeA:
;	lda #7			;2   2
;	dcp temp2		;5   7
	ldy temp2		;3   10
	bcs .DoDrawPlayer1	;2/3 12/13
	lda #0			;2   14
	.byte $2c		;4   18
.DoDrawPlayer1:
	lda (diamondGrfxPointer),y ;5   18 cycles to prep
	sta WSYNC		;~	Wait for the end of its current scanline
;~~~Start of 2nd Scanline~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	sta GRP1		;3 Store results for player 1
	stx ENAM1		;3 and missile 1
	lda #7			;2 load the playfield hight
	cmp temp5		;3 compare with A
	bcs DoDrawarena2	;2/3 if M < A then be ready to draw playfield
	ldx #0			;2 else draw blanks
	stx PF2			;3
	nop			;2
	beq DoNotDrawarena2	;3
DoDrawarena2:
	ldx vram		;3 load vram data to the first PF2
	stx PF2			;3
	ldx vram+1		;3 Load Y with the next PF2 write
DoNotDrawarena2:
;Draw player0
;A is still #7
;PlayerProcessorTypeA:
	dcp temp1		;5   7
	ldy temp1		;3   10
	bcs .DoDrawPlayer0	;2/3 12/13
	lda #0			;2   14
	.byte $2c		;4   18
.DoDrawPlayer0:
	lda (diamondGrfxPointer),y;5   18 cycles to prep
	tay			;2 move a to x (player 0 grfx)
	lda #0			;2
	nop			;2 wast 2 cycles
	stx PF2			;3 store VRAM byte to memory for the right side
;Drawmissile0
	dcp temp3		;5 decrement & compare with A
	sbc #2			;2
	jsr Sleep12		;12
	bit TIMINT		;4 bit test if the timer has expired
	bmi TimerExpired	;2
	jmp DiamindDropKernel	;3 jump back to the start of the loop
TimerExpired:
;~~~Drawportal and prep score~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	ldx #2 			;2 Load A with 2 so we can...
	stx ENABL		;3 enable the ball
	lda #0			;2 Load A with 0
	sta PF0			;3 Blank PlayField byte 0
	sta PF1			;3 Blank PlayField byte 1
	sta PF2			;3 Blank PlayField byte 2
	ldx #1			;2 Load X with 1
	stx NUSIZ0		;3 store for (Number-Size player-missile) 0 & 1
	stx NUSIZ1		;3 2 copies close
	lda #72			;2 load to A the scoreX pos for 4th digit
	ldy #64			;2 Load to Y the scoreX pos for 3rd digit
	jsr PosObjectX		;6 Set player1 grfx to the X pos in A
	tya			;2 Move Y to A
	dex			;2 X--
	ldy scorecolor		;3 Load to Y the scorecolor (White)
	sty COLUP0		;3 Store in color registers
	sty COLUP1		;3 For both player 0 & 1
	jsr PosObjectX		;6 Do 2nd jsr PosObjectX for player 0
	ldy #18			;2 Load Y with timer for 16 scanline Display
	lda pfscorecolor	;3 Load the Playfield scorecolor to A
	nop VSYNC		;2 Wast a bit of time so that the New color
				;Won't effect the Ball color
				;Ballcolor = Playfield color
	sta COLUPF		;3 Store the new color for the plafield
	stx ENABL		;3 Turn off the Ball (X = 0)
	sta WSYNC		;~ Finish Scanline so HMOVE will be off screen
	sta HMOVE		;3 Set Fine tune X pos for all grfx's
	sta WSYNC		;~ Finish Scanline to avoid HMOVE side effects
	sty TIM64T		;3 Y = #18, Store Timer for 16 Scanline Display
	lda scoreBackgroundcolor;3 Grab current Playfield color and use that
	jsr Sleep12		;12 as the new background color
	sta COLUBK		;3 Store Value as the BackGround color
	lda #0			;2 Turn off video delay for player 1
	sta VDELP1		;3 so the sprite lines up with player 0
	ldy #8			;2 8 counts of the loop
;~~~scoreDisplayLoop~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scoreLoop:
	sta WSYNC		;~ Fresh scanline
	lda pfscore1		;3 Load the 1st Playfield score byte
	sta PF1			;3 Store in Playfield byte 1
	lda (scoreDigit4),y	;5 Grab 4th digit
	sta GRP0		;3 store it in player 0
	lda (scoreDigit3),y	;5 Grab 3rd digit
	sta GRP1		;3 store it in player 1
	lax (scoreDigit1),y	;5 load 1st digit to a AND x
	lda (scoreDigit2),y	;5 load the 2nd digit to A
	jsr Sleep12		;12 Burn 12 cycles for correct Grfx stores
	sta GRP0		;3 store 2nd digit in player 0
	stx GRP1		;3 store 1st digit in player 1
	lda pfscore2		;3 load the 2nd Playfield score byte 
	sta PF1			;3 store in Playfield 1 for 2nd 1/2 of screen
	dey			;2 Y--
	bne scoreLoop		;3 Repeat Loop unless Y = 255
	sta WSYNC		;~ new scanline
	sty PF1			;3 clear out the grfx to finish the scoreDisplay
	sty NUSIZ0		;3 clear size and with for player0
	sty NUSIZ1		;3 clear size and with for player1
FinishScreenLoop:		;~ wait for the screen to end
	bit TIMINT		;4 test if the Timer Expired
	bpl FinishScreenLoop	;3 if it has not then keep testing untill true
	sta WSYNC		;~ finish scanline
	lda #$02		;2 load A with 2 to...
	sta VBLANK		;3 turn the screen off when in horizontal blank
	ldx #(31*76/64)		; load the OverScan Timer constant
	stx TIM64T		; Store to timers
	rts			;6 return from the kernel display for overscan
;===============================================================================
Start:	sei		;Disable interrupts
	cld		;Clear decimal mode
	lda #0		;Fill $0000 - $00ff with 0's
	tax		;This clears the TIA grfx chip
ClearRam:		;and the 128 bytes of ram at $0080 - $00ff
	sta $00,x	; this also sets the stack to $ff
	txs
	inx
	bne ClearRam
	jsr SystemSetup	; setup memory for the mainloop
;===============================================================================
;MainLoop
;===============================================================================
;The program "Falls in" to the main loop, the game will run itself from 
;here on out until the power is turned off.
;-------------------------------------------------------------------------------
MainLoop: subroutine
	jsr VSync	;calls a new TV fraim
;-------------------------------------------------------------------------------
;Vertical Blank, 37 scanlines before picture area
	jsr LoadColors			;loads new color data to the TIA
	jsr ProcessJoysticks		;read the joysticks and set memory
	jsr Prepscore			;prep score memory for the display
	jsr ProcessConsoleSwitches	;read the console switches, set memory 
	jsr PortalUpdate		;update the portal near bottom of screen
	jsr MissileUpdate		;update the diamond missile shape
	jsr ProcessPlayers		;check index out of bounds
	jsr PrepDisplay			;move object y pos to kernel memory
;-------------------------------------------------------------------------------
	jsr Picture			;drawing the display
;-------------------------------------------------------------------------------
;Overscan, 30 scanlines before Vertical Sync
	jsr SFX_UPDATE		;Update any Sound Effects
	jsr ProcessPlayfield	;checks if the playfield is at the portal
	jsr CheckCollision	;read any collisions of all objects
	jsr TickTimers		;tick bomb timers
;-------------------------------------------------------------------------------
	jmp MainLoop	;jump back to the start of the MainLoop
;===============================================================================
;VSync
;===============================================================================
;new TV fraims must be done by the programmer with address VSYNC.
;after 3 scanlines the TV beam has moved back to the top left corner.
;-------------------------------------------------------------------------------
VSync:	bit TIMINT	;spin the CPU to wast extra time from the RIOT chip.
	bpl VSync	;
	lda #$02	;Load A with 2 to...
	sta VSYNC	;Start VSYNC
	sta VBLANK	;Turn off the display
	sta WSYNC	;Wait for the end of the first scanline
	ldx #(40*76/64)	;Load X with the Timer constant for VSYNC & VBLANK
	stx TIM64T	;Store in timer (See MOS 6532 PIA for details)
	inc fraim	;inc the fraim counter
	stx CXCLR	;clear collision latches 
	sta WSYNC	;wait for the end of the scanline
	lsr		;shift a right so A = 1
	sta WSYNC	;wait for the end of the scanline
	sta VSYNC	;Stop VSYNC
Sleep12:		;wast time lable
	rts		;return from subroutine
;===============================================================================
;rand
;===============================================================================
; A Linear-FeedBack Shift Register will be good for our pseudo-random numbers
; OutPut: rand
; A = rand
;-------------------------------------------------------------------------------
TickRNG: subroutine
	lda rand	;Load rand
	lsr		;shift 1 bit right
	bcc .noeor	;if a set bit is shifted out then XOR
	eor #$B4 	;to make the rng more random
.noeor:	sta rand	;Store results back to memory
	rts 		;return
;===============================================================================
;System Setup
;===============================================================================
; Loads memory with known values, this is a one time setup.
; Any value that is changed to a different value on reset needs to be 
; in the reset routine.
;-------------------------------------------------------------------------------
SystemSetup:
	lda #%00110001		;Control Playfield Options
	sta CTRLPF		;Reflect Playfied, Have 8 clock ball
	lda INTIM		;use the current timer (random at startup)
	ora #$AA		;logic OR to make sure that seed is not 0.
	sta rand		;use as the rand seed
	stx TIM1T		;x = 0, clear out the timer
	lda #>ScoreGrfx		;Load the high byte of the score to
	sta scoreDigit1+1	;pointer memory
	sta scoreDigit2+1
	sta scoreDigit3+1
	sta scoreDigit4+1
;-------------------------------------------------------------------------------
;GameSetup:
	lda #24			 ;arenaY = 24
	sta arenaY
	lda #$77		 ;Load Starting pattern to VRAM
	sta vram
	sta vram+1
	lda #%00010101		 ;Load bombCount grfx to the pfscore's
	sta pfscore1
	sta pfscore2
	lda #>DiamondGrfx	 ;set hight byte of the diamondGrfxPointer
	sta diamondGrfxPointer+1 ; will never be changed
	rts
;===============================================================================
;bomb Grfx Patterns
;===============================================================================
BombTableLeft:
	.byte %00000000
	.byte %00010000
	.byte %00010100
	.byte %00010101
	.byte %00010101
	.byte %00010101
BombTableRight:
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %00000101
;===============================================================================
;ActivecolorPallet for Load Activecolor subroutine
;===============================================================================
ActivecolorPallett:
;color
	.byte $1c	;COLUP0
	.byte $42	;COLUP1
	.byte $52	;COLUPF
	.byte $00	;COLUBK
	.byte $0e	;scorecolor
	.byte $1c	;pfscorecolor
	.byte $52	;scoreBackgroundcolor
;B/W colors
	.byte $0e	;COLUP0
	.byte $08	;COLUP1
	.byte $0e	;COLUPF
	.byte $00	;COLUBK
	.byte $00	;scorecolor
	.byte $00	;pfscorecolor
	.byte $0e	;scoreBackgroundcolor
;===============================================================================
;missile XY values for the diamond shape
;===============================================================================
DiamondMissileXPos:
missileXPointer = #$50
	.byte (missileXPointer)
	.byte (missileXPointer-8)
	.byte (missileXPointer-16)
	.byte (missileXPointer-24)
	.byte (missileXPointer-16)
	.byte (missileXPointer-8)
	.byte (missileXPointer)
	.byte (missileXPointer+8)
	.byte (missileXPointer+16)
	.byte (missileXPointer+24)
	.byte (missileXPointer+16)
	.byte (missileXPointer+8)
DiamondMissileYPos:
missileYPointer = #$50
	.byte (missileYPointer)
	.byte (missileYPointer-8)
	.byte (missileYPointer-16)
	.byte (missileYPointer-24)
	.byte (missileYPointer-32)
	.byte (missileYPointer-40)
	.byte (missileYPointer-48)
	.byte (missileYPointer-40)
	.byte (missileYPointer-32)
	.byte (missileYPointer-24)
	.byte (missileYPointer-16)
	.byte (missileYPointer-8)
;===============================================================================
;Prep Display
;===============================================================================
;Prep memory for the Picture subroutine
;player Y pos 
;-------------------------------------------------------------------------------
;   A - holds the X position of the object
;   X - holds which object to position
;       0 = player0
;       1 = player1
;       2 = missile0
;       3 = missile1
;       4 = ball
;-------------------------------------------------------------------------------
;shoves all objects through the X cord generator.
;-------------------------------------------------------------------------------
PrepDisplay:	subroutine
	ldx #4		;loop through every object
.Loop:	lda player0y,x	;move Y pos to temp memory 
	sta temp1,x
	lda player0x,x	;Load next object X pos
	jsr PosObjectX	;turn into TIA memory
	dex		;x--
	bpl .Loop	;if x != -#, loop.
	sta WSYNC	;Wait for the end of the scanline
	sta HMOVE	;HMOVE command (See stella programmers guide)
	stx VDELP1	;video delay for player 1 (red ball)
	stx PF0		;solid bars on both sides of the screen
	rts		;return to mainloop
;===============================================================================
;LoadColors
;===============================================================================
;Load color data to the TIA for the display to use.
;-------------------------------------------------------------------------------
LoadColors:
	bit gameState			;bit test with the gamestate.
	bpl LoandcolorAttackcolors	;if the game is active branch ahead
;-------------------------------------------------------------------------------
LoadActivecolors: subroutine
;when the game is in the active state, load the non color attack color set
	lda #0
	sta colorIndex			;0 out the color index
	ldy #6				;Y = color Offset
	lda SWCHB			;load console switches
	and #%00001000			;filter out B/W switch
	bne .colorSwitchIsSetTocolor	; if SWCHB(3) = 0 then load B/W index
	ldy #13				;Y = B/W Offset
.colorSwitchIsSetTocolor:
	ldx #2
.LoopA:	lda ActivecolorPallett,y	;Move data to Ram for the kernel to use
	sta scorecolor,x
	dey				;scorecolor
	dex				;pfscorecolor
	bpl .LoopA			;scoreBackgroundcolor
	ldx #3
.LoopB: lda ActivecolorPallett,y
	sta COLUP0,x			;Pipe date straight to the TIA
	dey				;COLUP0
	dex 				;COLUP1
	bpl .LoopB			;COLUPF
	rts				;COLUBK
;===============================================================================
LoandcolorAttackcolors:	subroutine
;Load Inactive color set that will cycle through multiple colors 
;InactivecolorPallet stored in DiamondDropTables.asm
;-------------------------------------------------------------------------------
;	temp2 = 0	
	lda #0
	sta temp2
;	if colorClock >= (2*60) then colorClock = 0 : colorIndex=colorIndex+1
	lda fraim
	and #%01111111 	;every 127/60 fraims
	bne .colorClockNotMaxedOut
	inc colorIndex
.colorClockNotMaxedOut:
;	if colorIndex > 5 then colorIndex = 0
	lda colorIndex
	cmp #5
	bcc .colorIndexIsNotMaxedOut
	beq .colorIndexIsNotMaxedOut
	lda #0
	sta colorIndex
.colorIndexIsNotMaxedOut:
;	if !ConsoleSwitchB/W then temp2 = 6
	lda SWCHB
	and #%00001000
	bne .colorSwitchIsSetTocolor
	lda #$06
	sta temp2
.colorSwitchIsSetTocolor:
;Index = (colorIndex + b/wIndex) x 7
	lda colorIndex			;Load colorIndex
	clc				;Clear Carry
	adc temp2			;Add temp2 (B/W Offset)
	sta temp1			;Store in temp1
	asl				;Multiply by 8
	asl
	asl
	sec
	sbc temp1			;Subtract the index for X 7
	tay				;Transfer A to Y
;Load colors Routine
	ldx #2
.LoopA:	lda InactiveColorPallet,y
	sta scorecolor,x
	iny
	dex
	bpl .LoopA
	ldx #3
.LoopB:	lda InactiveColorPallet,y
	sta COLUP0,x
	iny
	dex
	bpl .LoopB
	rts
;===============================================================================	
;Prepscore
;===============================================================================
; Takes a 2-digit score and multiplys each digit by 8 for a index into the 
; ScoreGrfx Table for the screen to draw.
;--Prepscore Part 1-------------------------------------------------------------
Prepscore:	subroutine
	ldx #0
	ldy #1	;sets of digits to proccess through -1
.Loop:
	lda score,y
	pha
	jsr CreatescoreIndex
	pla
	lsr
	lsr
	lsr
	lsr
	jsr CreatescoreIndex
	dey
	bpl .Loop
	rts
CreatescoreIndex:
	and #$0f
	asl			;multiply number by 8
	asl			;
	asl			;
	clc
	adc #<ScoreGrfx-1	; add score digit offset 
	sta scoreDigit1,x
	inx
	inx
	rts
;===============================================================================
;PortalUpdate
;===============================================================================
; Update the X pos of the ball for the portal @ near the bottom of the screen
; Using this equation: ballx = (X * 8) + 49
;-------------------------------------------------------------------------------
PortalUpdate:	subroutine
	lda fraim	;look to the fraim counter 
	and #%00000111	;grab value range 0-7, X value
	asl		;Multiply by 8
	asl
	asl
	clc
	adc #49		;Add X pos offset
	sta ballx	;Store for PrepDisplay
	rts
;===============================================================================
;MissileUpdate
;===============================================================================
; Update the X pos of the missiles for the diamond
;-------------------------------------------------------------------------------
MissileUpdate:	subroutine
; if MissilePosClock = 0 then MissilePosClock = MissileClockCount
; | Missile0XposIndex=Missile0XposIndex+1
; | Missile1XposIndex=Missile1XposIndex-1
	lda fraim
	and #%00000011
	bne .MissilePosClockNeqMissileClockCount
	inc missile0posIndex	
	dec missile1posIndex
.MissilePosClockNeqMissileClockCount:
; if missile1posIndex = -1 (255) then Missile1poxIndex = 11
	lda missile1posIndex
	bpl .missile1posIndexIsNeq255
	lda #11
	sta missile1posIndex
.missile1posIndexIsNeq255:
; if missile0posIndex = 12 then missile0posIndex = 0
	lda missile0posIndex
	cmp #12
	bne .missile0posIndexIsNeq4
	lda #0
	sta missile0posIndex
.missile0posIndexIsNeq4:
;	Load results to the kernel memory
	ldx missile0posIndex
	ldy missile1posIndex
	
	lda DiamondMissileYPos,x
	sta missile0y
	lda DiamondMissileXPos,x
	sta missile0x
	lda DiamondMissileYPos,y
	sta missile1y			;same as missile0y
	lda DiamondMissileXPos,y
	sta missile1x	
	rts
;===============================================================================
;ProcessPlayers
;===============================================================================
; Loads The playerIndex variable and checks for if it will index out of of
; bounds, making corrections if nessesary.
; Then Takes the byte and uses it to index a huge X Y Table for the Player
; 0 and 1 X and Y positions for the PossisionObjects routine.
;-------------------------------------------------------------------------------
ProcessPlayers:	subroutine
; if playerIndex = 97 then playerIndex = 0
	lda playerIndex
	cmp #97
	bne .playerIndexIsLessThan96
	lda #0
	sta playerIndex
.playerIndexIsLessThan96:	
;if playerIndex > 128 then playerIndex = 96
	lda playerIndex
	bpl .playerIndexIsNot255
	lda #96
	sta playerIndex
.playerIndexIsNot255:
; if !variation(1) then devide playerindex by 4
	ldx playerIndex		; Loads the playerIndex into X 
	lda variation
	and #%00000010
	bne .variationSmooth
	lda playerIndex
	and #%11111100
	tax
.variationSmooth:	
	lda PlayerXpos,x	; Grabs X pos for player0
	sta player0x		; Store it
	lda PlayerYpos,x	; Grabs Y pos for Player0
	sta player0y		; Store it
	
;	txa		;2 Move X to A for conversion for other player
;	clc		;2 Adds 48 to properly convert the Index for player 1
;	adc #48		;2 playerIndex(Player0)+48 = playerIndex(Player1)
;	tax		;2 Move the new playerIndex into X
			;8 cycles total
						
;Fast X add: See FastMath.txt
	txa		;2
	sbx #256-48	;2, 4 cycle
;	a=x
;	x=x+Value			
						
	lda PlayerXpos,x	; Grabs X pos for player1
	sta player1x		; Store it
	lda PlayerYpos,x	; Grabs Y pos for player1
	sta player1y		; Store it
	rts			; return 
;===============================================================================
;ProcessJoysticks
;===============================================================================
; Process the hand controllers
; SWCHA = %76543210
;	   |||||||+- up		(P1)
;	   ||||||+-- Down	(P1)
;	   |||||+--- left	(P1)
;	   ||||+---- right	(P1)
;	   |||+----- up		(P0)
;	   ||+------ down	(P0)
;	   |+------- left	(P0)
;	   +-------- right	(P0)
; INPT4 = Player 0 button
; INPT5 = Player 1 button
;A "0" in a data bit indicates the joystick has been moved to close that switch.
;All "1's" in a player's nibble indicates that joystick is not moving.
;-------------------------------------------------------------------------------
ProcessJoysticks:	subroutine
	bit gameState
	bpl .GameNotActive
	lda SWCHA
	asl
	bcs .SWCHAPlayer0Right
	inc playerIndex
.SWCHAPlayer0Right:
	asl
	bcs .SWCHAPlayer0Left
	dec playerIndex
.SWCHAPlayer0Left:
	asl
	bcs .SWCHAPlayer0Down
	inc arenaY
.SWCHAPlayer0Down:
	bit INPT4
	bmi .ButtonNotPressed
	jsr UseBomb
.ButtonNotPressed:
.GameNotActive:
	rts
;===============================================================================
; Process console switches
;===============================================================================
; Reads SWCHB (Console Switches) and set memory according to its results
; SWCHB =  %76543210
;	    |||||||+- Game Reset 0 = switch pressed
;	    ||||||+-- Game Select 0 = switch pressed
;	    ||||+---- color - B/W 0 = B/W 1= color
;	    ||++-+--- (Not Used)
;	    |+------- P0 difficulty 0 = amateure(B) 1 = pro(A)
;	    +-------- P1 difficulty 0 = amateure(B) 1 = pro(A)
;-------------------------------------------------------------------------------
ProcessConsoleSwitches:	subroutine
	ldx #<DiamondGrfx
	bit SWCHB
	bvc .Player0DifficultyIsAmateure
;difficilty is pro
	ldx #<DiamondGrfx+8
.Player0DifficultyIsAmateure:
	stx diamondGrfxPointer
	ldx #0
;select
	lda SWCHB
	and #%00000010
	bne .SelectSwitchIsNotPressed
;is pressed
	jsr SelectRoutine
	ldx #$40
.SelectSwitchIsNotPressed:
	lda gameState
	and #%10000000
	sta gameState
	txa
	clc
	adc gameState
	sta gameState
;reset
	lda SWCHB
	lsr
	bcs .ResetIsNotPressed
	jsr ResetRoutine
	jsr LoadPlayfieldData
.ResetIsNotPressed:
	rts
;===============================================================================
; Select routine
;===============================================================================
; the select switch is used to change the variation of the game.
;-------------------------------------------------------------------------------
SelectRoutine:	subroutine
	bit gameState
	bvs .SelectFlagIsSet
	lda #$40
	and gameState
	sta gameState
	inc variation
	lda variation
	and #%00000111
	sta variation
	tax
	inx
	stx score
;	ldy #sfxCOLLECT
;	jsr SFX_TRIGGER
	brk
	.byte sfxCOLLECT
.SelectFlagIsSet:
	rts

;===============================================================================
;Reset Routine
;===============================================================================
; when the reset switch is pressed, this chunk of code will set a bunch of 
; memory for the new game.
;-------------------------------------------------------------------------------
ResetRoutine:
	lda #$80
	ora gameState
	sta gameState
	lda #0
	sta score
	sta score+1
	sta arenaY
	sta playerIndex
	lda #%00010101
	sta pfscore1
	sta pfscore2
	lda #6
	sta bombCount
	jsr SFX_OFF
	rts

;===============================================================================
; Use Bomb
;===============================================================================
; when the fire button is pressed, a "bomb" is used.
; vram Will be changed to this pattern 1000000000000001.
; a bomb will not be used if the pattern is already in.
;-------------------------------------------------------------------------------
UseBomb:	subroutine
	lda vram
	ldx vram+1
	cmp #%00000001
	bne .PatternValid
	cpx #%00000001
	bne .PatternValid
	beq .OutOfBombs
.PatternValid:
	lda bombCount
	beq .OutOfBombs
	dec bombCount
	ldx bombCount
	lda BombTableLeft,x
	sta pfscore1
	lda BombTableRight,x
	sta pfscore2
	ldy #30
	sty bombSoundTimer
	lda #$01
	sta vram
	sta vram+1
.OutOfBombs:
	rts
;===============================================================================
;Prcess Playfield
;===============================================================================
; Moves the playfield down every 6 or 3 frames.
;-------------------------------------------------------------------------------
ArenaSpeedNormal = 6
ArenaSpeedFast	= 3
ProcessPlayfield:	subroutine
	bit gameState
	bpl .GameNotActive
	ldx #ArenaSpeedNormal-1
	lda variation
	lsr	;and #%00000001
	bcc .variationFastArena
	ldx #ArenaSpeedFast-1
.variationFastArena:
	dec arenaClock
	bpl .ArenaClockIsNotZero
	stx arenaClock
	inc arenaY
.ArenaClockIsNotZero:
.GameNotActive:
	rts
;===============================================================================
;Load Playfield Data
;===============================================================================
; Ticks the RNG and then looks into a table for new playfield Data,
; 128 different patterns
;-------------------------------------------------------------------------------
TotalvariationsForPlayfield = 128 ; 128 max
LoadPlayfieldData:	subroutine
	lda variation
	and #%00000100
	beq .variationTable
	jsr TickRNG
	sta vram
	jsr TickRNG
	sta vram+1
	rts
.variationTable:
	jsr TickRNG
	and #%11111110
;	cmp #(TotalvariationsForPlayfield*2+2) not needed any more
;	bcs .variationTable
	tax
	lda VramData,x
	sta vram
	inx
	lda VramData,x
	sta vram+1
	rts
;===============================================================================
; Check Collision
;===============================================================================
; checks the colision of player 0, player1, missile0, missile1, ball,
; and playfield. what we are most concerned with is when the playfield
; colides with the ball and any of the players.
; Due to how the kernel is desined, the arena and ball collision must be 
; done by software instead of the hardware.
;-------------------------------------------------------------------------------
CheckCollision:	subroutine
	lda arenaY
	cmp #$55+1
	bcc .NoCollisionBLPF
;	beq .NoCollisionBLPF
	lda #0
	sta arenaY
	sed
	lda score+1
	clc
	adc #1
	sta score+1
	
	lda score+1
	bne .noinc
	lda score
	clc
	adc #1
	sta score
.noinc:
	cld
	jsr LoadPlayfieldData ;grab new PlayfieldData
;	ldy #sfxPING
;	jsr SFX_TRIGGER
	brk
	.byte sfxPING
.NoCollisionBLPF:
;check collision for both diamonds
;	ldy #sfxGAMEOVER
	bit CXP0FB
	bpl .NoCollisionP0PF
;	jsr SFX_TRIGGER
	brk
	.byte sfxGAMEOVER
	lda #%01000000
	and gameState
	sta gameState
.NoCollisionP0PF:
	bit CXP1FB
	bpl .NoCollisionP1PF
;	jsr SFX_TRIGGER
	brk
	.byte sfxGAMEOVER
	lda #%01000000
	and gameState
	sta gameState
.NoCollisionP1PF:
	rts
;===============================================================================
;TickTimers
;===============================================================================
; Ticks Timers for other subrutines that are not processed each frame.
;-------------------------------------------------------------------------------
TickTimers:	subroutine
;UseBomb: Sound of explosion from button press
	lda bombSoundTimer
	beq .BombSoundTimerIsGreaterThan0
	dec bombSoundTimer
	beq .BombSoundTimerIsGreaterThan0
;	ldy #sfxCOLLIDE
;	jsr SFX_TRIGGER
	brk
	.byte sfxCOLLIDE
.BombSoundTimerIsGreaterThan0:
	rts

;===============================================================================
	include SoundEngine.asm
	include DiamondDropTables.asm
	echo "-----",[(PosObjectX - *)]d , "bytes of ROM space left")
	align ($ffff-8*10-3-19-16)
;===============================================================================	
;----------
; PosObject
;----------
; subroutine for setting the X position of any TIA object
; when called, set the following registers:
;   A - holds the X position of the object
;   X - holds which object to position
;       0 = player0
;       1 = player1
;       2 = missile0
;       3 = missile1
;       4 = ball
; the routine will set the coarse X position of the object, as well as the
; fine-tune register that will be used when HMOVE is used.
;
; Note: The X position differs based on the object, for player0 and player1
;       0 is the leftmost pixel while for missile0, missile1 and ball 1 is
;       the leftmost pixel:
;           players     - X range is 0-159
;           missiles    - X range is 1-160
;           ball        - X range is 1-160
; Note: Setting players to double or quad size will affect the position of
;       the players.
;===============================================================================
PosObjectX:	subroutine
	sec	       ;set carry flah
	sta WSYNC      ;wait for sync
.DivideLoop:
	sbc #15        ;2 2 - each time thru this loop takes 5 cycles, which is 
	bcs .DivideLoop;2 4 - the same amount of time it takes to draw 15 pixels
	eor #7         ;2 6 - The EOR & ASL statements convert the remainder
	asl            ;2 8 - of position/15 to the value needed to fine tune
	asl            ;2 10- the X position
	asl            ;2 12
	asl            ;2 14
	sta.wx HMP0,X  ;5 19- store fine tuning of X
	sta RESP0,X    ;4 23- set coarse X position of object
	rts            ;6 29
;===============================================================================
;Custom graphics
;===============================================================================
;Custom graphics for the game
;-------------------------------------------------------------------------------
DiamondGrfx:
	.byte %00000000
	.byte %00011000
	.byte %00111100
	.byte %01111110
	.byte %01111110
	.byte %00111100
	.byte %00011000
	.byte %00000000
	
	.byte %00011000
	.byte %00111100
	.byte %01111110
	.byte %11111111
	.byte %11111111
	.byte %01111110
	.byte %00111100
	.byte %00011000

	include ScoreGrfx.asm ;must all be in the same page range
;===============================================================================	
	org $fffc
	.word Start
	.word BrkRoutine