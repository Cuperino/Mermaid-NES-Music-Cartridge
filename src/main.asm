.include "ppu.inc"
.include "apu.inc"
.include "z80_reg_sym.inc"

.include "neshdr.asm"
.include "neschar.asm"

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; Main code segement for the program
.segment "CODE"

; color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
DEFMASK        = %00001000 ; background enabled
vblanked       = $7F       ; Zero page address of Vblank count

; Famitracker NSF driver
RELOCATE_MUSIC = 1
.include "../3rdparty/nsf driver/driver.s"
; Wrapper code placed after end of DPCM samples
.segment "DPCM"

reset:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

;   ldx #$40
;   stx $4017   ; Disable APU frame IRQ

    ldx #$ff    ; Set up stack
    txs

  inx		; now X = 0
  stx $2000	; disable NMI
  stx $2001 	; disable rendering
  stx $4010 	; disable DMC IRQs

; first wait for vblank to make sure PPU is ready
; @wait_vblank_1:
;     BIT $2002
;     BPL @wait_vblank_1
    ; set all RAM to 0
    LDX #0
    LDA #0
@clear_memory:
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne @clear_memory

; @wait_vblank_2:
;     BIT $2002
;     BPL @wait_vblank_2
    ; PPU is now ready to use
    ; initialize sound APU
    LDA #0
    LDX #0
@apu_clear_loop:
    STA $4000, X ; write 0 to most APU registers
    INX
    CPX #$13
    BNE @apu_clear_loop
    LDA #$0F
    STA $4015 ; turn on square/noise/triangle channels
    ; initialize famitracker song
    LDA #0 ; choose song 0
    LDX #0 ; choose NTSC
    JSR INIT
    ; enable NMI and enter infinite loop (music will be updated by NMI handler)
    LDA #%10000000
    STA $2000
;
; main:
; load_palettes:
;   lda $2002
;   lda #$3f
;   sta $2006
;   lda #$00
;   sta $2006
;   ldx #$00
; @loop:
;   lda palettes, x
;   sta $2007
;   inx
;   cpx #$20
;   bne @loop
;
; enable_rendering:
;   lda #%10000000	; Enable NMI
;   sta $2000
;   lda #%00001000	; Enable Sprites
;   sta $2001
;
; forever:
;   jmp forever


;Palettes
	lda #$3F		;Select Palette ram &3F00
	sta PPUADDR		;PPUADDR H
	lda #0
	sta PPUADDR		;PPUADDR L

	ldx #4
PaletteAgain:
	lda Palettes-1,x
	sta $2007		;PPUDATA
	dex
	bne PaletteAgain

;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110
	sta $2001		;PPUMASK

	lda #$80		;NMI enable (Vblank)
	sta $2000		;PPUCTRL - VPHB SINN



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	jsr DefineTiles			;Define the tile patterns

	; Blue Border Left

	lda #0					;Start SX
	sta z_B
	lda #0					;Start SY
	sta z_C

	ldx #1					;Width in tiles
	ldy #29					;Height in tiles

	lda #0					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen

	; Blue Border Bottom

	lda #1					;Start SX
	sta z_B
	lda #29					;Start SY
	sta z_C

	ldx #30					;Width in tiles
	ldy #1					;Height in tiles

	lda #0					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen

	; Blue Background

	lda #1					;Start SX
	sta z_B
	lda #0					;Start SY
	sta z_C

	ldx #31					;Width in tiles
	ldy #28					;Height in tiles

	lda #1					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen

	; Dog 1

	lda #2					;Start SX
	sta z_B
	lda #22					;Start SY
	sta z_C

	ldx #5					;Width in tiles
	ldy #3					;Height in tiles

	lda #1					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	; Dog torso

	lda #1					;Start SX
	sta z_B
	lda #24					;Start SY
	sta z_C

	ldx #1					;Width in tiles
	ldy #4					;Height in tiles

	lda #16					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	; Dog white

	lda #2					;Start SX
	sta z_B
	lda #25					;Start SY
	sta z_C

	ldx #1					;Width in tiles
	ldy #2					;Height in tiles

	lda #20					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen

	; Dog chest

	lda #3					;Start SX
	sta z_B
	lda #25					;Start SY
	sta z_C

	ldx #2					;Width in tiles
	ldy #2					;Height in tiles

	lda #21					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	; Dog legs

	lda #2					;Start SX
	sta z_B
	lda #27					;Start SY
	sta z_C

	ldx #2					;Width in tiles
	ldy #1					;Height in tiles

	lda #25					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	; Top Border

	lda #10					;Start SX
	sta z_B
	lda #4					;Start SY
	sta z_C

	ldx #15					;Width in tiles
	ldy #1					;Height in tiles

	lda #28					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen

	; Left Border

	lda #9					;Start SX
	sta z_B
	lda #5					;Start SY
	sta z_C

	ldx #1					;Width in tiles
	ldy #15					;Height in tiles

	lda #30					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen

	; Right Border

	lda #25					;Start SX
	sta z_B
	lda #5					;Start SY
	sta z_C

	ldx #1					;Width in tiles
	ldy #15					;Height in tiles

	lda #32					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen

	; Bottom Border

	lda #10					;Start SX
	sta z_B
	lda #20					;Start SY
	sta z_C

	ldx #3					;Width in tiles
	ldy #1					;Height in tiles

	lda #34					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen


	; Board Background

	lda #10					;Start SX
	sta z_B
	lda #5					;Start SY
	sta z_C

	ldx #15					;Width in tiles
	ldy #15					;Height in tiles

	lda #31					;TileStart
	jsr FillAreaWithTile	;Draw the tiles to screen


	; Posters

	; Poster 1

	lda #11					;Start SX
	sta z_B
	lda #6					;Start SY
	sta z_C

	ldx #3					;Width in tiles
	ldy #4					;Height in tiles

	lda #36					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	; Poster 2

	lda #15					;Start SX
	sta z_B
	lda #10					;Start SY
	sta z_C

	ldx #3					;Width in tiles
	ldy #4					;Height in tiles

	lda #48					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	; Poster 3

	lda #19					;Start SX
	sta z_B
	lda #15					;Start SY
	sta z_C

	ldx #3					;Width in tiles
	ldy #3					;Height in tiles

	lda #60					;TileStart
	jsr FillAreaWithTiles	;Draw the tiles to screen

	; Loop
	jmp *					;Infinite loop

; forever:
;   jmp forever

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

waitframe:
	pha
		lda #$00
		sta vblanked		;Zero Vblanked
waitloop:
		lda vblanked		;Wait for the interrupt to change it
		beq waitloop
	pla
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



GetVDPScreenPos:			;BC=XYpos
		lda z_C
		;and #%00000111		;Ypos * 32 tiles per line
		asl
		asl
		asl
		asl
		asl
		ora z_B				;Add Xpos
		sta z_L				;Store in L byte
		lda z_C
		and #%11111000		;Other bits of Ypos for H byte
		lsr
		lsr
		lsr
		clc
		adc #$20			;$2000 ofset for base of tilemap


		; UNCOMMENT THIS LINE BEFORE DEPLOYING!!!
		jsr waitframe		;Wait for Vblank, COMMENT TO ITERATE FASTER DURING DEVELOPMENT


		sta PPUADDR			;PPUADDR
		lda z_L
		sta PPUADDR			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;BC=Bytes
	;DE=Destination Ram
	;HL=Source Bytes
DefineTiles:				;Send Data to tile definitions


	lda #<TilesetStart			;Source Bitmap Data
	sta z_L
	lda #>TilesetStart
	sta z_H

	lda #<(TilesetEnd-TilesetStart);Source Bitmap Data Length
	sta z_C
	lda #>(TilesetEnd-TilesetStart)
	sta z_B

	lda #<$0800				;Tile 128 (16 bytes per tile)
	sta z_E
	lda #>$0000
	sta z_D


	ldx z_C					;B=High byte of count - X=Low byte
	ldy #0
	jsr NesDisableScreen
	jsr prepareVram			;Calculate destination address

DefineTilesAgain:
	lda (z_HL),Y
	sta $2007 				;PPUDATA - Write data to data-port
	iny
	bne DefineTilesAgainYok

	inc z_H					;INC High byte Y=low byte
DefineTilesAgainYok:
	txa						;Is Low Byte Zero
	bne DefineTilesDecBC_C
	lda z_B					;Is High Byte Zero (Are We done?)
	beq DefineTilesAgainDone
	DEC z_B					;DEC Count High byte (X is low byte)
DefineTilesDecBC_C:
	DEx						;Decrease Count Low Byte
	jmp DefineTilesAgain
DefineTilesAgainDone:
	jmp NesEnableScreen


FillAreaWithTiles:
	sta z_D					;Backup tilenum
FillAreaWithTiles_Yagain:
	txa
	pha
		jsr GetVDPScreenPos	;Calculate Tilemap mempos

		lda z_D
FillAreaWithTiles_Xagain:
		sta $2007			;PPUDATA - Save Tile selection to Vram
		clc
		adc #1				;Move to next tile
		dex
		bne FillAreaWithTiles_Xagain
		sta z_D
		inc z_C				;INC Ypos
	pla
	tax
	dey
	bne FillAreaWithTiles_Yagain

	jmp ResetScroll			;Need to reset scroll after writing to VRAM

FillAreaWithTile:
	sta z_D					;Backup tilenum
FillAreaWithTile_Yagain:
	txa
	pha
		jsr GetVDPScreenPos	;Calculate Tilemap mempos

		lda z_D
FillAreaWithTile_Xagain:
		sta $2007			;PPUDATA - Save Tile selection to Vram
		clc
		dex
		bne FillAreaWithTile_Xagain
		sta z_D
		inc z_C				;INC Ypos
	pla
	tax
	dey
	bne FillAreaWithTile_Yagain

	jmp ResetScroll			;Need to reset scroll after writing to VRAM


prepareVram:				;Select a destination address

		lda z_D				;MSB - DEST ADDR
		sta PPUADDR			;PPUADDR
		lda z_E				;LSB - Dest ADDR
		sta PPUADDR			;PPUADDR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


NesDisableScreen:			;Turn OFF the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00000000
	sta $2001				;PPUMASK
	;lda #$00				;NMI disable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts


NesEnableScreen:			;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00011110
	sta $2001				;PPUMASK

	lda #$80				;NMI enable (Vblank)
	sta $2000				;PPUCTRL - VPHB SINN
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


ResetScroll:
	lda #0					;Scroll X
	sta PPUSCROLL			;PPUSCROLL
 	; lda #248				;Scroll y
	sta PPUSCROLL			;PPUSCROLL
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@loop:	lda sprite_placement, x 	; Load sprites into SPR-RAM
  sta $2004
  inx
  cpx #$1c
  bne @loop
  rti
;
; Sprite placement
sprite_placement:
  .byte $00, $00, $00, $00 	; Empty sprites set default, clearing memory
  .byte $00, $00, $00, $00

; Sprite data stored as PPU OAM (Object Attribute Memory), 4 bytes per PPU OAM as follows:
; 	1st: Y-coordinate | 2nd: sprite tile to display | 3rd: advanced attributes | 4th: X-coordinate
; SpriteData:
; 	; Build grid of 2x4 tiles:
; 	.byte $08, $00, $00, $08 ; 1st sprite: Y offset (0x08) =  8 pixels | Tile #0 | X offset (0x08) =  8 pixels
; 	.byte $08, $01, $00, $10 ; 2nd ...
; 	.byte $10, $02, $00, $08 ; 3rd ...
; 	.byte $10, $03, $00, $10 ; 4th ...
; 	.byte $18, $04, $00, $08 ; 5th ...
; 	.byte $18, $05, $00, $10 ; 6th ...
; 	.byte $20, $06, $00, $08 ; 7th ...

Palettes:
	; *NOTE: NES has 2 palettes, background and sprites, each with 4 different 4-color palettes,
	; ... for 32 colors total, color #0 = background color and stays the same for every palette.

   ; Background Palette
	.byte WHITE, (ORANGE | VERY_LIGHT), (GREEN   | VERY_LIGHT), (BLUE   | VERY_LIGHT)  ; alt TEAL instead of GREEN
 	.byte WHITE, (ORANGE | VERY_LIGHT), (GRAY_2  | VERY_LIGHT), (GRAY_2 | LIGHT) ; alt: (GRAY_1 | DARK) instead of (GRAY_2 | VERY_LIGHT)
	.byte WHITE, (ORANGE | VERY_LIGHT), (MAGENTA | VERY_LIGHT), (VIOLET | VERY_LIGHT)
	.byte WHITE, (ORANGE | VERY_LIGHT), (MAGENTA | VERY_LIGHT), (BLUE   | VERY_LIGHT)

	; Sprite Palette
	.byte (BLUE | VERY_LIGHT), (OLIVE | LIGHT), BLACK, $00
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00

nmi:
	; push registers to stack
    PHA
    TXA
    PHA
    TYA
    PHA

   	inc vblanked

	jsr ResetScroll
	; sta OAMADDR
	
	; ; keep default PPU config
	; sta PPUCTRL
	lda #DEFMASK
	sta PPUMASK

    ; call famitracker play routine
    JSR PLAY

    ; restore registers
    PLA
    TAY
    PLA
    TAX
    PLA
    RTI

; nmi:
;   ldx #$00 	; Set SPR-RAM address to 0
;   stx OAMADDR

irq:
   rti

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr irq         ; $fffe irq / brk
