; Mermaid NES Music Cartridge
; Copyright (C) 2022 Javier O. Cordero Pérez
;
; Mermaid NES Music Cartridge is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
;
; Mermaid NES Music Cartridge is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along with Foobar. If not, see <https://www.gnu.org/licenses/>. 

.include "ppu.inc"
.include "apu.inc"
.include "vars.inc"

.include "neshdr.asm"

; .include "neschar.asm"
.segment        "CHARS"
.incbin "../art/NEXXT/chr_bank.chr"
; .incbin "../art/OverlayPal/chr_bank.chr"

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; DCPM section required by NSF sound driver
.segment "DPCM"

; Main code segement for the program
.segment "CODE"
	jmp start

; Famitracker NSF driver
RELOCATE_MUSIC = 1
.include "../3rdparty/nsf driver/driver.s"
; Wrapper code placed after end of DPCM samples

Palettes:
.incbin "../art/NEXXT/palettes.pal"
; .incbin "../art/OverlayPal/palettes.dat"

; 	; *NOTE: NES has 2 palettes, background and sprites, each with 4 different 4-color palettes,
; 	; ... for 32 colors total, color #0 = background color and stays the same for every palette.

;    ; Background Palette
; 	.byte WHITE, (ORANGE | VERY_LIGHT), (GREEN   | VERY_LIGHT), (BLUE   | VERY_LIGHT)  ; alt TEAL instead of GREEN
;  	.byte WHITE, (ORANGE | VERY_LIGHT), (GRAY_2  | VERY_LIGHT), (GRAY_2 | LIGHT) ; alt: (GRAY_1 | DARK) instead of (GRAY_2 | VERY_LIGHT)
; 	.byte WHITE, (ORANGE | VERY_LIGHT), (MAGENTA | VERY_LIGHT), (VIOLET | VERY_LIGHT)
; 	.byte WHITE, (ORANGE | VERY_LIGHT), (MAGENTA | VERY_LIGHT), (BLUE   | VERY_LIGHT)

; 	; Sprite Palette
; 	.byte (BLUE | VERY_LIGHT), (OLIVE | LIGHT), BLACK, $00
; 	.byte $0f, $00, $00, $00
; 	.byte $0f, $00, $00, $00
; 	.byte $0f, $00, $00, $00

; color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
DEFMASK        = %00001010 ; background and left column enabled
; vblanked       = $7F       ; Zero page address of Vblank count

.macro WAIT_VBLANK
:  bit PPUSTATUS
   bpl :-
.endmacro

nametable:
.incbin "../art/NEXXT/nametable.nam"
; .incbin "../art/OverlayPal/nametable.nam"

start:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

	ldx #$40
	stx APU_FRAMECTR   ; Disable APU frame IRQ

    ldx #$ff    ; Set up stack
    txs

	jsr NesDisableScreen

	; First wait for vblank to make sure PPU is ready
	WAIT_VBLANK

    ; set all RAM to 0
    LDX #0
    LDA #0
	stx APU_MODCTRL 	; disable DMC IRQs
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

	; Second vblank
	WAIT_VBLANK

    ; PPU is now ready to use

    ; Initialize sound APU, this may be redundant since driver could be doing its own initialization.
    LDA #0
    LDX #0
@apu_clear_loop:
    STA APU_PULSE1CTRL, X ; write 0 to most APU registers
    INX
    CPX #$13
    BNE @apu_clear_loop
    LDA #$0F
    STA APU_CHANCTRL ; turn on square/noise/triangle channels
    ; initialize famitracker song
    LDA #0 ; choose song 0
    LDX #0 ; choose NTSC
    JSR INIT
    ; enable NMI and enter infinite loop (music will be updated by NMI handler)
    LDA #%10000000
    STA PPUCTRL


; Draw Background
LoadBackground:
	lda PPUSTATUS
	lda #$20
	sta PPUADDR
	lda #$00
	sta PPUADDR

	lda #>nametable
	sta z_H
	lda #<nametable
	sta z_L

	ldx #$00
	ldy #$00
OuterLoop:
InnerLoop:
	lda (z_HL),y
    sta PPUDATA
	iny
	cpy #$00

	bne InnerLoop

	inc z_H
	inx
	cpx #$04

	bne OuterLoop


; Store Palettes
	lda #$3F		;Select Palette ram &3F00
	sta PPUADDR		;PPUADDR H
	lda #0
	sta PPUADDR		;PPUADDR L

	ldx #0
PaletteLoop:
	lda Palettes,x
	sta PPUDATA		;PPUDATA
	inx
	cpx #16
	bne PaletteLoop

	jsr NesEnableScreen

@infinite_loop:
	jmp @infinite_loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


NesDisableScreen:			;Turn OFF the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #%00000000
	sta PPUMASK				;PPUMASK
	;lda #$00				;NMI disable (Vblank)
	sta PPUCTRL				;PPUCTRL - VPHB SINN
	rts


NesEnableScreen:			;Turn ON the screen
;(Sprite enable/back enable/Sprite leftstrip / backleftstrip)
	lda #DEFMASK
	sta PPUMASK				;PPUMASK

	lda #$80				;NMI enable (Vblank)
	sta PPUCTRL				;PPUCTRL - VPHB SINN
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


ResetScroll:
	lda #0					;Scroll X
	sta PPUSCROLL			;PPUSCROLL
 	; lda #248				;Scroll y
	sta PPUSCROLL			;PPUSCROLL
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@draw_sprites:
	lda sprite_placement, x 	; Load sprites into SPR-RAM
	sta OAMDATA
	inx
	cpx #$1c
	bne @draw_sprites
	rti

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

nmi:
	; push registers to stack
    pha
    txa
    pha
    tya
    pha

;   ldx #$00 	; Set SPR-RAM address to 0
;   stx OAMADDR
	
	; ; keep default PPU config
	; sta PPUCTRL
	lda #DEFMASK
	sta PPUMASK

    ; call famitracker play routine
    jsr PLAY

	jsr ResetScroll

    ; restore registers
    pla
    tay
    pla
    tax
    pla
    rti

irq:
   rti

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr start
  ;; External interrupt IRQ (unused)
  .addr irq         ; $fffe irq / brk
