; ------------------------------------------------------------------------
; Character data
; ------------------------------------------------------------------------

        .export         TilesetStart

.segment        "CHARS"

TilesetStart:
BlankBg:
        .byte   %00000000       ; 0000 1-00: 0 Blue
        .byte   %00000000       ; 0001 1-00:
        .byte   %00000000       ; 0002 1-00:
        .byte   %00000000       ; 0003 1-00:
        .byte   %00000000       ; 0004 1-00:
        .byte   %00000000       ; 0005 1-00:
        .byte   %00000000       ; 0006 1-00:
        .byte   %00000000       ; 0007 1-00:

        .byte   %00000000       ; 0008 1-01:
        .byte   %00000000       ; 0009 1-01:
        .byte   %00000000       ; 000A 1-01:
        .byte   %00000000       ; 000B 1-01:
        .byte   %00000000       ; 000C 1-01:
        .byte   %00000000       ; 000D 1-01:
        .byte   %00000000       ; 000E 1-01:
        .byte   %00000000       ; 000F 1-01:
BlankBgEnd:

WallPatternBg:
        .byte   %01110101       ; Background pattern 0
        .byte   %10101110       ; 
        .byte   %11010101       ; 
        .byte   %10111010       ; 
        .byte   %01110111       ; 
        .byte   %10101110       ; 
        .byte   %11010101       ; 
        .byte   %10111011       ; 

        .byte   %01010101       ; Background pattern 0
        .byte   %10101010       ; 
        .byte   %01010101       ; 
        .byte   %10101010       ; 
        .byte   %01010101       ; 
        .byte   %10101010       ; 
        .byte   %01010101       ; 
        .byte   %10101010       ; 
WallPatternBgEnd:

TopLeftCorner:
        .byte   %00000000       ; 2
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ;     ****
        .byte   %00000000       ;     ****
        .byte   %00000000       ;     ****
        .byte   %00000000       ;     ****

        .byte   %00000000       ;  
        .byte   %00000000       ;  
        .byte   %00000000       ;  
        .byte   %00000000       ; 
        .byte   %00001111       ;     ****
        .byte   %00001111       ;     ****
        .byte   %00001111       ;     ****
        .byte   %00001111       ;     ****
TopLeftCornerEnd:

TopBorder:
        .byte   %00000000       ; 3
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %11111111       ;  ********
        .byte   %11111111       ;  ********
        .byte   %11111111       ;  ********
        .byte   %11111111       ;  ********
TopBorderEnd:

TopRightCorner:
        .byte   %00000000       ; 4
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****

        .byte   %00000000       ; 
        .byte   %00000000       ;  
        .byte   %00000000       ;  
        .byte   %00000000       ;  
        .byte   %11110000       ;  ****
        .byte   %11110000       ;  ****
        .byte   %11110000       ;  ****
        .byte   %11110000       ;  ****
TopRightCornerEnd:

LeftBorder:
        .byte   %00000000       ;  **** 5
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****

        .byte   %00001111       ;  **** 
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
LeftBorderEnd:

BoardBackground:
        .byte   %01110101       ; 6 Board Background
        .byte   %10101110       ; 
        .byte   %11011101       ; 
        .byte   %10111010       ; 
        .byte   %01110111       ; 
        .byte   %10101110       ; 
        .byte   %11010101       ; 
        .byte   %10111011       ; 
        
        .byte   %11111111       ; Board Background
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
BoardByteackgroundEnd:

RightBorder:
        .byte   %00000000       ;      **** 7
        .byte   %00000000       ;      ****
        .byte   %00000000       ;      ****
        .byte   %00000000       ;      ****
        .byte   %00000000       ;      ****
        .byte   %00000000       ;      ****
        .byte   %00000000       ;      ****
        .byte   %00000000       ;      ****

        .byte   %11110000       ;      ****
        .byte   %11110000       ;      ****
        .byte   %11110000       ;      ****
        .byte   %11110000       ;      ****
        .byte   %11110000       ;      ****
        .byte   %11110000       ;      ****
        .byte   %11110000       ;      ****
        .byte   %11110000       ;      ****
RightBorderEnd:

BottomLeftCorner:
        .byte   %00000000       ;  ****    8
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********

        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00001111       ;  ****
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
BottomLeftCornerEnd:

BottomBorder:
        .byte   %00000000       ;  ******** 9
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ;  ********
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %11111111       ;  ********
        .byte   %11111111       ;  ********
        .byte   %11111111       ;  ********
        .byte   %11111111       ;  ********
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
BottomBorderEnd:

BottomRightCorner:
        .byte   %00000000       ;  ****     10
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ;  ****
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %11110000       ;  ****
        .byte   %11110000       ;  ****
        .byte   %11110000       ;  ****
        .byte   %11110000       ;  ****
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
BottomRightCornerEnd:

        .byte   %11111111       ; Dark gray            11
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 


        .byte   %11111111       ; Poster 1 Face          12
        .byte   %11011111       ; 
        .byte   %10011111       ; 
        .byte   %00001111       ; 
        .byte   %00011110       ; 
        .byte   %00011000       ; 
        .byte   %00010000       ; 
        .byte   %10000000       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %11000000       ; 
        .byte   %11100000       ; 
        .byte   %11100000       ; 
        .byte   %01100000       ; 

        .byte   %11111110       ; Poster 1 Left Arm          12
        .byte   %11111100       ; 
        .byte   %11111000       ; 
        .byte   %11100001       ; 
        .byte   %00000011       ; 
        .byte   %00000111       ; 
        .byte   %00011111       ; 
        .byte   %01111111       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %11111111       ; Poster 1 Right Arm          13
        .byte   %11111100       ; 
        .byte   %11111000       ; 
        .byte   %11110000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000111       ; 
        .byte   %01111111       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 


        .byte   %11010001       ; Poster 1 Chest
        .byte   %01110001       ; 
        .byte   %01110001       ; 
        .byte   %00110000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %01110000       ; 
        .byte   %01110000       ; 
        .byte   %01110000       ; 
        .byte   %00110000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 


        .byte   %11111111       ; Poster 1 Left flap
        .byte   %11111111       ; 
        .byte   %11111111       ; 
        .byte   %00111110       ; 
        .byte   %01111100       ; 
        .byte   %01100001       ; 
        .byte   %00000011       ; 
        .byte   %00001111       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %11111110       ; Poster 1 Right flap
        .byte   %11111110       ; 
        .byte   %01111100       ; 
        .byte   %00011101       ; 
        .byte   %00111111       ; 
        .byte   %00111111       ; 
        .byte   %00000011       ; 
        .byte   %00000011       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %00000000       ; Left foreleg
        .byte   %00000000       ; 
        .byte   %01000000       ; 
        .byte   %11000001       ; 
        .byte   %10000011       ; 
        .byte   %10000011       ; 
        .byte   %00000111       ; 
        .byte   %00000111       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %00011111       ; Right foreleg
        .byte   %00001111       ; 
        .byte   %00000111       ; 
        .byte   %00000011       ; 
        .byte   %11000001       ; 
        .byte   %11100000       ; 
        .byte   %11110000       ; 
        .byte   %11100001       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 

        .byte   %00011110       ; Right knee and thing in front
        .byte   %00011111       ; 
        .byte   %00000011       ; 
        .byte   %00000011       ; 
        .byte   %00000011       ; 
        .byte   %00000011       ; 
        .byte   %11010101       ; 
        .byte   %10111011       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 

        .byte   %00000001       ; Right tibia
        .byte   %00000000       ; 
        .byte   %11110000       ; 
        .byte   %11111110       ; 
        .byte   %11111110       ; 
        .byte   %11111100       ; 
        .byte   %11010101       ; 
        .byte   %10111011       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 

        .byte   %11100001       ; Left tibia
        .byte   %11100011       ; 
        .byte   %00001111       ; 
        .byte   %00011111       ; 
        .byte   %00011111       ; 
        .byte   %01011111       ; 
        .byte   %11010101       ; 
        .byte   %10111011       ; 

        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %00000000       ; 
        .byte   %11111111       ; 
        .byte   %11111111       ; 

TilesetEnd:
