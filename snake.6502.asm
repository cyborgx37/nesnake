; **********************************************************************************************************************
; * NESNAKE
; *
; * Classic Snake game, built from scratch for the NES
; *
; * Significant inspiration drawn from the "Nerdy Nights" series of tutorials:
; * http://nintendoage.com/forum/messageview.cfm?catid=22&threadid=4147
; *
; * Author: JD Bell
; * Created: Feb 24, 2019
; **********************************************************************************************************************
; * Special Memory Locations:
; *   $00 - Frame Counter
; *   $01 - Snake Tail Pointer - Points to the Y position address of the snake's current tail
; *   $02 - Snake Direction - A value indicating the snake's next direction:
; *           0001 - #$01 - Right
; *           0010 - #$02 - Down
; *           0100 - #$04 - Up
; *           1000 - #$08 - Left
; *   $03-$04 - Temporary Snake Segment Data (TSSD)
; *           $03 - X
; *           $04 - Y
; *   $05 - Grow Bit - #$01 if the snake should grow on the next move, otherwise #$00
; **********************************************************************************************************************

  .inesprg 1                  ; 1x 16KB PRG code
  .ineschr 1                  ; 1x  8KB CHR data
  .inesmap 0                  ; mapper 0 = NROM, no bank swapping
  .inesmir 1                  ; background mirroring

  .bank 0
  .org $C000 
RESET:
  SEI                         ; disable IRQs
  CLD                         ; disable decimal mode
  LDX #$40
  STX $4017                   ; disable APU frame IRQ
  LDX #$FF
  TXS                         ; Set up stack
  INX                         ; now X = 0
  STX $2000                   ; disable NMI
  STX $2001                   ; disable rendering
  STX $4010                   ; disable DMC IRQs

vblankwait1:                  ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, X
  STA $0100, X
  STA $0300, X
  STA $0400, X
  STA $0500, X
  STA $0600, X
  STA $0700, X
  LDA #$FE
  STA $0200, X                ; move all sprites off screen
  INX
  BNE clrmem
   
vblankwait2:                  ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002                   ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006                   ; write the high byte of $3F00 address
  LDA #00
  STA $2006                   ; write the low byte of $3F00 address
  LDX #00
LoadPalettesLoop:
  LDA palette, X              ; load palette byte
  STA $2007                   ; write to PPU
  INX                         ; set index to next byte
  CPX #$20            
  BNE LoadPalettesLoop        ; if x = $20, 32 bytes copied, all done

  LDX #00
LoadSpritesLoop:
  LDA sprites, X              ; load the next byte of sprite data
  STA $0200, X                ; store the sprite data
  INX
  CPX #$10
  BNE LoadSpritesLoop         ; if x = $10, 16 bytes copied, all done

  LDA #%10000000              ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00010000              ; enable sprites
  STA $2001

InitGame:
  LDA #00                     ; Reset the frame counter to 0
  STA $00 
  LDA #12                     ; Set the snake's starting length to 3: (3 segments * 4 bytes) - 1
  STA $01
  LDA #$02
  STA $02                     ; Set the default direction to down

Forever:
  JMP Forever                 ; jump back to Forever, infinite loop
  
 

NMI:
  LDA #00
  STA $2003                   ; set the low byte (00) of the RAM address
  LDA #02
  STA $4014                   ; set the high byte (02) of the RAM address, start the transfer

  LDX $00                     ; Get the frame counter
  CPX #60                     ; Have 60 frames gone by yet?
  BNE TickNextFrame           ; If not, skip to the next frame
  LDX #00                     ; If yes, then reset the counter to 0
  STX $00
  JSR MoveSnake               ; Then move the snake
TickNextFrame:
  ; Tick the frame counter
  INC $00                     ; Increment the frame counter

  RTI                         ; return from interrupt 


; ----------------------------------------------------------------------------------------------------------------------
; MOVE SNAKE
; Begin by calculating the head's new position, then shuffle the head into that position, then the second segment into
; the head's former position, then the third segment into the second's former position, etc.
MoveSnake:
  LDX #00                     ; Prime the loop counter

  LDY $02                     ; Get the direction to move the snake
  CPY #02                     ; 2 = Down
  BEQ MoveSnakeDown
  CPY #03                     ; 3 = Up
  BEQ MoveSnakeUp
  CPY #04                     ; 4 = Left
  BEQ MoveSnakeLeft
                              ; Anything else = Right (1 is assumed)

MoveSnakeRight:
  LDA $0203                   ; X Position
  ADC #08                     ; Add 8 Pixels
  STA $03
  LDA $0200                   ; Y Position
  STA $04
  JMP MoveSnakeLoop
MoveSnakeDown:
  LDA $0203                   ; X Position
  STA $03
  LDA $0200                   ; Y Position
  ADC #08                     ; Add 8 Pixels
  STA $04
  JMP MoveSnakeLoop
MoveSnakeUp:
  LDA $0203                   ; X Position
  STA $03
  LDA $0200                   ; Y Position
  SBC #08                     ; Subtract 8 Pixels
  STA $04
  JMP MoveSnakeLoop
MoveSnakeLeft:
  LDA $0203                   ; X Position
  SBC #08                     ; Subtract 8 Pixels
  STA $03
  LDA $0200                   ; Y Position
  STA $04

MoveSnakeLoop:                ; Shuffle the segments
  LDY $0200, X                ; Save the old Y Position
  LDA $04                     ; Get the new Y Position
  STA $0200, X                ; Set the new Y Position
  STY $04                     ; Save the old Y Position for the next segment

  INX
  INX
  INX

  LDY $0200, X                ; Save the old X Position
  LDA $03                     ; Get the new X Position
  STA $0200, X                ; Set the new X Position
  STY $03                     ; Save the old X Position for the next segment

  INX

  CPX $01
  BNE MoveSnakeLoop

  RTS                         ; Return from sub-routine
; ----------------------------------------------------------------------------------------------------------------------
  
  
  
  .bank 1
  .org $E000
palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$19,$16,$18,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .db $80, $00, $00, $80      ; sprite 0
  .db $78, $00, $00, $80      ; sprite 1
  .db $70, $00, $00, $80      ; sprite 2
  .db $68, $01, $00, $80      ; sprite 3

  .org $FFFA                  ; first of the three vectors starts here
  .dw NMI                     ; when an NMI happens (once per frame if enabled) the 
                              ; processor will jump to the label NMI:
  .dw RESET                   ; when the processor first turns on or is reset, it will jump
                              ; to the label RESET:
  .dw 0                       ; external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "snake.chr"         ; includes 8KB graphics file from SMB1