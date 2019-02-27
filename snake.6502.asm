  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;
; Special Memory Locations:
; $00 - Frame Counter
; $01 - Snake Tail Pointer - Points to the Y position address of the snake's current tail
; $02 - Snake Direction - A value indicating whether the snake should move up, down, left or right. Values are:
;         0001 - #$01 - Right
;         0010 - #$02 - Down
;         0100 - #$04 - Up
;         1000 - #$08 - Left
; $03-$04 - Temporary Snake Segment Data (TSSD)
;         $03 - X
;         $04 - Y
; $05 - Grow Bit - #$01 if the snake should grow on the next move, otherwise #$00

    
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
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
  STA $0200, X    ;move all sprites off screen
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002    ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006    ; write the high byte of $3F00 address
  LDA #$00
  STA $2006    ; write the low byte of $3F00 address
  LDX #$00
LoadPalettesLoop:
  LDA palette, X        ;load palette byte
  STA $2007             ;write to PPU
  INX                   ;set index to next byte
  CPX #$20            
  BNE LoadPalettesLoop  ;if x = $20, 32 bytes copied, all done

  LDX #$00
LoadSpritesLoop:
  LDA sprites, X        ;load the next byte of sprite data
  STA $0200, X          ;store the sprite data
  INX
  CPX #$10
  BNE LoadSpritesLoop  ;if x = $10, 16 bytes copied, all done

  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

InitGame:
  LDA #$00
  STA $00 ; Reset the frame counter to 0
  LDA #$0C
  STA $01 ; Set the snake's starting length to 3
  LDA #$02
  STA $02 ; Set the default direction to down

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
 

NMI:
  LDA #$00
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014  ; set the high byte (02) of the RAM address, start the transfer

  LDX $00           ; Get the frame counter
  CPX #$3C          ; Have 60 frames gone by yet?
  BNE TickNextFrame ; If not, skip to the next frame
  LDX #$00          ; If yes, then reset the counter to 0
  STX $00
  JSR MoveSnake     ; Then move the snake
TickNextFrame:
  ; Tick the frame counter
  LDX $00           ; Get the current frame count
  INX               ; Increment
  STX $00           ; Store the frame count

  RTI               ; return from interrupt 


; ----------------------------------------------------------------------------------------------------------------------
; MOVE SNAKE
MoveSnake:
  ; Begin by calculating the head's new position, then shuffle the head into that position, then the second segment into
  ; the head's former position, then the third segment into the second's former position, etc.
  LDX #$00 ; Prime the loop counter

  ; Calculate the new location for the head

  ; Now, figure out which direction we need to move in
  LDY $02 ; Get the direction to move the snake
  CLC
  CPY #$02
  BEQ MoveSnakeDown
  CPY #$03
  BEQ MoveSnakeUp
  CPY #$04
  BEQ MoveSnakeLeft
  ; The only other valid value is Right:

  ; Now, add or subtract 8 pixels to/from the appropriate axis
MoveSnakeRight:
  ; Calculate the new X Position
  LDA $0203
  ADC #08
  STA $03
  ; Calculate the new Y Position
  LDA $0200
  STA $04
  JMP MoveSnakeLoop
MoveSnakeDown:
  ; Calculate the new X Position
  LDA $0203
  STA $03
  ; Calculate the new Y Position
  LDA $0200
  ADC #08
  STA $04
  JMP MoveSnakeLoop
MoveSnakeUp:
  ; Calculate the new X Position
  LDA $0203
  STA $03
  ; Calculate the new Y Position
  LDA $0200
  SBC #08
  STA $04
  JMP MoveSnakeLoop
MoveSnakeLeft:
  ; Calculate the new X Position
  LDA $0203
  SBC #08
  STA $03
  ; Calculate the new Y Position
  LDA $0200
  STA $04

  ; Shuffle the segments
MoveSnakeLoop:
  ; Update the Y Position
  LDY $0200, X
  LDA $04
  STA $0200, X
  STY $04

  INX
  INX
  INX

  ; Update the X Position
  LDY $0200, X
  LDA $03
  STA $0200, X
  STY $03

  INX

  CPX $01
  BNE MoveSnakeLoop

  ; Return from sub-routine
  RTS
; ----------------------------------------------------------------------------------------------------------------------
  
  
  
  .bank 1
  .org $E000
palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$19,$16,$18,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0
  .db $78, $00, $00, $80   ;sprite 1
  .db $70, $00, $00, $80   ;sprite 2
  .db $68, $01, $00, $80   ;sprite 3

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "snake.chr"   ;includes 8KB graphics file from SMB1