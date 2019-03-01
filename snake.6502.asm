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
; *   $01 - Snake Tail Pointer - An offset representing the first byte of the snake's tail tile. For a snake that is 3
; *                              tiles long, the offset would be 2 tiles * 4 bytes per tile = 8
; *   $02 - Current Snake Direction - A value indicating the snake's current direction:
; *           #01 - Up    #03 - Left
; *           #02 - Down  #04 - Right
; *   $03 - Next Snake Direction - A value indicating the snake's next direction:
; *           #01 - Up    #03 - Left
; *           #02 - Down  #04 - Right
; *   $04-$07 - Temporary Snake Segment Data (TSSD)
; *           $04 - X     $06 - Tile Index
; *           $05 - Y     $07 - Tile Attrs
; *   $08 - Grow Bit - #$01 if the snake should grow on the next move, otherwise #$00
; **********************************************************************************************************************
; * Reference:
; *   Tile Attribute bits:
; *     7 6 5 43 2 10
; *     | | | ||   ||
; *     | | | ||   ++- Color Palette of sprite.  Choose which set of 4 from the 16 colors to use
; *     | | | ++------ CUSTOM: The "direction" of the tile (up = 00, down = 01, left = 10, right = 11)
; *     | | +--------- Priority (0: in front of background; 1: behind background)
; *     | +----------- Flip sprite horizontally
; *     +------------- Flip sprite vertically
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
  LDA #08                     ; The snake starts out at 3 tiles long, so the offset of the tail's first byte is 8
  STA $01
  LDA #$02
  STA $02                     ; Set the default direction to down
  STA $03                     ; Set the default direction to down

Forever:
  JMP Forever                 ; jump back to Forever, infinite loop
  
 

; ----------------------------------------------------------------------------------------------------------------------
; NMI
; An interrupt that occurs once every screen paint
NMI:
  LDA #00
  STA $2003                   ; set the low byte (00) of the RAM address
  LDA #02
  STA $4014                   ; set the high byte (02) of the RAM address, start the transfer

  JSR ReadController          ; Read the controller buttons and set the snake direction

  LDX $00                     ; Get the frame counter
  CPX #30                     ; Have 60 frames gone by yet?
  BNE TickNextFrame           ; If not, skip to the next frame
  LDX #00                     ; If yes, then reset the counter to 0
  STX $00
  JSR MoveSnake               ; Then move the snake
TickNextFrame:
  ; Tick the frame counter
  INC $00                     ; Increment the frame counter

  RTI                         ; return from interrupt 
; ----------------------------------------------------------------------------------------------------------------------



; ----------------------------------------------------------------------------------------------------------------------
; READ CONTROLLER
; Read which buttons are currently being pressed and set the snake direction accordingly
ReadController:
  LDA #$01                    ; Set $4016 to 01 and then 00 to latch the controller buttons
  STA $4016
  LDA #$00
  STA $4016                   ; Finish latching the controller buttons

  LDA $4016                   ; We have to burn a few loads to get to the buttons we want: A
  LDA $4016                   ; B
  LDA $4016                   ; Select
  LDA $4016                   ; Start

  LDA $4016                   ; Up
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadUpDone              ; If the user is not pressing up, then continue to the next button
  LDY $02                     ; Read the current snake direction
  CPY #02                     ; If the current snake direction is Down, then Up is invalid - ignore and continue
  BEQ ReadUpDone
  LDY #01                     ; Set the snake direction to Up
  STY $03
ReadUpDone:

  LDA $4016                   ; Down
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadDownDone            ; If the user is not pressing down, then continue to the next button
  LDY $02                     ; Read the current snake direction
  CPY #01                     ; If the current snake direction is Up, then Down is invalid - ignore and continue
  BEQ ReadDownDone
  LDY #02                     ; Set the snake direction to Down
  STY $03
ReadDownDone:

  LDA $4016                   ; Left
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadLeftDone            ; If the user is not pressing left, then continue to the next button
  LDY $02                     ; Read the current snake direction
  CPY #04                     ; If the current snake direction is Right, then Left is invalid - ignore and continue
  BEQ ReadLeftDone
  LDY #03                     ; Set the snake direction to Left
  STY $03
ReadLeftDone:

  LDA $4016                   ; Right
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadRightDone           ; If the user is not pressing right, then continue to the next button
  LDY $02                     ; Read the current snake direction
  CPY #03                     ; If the current snake direction is Left, then Right is invalid - ignore and continue
  BEQ ReadRightDone
  LDY #04                     ; Set the snake direction to Right
  STY $03
ReadRightDone:
  RTS                         ; Return from sub-routine
; ----------------------------------------------------------------------------------------------------------------------



; ----------------------------------------------------------------------------------------------------------------------
; MOVE SNAKE
; Begin by calculating the head's new position, then shuffle the head into that position, then the second segment into
; the head's former position, then the third segment into the second's former position, etc.
MoveSnake:
  LDX #00                     ; Prime the loop counter

  LDY $03                     ; Get the next direction to move the snake
  CPY #01                     ; 1 = Up
  BEQ MoveSnakeUp
  CPY #02                     ; 2 = Down
  BEQ MoveSnakeDown
  CPY #03                     ; 3 = Left
  BEQ MoveSnakeLeft
                              ; Anything else = Right (4 is assumed)

MoveSnakeRight:
  LDA $0203                   ; X Position
  ADC #07                     ; Add 8 Pixels
  STA $04
  LDA $0200                   ; Y Position
  STA $05
  LDA #$11                    ; Use the Left-Right Snake Head
  STA $06
  LDA #%01000000              ; Turn the head rightward
  STA $07
  JMP MoveSnakeLoop
MoveSnakeDown:
  LDA $0203                   ; X Position
  STA $04
  LDA $0200                   ; Y Position
  ADC #07                     ; Add 8 Pixels
  STA $05
  LDA #$10                    ; Use the Up-Down Snake Head
  STA $06
  LDA #%10000000              ; Turn the head downward
  STA $07
  JMP MoveSnakeLoop
MoveSnakeUp:
  LDA $0203                   ; X Position
  STA $04
  LDA $0200                   ; Y Position
  SBC #08                     ; Subtract 8 Pixels
  STA $05
  LDA #$10                    ; Use the Up-Down Snake Head
  STA $06
  LDA #%00000000              ; Turn the head upward
  STA $07
  JMP MoveSnakeLoop
MoveSnakeLeft:
  LDA $0203                   ; X Position
  SBC #08                     ; Subtract 8 Pixels
  STA $04
  LDA $0200                   ; Y Position
  STA $05
  LDA #$11                    ; Use the Left-Right Snake Head
  STA $06
  LDA #%00000000              ; Turn the head leftward
  STA $07

MoveSnakeLoop:                ; Shuffle the segments

                              ; Update the Y Position:
  LDY $0200, X                ; - Save the old Y Position
  LDA $05                     ; - Get the new Y Position
  STA $0200, X                ; - Set the new Y Position
  STY $05                     ; - Save the old Y Position for the next segment
  INX

  CPX #$01                    ; Updating the tile is tricky. If this segment is NOT the head, then we can just handle it
  BNE HandleBodyTile          ; like we would the X and Y position. But if it IS the head, then we need to choose the
                              ; appropriate tile and attributes for the next segment

HandleHeadTile:
                              ; Update the Tile:
  LDA $06                     ; - Get the new Tile Index
  STA $0200, X                ; - Set the new Tile Index
  INX
                              ; Update the Tile Attributes:
  LDA $07                     ; - Get the new Tile Attrs
  STA $0200, X                ; - Set the new Tile Attrs
  INX

  JSR ChooseNextBodyTile      ; The logic for choosing the next body tile is too complex to include here, so it's been
                              ; moved into its own subroutine
  JMP UpdateXPosition

HandleBodyTile:
                              ; Update the Tile:
  LDY $0200, X                ; - Save the old Tile Index
  LDA $06                     ; - Get the new Tile Index
  STA $0200, X                ; - Set the new Tile Index
  STY $06                     ; - Save the old Tile Index for the next segment
  INX
                              ; Update the Tile Attributes:
  LDY $0200, X                ; - Save the old Tile Attrs
  LDA $07                     ; - Get the new Tile Attrs
  STA $0200, X                ; - Set the new Tile Attrs
  STY $07                     ; - Save the old Tile Attrs for the next segment
  INX

UpdateXPosition:
                              ; Update the X Position:
  LDY $0200, X                ; - Save the old X Position
  LDA $04                     ; - Get the new X Position
  STA $0200, X                ; - Set the new X Position
  STY $04                     ; - Save the old X Position for the next segment
  INX

  CPX $01
  BNE MoveSnakeLoop

HandleSnakeTail:
                              ; Update the Y Position:
  LDA $05                     ; - Get the new Y Position
  STA $0200, X                ; - Set the new Y Position... note that we don't need to store the old position because
                              ;   there are no tiles after the tail
  INX

  TXA                         ; Start off by assuming that we'll be using the Up-Down Tail Tile
  TAY                         ; But...
  LDA #$30                    ; We need the "direction" of the old tile to know which tail tile index to use, so store
  STA $0200, Y                ; the current byte offset, check the direction, then backtrack to set the tail tile index
  INX

  LDA $07                     ; - Get the Tile Attrs
  AND #%00011000              ; - Isolate the "direction" of the tile
  LSR A                       ; - Shift the bits to the right 3 times so that we end up with a number between #00-#03
  LSR A
  LSR A
  BEQ SnakeTailUp             ; - 00 - the tile is pointing up
  CMP #01
  BEQ SnakeTailDown
  PHA                         ; So it turns out that we need the Left-Right Tail Tile. Push the tile direction onto the
  LDA #$31                    ; the stack, reset the tile to Left-Right using the Y register, then pull the tile
  STA $0200, Y                ; direction back into the accumulator and continue setting the tile attributes
  PLA
  CMP #02
  BEQ SnakeTailLeft
  CMP #03
  BEQ SnakeTailRight

SnakeTailUp:
SnakeTailLeft:
  LDA #%00000000
  JMP SetSnakeTailAttrs

SnakeTailDown:
  LDA #%10000000
  JMP SetSnakeTailAttrs

SnakeTailRight:
  LDA #%01000000
  JMP SetSnakeTailAttrs

SetSnakeTailAttrs:
  STA $0200, X                ; - Set the new Tile Attrs
  INX

UpdateTailXPosition:
                              ; Update the X Position:
  LDA $04                     ; - Get the new X Position
  STA $0200, X                ; - Set the new X Position

UpdateSnakeDirection:
  LDA $03                     ; Now that we're done moving the snake, set the snake's new current direction
  STA $02

  RTS                         ; Return from sub-routine
; ----------------------------------------------------------------------------------------------------------------------


; ----------------------------------------------------------------------------------------------------------------------
; CHOOSE NEXT BODY TILE
; There are several different tiles to choose from and different orientations the tiles could take. The following table
; lays out the tiles and (H)orizontal and (V)ertical flip bit
;
; ┌─────────┬─────────┬────────┬────────┬─────────┐
; │  / Next │   Up    │  Down  │  Left  │  Right  │
; │ Current │         │        │        │         │
; ├─────────┼─────────┼────────┼────────┼─────────┤
; │ Up      │ T$20    │ --     │ T$12   │ T$12 H  │
; │ Down    │ --      │ T$20 V │ T$12 V │ T$12 HV │
; │ Left    │ T$12 HV │ T$12 H │ T$21   │ --      │
; │ Right   │ T$12 V  │ T$12   │ --     │ T$21 H  │
; └─────────┴─────────┴────────┴────────┴─────────┘
;
ChooseNextBodyTile:
  LDA $02                     ; The simplest scenario is travelling in a straight line
  CMP $03
  BNE NextTileTurn

NextTileStraight:
  CMP #01
  BEQ NextTileStraight_Up
  CMP #02
  BEQ NextTileStraight_Down
  CMP #03
  BEQ NextTileStraight_Left
  CMP #04
  BEQ NextTileStraight_Right

NextTileStraight_Up:
  LDA #$20                    ; Use the Up-Down Snake Body
  STA $06
  LDA #%00000000              ; Turn body upward
  STA $07
  RTS                         ; Return from sub-routine
NextTileStraight_Down:
  LDA #$20                    ; Use the Up-Down Snake Body
  STA $06
  LDA #%10001000              ; Turn body downward
  STA $07
  RTS                         ; Return from sub-routine
NextTileStraight_Left:
  LDA #$21                    ; Use the Left-Right Snake Body
  STA $06
  LDA #%00010000              ; Turn body leftward
  STA $07
  RTS                         ; Return from sub-routine
NextTileStraight_Right:
  LDA #$21                    ; Use the Left-Right Snake Body
  STA $06
  LDA #%01011000              ; Turn body rightward
  STA $07
  RTS                         ; Return from sub-routine

NextTileTurn:
  LDA #$12                    ; All of our turns use the same tile. The real complexity comes in knowing how to flip it.
  STA $06

  LDA $03                     ; Get the next direction
  CMP #01
  BEQ NextTileTurn_ToUp
  CMP #02
  BEQ NextTileTurn_ToDown
  CMP #03
  BEQ NextTileTurn_ToLeft
  CMP #04
  BEQ NextTileTurn_ToRight

NextTileTurn_ToUp:
  LDA #%00000000
  LDY $02                     ; Get the next direction
  CPY #03
  BEQ NextTileTurn_LeftToUp
  CPY #04
  BEQ NextTileTurn_RightToUp

NextTileTurn_ToDown:
  LDA #%00001000
  LDY $02                     ; Get the next direction
  CPY #03
  BEQ NextTileTurn_LeftToDown
  CPY #04
  BEQ NextTileTurn_RightToDown

NextTileTurn_ToLeft:
  LDA #%00010000
  LDY $02                     ; Get the next direction
  CPY #01
  BEQ NextTileTurn_UpToLeft
  CPY #02
  BEQ NextTileTurn_DownToLeft

NextTileTurn_ToRight:
  LDA #%00011000
  LDY $02                     ; Get the next direction
  CPY #01
  BEQ NextTileTurn_UpToRight
  CPY #02
  BEQ NextTileTurn_DownToRight

NextTileTurn_UpToLeft:
NextTileTurn_RightToDown:
  ORA #%00000000
  JMP NextTileTurnDone

NextTileTurn_UpToRight:
NextTileTurn_LeftToDown:
  ORA #%01000000
  JMP NextTileTurnDone

NextTileTurn_DownToLeft:
NextTileTurn_RightToUp:
  ORA #%10000000
  JMP NextTileTurnDone

NextTileTurn_DownToRight:
NextTileTurn_LeftToUp:
  ORA #%11000000
  JMP NextTileTurnDone

NextTileTurnDone:
  STA $07
  RTS                         ; Return from sub-routine
; ----------------------------------------------------------------------------------------------------------------------



  .bank 1
  .org $E000
palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$19,$16,$18,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .db $80, $10, %10000000, $80      ; Snake Head
  .db $78, $20, %00000000, $80      ; Snake Body
  .db $70, $30, %10000000, $80      ; Snake Tail
  .db $68, $01, %00000000, $80      ; Apple

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