  .inesprg 1                  ; 1x 16KB PRG code
  .ineschr 1                  ; 1x  8KB CHR data
  .inesmap 0                  ; mapper 0 = NROM, no bank swapping
  .inesmir 1                  ; background mirroring

  .bank 0
  .org $C000 

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
; *   $00 - Frame Counter / Collision Flag - The frame counter is reset to 0 whenever the snake moves, so we can reuse
;                                            this memory for calculating coliision types
ptr_FRAME_COUNTER EQU $00
ptr_COLLISION_FLAG EQU $00
; *   $01 - Snake Tail Pointer - An offset representing the first byte of the snake's tail tile. For a snake that is 3
; *                              tiles long, the offset would be 2 tiles * 4 bytes per tile = 8
ptr_SNAKE_TAIL EQU $01
; *   $02 - Current Snake Direction - A value indicating the snake's current direction:
; *           #01 - Up    #03 - Left
; *           #02 - Down  #04 - Right
ptr_SNAKE_DIR_CUR EQU $02
; *   $03 - Next Snake Direction - A value indicating the snake's next direction:
; *           #01 - Up    #03 - Left
; *           #02 - Down  #04 - Right
ptr_SNAKE_DIR_NXT EQU $03
; *   $04-$07 - Temporary Snake Segment Data (TSSD)
; *           $04 - X     $06 - Tile Index
; *           $05 - Y     $07 - Tile Attrs
ptr_SNAKE_TSSD_X EQU $04
ptr_SNAKE_TSSD_Y EQU $05
ptr_SNAKE_TSSD_TILE EQU $06
ptr_SNAKE_TSSD_ATTR EQU $07
; *   $08 - Game Flags - Various game flags to communicate critical changes in state
;       Flag Reference:
;         7 6 5 4 3 2 1 0
;                   |
;                   +-------- Grow Bit - If 1, the snake should grow by one segment
ptr_GAME_FLAGS EQU $08
flag_GROW EQU %00000100
; *   $0200-$0203 - Apple Sprite
; *           $0200 - Apple Y
ptr_APPLE_Y EQU $0200
; *           $0203 - Apple X
ptr_APPLE_X EQU $0203
; *   $0204-$0207 - Snake Head Sprite
; *           $0200 - Snake Head Y
ptr_SNAKE_HEAD_Y EQU $0204
; *           $0203 - Snake Head X
ptr_SNAKE_HEAD_X EQU $0207
; *   $4016 - Controller Buttons State - Each read will return a button state then adv to next button
ptr_BTN_STATE EQU $4016
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
  STA ptr_APPLE_Y, X          ; store the sprite data
  INX
  CPX #$10
  BNE LoadSpritesLoop         ; if x = $10, 16 bytes copied, all done

  LDA #%10000000              ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00010000              ; enable sprites
  STA $2001

InitGame:
  LDA #00                     ; Reset the frame counter to 0
  STA ptr_FRAME_COUNTER 
  LDA #08                     ; The snake starts out at 3 tiles long, so the offset of the tail's first byte is 8
  STA ptr_SNAKE_TAIL
  LDA #$01
  STA ptr_SNAKE_DIR_CUR       ; Set the default direction to Up
  STA ptr_SNAKE_DIR_NXT       ; Set the default direction to Up
  LDA #00
  STA ptr_GAME_FLAGS          ; Set the grow flag to 0

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

  LDX ptr_FRAME_COUNTER       ; Get the frame counter
  CPX #30                     ; Have 60 frames gone by yet?
  BNE TickNextFrame           ; If not, skip to the next frame
  JSR MoveSnake               ; If yes, move the snake
  LDX #00                     ; Then reset the counter to 0
  STX ptr_FRAME_COUNTER
TickNextFrame:
  ; Tick the frame counter
  INC ptr_FRAME_COUNTER       ; Increment the frame counter

  RTI                         ; return from interrupt 
; ----------------------------------------------------------------------------------------------------------------------



; ----------------------------------------------------------------------------------------------------------------------
; READ CONTROLLER
; Read which buttons are currently being pressed and set the snake direction accordingly
ReadController:
  LDA #$01                    ; Set $4016 (ptr_BTN_STATE) to 01 and then 00 to latch the controller buttons
  STA ptr_BTN_STATE
  LDA #$00
  STA ptr_BTN_STATE           ; Finish latching the controller buttons

  LDA ptr_BTN_STATE           ; We have to burn a few loads to get to the buttons we want: A
  LDA ptr_BTN_STATE           ; B
  LDA ptr_BTN_STATE           ; Select
  LDA ptr_BTN_STATE           ; Start

  LDA ptr_BTN_STATE           ; Up
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadUpDone              ; If the user is not pressing up, then continue to the next button
  LDY ptr_SNAKE_DIR_CUR       ; Read the current snake direction
  CPY #02                     ; If the current snake direction is Down, then Up is invalid - ignore and continue
  BEQ ReadUpDone
  LDY #01                     ; Set the snake direction to Up
  STY ptr_SNAKE_DIR_NXT
ReadUpDone:

  LDA ptr_BTN_STATE           ; Down
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadDownDone            ; If the user is not pressing down, then continue to the next button
  LDY ptr_SNAKE_DIR_CUR       ; Read the current snake direction
  CPY #01                     ; If the current snake direction is Up, then Down is invalid - ignore and continue
  BEQ ReadDownDone
  LDY #02                     ; Set the snake direction to Down
  STY ptr_SNAKE_DIR_NXT
ReadDownDone:

  LDA ptr_BTN_STATE           ; Left
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadLeftDone            ; If the user is not pressing left, then continue to the next button
  LDY ptr_SNAKE_DIR_CUR       ; Read the current snake direction
  CPY #04                     ; If the current snake direction is Right, then Left is invalid - ignore and continue
  BEQ ReadLeftDone
  LDY #03                     ; Set the snake direction to Left
  STY ptr_SNAKE_DIR_NXT
ReadLeftDone:

  LDA ptr_BTN_STATE           ; Right
  AND #%00000001              ; Clear all but the 0 bit
  BEQ ReadRightDone           ; If the user is not pressing right, then continue to the next button
  LDY ptr_SNAKE_DIR_CUR       ; Read the current snake direction
  CPY #03                     ; If the current snake direction is Left, then Right is invalid - ignore and continue
  BEQ ReadRightDone
  LDY #04                     ; Set the snake direction to Right
  STY ptr_SNAKE_DIR_NXT
ReadRightDone:
  RTS                         ; Return from sub-routine
; ----------------------------------------------------------------------------------------------------------------------



; ----------------------------------------------------------------------------------------------------------------------
; MOVE SNAKE
; Begin by calculating the head's new position, then shuffle the head into that position, then the second segment into
; the head's former position, then the third segment into the second's former position, etc.
MoveSnake:
  JSR DetectCollisions
  LDA ptr_GAME_FLAGS          ; Get the current Game Flags
  AND #flag_GROW              ; Isolate the Grow Flag
  BEQ ReadNextSnakeDirection  ; If it's 0, then just continue on with shuffling the snake
  PHA                         ; Store the grow flag value for later

GrowSnake:
  LDX ptr_SNAKE_TAIL          ; Kick off X at the snake tail offset
  LDA ptr_SNAKE_HEAD_Y, X     ; Copy the tail sprite info to seed the new tail
  STA ptr_SNAKE_TSSD_Y
  INX
  LDA ptr_SNAKE_HEAD_Y, X
  STA ptr_SNAKE_TSSD_TILE
  INX
  LDA ptr_SNAKE_HEAD_Y, X
  STA ptr_SNAKE_TSSD_ATTR
  INX
  LDA ptr_SNAKE_HEAD_Y, X
  STA ptr_SNAKE_TSSD_X
  INX

  LDA ptr_SNAKE_TSSD_Y        ; Seed the new tail Y
  STA ptr_SNAKE_HEAD_Y, X
  INX
  LDA #01      ; Seed the new tail Tile
  STA ptr_SNAKE_HEAD_Y, X
  INX
  LDA ptr_SNAKE_TSSD_ATTR     ; Seed the new tail Tile Attr
  STA ptr_SNAKE_HEAD_Y, X
  INX
  LDA ptr_SNAKE_TSSD_X        ; Seed the new tail X
  STA ptr_SNAKE_HEAD_Y, X

  PLA                         ; Retrieve the grow flag value from earlier
  CLC
  ADC ptr_SNAKE_TAIL          ; Add the grow flag value (4) to the snake tail pointer and set tail pointer to the result
  STA ptr_SNAKE_TAIL          ; This is how we grow the snake's sprite array


ReadNextSnakeDirection:
  LDX #00                     ; Prime the loop counter

  LDY ptr_SNAKE_DIR_NXT       ; Get the next direction to move the snake
  CPY #01                     ; 1 = Up
  BEQ MoveSnakeUp
  CPY #02                     ; 2 = Down
  BEQ MoveSnakeDown
  CPY #03                     ; 3 = Left
  BEQ MoveSnakeLeft
                              ; Anything else = Right (4 is assumed)

MoveSnakeRight:
  LDA ptr_SNAKE_HEAD_X        ; Copy the Snake's current X Position into TSSD,
  CLC
  ADC #08                     ; Adding 8 Pixels
  STA ptr_SNAKE_TSSD_X
  LDA ptr_SNAKE_HEAD_Y        ; Copy the Snake's current Y Position into TSSD
  STA ptr_SNAKE_TSSD_Y
  LDA #$11                    ; Use the Left-Right Snake Head
  STA ptr_SNAKE_TSSD_TILE
  LDA #%01000000              ; Turn the head rightward
  STA ptr_SNAKE_TSSD_ATTR
  JMP MoveSnakeLoop
MoveSnakeDown:
  LDA ptr_SNAKE_HEAD_X        ; Copy the Snake's current X Position into TSSD
  STA ptr_SNAKE_TSSD_X
  LDA ptr_SNAKE_HEAD_Y        ; Copy the Snake's current Y Position into TSSD,
  CLC
  ADC #08                     ; Adding 8 Pixels
  STA ptr_SNAKE_TSSD_Y
  LDA #$10                    ; Use the Up-Down Snake Head
  STA ptr_SNAKE_TSSD_TILE
  LDA #%10000000              ; Turn the head downward
  STA ptr_SNAKE_TSSD_ATTR
  JMP MoveSnakeLoop
MoveSnakeUp:
  LDA ptr_SNAKE_HEAD_X        ; Copy the Snake's current X Position into TSSD
  STA ptr_SNAKE_TSSD_X
  LDA ptr_SNAKE_HEAD_Y        ; Copy the Snake's current Y Position into TSSD,
  SBC #08                     ; Subtracting 8 Pixels
  STA ptr_SNAKE_TSSD_Y
  LDA #$10                    ; Use the Up-Down Snake Head
  STA ptr_SNAKE_TSSD_TILE
  LDA #%00000000              ; Turn the head upward
  STA ptr_SNAKE_TSSD_ATTR
  JMP MoveSnakeLoop
MoveSnakeLeft:
  LDA ptr_SNAKE_HEAD_X        ; Copy the Snake's current X Position into TSSD,
  SBC #08                     ; Subtracting 8 Pixels
  STA ptr_SNAKE_TSSD_X
  LDA ptr_SNAKE_HEAD_Y        ; Copy the Snake's current Y Position into TSSD
  STA ptr_SNAKE_TSSD_Y
  LDA #$11                    ; Use the Left-Right Snake Head
  STA ptr_SNAKE_TSSD_TILE
  LDA #%00000000              ; Turn the head leftward
  STA ptr_SNAKE_TSSD_ATTR

MoveSnakeLoop:                ; Shuffle the segments

                              ; Update the Y Position:
  LDY ptr_SNAKE_HEAD_Y, X     ; - Save the old Y Position
  LDA ptr_SNAKE_TSSD_Y        ; - Get the new Y Position
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new Y Position
  STY ptr_SNAKE_TSSD_Y        ; - Save the old Y Position for the next segment
  INX

  CPX #$01                    ; Updating the tile is tricky. If this segment is NOT the head, then we can just handle it
  BNE HandleBodyTile          ; like we would the X and Y position. But if it IS the head, then we need to choose the
                              ; appropriate tile and attributes for the next segment

HandleHeadTile:
                              ; Update the Tile:
  LDA ptr_SNAKE_TSSD_TILE     ; - Get the new Tile Index
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new Tile Index
  INX
                              ; Update the Tile Attributes:
  LDA ptr_SNAKE_TSSD_ATTR     ; - Get the new Tile Attrs
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new Tile Attrs
  INX

  JSR ChooseNextBodyTile      ; The logic for choosing the next body tile is too complex to include here, so it's been
                              ; moved into its own subroutine
  JMP UpdateXPosition

HandleBodyTile:
                              ; Update the Tile:
  LDY ptr_SNAKE_HEAD_Y, X     ; - Save the old Tile Index
  LDA ptr_SNAKE_TSSD_TILE     ; - Get the new Tile Index
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new Tile Index
  STY ptr_SNAKE_TSSD_TILE     ; - Save the old Tile Index for the next segment
  INX
                              ; Update the Tile Attributes:
  LDY ptr_SNAKE_HEAD_Y, X     ; - Save the old Tile Attrs
  LDA ptr_SNAKE_TSSD_ATTR     ; - Get the new Tile Attrs
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new Tile Attrs
  STY ptr_SNAKE_TSSD_ATTR     ; - Save the old Tile Attrs for the next segment
  INX

UpdateXPosition:
                              ; Update the X Position:
  LDY ptr_SNAKE_HEAD_Y, X     ; - Save the old X Position
  LDA ptr_SNAKE_TSSD_X        ; - Get the new X Position
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new X Position
  STY ptr_SNAKE_TSSD_X        ; - Save the old X Position for the next segment
  INX

  CPX ptr_SNAKE_TAIL
  BNE MoveSnakeLoop

  LDA ptr_GAME_FLAGS          ; Check the grow bit. If we're not growing (the normal case) then just handle the tail
  AND #flag_GROW              ; like we would the rest of the body. But, if the grow bit is set, then we've already
  BEQ HandleSnakeTail         ; added the new body segment and the tail should just stay in place.
                              ; Reset the value of the grow flag to 0

  LDA ptr_GAME_FLAGS          ; Erase the Grow Bit
  EOR #flag_GROW
  STA ptr_GAME_FLAGS

HandleSnakeTail:
                              ; Update the Y Position:
  LDA ptr_SNAKE_TSSD_Y        ; - Get the new Y Position
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new Y Position... note that we don't need to store the old position because
                              ;   there are no tiles after the tail
  INX

  TXA                         ; Start off by assuming that we'll be using the Up-Down Tail Tile
  TAY                         ; But...
  LDA #$30                    ; We need the "direction" of the old tile to know which tail tile index to use, so store
  STA ptr_SNAKE_HEAD_Y, Y     ; the current byte offset, check the direction, then backtrack to set the tail tile index
  INX

  LDA ptr_SNAKE_TSSD_ATTR     ; - Get the Tile Attrs
  AND #%00011000              ; - Isolate the "direction" of the tile
  PHA                         ; Keep the custom Tile Attrs for later use
  LSR A                       ; - Shift the bits to the right 3 times so that we end up with a number between #00-#03
  LSR A
  LSR A
  BEQ SnakeTailUp             ; - 00 - the tile is pointing up
  CMP #01
  BEQ SnakeTailDown
  PHA                         ; So it turns out that we need the Left-Right Tail Tile. Push the tile direction onto the
  LDA #$31                    ; the stack, reset the tile to Left-Right using the Y register, then pull the tile
  STA ptr_SNAKE_HEAD_Y, Y     ; direction back into the accumulator and continue setting the tile attributes
  PLA
  CMP #02
  BEQ SnakeTailLeft
  CMP #03
  BEQ SnakeTailRight

SnakeTailUp:
SnakeTailLeft:
  PLA                         ; Get the custom attributes. We don't need any others.
  JMP SetSnakeTailAttrs

SnakeTailDown:
SnakeTailRight:
  PLA                         ; Get the custom attributes.
  ORA #%11000000              ; Merge in the flip bits
  JMP SetSnakeTailAttrs

SetSnakeTailAttrs:
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new Tile Attrs
  INX

UpdateTailXPosition:
                              ; Update the X Position:
  LDA ptr_SNAKE_TSSD_X        ; - Get the new X Position
  STA ptr_SNAKE_HEAD_Y, X     ; - Set the new X Position

UpdateSnakeDirection:
  LDA ptr_SNAKE_DIR_NXT       ; Now that we're done moving the snake, set the snake's new current direction
  STA ptr_SNAKE_DIR_CUR

  RTS                         ; Return from sub-routine
; ----------------------------------------------------------------------------------------------------------------------


; ----------------------------------------------------------------------------------------------------------------------
; DETECT COLLISIONS
; Handles the logic for detecting a collision and then doing something about it.
; Expected Memory State:
;   This logic runs before the snake has "actually" moved, so the current Snake Head is not useful. Instead, the snake's
;   next head position should be stored in the TSSD. The values in the TSSD will be used to detect a collision with the
;   snake's body, the apple or the world edges.
DetectCollisions:

  LDA ptr_SNAKE_HEAD_X        ; Start by comparing the X Position of the Snake Head and Apple
  EOR ptr_APPLE_X
  STA ptr_COLLISION_FLAG      ; We'll borrow the frame counter because it's reset to 0 once the snake is done moving

  LDA ptr_SNAKE_HEAD_Y        ; Then compare the Y Position of the Snake Head and Apple
  EOR ptr_APPLE_Y

  ORA ptr_COLLISION_FLAG      ; Then OR them together
  BEQ HandleAppleCollision    ; If the result is 0, then the snake has eaten the apple

  RTS                         ; No collisions detected: Return from sub-routine

HandleAppleCollision:
  LDA ptr_GAME_FLAGS          ; Set the Grow Bit
  ORA #flag_GROW
  STA ptr_GAME_FLAGS

  RTS                         ; Return from sub-routine

HandleOtherCollision:
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
  LDA ptr_SNAKE_DIR_CUR       ; The simplest scenario is travelling in a straight line
  CMP ptr_SNAKE_DIR_NXT
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
  STA ptr_SNAKE_TSSD_TILE
  LDA #%00000000              ; Turn body upward
  STA ptr_SNAKE_TSSD_ATTR
  RTS                         ; Return from sub-routine
NextTileStraight_Down:
  LDA #$20                    ; Use the Up-Down Snake Body
  STA ptr_SNAKE_TSSD_TILE
  LDA #%10001000              ; Turn body downward
  STA ptr_SNAKE_TSSD_ATTR
  RTS                         ; Return from sub-routine
NextTileStraight_Left:
  LDA #$21                    ; Use the Left-Right Snake Body
  STA ptr_SNAKE_TSSD_TILE
  LDA #%00010000              ; Turn body leftward
  STA ptr_SNAKE_TSSD_ATTR
  RTS                         ; Return from sub-routine
NextTileStraight_Right:
  LDA #$21                    ; Use the Left-Right Snake Body
  STA ptr_SNAKE_TSSD_TILE
  LDA #%01011000              ; Turn body rightward
  STA ptr_SNAKE_TSSD_ATTR
  RTS                         ; Return from sub-routine

NextTileTurn:
  LDA #$12                    ; All of our turns use the same tile. The real complexity comes in knowing how to flip it.
  STA ptr_SNAKE_TSSD_TILE

  LDA ptr_SNAKE_DIR_NXT       ; Get the next direction
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
  LDY ptr_SNAKE_DIR_CUR       ; Get the next direction
  CPY #03
  BEQ NextTileTurn_LeftToUp
  CPY #04
  BEQ NextTileTurn_RightToUp

NextTileTurn_ToDown:
  LDA #%00001000
  LDY ptr_SNAKE_DIR_CUR       ; Get the next direction
  CPY #03
  BEQ NextTileTurn_LeftToDown
  CPY #04
  BEQ NextTileTurn_RightToDown

NextTileTurn_ToLeft:
  LDA #%00010000
  LDY ptr_SNAKE_DIR_CUR       ; Get the next direction
  CPY #01
  BEQ NextTileTurn_UpToLeft
  CPY #02
  BEQ NextTileTurn_DownToLeft

NextTileTurn_ToRight:
  LDA #%00011000
  LDY ptr_SNAKE_DIR_CUR       ; Get the next direction
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
  STA ptr_SNAKE_TSSD_ATTR
  RTS                         ; Return from sub-routine
; ----------------------------------------------------------------------------------------------------------------------



  .bank 1
  .org $E000
palette:
  .db $0F,$31,$32,$33         ; background palette (00)
  .db $0F,$35,$36,$37         ; background palette (01)
  .db $0F,$39,$3A,$3B         ; background palette (10)
  .db $0F,$3D,$3E,$0F         ; background palette (11)
  .db $0F,$31,$06,$0C         ; snake palette (00)
  .db $0F,$19,$16,$18         ; apple palette (01)
  .db $0F,$1C,$15,$14         ; foreground palette (10)
  .db $0F,$02,$38,$3C         ; foreground palette (11)

sprites:
     ;vert tile attr horiz
  .db $80, $01, %00000001, $80      ; Apple
  .db $68, $10, %00000000, $80      ; Snake Head
  .db $70, $20, %00000000, $80      ; Snake Body
  .db $78, $30, %00000000, $80      ; Snake Tail

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