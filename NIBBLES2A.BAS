'
'
'                  Copyright (C) Microsoft Corporation 1990
'
' Nibbles is a game for one or two players.  Navigate your snakes
' around the game board trying to eat up numbers while avoiding
' running into walls or other snakes.  The more numbers you eat up,
' the more points you gain and the longer your snake becomes.
'
' To run this game, press Shift+F5.
'
' To exit QBasic, press Alt, F, X.
'
' To get help on a BASIC keyword, move the cursor to the keyword and press
' F1 or click the right mouse button.
'

'Set default data type to integer for faster game play
DEFINT A-Z

'User-defined TYPEs
TYPE snakeBody
    row AS INTEGER
    col AS INTEGER
END TYPE

'This type defines the player's snake
TYPE snaketype
    head AS INTEGER
    length AS INTEGER
    row AS INTEGER
    col AS INTEGER
    direction AS INTEGER
    lives AS INTEGER
    score AS INTEGER
    scolor AS INTEGER
    alive AS INTEGER
END TYPE

'This type is used to represent the playing screen in memory
'It is used to simulate graphics in text mode, and has some interesting,
'and slightly advanced methods to increasing the speed of operation.
'Instead of the normal 80x25 text graphics using chr$(219) "€", we will be
'using chr$(220)"‹" and chr$(223) "ﬂ" and chr$(219) "€" to mimic an 80x50
'pixel screen.
'Check out sub-programs SET and POINTISTHERE to see how this is implemented
'feel free to copy these (as well as arenaType and the DIM ARENA stmt and the
'initialization code in the DrawScreen subprogram) and use them in your own
'programs
TYPE arenaType
    realRow AS INTEGER 'Maps the 80x50 point into the real 80x25
    acolor AS INTEGER 'Stores the current color of the point
    sister AS INTEGER 'Each char has 2 points in it.  .SISTER is
END TYPE '-1 if sister point is above, +1 if below

'Sub Declarations
DECLARE SUB SpacePause (text$)
DECLARE SUB PrintScore (NumPlayers%, score1%, score2%, lives1%, lives2%, E$, PlayerAlive)
DECLARE SUB Intro ()
DECLARE SUB GetInputs (NumPlayers, speed, diff$, E$, falsenum$, game$, sclr1, sclr2, gspeed)
DECLARE SUB DrawScreen ()
DECLARE SUB NibblesMenu (NumPlayers%, speed%, diff$, E$, falsenum$, game$, sclr1, sclr2, fp)
DECLARE SUB PlayTimeTrial (NumPlayers%, speed%, sclr1%, StartTime!, TimePast!)
DECLARE SUB PlayNibbles (NumPlayers, speed, diff$, E$, falsenum$, sclr1, sclr2)
DECLARE SUB Set (row, col, acolor)
DECLARE SUB Center (row, text$)
DECLARE SUB DoIntro ()
DECLARE SUB Initialize ()
DECLARE SUB SparklePause ()
DECLARE SUB Level (WhatToDO, sammy() AS snaketype)
DECLARE SUB InitColors ()
DECLARE SUB EraseSnake (snake() AS ANY, snakeBod() AS ANY, snakeNum%)
DECLARE SUB HighScore (score1%, score2%, NumPlayers, E$)
DECLARE SUB Intructions ()
DECLARE SUB ShowBox (text$)
DECLARE SUB TimerClock (t$, StartTime!, TimePast!)
DECLARE SUB HighTimeX (score1%)
DECLARE FUNCTION StillWantsToPlay ()
DECLARE FUNCTION PointIsThere (row, col, backColor)

'Constants
CONST True = -1
CONST False = NOT True
CONST MAXSNAKELENGTH = 1000
CONST STARTOVER = 1 ' Parameters to 'Level' SUB
CONST SAMELEVEL = 2
CONST NEXTLEVEL = 3

'Global Variables
DIM SHARED arena(1 TO 50, 1 TO 80) AS arenaType
DIM SHARED CurLevel, colorTable(10)
DIM Option$(1 TO 4)

RANDOMIZE TIMER
GOSUB ClearKeyLocks
Intro
DO
    NibblesMenu NumPlayers, speed, diff$, E$, falsenum$, game$, sclr1, sclr2, fp
    GOSUB SetColors
    DrawScreen
    IF game$ = "Nibbles" THEN
        DO
            PlayNibbles NumPlayers, speed, diff$, E$, falsenum$, sclr1, sclr2
        LOOP WHILE StillWantsToPlay
    END IF
    IF game$ = "Time" THEN
        DO
            ON TIMER(1) GOSUB TimeUpdate
            timesy = 61
            CLS
            PRINT "Time Remaining: "; TIME$
            StartTime! = TIMER
            TimePast! = 0
            PlayTimeTrial NumPlayers, speed, sclr1, StartTime!, TimePast!
            TIMER OFF
        LOOP WHILE StillWantsToPlay
    END IF
LOOP

TimeUpdate:
COLOR 13, 9
timesy = timesy - 1
LOCATE 1, 1: PRINT "Time Remaining: "; timesy
IF timesy = 0 THEN t$ = "time over"
RETURN

ClearKeyLocks:
DEF SEG = 0 ' Turn off CapLock, NumLock and ScrollLock
KeyFlags = PEEK(1047)
POKE 1047, &H0
DEF SEG
RETURN

SetColors:
IF monitor$ = "M" THEN
    RESTORE mono
ELSE
    RESTORE normal
END IF

FOR a = 1 TO 6
    READ colorTable(a)
NEXT a
RETURN

'snake1     snake2   Walls  Background  Dialogs-Fore  Back
mono: DATA 15,7,7,0,15,0
normal: DATA 14,13,12,1,15,4
END

'Center:
'  Centers text on given row
SUB Center (row, text$)
LOCATE row, 41 - LEN(text$) / 2
PRINT text$;
END SUB

'DrawScreen:
'  Draws playing field
SUB DrawScreen

'initialize screen
VIEW PRINT
COLOR colorTable(1), colorTable(4)
CLS

'Print title & message
Center 1, "Hung and Ron's"
Center 3, "New Orlean Nibbles!"
Center 11, "Creating Level..."
    
'Initialize arena array
FOR row = 1 TO 50
    FOR col = 1 TO 80
        arena(row, col).realRow = INT((row + 1) / 2)
        arena(row, col).sister = (row MOD 2) * 2 - 1
    NEXT col
NEXT row
END SUB

'EraseSnake:
'  Erases snake to facilitate moving through playing field
SUB EraseSnake (snake() AS snaketype, snakeBod() AS snakeBody, snakeNum)

FOR C = 0 TO 9
    FOR b = snake(snakeNum).length - C TO 0 STEP -10
        tail = (snake(snakeNum).head + MAXSNAKELENGTH - b) MOD MAXSNAKELENGTH
        Set snakeBod(tail, snakeNum).row, snakeBod(tail, snakeNum).col, colorTable(4)
    NEXT b
NEXT C
    
END SUB

'GetInputs:
'  Gets player inputs
SUB GetInputs (NumPlayers, speed, diff$, E$, falsenum$, game$, sclr1, sclr2, gspeed)
ExitOp$ = "Exit Options Menu"
COLOR 7, 0
CLS
LOCATE 5, 10: PRINT "Number of players:"
LOCATE 7, 10: PRINT "Skill level (1 to 100):"
LOCATE 8, 15: PRINT "10  = Beginner or Chicken"
LOCATE 9, 15: PRINT "70  = Really, Really Good"
LOCATE 10, 15: PRINT "100 = Are you crazy???"
LOCATE 13, 10: PRINT "Increase game speed during play:"
LOCATE 15, 10: PRINT "Do you want infinite lives? (y/n)"
LOCATE 17, 10: PRINT "Do you want false numbers in the game? (y/n)"
COLOR 14
LOCATE 5, 60: PRINT NumPlayers
COLOR 9
LOCATE 7, 60: PRINT gspeed
LOCATE 13, 61: PRINT diff$
LOCATE 15, 61: PRINT E$
LOCATE 17, 61: PRINT falsenum$
LOCATE 19, 10: PRINT ExitOp$
COLOR 15, 2: Center 1, "O P T I O N S   M E N U": COLOR 9, 0
COLOR 15: Center 3, "Press Spacebar to Select": COLOR 9
ochoice = 1

DO
    kybd$ = INKEY$
    SELECT CASE kybd$
        CASE CHR$(0) + "H": opc = ochoice: ochoice = ochoice - 1
        CASE CHR$(0) + "P": opc = ochoice: ochoice = ochoice + 1
        CASE CHR$(32), CHR$(13): coolpress = 1
    END SELECT
    COLOR 9
    IF coolpress <> 1 THEN
        IF opc = 1 THEN LOCATE 5, 60: PRINT NumPlayers
        IF opc = 2 THEN LOCATE 7, 60: PRINT gspeed
        IF opc = 3 THEN LOCATE 13, 61: PRINT diff$
        IF opc = 4 THEN LOCATE 15, 61: PRINT E$
        IF opc = 5 THEN LOCATE 17, 61: PRINT falsenum$
        IF opc = 6 THEN LOCATE 19, 10: PRINT ExitOp$
    END IF
    COLOR 14
    IF ochoice = 0 THEN ochoice = 6
    IF ochoice = 7 THEN ochoice = 1
    IF ochoice = 1 THEN GOSUB Players
    IF ochoice = 2 THEN GOSUB gspeed
    IF ochoice = 3 THEN GOSUB IncSpeed
    IF ochoice = 4 THEN GOSUB Infinite
    IF ochoice = 5 THEN GOSUB False
    IF ochoice = 6 THEN GOSUB ExitOptionsMenu
    IF coolpress = 1 THEN coolpress = 0
LOOP

Players:
IF coolpress = 1 THEN NumPlayers = ABS(NumPlayers - 3)
LOCATE 5, 60: PRINT NumPlayers
RETURN

gspeed:
IF coolpress = 1 THEN
    DO
        LOCATE 7, 55: PRINT "               "
        LOCATE 7, 59: INPUT gspeed
    LOOP UNTIL gspeed > 0 AND gspeed < 101
    LOCATE 7, 55: PRINT "               "
    speed = gspeed
    speed = (100 - speed) * 2 + 1
    StartTime# = TIMER ' Calculate speed of system
    FOR i# = 1 TO 1000: NEXT i# ' and do some compensation
    stopTime# = TIMER
    speed = 100 * speed * .5 / (1000 + stopTime# - StartTime#)
END IF
LOCATE 7, 60: PRINT gspeed
RETURN

IncSpeed:
IF coolpress = 1 AND diff$ = "Y" THEN diff$ = "N": coolpress = 0
IF coolpress = 1 AND diff$ = "N" THEN diff$ = "Y"
LOCATE 13, 61: PRINT diff$
22 RETURN

Infinite:
IF coolpress = 1 AND E$ = "N" THEN E$ = "Y": coolpress = 0
IF coolpress = 1 AND E$ = "Y" THEN E$ = "N"
LOCATE 15, 61: PRINT E$
RETURN

False:
IF coolpress = 1 AND falsenum$ = "Y" THEN falsenum$ = "N": coolpress = 0
IF coolpress = 1 AND falsenum$ = "N" THEN falsenum$ = "Y"
LOCATE 17, 61: PRINT falsenum$
RETURN

ExitOptionsMenu:
IF coolpress = 1 THEN EXIT SUB
LOCATE 19, 10: PRINT ExitOp$
RETURN

END SUB

SUB HighScore (score1, score2, NumPlayers, E$)
   
score1 = score1 * 10 'compensate for the way the game runs
score2 = score2 * 10
high1 = 0
high2 = 0

IF E$ = "y" THEN EXIT SUB
'initialize screen
CLS

DIM a$(16), b(16), C$(16)
F$ = "Highscor.e"

OPEN F$ FOR INPUT AS #1
FOR x = 1 TO 15
    INPUT #1, a$(x) 'Read entries from the file.
    INPUT #1, b(x)
    INPUT #1, C$(x)
NEXT x
CLOSE #1

IF NumPlayers = 1 THEN GOTO 20
IF score2 < b(15) THEN GOTO 10
FOR x = 14 TO 1 STEP -1
    IF score2 > b(x) THEN b(x + 1) = b(x): a$(x + 1) = a$(x): C$(x + 1) = C$(x)
    IF score2 > b(x) AND x = 1 THEN b(x) = score2: v = 0: EXIT FOR
    IF score2 < b(x) OR score2 = b(x) THEN b(x + 1) = score2: v = x: x = 1
NEXT x
   
'Print title & message
LOCATE 10, 25: PRINT "Player 2 score made the top 15!"
LOCATE 11, 25: INPUT "Please enter your name Player 2"; Name2$
LOCATE 12, 25: PRINT "Press key for Player 1 score..."
SLEEP
a$(v + 1) = Name2$
C$(v + 1) = DATE$
high1 = v + 1
GOTO 20

10 Center 10, "Player 2 score did not make the top 15,"
Center 11, "Please Try Again!  Press key to continue"
SLEEP
  
20 CLS
IF score1 < b(15) THEN GOTO 60
FOR x = 14 TO 1 STEP -1
    IF score1 > b(x) THEN b(x + 1) = b(x): a$(x + 1) = a$(x)
    IF score1 > b(x) AND x = 1 THEN b(x) = score1: v = 0: EXIT FOR
    IF score1 < b(x) OR score1 = b(x) THEN b(x + 1) = score1: v = x: x = 1
NEXT x
   
IF NumPlayers = 1 THEN LOCATE 10, 25: PRINT "Your score made the top 15!": LOCATE 11, 25: INPUT "Please enter your name"; Name1$
IF NumPlayers = 2 THEN LOCATE 10, 25: PRINT "Player 1 score made the top 15!": LOCATE 11, 25: INPUT "Please enter your name Player 1"; Name1$
Center 12, "Please wait for the top scores ..."
Center 13, "Press key to continue ..."
a$(v + 1) = Name1$
C$(v + 1) = DATE$
high2 = v + 1
GOTO 70

60 Center 10, "Sorry Player 1 score did not make the top 15,"
Center 11, "Please Try Again! Press key to continue."

70 'opening file highscor.e for output
   
OPEN F$ FOR OUTPUT AS #1
FOR x = 1 TO 15
    WRITE #1, a$(x)
    WRITE #1, b(x)
    WRITE #1, C$(x)
NEXT x
CLOSE #1
SLEEP

OPEN F$ FOR INPUT AS #1
x = 1
DO WHILE NOT EOF(1)
    INPUT #1, a$(x) 'Read entries from the file.
    INPUT #1, b(x)
    INPUT #1, C$(x)
    x = x + 1
LOOP
v = x

' play music
PLAY "MBo3L8ED+ED+Eo2Bo3DCL2o2A": ' play "o2l16e-9n0e-d-d-n0e-n0e-n0e-d-d-d-n0e-9n0e-d-d-n0e-"

' Print High Score Table
COLOR 10, 1
CLS
Center 2, "NIBBLES 5.3  *** HIGH SCORE TABLE TOP 15 ***"
COLOR 13
LOCATE 4, 12: PRINT "NAME AND PLACE": LOCATE 4, 42: PRINT "Total Score": LOCATE 4, 58: PRINT "Date"
LOCATE 5, 12: PRINT "--------------": LOCATE 5, 42: PRINT "-----------": LOCATE 5, 58: PRINT "----"
Center 22, "Press any key to continue   ***   (C) 1994 Hung & Ron"
COLOR 15
FOR x = 1 TO 15
    IF x = high1 OR x = high2 THEN COLOR 31
    b$ = STR$(b(x))
    LOCATE x + 5, 12: PRINT x; ". "; a$(x): LOCATE x + 5, 50 - LEN(b$): PRINT b(x): LOCATE x + 5, 58: PRINT C$(x)
    IF x = high1 OR x = high2 THEN COLOR 15
NEXT x
CLOSE #1
SLEEP
END SUB

SUB HighTimeX (score1)
  
score1 = score1 * 10 'compensate for the way the game runs
high2 = 0
'initialize screen
CLS

DIM a$(11), b(11), C$(11)
p$ = "Highscor.et"

OPEN p$ FOR INPUT AS #1
FOR x = 1 TO 10
    INPUT #1, a$(x) 'Read entries from the file.
    INPUT #1, b(x)
    INPUT #1, C$(x)
NEXT x
CLOSE #1
CLS
IF score1 < b(10) THEN GOTO 63
FOR x = 9 TO 1 STEP -1
    IF score1 > b(x) THEN b(x + 1) = b(x): a$(x + 1) = a$(x): C$(x + 1) = C$(x)
    IF score1 > b(x) AND x = 1 THEN b(x) = score1: v = 0: EXIT FOR
    IF score1 < b(x) OR score1 = b(x) THEN b(x + 1) = score1: v = x: x = 1
NEXT x
  
LOCATE 10, 25: COLOR 15: PRINT "Your score made the top 15!": LOCATE 11, 25: INPUT "Please enter your name"; Name1$
Center 12, "Please wait for the top scores ..."
Center 13, "Press key to continue ..."
a$(v + 1) = Name1$
C$(v + 1) = DATE$
high2 = v + 1
GOTO 73

63 COLOR 15
Center 10, "Sorry your score did not make the top 15,"
Center 11, "Please Try Again! Press key to continue."

73 'opening file highscor.e for output
  
OPEN p$ FOR OUTPUT AS #1
FOR x = 1 TO 10
    WRITE #1, a$(x)
    WRITE #1, b(x)
    WRITE #1, C$(x)
NEXT x
CLOSE #1
SLEEP

OPEN p$ FOR INPUT AS #1
x = 1
DO WHILE NOT EOF(1)
    INPUT #1, a$(x) 'Read entries from the file.
    INPUT #1, b(x)
    INPUT #1, C$(x)
    x = x + 1
LOOP
v = x

' play music
PLAY "MBo3L8ED+ED+Eo2Bo3DCL2o2A": ' play "o2l16e-9n0e-d-d-n0e-n0e-n0e-d-d-d-n0e-9n0e-d-d-n0e-"

' Print High Score Table
COLOR 10, 1
CLS
Center 2, "NIBBLES 5.3  *** HIGH SCORE TABLE TOP 10 ***"
COLOR 13
LOCATE 4, 12: PRINT "NAME AND PLACE": LOCATE 4, 42: PRINT "Total Score": LOCATE 4, 58: PRINT "Date"
LOCATE 5, 12: PRINT "--------------": LOCATE 5, 42: PRINT "-----------": LOCATE 5, 58: PRINT "----"
Center 22, "Press any key to continue   ***   (C) 1994 Hung & Ron"
COLOR 15
FOR x = 1 TO 10
    IF x = high2 THEN COLOR 31
    b$ = STR$(b(x))
    LOCATE x + 5, 12: PRINT x; ". "; a$(x): LOCATE x + 5, 50 - LEN(b$): PRINT b(x): LOCATE x + 5, 58: PRINT C$(x)
    IF x = high2 THEN COLOR 15
NEXT x
CLOSE #1
SLEEP
END SUB

'InitColors:
'Initializes playing field colors
SUB InitColors
    
FOR row = 1 TO 50
    FOR col = 1 TO 80
        arena(row, col).acolor = colorTable(4)
    NEXT col
NEXT row

CLS
   
'Set (turn on) pixels for screen border
FOR col = 1 TO 80
    Set 3, col, colorTable(3)
    Set 50, col, colorTable(3)
NEXT col

FOR row = 4 TO 49
    Set row, 1, colorTable(3)
    Set row, 80, colorTable(3)
NEXT row

END SUB

'Intro:
'  Displays game introduction
SUB Intro

SCREEN 0
WIDTH 80, 25
COLOR , 0
CLS
COLOR 12, 0
Center 2, "QBASIC   HYPER   TURBO"
WHILE x < 3000: x = x + 1
WEND
PLAY "MBo3L8ED+ED+Eo2Bo3DCL2o2A": PLAY "MBT160o1l10ccdedcdl4ecc"
LOCATE 4
COLOR 10
PRINT "      ‹    ‹  ‹‹‹‹‹  ‹‹‹‹   ‹‹‹‹   ‹      ‹‹‹‹‹   ‹‹‹‹"
FOR x = 1 TO 9000: NEXT x
PRINT "      €ﬂ‹  €    €    €   €  €   €  €      €      €    ﬂ"
FOR x = 1 TO 9000: NEXT x
PRINT "      € ﬂ‹ €    €    €‹‹‹ﬂ  €‹‹‹ﬂ  €      €‹‹‹‹  ﬂ‹‹‹‹"
FOR x = 1 TO 9000: NEXT x
PRINT "      €  ﬂ‹€    €    €   €  €   €  €      €           €"
FOR x = 1 TO 9000: NEXT x
PRINT "      €    €  ‹‹€‹‹  €‹‹‹ﬂ  €‹‹‹ﬂ  €‹‹‹‹  €‹‹‹‹  ﬂ‹‹‹‹ﬂ"
FOR x = 1 TO 9000: NEXT x

LOCATE 12: COLOR 13
PRINT "                   ∞∞∞∞∞∞∞∞∞∞∞"
PRINT "∞∞∞∞            ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
PRINT "∞∞∞∞∞∞        ∞∞∞∞∞    ∞∞    ∞∞∞∞∞"
PRINT "∞∞∞∞∞∞∞∞∞∞   ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞ ∞∞∞∞∞∞∞∞∞∞ ∞∞∞∞"
PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞          ∞∞∞∞"
PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
PRINT " ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
FOR x& = 1 TO 30000: NEXT x&

LOCATE 12: COLOR 13
PRINT "                   ±±±±±±±±±±±"
PRINT "±±±±            ±±±±±±±±±±±±±±±±"
PRINT "±±±±±±        ±±±±±    ±±    ±±±±±"
PRINT "±±±±±±±±±±   ±±±±±±±±±±±±±±±±±±±±±±"
PRINT "±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±"
PRINT "±±±±±±±±±±±±±±±±±± ±±±±±±±±±± ±±±±"
PRINT "±±±±±±±±±±±±±±±±±±±          ±±±±"
PRINT "±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±"
PRINT "±±±±±±±±±±±±±±±±±±±±±±±±±±±±"
PRINT " ±±±±±±±±±±±±±±±±±±±±±±±±"
FOR x& = 1 TO 30000: NEXT x&
                       
LOCATE 12: COLOR 13
PRINT "‹                  ‹€€€€€€€€€‹"
PRINT "€€€‹            ‹€€€€€€€€€€€€€€‹"
PRINT "€€€€€‹        ‹€€€€    €€    €€€€‹"
PRINT "€€€€€€€€‹‹   ‹€€€€€€€€€€€€€€€€€€€€‹"
PRINT "€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€"
COLOR 13, 1
PRINT "€€€€€€€€€€€€€€€€€€ €€€€€€€€€€ €€€€"
PRINT "€€€€€€€€€€€€€€€€€€€          €€€": COLOR 13, 0: LOCATE 18, 33: PRINT "ﬂ"
LOCATE 19
PRINT "€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€ﬂ"
PRINT "€€€€€€€€€€€€€€€€€€€€€€€€€€ﬂﬂ"
PRINT " €€€€€€€€€€€€€€€€€€€€€€€ﬂ"
COLOR 11
LOCATE 14, 20: PRINT "€ﬂﬂ€": LOCATE 14, 26: PRINT "€ﬂﬂ€"

FOR x = 69 TO 50 STEP -1
    LOCATE 11, x + 1
    COLOR 0
    PRINT "Version 5.3"
    LOCATE 11, x
    COLOR 14
    PRINT "Version 5.3"
    FOR y = 1 TO 9000: NEXT y
NEXT x

FOR x = 1 TO 16
    COLOR x
    IF x = 16 THEN COLOR 14
    LOCATE 11, 50: PRINT "Version 5.3"
    FOR y = 1 TO 8000: NEXT y
NEXT x

COLOR 15
LOCATE 15, 38: PRINT "Copyright (C) Microsoft Corporation 1990"
LOCATE 16, 38: PRINT "Expanded By Hung Phan and Ron Yeh"

PLAY "MBT160O1L8cabdeacfecCL10DEDCDL4ECC"

Center 23, "Press any key to play, I for Instructions"

SparklePause

END SUB

SUB Intructions
'  Displays game introduction
SCREEN 0
WIDTH 80, 25
COLOR 12, 0
CLS
a$ = "*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *"
b$ = "  *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *  "
Center 3, "QBASIC   HYPER   TURBO   NIBBLES - Instructions"
COLOR 13
LOCATE 2, 3: PRINT "VERSION 5.3"
COLOR 15
Center 5, "Copyright (C) Microsoft Corporation 1990"
Center 6, "Expanded By Hung Phan & Ron Yeh"
COLOR 7
Center 8, "Nibbles is a game for one or two players.  Navigate your snakes"
Center 9, "around the game board trying to eat up numbers while avoiding"
Center 10, "running into walls or other snakes.  The more numbers you eat up,"
Center 11, "the more points you gain and the longer your snake becomes."
Center 13, " Game Controls "
Center 15, "  General             Player 1               Player 2    "
Center 16, "            (Up & Left)  (Up & Right)        (Up)      "
Center 17, "P - Pause              \  /                    R       "
Center 18, "                                            (Left) D   G (Right)  "
Center 19, "                       /  \                    V       "
Center 20, "            (Down & left) (Down & Right)     (Down)     "
Center 24, "Press any key to continue or I for further Intructions"

COLOR 11 ' print border
DO
    Center 1, a$
    Center 22, a$
    kbd$ = UCASE$(INKEY$)
    FOR x = 1 TO 50: NEXT x
    Center 1, b$
    Center 22, b$
LOOP UNTIL kbd$ = "I" OR kdb$ = "i" OR kbd$ <> ""
IF kbd$ <> "I" AND kbd$ <> "i" THEN EXIT SUB


COLOR 12, 0
CLS
Center 3, "QBASIC  NIBBLES  5.3  *  EXTENDED  INTRUCTIONS"
COLOR 15
Center 5, "Copyright (C) 1994 Hung Phan - Greatly Improved by Ron Yeh"
COLOR 7
Center 7, "Now with Nibbles Version 5.3, you can move diagonally as well"
Center 8, "as the basic right, left, up and down.  Also there are two new"
Center 9, "keys for each snake.  They are the speed up and speed down keys."
Center 10, "For player one, the '[' or '{' will slow the snake down while"
Center 11, "the ']' or '}' key will speed your snake up.  For player two"
Center 12, "the '`' (the key next to the 1) will speed down; the '1' key"
Center 13, "will speed the second snake up. Once again, the general keys:"
Center 15, "  General             Player 1               Player 2    "
Center 16, "                               (Up)          (Left & Up)  (Right & Up)  "
Center 17, "P - Pause                " + CHR$(24) + "                    E   T     "
Center 18, "                     (Left) " + CHR$(27) + "   " + CHR$(26) + " (Right)                        "
Center 19, "                         " + CHR$(25) + "                    C   B     "
Center 20, "                             (Down)        (Left & Down)  (Right & Down"
Center 24, "Press any key to continue or I for further Intructions"

COLOR 11 ' print border
DO
    Center 1, a$
    Center 22, a$
    kbd$ = UCASE$(INKEY$)
    FOR x = 1 TO 50: NEXT x
    Center 1, b$
    Center 22, b$
LOOP UNTIL kbd$ = "I" OR kdb$ = "i" OR kbd$ <> ""
IF kbd$ <> "I" AND kbd$ <> "i" THEN EXIT SUB

COLOR 12, 0
CLS
Center 3, "QBASIC  NIBBLES  5.3  *  EXTENDED  INTRUCTIONS"
COLOR 15
Center 5, "Copyright (C) 1994 Hung Phan - Greatly Improved by Ron Yeh"
COLOR 7
Center 7, "There is also a new general key.  The key is 'm' which diverts"
Center 8, "the control of both snakes to the Player 1 control pad. To do"
Center 9, "this, select 2 players and then press 'm' during regular play."
Center 10, "You can now also have a infinite number of lives.  Of course, if"
Center 11, "you choose this, you can not enter the high score list. To quit"
Center 12, "during a game press '9' four times. But if you quit during the"
Center 13, "game you will not be allowed to enter the High Score.  Also new"
Center 14, "with Version 5.3: If one player loses all of his lives, the"
Center 15, "other player can continue on with the number of lives he still"
Center 16, "has left, making it more fair, if the other snake is suicidal."
Center 18, "Newer versions will be coming out soon.  Enjoy !!!"
Center 24, "Press any key to play or I for further instructions"

COLOR 11 ' print border
DO
    Center 1, a$
    Center 22, a$
    kbd$ = UCASE$(INKEY$)
    FOR x = 1 TO 50: NEXT x
    Center 1, b$
    Center 22, b$
LOOP UNTIL kbd$ = "I" OR kdb$ = "i" OR kbd$ <> ""
IF kbd$ <> "I" AND kbd$ <> "i" THEN EXIT SUB

COLOR 12, 0
CLS
Center 3, "QBASIC  NIBBLES  5.3  *  EXTENDED  INTRUCTIONS"
COLOR 15
Center 5, "Expanded By Hung Phan and Ron Yeh"
COLOR 7
Center 7, "The `S' key controls whether there is sound or not.  To turn off"
Center 8, "the sound, press `S' during play.  Press it again to reactivate"
Center 9, "sound.  You also get a free snake for every 3000 pts. you earn."
Center 10, "At the Main Menu, you can choose your snake's color.  Keys 1-7"
Center 11, "control player 1's color.  Keys q,w,e,r,t,y & u control player"
Center 12, "2's color.  A new feature in 5.3 is TimeTrials.  In TimeTrials,"
Center 13, "you try to eat as many objects as you can in the set amount of"
Center 14, "time.  Send comments, questions, or new ideas to:"
Center 15, "161 W. Norwood Place -- San Gabriel CA 91776"
Center 18, "Letter Bombs will be returned to sender. Thank you!"
Center 20, "If you find any glitches or bugs, tell me."
Center 24, "Press any key to play SUPER NIBBLES!!!"

COLOR 11 ' print border
DO
    Center 1, a$
    Center 22, a$
    kbd$ = UCASE$(INKEY$)
    FOR x = 1 TO 50: NEXT x
    Center 1, b$
    Center 22, b$
LOOP UNTIL kbd$ <> ""
   
END SUB

'Level:
'Sets game level
SUB Level (WhatToDO, sammy() AS snaketype) STATIC
    
SELECT CASE (WhatToDO)

    CASE STARTOVER
        CurLevel = 1
    CASE NEXTLEVEL
        CurLevel = CurLevel + 1
END SELECT

sammy(1).head = 1 'Initialize Snakes
sammy(1).length = 2
sammy(1).alive = True
sammy(2).head = 1
sammy(2).length = 2
sammy(2).alive = True

InitColors
IF CurLevel < 19 THEN GOTO 50
ronLevel = CurLevel
IF CurLevel / 2 = CurLevel \ 2 AND CurLevel > 19 THEN CurLevel = 100 ELSE CurLevel = 101

50 SELECT CASE CurLevel
    CASE 1
        FOR i = 35 TO 45
            FOR k = 21 TO 29
                Set k, i, colorTable(3)
            NEXT k
        NEXT i
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 4: sammy(2).direction = 3
 
    CASE 2
        FOR i = 10 TO 50 STEP 2
            Set i, 20, colorTable(3)
            Set i, 60, colorTable(3)
            Set i, i, colorTable(3)

        NEXT i
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 1: sammy(2).direction = 1

    CASE 3
        FOR i = 20 TO 60 STEP 4
            FOR k = 10 TO 40 STEP 4
                Set k, i, colorTable(3)
                FOR C = 1 TO 4
                    v = k - 1 + C
                    Set v, 5, colorTable(3)
                    Set v, 75, colorTable(3)
                NEXT C
            NEXT k
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 60: sammy(2).col = 20
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 4
        FOR i = 20 TO 60
            FOR k = 10 TO 40 STEP 7
                Set k, i, colorTable(3)
            NEXT k
        NEXT i
        sammy(1).row = 30: sammy(2).row = 30
        sammy(1).col = 5: sammy(2).col = 65
        sammy(1).direction = 4: sammy(2).direction = 3

    CASE 5
        FOR i = 2 TO 60 STEP 2
            Set 38, i, colorTable(3)
            Set 15, 81 - i, colorTable(3)
        NEXT i
        FOR i = 30 TO 50 STEP 2
            Set 26, i, colorTable(6)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 60: sammy(2).col = 21
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 6
        FOR i = 4 TO 48
            IF i > 27 OR i < 26 THEN
                Set i, 10, colorTable(3)
                Set i + 1, 20, colorTable(3)
                Set i, 30, colorTable(3)
                Set i + 1, 40, colorTable(3)
                Set i, 50, colorTable(3)
                Set i + 1, 60, colorTable(3)
                Set i, 70, colorTable(3)
            END IF
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 7
        FOR i = 10 TO 70 STEP 5
            FOR k = 9 TO 45 STEP 5
                Set k, i, colorTable(3)
            NEXT k
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 60: sammy(2).col = 20
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 8
        FOR i = 13 TO 39
            Set i, 21, colorTable(3)
            Set i, 59, colorTable(3)
        NEXT i
        FOR i = 23 TO 57
            Set 11, i, colorTable(3)
            Set 41, i, colorTable(3)
        NEXT i
        FOR i = 5 TO 48
            Set i, 6, colorTable(3)
            Set i, 75, colorTable(3)
        NEXT i
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 1: sammy(2).direction = 2

    CASE 9
        FOR i = 4 TO 49
            IF i > 30 OR i < 23 THEN
                Set i, 10, colorTable(3)
                Set i, 30, colorTable(3)
                Set i, 50, colorTable(3)
                Set i, 70, colorTable(3)
             
            END IF
        NEXT i
        FOR i = 4 TO 49 STEP 2
            Set i, 40, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 10
        FOR i = 5 TO 77 STEP 2
            Set 15, i, colorTable(3)
        NEXT i
        FOR i = 29 TO 38
            Set i, 40, colorTable(3)
        NEXT i
        FOR i = 35 TO 45
            Set 26, i, colorTable(3)
        NEXT i
        FOR i = 27 TO 35
            Set i, 35, colorTable(3)
            Set i, 45, colorTable(3)
        NEXT i
        FOR i = 30 TO 50
            Set 39, i, colorTable(3)
        NEXT i
        FOR i = 30 TO 38
            Set i, 30, colorTable(3)
            Set i, 50, colorTable(3)
        NEXT i
        FOR i = 26 TO 54
            Set 22, i, colorTable(3)
        NEXT i
        FOR i = 23 TO 49 STEP 2
            Set i, 26, colorTable(3)
            Set i, 54, colorTable(3)
        NEXT i
        FOR i = 12 TO 46
            Set i, 4, colorTable(3)
            Set i, 77, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 7
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 11
        FOR i = 4 TO 49 STEP 3
            FOR k = 10 TO 50
                Set i, 40, colorTable(3)
                IF k > 47 OR k < 44 THEN Set k, k, colorTable(3)
                IF k > 47 OR k < 44 THEN Set k, 80 - k, colorTable(3)
            NEXT k
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 12
        FOR i = 4 TO 40 STEP 3
            Set i, 10, colorTable(3)
            Set 53 - i, 20, colorTable(3)
            Set i, 30, colorTable(3)
            Set 53 - i, 40, colorTable(3)
            Set i, 50, colorTable(3)
            Set 53 - i, 60, colorTable(3)
            Set i, 70, colorTable(3)
        NEXT i
        FOR i = 10 TO 70
            FOR k = 10 TO 40 STEP 5
                Set k, i, colorTable(3)
            NEXT k
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 3: sammy(2).direction = 3

    CASE 13
        FOR i = 4 TO 49
            IF i > 6 THEN Set i, i, colorTable(3)
            FOR k = 10 TO 28 STEP 20
                IF i < 47 AND i + k < 49 THEN Set i, i + k, colorTable(3)
                IF i > 6 AND i + k < 39 THEN Set i, i + k + 10, colorTable(3)
            NEXT k
        NEXT i
        FOR i = 8 TO 48 STEP 10
            FOR k = 40 TO 75
                IF i = 8 THEN Set i + 1, k, colorTable(3)
                IF i > 8 AND k > 48 THEN Set i + 1, k, colorTable(3)
                IF i = 8 OR i = 28 AND k > 48 THEN Set i + 1, k + 5, colorTable(3)
            NEXT k
        NEXT i
        FOR i = 1 TO 36
            IF i < 17 THEN Set 23, i, colorTable(3)
            IF i > 6 AND i < 34 THEN Set 33, i, colorTable(3)
            Set 43, i, colorTable(3)
        NEXT i
        sammy(1).row = 40: sammy(2).row = 15
        sammy(1).col = 75: sammy(2).col = 5
        sammy(1).direction = 3: sammy(2).direction = 2

    CASE 14
        FOR i = 6 TO 47
            Set i, 20, colorTable(3)
            Set i, 40, colorTable(3)
            Set i, 60, colorTable(3)
        NEXT i
        FOR i = 5 TO 75
            Set 15, i, colorTable(3)
            Set 25, i, colorTable(3)
            Set 37, i, colorTable(3)
        NEXT i
        Set 15, 30, colorTable(4)
        Set 25, 30, colorTable(4)
        Set 25, 50, colorTable(4)
        Set 37, 50, colorTable(4)
        Set 20, 40, colorTable(4)
        Set 20, 60, colorTable(4)
        Set 32, 20, colorTable(4)
        Set 32, 40, colorTable(4)
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 15
        FOR i = 4 TO 49 STEP 2
            Set i, i, colorTable(3)
            Set i, i + 30, colorTable(3)
            Set i, 80 - i, colorTable(3)
            Set i, 80 - (i + 30), colorTable(3)
            Set i, 25, colorTable(5)
            Set i, 55, colorTable(5)

        NEXT i
        sammy(1).row = 40: sammy(2).row = 15
        sammy(1).col = 75: sammy(2).col = 5
        sammy(1).direction = 1: sammy(2).direction = 2

    CASE 16
        FOR i = 22 TO 58
            Set 18, i, colorTable(3)
            Set 32, i, colorTable(3)
        NEXT i
        FOR i = 19 TO 31
            Set i, 21, colorTable(3)
            Set i, 59, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 17
        FOR i = 2 TO 78 STEP 2
            Set 27, i, colorTable(3)
            Set 28, i, colorTable(3)
            Set 24, i + 1, colorTable(3)
            Set 23, i + 1, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 18
        FOR i = 4 TO 48
            Set i + 1, i - 1, colorTable(3)
            Set i, i + 30, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 100
        FOR i = 6 TO 47 STEP 2
            Set i, 10, colorTable(3)
            Set i + 1, 20, colorTable(3)
            Set i, 30, colorTable(3)
            Set i + 1, 40, colorTable(3)
            Set i, 50, colorTable(3)
            Set i + 1, 60, colorTable(3)
            Set i, 70, colorTable(3)
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1
        CurLevel = ronLevel

    CASE 101
        RANDOMIZE TIMER
        FOR x = 1 TO 100
            DO
                i = INT(RND * 65) + 1
                k = INT(RND * 45) + 4
            LOOP UNTIL i > 0 AND i < 60 AND k > 4 AND k < 50
            Set k, i, colorTable(3)
        NEXT x
        sammy(1).row = 5: sammy(2).row = 45
        sammy(1).col = 65: sammy(2).col = 65
        sammy(1).direction = 2: sammy(2).direction = 1
        CurLevel = ronLevel

END SELECT
END SUB

SUB NibblesMenu (NumPlayers, speed, diff$, E$, falsenum$, game$, sclr1, sclr2, fp)

IF sclr1 = 0 THEN sclr1 = 2
IF sclr2 = 0 THEN sclr2 = 6

5 CLS
' fp=first time that player has played game
IF fp = 0 THEN
    NumPlayers = 1
    gspeed = 40
    speed = gspeed
    speed = (100 - speed) * 2 + 1
    StartTime# = TIMER ' Calculate speed of system
    FOR i# = 1 TO 1000: NEXT i# ' and do some compensation
    stopTime# = TIMER
    speed = 100 * speed * .5 / (1000 + stopTime# - StartTime#)
    diff$ = "N"
    E$ = "N"
    falsenum$ = "N"
    fp = gspeed
ELSE gspeed = fp
END IF
                                                  
COLOR 15, 13: Center 1, "N I B B L E S   M A I N   M E N U"
LOCATE 3, 3: COLOR 15, 0: PRINT "Snake 1: ": LOCATE 3, 12: COLOR sclr1 + 8: PRINT "€"
LOCATE 5, 3: COLOR 15: PRINT "Snake 2: ": LOCATE 5, 12: COLOR sclr2 + 8: PRINT "€"
COLOR 14
Center 10, "S U P E R   N I B B L E S !!!"
COLOR 9
Center 12, "T I M E   T R I A L S"

Center 14, "O P T I O N S"
LOCATE 11, 65: PRINT "‹‹‹‹‹‹‹‹‹"
LOCATE 12, 65: PRINT "›E X I Tﬁ"
LOCATE 13, 65: PRINT "ﬂﬂﬂﬂﬂﬂﬂﬂﬂ"
COLOR 15
Center 18, "Press Enter to Select"

choice = 1
ronschoice = 0
DO
    kbd$ = INKEY$
    SELECT CASE kbd$
        CASE CHR$(0) + "H": pc = choice: choice = choice - 1
        CASE CHR$(0) + "P": pc = choice: choice = choice + 1
        CASE CHR$(0) + "M", CHR$(0) + "K": ronschoice = ABS(ronschoice - 1)
        CASE "1", "2", "3", "4", "5", "6", "7": sclr1 = VAL(kbd$)
        CASE "q": sclr2 = 7
        CASE "w": sclr2 = 6
        CASE "e": sclr2 = 5
        CASE "r": sclr2 = 4
        CASE "t": sclr2 = 3
        CASE "y": sclr2 = 2
        CASE "u": sclr2 = 1
    END SELECT
    LOCATE 3, 12: COLOR sclr1 + 8: PRINT "€"
    LOCATE 5, 12: COLOR sclr2 + 8: PRINT "€"
    COLOR 9
    IF pc = 1 THEN Center 10, "S U P E R   N I B B L E S !!!"
    IF pc = 2 THEN Center 12, "T I M E   T R I A L S"
    IF pc = 3 THEN Center 14, "O P T I O N S"
    IF ronschoice = 1 THEN
        'variable (mathwiz) that controls whether exit option is on or off
        mathwiz$ = "on"
        Center 10, "S U P E R   N I B B L E S !!!"
        Center 12, "T I M E   T R I A L S"
        Center 14, "O P T I O N S"
    END IF
    COLOR 14
    IF mathwiz$ = "on" THEN GOTO 951
    IF choice < 1 THEN choice = 3
    IF choice > 3 THEN choice = 1
    IF choice = 1 THEN
        Center 10, "S U P E R   N I B B L E S !!!"
    END IF
    IF choice = 2 THEN
        Center 12, "T I M E   T R I A L S"
    END IF
    IF choice = 3 THEN
        Center 14, "O P T I O N S"
    END IF
   
    951 IF ronschoice = 1 THEN
        LOCATE 11, 65: PRINT "‹‹‹‹‹‹‹‹‹"
        LOCATE 12, 65: PRINT "›E X I Tﬁ"
        LOCATE 13, 65: PRINT "ﬂﬂﬂﬂﬂﬂﬂﬂﬂ"
    END IF
    IF ronschoice = 0 THEN
        mathwiz$ = "off"
        COLOR 9
        LOCATE 11, 65: PRINT "‹‹‹‹‹‹‹‹‹"
        LOCATE 12, 65: PRINT "›E X I Tﬁ"
        LOCATE 13, 65: PRINT "ﬂﬂﬂﬂﬂﬂﬂﬂﬂ"
    END IF
   
LOOP UNTIL kbd$ = CHR$(13) OR kbd$ = CHR$(32) OR kbd$ = CHR$(27)

IF kbd$ = CHR$(27) THEN
    CLS
    END
END IF
IF ronschoice = 1 THEN
    COLOR 7, 0
    CLS
    DEF SEG = 0 ' Restore CapLock, NumLock and ScrollLock states
    POKE 1047, KeyFlags
    DEF SEG
    Center 13, "Thank You for Playing Nibbles 5.3!"
    END
END IF

IF choice = 3 THEN
    C = 1
    GetInputs NumPlayers, speed, diff$, E$, falsenum$, game$, sclr1, sclr2, gspeed
    fp = gspeed
    GOTO 5
END IF
IF choice = 1 THEN game$ = "Nibbles"
IF choice = 2 THEN game$ = "Time"
END SUB

'PlayNibbles:
'  Main routine that controls game play
SUB PlayNibbles (NumPlayers, speed, diff$, E$, falsenum$, sclr1, sclr2)

'Initialize Snakes
DIM sammyBody(MAXSNAKELENGTH - 1, 1 TO 2) AS snakeBody
DIM sammy(1 TO 2) AS snaketype
DIM F(1 TO 2)
DIM freeman(1 TO 2)
sammy(1).lives = 7
sammy(1).score = 0
sammy(1).scolor = sclr1 + 8
sammy(2).lives = 7
sammy(2).score = 0
sammy(2).scolor = sclr2 + 8
                 
Level STARTOVER, sammy()
startRow1 = sammy(1).row: startCol1 = sammy(1).col
startRow2 = sammy(2).row: startCol2 = sammy(2).col

nsound = 1
curspeed = speed
PlayerAlive = 1
BackPlayers = NumPlayers 'variable that checks how many players
'that the game starts with

'play Nibbles until finished

SpacePause "     Level" + STR$(CurLevel) + ",  Push Space"
gameOver = False
DO
    IF NumPlayers = 1 THEN
        sammy(2).row = 0
    END IF

    number = 1 'Current number that snakes are trying to run into
    nonum = True 'nonum = TRUE if a number is not on the screen

    Playerdied = False
    PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive
    IF nsound = 1 THEN PLAY "mbT160O1>L20CDEDCDL10ECC"

    25 DO
        'Print number if no number exists
        30 IF nonum = True THEN
            DO
                NumberRow = INT(RND(1) * 47 + 3)
                NumberCol = INT(RND(1) * 78 + 2)
                sisterRow = NumberRow + arena(NumberRow, NumberCol).sister
            LOOP UNTIL NOT PointIsThere(NumberRow, NumberCol, colorTable(4)) AND NOT PointIsThere(sisterRow, NumberCol, colorTable(4))
            NumberRow = arena(NumberRow, NumberCol).realRow
            nonum = False: nonumcount = nonumcount + 1
            COLOR colorTable(1), colorTable(4)
            LOCATE NumberRow, NumberCol
            PRINT RIGHT$(STR$(number), 1);
            count = 0
        END IF

        IF nonumcount < 3 AND falsenum$ = "Y" THEN nonum = True: GOTO 30

        'Delay game
        'FOR a# = 1 TO 700 * curspeed: NEXT a#
        _DELAY 0.008 * curspeed 'RONYEH 2015

        'Get keyboard input & Change direction accordingly
        kbd$ = INKEY$
        SELECT CASE kbd$
            CASE "r", "R": IF sammy(2).direction <> 2 THEN sammy(2).direction = 1
            CASE "v", "V": IF sammy(2).direction <> 1 THEN sammy(2).direction = 2
            CASE "d", "D": IF sammy(2).direction <> 4 THEN sammy(2).direction = 3
            CASE "g", "G": IF sammy(2).direction <> 3 THEN sammy(2).direction = 4
            CASE "e", "E": IF sammy(2).direction <> 8 THEN sammy(2).direction = 5
            CASE "t", "T": IF sammy(2).direction <> 7 THEN sammy(2).direction = 6
            CASE "c", "C": IF sammy(2).direction <> 6 THEN sammy(2).direction = 7
            CASE "b", "B": IF sammy(2).direction <> 5 THEN sammy(2).direction = 8
            CASE CHR$(0) + "H": IF sammy(1).direction <> 2 THEN sammy(1).direction = 1
            CASE CHR$(0) + "P": IF sammy(1).direction <> 1 THEN sammy(1).direction = 2
            CASE CHR$(0) + "K": IF sammy(1).direction <> 4 THEN sammy(1).direction = 3
            CASE CHR$(0) + "M": IF sammy(1).direction <> 3 THEN sammy(1).direction = 4
            CASE CHR$(0) + "G": IF sammy(1).direction <> 8 THEN sammy(1).direction = 5
            CASE CHR$(0) + "I": IF sammy(1).direction <> 7 THEN sammy(1).direction = 6
            CASE CHR$(0) + "O": IF sammy(1).direction <> 6 THEN sammy(1).direction = 7
            CASE CHR$(0) + "Q": IF sammy(1).direction <> 5 THEN sammy(1).direction = 8
            CASE "`": IF F(2) <> 0 THEN F(2) = F(2) - 1: sammy(2).direction = sammy(2).direction
            CASE "1": IF F(2) <> 4 THEN F(2) = F(2) + 1: sammy(2).direction = sammy(2).direction
            CASE "[", "{": IF F(1) <> 0 THEN F(1) = F(1) - 1: sammy(1).direction = sammy(1).direction
            CASE "}", "]": IF F(1) <> 4 THEN F(1) = F(1) + 1: sammy(1).direction = sammy(1).direction
            CASE "p", "P": SpacePause " Game Paused ... Push Space  "
            CASE "m", "M": C = ABS(C - 1)
            CASE "s", "S": nsound = ABS(nsound - 1): ShowBox " Sound on/off button pressed "
            CASE "9": endgame = endgame + 1: IF endgame = 4 THEN endgame = 0: EXIT SUB
            CASE ELSE
        END SELECT

        FOR a = PlayerAlive TO NumPlayers
            'Move Snake
            IF C = 1 THEN sammy(2).direction = sammy(1).direction
            SELECT CASE sammy(a).direction
                CASE 1: sammy(a).row = sammy(a).row - 1 * (F(a) + 1)
                CASE 2: sammy(a).row = sammy(a).row + 1 * (F(a) + 1)
                CASE 3: sammy(a).col = sammy(a).col - 1 * (F(a) + 1)
                CASE 4: sammy(a).col = sammy(a).col + 1 * (F(a) + 1)
                CASE 5: sammy(a).row = sammy(a).row - .6 * (F(a) + 1): sammy(a).col = sammy(a).col - .6 * (F(a) + 1)
                CASE 6: sammy(a).row = sammy(a).row - .6 * (F(a) + 1): sammy(a).col = sammy(a).col + .6 * (F(a) + 1)
                CASE 7: sammy(a).col = sammy(a).col - .6 * (F(a) + 1): sammy(a).row = sammy(a).row + .6 * (F(a) + 1)
                CASE 8: sammy(a).col = sammy(a).col + .6 * (F(a) + 1): sammy(a).row = sammy(a).row + .6 * (F(a) + 1)
                CASE ELSE
            END SELECT

            'If snake hits number, respond accordingly
            IF NumberRow = INT((sammy(a).row + 1) / 2) AND NumberCol = sammy(a).col THEN
                nonumcount = 0
                RANDOMIZE TIMER
                IF nsound = 1 THEN
                    x = INT(RND * 11) + 1
                    SELECT CASE x
                        CASE 1: PLAY "mbo2l16b9n0baan0g"
                        CASE 2: PLAY "mbo2l16b9n0baan0g"
                        CASE 3: PLAY "mbo2deadg9c"
                        CASE 4: PLAY "mbo2edcaa"
                        CASE 5: PLAY "mbo4cbaeag"
                        CASE 6: PLAY "mbo2f9g9aac9"
                        CASE 7: PLAY "mbo2f9af6accd"
                        CASE 8: PLAY "mbo2ba12ga7cd"
                        CASE 9: PLAY "mbo3e7e7cb4da"
                        CASE 10: PLAY "mbo2ab4c9de"
                        CASE ELSE: PLAY "mbo2c8c8d8f8e8c8c8d8c8g8f8"
                    END SELECT
                END IF

                IF sammy(a).length < (MAXSNAKELENGTH - 30) THEN
                    sammy(a).length = sammy(a).length + number * 4
                END IF
                IF falsenum$ = "Y" THEN sammy(a).score = sammy(a).score + 3
                sammy(a).score = sammy(a).score + number + CurLevel

                'check if snake has past every 5000 pt. mark, if snake has
                'then give snake a free man
                IF sammy(a).score >= 300 * (freeman(a) + 1) THEN
                    sammy(a).lives = sammy(a).lives + 1
                    freeman(a) = freeman(a) + 1
                    ShowBox "   FREE SNAKE !!!    "
                    FOR s = 1 TO 3
                        FOR i% = 440 TO 1000 STEP 20
                            IF nsound = 1 THEN SOUND i%, i% / 1000
                        NEXT i%
                    NEXT s
                END IF
                   
                PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive
                number = number + 1
                IF number = 7 THEN
                    EraseSnake sammy(), sammyBody(), 1
                    EraseSnake sammy(), sammyBody(), 2
                    LOCATE NumberRow, NumberCol: PRINT " "
                    Level NEXTLEVEL, sammy()
                    PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive
                    SpacePause "     Level" + STR$(CurLevel) + ",  Push Space"
                    IF NumPlayers = 1 THEN sammy(2).row = 0
                    number = 1
                    IF diff$ = "P" THEN speed = speed - 10: curspeed = speed
                END IF
                nonum = True
                IF curspeed < 1 THEN curspeed = 1
            END IF
        NEXT a
           
        FOR a = PlayerAlive TO NumPlayers
            'If player runs into any point, or the head of the other snake, it dies.
            IF sammy(a).row < 3 OR sammy(a).row > 49 OR sammy(a).col < 1 OR sammy(a).col > 79 OR PointIsThere(sammy(a).row, sammy(a).col, colorTable(4)) OR (sammy(1).row = sammy(2).row AND sammy(1).col = sammy(2).col) THEN
                IF PlayerAlive = 2 AND sammy(1).row = sammy(2).row AND sammy(1).col = sammy(2).col THEN GOTO 17
                IF nsound = 1 THEN PLAY "MBO0L32EFGEFDC"
                COLOR , colorTable(4)
                LOCATE NumberRow, NumberCol
                PRINT " "
                   
                Playerdied = True
                sammy(a).alive = False
                sammy(a).lives = sammy(a).lives - 1

                'Otherwise, move the snake, and erase the tail
               
            ELSE
                17 sammy(a).head = (sammy(a).head + 1) MOD MAXSNAKELENGTH
                sammyBody(sammy(a).head, a).row = sammy(a).row
                sammyBody(sammy(a).head, a).col = sammy(a).col
                tail = (sammy(a).head + MAXSNAKELENGTH - sammy(a).length) MOD MAXSNAKELENGTH
                Set sammyBody(tail, a).row, sammyBody(tail, a).col, colorTable(4)
                sammyBody(tail, a).row = 0
                Set sammy(a).row, sammy(a).col, sammy(a).scolor
            END IF
        NEXT a

    LOOP UNTIL Playerdied

    curspeed = speed ' reset speed to initial value
    F(1) = 0
    F(2) = 0
    nonumcount = 0
       
    FOR a = PlayerAlive TO NumPlayers
        EraseSnake sammy(), sammyBody(), a

        'If dead, then erase snake in really cool way
        IF sammy(a).alive = False THEN
            'Update score
            sammy(a).score = sammy(a).score - (CurLevel * 2)
            PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive
                
            RANDOMIZE TIMER
            yeh = INT(RND * 11) + 1
            IF a = 1 THEN
                SELECT CASE yeh
                    CASE 1: SpacePause "       Try harder...       >>"
                    CASE 2: SpacePause "   I'm very disappointed!  >>"
                    CASE 3: SpacePause "    Aim for the numbers.   >>"
                    CASE 4: SpacePause "  Pretty crummy steering.  >>"
                    CASE 5: SpacePause "      What was that???     >>"
                    CASE 6: SpacePause " GATEWAY 2000 Rules!!!!!!! >>"
                    CASE 7: SpacePause "  Better Luck Next Time!   >>"
                    CASE 8: SpacePause "   You just bit the dust.  >>"
                    CASE 9: SpacePause "O.J. Simpson didn't do it! >>"
                    CASE 10: SpacePause "  Smash! Bam! Crack! Boom! >>"
                    CASE ELSE: SpacePause "  Practice Makes Perfect   >>"
                END SELECT
            ELSE
                SELECT CASE yeh
                    CASE 1: SpacePause "<<       Try harder...       "
                    CASE 2: SpacePause "<<       I'm ashamed!        "
                    CASE 3: SpacePause "<<    Look! It's Elvis!!!    "
                    CASE 4: SpacePause "<<       I give up!!!        "
                    CASE 5: SpacePause "<<What's the meaning of life?"
                    CASE 6: SpacePause "<<  GATEWAY 2000 Rules!!!!!! "
                    CASE 7: SpacePause "<<  Better Luck Next Time!   "
                    CASE 8: SpacePause "<<  You just lost your snake."
                    CASE 9: SpacePause "<<      O.J.'s innocent!     "
                    CASE 10: SpacePause "<< What a waste of a snake!!!"
                    CASE ELSE: SpacePause "If you want it, it will come."
                END SELECT
               
            END IF
        END IF
    NEXT a
        
    Level SAMELEVEL, sammy()
    PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive

    IF E$ = "Y" THEN E = 800
    IF E$ = "N" THEN E = 0
                     
    'Play next round, until either of snake's lives have run out.
LOOP UNTIL sammy(1).lives = E OR sammy(2).lives = E

IF NumPlayers = 1 AND BackPlayers = 1 THEN GOTO 100
IF sammy(1).lives < 1 AND sammy(2).lives < 1 THEN GOTO 100
IF sammy(1).lives = E THEN PlayerAlive = 2: sammy(1).lives = -1
IF sammy(2).lives = E THEN PlayerAlive = 1: NumPlayers = 1: sammy(2).lives = -1
IF sammy(1).lives = E AND sammy(2).lives = E THEN PlayerAlive = 0

IF PlayerAlive <> 0 THEN GOTO 25

'send scores to check for High Score Table
100 NumPlayers = BackPlayers 'Resets the initial value of NumPlayers
CALL HighScore(sammy(1).score, sammy(2).score, NumPlayers, E$)

END SUB

' Play Time Trial
' Only One Player Allowed
' 1 minute time limit
SUB PlayTimeTrial (NumPlayers, speed, sclr1, StartTime!, TimePast!)

NumPlayers = 1
PlayerAlive = 1
E$ = "Y"
t$ = ""
DIM NumbCol(1 TO 10)
DIM NumbRow(1 TO 10)

'Initialize Snakes
DIM sammyBody(MAXSNAKELENGTH - 1, 1 TO 2) AS snakeBody
DIM sammy(1 TO 2) AS snaketype
DIM F(1)
sammy(1).lives = 7
sammy(1).score = 0
sammy(1).scolor = sclr1 + 8
   
nsound = 1
curspeed = speed

Level STARTOVER, sammy()
startRow1 = sammy(1).row: startCol1 = sammy(1).col

'play Nibbles until finished

SpacePause "     Level" + STR$(CurLevel) + ",  Push Space"
TIMER ON
gameOver = False
    
DO
    sammy(2).row = 0
    number = 1 'Current number that snakes are trying to run into
    nonum = True 'nonum = TRUE if a number is not on the screen
    MissingNum = 1

    Playerdied = False
    FOR y = 1 TO 10
        NumbCol(y) = 0
    NEXT y
    PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive
    IF nsound = 1 THEN PLAY "mbT160O1>L20CDEDCDL10ECC"
       
    DO
        'Print number if no number exists
        11 IF nonum = True THEN
            DO
                NumberRow = INT(RND(1) * 47 + 3)
                NumberCol = INT(RND(1) * 78 + 2)
                sisterRow = NumberRow + arena(NumberRow, NumberCol).sister
            LOOP UNTIL NOT PointIsThere(NumberRow, NumberCol, colorTable(4)) AND NOT PointIsThere(sisterRow, NumberCol, colorTable(4))
            NumberRow = arena(NumberRow, NumberCol).realRow
            nonum = False
            COLOR colorTable(1), colorTable(4)
            NumbRow(MissingNum) = NumberRow
            NumbCol(MissingNum) = NumberCol
            LOCATE NumberRow, NumberCol: PRINT ""
            count = 0
            FOR y = 1 TO 10
                IF NumbCol(y) < 1 THEN MissingNum = y: nonum = True: GOTO 11
            NEXT y
        END IF

        'Delay game
        'FOR a# = 1 TO 700 * curspeed: NEXT a#
        _DELAY 0.008 * curspeed 'RONYEH 2015

        'Get keyboard input & Change direction accordingly
        kbd$ = INKEY$
        SELECT CASE kbd$
            CASE CHR$(0) + "H": IF sammy(1).direction <> 2 THEN sammy(1).direction = 1
            CASE CHR$(0) + "P": IF sammy(1).direction <> 1 THEN sammy(1).direction = 2
            CASE CHR$(0) + "K": IF sammy(1).direction <> 4 THEN sammy(1).direction = 3
            CASE CHR$(0) + "M": IF sammy(1).direction <> 3 THEN sammy(1).direction = 4
            CASE CHR$(0) + "G": IF sammy(1).direction <> 8 THEN sammy(1).direction = 5
            CASE CHR$(0) + "I": IF sammy(1).direction <> 7 THEN sammy(1).direction = 6
            CASE CHR$(0) + "O": IF sammy(1).direction <> 6 THEN sammy(1).direction = 7
            CASE CHR$(0) + "Q": IF sammy(1).direction <> 5 THEN sammy(1).direction = 8
            CASE "[", "{": IF F(1) <> 0 THEN F(1) = F(1) - 1: sammy(1).direction = sammy(1).direction
            CASE "}", "]": IF F(1) <> 4 THEN F(1) = F(1) + 1: sammy(1).direction = sammy(1).direction
            CASE "p", "P": TIMER STOP: SpacePause " Game Paused ... Push Space  ": TIMER ON
            CASE "s", "S": nsound = ABS(nsound - 1): ShowBox " Sound on/off button pressed "
            CASE "9": endgame = endgame + 1: IF endgame = 4 THEN endgame = 0: EXIT SUB
            CASE ELSE
        END SELECT

        'Move Snake
        SELECT CASE sammy(1).direction
            CASE 1: sammy(1).row = sammy(1).row - 1 * (F(1) + 1)
            CASE 2: sammy(1).row = sammy(1).row + 1 * (F(1) + 1)
            CASE 3: sammy(1).col = sammy(1).col - 1 * (F(1) + 1)
            CASE 4: sammy(1).col = sammy(1).col + 1 * (F(1) + 1)
            CASE 5: sammy(1).row = sammy(1).row - .6 * (F(1) + 1): sammy(a).col = sammy(1).col - .6 * (F(1) + 1)
            CASE 6: sammy(1).row = sammy(1).row - .6 * (F(1) + 1): sammy(a).col = sammy(1).col + .6 * (F(1) + 1)
            CASE 7: sammy(1).col = sammy(1).col - .6 * (F(1) + 1): sammy(a).row = sammy(1).row + .6 * (F(1) + 1)
            CASE 8: sammy(1).col = sammy(1).col + .6 * (F(1) + 1): sammy(a).row = sammy(1).row + .6 * (F(1) + 1)
            CASE ELSE
        END SELECT

        'If snake hits number, respond accordingly
        FOR y = 1 TO 10
            IF NumbRow(y) = INT((sammy(1).row + 1) / 2) AND NumbCol(y) = sammy(1).col THEN
                MissingNum = y
                GOTO 65
            END IF
        NEXT y
        GOTO 66
               
        65 RANDOMIZE TIMER
        IF nsound = 1 THEN
            x = INT(RND * 9) + 1
            SELECT CASE x
                CASE 1: PLAY "mbo3e7e7"
                CASE 2: PLAY "mbo2ab4c"
                CASE 3: PLAY "mbo2de9c"
                CASE 4: PLAY "mbo2ea"
                CASE 5: PLAY "mbo4cag"
                CASE 6: PLAY "mbo2f9g9"
                CASE 7: PLAY "mbo2f9a6ad"
                CASE 8: PLAY "mbo2a12ga7cd"
                CASE ELSE: PLAY "mbo2c8c8d8f8"
            END SELECT
        END IF

        IF sammy(1).length < (MAXSNAKELENGTH - 30) THEN
            sammy(1).length = sammy(1).length + number * 2
        END IF
        sammy(1).score = sammy(1).score + number * 2
        PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive
        nonum = True
        IF curspeed < 1 THEN curspeed = 1
              
        66 a = 1
        'If player runs into any point, or the head of the other snake, it dies.
        IF sammy(a).row < 3 OR sammy(a).row > 49 OR sammy(a).col < 1 OR sammy(a).col > 79 OR PointIsThere(sammy(a).row, sammy(a).col, colorTable(4)) OR (sammy(1).row = sammy(2).row AND sammy(1).col = sammy(2).col) THEN
            IF PlayerAlive = 2 AND sammy(1).row = sammy(2).row AND sammy(1).col = sammy(2).col THEN GOTO 15
            IF nsound = 1 THEN PLAY "MBO0L32EFGEFDC"
            COLOR , colorTable(4)
            LOCATE NumberRow, NumberCol
            PRINT " "
                  
            Playerdied = True
            sammy(a).alive = False
            sammy(a).lives = sammy(a).lives - 1

            'Otherwise, move the snake, and erase the tail
              
        ELSE
            15 sammy(a).head = (sammy(a).head + 1) MOD MAXSNAKELENGTH
            sammyBody(sammy(a).head, a).row = sammy(a).row
            sammyBody(sammy(a).head, a).col = sammy(a).col
            tail = (sammy(a).head + MAXSNAKELENGTH - sammy(a).length) MOD MAXSNAKELENGTH
            Set sammyBody(tail, a).row, sammyBody(tail, a).col, colorTable(4)
            sammyBody(tail, a).row = 0
            Set sammy(a).row, sammy(a).col, sammy(a).scolor
        END IF
        CALL TimerClock(t$, StartTime!, TimePast!)
    LOOP UNTIL Playerdied OR t$ = "time over"

    curspeed = speed ' reset speed to initial value
    F(1) = 0
       
    FOR a = 1 TO NumPlayers
        EraseSnake sammy(), sammyBody(), a

        'If dead, then erase snake in really cool way
        IF sammy(a).alive = False THEN
            'Update score
            sammy(a).score = sammy(a).score - 10
            PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive
               
              
            IF a = 1 THEN
                SpacePause "1stPlayer Dies! Push Space ->"
            END IF
        END IF
    NEXT a
       
    Level SAMELEVEL, sammy()
    PrintScore NumPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives, E$, PlayerAlive

    E = 800

    'Play next round, until time runs out.
    CALL TimerClock(t$, StartTime!, TimePast!)
LOOP WHILE t$ <> "time over"
   
TIMER OFF
SpacePause "Time Ran Out -- Game Over"
'send scores to check for High Score Table

CALL HighTimeX(sammy(1).score)

END SUB

'PointIsThere:
'  Checks the global  arena array to see if the boolean flag is set
FUNCTION PointIsThere (row, col, acolor)
IF row <> 0 THEN
    IF row < 3 OR row > 50 OR col > 79 OR col < 1 THEN Playerdied = True: EXIT FUNCTION
    IF arena(row, col).acolor <> acolor THEN
        PointIsThere = True
    ELSE
        PointIsThere = False
    END IF
END IF
END FUNCTION

'PrintScore:
'  Prints players scores and number of lives remaining
SUB PrintScore (NumPlayers, score1, score2, lives1, lives2, E$, PlayerAlive)
COLOR 15, colorTable(4)

IF NumPlayers = 2 THEN
    LOCATE 1, 1
    IF E$ = "Y" THEN PRINT USING "#,###,#0 Lives: Infinite <-2nd Player"; score2
    IF E$ <> "Y" THEN PRINT USING "#,###,#0 Lives: # <-2nd Player"; score2; lives2
END IF

IF PlayerAlive <> 2 THEN
    IF E$ <> "Y" THEN LOCATE 1, 49: PRINT USING "1st Player-> Lives: # #,###,#0"; lives1; score1
    IF E$ = "Y" THEN LOCATE 1, 40: PRINT USING "1st Player-> Lives: Infinite #,###,#0"; score1
END IF

END SUB

'Set:
'  Sets row and column on playing field to given color to facilitate moving
'  of snakes around the field.
SUB Set (row, col, acolor)
IF row <> 0 THEN
    arena(row, col).acolor = acolor 'assign color to arena
    realRow = arena(row, col).realRow 'Get real row of pixel
    topFlag = arena(row, col).sister + 1 / 2 'Deduce whether pixel
    'is on topﬂ, or bottom‹
    sisterRow = row + arena(row, col).sister 'Get arena row of sister
    sisterColor = arena(sisterRow, col).acolor 'Determine sister's color

    LOCATE realRow, col

    IF acolor = sisterColor THEN 'If both points are same
        COLOR acolor, acolor 'Print chr$(219) "€"
        PRINT CHR$(219);
    ELSE
        IF topFlag THEN 'Since you cannot have
            IF acolor > 7 THEN 'bright backgrounds
                COLOR acolor, sisterColor 'determine best combo
                PRINT CHR$(223); 'to use.
            ELSE
                COLOR sisterColor, acolor
                PRINT CHR$(220);
            END IF
        ELSE
            IF acolor > 7 THEN
                COLOR acolor, sisterColor
                PRINT CHR$(220);
            ELSE
                COLOR sisterColor, acolor
                PRINT CHR$(223);
            END IF
        END IF
    END IF
END IF
END SUB

'Showbox
'  Shows a new function and displays it onto the screen for about a second
SUB ShowBox (text$)

COLOR colorTable(5), colorTable(2)
Center 11, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
Center 12, "€ " + LEFT$(text$ + SPACE$(29), 29) + " €"
Center 13, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
FOR s = 1 TO 20000: NEXT s
FOR s = 1 TO 20000: NEXT s
COLOR 15, colorTable(4)
  

FOR i = 21 TO 26 ' Restore the screen background
    FOR j = 24 TO 56
        Set i, j, arena(i, j).acolor
    NEXT j
NEXT i

END SUB

'SpacePause:
'  Pauses game play and waits for space bar to be pressed before continuing
SUB SpacePause (text$)

COLOR colorTable(5), colorTable(2)
Center 11, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
Center 12, "€ " + LEFT$(text$ + SPACE$(29), 29) + " €"
Center 13, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
WHILE INKEY$ <> "": WEND
WHILE INKEY$ <> " ": WEND
COLOR 15, colorTable(4)
   

FOR i = 21 TO 26 ' Restore the screen background
    FOR j = 24 TO 56
        Set i, j, arena(i, j).acolor
    NEXT j
NEXT i

END SUB

'SparklePause:
'  Creates flashing border for intro screen
SUB SparklePause

COLOR 11, 0
a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "
WHILE INKEY$ <> "": WEND 'Clear keyboard buffer

DO
    kbd$ = UCASE$(INKEY$)
    FOR a = 1 TO 5
        _LIMIT 20 'RONYEH 2015

        LOCATE 1, 1 'print horizontal sparkles
        PRINT MID$(a$, a, 80);
        LOCATE 22, 1
        PRINT MID$(a$, 6 - a, 80);

        FOR b = 2 TO 21 'Print Vertical sparkles
            C = (a + b) MOD 5
            IF C = 1 THEN
                LOCATE b, 80
                PRINT "*";
                LOCATE 23 - b, 1
                PRINT "*";
            ELSE
                LOCATE b, 80
                PRINT " ";
                LOCATE 23 - b, 1
                PRINT " "

            END IF
        NEXT b
    NEXT a
LOOP UNTIL kbd$ = "I" OR kdb$ = "i" OR kbd$ <> ""
IF kbd$ = "I" OR kdb$ = "i" THEN CALL Intructions

END SUB

'StillWantsToPlay:
'  Determines if users want to play game again.
FUNCTION StillWantsToPlay

COLOR colorTable(5), colorTable(6)
Center 9, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
Center 10, "€     Thank You for playing     €"
Center 11, "€      Nibbles Version 5.3      €"
Center 12, "€                               €"
Center 13, "€       G A M E   O V E R       €"
Center 14, "€                               €"
Center 15, "€      Play Again?   (Y/N)      €"
Center 16, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
   
   
' play "MBO0G16>CCED"
WHILE INKEY$ <> "": WEND
DO
    kbd$ = UCASE$(INKEY$)
LOOP UNTIL kbd$ = "Y" OR kbd$ = "N"

COLOR 15, colorTable(4)
Center 10, "                                 "
Center 11, "                                 "
Center 12, "                                 "
Center 13, "                                 "
Center 14, "                                 "

IF kbd$ = "Y" THEN
    StillWantsToPlay = True
ELSE
    StillWantsToPlay = False
    COLOR 7, 0
    CLS
END IF

END FUNCTION

SUB TimerClock (t$, StartTime!, TimePast!)
WHILE TimePast! < 61
    TimePast! = TIMER - StartTime!
    EXIT SUB
WEND
t$ = "time over"
TIMER OFF
END SUB

