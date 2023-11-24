include irvine32.inc 
Include macros.inc
.data
    column DWORD 0
    grid BYTE -1, -1, -1, -1, -1, -1, -1
    rowSize = ($ - grid)
        BYTE -1, -1, -1, -1, -1, -1, -1
        BYTE -1, -1, -1, -1, -1, -1, -1
        BYTE -1, -1, -1, -1, -1, -1, -1
        BYTE -1, -1, -1, -1, -1, -1, -1
        BYTE -1, -1, -1, -1, -1, -1, -1
    player DWORD 1
.code
main proc
    mov DH, 6       ;printing numbers on the top of gird
    mov DL, 20
    call Gotoxy
    mov ebx, 0
    mov ecx, 7
    mainL1:
        mov eax, ebx
        call writeDec
        mov al, " "
        call writeChar
        inc ebx
    Loop mainL1
    call printGrid


whileLoop:
    call printArrow
    call readChar
    cmp al, '1'
    jne checkEnterkey
    je breakLoop
checkEnterKey:      ;temporary label
    cmp ax, 1C0Dh   ;enter key ascii value
    jne checkRight
    call markPosition
    cmp eax, -1
    je whileLoop
    
    ;call winCheck
    ;call gridFull
    call changePlayer
    call printGrid
    jmp whileLoop
checkRight:
    cmp ax, 4D00h   ;right-arrow key ascii value
    jne checkLeft
    inc column
    jmp exit1
checkLeft:
    cmp ax, 4B00h   ;left-arrow key ascii value
    jne exit1
    dec column
exit1:
    cmp column, 0
    jge valid1
    mov column, 6
valid1:
    cmp column, 7
    jb valid2
    mov column, 0
valid2:
    jmp whileLoop

breakLoop:
    mov DH, 20
    call gotoXY
    exit
main ENDP 

printArrow PROC     ;column value passed through variable "column"
    mov DH, 7
    mov DL, 20
    call Gotoxy
    mov al, " "
    mov ecx, 7
    mov ebx, 0
    printArrowL1:
        cmp ebx, column 
        jne printArrowfalse
        mov al, 31
        call writeChar 
        mov al, " "
        jmp printArrowendif
    printArrowfalse:
        call writeChar
    printArrowendif:
        call writeChar
        inc ebx
    Loop printArrowL1
    ret
printArrow ENDP

printGrid PROC      ;no value passed, only use grid 2d-array offset
    mov DL, 19
    mov DH, 8
    mov esi, OFFSET grid
    mov ecx, 6
    mov ebx, 0  ;for rows
    printGridL1:
        call Gotoxy
        push ecx
        mov edi, 0  ;for cols 
        mov al, " "
        call writeChar
        mov ecx, 7
        printGridL2:
            mov eax, 0
            mov al, [esi + edi]
            cmp al, -1  
            jne checkPlayer1    ;not empty, so check players
            mov al, " "
            jmp printElement
        checkPlayer1:
            cmp al, 1   
            jne checkPlayer2    ;not player 1, so jump to player 2
            mov eax, red + (black *16)
            call SetTextColor
            mov al, 'O'
            jmp printElement
        checkPlayer2:
            mov eax, yellow + (black *16)
            call SetTextColor
            mov al, 'O'
            jmp printElement
        printElement:
            call writeChar
            mov al, " "
            call writeChar
            inc edi
        Loop printGridL2
        call Crlf
        add esi, rowSize
        inc edi
        inc dh
        pop ecx
    Loop printGridL1
    mov eax, white + (black *16)
    call SetTextColor
    ret
printGrid ENDP

markPosition PROC           ;uses value in column and player variable
    mov ecx, 6              ;return 1 if valid, otherwise -1
    mov esi, OFFSET grid
    add esi, column
    add esi, rowSize*5

    markPositionL1:
        mov  al, BYTE PTR [esi]
        cmp al, -1
        jne checkNextPosition
        mov al, BYTE PTR player
        mov [esi], al
        mov eax, 1
        jmp markPositionExit
        checkNextPosition:
        sub esi, rowSize
    Loop markPositionL1
    mov eax, -1         
    markPositionExit:
        ret
markPosition ENDP   

ChangePlayer PROC   ;switch value of player variable
    cmp player, 1
    jne player2
    mov player, 2
    jmp ChangePlayerExit
player2:
    mov player, 1
ChangePlayerExit:
    ret
ChangePLayer ENDP

end main