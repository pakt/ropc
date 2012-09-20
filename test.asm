;extern  printf      

global start_rop

;----------------------------------------
; DATA
;----------------------------------------
SECTION .data       

a:      dd  5       
fmt:    db "a=%d, eax=%d", 10, 0 


;----------------------------------------
; CODE
;----------------------------------------
SECTION .text                   


    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ; nops above are necessary -- without them BAP emits incorrect IR
    xor eax, eax
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx
    xor esi, esi
    xor edi, edi
    xor ebp, ebp
    xor esp, esp
    inc esp
    shl esp, 10
load_const: 
    pop eax
    ret
read_mem:
    mov eax, [ebx+0xdeadbeef]
    mov edx, [ecx+0x0badf00d]
    ret
read_mem_op:
    xor eax, [ebx+0xf00d]
    ret
write_mem:
    mov eax, 0
    mov edx, 0x11223344
    mov [ebx+0xbadc0de], ecx
    ret
write_mem_op:
    xor [eax+0xbeef], ebx
    xor eax, eax
    xor ebx, ebx
    ret
g4:
    xor eax, edx
    mov esi, [esp+8]
    mov ebp, edi
    pop ecx
    ret 
g5:
    add edx, ecx
    ret

tricky:
    add eax, ebx        
    xor ebx, ebx
    sub ecx, 0x123
    setz bl
    add eax, ebx
    ret

copy_reg:
    mov eax, ebx
    ret

load_flags:
    lahf
    ret

stuff:
    pop ebx
    ret

    mov eax, [ebx]
    ret

    mov [eax], ebx
    ret

    add esp, eax
    ret

save_esp:
    mov eax, esp
    ret

    add esp, ebx
    pop ecx
    pop edx
    ret

    add eax, ebx
    ret

    pop eax
    ret

    pop ebx
    ret

    pop ecx
    ret

    pop edx
    ret

    mov [ecx], edx
    ret

    mov edx, [ecx]
    ret

    add ecx, eax
    ret

    add edx, ebx
    ret

    add ecx, edx
    ret

    sub ecx, edx
    ret

    mov ecx, [ecx]
    ret

    mov ecx, [edx]
    ret

    mov edx, [edx]
    ret

    mov eax, [eax]
    ret

    mov ebx, [eax]
    ret

    mov [eax], eax
    ret
    
    mov [ebx], eax
    ret

    mov [eax], ecx
    ret

    mov edx, [ebx]
    ret

    sub eax, ebx
    ret

    add eax, ebx
    ret
    
    or eax, ebx
    ret
    
    and eax, ebx
    ret

    and ecx, edx
    ret

    and ebx, ecx
    ret

    and eax, edx
    ret

    and ebx, edx
    ret

    mul ebx 
    ret

    mul ecx
    ret

    mul edx
    ret
    
    add ebx, ecx
    ret

    add eax, ecx
    ret

    pop ecx
    ret
    
    xor edx, edx
    div ebx
    ret

    xor eax, ebx
    ret

    xor eax, ecx
    ret

    xor eax, edx
    ret

start_rop:
    mov esp, [esp+4]
    ret
