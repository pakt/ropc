This is an example of a ROP payload with conditional branches, recursion
and emulated stack. Payload is pure -- it does not execute code from 
executable memory.

To see how it works, compile and link test.(c/asm) and run a.out with
compiled.bin as parameter. Expected output (tested on ubuntu 11.10):

p@ubuntu:~/ropc$ md5sum a.out
0c66c5eaaf2781469384fc89278b38c6  a.out
p@ubuntu:~/ropc$ ./a.out compiled.bin 
buf=0x0a1c8170
roundup buf=0x0a1d0000
0
1
1
2
3
5
8
13
21
34
55
Segmentation fault

Segmentation fault at the end is not an error, this is how ROP payloads
are supposed to terminate -- with a controlled SIGSEGV.

Printed numbers are elements of the Fibonacci sequence. The ROP program
calculating them is implemented in fib.ropl. The ROPL language is easier
to use than assembly:

fun fib(n, out){
    x = 0
    y = 0

    cmp n, 0
    je copy
    cmp n, 1
    je copy
    
    fib(n-1, @x)
    fib(n-2, @y)

    [out] = x+y
    jmp exit
copy:
    [out] = n
exit:
}

fun main(){
    fmt = "%d\n"
    i = 0
    x = 0
print:
    fib(i, @x)
    !printf(fmt, x)
    i = i+1
    cmp i, 11
    jne print
}

To see the "decompiled" payload, check decompiled.txt. Notice how much space
is required to emulate a stack for local variables, function params, etc. 

gadgets.txt lists all gadgets found in a.out. Gadgets' semantics were verified with
STP, just like in the Q compiler by CMU team.

parser.mly described ROPL's grammar.
