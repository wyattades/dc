# Arbitrary-precision calculator
An implementation of Unix's dc written in OCaml

## Operands:

+: Pops two values off the stack, adds them, and pushes the result. The precision of the result is determined only by the values of the arguments, and is enough to be exact.

-: Pops two values, subtracts the first one popped from the second one popped, and pushes the result.

\*: Pops two values, multiplies them, and pushes the result. The number of fraction digits in the result is the largest of the precision value, the number of fraction digits in the multiplier, or the number of fraction digits in the multiplicand; but in no event exceeding the number of digits required for an exact result.

/: Pops two values, divides the second one popped from the first one popped, and pushes the result. The number of fraction digits is specified by the precision value.

^: Pops two values and exponentiates, using the first value popped as the exponent and the second popped as the base. The fraction part of the exponent is ignored. The precision value specifies the number of fraction digits in the result.

%: Pops two values, computes the remainder of the division that the "\/" command would do, and pushes that. The value computed is the same as that computed by the sequence "Sd dld/ Ld*-".

c: Clears the stack, rendering it empty.

d: Duplicates the value on the top of the stack, pushing another copy of it. Thus, "4d\*p" computes 4 squared and prints it.

f: Prints the entire contents of the stack without altering anything. This is a good command to use if you are lost or want to figure out what the effect of some command has been.

p: Prints the value on the top of the stack, without altering the stack. A newline is printed after the value.

sr: Pop the value off the top of the stack and store it into register r.

lr: Copy the value in register r, and push it onto the stack. This does not alter the contents of r. Each register also contains its own stack. The current register value is the top of the register's stack.
