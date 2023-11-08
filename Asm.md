# Assembly Of Command Lisp
## Registers
All the operands should be load to register to perform operation.

- `a0`: Acts as Null Unit. All the write operation to here will be deprecated. Read operation is not allowed
- `a1~a10`: Used for function arguments
- `x0~x10`: General purpose registers
- `t0~t10`: Save temporary values
- `sp`: Stack pointer, always points to the stack top
- `pc`: Program counter, points to the current instruction

## Data Types
All values and address should be 32 bit integer

## Memory
- Load from memory `ld t0 addr`
- Save to memory `sd t0 addr`

## System Call
The system call is performed by asm `syscall`. Before which you should set:
- `a1`: the operation code:
- `a2`: the arguments 1
- `a3`: the arguments 2
  - `write` (Operation Code is `93`): Write to output (`a2` for destination and `a3` the value)
    - `a2` can be `0` for **tellraw** and `1` for **titleraw**
  - Other commands will also be compiled to syscall with a unique operation code

## Instructions
The registers below should be `a0~a5` or `x0~x10`
- `ld reg0 *addr`: Load the value from `addr` to register `reg0`
- `sd reg0 *addr`: Save the value from register `reg0` to `addr`
- `set reg0 value`: Set a register to value
- `cst value`: Push a value into stack top
- `add reg0 reg1`: Add the value of `reg1` to `reg0` and saves to `reg0`
- `mul reg0 reg1`: Mul the value of `reg1` to `reg0` and saves to `reg0`
- `pop reg0`: Pop the stack top out and save to `reg0`
- `swap reg0 reg1`: Swap the value in `reg0` and `reg1`
- `goto reg0`: Set the program counter to the address stored in `reg0`
- `syscall`: perform system call
- `exit`: Clear all the registers and exit the program

## Examples
The simple addition:
```lisp
(tellraw (+ 114 514))
```
will be compiled to:
```asm
# Add begin
cst 114
cst 514
pop x0
pop x1
add x0, x1 # x0 = 628
# Add end
# Write begin
set a1 93
ld a2 x0
syscall
# Write end (The screens shows "628")
```