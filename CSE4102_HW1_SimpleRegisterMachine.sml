(*
 *[TYPE] MachineState = {a:int, b:int}
 * Captures the computer's current state via registers a and b.
 *)
type MachineState = {a:int, b:int};

(*
 *[FUNCTION] newMachine = fn : int * int -> MachineState
 * Constructs a new MachineState with the specified values as registers a and b respectively.
 * Arguments must be two integers; the first will set the a register, and the second will set the b register.
 *)
fun newMachine(a,b) : MachineState = {a=a, b=b};

(*
 *[FUNCTION] getA = fn : MachineState -> int
 * Returns the a register of the specified machine.
 * Argument must be a MachineState.
 *)
fun getA (m:MachineState) = #a m;

(*
 *[FUNCTION] getB = fn :  MachineState -> int
 * Returns the b register of the specified machine.
 * Argument must be a MachineState.
 *)
fun getB (m:MachineState) = #b m;

(*
 *[FUNCTION] setA = fn : MachineState * int -> MachineState
 * Returns a new MachineState with:
 *      > the specified integer as register a.
 *      > the specified machine's b register as register b.
 * Arguments must be a MachineState and an integer. 
 *)
fun setA (m:MachineState, newA) = newMachine(newA, getB m);
(*getB used instead of #b m; while equivalent, getB is clearer to programmer*)

(*
 *[FUNCTION] setB = fn : MachineState * int -> MachineState
 * Returns a new MachineState with:
 *      > the specified machine's a register as register a.
 *      > the specified integer as register b.
 * Arguments must be a MachineState and an integer. 
 *)
fun setB (m:MachineState, newB) = newMachine(getA m, newB);
(*getA used instead of #a m; while equivalent, getA is clearer to programmer*)

(*
 *[TYPE] Instruction = SetA of int | SetB of int | Add | Sub | Disp;
 * Representation of the machine's instruction set:
 *     > SetA a: sets the value of the a register to the specified integer value.
 *     > SetB b: sets the value of the b register to the specified integer value.
 *     > Add: adds the a and b registers together and leaves the sum in the a register.
 *     > Sub: subtracts the b register from the a register and leaves the difference in the a register.
 *     > Disp: displays the value of the a register.
 * Note that SetA and SetB must be followed by an integer to be considered an Instruction.
 * (Description of each datatype from "HW1- Simple Register Machine.pdf")
 *)
datatype Instruction = SetA of int | SetB of int | Add | Sub | Disp;

(*
 *[FUNCTION] i2s = fn : Instruction -> string
 * Returns a string representation of the given Instruction.
 * Argument must be an Instruction.
 *)
fun i2s (SetA a) = "SetA " ^ Int.toString(a)
  | i2s (SetB b) = "SetB " ^ Int.toString(b)
  | i2s  Add = "Add"
  | i2s  Sub = "Sub"
  | i2s  Disp = "Disp";

(*
 *[FUNCTION] eval = fn : MachineState * Instruction -> MachineState
 * Executes the specified Instruction on the specified MachineState and returns the MachineState.
 * Function of each Instruction found in Instruction datatype.
 * Arguments must be a MachineState and an Instruction.
 *)
fun eval(m:MachineState, SetA a):MachineState = (setA(m,a);m)
  | eval(m:MachineState, SetB b):MachineState = (setB(m,b);m)
  | eval(m:MachineState, Add):MachineState = (setA(m, getA m + getB m);m)
  | eval(m:MachineState, Sub):MachineState = (setA(m, getA m - getB m);m)
  | eval(m:MachineState, Disp):MachineState =
    (print(Int.toString(getA m) ^ "\n");
     m);

(*
 *[FUNCTION] run = fn : MachineState * Instruction list -> MachineState
 * Executes the specfied list of Instructions on the specified MachineState, and prints its respective string. Returns the MachineState at the end of execution.
 * Arguments must be a MachineState an an Instruction list.
 *)
fun run(m:MachineState, []:Instruction list) = m
  | run(m:MachineState, prog:Instruction list)=
    let val instr = hd prog
	val instrs = tl prog
	val _ = print(i2s instr ^ "\n")
	val m1 = eval(m, instr)		
    in  run(m1,instrs)
    end;
