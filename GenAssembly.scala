package scala.scalanative.nbc

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable

object GenAssembly {
  // We use AT&T syntax

  val buffer: mutable.UnrolledBuffer[String] = mutable.UnrolledBuffer.empty[String]

  val builtins: Seq[(String, Seq[Int])] = // Builtin names, argument sizes
    Seq(
      //("scalanative_alloc", Seq(64)),        // alloc(long)
      //("scalanative_classalloc", Seq(64)),   // classalloc(ptr)
      //("scalanative_field", Seq(64, 64, 32)) // field(ptr, ptr, int)
      // ...
    )

  def p(op: Short, byte: Int): Byte = ((op & (0xf << 4*(3-byte))) >> 4*(3-byte)).toByte
  def sizeToBytes(size:Int): Int = size match {
    case 0 => 1
    case 1 => 2
    case 2 => 4
    case 3 => 8
  }

  def imm(value: Int): String = "$" + value
  def reg(num: Int, size: Int): String = {
    val suffix = size match {
      case 0 => "b"
      case 1 => "w"
      case 2 => "d"
      case 3 => ""
    }
    "%r" + (num + 8) + suffix
  }
  def arg(num: Byte): BinArg = num match {
    case r if r < 8 => Reg(r)
    case 0x8        => SReg
    case 0xc        => Imm
    case 0xf        => Mem
  }
  def dest(num: Byte): BinArg = num match {
    case r if r < 8 => Reg(r)
    case 0x8        => SReg
    case 0xf        => Mem
  }

  // %rax, %rdx, %rsp are used in special cases
  // %rax holds the code section address
  // %rbx holds the spilled registers table
  // %rdx holds the opcode offset table
  val rCode = "%rax"
  val rReg  = "%rbx" // TODO push spilled length on register stack
  val rOOT  = "%rdx"
  val rL  = "%rcx"
  val rPC = "%rbp"
  val rSP = "%rsp" // Stack pointer (not managed by us)
  def rTmp1(size: Int) = size match { // Temporary register
    case 0 => "%sil"
    case 1 => "%si"
    case 2 => "%esi"
    case 3 => "%rsi"
  }
  def rTmp2(size: Int) = size match { // Temporary register
    case 0 => "%dil"
    case 1 => "%di"
    case 2 => "%edi"
    case 3 => "%rdi"
  }
  def getOperand(arg: BinArg, size: Int, tmp: Int => String, immOffset: Int): (String, Int) = {
    arg match {
      case Reg(r) => (reg(r, size), 0 + immOffset)
      case SReg   =>
        inst("xorq", Seq(tmp(3), tmp(3)))
        inst("movw", Seq(dataAt(rPC, immOffset), tmp(1)))
        (sReg(tmp(3)), 2 + immOffset)
      case Mem    =>
        inst("movq", Seq(dataAt(rPC, immOffset), tmp(3)))
        (dataAt(tmp(3), 0), 8 + immOffset)
      case Imm   => (dataAt(rPC, immOffset), sizeToBytes(size) + immOffset)
    }
  }
  def getOperandF(arg: BinArg, size: Int, tmp: Int => String, immOffset: Int, reg: String): (String, Int) = {
    val (operand, off) = getOperand(arg, size + 2, tmp, immOffset)
    size match {
      case 0 => 
        inst("movl", Seq(operand, tmp(2))) // Upper 32 bits are zeroed out on move
        inst("movq", Seq(tmp(3), reg))
      case 1 =>
        inst("movq", Seq(operand, reg))
    }
    (operand, off)
  }
  def allRegs: Seq[String] = Seq(rCode, rReg, rL, rOOT, rPC) ++ (0 until 8).map(reg(_,3))
  def dataAt(reg: String, off: Int): String = (off match {
    case 0 => ""
    case _ => off.toString
  }) + "(" + rCode + "," + reg + ")"
  def sReg(numReg: String): String = "-64(" + rReg + "," + numReg + ", 8)" // -64 because spilled regs begin at 8

  def op(op: String, size: Int): String = op match {
    case _ => op + (size match {
      case -1 => ""
      case 0 => "b"
      case 1 => "w"
      case 2 => "l"
      case 3 => "q"
    })
  }
  def opF(op: String, size: Int): String = op + (size match {
    case 0 => "ss"
    case 1 => "sd"
  })

  def inst(opcode: String, args: Seq[String]): Unit = {
    val instr = opcode + " " + args.mkString(", ")
    buffer += instr
  }

  sealed abstract class BinArg
  final case class Reg(id: Int) extends BinArg
  final case object SReg extends BinArg
  final case object Mem extends BinArg
  final case object Imm extends BinArg

  def nullary(opcode: String, size: Int): Int = {
    inst(op(opcode, size), Seq())
    2
  }
  def unary(opcode: String, size: Int, arg1: Byte): Int = {
    val operation = op(opcode, size)
    val (operand, off) = getOperand(dest(arg1), size, rTmp1, 2) // In principle no immediate push
    inst(operation, Seq(operand))
    off
  }
  def binary(opcode: String, size: Int, arg1: Byte, arg2: Byte): Int =
    customBinary(op(opcode, size), false, size, arg1, false, size, arg2)
  def binaryF(opcode: String, size: Int, arg1: Byte, arg2: Byte): Int =
    customBinary(opF(opcode, size), true, size, arg1, true, size, arg2)
  def customBinary(operation: String, float1: Boolean, size1: Int, arg1: Byte, float2: Boolean, size2: Int, arg2: Byte): Int = {
    val (operand2, off2) =
      if (float2) getOperandF(dest(arg2), size2, rTmp2, 2, "%mm1")
      else getOperand(dest(arg2), size2, rTmp2, 2)
    val (operand1, off1) =
      if (float1) getOperandF(arg(arg1), size1, rTmp1, off2, "%mm0")
      else getOperand(arg(arg1), size1, rTmp1, off2)
    if (!float1 && !float2 && (!arg(arg1).isInstanceOf[Reg]) && (!dest(arg2).isInstanceOf[Reg])) {
      inst(op("mov", size2), Seq(operand2, rTmp2(size2)))
      inst(operation, Seq(operand1, rTmp2(size2)))
      inst(op("mov", size2), Seq(rTmp2(size2), operand2))
    } else {
      inst(operation, Seq(if (float1) "%xmm0" else operand1, if (float2) "%xmm1" else operand2))
      if (float2) {
        size2 match {
          case 0 =>
            inst("movq", Seq("%mm1", rTmp2(3)))
            inst("movl", Seq(rTmp2(2), operand2))
          case 1 =>
            inst("movq", Seq("%mm1", operand2))
        }
      }
    }
    off1
  }

  def genAssembly(opc: Short): String = {
    buffer.clear()

    val length = p(opc, 0) match {
      case 0x0 => // Move
        binary("mov", p(opc, 1), p(opc, 3), p(opc, 2))

      case 0x1 => // Stack
        if (p(opc, 2) != 0) throw new Exception()
        val operand = p(opc, 1) & 0xc match {
          case 0x0 => "push"
          case 0x4 => "pop"
        }
        val size = p(opc, 1) & 0x3
        (size, operand) match { // Most cases can be handled by moving too much stuff
          case (0, "pop") | (2, "pop") if dest(p(opc, 3)) == Mem => // Need special care when popping to memory
            val (operand, off) = getOperand(Mem, size, rTmp1, 2)
            inst(op("pop", size + 1), Seq(rTmp2(size + 1)))
            inst(op("mov", size), Seq(rTmp2(size), operand))
            off
          case _ => unary(operand, size | 0x1, p(opc, 3))
        }

      case 0x2 => // Memory
        if ((p(opc, 1) & 0x8) != 0) throw new Exception()
        val size = p(opc, 1) & 0x3
        binary("mov", size, p(opc, 3), p(opc, 2))

      case 0x3 => // Bitwise arithmetic
        val operand = p(opc, 1) & 0xc match {
          case 0x0 => "and"
          case 0x4 => "or"
          case 0x8 => "xor"
        }
        val size = p(opc, 1) & 0x3
        binary(operand, size, p(opc, 3), p(opc, 2))

      case 0x4 => // Integer arithmetic
        val operand = p(opc, 1) & 0xc match {
          case 0x0 => "add"
          case 0x2 => "sub"
          case 0x4 => "imul"
        }
        val size = p(opc, 1) & 0x3
        
        (operand, size, arg(p(opc, 3)), dest(p(opc, 2))) match {
          case ("imul", 0, _, _) => // Can't multiply single bytes
            val mulSize = size match {
              case 0 => 1
              case _ => size
            }
            val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
            val (operand1, off1) = getOperand(arg(p(opc, 3)), mulSize, rTmp1, off2)
            inst(op("mov", size), Seq(operand2, rTmp2(size)))
            inst(op("imul", mulSize), Seq(operand1, rTmp2(mulSize)))
            inst(op("mov", size), Seq(rTmp2(size), operand2))
            off1
          case ("imul", _, Reg(_), (SReg | Mem)) => // Can't multiply directly into memory
            val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
            val (operand1, off1) = getOperand(arg(p(opc, 3)), size, rTmp1, off2)
            inst(op("mov", size), Seq(operand2, rTmp2(size)))
            inst(op("imul", size), Seq(operand1, rTmp2(size)))
            inst(op("mov", size), Seq(rTmp2(size), operand2))
            off1
          case _ => binary(operand, size, p(opc, 3), p(opc, 2))
        }

      case 0x5 => // Floating-point arithmetic
        val operand = p(opc, 1) & 0xc match {
          case 0x0 => "add"
          case 0x4 => "sub"
          case 0x8 => "mul"
        }
        val size = p(opc, 1) & 0x3
        binaryF(operand, size, p(opc, 3), p(opc, 2))

      case 0x6 => // Division
        val size = p(opc, 1) & 0x3
        def rA(size: Int) = size match {
          case 0 => "%al"
          case 1 => "%ax"
          case 2 => "%eax"
          case 3 => "%rax"
        }
        def rD(size: Int) = size match {
          case 0 => "%dl"
          case 1 => "%dx"
          case 2 => "%edx"
          case 3 => "%rdx"
        }
        p(opc, 1) & 0x8 match {
          case 0x0 => // Integer division
            // Step 0: Save rax and rdx
            inst("pushq", Seq(rCode))
            inst("pushq", Seq(rOOT))

            // Step 1: Put divided in rdx:rax
            val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
            inst(op("mov", size), Seq(operand2, rA(size)))

            // Step 2: Sign or zero extend divided
            p(opc, 1) & 0x4 match {
              case 0x0 => // Signed division
                size match {
                  case 0x0 => inst("cbtw", Seq())
                  case 0x1 => inst("cwtd", Seq())
                  case 0x2 => inst("cltq", Seq())
                  case 0x3 => inst("cqto", Seq())
                }
              case 0x4 => // Unsigned division
                inst(op("mov", size), Seq(imm(0), rD(size)))
            }

            // Step 3: Divide
            val (operand1, off1) = getOperand(arg(p(opc, 3)), size, rTmp1, off2)
            inst(op(if ((p(opc, 1) & 0x4) == 0x0) "idiv" else "div", size), Seq(operand1))

            // Step 4: Get result
            inst(op("mov", size), Seq(rA(size), operand2))

            // Step 5: Restore rax and rdx
            inst("popq", Seq(rOOT))
            inst("popq", Seq(rCode))

            off1
          case 0x8 => // Floating-point division
            if (size > 1) throw new Exception()
            binaryF("div", size, p(opc, 3), p(opc, 2))
        }

      case 0x7 => // Modulo
        val size = p(opc, 1) & 0x3
        def rA(size: Int) = size match {
          case 0 => "%al"
          case 1 => "%ax"
          case 2 => "%eax"
          case 3 => "%rax"
        }
        def rD(size: Int) = size match {
          case 0 => "%dl"
          case 1 => "%dx"
          case 2 => "%edx"
          case 3 => "%rdx"
        }
        p(opc, 1) & 0x8 match {
          case 0x0 => // Integer modulo
            // Step 0: Save rax and rdx
            inst("pushq", Seq(rCode))
            inst("pushq", Seq(rOOT))

            // Step 1: Put divided in rdx:rax
            val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
            inst(op("mov", size), Seq(operand2, rA(size)))

            // Step 2: Sign or zero extend divided
            p(opc, 1) & 0x4 match {
              case 0x0 => // Signed modulo
                size match {
                  case 0x0 => inst("cbtw", Seq())
                  case 0x1 => inst("cwtd", Seq())
                  case 0x2 => inst("cltq", Seq())
                  case 0x3 => inst("cqto", Seq())
                }
              case 0x4 => // Unsigned modulo
                inst(op("mov", size), Seq(imm(0), rD(size)))
            }

            // Step 3: Divide
            val (operand1, off1) = getOperand(arg(p(opc, 3)), size, rTmp1, off2)
            inst(op("idiv", size), Seq(operand1))

            // Step 4: Get result
            inst(op("mov", size), Seq(rD(size), operand2))

            // Step 5: Restore rax and rdx
            inst("popq", Seq(rOOT))
            inst("popq", Seq(rCode))

            off1
          case 0x8 => // Floating-point modulo
            /*if (size > 1)*/ throw new Exception()
            // TODO
            //binaryF("fprem1", size, p(opc, 3), p(opc, 2))
        }

      case 0x8 => // Truncation and floating-point extension
        p(opc, 1) & 0x8 match {
          case 0x0 =>
            val destsize = p(opc, 1) match {
              case 0x0 => 8
              case 0x1 => 8
              case 0x2 => 8
              case 0x3 => 16
              case 0x4 => 16
              case 0x5 => 32
            }
            binary("mov", destsize, p(opc, 3), p(opc, 2))
          case 0x8 =>
            val (operand, s1, s2) = p(opc, 1) match {
              case 0x8 => ("cvtsd2ss", 1, 0)
              case 0xf => ("cvtss2sd", 0, 1)
            }
            customBinary(operand, true, s1, p(opc, 3), true, s2, p(opc, 2))
        }

      case 0x9 => // Integer extension
        val (s1, preS2) = p(opc, 1) & 0x7 match {
          case 0x0 => (0,1)
          case 0x1 => (0,2)
          case 0x2 => (0,3)
          case 0x3 => (1,2)
          case 0x4 => (1,3)
          case 0x5 => (2,3) // Auto zeroing out on move
        }
        val (operation, s2) = (p(opc, 1) & 0x8 match {
          case 0x0 if (s1 == 2 && preS2 == 3) => ("movl", 2)
          case 0x0 => ("movz" + op("", s1) + op("", preS2), preS2)
          case 0x8 => ("movs" + op("", s1) + op("", preS2), preS2)
        })

        (arg(p(opc, 3)), dest(p(opc, 2))) match {
          case (Reg(_), (SReg | Mem)) => // Can't extend directly into memory
            val (operand2, off2) = getOperand(dest(p(opc, 2)), s2, rTmp2, 2)
            val (operand1, off1) = getOperand(arg(p(opc, 3)), s1, rTmp1, off2)
            inst(op("mov", s2), Seq(operand2, rTmp2(s2)))
            inst(operation, Seq(operand1, rTmp2(s2)))
            inst(op("mov", s2), Seq(rTmp2(s2), operand2))
            off1
          case _ => customBinary(operation, false, s1, p(opc, 3), false, s2, p(opc, 2))
        }

      case 0xa => // Floating-point to integer TODO unsigned conversion is tricky
        val s1 = p(opc, 1) & 0x4
        val s2 = p(opc, 1) & 0x3 match {
          case s if s < 2 => 2
          case s          => s
        }
        val operation = (s1, p(opc, 1) & 0x8) match {
          case (0x0, 0x0) => op("cvttss2si", s2)
          //case (0x0, 0x8) => "cvttss2ui" // TODO probably not
          case (0x1, 0x0) => op("cvttsd2si", s2)
          //case (0x1, 0x8) => "cvttsd2ui" // TODO probably not
        }
        dest(p(opc, 2)) match {
          case SReg | Mem => // Can't convert directly into memory
            val (operand2, off2) = getOperand(dest(p(opc, 2)), s2, rTmp2, 2)
            val (operand1, off1) = getOperandF(arg(p(opc, 3)), s1, rTmp1, off2, "%xmm0")
            inst(op("mov", s2), Seq(operand2, rTmp2(s2)))
            inst(operation, Seq("%xmm0", rTmp2(s2)))
            inst(op("mov", s2), Seq(rTmp2(s2), operand2))
            off1
          case _ => customBinary(operation, true, s1, p(opc, 3), false, s2, p(opc, 2))
        }

      case 0xb => // Integer to floating-point
        val size1 = p(opc, 1) & 0x6
        val size2 = p(opc, 1) & 0x1
        val operation = (size2, p(opc, 1) & 0x8) match {
          case (0x0, 0x0) => op("cvtsi2ss", if (size1 < 2) 2 else size1)
          //case (0x0, 0x8) => "cvttui2ss" // TODO probably not
          case (0x1, 0x0) => op("cvtsi2sd", if (size1 < 2) 2 else size1)
          //case (0x1, 0x8) => "cvttui2sd" // TODO probably not
        }
        size1 match {
          case 0 | 1 => // Can't convert from byte or word
            val (operand2, off2) = getOperandF(dest(p(opc, 2)), size2, rTmp2, 2, "%mm1")
            val (operand1, off1) = getOperand(arg(p(opc, 3)), size1, rTmp1, off2)
            inst(op("movz", size1) + "l", Seq(operand1, rTmp1(2)))
            inst(operation, Seq(rTmp1(2), "%xmm1"))
            size2 match {
              case 0 =>
                inst("movq", Seq("%mm1", rTmp2(3)))
                inst("movl", Seq(rTmp2(2), operand2))
              case 1 =>
                inst("movq", Seq("%mm1", operand2))
            }
            off1
          case _ => customBinary(operation, false, size1, p(opc, 3), true, size2, p(opc, 2))
        }

      case 0xc => // Control flow
        if (p(opc, 2) != 0) throw new Exception()
        val (operand, off) = getOperand(arg(p(opc, 3)), 8, rTmp1, 2)
        if (p(opc, 1) == 0x0) { // Call
          inst("movq", Seq(rPC, rL))
          inst("addq", Seq(imm(off), rL))
          inst("movq", Seq(operand, rPC))
          -1
        } else {
          val operation = p(opc, 1) match {
            case 0x1 => "mov"
            case 0x2 => "cmove"
            case 0x3 => "cmovne"
            case 0x4 => "cmovbe"
            case 0x5 => "cmovb"
            case 0x6 => "cmovae"
            case 0x7 => "cmova"
          }
          inst(operation, Seq(operand, rPC))
          -1
        }


      case 0xd => // Return and Halt
        if (p(opc, 2) != 0 || p(opc, 3) != 0) throw new Exception()
        p(opc, 1) match {
          case 0x0 =>
            // Restore register stack
            inst("movq", Seq(dataAt(rReg, -16), rReg))
            // Transfer execution to caller
            inst("movq", Seq(rL, rPC))
          case 0xf => inst("ret", Seq()) // Return from tight loop function TODO restore state
        }
        -1

      case 0xe => // Conditional ops
        val operand = p(opc, 1) match {
          case 0x0 => "sete"
          case 0x1 => "setne"
          case 0x4 => "setle"
          case 0x5 => "setl"
          case 0x6 => "setge"
          case 0x7 => "setg"
          case 0x8 => "setbe"
          case 0x9 => "setb"
          case 0xa => "setae"
          case 0xb => "seta"
        }
        unary(operand, 0, p(opc, 3))

      case 0xf if (p(opc, 1) == 0xf) => // Alloc
        val (operand2, off2) = getOperand(dest(p(opc, 2)), 3, rTmp2, 2)
        val (operand1, off1) = getOperand(arg(p(opc, 3)), 1, rTmp1, off2)
        inst("movswq", Seq(operand1, rTmp1(3)))
        inst("addq", Seq(rTmp1(3), "%rsp"))
        inst("movq", Seq("%rsp", operand2))
        off1

      case 0xf => // Builtin functions
        val (name, args) = builtins(opc & 0xfff)

        // Step 1: save all registers on Register stack
        inst("movq", Seq(dataAt(rReg, -8), rTmp1(3)))
        inst("imulq", Seq(imm(8), rTmp1(3)))
        inst("addq", Seq(rReg, rTmp1(3)))
        inst("movq", Seq(rReg, dataAt(rTmp1(3), 0)))
        inst("movq", Seq(rTmp1(3), rReg))
        allRegs.zipWithIndex.foreach { case (reg, idx) =>
          inst("movq", Seq(reg, dataAt(rReg, (idx+1)*8)))
        }

        // Step 2: Put arguments in registers (max. 6 arguments)
        val argRegs = Seq("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9")
        argRegs.reverse.foreach { case reg =>
          inst("popq", Seq(reg)) // Upper bytes will be ignored on smaller sizes
        }

        // Step 3: Call function
        inst("call", Seq(name))

        // Step 4: Get return value
        inst("pushq", Seq("%rax"))

        // Step 5: Restore state
        allRegs.zipWithIndex.foreach { case (reg, idx) =>
          inst("movq", Seq(dataAt(rReg, idx*8), reg))
        }
        inst("movq", Seq(dataAt(rReg, 0), rReg))
        2
    }
    if (length != -1) {
      inst("addq", Seq(imm(length), rPC))
    }
    inst("movzwq", Seq(dataAt(rPC, 0), rTmp1(3))) // Opcode is in tmp
    inst("jmp", Seq("*(" + rOOT + "," + rTmp1(3) + ",8)"))

    val labelName = "assembly_" + (0 until 4).map(i => Integer.toHexString(p(opc, i))).mkString("")
    ".globl " + labelName + "\n" + labelName + ":\n    " + buffer.mkString(" # " + labelName + "\n    ") + "\n"
  }

  def apply(): Unit = {
    val file = new File("assembly.S")
    val hFile = new File("opcode_offset_table.h")
    val cFile = new File("opcode_offset_table.c")
    val bw = new BufferedWriter(new FileWriter(file))
    val hbw = new BufferedWriter(new FileWriter(hFile))
    val cbw = new BufferedWriter(new FileWriter(cFile))
    cbw.write("#include <stdlib.h>\n#include \"opcode_offset_table.h\"\n\nvoid* opcode_offset_table[65536] = {\n")
    (0 to 0xffff).foreach { x =>
      try {
        var assembly = genAssembly(x.toShort)
        bw.write(assembly)
        hbw.write("extern void* assembly_" + String.format("%04x", x: Integer) + " asm(\"assembly_" + String.format("%04x", x: Integer) + "\");\n")
        cbw.write("    &assembly_" + String.format("%04x", x: Integer) + ", \n")
      } catch {
        case _ =>
          cbw.write("    NULL, \n")
      }
    }
    cbw.write("};")
    bw.close()
    hbw.close()
    cbw.close()
  }
}
