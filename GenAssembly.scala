package scala.scalanative.nbc

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable

object GenAssembly {
  // We use AT&T syntax

  val buffer: mutable.UnrolledBuffer[String] = mutable.UnrolledBuffer.empty[String]

  // Registers
  val rA = Seq("%al", "%ax", "%eax", "%rax")
  val rB = Seq("%bl", "%bx", "%ebx", "%rbx")
  val rC = Seq("%cl", "%cx", "%ecx", "%rcx")
  val rD = Seq("%dl", "%dx", "%edx", "%rdx")
  def rTmp1 = Seq("%sil", "%si", "%esi", "%rsi")
  def rTmp2 = Seq("%dil", "%di", "%edi", "%rdi")

  val rCode = rA(3) // Code pointer
  val rReg  = rB(3) // Register stack pointer
  val rL    = rC(3) // Link register
  val rOOT  = rD(3) // Opcode offset table pointer
  val rPC   = "%rbp" // Program counter
  val rSP   = "%rsp" // Stack pointer (not managed by us)

  def reg(num: Int, size: Int): String = {
    val suffix = Seq("b", "w", "d", "")(size)
    "%r" + (num + 8) + suffix
  }
  def imm(value: Int): String = "$" + value

  // Opcode parsing
  def p(op: Short, byte: Int): Byte = ((op & (0xf << 4*(3-byte))) >> 4*(3-byte)).toByte
  sealed abstract class BinArg {
    def isReg: Boolean
  }
  final case class Reg(id: Int) extends BinArg {
    override def isReg = true
  }
  final case object SReg extends BinArg {
    override def isReg = false
  }
  final case object Mem extends BinArg {
    override def isReg = false
  }
  final case object Imm extends BinArg {
    override def isReg = false
  }
  final case object Nop extends BinArg {
    override def isReg = true
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
    case 0x9        => Nop
  }

  // Operands
  def getOperand(arg: BinArg, size: Int, tmp: Int => String, immOffset: Int): (String, Int) = {
    val sizeToBytes = Seq(1, 2, 4, 8)
    arg match {
      case Reg(r) => (reg(r, size), 0 + immOffset)
      case SReg   =>
        inst("movzwq", Seq(dataAt(rPC, immOffset), tmp(3)))
        (sReg(tmp(3)), 2 + immOffset)
      case Mem    => (dataAt(rPC, immOffset), 8 + immOffset)
      case Imm   => (dataAt(rPC, immOffset), sizeToBytes(size) + immOffset)
      case Nop   => (tmp(size), 0 + immOffset)
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
  def dataAt(reg: String, off: Int): String = (off match {
    case 0 => ""
    case _ => off.toString
  }) + "(" + rCode + "," + reg + ")"
  def offset(reg: String, off: Int): String = (off match {
    case 0 => ""
    case _ => off.toString
  }) + "(" + reg + ")"
  def sReg(numReg: String): String = "-64(" + rReg + "," + numReg + ", 8)" // -64 because spilled regs begin at 8

  // Instructions
  def op(op: String, size: Int): String = op + Seq("b", "w", "l", "q")(size)
  def opF(op: String, size: Int): String = op + Seq("ss", "sd")(size)
  def inst(opcode: String, args: Seq[String]): Unit = {
    val instr = opcode + " " + args.mkString(", ")
    buffer += instr
  }

  // Generic operations
  def nullary(opcode: String, size: Int): Int = {
    inst(op(opcode, size), Seq())
    2
  }
  def unary(opcode: String, size: Int, arg1: Byte, dest1: Boolean, popFlags: Boolean = false): Int = {
    val operation = op(opcode, size)
    val (operand, off) = getOperand(if (dest1) dest(arg1) else arg(arg1), size, rTmp1, 2)
    if (popFlags) inst("popf", Seq())
    inst(operation, Seq(operand))
    off
  }
  def binary(opcode: String, size: Int, arg1: Byte, arg2: Byte, notToMemory: Boolean = false): Int =
    customBinary(op(opcode, size), false, size, arg1, false, size, arg2, true, notToMemory = notToMemory)
  def binaryF(opcode: String, size: Int, arg1: Byte, arg2: Byte): Int =
    customBinary(opF(opcode, size), true, size, arg1, true, size, arg2, true)
  def customBinary(operation: String, float1: Boolean, size1: Int, arg1: Byte, float2: Boolean, size2: Int, arg2: Byte, dest2: Boolean, notToMemory: Boolean = false, pushFlags: Boolean = false): Int = {
    val binArg1 = arg(arg1)
    val binArg2 = if (dest2) dest(arg2) else arg(arg2)
    val (operand2, off2) =
      if (float2) getOperandF(binArg2, size2, rTmp2, 2, "%xmm1")
      else getOperand(binArg2, size2, rTmp2, 2)
    val (operand1, off1) =
      if (float1) getOperandF(binArg1, size1, rTmp1, off2, "%xmm0")
      else getOperand(binArg1, size1, rTmp1, off2)

    if (!float2 && (notToMemory || (!binArg1.isReg && !binArg2.isReg))) {
      if (notToMemory) {
        inst("pushq", Seq(rD(3))) // Save rOOT
        inst(op("mov", size2), Seq(operand2, rD(size2)))
        inst(operation, Seq(if (float1) "%xmm0" else operand1, rD(size2)))
        inst(op("mov", size2), Seq(rD(size2), operand2))
        inst("popq", Seq(rD(3))) // Restore rOOT
      } else {
        inst(op("mov", size1), Seq(if (float1) "%xmm0" else operand1, rTmp1(size1)))
        inst(operation, Seq(rTmp1(size1), operand2))        
        if (pushFlags) inst("pushf", Seq())
      }
    } else {
      inst(operation, Seq(if (float1) "%xmm0" else operand1, if (float2) "%xmm1" else operand2))
      if (pushFlags) inst("pushf", Seq())
      if (float2) {
        size2 match {
          case 0 =>
            inst("movq", Seq("%xmm1", rTmp2(3)))
            inst("movl", Seq(rTmp2(2), operand2))
          case 1 =>
            inst("movq", Seq("%xmm1", operand2))
        }
      }
    }
    off1
  }

  def genAssembly(opc: Short): String = {
    buffer.clear()

    val length: Int = p(opc, 0) match {
      case 0x0 => // Move
        binary("mov", p(opc, 1), p(opc, 3), p(opc, 2))

      case 0x1 => // Stack
        if (p(opc, 2) != 0) throw new Exception()
        val operation = p(opc, 1) & 0xc match {
          case 0x0 => "push"
          case 0x4 => "pop"
        }
        val size = p(opc, 1) & 0x3
        size match {
          case 0 | 2 =>
            val newOperation = op(operation, size + 1)
            val binArg = operation match {
              case "push" => arg(p(opc, 3))
              case "pop" => dest(p(opc, 3))
            }
            val (operand, off) = getOperand(binArg, size, rTmp1, 2)
            operation match {
              case "push" =>
                inst(size match {
                  case 0 => "movzbw"
                  case 2 => "movl"
                }, Seq(operand, rTmp1(size match {
                  case 0 => 1
                  case 2 => 2
                })))
                inst(newOperation, Seq(rTmp1(size + 1)))
              case "pop" =>
                inst(newOperation, Seq(rTmp2(size + 1)))
                inst(size match {
                  case 0 => "movb"
                  case 2 => "movl"
                }, Seq(rTmp2(size), operand))
            }
            off
          case 1 | 3 =>
            unary(operation, size, p(opc, 3), operation == "pop")
        }

      case 0x2 => // Memory
        if ((p(opc, 1) & 0x8) != 0) throw new Exception()
        val size = p(opc, 1) & 0x3
        p(opc, 1) & 0x4 match {
          case 0x0 => // Store
            val (address, off2) = getOperand(dest(p(opc, 2)), 3, rTmp2, 2) // Address
            val (value, off1) = getOperand(arg(p(opc, 3)), size, rTmp1, off2) // Value
            (dest(p(opc, 2)), arg(p(opc, 3))) match {
              case (Reg(_), Reg(_)) =>
                inst(op("mov", size), Seq(value, dataAt(address, 0)))
              case (Reg(_), _) =>
                inst(op("mov", size), Seq(value, rTmp1(size)))
                inst(op("mov", size), Seq(rTmp1(size), dataAt(address, 0)))
              case (_, Reg(_)) =>
                inst("movq", Seq(address, rTmp2(3)))
                inst(op("mov", size), Seq(value, dataAt(rTmp2(3), 0)))
              case _ =>
                inst("movq", Seq(address, rTmp2(3)))
                inst(op("mov", size), Seq(value, rTmp1(size)))
                inst(op("mov", size), Seq(rTmp1(size), dataAt(rTmp2(3), 0)))                
            }
            off1
          case 0x4 => // Load
            val (destination, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2) // Destination
            val (address, off1) = getOperand(dest(p(opc, 3)), 3, rTmp1, off2) // Address
            (dest(p(opc, 2)), dest(p(opc, 3))) match {
              case (Reg(_), Reg(_)) =>
                inst(op("mov", size), Seq(dataAt(address, 0), destination))
              case (Reg(_), _) =>
                inst("movq", Seq(address, rTmp1(3)))
                inst(op("mov", size), Seq(dataAt(rTmp1(3), 0), destination))
              case (_, Reg(_)) =>
                inst(op("mov", size), Seq(dataAt(address, 0), rTmp1(size)))
                inst(op("mov", size), Seq(rTmp1(size), destination))
              case _ =>
                inst("movq", Seq(address, rTmp1(3)))
                inst(op("mov", size), Seq(dataAt(rTmp1(3), 0), rTmp1(size)))
                inst(op("mov", size), Seq(rTmp1(size), destination))
            }
            off1
        }

      case (0x3 | 0x4 | 0x5) if ((p(opc, 1) & 0xc) == 0xc) => // Shifts
        val operand = p(opc, 0) match {
          case 0x3 => "shl"
          case 0x4 => "shr"
          case 0x5 => "sar"
        }
        val size = p(opc, 1) & 0x3
        
        val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
        val (operand1, off1) = getOperand(arg(p(opc, 3)), 0, rTmp1, off2)
        
        inst("pushq", Seq(rC(3)))
        inst("movb", Seq(operand1, rC(0)))
        inst(op(operand, size), Seq(rC(0), operand2))
        inst("popq", Seq(rC(3)))

        off1

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
          case 0x4 => "sub"
          case 0x8 => "imul"
        }
        val size = p(opc, 1) & 0x3
        
        (operand, size) match {
          case ("imul", 0) => // Can't multiply single bytes
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
          case _ => binary(operand, size, p(opc, 3), p(opc, 2), notToMemory = true)
        }

      case 0x5 => // Floating-point arithmetic
        val operand = p(opc, 1) & 0xc match {
          case 0x0 => "add"
          case 0x4 => "sub"
          case 0x8 => "mul"
        }
        val size = p(opc, 1) & 0x3
        binaryF(operand, size, p(opc, 3), p(opc, 2))

      case 0x6 | 0x7 => // Division and modulo
        val size = p(opc, 1) & 0x3
        p(opc, 1) & 0x8 match {
          case 0x0 => // Integer division and modulo
            // Step 1: Save rcx and rdx (we need 3 registers)
            inst("pushq", Seq(rC(3)))
            inst("pushq", Seq(rD(3)))

            // Step 2: Get operands so we can dispose of rax
            val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
            inst(op("mov", size), Seq(operand2, rC(size)))
            val (operand1, off1) = getOperand(arg(p(opc, 3)), size, rTmp1, off2)
            inst(op("mov", size), Seq(operand1, rTmp1(size)))

            // Step 3: Save rax
            inst("pushq", Seq(rA(3))) // Must be done after we get the operands

            // Step 4: Put divided in rdx:rax
            inst(op("mov", size), Seq(rC(size), rA(size)))

            // Step 5: Sign or zero extend divided
            p(opc, 1) & 0x4 match {
              case 0x0 => // Signed division
                size match {
                  case 0x0 => inst("cbtw", Seq())
                  case 0x1 => inst("cwtd", Seq())
                  case 0x2 => inst("cltd", Seq())
                  case 0x3 => inst("cqto", Seq())
                }
              case 0x4 => // Unsigned division
                inst(op("mov", size), Seq(imm(0), rD(size)))
            }

            // Step 6: Divide
            inst(op(if ((p(opc, 1) & 0x4) == 0x0) "idiv" else "div", size), Seq(rTmp1(size)))

            // Step 7: Get result
            val result = p(opc, 0) match {
              case 0x6 => rA(size)
              case 0x7 => rD(size)
            }
            inst(op("mov", size), Seq(result, rC(size)))

            // Step 8: Restore rax and rdx
            inst("popq", Seq(rA(3)))

            // Step 9: Store result
            inst(op("mov", size), Seq(rC(size), operand2))

            // Step 10: Restore rcx and rdx
            inst("popq", Seq(rD(3)))
            inst("popq", Seq(rC(3)))

            off1
          case 0x8 => // Floating-point division
            if (p(opc, 0) == 0x7) throw new Exception() // Floating point remainder not supported
            if (size > 1 || (p(opc, 1) & 0x4) == 1) throw new Exception()
            binaryF("div", size, p(opc, 3), p(opc, 2))
        }

      case 0x8 => // Truncation and floating-point extension
        p(opc, 1) & 0x8 match {
          case 0x0 =>
            val destsize = p(opc, 1) match {
              case 0x0 => 0
              case 0x1 => 0
              case 0x2 => 0
              case 0x3 => 1
              case 0x4 => 1
              case 0x5 => 2
            }
            binary("mov", destsize, p(opc, 3), p(opc, 2))
          case 0x8 =>
            val (operand, s1, s2) = p(opc, 1) match {
              case 0x8 => ("cvtsd2ss", 1, 0)
              case 0xf => ("cvtss2sd", 0, 1)
            }
            customBinary(operand, true, s1, p(opc, 3), true, s2, p(opc, 2), true)
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
        customBinary(operation, false, s1, p(opc, 3), false, s2, p(opc, 2), true, notToMemory = true)

      case 0xa => // Floating-point to integer
        val s1 = (p(opc, 1) & 0x4) >> 2
        val s2 = p(opc, 1) & 0x3 match {
          case s if s < 2 => 2
          case s          => s
        }
        val operation = (s1, p(opc, 1) & 0x8) match { // Unsigned conversion not supported
          case (0x0, 0x0) => op("cvttss2si", s2)
          case (0x1, 0x0) => op("cvttsd2si", s2)
        }
        customBinary(operation, true, s1, p(opc, 3), false, s2, p(opc, 2), true, notToMemory = true)

      case 0xb => // Integer to floating-point
        val size1 = (p(opc, 1) & 0x6) >> 1
        val size2 = p(opc, 1) & 0x1
        val operation = (size2, p(opc, 1) & 0x8) match { // Unsigned conversion not supported
          case (0x0, 0x0) => op("cvtsi2ss", if (size1 < 2) 2 else size1)
          case (0x1, 0x0) => op("cvtsi2sd", if (size1 < 2) 2 else size1)
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
          case _ => customBinary(operation, false, size1, p(opc, 3), true, size2, p(opc, 2), true)
        }

      case 0xc => // Control flow
        if (p(opc, 2) != 0) throw new Exception()
        p(opc, 1) match {
          case 0x0 => // Call
            val (operand, off) = getOperand(arg(p(opc, 3)), 3, rTmp1, 2)
            inst("pushq", Seq(rL))
            inst("movq", Seq(rPC, rL))
            inst("addq", Seq(imm(off), rL))
            inst("movq", Seq(operand, rPC))
          case 0x1 => // Jump
            val (operand, off) = getOperand(arg(p(opc, 3)), 3, rTmp1, 2)
            inst("movq", Seq(operand, rPC))
          case 0x2 => // JumpIf
            val (operand, off) = getOperand(arg(p(opc, 3)), 0, rTmp1, 2)
            val (jumpdest, offd) = getOperand(Mem, 3, rTmp2, off)
            inst("pushq", Seq(rOOT))
            inst("movq", Seq(rPC, rOOT))
            inst("addq", Seq(imm(offd), rOOT))
            inst("cmpb", Seq(imm(0), operand))
            inst("cmovneq", Seq(jumpdest, rOOT))
            inst("movq", Seq(rOOT, rPC))
            inst("popq", Seq(rOOT))
        }
        -1

      case 0xd => // Return and Halt
        if (p(opc, 2) != 0 || p(opc, 3) != 0) throw new Exception()
        p(opc, 1) & 0xe match {
          case 0x0 =>
            // Transfer execution to caller
            inst("movq", Seq(rL, rPC))
            // Restore previous rL
            inst("movq", Seq(offset(rReg, -24), rL))
            // Restore registers
            (0 until 8).foreach { idx =>
              inst("movq", Seq(offset(rReg, -8 * (idx + 4)), reg(idx, 3)))
            }
            // Restore register stack
            inst("movq", Seq(offset(rReg, -16), rReg))
          //case 0xf => inst("ret", Seq()) // Return from tight loop function TODO restore state
        }
        p(opc, 1) match {
          case 0x0 => -1
          case 0x1 => -2 // Put return value in register and return to loop
        }

      case 0xe if p(opc, 1) == 0xf => // Conditional ops
        val operation = p(opc, 2) match {
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
        unary(operation, 0, p(opc, 3), true, popFlags = true)

      case 0xe => // Comparisons
        val size = p(opc, 1) & 0x3
        p(opc, 1) & 0xc match {
          case 0x0 | 0x4 =>
            customBinary(op("cmp", size), false, size, p(opc, 3), false, size, p(opc, 2), false, pushFlags = true)
          case 0x8 =>
            customBinary(opF("ucomi", size), true, size, p(opc, 3), true, size, p(opc, 2), false, pushFlags = true)
        }

      case 0xf if (p(opc, 1) == 0xf) => // Alloc
        val (operand2, off2) = getOperand(dest(p(opc, 2)), 3, rTmp2, 2) // Destination
        inst("movq", Seq(offset(rReg, -8), rTmp1(3)))
        inst("imulq", Seq(imm(8), rTmp1(3)))
        inst("addq", Seq(rReg, rTmp1(3)))
        inst("movq", Seq(rTmp1(3), operand2))
        inst("subq", Seq(rCode, operand2))

        val (operand1, off1) = getOperand(arg(p(opc, 3)), 1, rTmp1, off2) // Size
        inst("movzwq", Seq(operand1, rTmp1(3)))
        inst("addq", Seq(rTmp1(3), offset(rReg, -8)))

        off1

      case 0xf if (p(opc, 1) == 0xe) => // Function header
        // Step 1: compute new rReg
        inst("movq", Seq(offset(rReg, -8), rTmp1(3)))
        inst("addq", Seq(imm(11), rTmp1(3))) // 11 = (saved regs (8) + prev rL + prev rReg + size)
        inst("imulq", Seq(imm(8), rTmp1(3)))
        inst("addq", Seq(rReg, rTmp1(3))) // rTmp1 holds the address of the first free slot

        // Step 2: save registers
        (0 until 8).foreach { idx =>
          inst("movq", Seq(reg(idx, 3), offset(rTmp1(3), -8 * (idx + 4))))
        }

        // Step 3: create register stack
        inst("popq", Seq(offset(rTmp1(3), -24))) // Pop rL
        inst("movq", Seq(rReg, offset(rTmp1(3), -16)))
        inst("movq", Seq(imm(opc & 0xff), offset(rTmp1(3), -8)))
        inst("movq", Seq(rTmp1(3), rReg))
        2

      case 0xf => // Builtin functions
        val builtins: Seq[(String, Int, Seq[Int])] = // Builtin names, return size, argument sizes (-1 == physical pointer)
          Seq(
            ("scalanative_init", 64, Seq()),
            ("scalanative_alloc", -1, Seq(64, 64)),
            ("scalanative_field", 64, Seq(64, -1, 32)),
            ("scalanative_method_virtual", 64, Seq(-1, 32)),
            ("scalanative_method_trait", 64, Seq(-1, -1, 32, 32)),
            ("scalanative_is_class", 8, Seq(-1, -1)),
            ("scalanative_is_trait", 8, Seq(-1, -1, 32)),
            ("llvm_memset", 64, Seq(-1, 8, 64, 32, 8)),
            ("llvm_memmove", 64, Seq(-1, -1, 64, 32, 8)),
            ("llvm_ctpop", 32, Seq(32)),
            ("llvm_bswap", 32, Seq(32)),
            ("llvm_ctlz", 32, Seq(32)),
            ("scalanative_platform_is_windows", 8, Seq()),
            ("scalanative_strlen", 64, Seq(-1)),
            ("scalanative_environ", -1, Seq()),
            ("scalanative_stdin_fileno", 32, Seq()),
            ("scalanative_stdout_fileno", 32, Seq()),
            ("scalanative_stderr_fileno", 32, Seq()),
            ("scalanative_nano_time", 64, Seq()),
            ("write", 32, Seq(32, -1, 64))
          )
        val (name, ret, args) = builtins(opc & 0xfff)

        val callerSavedRegs: Seq[String] = Seq(rA(3), rC(3), rD(3), rPC) ++ (0 to 3).map(reg(_,3))

        // Step 1: save all registers on Register stack
        inst("movq", Seq(offset(rReg, -8), rTmp1(3)))
        inst("imulq", Seq(imm(8), rTmp1(3)))
        inst("addq", Seq(rReg, rTmp1(3)))
        inst("movq", Seq(rReg, offset(rTmp1(3), 0)))
        inst("movq", Seq(rTmp1(3), rReg))
        callerSavedRegs.zipWithIndex.foreach { case (reg, idx) =>
          inst("movq", Seq(reg, offset(rReg, (idx+1)*8)))
        }
        
        // Step 2: Put arguments in registers (max. 6 arguments)
        val argRegs: Seq[Int => String] = Seq(rTmp2, rTmp1, rD, rC, reg(0, _), reg(1, _))
        argRegs.zip(args).foreach {
          case (r, -1) => // pointer
            inst("popq", Seq(r(3)))
            inst("addq", Seq(rCode, r(3))) // Convert bytecode address to physical address
          case (r, 8 | 16) => // normal value
            inst("popw", Seq(r(1))) // Upper bytes will be ignored on smaller sizes
          case (r, 32 | 64) =>
            inst("popq", Seq(r(3)))
        }

        // Step 3: Align stack
        inst("movq", Seq(rSP, rPC)) // Callee-saved register
        inst("andq", Seq(imm(-16), rSP))
        inst("pushq", Seq(imm(0))) // Padding to maintain 16-byte alignment
        inst("pushq", Seq(rPC))

        // Step 4: Call function
        inst("call", Seq("_" + name))

        // Step 5: Restore stack
        inst("popq", Seq(rPC))
        inst("popq", Seq(rTmp1(3)))
        inst("movq", Seq(rPC, rSP))

        // Step 6: Get return value
        ret match {
          case 8 | 16 =>
            inst("pushw", Seq(rA(1)))
          case 32 | 64 | -1 =>
            inst("pushq", Seq(rA(3)))
          case 0 => ()
        }

        // Step 7: Restore state
        callerSavedRegs.zipWithIndex.foreach { case (reg, idx) =>
          inst("movq", Seq(offset(rReg, (idx+1)*8), reg))
        }
        inst("movq", Seq(offset(rReg, 0), rReg))

        if (ret == -1) { // pointer
            inst("subq", Seq(rCode, offset(rSP, 0))) // Convert physical address to bytecode address
        }
        2
    }
    if (length == -2) { // final return
      inst("addq", Seq(rCode, rPC))
      inst("jmpq", Seq("*" + rPC))
    } else {
      if (length != -1) {
        inst("addq", Seq(imm(length), rPC))
      }
      inst("movzwq", Seq(dataAt(rPC, 0), rTmp1(3))) // Opcode is in tmp
      inst("jmp", Seq("*(" + rOOT + "," + rTmp1(3) + ",8)"))
    }

    val labelName = "assembly_" + (0 until 4).map(i => Integer.toHexString(p(opc, i))).mkString("")
    ".globl " + labelName + "\n" + labelName + ":\n    " + buffer.mkString(" # " + labelName + "\n    ") + "\n"
  }

  def apply(): Unit = {
    val file = new File("src/assembly.S")
    val hFile = new File("inc/opcode_offset_table.h")
    val cFile = new File("src/opcode_offset_table.c")
    val bw = new BufferedWriter(new FileWriter(file))
    val hbw = new BufferedWriter(new FileWriter(hFile))
    val cbw = new BufferedWriter(new FileWriter(cFile))
    cbw.write("#include <stdlib.h>\n#include \"opcode_offset_table.h\"\n\nvoid* opcode_offset_table[65536] = {\n")
    (0 to 0xffff).foreach { x =>
      try {
        val assembly = genAssembly(x.toShort)
        bw.write(assembly)
        hbw.write("extern void* assembly_" + String.format("%04x", x: Integer) + " asm(\"assembly_" + String.format("%04x", x: Integer) + "\");\n")
        cbw.write("    &assembly_" + String.format("%04x", x: Integer) + ", \n")
      } catch {
        case _: Throwable =>
          cbw.write("    NULL, \n")
      }
    }
    cbw.write("};")
    bw.close()
    hbw.close()
    cbw.close()
  }
}
