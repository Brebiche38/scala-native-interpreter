package scala.scalanative.nbc

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable

object GenAssembly {
  // We use AT&T syntax

  val buffer: mutable.UnrolledBuffer[String] = mutable.UnrolledBuffer.empty[String]

  val builtins: Seq[(String, Int, Seq[Int])] = // Builtin names, return size (-1 = physical pointer), argument sizes (-1 == physical pointer)
    Seq(
      ("scalanative_init", 64, Seq()),
      ("scalanative_alloc", -1, Seq(64, 64)),
      ("scalanative_field", 64, Seq(64, -1, 32)),
      ("scalanative_method_virtual", 64, Seq(-1, 32)),
      ("scalanative_method_static", 64, Seq(64)),
      ("scalanative_method_trait", 64, Seq(-1, -1, 32, 32)),
      ("scalanative_is_class", 8, Seq(-1, -1)),
      ("scalanative_is_trait", 8, Seq(-1, -1, 32)),
      ("scalanative_alloc", -1, Seq(64, 64)),
      ("scalanative_alloc_atomic", -1, Seq(64, 64)),
      ("llvm_memset", 64, Seq(-1, 8, 64, 32, 8)),
      ("scalanative_unwind_get_context", 32, Seq(-1)),
      ("scalanative_unwind_init_local", 32, Seq(-1, -1)),
      ("scalanative_unwind_step", 32, Seq(-1)),
      ("scalanative_unwind_get_proc_name", 32, Seq(-1, -1, 64, -1)),
      ("llvm_ctpop", 32, Seq(32)),
      ("llvm_bswap", 32, Seq(32)),
      ("llvm_ctlz", 32, Seq(32)),
      ("elem", 64, Seq(64, 64, 64, 64)),
      ("scalanative_platform_is_windows", 8, Seq()),
      ("scalanative_environ", -1, Seq()),
      ("scalanative_strlen", 64, Seq(-1)),
      ("llvm_memmove", 64, Seq(-1, -1, 64, 32, 8)),
      ("stacktrace_in", 64, Seq(64)),
      ("stacktrace_out", 64, Seq()),
      ("scalanative_stdin_fileno", 32, Seq()),
      ("scalanative_stdout_fileno", 32, Seq()),
      ("scalanative_stderr_fileno", 32, Seq()),
      ("write", 32, Seq(32, -1, 64))
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
    case 0x9        => Nop
    //case 0xf        => Mem
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
  def allRegs: Seq[String] = Seq(rCode, rReg, rL, rOOT, rPC) ++ (0 until 8).map(reg(_,3))
  def dataAt(reg: String, off: Int): String = (off match {
    case 0 => ""
    case _ => off.toString
  }) + "(" + rCode + "," + reg + ")"
  def offset(reg: String, off: Int): String = (off match {
    case 0 => ""
    case _ => off.toString
  }) + "(" + reg + ")"
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
  final case object Nop extends BinArg

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
  def binary(opcode: String, size: Int, arg1: Byte, arg2: Byte): Int =
    customBinary(op(opcode, size), false, size, arg1, false, size, arg2, true)
  def binaryF(opcode: String, size: Int, arg1: Byte, arg2: Byte): Int =
    customBinary(opF(opcode, size), true, size, arg1, true, size, arg2, true)
  def customBinary(operation: String, float1: Boolean, size1: Int, arg1: Byte, float2: Boolean, size2: Int, arg2: Byte, dest2: Boolean, pushFlags: Boolean = false): Int = {
    val binArg1 = arg(arg1)
    val binArg2 = if (dest2) dest(arg2) else arg(arg2)
    val (operand2, off2) =
      if (float2) getOperandF(binArg2, size2, rTmp2, 2, "%xmm1")
      else getOperand(binArg2, size2, rTmp2, 2)
    val (operand1, off1) =
      if (float1) getOperandF(binArg1, size1, rTmp1, off2, "%xmm0")
      else getOperand(binArg1, size1, rTmp1, off2)
    if (!float1 && !float2 && (!binArg1.isInstanceOf[Reg]) && (!binArg2.isInstanceOf[Reg]) && (binArg2 != Nop)) {
      if (!pushFlags) {
        def rD(size: Int) = size match {
          case 0 => "%dl"
          case 1 => "%dx"
          case 2 => "%edx"
          case 3 => "%rdx"
        }

        inst("pushq", Seq(rOOT))
        inst(op("mov", size2), Seq(operand2, rD(size2)))
        inst(operation, Seq(operand1, rD(size2)))
        inst(op("mov", size2), Seq(rD(size2), operand2))
        inst("popq", Seq(rOOT))
      } else {
        inst(op("mov", size2), Seq(operand2, rTmp2(size2)))
        inst(operation, Seq(operand1, rTmp2(size2)))
        inst("pushf", Seq())
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
        
        inst("pushq", Seq("%rcx"))
        inst("movb", Seq(operand1, "%cl"))
        inst(op(operand, size), Seq("%cl", operand2))
        inst("popq", Seq("%rcx"))

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
            // Step -1: Get operands so we can dispose of rax
            val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
            inst(op("mov", size), Seq(operand2, rTmp2(size)))
            val (operand1, off1) = getOperand(arg(p(opc, 3)), size, rTmp1, off2)
            inst(op("mov", size), Seq(operand1, rTmp1(size)))

            // Step 0: Save rax and rdx
            inst("pushq", Seq(rCode))
            inst("pushq", Seq(rOOT))

            // Step 1: Put divided in rdx:rax
            inst(op("mov", size), Seq(rTmp2(size), rA(size)))

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
            inst(op(if ((p(opc, 1) & 0x4) == 0x0) "idiv" else "div", size), Seq(rTmp1(size)))

            // Step 4: Get result
            inst(op("mov", size), Seq(rA(size), rTmp2(size)))

            // Step 5: Restore rax and rdx
            inst("popq", Seq(rOOT))
            inst("popq", Seq(rCode))

            // Step 6: Store result
            inst(op("mov", size), Seq(rTmp2(size), operand2))

            off1
          case 0x8 => // Floating-point division
            if (size > 1 || (p(opc, 1) & 0x4) == 1) throw new Exception()
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
            // Step -1: Get operands so we can dispose of rax
            val (operand2, off2) = getOperand(dest(p(opc, 2)), size, rTmp2, 2)
            inst(op("mov", size), Seq(operand2, rTmp2(size)))
            val (operand1, off1) = getOperand(arg(p(opc, 3)), size, rTmp1, off2)
            inst(op("mov", size), Seq(operand1, rTmp1(size)))

            // Step 0: Save rax and rdx
            inst("pushq", Seq(rCode))
            inst("pushq", Seq(rOOT))

            // Step 1: Put divided in rdx:rax
            inst(op("mov", size), Seq(rTmp2(size), rA(size)))

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
            inst(op("idiv", size), Seq(rTmp1(size)))

            // Step 4: Get result
            inst(op("mov", size), Seq(rD(size), rTmp2(size)))

            // Step 5: Restore rax and rdx
            inst("popq", Seq(rOOT))
            inst("popq", Seq(rCode))

            // Step 6: Store result
            inst(op("mov", size), Seq(rTmp2(size), operand2))

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

        (arg(p(opc, 3)), dest(p(opc, 2))) match {
          case (Reg(_), (SReg | Mem)) => // Can't extend directly into memory
            val (operand2, off2) = getOperand(dest(p(opc, 2)), s2, rTmp2, 2)
            val (operand1, off1) = getOperand(arg(p(opc, 3)), s1, rTmp1, off2)

            def rD(size: Int) = size match {
              case 0 => "%dl"
              case 1 => "%dx"
              case 2 => "%edx"
              case 3 => "%rdx"
            }
            
            inst("pushq", Seq(rOOT))
            inst(op("mov", s2), Seq(operand2, rD(s2)))
            inst(operation, Seq(operand1, rD(s2)))
            inst(op("mov", s2), Seq(rD(s2), operand2))
            inst("popq", Seq(rOOT))
            off1
          case _ => customBinary(operation, false, s1, p(opc, 3), false, s2, p(opc, 2), true)
        }

      case 0xa => // Floating-point to integer TODO unsigned conversion is tricky
        val s1 = (p(opc, 1) & 0x4) >> 2
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
          case _ => customBinary(operation, true, s1, p(opc, 3), false, s2, p(opc, 2), true)
        }

      case 0xb => // Integer to floating-point
        val size1 = (p(opc, 1) & 0x6) >> 1
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
          case _ => customBinary(operation, false, size1, p(opc, 3), true, size2, p(opc, 2), true)
        }

      case 0xc => // Control flow
        if (p(opc, 2) != 0) throw new Exception()
        p(opc, 1) match {
          case 0x0 => // Call
            // Step 2: call function
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
        val (name, ret, args) = builtins(opc & 0xfff)

        // Step 1: save all registers on Register stack
        inst("movq", Seq(offset(rReg, -8), rTmp1(3)))
        inst("imulq", Seq(imm(8), rTmp1(3)))
        inst("addq", Seq(rReg, rTmp1(3)))
        inst("movq", Seq(rReg, offset(rTmp1(3), 0)))
        inst("movq", Seq(rTmp1(3), rReg))
        allRegs.zipWithIndex.foreach { case (reg, idx) =>
          inst("movq", Seq(reg, offset(rReg, (idx+1)*8)))
        }

        // Step 2: Put arguments in registers (max. 6 arguments)
        val argRegs = Seq(("%rdi", "%di"), ("%rsi", "%si"), ("%rdx", "%dx"), ("%rcx", "%cx"), ("%r8", "%r8w"), ("%r9", "%r9w"))
        argRegs.zip(args).foreach {
          case (reg, -1) => // pointer
            inst("popq", Seq(reg._1))
            inst("addq", Seq(rCode, reg._1)) // Convert bytecode address to physical address
          case (reg, 8 | 16) => // normal value
            inst("popw", Seq(reg._2)) // Upper bytes will be ignored on smaller sizes
          case (reg, 32 | 64) =>
            inst("popq", Seq(reg._1))
        }

        // Step 3a: Align stack
        inst("movq", Seq(rSP, rPC)) // Callee-saved register
        inst("andq", Seq(imm(-16), rSP))
        inst("pushq", Seq(rPC))
        inst("pushq", Seq(rPC)) // Twice to maintain 16-byte alignment

        // Step 3: Call function
        inst("call", Seq("_" + name))

        // Step 3b: Restore stack
        inst("popq", Seq(rPC)) // Twice to restore alignment
        inst("popq", Seq(rPC))
        inst("movq", Seq(rPC, rSP))

        // Step 4: Get return value
        ret match {
          case 8 | 16 =>
            inst("pushw", Seq("%ax"))
          case 32 | 64 | -1 =>
            inst("pushq", Seq("%rax"))
          case 0 => ()
        }

        // Step 5: Restore state
        allRegs.zipWithIndex.foreach { case (reg, idx) =>
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
        case t: Throwable =>
          if (x == 0x1609) t.printStackTrace
          cbw.write("    NULL, \n")
      }
    }
    cbw.write("};")
    bw.close()
    hbw.close()
    cbw.close()
  }
}
