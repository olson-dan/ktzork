import kotlinx.cinterop.ByteVar
import kotlinx.cinterop.allocArray
import kotlinx.cinterop.memScoped
import kotlinx.cinterop.readBytes
import platform.posix.*
import kotlin.math.min


fun <T> ArrayList<T>.pop(): T {
    val last = this[this.lastIndex]
    this.removeAt(this.lastIndex)
    return last
}

fun <T> ArrayList<T>.last(): T {
    return this[this.lastIndex]
}

val opnames: Array<Array<String>> = arrayOf(
    arrayOf(
        "rtrue",
        "rfalse",
        "print",
        "print_ret",
        "no",
        "save",
        "restore",
        "restart",
        "ret_popped",
        "pop",
        "quit",
        "new_line",
        "show_status",
        "verify",
        "extended",
        "piracy",
    ),
    arrayOf(
        "jz",
        "get_sibling",
        "get_child",
        "get_parent",
        "get_prop_len",
        "inc",
        "dec",
        "print_addr",
        "call_1s",
        "remove_obj",
        "print_obj",
        "ret",
        "jump",
        "print_paddr",
        "load",
        "not",
        "call_1n",
    ),
    arrayOf(
        "none",
        "je",
        "jl",
        "jg",
        "dec_chk",
        "inc_chk",
        "jin",
        "test",
        "or",
        "and",
        "test_attr",
        "set_attr",
        "clear_attr",
        "store",
        "insert_obj",
        "loadw",
        "loadb",
        "get_prop",
        "get_prop_addr",
        "get_next_prop",
        "add",
        "sub",
        "mul",
        "div",
        "mod",
        "call_2s",
        "call_2n",
        "set_colour",
    ),
    arrayOf(
        "call",
        "storew",
        "storeb",
        "put_prop",
        "sread",
        "print_char",
        "print_num",
        "random",
        "push",
        "pull",
        "split_window",
        "set_window",
        "call-vs2",
        "erase_window",
        "erase_line",
        "set_cursor",
        "get_cursor",
        "set_text_style",
        "buffer_mode",
        "output_stream",
        "input_stream",
        "sound_effect",
        "read_char",
        "scan_table",
        "not_v4",
        "call_vn",
        "call_vn2",
        "tokenise",
        "encode_text",
        "copy_table",
        "print_table",
        "check_arg_count",
    )
)


enum class ZStringShift {
    ZERO, ONE, TWO
}

class ZString(private val memory: Memory, private val offset: Int, private val maxLength: Int?) {
    var length = 0
    var contents = ""

    init {
        var bytes = ArrayList<Byte>()
        while (true) {
            if (length == maxLength) {
                break
            }
            val x = memory.readU16(offset + length)
            length += 2

            bytes.add((x.shr(10).and(0x1f).toByte()))
            bytes.add(x.shr(5).and(0x1f).toByte())
            bytes.add(x.and(0x1f).toByte())

            if (x.and(0x8000) != 0) {
                break
            }
        }
        withBytes(memory, bytes.toByteArray())
    }

    fun withBytes(memory: Memory, bytes: ByteArray) {
        var shift = ZStringShift.ZERO
        var skipCount = 0
        bytes.forEachIndexed { index, c ->
            if (skipCount != 0) {
                skipCount -= 1
            } else {
                when (c.toInt()) {
                    0 -> contents += " "
                    1, 2, 3 -> {
                        skipCount = 1 // Skip abbrev
                        val abbrevIndex = bytes[index + 1]
                        val table = memory.readU16(0x18)
                        val tableIndex = 32 * (c - 1) + abbrevIndex
                        val tableOffset = memory.readU16(table + tableIndex * 2)
                        val abbrev = ZString(memory, tableOffset * 2, null)
                        contents += abbrev.contents
                    }

                    4 -> shift = ZStringShift.ONE
                    5 -> shift = ZStringShift.TWO
                    else -> {
                        if (shift == ZStringShift.TWO && c.toInt() == 6) {
                            skipCount = 2
                            // TODO
                        } else {
                            contents += when (shift) {
                                ZStringShift.ZERO -> "______abcdefghijklmnopqrstuvwxyz"[c.toInt()]
                                ZStringShift.ONE -> "______ABCDEFGHIJKLMNOPQRSTUVWXYZ"[c.toInt()]
                                ZStringShift.TWO -> "______^\n0123456789.,!?_#'\"/\\-:()"[c.toInt()]
                            }
                        }
                        shift = ZStringShift.ZERO
                    }
                }
            }
        }
    }
}

class Memory(private val memory: ByteArray) {
    val length = memory.size

    fun readU16(offset: Int): Int {
        return ((memory[offset].toUByte().toInt() shl 8) or memory[offset + 1].toUByte().toInt())
    }

    fun readU8(offset: Int): Int {
        return memory[offset].toUByte().toInt()
    }

    fun writeU8(offset: Int, value: Int) {
        memory[offset] = (value and 0xff).toByte()
    }

    fun writeU16(offset: Int, value: Int) {
        memory[offset] = ((value shl 8) and 0xff).toByte()
        memory[offset + 1] = (value and 0xff).toByte()
    }
}

class Header(memory: Memory) {
    val dynamicStart = 0
    val dynamicEnd = memory.readU16(0xe)
    val staticStart = dynamicEnd
    val staticEnd = staticStart + min(0xffff, memory.length)
    val highStart = memory.readU16(0x4)
    val highEnd = memory.length
    val globals = memory.readU16(0xc)
    val checksum = memory.readU16(0x1c)
}

enum class Encoding {
    Op0, Op1, Op2, Var
}

enum class OperandType {
    Large, Small, Variable, Indirect, Omitted
}

enum class RetType {
    Variable, Indirect, Omitted
}

fun Int.hex(length: Int): String {
    return toString(16).padStart(length, '0')
}

class Operand(val type: OperandType, val value: Int) {
    override fun toString(): String {
        return when (type) {
            OperandType.Large -> "#" + value.hex(2)
            OperandType.Small -> "#" + value.hex(2)
            OperandType.Variable -> if (value == 0) {
                "(SP)+"
            } else if (this.value > 0x10) {
                "G" + (value - 0x10).hex(2)
            } else {
                "L" + (value - 1).hex(2)
            }

            OperandType.Indirect -> if (value == 0) {
                "[(SP)]"
            } else if (value > 0x10) {
                "[G" + (value - 0x10).hex(2) + "]"
            } else {
                "[L" + (value - 1).hex(2) + "]"
            }

            OperandType.Omitted -> ""
        }
    }
}

class Return(val retType: RetType, val value: Int) {
    override fun toString(): String {
        return when (retType) {
            RetType.Indirect -> {
                if (value == 0) {
                    " -> -(SP)"
                } else if (value > 0x10) {
                    "-> G" + (value - 0x10).hex(2)
                } else {
                    "-> L" + (value - 1).hex(2)
                }
            }

            RetType.Variable -> {
                if (value == 0) {
                    " -> (SP)"
                } else if (value > 0x10) {
                    "-> G" + (value - 0x10).hex(2)
                } else {
                    "-> L" + (value - 1).hex(2)
                }
            }

            RetType.Omitted -> ""
        }
    }
}

class Instruction(private val memory: Memory, private val ip: Int) {
    private var optype = Encoding.Op0
    private var opcode = 0
    var name = ""
    var length = 0
    var ret = Return(RetType.Omitted, 0)
    var args: Array<Operand> = arrayOf()
    var string: ZString? = null

    init {
        val op = memory.readU8(ip)
        when ((op and 0xc0) shr 6) {
            3 -> decodeVar(op)
            2 -> decodeShort(op)
            else -> decodeLong(op)
        }
        name = opnames[optype.ordinal][opcode]
        addReturn()
        addBranch()
        addPrint()
    }


    private fun decodeShort(op: Int) {
        opcode = op and 0xf
        when ((op and 0x30) shr 4) {
            3 -> {
                optype = Encoding.Op0
                length = 1
            }

            2 -> {
                optype = Encoding.Op1
                length = 2
                args = args.plusElement(Operand(OperandType.Variable, memory.readU8(ip + 1)))
            }

            1 -> {
                optype = Encoding.Op1
                length = 2
                args = args.plusElement(Operand(OperandType.Small, memory.readU8(ip + 1)))
            }

            else -> {
                optype = Encoding.Op1
                length = 3
                args = args.plusElement(Operand(OperandType.Large, memory.readU16(ip + 1)))
            }
        }

    }

    private fun decodeLong(op: Int) {
        val x = memory.readU8(ip + 1)
        val y = memory.readU8(ip + 2)
        opcode = op and 0x1f
        optype = Encoding.Op2
        length = 3
        args = if ((op and 0x40) != 0) {
            args.plusElement(Operand(OperandType.Variable, x))
        } else {
            args.plusElement(Operand(OperandType.Small, x))
        }
        args = if ((op and 0x20) != 0) {
            args.plusElement(Operand(OperandType.Variable, y))
        } else {
            args.plusElement(Operand(OperandType.Small, y))
        }
    }

    private fun decodeVar(op: Int) {
        val optypes = memory.readU8(ip + 1)
        length = 2
        opcode = op and 0x1f
        optype = if ((op and 0x20) != 0) {
            Encoding.Var
        } else {
            Encoding.Op2
        }
        for (index in 0..3) {
            val shift = (3 - index) * 2
            val mask = 3 shl shift
            args = when ((optypes and mask) shr shift) {
                3 -> args
                2 -> {
                    length += 1
                    args.plusElement(Operand(OperandType.Variable, memory.readU8(ip * length - 1)))
                }

                1 -> {
                    length += 1
                    args.plusElement(Operand(OperandType.Small, memory.readU8(ip + length - 1)))
                }

                else -> {
                    length += 2
                    args.plusElement(Operand(OperandType.Large, memory.readU16((ip + length - 2))))
                }
            }
        }
    }

    private fun addReturn() {
        when (optype) {
            Encoding.Op2 -> {
                if ((opcode in 0x08..0x09) || (opcode in 0x0f..0x19)) {
                    ret = Return(RetType.Variable, memory.readU8(ip + length))
                    length += 1
                }
            }

            Encoding.Op1 -> {
                if ((opcode in 0x01..0x04) || (opcode == 0x08) || (opcode in 0x0e..0x0f)) {
                    ret = Return(RetType.Variable, memory.readU8(ip + length))
                    length += 1
                }
            }

            Encoding.Var -> {
                if (opcode == 0x0 || opcode == 0x7) {
                    ret = Return(RetType.Variable, memory.readU8(ip + length))
                    length += 1
                }
            }

            else -> {}
        }
    }

    private fun addBranch() {

    }

    private fun addPrint() {
        if (optype == Encoding.Op0 && (opcode == 2 || opcode == 3)) {
            string = ZString(memory, ip + length, null)
            length += string!!.length
        }
    }

    override fun toString(): String {
        val dispArgs = args.joinToString { "$it" }
        val dispName = name.uppercase()
        val offset = ip.hex(8).uppercase()
        val dispString = string?.contents?.let { "\"$it\"" } ?: ""
        return "[$offset] $dispName\t$dispArgs$ret$dispString"
    }
}

data class Frame(val addr: Int, val stackStart: Int, val numLocals: Int, val returnStorage: Return, val returnAddr: Int)

class Machine(private var memory: Memory, private val header: Header) {
    private var finished = false
    private var ip = memory.readU16(0x6)
    private var stack = ArrayList<Int>()
    private var frames = ArrayList<Frame>()

    fun run() {
        while (!finished) {
            val instruction = Instruction(memory, ip)
            execute(instruction)
        }
    }

    private fun Instruction.vars(): List<Int> {
        return args.map { readVar(it) }
    }

    private fun writeLocal(v: Int, value: Int) {
        if (frames.size > 0) {
            val frame = frames.last()
            val index = frame.stackStart + v
            stack[index] = value
        }
    }

    private fun readLocal(v: Int): Int {
        return if (frames.size > 0) {
            val frame = frames.last()
            val index = frame.stackStart + v
            stack[index]
        } else {
            0
        }
    }

    private fun readDirect(v: Operand): Int {
        return readVar(v)
    }

    private fun readGlobal(v: Int): Int {
        val offset = header.globals + header.dynamicStart + v * 2
        return memory.readU16(offset)
    }

    private fun writeGlobal(v: Int, value: Int) {
        val offset = header.globals + header.dynamicStart + v * 2
        memory.writeU16(offset, value)
    }

    private fun readVar(v: Operand): Int {
        return if (v.type == OperandType.Variable || v.type == OperandType.Indirect) {
            if (v.value >= 0x10) {
                readGlobal(v.value - 0x10)
            } else if (v.value == 0) {
                if (v.type == OperandType.Indirect) {
                    stack.last()
                } else {
                    stack.pop()
                }
            } else {
                readLocal(v.value - 1)
            }
        } else {
            v.value
        }
    }

    private fun writeVar(v: Return, value: Int) {
        if (v.retType == RetType.Variable || v.retType == RetType.Indirect) {
            if (v.value >= 0x10) {
                writeGlobal(v.value - 0x10, value)
            } else if (v.value == 0) {
                if (v.retType != RetType.Indirect) {
                    stack.add(value)
                }
            } else {
                writeLocal(v.value - 1, value)
            }
        }
    }

    private fun call(i: Instruction) {
        val addr = header.dynamicStart + readVar(i.args[0]) * 2
        val retAddr = ip + i.length
        val args = i.args.slice(1 until i.args.size).map { readVar(it) }
        if (addr - header.dynamicStart == 0) {
            writeVar(i.ret, 0)
            ip = retAddr
        } else {
            val numLocals = memory.readU8(addr)
            frames.add(Frame(addr, stack.size, numLocals, i.ret, retAddr))
            for (arg in 0 until numLocals) {
                if (arg < args.size) {
                    stack.add(args[arg])
                } else {
                    val x = memory.readU16(addr + 1 + arg * 2)
                    stack.add(x)
                }
            }
            ip = addr + 1 + numLocals * 2
        }
    }

    private fun ret(value: Int) {
        val frame = frames.pop()
        while (stack.lastIndex > frame.stackStart) {
            stack.pop()
        }
        writeVar(frame.returnStorage, value)
        ip = frame.returnAddr
    }

    private fun execute(i: Instruction) {
        val startIP = ip
        when (i.name) {
            "call" -> call(i)
            "print" -> print(i.string!!.contents)
            "rtrue" -> ret(1)
            "rfalse" -> ret(0)
            "store" -> {
                val (x, y) = i.vars()
                writeVar(Return(RetType.Indirect, x), y)
            }

            "print_paddr" -> {
                val (x) = i.vars()
                val paddr = header.dynamicStart + 2 * x
                val s = ZString(memory, paddr, null)
                print(s.contents)
            }

            "inc" -> {
                val (x) = i.args.map { readDirect(it) }
                val old = readVar(Operand(OperandType.Variable, x))
                val inc = (old + 1).mod(0x10000)
                writeVar(Return(RetType.Variable, x), inc)
            }

            else -> {
                println("Unknown instruction:\n$i")
                finished = true
                return
            }
        }
        if (ip == startIP) {
            ip += i.length
        }
    }
}

fun readAllBytes(filename: String): ByteArray {
    val fh = open(filename, O_RDONLY)
    if (fh == -1) {
        throw IllegalArgumentException("Cannot open input file $filename")
    }
    try {
        val len = lseek(fh, 0, SEEK_END)
        if (len.toInt() == -1) {
            throw IllegalStateException("Couldn't get length of $filename")
        }
        lseek(fh, 0, SEEK_SET)

        memScoped {
            val buffer = allocArray<ByteVar>(len.toInt())
            val readCount = read(fh, buffer, len.toULong())

            if (readCount.toULong() != len.toULong()) {
                throw IllegalStateException("Error when reading $filename")
            }

            return buffer.readBytes(len.toInt())
        }
    } finally {
        close(fh)
    }
}

fun main(args: Array<String>) {
    val memory = Memory(readAllBytes(args.firstOrNull() ?: "czech.z3"))
    val header = Header(memory)
    val machine = Machine(memory, header)
    machine.run()
}