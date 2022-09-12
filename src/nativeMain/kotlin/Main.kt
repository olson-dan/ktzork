import kotlinx.cinterop.ByteVar
import kotlinx.cinterop.allocArray
import kotlinx.cinterop.memScoped
import kotlinx.cinterop.readBytes
import platform.posix.*
import kotlin.math.min
import kotlin.random.Random


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

open class ZString(private val memory: Memory, private val offset: Int) {
    var length = 0
    var contents = ""

    init {
        withBytes(memory, decode(null))
    }

    private fun decode(maxLength: Int?): ByteArray {
        val bytes = ArrayList<Byte>()
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
        return bytes.toByteArray()
    }

    private fun withBytes(memory: Memory, bytes: ByteArray) {
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
                        val abbrev = ZString(memory, tableOffset * 2)
                        contents += abbrev.contents
                    }

                    4 -> shift = ZStringShift.ONE
                    5 -> shift = ZStringShift.TWO
                    else -> {
                        if (shift == ZStringShift.TWO && c.toInt() == 6) {
                            skipCount = 2
                            val (x, y) = bytes.slice(index + 1..index + 2).map { it.toInt() }
                            contents += x.shl(5).or(y.and(0x1f)).toChar()
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

    override fun toString(): String {
        return contents
    }
}

class Memory(private val memory: ByteArray) {
    val length = memory.size

    fun readU16(offset: Int) = (readU8(offset).shl(8).or(readU8(offset + 1)))
    fun readU8(offset: Int) = memory[offset].toUByte().toInt()

    fun writeU8(offset: Int, value: Int) {
        memory[offset] = (value and 0xff).toByte()
    }

    fun writeU16(offset: Int, value: Int) {
        writeU8(offset, value shr 8)
        writeU8(offset + 1, value)
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

fun Int.hex(length: Int): String {
    return toString(16).padStart(length, '0')
}

class Operand(val type: OperandType, val value: Int) {
    override fun toString(): String {
        return when (type) {
            OperandType.Large -> "#" + value.hex(4)
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

sealed class Return(val value: Int) {
    override fun toString(): String {
        return when (this) {
            is ReturnIndirect -> when {
                value == 0 -> " -> (SP)"
                value > 0x10 -> "-> G" + (value - 0x10).hex(2)
                else -> "-> L" + (value - 1).hex(2)
            }

            is ReturnVariable -> when {
                value == 0 -> " -> -(SP)"
                value > 0x10 -> "-> G" + (value - 0x10).hex(2)
                else -> "-> L" + (value - 1).hex(2)
            }
        }
    }
}

class ReturnIndirect(value: Int) : Return(value)
class ReturnVariable(value: Int) : Return(value)

class Instruction(private val memory: Memory, val ip: Int) {
    private var optype = Encoding.Op0
    private var opcode = 0
    var name = ""
    var length = 0
    var ret: Return? = null
    var args: Array<Operand> = arrayOf()
    var jumpOffset: Int? = null
    var compare: Boolean? = null
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
                    args.plusElement(Operand(OperandType.Variable, memory.readU8(ip + length - 1)))
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
                    ret = ReturnVariable(memory.readU8(ip + length))
                    length += 1
                }
            }

            Encoding.Op1 -> {
                if ((opcode in 0x01..0x04) || (opcode == 0x08) || (opcode in 0x0e..0x0f)) {
                    ret = ReturnVariable(memory.readU8(ip + length))
                    length += 1
                }
            }

            Encoding.Var -> {
                if (opcode == 0x0 || opcode == 0x7) {
                    ret = ReturnVariable(memory.readU8(ip + length))
                    length += 1
                }
            }

            else -> {}
        }
    }

    private fun addBranch() {
        if (when (optype) {
                Encoding.Op2 -> (opcode in 1..7) || opcode == 10
                Encoding.Op1 -> (opcode <= 2)
                Encoding.Op0 -> (opcode in arrayOf(5, 6, 0xd, 0xf))
                else -> false
            }
        ) {
            val branch1 = memory.readU8(ip + length)
            var (offset, len) =
                branch1.and(0x80).shl(8).let {
                    if (branch1.and(0x40) != 0) {
                        Pair(it.or(branch1.and(0x3f)), 1)
                    } else {
                        val branch2 = memory.readU8(ip + length + 1)
                        Pair(it.or(branch1.and(0x1f).shl(8)).or(branch2), 2)
                    }
                }
            compare = offset.and(0x8000) != 0
            offset = offset.and(0x7fff)
            if (offset > 0x0fff) {
                offset = -(0x1fff - offset + 1)
            }
            jumpOffset = offset
            length += len
        }
    }

    private fun addPrint() {
        if (optype == Encoding.Op0 && (opcode == 2 || opcode == 3)) {
            string = ZString(memory, ip + length)
            length += string!!.length
        }
    }

    override fun toString(): String {
        val dispArgs = args.joinToString { "$it" }
        val dispRet = ret?.let { "$ret" } ?: ""
        val dispName = name.uppercase()
        val offset = ip.hex(8).uppercase()
        val dispCompare = compare?.let { " [$compare]".uppercase() } ?: ""
        val dispOffset = jumpOffset?.let {
            when (it) {
                0 -> " RFALSE"
                1 -> " RTRUE"
                else -> " " + (ip + length + it - 2).hex(8).uppercase()
            }
        } ?: ""
        val dispString = string?.let { " \"$it\"" } ?: ""
        return "[$offset] $dispName\t$dispArgs$dispRet$dispCompare$dispOffset$dispString"
    }
}

data class Frame(
    val addr: Int,
    val stackStart: Int,
    val numLocals: Int,
    val returnStorage: Return?,
    val returnAddr: Int
)

open class Property(private val memory: Memory, val offset: Int) {
    private val size = memory.readU8(offset)
    val index = size.and(31)
    val length = size.and(0xe0).shr(5) + 1
    var value
        get() = if (length == 1) {
            memory.readU8(offset + 1)
        } else {
            memory.readU16(offset + 1)
        }
        set(v) = if (length == 1) {
            memory.writeU8(offset + 1, v)
        } else {
            memory.writeU16(offset + 1, v)
        }
    val next get() = Property(memory, offset + length + 1)
}

class PropertyDefault(memory: Memory, index: Int) : Property(memory, memory.readU16(0xa) + (index - 1) * 2)

const val NUM_OBJECT_DEFAULTS = 31
const val OBJECT_SIZE = 9
const val OBJECT_DEFAULT_TABLE_SIZE = NUM_OBJECT_DEFAULTS * 2

class Object(private val memory: Memory, val index: Int) {
    private val addr = memory.readU16(0xa) + OBJECT_DEFAULT_TABLE_SIZE + (index - 1) * OBJECT_SIZE
    val offset = memory.readU16(addr + 7)
    var attrib
        get() = memory.readU16(addr).shl(16).or(memory.readU16(addr + 2))
        set(value) {
            memory.writeU16(addr, value.shr(16))
            memory.writeU16(addr + 2, value)
        }
    var parent
        get() = memory.readU8(addr + 4)
        set(value) = memory.writeU8(addr + 4, value)
    var sibling
        get() = memory.readU8(addr + 5)
        set(value) = memory.writeU8(addr + 5, value)
    var child
        get() = memory.readU8(addr + 6)
        set(value) = memory.writeU8(addr + 6, value)
    val name get() = ZString(memory, offset + 1)

    fun getProperty(index: Int): Property {
        var p = Property(memory, offset + 1 + name.length)
        while (true) {
            when (p.index) {
                index -> return p
                0 -> return PropertyDefault(memory, index)
                else -> p = p.next
            }
        }
    }

    fun getNextProperty(index: Int): Int {
        var p = Property(memory, offset + 1 + name.length)
        if (index == 0) {
            return p.index
        }
        while (true) {
            when (p.index) {
                index -> return p.next.index
                0 -> return 0
                else -> p = p.next
            }
        }
    }

    fun remove() {
        if (parent != 0) {
            val p = Object(memory, parent)
            var c = Object(memory, p.child)

            if (c.index == index) {
                p.child = sibling
            } else {
                while (c.sibling != index) {
                    c = Object(memory, c.sibling)
                }
                c.sibling = sibling
            }
        }
        parent = 0
        sibling = 0
    }

    fun insert(other: Int) {
        remove()
        val dest = Object(memory, other)
        sibling = dest.child
        parent = dest.index
        dest.child = index
    }
}

class Machine(private var memory: Memory, private val header: Header) {
    private var finished = false
    private var ip = memory.readU16(0x6)
    private var stack = ArrayList<Int>()
    private var frames = ArrayList<Frame>()
    private var random = Random(0)

    fun run() {
        while (!finished) {
            val instruction = Instruction(memory, ip)
            execute(instruction)
        }
    }


    private fun paddr(offset: Int) = header.dynamicStart + 2 * offset
    private fun addr(offset: Int) = header.dynamicStart + offset
    private fun obj(index: Int) = Object(memory, index)

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

    private fun readDirect(v: Operand) = readVar(v).toUByte().toInt()

    private fun readGlobal(v: Int): Int {
        val offset = header.globals + paddr(v)
        return memory.readU16(offset)
    }

    private fun writeGlobal(v: Int, value: Int) {
        val offset = header.globals + paddr(v)
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
        if (v.value >= 0x10) {
            writeGlobal(v.value - 0x10, value)
        } else if (v.value == 0) {
            when (v) {
                is ReturnIndirect -> stack[stack.lastIndex] = value
                is ReturnVariable -> stack.add(value)
            }
        } else {
            writeLocal(v.value - 1, value)
        }
    }

    private fun call(i: Instruction) {
        val addr = paddr(readVar(i.args[0]))
        val retAddr = ip + i.length
        val args = i.args.slice(1 until i.args.size).map { readVar(it) }
        if (addr - header.dynamicStart == 0) {
            writeVar(i.ret!!, 0)
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
        while (stack.lastIndex >= frame.stackStart) {
            stack.pop()
        }
        writeVar(frame.returnStorage!!, value)
        ip = frame.returnAddr
    }

    private fun jump(i: Instruction, compare: Boolean) {
        if (compare == i.compare) {
            when (val offset = i.jumpOffset!!) {
                0 -> ret(0)
                1 -> ret(1)
                else -> {
                    ip = i.ip + i.length + offset - 2
                }
            }
        }
    }

    private fun <T> Instruction.r(o: Operand? = null, transform: (List<Int>) -> T): Pair<Instruction, T> {
        return Pair(this, transform(o?.let { listOf(readVar(it)) } ?: args.map { readVar(it) }))
    }

    private fun Pair<Instruction, Int>.w(r: Return? = null) {
        val (i, arg) = this
        writeVar(r ?: i.ret!!, arg)
    }

    private fun <T> Pair<Instruction, T>.w(transform: (T) -> Pair<Return, Int>) {
        val (_, arg) = this
        val (ret, v) = transform(arg)
        writeVar(ret, v)
    }


    private fun Instruction.direct(transform: (Int) -> Int) {
        val (x) = args.map { readDirect(it) }
        val value = readVar(Operand(OperandType.Variable, x))
        writeVar(ReturnVariable(x), transform(value))
    }

    private fun execute(i: Instruction) {
        val startIP = ip
        //println("$i (stack: $stack)")
        when (i.name) {
            "call" -> call(i)
            "print" -> i.string?.let { print(it) }
            "rtrue" -> ret(1)
            "rfalse" -> ret(0)
            "store" -> i.r { x -> x }.w { (x, y) -> Pair(ReturnIndirect(x), y) }
            "print_paddr" -> i.r { (x) -> print(ZString(memory, paddr(x))) }
            "inc" -> i.direct { x -> (x + 1).mod(0x10000) }
            "dec" -> i.direct { x -> (x - 1).mod(0x10000) }
            "jz" -> i.r { (x) -> jump(i, x == 0) }
            "add" -> i.r { (x, y) -> (x + y).mod(0x10000) }.w()
            "sub" -> i.r { (x, y) -> (x - y).mod(0x10000) }.w()
            "mul" -> i.r { (x, y) -> (x * y).mod(0x10000) }.w()
            "div" -> i.r { (x, y) -> (x.toShort() / y.toShort()).toUShort().toInt() }.w()
            "mod" -> i.r { (x, y) -> (x.toShort() % y.toShort()).toUShort().toInt() }.w()
            "not" -> i.r { (x) -> x.inv().and(0xffff) }.w()
            "and" -> i.r { (x, y) -> x.and(y) }.w()
            "or" -> i.r { (x, y) -> x.or(y) }.w()
            "loadw" -> i.r { (x, y) -> memory.readU16(paddr(y) + x) }.w()
            "loadb" -> i.r { (x, y) -> memory.readU8(addr(x + y)) }.w()
            "storew" -> i.r { (x, y, z) -> memory.writeU16(paddr(y) + x, z) }
            "storeb" -> i.r { (x, y, z) -> memory.writeU8(addr(x + y), z) }
            "print_num" -> i.r { (x) -> print(x.toShort()) }
            "jump" -> i.r { (x) -> ip += i.length + x.toShort().toInt() - 2 }
            "je" -> i.r { x -> jump(i, x.drop(1).any { y -> x[0] == y }) }
            "jg" -> i.r { (x, y) -> jump(i, x.toShort() > y.toShort()) }
            "jl" -> i.r { (x, y) -> jump(i, x.toShort() < y.toShort()) }
            "push" -> i.r { (x) -> stack.add(x) }
            "pop" -> stack.pop()
            "pull" -> i.r { (x) -> listOf(x, stack.pop()) }.w { (x, y) -> Pair(ReturnIndirect(x), y) }
            "load" -> i.r { (x) -> readVar(Operand(OperandType.Indirect, x)) }.w()
            "ret" -> i.r { (x) -> ret(x) }
            "ret_popped" -> ret(stack.pop())
            "get_parent" -> i.r { (x) -> obj(x).parent }.w()
            "get_sibling" -> i.r { (x) -> obj(x).sibling }.also { (_, x) -> jump(i, x != 0) }.w()
            "get_child" -> i.r { (x) -> obj(x).child }.also { (_, x) -> jump(i, x != 0) }.w()
            "get_next_prop" -> i.r { (x, y) -> obj(x).getNextProperty(y) }.w()
            "get_prop_addr" -> i.r { (x, y) -> obj(x).getProperty(y).offset + 1 }.w()
            "get_prop_len" -> i.r { (x) -> Property(memory, x - 1).length }.w()
            "get_prop" -> i.r { (x, y) -> obj(x).getProperty(y).value }.w()
            "put_prop" -> i.r { (x, y, z) -> obj(x).getProperty(y).value = z }
            "remove_obj" -> i.r { (x) -> obj(x).remove() }
            "insert_obj" -> i.r { (x, y) -> obj(x).insert(y) }
            "jin" -> i.r { (x, y) -> jump(i, obj(x).parent == y) }
            "test_attr" -> i.r { (x, y) -> jump(i, obj(x).attrib.and(1.shl(31 - y)) != 0) }
            "set_attr" -> i.r { (x, y) -> obj(x).attrib = obj(x).attrib or 1.shl(31 - y) }
            "clear_attr" -> i.r { (x, y) -> obj(x).attrib = obj(x).attrib and 1.shl(31 - y).inv() }
            "test" -> i.r { (x, y) -> jump(i, x.and(y) == y) }
            "verify" -> jump(i, true)
            "print_char" -> i.r { (x) -> print(x.toChar()) }
            "new_line" -> println()
            "print_ret" -> i.string?.let { println(it) }.also { ret(1) }
            "print_addr" -> i.r { (x) -> print(ZString(memory, addr(x))) }
            "print_obj" -> i.r { (x) -> print(obj(x).name) }
            "quit" -> finished = true
            "random" -> i.r { (x) ->
                if (x.toShort() < 0) {
                    random = Random(x.toLong())
                    0
                } else {
                    random.nextInt().toUShort().toInt() + 1
                }
            }.w()

            "dec_chk" -> i.direct { x ->
                (x - 1).mod(0x10000).also { jump(i, it.toShort() < readVar(i.args[1]).toShort()) }
            }

            "inc_chk" -> i.direct { x ->
                (x + 1).mod(0x10000).also { jump(i, it.toShort() > readVar(i.args[1]).toShort()) }
            }


            else -> {
                println("\n---------------\nUnknown instruction:\n$i")
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
    val memory = Memory(readAllBytes(args.firstOrNull() ?: "zork.z3"))
    val header = Header(memory)
    val machine = Machine(memory, header)
    machine.run()
}