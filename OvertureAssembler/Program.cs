using System.CommandLine;
using System.ComponentModel.Design;

namespace OvertureAssembler
{
    internal class Program
    {
        static async Task Main(string[] args)
        {
            RootCommand rootCommand = new("An assembler for the OVERTURE architecture in 'Turing Complete'");

            Argument<FileInfo> file = new("file", "Location of the assembly file");
            Option<bool> binaryOutput = new("--binary", "Outputs the result as a list of binary values");
            Option<FileInfo?> outputFile = new("--output", "Outputs the result into the file as lines of ASCII decimal numbers");

            rootCommand.Add(file);
            rootCommand.Add(binaryOutput);
            rootCommand.Add(outputFile);

            Command inst = new("--inst", "Prints a list of instructions");
            inst.SetHandler(Assembler.WriteInstructions);
            rootCommand.Add(inst);

            rootCommand.SetHandler((fileValue, binaryOutputValue, outputFileValue) => {
                string[] lines = File.ReadAllLines(fileValue.FullName);

                Assembler assembler = new();
                byte[] byteCode = assembler.Assemble(lines);

                for (int i = 0; i < byteCode.Length; i++)
                {
                    byte code = byteCode[i];
                    if (binaryOutputValue)
                    {
                        Console.WriteLine($"{i:x3}: {code:b8}");
                    }
                    else
                    {
                        Console.WriteLine(code);
                    }
                }

                if (outputFileValue != null)
                {
                    using FileStream s = outputFileValue.Open(FileMode.Create);
                    using StreamWriter sw = new(s);

                    foreach (byte code in byteCode)
                    {
                        sw.WriteLine(code);
                    }
                }
            }, file, binaryOutput, outputFile);

            await rootCommand.InvokeAsync(args);
        }
    }

    public enum Register : byte
    {
        r0 = 0b000,
        r1 = 0b001,
        r2 = 0b010,
        r3 = 0b011,
        r4 = 0b100,
        r5 = 0b101,
        inOut = 0b110,
    }

    public class Assembler
    {
        private readonly Dictionary<string, Label> labels = new();

        public static void WriteInstructions()
        {
            Console.WriteLine($"Data operations:");
            Console.WriteLine($"  mov <dest> <src> - copies the value from register 'src' to 'dest'. Can be any register r0 to r5 and in (src only) or out (dest only).");
            Console.WriteLine($"  li <imm>         - loads the immediate value ('imm') into register 0. The immediate must be in range 0 to {MaxImmediate}.");
            Console.WriteLine($"Arithmetic operations");
            Console.WriteLine($"  or   - copies the result of 'bitwise or' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  nor  - copies the result of 'bitwise nor' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  and  - copies the result of 'bitwise and' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  nand - copies the result of 'bitwise nand' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  add  - copies the sum of 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  sub  - copies the difference between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"Conditional operations");
            Console.WriteLine($"  j [lbl]    - Makes an unconditional jump to the instruction address of 'r0' or 'lbl'");
            Console.WriteLine($"  je [lbl]   - When 'r3' is equal to 0, jumps to the instruction address of 'r0' or 'lbl'");
            Console.WriteLine($"  jne [lbl]  - When 'r3' is not equal to 0, jumps to the instruction address of 'r0' or 'lbl'");
            Console.WriteLine($"  jgt [lbl]  - When 'r3' is greater than 0, jumps to the instruction address of 'r0' or 'lbl'");
            Console.WriteLine($"  jgte [lbl] - When 'r3' is greater than or equal to 0, jumps to the instruction address of 'r0' or 'lbl'");
            Console.WriteLine($"  jlt [lbl]  - When 'r3' is less than 0, jumps to the instruction address of 'r0' or 'lbl'");
            Console.WriteLine($"  jlte [lbl] - When 'r3' is less than or equal to 0, jumps to the instruction address of 'r0' or 'lbl'");
            Console.WriteLine($"  Note: When 'lbl' is present, it'll insert an immediate load to the label and overwrite 'r0'!");
            Console.WriteLine();
            Console.WriteLine($"- Labels are indicated by a name and colon (:)");
            Console.WriteLine($"- Immediates are by default encoded in decimal. In hexadecimal when it is either prefixed by '0x' or suffixed by 'h'. In binary when it is prefixed by '0b'.");
            Console.WriteLine($"- Everything after '#' is ignored.");
        }

        public record struct Reference(byte Offset, int LineNumber);

        public class Label(string name)
        {
            public string Name { get; } = name;
            public Reference? Offset { get; private set; }
            public List<Reference> References { get; } = [];

            public void SetOffset(Reference offset)
            {
                if (Offset.HasValue)
                {
                    throw new InvalidOperationException($"Multiple declarations of label '{Name}' already in line {Offset.Value.LineNumber}");
                }
                Offset = offset;
            }
        }

        private ref struct ByteCode
        {
            public readonly Span<byte> Bytes;
            public bool Failed { readonly get; private set; }
            public readonly byte Cursor => Failed ? (byte)0 : cursor;

            private byte cursor;

            public ByteCode(Span<byte> bytes)
            {
                Bytes = bytes;
                Failed = false;
                cursor = 0;
            }

            private void AddInstruction(OpCode opcode, byte data)
            {
                if (Failed) return;

                if ((data & ~MaxImmediate) > 0)
                {
                    throw new InvalidOperationException("Data clobbers the opcode portion");
                }
                byte inst = (byte)(data | (byte)opcode << 6);
                Add(inst);
            }

            public void Add(byte value) => Bytes[cursor++] = value;

            public void AddInstruction(byte immediate)
            {
                if (immediate > MaxImmediate)
                {
                    throw new InvalidOperationException("Immediate value outside range");
                }
                AddInstruction(OpCode.Immediate, immediate);
            }

            public void AddInstruction(Arithmetic arithmetic)
            {
                AddInstruction(OpCode.Calculate, (byte)arithmetic);
            }

            public void AddInstruction(Condition condition)
            {
                AddInstruction(OpCode.Conditional, (byte)condition);
            }

            public void AddInstruction(Register destinationRegister, Register sourceRegister)
            {
                if (!Enum.IsDefined(destinationRegister) || !Enum.IsDefined(sourceRegister))
                {
                    throw new InvalidOperationException("Undefined register index");
                }
                AddInstruction(OpCode.Move, (byte)(((byte)sourceRegister << 3) | (byte)destinationRegister));
            }

            public void Fail() => Failed = true;
        }

        private Label GetLabel(string name)
        {
            if (!labels.TryGetValue(name, out Label? labelObj))
            {
                labelObj = new(name);
                labels[name] = labelObj;
            }
            return labelObj;
        }

        private static bool TryParse(ReadOnlySpan<char> token, out byte result)
        {
            System.Globalization.NumberStyles numberStyle;
            if (token.StartsWith("0x", StringComparison.InvariantCultureIgnoreCase))
            {
                token = token[2..];
                numberStyle = System.Globalization.NumberStyles.HexNumber;
            }
            else if (token[^1] == 'h' || token[^1] == 'H')
            {
                token = token[..^1];
                numberStyle = System.Globalization.NumberStyles.HexNumber;
            }
            else if (token.StartsWith("0b", StringComparison.InvariantCultureIgnoreCase))
            {
                token = token[2..];
                numberStyle = System.Globalization.NumberStyles.BinaryNumber;
            }
            else
            {
                numberStyle = System.Globalization.NumberStyles.Integer;
            }
            return byte.TryParse(token, numberStyle, null, out result);
        }

        public byte[] Assemble(string[] lines)
        {
            labels.Clear();

            Span<byte> bytes = stackalloc byte[256];
            ByteCode byteCode = new(bytes);

            for (int lineNumber = 1; lineNumber <= lines.Length; lineNumber++)
            {
                Tokenizer tokenizer = new(lines[lineNumber - 1]);

                try
                {
                    ReadOnlySpan<char> token = tokenizer.NextToken();

                    if (token.Length == 0)
                    {
                        continue;
                    }

                    while (!tokenizer.AtEnd)
                    {
                        if (tokenizer.Current == Tokenizer.LabelChar)
                        {
                            Label label = GetLabel(token.ToString());
                            label.SetOffset(new(byteCode.Cursor, lineNumber));
                            tokenizer.NextChar();
                            token = tokenizer.NextToken();
                            break;
                        }
                        else if (!char.IsWhiteSpace(tokenizer.Current))
                        {
                            break;
                        }
                        tokenizer.NextChar();
                    }

                    if (token.Length == 0)
                    {
                        continue;
                    }

                    if (MemoryExtensions.Equals(token, "li", StringComparison.InvariantCulture))
                    {
                        ReadOnlySpan<char> value = tokenizer.NextToken();
                        if (!TryParse(value, out byte immediateValue) || immediateValue > MaxImmediate)
                        {
                            throw new InvalidOperationException($"Immediate value must be a number in range 0 - {MaxImmediate}");
                        }

                        byteCode.AddInstruction(immediateValue);
                    }
                    else if (MemoryExtensions.Equals(token, "mov", StringComparison.InvariantCulture))
                    {
                        ReadOnlySpan<char> destination = tokenizer.NextToken();
                        ReadOnlySpan<char> source = tokenizer.NextToken();

                        Register destinationRegister = ParseRegisterName(destination, OutputRegisterName);
                        Register sourceRegister = ParseRegisterName(source, InputRegisterName);

                        byteCode.AddInstruction(destinationRegister, sourceRegister);
                    }
                    else if (token.Length >= 1 && token[0] == 'j')
                    {
                        Condition condition;
                        if (MemoryExtensions.Equals(token, "j", StringComparison.InvariantCulture))
                        {
                            condition = Condition.Unconditional;
                        }
                        else if (MemoryExtensions.Equals(token, "je", StringComparison.InvariantCulture))
                        {
                            condition = Condition.Equal;
                        }
                        else if (MemoryExtensions.Equals(token, "jne", StringComparison.InvariantCulture))
                        {
                            condition = Condition.NotEqual;
                        }
                        else if (MemoryExtensions.Equals(token, "jgt", StringComparison.InvariantCulture))
                        {
                            condition = Condition.GreaterThan;
                        }
                        else if (MemoryExtensions.Equals(token, "jgte", StringComparison.InvariantCulture))
                        {
                            condition = Condition.GreaterThanEqual;
                        }
                        else if (MemoryExtensions.Equals(token, "jlt", StringComparison.InvariantCulture))
                        {
                            condition = Condition.LessThan;
                        }
                        else if (MemoryExtensions.Equals(token, "jlte", StringComparison.InvariantCulture))
                        {
                            condition = Condition.LessThanEqual;
                        }
                        else
                        {
                            throw new InvalidOperationException($"Unknown token '{token}'");
                        }

                        ReadOnlySpan<char> labelName = tokenizer.NextToken();
                        if (labelName.Length > 0)
                        {
                            Label label = GetLabel(labelName.ToString());
                            byte offset = 0;
                            if (!byteCode.Failed)
                            {
                                offset = byteCode.Cursor;
                                byteCode.Add(0xFF);
                            }
                            label.References.Add(new(offset, lineNumber));
                        }

                        byteCode.AddInstruction(condition);
                    }
                    else if (MemoryExtensions.Equals(token, "and", StringComparison.InvariantCulture))
                    {
                        byteCode.AddInstruction(Arithmetic.And);
                    }
                    else if (MemoryExtensions.Equals(token, "nand", StringComparison.InvariantCulture))
                    {
                        byteCode.AddInstruction(Arithmetic.Nand);
                    }
                    else if (MemoryExtensions.Equals(token, "or", StringComparison.InvariantCulture))
                    {
                        byteCode.AddInstruction(Arithmetic.Or);
                    }
                    else if (MemoryExtensions.Equals(token, "nor", StringComparison.InvariantCulture))
                    {
                        byteCode.AddInstruction(Arithmetic.Nor);
                    }
                    else if (MemoryExtensions.Equals(token, "add", StringComparison.InvariantCulture))
                    {
                        byteCode.AddInstruction(Arithmetic.Add);
                    }
                    else if (MemoryExtensions.Equals(token, "sub", StringComparison.InvariantCulture))
                    {
                        byteCode.AddInstruction(Arithmetic.Sub);
                    }
                    else
                    {
                        throw new InvalidOperationException($"Unknown token '{token}'");
                    }
                }
                catch (InvalidOperationException ex)
                {
                    Console.WriteLine($"Assembly error in line #{lineNumber}: {ex.Message}");

                    // As soon as one error occurred, we still try to assemble the following instructions, but do not
                    // output any result (only further warnings or errors).
                    byteCode.Fail();

                    continue;
                }
            }

            foreach (Label label in labels.Values)
            {
                if (!label.Offset.HasValue)
                {
                    Console.WriteLine($"Unknown label '{label.Name}' referenced in line {string.Join(", ", label.References.Select(r => r.LineNumber))}");
                    byteCode.Fail();
                }
                else if (label.References.Count == 0)
                {
                    Console.WriteLine($"Unreferenced label '{label.Name}' in line {label.Offset.Value.LineNumber}");
                }
                else
                {
                    byte absoluteOffset = label.Offset.Value.Offset;
                    foreach (Reference reference in label.References)
                    {
                        if (absoluteOffset > Assembler.MaxImmediate)
                        {
                            byteCode.Fail();
                            throw new InvalidOperationException($"Location of label '{label.Name}' is outside of the maximum immediate possible.");
                        }

                        if (!byteCode.Failed)
                        {
                            byteCode.Bytes[reference.Offset] = absoluteOffset;
                        }
                    }
                }
            }

            if (byteCode.Failed)
            {
                return [];
            }

            return byteCode.Bytes[..byteCode.Cursor].ToArray();
        }

        public enum OpCode : byte
        {
            Immediate = 0b00,
            Move = 0b10,
            Calculate = 0b01,
            Conditional = 0b11,
        }

        public enum Condition : byte
        {
            Never = 0b00,
            Unconditional = 0b100,
            Equal = 0b01,
            NotEqual = Unconditional | Equal,
            LessThan = 0b10,
            GreaterThanEqual = Unconditional | LessThan,
            LessThanEqual = LessThan | Equal,
            GreaterThan = Unconditional | LessThanEqual,
        }

        public enum Arithmetic : byte
        {
            Or = 0b000,
            Nor = 0b010,
            And = 0b011,
            Nand = 0b001,
            Add = 0b100,
            Sub = 0b101,
        }

        public const byte MaxImmediate = 0b11_1111;

        private const string InputRegisterName = "in";
        private const string OutputRegisterName = "out";

        private static Register ParseRegisterName(ReadOnlySpan<char> registerName, string inOutRegisterName)
        {
            if (MemoryExtensions.Equals(registerName, "r0", StringComparison.InvariantCulture)) return Register.r0;
            else if (MemoryExtensions.Equals(registerName, "r1", StringComparison.InvariantCulture)) return Register.r1;
            else if (MemoryExtensions.Equals(registerName, "r2", StringComparison.InvariantCulture)) return Register.r2;
            else if (MemoryExtensions.Equals(registerName, "r3", StringComparison.InvariantCulture)) return Register.r3;
            else if (MemoryExtensions.Equals(registerName, "r4", StringComparison.InvariantCulture)) return Register.r4;
            else if (MemoryExtensions.Equals(registerName, "r5", StringComparison.InvariantCulture)) return Register.r5;
            else if (MemoryExtensions.Equals(registerName, inOutRegisterName, StringComparison.InvariantCulture)) return Register.inOut;

            throw new InvalidOperationException($"Unknown register name '{registerName}'");
        }

        private ref struct Tokenizer
        {
            public const char CommentChar = '#';
            public const char LabelChar = ':';

            public string Line;
            private int offset;

            public readonly char Current => Line[offset];

            public readonly bool AtEnd => offset >= Line.Length;

            public Tokenizer(string line) => Line = line;

            private void CheckComment()
            {
                if (!AtEnd && Current == CommentChar)
                {
                    offset = Line.Length;
                }
            }

            public bool NextChar()
            {
                if (!AtEnd)
                {
                    offset++;
                    return true;
                }
                return false;
            }

            public ReadOnlySpan<char> NextToken()
            {
                if (AtEnd)
                {
                    return [];
                }

                while (!AtEnd && char.IsWhiteSpace(Current))
                {
                    offset++;
                }

                CheckComment();

                int startIndex = offset;
                while (!AtEnd && !(char.IsWhiteSpace(Current) || Current == LabelChar || Current == CommentChar))
                {
                    offset++;
                }
                int endIndex = offset;

                CheckComment();

                return Line.AsSpan(startIndex, endIndex - startIndex);
            }
        }
    }
}
