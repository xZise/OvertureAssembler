namespace OvertureAssembler
{
    internal class Program
    {

        static void Main(string[] args)
        {
            if (args.Length == 0 || (args.Length == 1 && (args[0] == "--help" || args[0] == "-h")))
            {
                Assembler.WriteHelp();
                return;
            }

            if (args.Length == 1 && args[0] == "--inst")
            {
                Assembler.WriteInstructions();
                return;
            }

            if (args.Length > 1)
            {
                Console.Error.WriteLine("Supports only one file at the moment");
                return;
            }

            string[] lines = File.ReadAllLines(args[0]);

            Assembler assembler = new Assembler();
            byte[] byteCode = assembler.Assemble(lines);

            for (int i = 0; i < byteCode.Length; i++)
            {
                byte code = byteCode[i];
                Console.WriteLine($"{i:x3}: {code:b8}");
            }
        }
    }

    public class Assembler
    {
        private readonly Dictionary<string, Label> labels = new();

        public static void WriteHelp()
        {
            Console.WriteLine("Compilies the instructions of the file to OVERTURE architecture.");
            Console.WriteLine("  --help - prints this help");
            Console.WriteLine("  --inst - prints a list of instructions");
        }

        public static void WriteInstructions()
        {
            Console.WriteLine($"Data operations:");
            Console.WriteLine($"  mov <dest> <src> - copies the value from register 'src' to 'dest'. Can be any register r0 to r5 and in (src only) or out (dest only).");
            Console.WriteLine($"  li <imm>         - loads the immediate value ('imm') into register 0. The immediate must be in range 0 to {MaxImmediate}.");
            Console.WriteLine($"Arithmetic operations");
            Console.WriteLine($"  <or>   - copies the result of 'bitwise or' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  <nor>  - copies the result of 'bitwise nor' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  <and>  - copies the result of 'bitwise and' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  <nand> - copies the result of 'bitwise nand' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  <add>  - copies the sum of 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  <sub>  - copies the difference between 'r1' and 'r2' into 'r3'");
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
            Console.WriteLine($"- Immediates are encoded in decimal. In hexadecimal when it is either prefixed by '0x' or suffixed by 'h'.");
        }

        public record struct Reference(byte Offset, int LineNumber);

        public class Label(string name)
        {
            private Reference? offset;

            public string Name { get; } = name;
            public Reference? Offset
            {
                get => offset;
                set
                {
                    if (offset.HasValue)
                    {
                        throw new InvalidOperationException($"Multiple declarations of label '{Name}' already in line {offset.Value.LineNumber}");
                    }
                    offset = value;
                }
            }
            public List<Reference> References { get; } = [];
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
            bool asHex;
            if (token.StartsWith("0x", StringComparison.InvariantCultureIgnoreCase))
            {
                token = token[2..];
                asHex = true;
            }
            else if (token[^1] == 'h' || token[^1] == 'H')
            {
                token = token[..^1];
                asHex = true;
            }
            else
            {
                asHex = false;
            }
            if (asHex)
            {
                return byte.TryParse(token, System.Globalization.NumberStyles.HexNumber, null, out result);
            }
            return byte.TryParse(token, out result);
        }

        public byte[] Assemble(string[] lines)
        {
            labels.Clear();

            List<byte> byteCode = [];
            bool validProgram = true;

            for (int lineNumber = 1; lineNumber <= lines.Length; lineNumber++)
            {
                string line = lines[lineNumber - 1];

                OpCode opcode = OpCode.Immediate;
                byte data = 0;
                bool validLine = true;
                try
                {
                    int offset = 0;
                    ReadOnlySpan<char> token = NextToken(line, ref offset);

                    if (token.Length == 0)
                    {
                        continue;
                    }

                    for (int i = offset; i < line.Length; i++)
                    {
                        if (line[i] == ':')
                        {
                            Label label = GetLabel(token.ToString());
                            label.Offset = new((byte)byteCode.Count, lineNumber);
                            offset = i + 1;
                            token = NextToken(line, ref offset);
                            break;
                        }
                        else if (!char.IsWhiteSpace(line[i]))
                        {
                            break;
                        }
                    }

                    if (token.Length == 0)
                    {
                        continue;
                    }

                    if (MemoryExtensions.Equals(token, "li", StringComparison.InvariantCulture))
                    {
                        opcode = OpCode.Immediate;

                        ReadOnlySpan<char> value = NextToken(line, ref offset);
                        if (!TryParse(value, out byte immediateValue) || immediateValue > MaxImmediate)
                        {
                            throw new InvalidOperationException($"Immediate value must be a number in range 0 - {MaxImmediate}");
                        }

                        data = immediateValue;
                    }
                    else if (MemoryExtensions.Equals(token, "mov", StringComparison.InvariantCulture))
                    {
                        ReadOnlySpan<char> destination = NextToken(line, ref offset);
                        ReadOnlySpan<char> source = NextToken(line, ref offset);

                        opcode = OpCode.Move;

                        byte destinationRegister = ParseRegisterName(destination, OutputRegisterName);
                        byte sourceRegister = ParseRegisterName(source, InputRegisterName);

                        data = (byte)((sourceRegister << 3) | destinationRegister);
                    }
                    else if (token.Length >= 1 && token[0] == 'j')
                    {
                        opcode = OpCode.Conditional;
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
                        data = (byte)condition;

                        ReadOnlySpan<char> labelName = NextToken(line, ref offset);
                        if (labelName.Length > 0)
                        {
                            Label label = GetLabel(labelName.ToString());
                            label.References.Add(new((byte)byteCode.Count, lineNumber));
                            byteCode.Add(0xFF);
                        }
                    }
                    else if (MemoryExtensions.Equals(token, "and", StringComparison.InvariantCulture))
                    {
                        data = (byte)Arithmetic.And;
                    }
                    else if (MemoryExtensions.Equals(token, "nand", StringComparison.InvariantCulture))
                    {
                        data = (byte)Arithmetic.Nand;
                    }
                    else if (MemoryExtensions.Equals(token, "or", StringComparison.InvariantCulture))
                    {
                        data = (byte)Arithmetic.Or;
                    }
                    else if (MemoryExtensions.Equals(token, "nor", StringComparison.InvariantCulture))
                    {
                        data = (byte)Arithmetic.Nor;
                    }
                    else if (MemoryExtensions.Equals(token, "add", StringComparison.InvariantCulture))
                    {
                        data = (byte)Arithmetic.Add;
                    }
                    else if (MemoryExtensions.Equals(token, "sub", StringComparison.InvariantCulture))
                    {
                        data = (byte)Arithmetic.Sub;
                    }
                    else
                    {
                        throw new InvalidOperationException($"Unknown token '{token}'");
                    }
                }
                catch (InvalidOperationException ex)
                {
                    Console.WriteLine($"Assembly error in line #{lineNumber}: {ex.Message}");
                    validLine = false;
                }

                if (validProgram)
                {
                    if (validLine)
                    {
                        data |= (byte)((byte)opcode << 6);

                        byteCode.Add(data);
                    }
                    else
                    {
                        validProgram = false;
                        byteCode.Clear();
                    }
                }
            }

            foreach (Label label in labels.Values)
            {
                if (!label.Offset.HasValue)
                {
                    Console.WriteLine($"Unknown label '{label.Name}' referenced in line {string.Join(", ", label.References.Select(r => r.LineNumber))}");
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
                            throw new InvalidOperationException($"Location of label '{label.Name}' is outside of the maximum immediate possible.");
                        }
                        byteCode[reference.Offset] = absoluteOffset;
                    }
                }
            }

            return byteCode.ToArray();
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

        private static byte ParseRegisterName(ReadOnlySpan<char> registerName, string inOutRegisterName)
        {
            if (MemoryExtensions.Equals(registerName, "r0", StringComparison.InvariantCulture)) return 0b000;
            else if (MemoryExtensions.Equals(registerName, "r1", StringComparison.InvariantCulture)) return 0b001;
            else if (MemoryExtensions.Equals(registerName, "r2", StringComparison.InvariantCulture)) return 0b010;
            else if (MemoryExtensions.Equals(registerName, "r3", StringComparison.InvariantCulture)) return 0b011;
            else if (MemoryExtensions.Equals(registerName, "r4", StringComparison.InvariantCulture)) return 0b100;
            else if (MemoryExtensions.Equals(registerName, "r5", StringComparison.InvariantCulture)) return 0b101;
            else if (MemoryExtensions.Equals(registerName, inOutRegisterName, StringComparison.InvariantCulture)) return 0b110;

            throw new InvalidOperationException($"Unknown register name '{registerName}'");
        }

        static ReadOnlySpan<char> NextToken(string keyword, ref int offset)
        {
            while (offset < keyword.Length && char.IsWhiteSpace(keyword[offset]))
            {
                offset++;
            }
            int startIndex = offset;

            while (offset < keyword.Length && !(char.IsWhiteSpace(keyword[offset]) || keyword[offset] == ':'))
            {
                offset++;
            }
            int endIndex = offset;
            return keyword.AsSpan(startIndex, endIndex - startIndex);
        }
    }
}
