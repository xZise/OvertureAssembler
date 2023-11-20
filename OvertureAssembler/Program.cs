using System.CommandLine;

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

            Option<bool> enableXorOpcodes = new("--enable-xor", "Enables xor and xnor opcodes.");
            Option<bool> enableRelativeJumps = new("--enable-relative-jumps", "Enables relative jumps");

            rootCommand.Add(file);
            rootCommand.Add(binaryOutput);
            rootCommand.Add(outputFile);

            Command inst = new("--inst", "Prints a list of instructions");
            inst.SetHandler(Assembler.WriteInstructions);
            rootCommand.Add(inst);

            rootCommand.SetHandler((fileValue, binaryOutputValue, outputFileValue, enableXorOpcodesValue, enableRelativeJumpsValue) => {
                string[] lines = File.ReadAllLines(fileValue.FullName);

                Assembler assembler = new()
                {
                    EnableXorOpcodes = enableXorOpcodesValue,
                    EnableRelativeJumps = enableRelativeJumpsValue,
                };
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
		
                assembler.WriteMessages();

                if (outputFileValue != null)
                {
                    using FileStream s = outputFileValue.Open(FileMode.Create);
                    using StreamWriter sw = new(s);

                    foreach (byte code in byteCode)
                    {
                        sw.WriteLine(code);
                    }
                }
            }, file, binaryOutput, outputFile, enableXorOpcodes, enableRelativeJumps);

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
        private readonly List<AssemblyMessage> messages = new();

        public IReadOnlyList<AssemblyMessage> AssemblyMessages => messages.AsReadOnly();

        public bool EnableXorOpcodes { get; set; }
        public bool EnableRelativeJumps { get; set; }

        public void WriteMessages()
        {
            foreach (AssemblyMessage message in messages.OrderByDescending(m => m.IsError).ThenBy(m => m.Location.LineNumber))
            {
                Console.WriteLine(message);
            }
        }

        public bool Failed => messages.Any(m => m.IsError);

        public static void WriteInstructions()
        {
            Console.WriteLine($"Data operations:");
            Console.WriteLine($"  mov <dest> <src> - copies the value from register 'src' to 'dest'. Can be any register r0 to r5 and in (src only) or out (dest only).");
            Console.WriteLine($"                     (src and dest must be different when they aren't in and out)");
            Console.WriteLine($"  li <imm>         - loads the immediate value ('imm') into register 0. The immediate must be in range 0 to {MaxImmediate}.");
            Console.WriteLine($"  lsi <signed imm> - loads the signed immediate value ('signed imm') into register 0. The immediate must be in range {MinSignedImmediate} to {MaxSignedImmediate}");
            Console.WriteLine();
            Console.WriteLine($"Arithmetic operations");
            Console.WriteLine($"  or   - copies the result of 'bitwise or' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  nor  - copies the result of 'bitwise nor' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  and  - copies the result of 'bitwise and' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  nand - copies the result of 'bitwise nand' between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  add  - copies the sum of 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  sub  - copies the difference between 'r1' and 'r2' into 'r3'");
            Console.WriteLine($"  xor  - copies the result of 'bitwise xor' between 'r1' and 'r2' into 'r3' (must be enabled)");
            Console.WriteLine($"  xnor - copies the result of 'bitwise xnor' between 'r1' and 'r2' into 'r3' (must be enabled)");
            Console.WriteLine();
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
            Console.WriteLine($"- Immediates are by default encoded in decimal. Unsigned immediates can also be encoded in hexadecimal by either prefixing with '0x' or suffixing with 'h'. They can also be encoded in binary by prefixing with '0b'.");
            Console.WriteLine($"- Everything after '#' is ignored.");
        }

        public readonly record struct FileLocation(int LineNumber, int Column)
        {
            public readonly string FormatLineColumn => $"{LineNumber}:{Column}";
        }

        public readonly record struct Reference(byte Offset, FileLocation Location);

        public readonly ref struct Token
        {
            public readonly ReadOnlySpan<char> Span;
            public readonly int Column;

            public readonly bool Exists => Span.Length > 0;

            public Token(ReadOnlySpan<char> span, int column)
            {
                Span = span;
                Column = column;
            }

            public bool Equals(string name) => MemoryExtensions.Equals(Span, name, StringComparison.InvariantCulture);

            public AssemblyException CreateException(string message) => new(message, Column);
        }

        public class Label(string name)
        {
            public string Name { get; } = name;
            public Reference? Offset { get; private set; }
            public List<Reference> References { get; } = [];

            public void SetOffset(Reference offset)
            {
                if (Offset.HasValue)
                {
                    throw new AssemblyException($"Multiple declarations of label '{Name}' already in {Offset.Value.Location.FormatLineColumn}", offset.Location.Column);
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

        public class AssemblyException : Exception
        {
            public int Column { get; }

            public AssemblyException(string? message, int column) : base(message)
            {
                Column = column;
            }

            public static AssemblyException UnknownToken(Token token)
            {
                return token.CreateException($"Unknown token '{token.Span}'");
            }
        }

        public sealed record AssemblyMessage(FileLocation Location, bool IsError, string Message)
        {
            public override string ToString()
            {
                return $"{(IsError ? "Error" : "Warning")} - {Location.FormatLineColumn} - {Message}";
            }
        }

        private void AddWarning(FileLocation location, string Message)
        {
            AssemblyMessage message = new(location, false, Message);
            messages.Add(message);
        }

        private void AddError(FileLocation location, string Message)
        {
            AssemblyMessage message = new(location, true, Message);
            messages.Add(message);
        }

        public byte[] Assemble(string[] lines)
        {
            labels.Clear();
            messages.Clear();

            Span<byte> bytes = stackalloc byte[256];
            ByteCode byteCode = new(bytes);

            for (int lineNumber = 1; lineNumber <= lines.Length; lineNumber++)
            {
                Tokenizer tokenizer = new(lines[lineNumber - 1]);

                try
                {
                    Token token = tokenizer.NextToken();

                    if (!token.Exists)
                    {
                        continue;
                    }

                    while (!tokenizer.AtEnd)
                    {
                        if (tokenizer.Current == Tokenizer.LabelChar)
                        {
                            Label label = GetLabel(token.Span.ToString());
                            label.SetOffset(new(byteCode.Cursor, new(lineNumber, token.Column)));
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

                    if (!token.Exists)
                    {
                        continue;
                    }

                    if (token.Equals("li"))
                    {
                        Token value = tokenizer.NextToken();
                        if (!TryParse(value.Span, out byte immediateValue) || immediateValue > MaxImmediate)
                        {
                            throw value.CreateException($"Immediate value must be a number in range 0 - {MaxImmediate}");
                        }

                        byteCode.AddInstruction(immediateValue);
                    }
                    else if (token.Equals("lsi"))
                    {
                        Token value = tokenizer.NextToken();
                        if (!sbyte.TryParse(value.Span, out sbyte immediateValue) || immediateValue < MinSignedImmediate || immediateValue > MaxSignedImmediate)
                        {
                            throw value.CreateException($"Immediate value must be a number in range {MinSignedImmediate} - {MaxSignedImmediate}");
                        }

                        byteCode.AddInstruction((byte)(immediateValue & MaxImmediate));
                    }
                    else if (token.Equals("mov"))
                    {
                        Token destination = tokenizer.NextToken();
                        Token source = tokenizer.NextToken();

                        Register destinationRegister = ParseRegisterName(destination, OutputRegisterName);
                        Register sourceRegister = ParseRegisterName(source, InputRegisterName);

                        if (destinationRegister == sourceRegister && destinationRegister != Register.inOut)
                        {
                            throw destination.CreateException($"Source and destination register ({destinationRegister}) are identical");
                        }

                        byteCode.AddInstruction(destinationRegister, sourceRegister);
                    }
                    else if (token.Span.Length >= 1 && token.Span[0] == 'j')
                    {
                        Condition condition;
                        if (token.Equals("j"))
                        {
                            condition = Condition.Unconditional;
                        }
                        else if (token.Equals("je"))
                        {
                            condition = Condition.Equal;
                        }
                        else if (token.Equals("jne"))
                        {
                            condition = Condition.NotEqual;
                        }
                        else if (token.Equals("jgt"))
                        {
                            condition = Condition.GreaterThan;
                        }
                        else if (token.Equals("jgte"))
                        {
                            condition = Condition.GreaterThanEqual;
                        }
                        else if (token.Equals("jlt"))
                        {
                            condition = Condition.LessThan;
                        }
                        else if (token.Equals("jlte"))
                        {
                            condition = Condition.LessThanEqual;
                        }
                        else
                        {
                            throw AssemblyException.UnknownToken(token);
                        }

                        Token labelName = tokenizer.NextToken();
                        if (labelName.Exists)
                        {
                            Label label = GetLabel(labelName.Span.ToString());
                            byte offset = 0;
                            if (!byteCode.Failed)
                            {
                                offset = byteCode.Cursor;
                                byteCode.Add(0xFF);
                            }
                            label.References.Add(new(offset, new(lineNumber, labelName.Column)));
                        }

                        byteCode.AddInstruction(condition);
                    }
                    else if (token.Equals("and"))
                    {
                        byteCode.AddInstruction(Arithmetic.And);
                    }
                    else if (token.Equals("nand"))
                    {
                        byteCode.AddInstruction(Arithmetic.Nand);
                    }
                    else if (token.Equals("or"))
                    {
                        byteCode.AddInstruction(Arithmetic.Or);
                    }
                    else if (token.Equals("nor"))
                    {
                        byteCode.AddInstruction(Arithmetic.Nor);
                    }
                    else if (token.Equals("add"))
                    {
                        byteCode.AddInstruction(Arithmetic.Add);
                    }
                    else if (token.Equals("sub"))
                    {
                        byteCode.AddInstruction(Arithmetic.Sub);
                    }
                    else if (token.Equals("xor"))
                    {
                        if (!EnableXorOpcodes)
                        {
                            throw token.CreateException("The xor-opcode is only supported in additional codes.");
                        }
                        byteCode.AddInstruction(Arithmetic.Xor);
                    }
                    else if (token.Equals("xnor"))
                    {
                        if (!EnableXorOpcodes)
                        {
                            throw token.CreateException("The xnor-opcode is only supported in additional codes.");
                        }
                        byteCode.AddInstruction(Arithmetic.Xnor);
                    }
                    else
                    {
                        throw AssemblyException.UnknownToken(token);
                    }
                }
                catch (AssemblyException ex)
                {
                    AddError(new(lineNumber, ex.Column), ex.Message);

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
                    foreach (Reference reference in label.References)
                    {
                        AddError(reference.Location, $"Unknown label '{label.Name}' referenced.");
                        byteCode.Fail();
                    }
                }
                else if (label.References.Count == 0)
                {
                    AddWarning(label.Offset.Value.Location, $"Unreferenced label '{label.Name}'.");
                }
                else
                {
                    byte absoluteOffset = label.Offset.Value.Offset;
                    foreach (Reference reference in label.References)
                    {
                        if (absoluteOffset > Assembler.MaxImmediate)
                        {
                            if (!byteCode.Failed && EnableRelativeJumps)
                            {
                                // TODO: Verify range checks/overflows
                                byte maxRelativeOffset = MaxImmediate >> 1;
                                sbyte relativeOffset = (sbyte)(absoluteOffset - reference.Offset);
                                if (Math.Abs(relativeOffset) < maxRelativeOffset)
                                {
                                    // TODO: Verify that the immediate has the correct sign extension (e.g. 5th bit is set for negative)
                                    byteCode.Bytes[reference.Offset] = (byte)(MaxImmediate & relativeOffset);
                                    // Set "relative jump"-flag. Might be improved so that it isn't hardcoded?
                                    byteCode.Bytes[reference.Offset + 1] |= 0b00_001_000;
                                    continue;
                                }
                            }

                            byteCode.Fail();
                            AddError(label.Offset.Value.Location, $"Location of label '{label.Name}' is outside of the maximum immediate possible.");
                            break;
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
            Xor = 0b110,  // Additional
            Xnor = 0b111, // Additional
        }

        public const byte MaxImmediate = 0b11_1111;
        public const sbyte MaxSignedImmediate = MaxImmediate >> 1;
        public const sbyte MinSignedImmediate = -(MaxSignedImmediate + 1);

        private const string InputRegisterName = "in";
        private const string OutputRegisterName = "out";

        private static Register ParseRegisterName(Token registerName, string inOutRegisterName)
        {
            if (registerName.Equals("r0")) return Register.r0;
            else if (registerName.Equals("r1")) return Register.r1;
            else if (registerName.Equals("r2")) return Register.r2;
            else if (registerName.Equals("r3")) return Register.r3;
            else if (registerName.Equals("r4")) return Register.r4;
            else if (registerName.Equals("r5")) return Register.r5;
            else if (registerName.Equals(inOutRegisterName)) return Register.inOut;

            throw registerName.CreateException($"Unknown register name '{registerName.Span}'");
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

            public Token NextToken()
            {
                if (AtEnd)
                {
                    return new();
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

                return new(Line.AsSpan(startIndex, endIndex - startIndex), startIndex + 1);
            }
        }
    }
}
