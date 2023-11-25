using OvertureAssembler;

namespace OvertureAssemblerTest
{
    public class TestInstructions
    {
        [Theory]
        [MemberData(nameof(ImmediateValues))]
        [InlineData(0b01_000_000, "or")]
        [InlineData(0b01_000_010, "nor")]
        [InlineData(0b01_000_011, "and")]
        [InlineData(0b01_000_001, "nand")]
        [InlineData(0b01_000_100, "add")]
        [InlineData(0b01_000_101, "sub")]
        
        [InlineData(0b10_000_001, "mov r1 r0")]
        [InlineData(0b10_000_010, "mov r2 r0")]
        [InlineData(0b10_000_011, "mov r3 r0")]
        [InlineData(0b10_000_100, "mov r4 r0")]
        [InlineData(0b10_000_101, "mov r5 r0")]
        [InlineData(0b10_000_110, "mov out r0")]

        [InlineData(0b10_001_000, "mov r0 r1")]
        [InlineData(0b10_001_010, "mov r2 r1")]
        [InlineData(0b10_001_011, "mov r3 r1")]
        [InlineData(0b10_001_100, "mov r4 r1")]
        [InlineData(0b10_001_101, "mov r5 r1")]
        [InlineData(0b10_001_110, "mov out r1")]

        [InlineData(0b10_010_000, "mov r0 r2")]
        [InlineData(0b10_010_001, "mov r1 r2")]
        [InlineData(0b10_010_011, "mov r3 r2")]
        [InlineData(0b10_010_100, "mov r4 r2")]
        [InlineData(0b10_010_101, "mov r5 r2")]
        [InlineData(0b10_010_110, "mov out r2")]

        [InlineData(0b10_011_000, "mov r0 r3")]
        [InlineData(0b10_011_001, "mov r1 r3")]
        [InlineData(0b10_011_010, "mov r2 r3")]
        [InlineData(0b10_011_100, "mov r4 r3")]
        [InlineData(0b10_011_101, "mov r5 r3")]
        [InlineData(0b10_011_110, "mov out r3")]

        [InlineData(0b10_100_000, "mov r0 r4")]
        [InlineData(0b10_100_001, "mov r1 r4")]
        [InlineData(0b10_100_010, "mov r2 r4")]
        [InlineData(0b10_100_011, "mov r3 r4")]
        [InlineData(0b10_100_101, "mov r5 r4")]
        [InlineData(0b10_100_110, "mov out r4")]

        [InlineData(0b10_101_000, "mov r0 r5")]
        [InlineData(0b10_101_001, "mov r1 r5")]
        [InlineData(0b10_101_010, "mov r2 r5")]
        [InlineData(0b10_101_011, "mov r3 r5")]
        [InlineData(0b10_101_100, "mov r4 r5")]
        [InlineData(0b10_101_110, "mov out r5")]

        [InlineData(0b10_110_000, "mov r0 in")]
        [InlineData(0b10_110_001, "mov r1 in")]
        [InlineData(0b10_110_010, "mov r2 in")]
        [InlineData(0b10_110_011, "mov r3 in")]
        [InlineData(0b10_110_100, "mov r4 in")]
        [InlineData(0b10_110_101, "mov r5 in")]
        public void TestInstruction(byte expectedByteCode, string instruction)
        {
            Assembler assembler = new();
            byte[] byteCode = assembler.Assemble([instruction]);
            Assert.Equal([expectedByteCode], byteCode);
            Assert.Empty(assembler.AssemblyMessages);
            Assert.False(assembler.Failed);
        }

        public static TheoryData<byte, string> ImmediateValues()
        {
            TheoryData<byte, string> immediateInstructions = new();
            for (byte b = 0; b <= Assembler.MaxImmediate; b++)
            {
                immediateInstructions.Add(b, $"li {b}");
                immediateInstructions.Add(b, $"li 0x{b:x}");
                immediateInstructions.Add(b, $"li 0x{b:X}");
                immediateInstructions.Add(b, $"li 0X{b:x}");
                immediateInstructions.Add(b, $"li 0X{b:X}");
                immediateInstructions.Add(b, $"li {b:x}h");
                immediateInstructions.Add(b, $"li {b:X}h");
                immediateInstructions.Add(b, $"li {b:x}H");
                immediateInstructions.Add(b, $"li {b:X}H");
            }
            return immediateInstructions;
        }

        [Fact]
        public void TestIdentical()
        {
            Assembler assembler = new();
            byte[] byteCode = assembler.Assemble(["mov r1 r1"]);
            Utils.AssertError(assembler, "Source and destination register (r1) are identical", 1, 5);

            // Exception: in -> out is valid
            byteCode = assembler.Assemble(["mov out in"]);
            Assert.Equal([0b10_110_110], byteCode);
            Assert.Empty(assembler.AssemblyMessages);
            Assert.False(assembler.Failed);
        }

        [Fact]
        public void TestMissingParameter()
        {
            Assembler assembler = new();
            byte[] byteCode = assembler.Assemble(["mov r0"]);
            Assert.Empty(byteCode);
            Utils.AssertError(assembler, "Unknown register name ''", 1, 7);
        }

        [Theory]
        [InlineData("mov r0 r8", 8)]
        [InlineData("mov r8 r0", 5)]
        [InlineData("mov r8 r8", 5)]
        public void TestInvalidParameter(string instruction, int column)
        {
            Assembler assembler = new();
            byte[] byteCode = assembler.Assemble([instruction]);
            Assert.Empty(byteCode);
            Utils.AssertError(assembler, "Unknown register name 'r8'", 1, column);
        }
    }
}