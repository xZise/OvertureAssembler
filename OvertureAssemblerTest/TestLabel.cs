using OvertureAssembler;

namespace OvertureAssemblerTest
{
    public class TestLabel
    {
        [Fact]
        public void TestMissing()
        {
            Assembler assembler = new();
            byte[] code = assembler.Assemble(["j missing"]);
            Assert.Empty(code);
            Utils.AssertError(assembler, "Unknown label 'missing' referenced.", 1, 3);
        }

        [Fact]
        public void TestTooLarge()
        {
            Assembler assembler = new();
            // Use jump with label, as they take 2 bytes (with the immediate load before)
            List<string> lines = Enumerable.Repeat("j s", 31).ToList();
            lines.Insert(0, "s: j missing");
            lines.Add("missing: mov r1 r2");
            byte[] code = assembler.Assemble(lines.ToArray());
            Assert.Empty(code);
            Utils.AssertError(assembler, "Location of label 'missing' is outside of the maximum immediate possible.", 33, 1);
        }

        [Fact]
        public void TestRelativeJump()
        {
            const byte JUMP = 0b11_000_100;
            const byte LABEL_S = 0b00_111110;
            const byte MOV_R1_R2 = 0b10_010_001;
            const byte JUMP_RELATIVE = 0b11_001_100;
            const byte LABEL_RELATIVE = 0b00_000010;

            Assembler assembler = new();
            assembler.EnableRelativeJumps = true;
            // Use jump with label, as they take 2 bytes (with the immediate load before)
            List<string> lines = Enumerable.Repeat("j s", 31).ToList();
            lines.Add("s: j relative");
            lines.Add("relative: mov r1 r2");
            byte[] code = assembler.Assemble(lines.ToArray());
            Assert.Equal([LABEL_S, JUMP, LABEL_S, JUMP, LABEL_S, JUMP, LABEL_S, JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,

                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,

                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,

                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,

                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,

                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_S,
                JUMP,
                LABEL_RELATIVE,
                JUMP_RELATIVE,
                MOV_R1_R2,
            ], code);
            Assert.Empty(assembler.AssemblyMessages);
            Assert.False(assembler.Failed);
        }

        [Fact]
        public void TestJumpForward()
        {
            Assembler assembler = new();
            byte[] code = assembler.Assemble(["j forward", "forward: mov r1 r2"]);
            Assert.Equal([0b00_000010, 0b11_000100, 0b10_010001], code);
            Assert.Empty(assembler.AssemblyMessages);
            Assert.False(assembler.Failed);
        }

        [Fact]
        public void TestJumpBackward()
        {
            Assembler assembler = new();
            byte[] code = assembler.Assemble(["backward: mov r1 r2", "j backward"]);
            Assert.Equal([0b10_010001, 0b00_000000, 0b11_000100], code);
            Assert.Empty(assembler.AssemblyMessages);
            Assert.False(assembler.Failed);
        }
    }
}
