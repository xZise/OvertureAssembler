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
            AssertError(assembler, "Location of label 'missing' is outside of the maximum immediate possible.", 33, 1);
        }

        [Fact]
        public void TestJumpForward()
        {
            Assembler assembler = new();
            byte[] code = assembler.Assemble(["j forward", "forward: mov r1 r2"]);
            Assert.Equal([0b00_000010, 0b11_000100, 0b10_010001], code);
        }

        [Fact]
        public void TestJumpBackward()
        {
            Assembler assembler = new();
            byte[] code = assembler.Assemble(["backward: mov r1 r2", "j backward"]);
            Assert.Equal([0b10_010001, 0b00_000000, 0b11_000100], code);
            AssertError(assembler, "Unknown label 'missing' referenced.", 1, 3);
        }

        private static void AssertError(Assembler assembler, string message, int lineNumber, int column)
        {
            Assert.NotEmpty(assembler.AssemblyMessages);
            Assembler.AssemblyMessage assemblyMessage = assembler.AssemblyMessages[0];
            Assert.True(assemblyMessage.IsError);
            Assert.Equal(message, assemblyMessage.Message);
            Assert.Equal(lineNumber, assemblyMessage.Location.LineNumber);
            Assert.Equal(column, assemblyMessage.Location.Column);
        }
    }
}
