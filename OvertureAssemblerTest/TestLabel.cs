using OvertureAssembler;

namespace OvertureAssemblerTest
{
    public class TestLabel
    {
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
        }
    }
}
