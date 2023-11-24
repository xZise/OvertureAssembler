using OvertureAssembler;

namespace OvertureAssemblerTest
{
    public static class Utils
    {
        public static void AssertError(Assembler assembler, string message, int lineNumber, int column)
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
