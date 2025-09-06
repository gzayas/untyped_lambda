import Parser
import Lamb.Syntax
namespace Lamb.Parser.Basic

abbrev Parser := SimpleParserT Substring Char (StateM Syntax.Context)

end Lamb.Parser.Basic
