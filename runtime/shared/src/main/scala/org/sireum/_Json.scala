package org.sireum

object Json_Ext {
  def parseString(text: String, offset: Z): (Option[String], Z) = {
    val s = text.value
    val sz = s.length
    var i = offset.toInt
    val sb = new StringBuilder

    if (i >= sz) return (None(), _Z(i))

    s(i) match {
      case '"' =>
        i += 1
        if (i >= sz) return (None(), _Z(i))
        var c = s(i)
        while (i < sz && c != '"') {
          c match {
            case '\\' =>
              i += 1
              if (i >= sz) return (None(), _Z(i))
              c = s(i)
              c match {
                case '"' => sb.append('"')
                case '\\' => sb.append('\\')
                case '/' => sb.append('/')
                case 'b' => sb.append('\b')
                case 'f' => sb.append('\f')
                case 'n' => sb.append('\n')
                case 'r' => sb.append('\r')
                case 't' => sb.append('\t')
                case 'u' =>
                  i += 1
                  if (i + 4 >= sz) return (None(), _Z(sz))
                  c = Integer.parseInt(s.substring(i, i + 4), 16).toChar
                  sb.append(c)
                  i += 4
                case _ => return (None(), _Z(i))
              }
            case _ => sb.append(c)
          }
          i += 1
          if (i < sz) c = s(i)
        }
        if (c == '"') return (Some(_2String(sb.toString)), _Z(i + 1))
        else return (None(), _Z(sz))
      case _ => return (None(), _Z(i))
    }
  }

  def parseNumber(text: String, offset: Z): (Option[String], Z) = {
    val s = text.value
    val sz = s.length
    var i = offset.toInt
    val sb = new StringBuilder

    if (i >= sz) return (None(), _Z(i))
  }
}
