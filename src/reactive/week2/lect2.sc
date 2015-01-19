//identity and change
//lect3 loops
def WHILE(condition: => Boolean)(command: => Unit): Unit = {
  if(condition) {
    command
    WHILE(condition)(command)
  } else ()
}
var i = 0
WHILE(i < 5) {
  print(i)
  i = i + 1
}
object REPEAT {
  def apply(command: => Unit): Repeat = {
    new Repeat(command)
  }
}
object DO {
  def apply(command: => Unit): Do = {
    new Do(command)
  }
}
class Do(command: => Unit) {
  def WHILE(condition: => Boolean): Unit = {
    command
    if(condition) {
      DO(command).WHILE(condition)
    } else {
      ()
    }
  }
}

class Repeat(command: => Unit) {
  def UNTIL(condition: => Boolean): Unit = {
    command
    if(condition) {
      ()
    } else {
      REPEAT(command).UNTIL(condition)
    }
  }
}

var j = 0
REPEAT {
  print(j)
  j = j + 1
} UNTIL (j == 5)

j = 0
DO {
  print(j)
  j = j + 1
} WHILE (j < 5)