type Action = () => Unit


class Wire {

  private var sigVal = false
  private var actions: List[Action] = List()

  def getSignal: Boolean = sigVal
  def setSignal(sign: Boolean): Unit = {
    if(sigVal != sign) {
      sigVal = sign
      actions foreach (_())
    }
  }
  def addAction(a: Action): Unit = {
    actions = a :: actions
    a()
  }
}

trait Simulation {
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()

  private var curtime = 0

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: => Unit ): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda  = insert(agenda, item)
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }

  private def insert(ag: Agenda, event: Event): Agenda = ag match {
    case first :: rest if first.time <= event.time =>
      first :: insert(rest, event)
    case _ => event :: ag
//    case Nil =>
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }
}


trait Parameters {

  val InverterDelay = 2
  val AndGateDelay = 3
  val OrGateDelay = 5

}

class Gates extends Simulation with Parameters {
  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {output setSignal !inputSig}
    }
    input addAction invertAction
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val sig1 = in1.getSignal
      val sig2 = in2.getSignal
      afterDelay(OrGateDelay) { output setSignal (sig1 | sig2)}
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val sig1 = in1.getSignal
      val sig2 = in2.getSignal
      afterDelay(AndGateDelay) { output setSignal (sig1 & sig2)}
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}" )
    }
    wire addAction probeAction
  }

}

class Circuits extends Gates {

  def halfAdder(in1: Wire, in2: Wire, sum: Wire, c: Wire): Unit = {
    val a = new Wire
    val b = new Wire

    orGate(in1, in2, a)
    andGate(in1, in2, c)
    inverter(c, b)
    andGate(a, b, sum)
  }

  def fullAdder(in1: Wire, in2: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s = new Wire
    val c1,c2 = new Wire

    halfAdder(in2, cin, s, c1)
    halfAdder(in1, s, sum, c2)
    orGate(c1,c2, cout)

  }
}


val in1, in2, sum, carry = new Wire
val c = new Circuits
c.halfAdder(in1, in2, sum, carry)
c.probe("sum", sum)
c.probe("carry", carry)

in1 setSignal true
c.run()

in2 setSignal true
c.run()

in1 setSignal false
c.run()







