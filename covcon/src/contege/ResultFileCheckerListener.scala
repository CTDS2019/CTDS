package contege
import java.io.PrintStream

class ResultFileCheckerListener(fileName: String) extends CheckerListener {

  private val ps = new PrintStream(fileName)

  def appendResultMsg(s: String) = {
    //之前把这里注销了，实际上不可以，因为这个方法在很多地方被调用。
    ps.println(s)
  }

  def notifyDoneAndBugFound(testCode: String) = {
    ps.flush
    ps.close
  }

  def notifyDoneNoBug = {
    ps.flush
    ps.close
  }

  // ignore everything that is not result-related
  def updateNbGeneratedTests(nb: Long) = {}
  def appendStatusMsg(s: String) = {}

}