package contege.seqexec.reflective

import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.util.{List => JList}
import java.util.ArrayList
import java.util.Collections
import contege._
import contege.seqgen._
import contege.seqexec._

import clinitrewriter.Clinit

/**
 * Executes tests reflectively.
 */
class SequenceExecutor(stats: Stats, config: Config) {

	def execute(seq: AbstractCallSequence[_]): Option[Throwable] = {
		if (seq.isInstanceOf[Prefix]) {
			stats.executedSequences.incr
			if (config.callClinit) Clinit.reset
			val result = seq.execute
			result
		} else if (seq.isInstanceOf[Suffix]) {
			val suffix = seq.asInstanceOf[Suffix]
			stats.executedSequences.incr
			if (config.callClinit) Clinit.reset
			val var2Object = Map[Variable, Object]()
			val prefixResult = suffix.prefix.execute(var2Object)
			if (!prefixResult.isEmpty) {
				println("prefix fails when used with suffix -- this shouldn't happen...")
				return prefixResult
			}
			assert(!var2Object.isEmpty, "At least the CUT object should be here: "+var2Object)
			suffix.execute(var2Object)
		} else throw new IllegalArgumentException("unexpected subtype "+seq.getClass.getName)
	}
	
	def executeWithOutputVector(seq: AbstractCallSequence[_]): (Option[Throwable], OutputVector) = {
		if (seq.isInstanceOf[Prefix]) {
			stats.executedSequences.incr
			if (config.callClinit) Clinit.reset
			seq.executeWithOutputVector
		} else if (seq.isInstanceOf[Suffix]) {
			val suffix = seq.asInstanceOf[Suffix]
			stats.executedSequences.incr
			if (config.callClinit) Clinit.reset
		
			val var2Object = Map[Variable, Object]()
			val outputVector = new OutputVector
			val (prefixResult, prefixOutputVector) = suffix.prefix.executeWithOutputVector(var2Object, outputVector)
			if (!prefixResult.isEmpty) {
				println("prefix fails when used with suffix -- this shouldn't happen...")
				return (prefixResult, prefixOutputVector)
			}
			assert(!var2Object.isEmpty, "At least the CUT object should be here: "+var2Object)
			suffix.executeWithOutputVector(var2Object, outputVector)
		} else throw new IllegalArgumentException("unexpected subtype "+seq.getClass.getName)
	}
	
	/**
	 * Returns None if the execution passes and a message containing the reason for failure otherwise.
	 */
	def executeConcurrently(prefix: Prefix,
							suffix1: Suffix,
							suffix2: Suffix): JList[Throwable] = {
		assert(prefix == suffix1.prefix && suffix1.prefix == suffix2.prefix)
		
		stats.concurrentRuns.incr
				
		if (config.callClinit) Clinit.reset
		
		val result = Collections.synchronizedList(new ArrayList[Throwable]())
		
		val var2Object = Map[Variable, Object]()
		val prefixResult = prefix.execute(var2Object)
		if (!prefixResult.isEmpty) {
			// this shouldn't happen (normally) - but can happen, e.g. if there are too many open files now, or if the sequential execution is non-deterministic
			// --> behave as if the concurrent run was OK (we can't show it to cause an exception)
			//这种情况不应该发生(通常情况下)，但是也有可能发生，例如现在打开的文件太多，或者顺序执行不确定
			//——>表现得好像并发运行是OK的(我们不能显示它来引发异常)

			//前缀执行已经发生了异常，就不需要继续执行了
			//如果result为空，返回，不为空，报异常
			assert(result.isEmpty)
			return result
		}
		//如果前缀执行没有异常，就执行到这
		//var2Object现在为空要报异常
		assert(!var2Object.isEmpty, "\nThere must be a CUT object at least!\nPrefix:\n"+prefix)
		val var2ObjectT1 = var2Object.clone
		val var2ObjectT2 = var2Object.clone
		
		var unexpectedException: Option[Throwable] = None
		val t1 = new Thread() {
			override def run {
				try {
					val msg = suffix1.execute(var2ObjectT1)
					if (msg.isDefined) result.add(msg.get)
				} catch {
					case e: Throwable => unexpectedException = Some(e)
				}
			}
		}
		val t2 = new Thread() {
			override def run {
				try {
					val msg = suffix2.execute(var2ObjectT2)
					if (msg.isDefined) result.add(msg.get)
				} catch {
					case e: Throwable => unexpectedException = Some(e)
				}
			}
		}
		
		t1.start
		t2.start
		t1.join
		t2.join		

		// propagate unexpected exceptions (e.g. violations of assertions in ConTeGe) to the main thread
		// 将意外的异常(例如，对contest中的断言的违反)传播到主线程
		if (unexpectedException.isDefined) throw unexpectedException.get
		
		if (!result.isEmpty) stats.failedConcRuns.incr
		else stats.succeededConcRuns.incr
		
		result
		
	}
	def executeConcurrently2(prefix: Prefix,
													 suffix1: Suffix,
													 suffix2: Suffix): JList[Throwable] = {
		assert(prefix == suffix1.prefix && suffix1.prefix == suffix2.prefix)

		stats.concurrentRuns.incr

		if (config.callClinit) Clinit.reset

		val result = Collections.synchronizedList(new ArrayList[Throwable]())

		val var2Object = Map[Variable, Object]()
		val prefixResult = prefix.execute(var2Object)
		if (!prefixResult.isEmpty) {
			// this shouldn't happen (normally) - but can happen, e.g. if there are too many open files now, or if the sequential execution is non-deterministic
			// --> behave as if the concurrent run was OK (we can't show it to cause an exception)
			//这种情况不应该发生(通常情况下)，但是也有可能发生，例如现在打开的文件太多，或者顺序执行不确定
			//——>表现得好像并发运行是OK的(我们不能显示它来引发异常)
			//前缀执行已经发生了异常，就不需要继续执行了
			//如果result为空，返回，不为空，报异常
			assert(result.isEmpty)
			return result
		}
		//如果前缀执行没有异常，就执行到这
		//var2Object现在为空要报异常
		assert(!var2Object.isEmpty, "\nThere must be a CUT object at least!\nPrefix:\n"+prefix)
		val var2ObjectT1 = var2Object.clone
		val var2ObjectT2 = var2Object.clone

		var unexpectedException: Option[Throwable] = None
		val t1 = new Thread() {
			override def run {
				try {
					val msg = suffix1.execute2(var2ObjectT1)
					if (msg.isDefined) result.add(msg.get)
				} catch {
					case e: Throwable => unexpectedException = Some(e)
				}
			}
		}
		val t2 = new Thread() {
			override def run {
				try {
					val msg = suffix2.execute(var2ObjectT2)
					if (msg.isDefined) result.add(msg.get)
				} catch {
					case e: Throwable => unexpectedException = Some(e)
				}
			}
		}

		t1.start
		t2.start
		t1.join
		t2.join
		// propagate unexpected exceptions (e.g. violations of assertions in ConTeGe) to the main thread
		// 将意外的异常(例如，对contest中的断言的违反)传播到主线程
		if (unexpectedException.isDefined) throw unexpectedException.get

		if (!result.isEmpty) stats.failedConcRuns.incr
		else stats.succeededConcRuns.incr

		result

	}
		
}
