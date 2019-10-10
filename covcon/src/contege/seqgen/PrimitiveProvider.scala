package contege.seqgen

import contege.Random

/**
 * Provides random values for primitive types.
 */
class PrimitiveProvider(random: Random) {

    def isNonRefType(typ: String) = {
        if (typ == "int") true
		else if (typ == "byte") true
		else if (typ == "short") true
		else if (typ == "long") true
		else if (typ == "char") true
		else if (typ == "boolean") true
		else if (typ == "float") true
		else if (typ == "double") true		
		else false
    }
    
	def isPrimitiveOrWrapper(typ: String) = {
		if (typ == "int" || typ == "java.lang.Integer") true
		else if (typ == "java.lang.String") true  // we treat Strings as primitive types
		else if (typ == "byte" || typ == "java.lang.Byte") true
		else if (typ == "short" || typ == "java.lang.Short") true
		else if (typ == "long" || typ == "java.lang.Long") true
		else if (typ == "char" || typ == "java.lang.Character") true
		else if (typ == "boolean" || typ == "java.lang.Boolean") true
		else if (typ == "float" || typ == "java.lang.Float") true
		else if (typ == "double" || typ == "java.lang.Double") true		
		else false
	}
	def isArray(typ: String) = {
		if (typ.charAt(0).equals('[') ){
			if (typ.charAt(1).equals('B') || typ.charAt(1).equals('C') || typ.charAt(1).equals('D') ||
				typ.charAt(1).equals('F') || typ.charAt(1).equals('I') || typ.charAt(1).equals('J') ||
				typ.charAt(1).equals('S') || typ.charAt(1).equals('Z') ){
				true
			}
			else false
		}
		else false

	}

	def next(typ: String): Object = {
		if (typ == "int" || typ == "java.lang.Integer") {
		  //println("RandomInt : " + nextInt)
		  nextInt.asInstanceOf[Object]
		  }
		else if (typ == "java.lang.String") nextString  // we treat Strings as primitive types
		else if (typ == "byte" || typ == "java.lang.Byte") nextByte.asInstanceOf[Object]
		else if (typ == "short" || typ == "java.lang.Short") nextShort.asInstanceOf[Object]
		else if (typ == "long" || typ == "java.lang.Long") nextLong.asInstanceOf[Object]
		else if (typ == "char" || typ == "java.lang.Character") nextChar.asInstanceOf[Object]
		else if (typ == "boolean" || typ == "java.lang.Boolean") nextBoolean.asInstanceOf[Object]
		else if (typ == "float" || typ == "java.lang.Float") nextFloat.asInstanceOf[Object]
		else if (typ == "double" || typ == "java.lang.Double") nextDouble.asInstanceOf[Object]
		else throw new RuntimeException("unsupported type: "+typ)
	}
	def nextArray(typ: String): Object = {
		if (typ.charAt(1).equals('B') ){
			val arr:Array[Byte] = new Array[Byte](3)
			arr.asInstanceOf[Object]
		}else if (typ.charAt(1).equals('C') ){
			val arr:Array[Char] = new Array[Char](3)
			arr.asInstanceOf[Object]
		}else if (typ.charAt(1).equals('D') ){
			val arr:Array[Double] = new Array[Double](3)
			arr.asInstanceOf[Object]
		}else if (typ.charAt(1).equals('F') ){
			val arr:Array[Float] = new Array[Float](3)
			arr.asInstanceOf[Object]
		}else if (typ.charAt(1).equals('I') ){
			val arr:Array[Int] = new Array[Int](3)
			arr.asInstanceOf[Object]
		}else if (typ.charAt(1).equals('J') ){
			val arr:Array[Long] = new Array[Long](3)
			arr.asInstanceOf[Object]
		}else if (typ.charAt(1).equals('S') ){
			val arr:Array[Short] = new Array[Short](3)
			arr.asInstanceOf[Object]
		}else if (typ.charAt(1).equals('Z') ){
			val arr:Array[Boolean] = new Array[Boolean](3)
			arr.asInstanceOf[Object]
		}else throw new RuntimeException("unsupported type: "+typ)

	}
	
	private def intPredefined = List(-100, -3, -2, -1, 0, 1, 2, 3, 100)
	private def charPredefined = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
			                          'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
			                          '0','1','2','3','4','5','6','7','8','9',
			                           '~','`','!','@','#','$','%','^','&','*','(',')','-','_','+','=','{','}','[',']',':',';','\'','<','>',
			                           ',','.','?','/','|',' ') // all chars found on a US keyboard, except for '\' and '"' which causes trouble in Strings 
    private def floatPredefined = List(-100f, -3.0f, -2.0f, -1.0f, -0.1f, 0.0f, 0.1f, 1.0f, 2.0f, 3.0f, 100f)
    private def doublePredefined = List(-100d, -3.0d, -2.0d, -1.0d, -0.1d, 0.0d, 0.1d, 1.0d, 2.0d, 3.0d, 100d)
	
	def nextInt = {
		intPredefined(random.nextInt(intPredefined.size))
	}
	
	def nextString = {
		val sb = new StringBuilder
		while (random.nextBool) sb.append(nextChar)
		sb.toString
	}
	
	def nextByte = {
		intPredefined(random.nextInt(intPredefined.size)).asInstanceOf[Byte]
	}
	
	def nextShort = {
		intPredefined(random.nextInt(intPredefined.size)).asInstanceOf[Short]
	}
	
	def nextLong = {
		intPredefined(random.nextInt(intPredefined.size)).asInstanceOf[Long]
	}
	
	def nextChar = {
		charPredefined(random.nextInt(charPredefined.size)) // always use predefined chars, random chars typically make no sense
	}
	
	def nextBoolean = random.nextBool
	
	def nextFloat = {
		floatPredefined(random.nextInt(floatPredefined.size))
	}
	
	def nextDouble = {
		doublePredefined(random.nextInt(doublePredefined.size))
	}
	
}