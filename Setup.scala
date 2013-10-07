package multi_svm

import math._

object Setup {
	def featureGen(inputFile: String) : Array[(Array[Double],Int)] = {
		val digits = inputFile.split("\n")
		var featureVectors = new Array[(Array[Double],Int)](digits.length)
		var i = 0
		for (digit <- digits) {
			val temp = digit.split(",")
			val numberedTemp = new Array[Int](temp.length)
			for (i <- 0 to temp.length-1) {
				numberedTemp(i) = temp(i).toInt
			}
			val tmp = numberedTemp.splitAt(1)
			val actualDigit = tmp._1
			var featureVector = new Array[Double](tmp._2.length)
			for (i <- 0 to tmp._2.length-1) {
				featureVector(i) = 2*(tmp._2(i)/255.0)-1
			}

			featureVectors(i) = (featureVector,actualDigit.head)
			i+=1
		}

		//println(featureVectors(10)._1)
		//featureVectors(10)._2.foreach(x => print(f"$x%1.2f ,"))
		

		return featureVectors
	}
}