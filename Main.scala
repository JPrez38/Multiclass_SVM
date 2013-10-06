package multi_svm

import math._

object Main {
	val trainingData = getDataFromFile("data/mnist_train.txt")
	val testData = getDataFromFile("data/mnist_test.txt")
	def main(args: Array[String]) = {
		val start = System.currentTimeMillis
		runMultiClassSVM()

		val end = System.currentTimeMillis
		println("Total Running Time of all Tests: " + (end-start)/1000.0 + " seconds")
	}

	def runMultiClassSVM() = {
		val featureVectors = Setup.featureGen(trainingData)
	}

	def getDataFromFile(file: String) : String = {
		import scala.io.Source 
		val source = Source.fromFile(file)
		val lines = source.getLines mkString "\n"
		return lines
	}
}