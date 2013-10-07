package multi_svm

import math._

object Main {
	val trainingData = getDataFromFile("data/mnist_train.txt")
	val testData = getDataFromFile("data/mnist_test.txt")
	def main(args: Array[String]) = {
		val start = System.currentTimeMillis
		runMultiClassSVM(true,2000,pow(2,-3))

		val end = System.currentTimeMillis
		println("Total Running Time of all Tests: " + (end-start)/1000.0 + " seconds")
	}

	def runMultiClassSVM(useTestData: Boolean,trainingSize:Int,lambda: Double) = {
		val featureVectors = Setup.featureGen(trainingData)
		val tmp = featureVectors.splitAt(trainingSize) /*splits training data set for validation set*/
		val trainingDataSet = tmp._1
		val validationDataSet = if(useTestData) Setup.featureGen(testData) else tmp._2
		val svm_classifiers = MultiSVM.multi_svm_train(trainingDataSet,lambda)
		val testError = MultiSVM.multi_svm_test(validationDataSet,svm_classifiers)
		println(f"Test Error: $testError%1.3f for lambda of $lambda")
	}

	def getDataFromFile(file: String) : String = {
		import scala.io.Source 
		val source = Source.fromFile(file)
		val lines = source.getLines mkString "\n"
		return lines
	}
}