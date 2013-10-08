package multi_svm

import math._

object Main {
	val trainingData = getDataFromFile("data/mnist_train.txt")
	val testData = getDataFromFile("data/mnist_test.txt")
	def main(args: Array[String]) = {
		val start = System.currentTimeMillis
		//runMultiClassSVM(true,2000,pow(2,-3))
		var lambdas = List[Double]()
		for (i <- -5 to 1) {lambdas::= (math.pow(2,i))}
		lambdas = lambdas.reverse
		//lambdas.foreach(x => runCrossValidationSVM(false,2000,x))
		runMultiClassSVM(true,2000,math.pow(2,-3))

		val end = System.currentTimeMillis
		println("Total Running Time of all Tests: " + (end-start)/1000.0 + " seconds")
	}

	def runMultiClassSVM(useTestData: Boolean,trainingSize:Int,lambda: Double) = {
		val start = System.currentTimeMillis
		val featureVectors = Setup.featureGen(trainingData)
		val tmp = featureVectors.splitAt(trainingSize) /*splits training data set for validation set*/
		val trainingDataSet = tmp._1
		val validationDataSet = if(useTestData) Setup.featureGen(testData) else tmp._2
		val svm_classifiers = MultiSVM.multi_svm_train(trainingDataSet,lambda)
		val testError = MultiSVM.multi_svm_test(validationDataSet,svm_classifiers)
		println(f"Test Error: $testError%1.3f for lambda of $lambda")
		val end = System.currentTimeMillis
		println("Total Running Time of algorithm: " + (end-start)/1000.0 + " seconds")
	}

	def runCrossValidationSVM(useTestData: Boolean,trainingSize:Int,lambda: Double) = {
		val start = System.currentTimeMillis
		val featureVectors = Setup.featureGen(trainingData)
		val tmp = featureVectors.splitAt(trainingSize) /*splits training data set for validation set*/
		val trainingDataSet = tmp._1
		val validationDataSet = if(useTestData) Setup.featureGen(testData) else tmp._2
		val testError = MultiSVM.cross_validation_svm(trainingDataSet,lambda,5)
		println(f"Cross Validation Error: $testError%1.3f for lambda of $lambda")
		val end = System.currentTimeMillis
		println("Total Running Time of algorithm: " + (end-start)/1000.0 + " seconds")
	}	

	def getDataFromFile(file: String) : String = {
		import scala.io.Source 
		val source = Source.fromFile(file)
		val lines = source.getLines mkString "\n"
		return lines
	}

	def outputScaledData(data: Array[(Array[Double],Int)]) = {
		for(x <- data) {
			print(x._2 + " ")
			for( i <- 0 to x._1.length-1) {
				print((i+1)+":"+x._1(i)+" ")
			}
			print("\n")
		}
	}
}