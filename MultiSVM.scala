package multi_svm

import math._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object MultiSVM {
	def dot[T <% Double](m1: Iterable[T], m2: Iterable[Double]) : Double = {
		require(m1.size == m2.size) 
		return (for ((x, y) <- m1 zip m2) yield x * y).sum
	}

	def magnitude(x:Array[Double]) : Double = {
		var magnitude = math.sqrt((for (x_i <- x) yield x_i*x_i).sum)
		if (magnitude == 0.0) return .000001 else magnitude
	}

	def subtractVector(a:Array[Double],b:Array[Double]) : Array[Double] = {
		var c = new Array[Double](a.size)
		require(a.size == b.size)
		for (i <- 0 to a.size-1){ c(i) = (a(i) - b(i)) }
		return c
	}

	def addVector(a:Array[Double],b:Array[Double]) : Array[Double] = {
		var c = new Array[Double](a.size)
		require(a.size == b.size)
		for (i <- 0 to a.size-1){ c(i) = (a(i) + b(i)) }
		return c
	}

	def scalarVectorMultiply(a: Array[Double],scalar:Double) : Array[Double] = {
		var b = new Array[Double](a.size)
		for (i <- 0 to a.size-1) { b(i) = a(i) * scalar}
		return b	
	}

	def scalarVectorMultiply(a: Array[Int],scalar:Double) : Array[Double] = {
		var b = new Array[Double](a.size)
		for (i <- 0 to a.size-1) { b(i) = a(i) * scalar}
		return b	
	}

	def multi_svm_train(data: Array[(Array[Double],Int)],lambda: Double) : Array[Array[Double]] = {
		var svm_classifier_weights = new Array[Array[Double]](10)
		for(i <- 0 to 9) {
			svm_classifier_weights(i) = classifier_svm_train(data,lambda,i)
		}
		return svm_classifier_weights
	}

	def classifier_svm_train(data: Array[(Array[Double],Int)],lambda: Double, classifier: Int) : Array[Double] = {

		var tmpData = new Array[(Array[Double],Int)](data.length)
		for (i <- 0 to tmpData.length-1) {
			val tmpVectors = data(i)._1 
			val tmpClass = if (data(i)._2 == classifier) 1 else -1
			tmpData(i) = (tmpVectors,tmpClass)
		}	
		return pegasos_svm_train(tmpData,lambda)
	}

	def pegasos_svm_train(data: Array[(Array[Double],Int)],lambda: Double) : Array[Double] = {
		var weights = new Array[Double](data(0)._1.size)
		var del = new Array[Double](weights.size)
		var t = 0
		for (iter <- 1 to 20) {
			var empiricalLoss = 0.0
			for (digit <- data) {
				val x = digit._1
				val y = digit._2
				t += 1
				val eta = 1/(t*lambda)
				if ((y*dot(x,weights) < 1)) {
					del = addVector(scalarVectorMultiply(weights,(1-eta*lambda)),scalarVectorMultiply(x,(eta*y)))
				} else {
					del = scalarVectorMultiply(weights,(1-eta*lambda))
				}
				val tmp = (1/(math.sqrt(lambda)))/magnitude(del)
				weights = scalarVectorMultiply(del,math.min(1,tmp))

				empiricalLoss += math.max(0,(1-y*dot(x,weights)))
			}

			empiricalLoss = empiricalLoss / data.size
			val regularizationTerm = (lambda * math.pow(magnitude(weights),2)) / 2
			val svmObjective = regularizationTerm + empiricalLoss
			//println(f"SVM Objective at iteration $iter is: $svmObjective%1.3f")
		}
		return weights
	}
	
}

